#!/usr/bin/env python3
"""
WKBL Schedule Collector

Collects game schedules from WKBL official site.
- Scheduled games: future games without scores
- Completed games: can link to games table via game_id
"""

import argparse
import re
import sqlite3
import sys
import time
from pathlib import Path
from urllib.parse import parse_qs, urlparse

import requests
from bs4 import BeautifulSoup

BASE_URL = "https://www.wkbl.or.kr"
SCHEDULE_URL = f"{BASE_URL}/game/sch/schedule1.asp"

ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_DB_PATH = ROOT_DIR / "data" / "wkbl.db"

REQUEST_TIMEOUT = (5, 20)
REQUEST_RETRIES = 3
REQUEST_BACKOFF = 0.6

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    )
}

# Team code mapping (WKBL internal names -> team codes)
TEAM_NAME_TO_CODE = {
    "삼성생명": "SL",
    "KB스타즈": "KB",
    "신한은행": "SH",
    "우리은행": "WR",
    "하나은행": "HN",
    "BNK 썸": "BNK",
    "BNK썸": "BNK",
    # All-star teams (game_type=10)
    "팀 유니블": "UNIBL",
    "팀 포니블": "PNIBL",
}


def request_with_retry(url: str, session: requests.Session) -> str:
    """GET request with retry logic."""
    last_exc = None
    for attempt in range(1, REQUEST_RETRIES + 1):
        try:
            res = session.get(url, timeout=REQUEST_TIMEOUT, headers=HEADERS)
            res.raise_for_status()
            res.encoding = res.apparent_encoding
            return res.text
        except requests.RequestException as exc:
            last_exc = exc
            if attempt == REQUEST_RETRIES:
                raise
            time.sleep(REQUEST_BACKOFF * attempt)
    if last_exc:
        raise last_exc
    raise RuntimeError("request failed without exception")


def get_current_season(session: requests.Session) -> str:
    """Get current season code."""
    html = request_with_retry(SCHEDULE_URL, session)
    soup = BeautifulSoup(html, "html.parser")
    opt = soup.select_one("#season_gu option[selected]")
    return opt.get("value") if opt else "046"


def get_months_for_season(season: str, session: requests.Session) -> list[str]:
    """Get available months for a season."""
    url = f"{SCHEDULE_URL}?gun=1&season_gu={season}"
    html = request_with_retry(url, session)
    soup = BeautifulSoup(html, "html.parser")
    months = []
    for opt in soup.select("#ym option"):
        val = opt.get("value", "")
        if val and val.isdigit() and len(val) == 6:  # YYYYMM format
            months.append(val)
    return sorted(months)


def parse_team_code(team_name: str) -> str:
    """Convert team display name to team code."""
    name = team_name.strip()
    if name in TEAM_NAME_TO_CODE:
        return TEAM_NAME_TO_CODE[name]
    # Fuzzy match
    for key, code in TEAM_NAME_TO_CODE.items():
        if key in name or name in key:
            return code
    return name[:3].upper()  # Fallback to first 3 chars


def parse_schedule_page(html: str, season: str, ym: str) -> list[dict]:
    """Parse schedule HTML and extract game entries."""
    soup = BeautifulSoup(html, "html.parser")
    games = []

    # Find all game result links
    links = soup.find_all("a", href=re.compile(r"/game/result\.asp"))

    for link in links:
        href = link.get("href", "")
        text = link.get_text(strip=True)

        # Parse game parameters from URL
        qs = parse_qs(urlparse(href).query)
        game_type = qs.get("game_type", ["01"])[0]
        game_no = qs.get("game_no", [""])[0]
        link_ym = qs.get("ym", [ym])[0]

        # Check if game has score (completed)
        score_match = re.search(r'\[(\d+)vs(\d+)\]', text)
        has_score = bool(score_match)

        # Find parent row/cell to get team names
        parent = link.find_parent("td") or link.find_parent("li")
        if not parent:
            continue

        # Look for team names in nearby elements
        # Pattern: "Team1 vs Team2 [score]"
        row = link.find_parent("tr")
        if row:
            row_text = row.get_text(" ", strip=True)
            # Extract teams from pattern: "TeamA vs TeamB"
            vs_match = re.search(r'([가-힣A-Za-z ]+)\s*vs\s*([가-힣A-Za-z ]+)', row_text)
            if vs_match:
                home_team = vs_match.group(1).strip()
                away_team = vs_match.group(2).strip()

                # Determine game date from ym (YYYYMM) + day context
                # We'll need more sophisticated parsing for exact date
                year = link_ym[:4]
                month = link_ym[4:6]

                # Try to find day number in row context
                day_match = re.search(r'(?:^|\D)(\d{1,2})(?:\D|$)', row_text)
                day = day_match.group(1).zfill(2) if day_match else "01"

                game_date = f"{year}-{month}-{day}"

                game = {
                    "game_date": game_date,
                    "game_time": None,  # WKBL site doesn't show time in schedule view
                    "season_code": season,
                    "home_team_code": parse_team_code(home_team),
                    "away_team_code": parse_team_code(away_team),
                    "venue": None,  # Not available in schedule view
                    "status": "final" if has_score else "scheduled",
                    "game_type": game_type,
                    "game_no": game_no,
                }
                games.append(game)

    return games


def collect_schedule(season: str, months: list[str], session: requests.Session) -> list[dict]:
    """Collect schedule for given season and months."""
    all_games = []
    seen = set()

    for ym in months:
        url = f"{SCHEDULE_URL}?gun=1&season_gu={season}&viewType=2&ym={ym}"
        try:
            html = request_with_retry(url, session)
            games = parse_schedule_page(html, season, ym)

            for g in games:
                key = (g["game_date"], g["home_team_code"], g["away_team_code"])
                if key not in seen:
                    seen.add(key)
                    all_games.append(g)

            print(f"[{season}] {ym}: {len(games)} games found", flush=True)
            time.sleep(0.3)  # Be polite
        except Exception as e:
            print(f"[{season}] {ym}: ERROR - {e}", file=sys.stderr)

    return all_games


def save_to_db(games: list[dict], db_path: Path, dry_run: bool = False) -> int:
    """Save schedule entries to SQLite database."""
    if dry_run:
        print(f"DRY RUN: Would insert {len(games)} schedule entries")
        for g in games[:5]:
            print(f"  {g['game_date']} {g['home_team_code']} vs {g['away_team_code']} ({g['status']})")
        return len(games)

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Ensure schedule table exists
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS schedule (
          schedule_id INTEGER PRIMARY KEY AUTOINCREMENT,
          game_date TEXT NOT NULL,
          game_time TEXT,
          season_code TEXT NOT NULL,
          home_team_code TEXT NOT NULL,
          away_team_code TEXT NOT NULL,
          venue TEXT,
          status TEXT DEFAULT 'scheduled',
          game_id INTEGER,
          UNIQUE(game_date, home_team_code, away_team_code)
        )
    """)

    # Create indexes if not exist
    cursor.execute("CREATE INDEX IF NOT EXISTS idx_schedule_date ON schedule(game_date)")
    cursor.execute("CREATE INDEX IF NOT EXISTS idx_schedule_status ON schedule(status)")
    cursor.execute("CREATE INDEX IF NOT EXISTS idx_schedule_season ON schedule(season_code)")

    inserted = 0
    updated = 0

    for g in games:
        try:
            cursor.execute("""
                INSERT INTO schedule (game_date, game_time, season_code, home_team_code, away_team_code, venue, status)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(game_date, home_team_code, away_team_code) DO UPDATE SET
                    status = excluded.status,
                    game_time = COALESCE(excluded.game_time, game_time)
            """, (
                g["game_date"],
                g.get("game_time"),
                g["season_code"],
                g["home_team_code"],
                g["away_team_code"],
                g.get("venue"),
                g["status"],
            ))
            if cursor.rowcount > 0:
                inserted += 1
            else:
                updated += 1
        except sqlite3.Error as e:
            print(f"DB error for {g}: {e}", file=sys.stderr)

    conn.commit()
    conn.close()

    print(f"Saved: {inserted} inserted, {updated} updated")
    return inserted + updated


def main() -> int:
    parser = argparse.ArgumentParser(description="Collect WKBL game schedule")
    parser.add_argument("--season", help="Season code (default: current)")
    parser.add_argument("--months", help="Specific months (comma-separated YYYYMM)")
    parser.add_argument("--db", type=Path, default=DEFAULT_DB_PATH, help="SQLite database path")
    parser.add_argument("--dry-run", action="store_true", help="Don't write to database")
    args = parser.parse_args()

    session = requests.Session()

    # Determine season
    season = args.season or get_current_season(session)
    print(f"Collecting schedule for season {season}")

    # Determine months
    if args.months:
        months = [m.strip() for m in args.months.split(",")]
    else:
        months = get_months_for_season(season, session)

    if not months:
        print("No months found for this season", file=sys.stderr)
        return 1

    print(f"Processing {len(months)} months: {months}")

    # Collect schedule
    games = collect_schedule(season, months, session)

    print(f"\nTotal: {len(games)} unique games collected")

    # Filter to only scheduled (future) games if desired
    scheduled = [g for g in games if g["status"] == "scheduled"]
    completed = [g for g in games if g["status"] == "final"]
    print(f"  Scheduled: {len(scheduled)}, Completed: {len(completed)}")

    # Save to database
    if games:
        save_to_db(games, args.db, args.dry_run)

    return 0


if __name__ == "__main__":
    sys.exit(main())
