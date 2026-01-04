#!/usr/bin/env python3
"""
Scrape WKBL game results and player statistics for the 2025-26 season.

Fetches:
- Game schedule (date, teams, scores, stadium)
- Player box scores with advanced stats (TS%, eFG%, Game Score)

Usage:
    python scripts/scrape_wkbl_games.py
    python scripts/scrape_wkbl_games.py --limit 5  # Test with 5 games
    python scripts/scrape_wkbl_games.py --dry-run  # Don't update DB
    python scripts/scrape_wkbl_games.py --game-no 38  # Scrape specific game
"""

from __future__ import annotations

import argparse
import json
import re
import sqlite3
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

import requests
from bs4 import BeautifulSoup

# Paths
ROOT_DIR = Path(__file__).resolve().parents[1]
DB_PATH = ROOT_DIR / "data" / "wkbl.db"
OUTPUT_JSON = ROOT_DIR / "data" / "scraped_games.json"

# WKBL URLs
GAME_RESULT_URL = "https://www.wkbl.or.kr/game/result.asp"
PLAYER_STATS_URL = "https://www.wkbl.or.kr/game/ajax/ajax_game_result_2.asp"
SCHEDULE_URL = "https://www.wkbl.or.kr/game/sch/schedule1.asp"

# Request settings (be respectful to WKBL servers)
REQUEST_DELAY = 1.0  # seconds between requests
REQUEST_TIMEOUT = 15
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7",
    "Content-Type": "application/x-www-form-urlencoded",
}

# Team code mapping (Korean name -> code)
TEAM_NAME_TO_CODE = {
    "삼성생명": "03",
    "신한은행": "07",
    "우리은행": "05",
    "하나은행": "09",
    "BNK 썸": "11",
    "BNK썸": "11",
    "KB스타즈": "01",
}

# Season code for 2025-26
SEASON_CODE = "046"


@dataclass
class GameInfo:
    """Game basic information."""
    game_id: str
    season_code: str
    game_type: str
    game_no: int
    game_date: Optional[str]
    home_team_code: str
    away_team_code: str
    home_score: int
    away_score: int
    stadium: Optional[str]
    # Quarter scores
    q1_home: Optional[int] = None
    q2_home: Optional[int] = None
    q3_home: Optional[int] = None
    q4_home: Optional[int] = None
    ot_home: int = 0
    q1_away: Optional[int] = None
    q2_away: Optional[int] = None
    q3_away: Optional[int] = None
    q4_away: Optional[int] = None
    ot_away: int = 0


@dataclass
class PlayerGameStats:
    """Player statistics for a single game."""
    game_id: str
    team_code: str
    player_name: str
    player_id: Optional[str]
    position: str
    min_seconds: int
    fg_2p_m: int
    fg_2p_a: int
    fg_3p_m: int
    fg_3p_a: int
    ft_m: int
    ft_a: int
    reb_off: int
    reb_def: int
    reb_tot: int
    ast: int
    pf: int
    stl: int
    tov: int
    blk: int
    pts: int
    # Advanced stats (calculated)
    ts_pct: Optional[float] = None
    efg_pct: Optional[float] = None
    game_score: Optional[float] = None


def parse_minutes(min_str: str) -> int:
    """Parse minutes string like '40:00' to seconds."""
    if not min_str or min_str == "-":
        return 0
    try:
        parts = min_str.split(":")
        if len(parts) == 2:
            return int(parts[0]) * 60 + int(parts[1])
        return int(parts[0]) * 60
    except (ValueError, IndexError):
        return 0


def parse_shot_stats(stat_str: str) -> tuple[int, int]:
    """Parse shot stats like '2-7' to (made, attempted)."""
    if not stat_str or stat_str == "-":
        return 0, 0
    try:
        parts = stat_str.split("-")
        if len(parts) == 2:
            return int(parts[0]), int(parts[1])
        return 0, 0
    except (ValueError, IndexError):
        return 0, 0


def parse_date(date_str: str) -> Optional[str]:
    """Parse Korean date string like '2025년 12월 29일 월요일' to 'YYYY-MM-DD'."""
    if not date_str:
        return None
    match = re.search(r"(\d{4})년\s*(\d{1,2})월\s*(\d{1,2})일", date_str)
    if match:
        year, month, day = match.groups()
        return f"{year}-{int(month):02d}-{int(day):02d}"
    return None


def normalize_team_name(name: str) -> str:
    """Normalize team name by removing spaces for comparison.

    Handles cases like "BNK 썸" vs "BNK썸".
    """
    return name.replace(" ", "")


def calculate_ts_pct(pts: int, fg_2p_a: int, fg_3p_a: int, ft_a: int) -> Optional[float]:
    """Calculate True Shooting Percentage.

    TS% = PTS / (2 * (FGA + 0.44 * FTA))
    """
    fga = fg_2p_a + fg_3p_a
    denominator = 2 * (fga + 0.44 * ft_a)
    if denominator == 0:
        return None
    return pts / denominator


def calculate_efg_pct(fg_2p_m: int, fg_3p_m: int, fg_2p_a: int, fg_3p_a: int) -> Optional[float]:
    """Calculate Effective Field Goal Percentage.

    eFG% = (FGM + 0.5 * 3PM) / FGA
    """
    fga = fg_2p_a + fg_3p_a
    if fga == 0:
        return None
    fgm = fg_2p_m + fg_3p_m
    return (fgm + 0.5 * fg_3p_m) / fga


def calculate_game_score(
    pts: int, fg_2p_m: int, fg_3p_m: int, fg_2p_a: int, fg_3p_a: int,
    ft_m: int, ft_a: int, reb_off: int, reb_def: int,
    stl: int, ast: int, blk: int, pf: int, tov: int
) -> float:
    """Calculate Game Score (John Hollinger's formula).

    Game Score = PTS + 0.4*FGM - 0.7*FGA - 0.4*(FTA-FTM) + 0.7*ORB + 0.3*DRB + STL + 0.7*AST + 0.7*BLK - 0.4*PF - TOV
    """
    fgm = fg_2p_m + fg_3p_m
    fga = fg_2p_a + fg_3p_a
    return (
        pts
        + 0.4 * fgm
        - 0.7 * fga
        - 0.4 * (ft_a - ft_m)
        + 0.7 * reb_off
        + 0.3 * reb_def
        + stl
        + 0.7 * ast
        + 0.7 * blk
        - 0.4 * pf
        - tov
    )


def get_game_numbers_from_schedule(season_code: str = "046", year_month: str = None) -> list[int]:
    """Get list of game numbers from the schedule page."""
    game_numbers = set()

    # Scrape multiple months
    months = [year_month] if year_month else ["202511", "202512", "202601", "202602", "202603"]

    for ym in months:
        try:
            params = {"ym": ym}
            resp = requests.get(SCHEDULE_URL, params=params, headers=HEADERS, timeout=REQUEST_TIMEOUT)
            resp.raise_for_status()

            # Find game links
            links = re.findall(r"game_no=(\d+)", resp.text)
            for game_no in links:
                game_numbers.add(int(game_no))

            time.sleep(REQUEST_DELAY)
        except requests.RequestException as e:
            print(f"  Warning: Failed to fetch schedule for {ym}: {e}")

    return sorted(game_numbers)


def scrape_game_result(season_code: str, game_type: str, game_no: int, ym: str = None) -> Optional[GameInfo]:
    """Scrape game result from WKBL."""
    params = {
        "season_gu": season_code,
        "gun": "1",
        "game_type": game_type,
        "game_no": str(game_no),
        "viewType": "2",
    }
    if ym:
        params["ym"] = ym

    try:
        resp = requests.get(GAME_RESULT_URL, params=params, headers=HEADERS, timeout=REQUEST_TIMEOUT)
        resp.raise_for_status()
        resp.encoding = "utf-8"

        soup = BeautifulSoup(resp.text, "html.parser")

        # Note: "경기 결과 데이터가 없습니다" appears in commented JS, not actual content
        # We check for actual game data elements instead

        # Parse game info
        info_elem = soup.select_one("p.info_game")
        if not info_elem:
            return None

        spans = info_elem.find_all("span")
        date_str = spans[0].get_text(strip=True) if len(spans) > 0 else None
        stadium = spans[2].get_text(strip=True) if len(spans) > 2 else None

        # Parse teams and scores
        away_team_elem = soup.select_one(".info_team.away")
        home_team_elem = soup.select_one(".info_team.home")

        if not away_team_elem or not home_team_elem:
            return None

        away_name = away_team_elem.select_one(".team_name").get_text(strip=True)
        home_name = home_team_elem.select_one(".team_name").get_text(strip=True)
        away_score = int(away_team_elem.select_one(".txt_score").get_text(strip=True))
        home_score = int(home_team_elem.select_one(".txt_score").get_text(strip=True))

        # Map team names to codes
        away_code = TEAM_NAME_TO_CODE.get(away_name, away_name[:2])
        home_code = TEAM_NAME_TO_CODE.get(home_name, home_name[:2])

        game_id = f"{season_code}-{game_type}-{game_no}"

        # Parse quarter scores from the score table
        q1_home, q2_home, q3_home, q4_home, ot_home = None, None, None, None, 0
        q1_away, q2_away, q3_away, q4_away, ot_away = None, None, None, None, 0

        # Find the quarter score table (contains 1Q, 2Q, 3Q, 4Q headers)
        score_tables = soup.find_all("table")
        for table in score_tables:
            headers = [th.get_text(strip=True) for th in table.find_all("th")]
            if "1Q" in headers and "2Q" in headers:
                rows = table.find_all("tr")
                for row in rows:
                    cells = row.find_all("td")
                    if len(cells) >= 6:  # Team name, 1Q, 2Q, 3Q, 4Q, EQ, TOTAL
                        team_cell = cells[0].get_text(strip=True)
                        try:
                            q1 = int(cells[1].get_text(strip=True) or 0)
                            q2 = int(cells[2].get_text(strip=True) or 0)
                            q3 = int(cells[3].get_text(strip=True) or 0)
                            q4 = int(cells[4].get_text(strip=True) or 0)
                            ot = int(cells[5].get_text(strip=True) or 0) if len(cells) > 5 else 0

                            # Match team name to determine home/away
                            # Normalize for comparison (handles "BNK 썸" vs "BNK썸")
                            normalized_cell = normalize_team_name(team_cell)
                            normalized_away = normalize_team_name(away_name)
                            normalized_home = normalize_team_name(home_name)
                            if normalized_away in normalized_cell:
                                q1_away, q2_away, q3_away, q4_away, ot_away = q1, q2, q3, q4, ot
                            elif normalized_home in normalized_cell:
                                q1_home, q2_home, q3_home, q4_home, ot_home = q1, q2, q3, q4, ot
                        except (ValueError, IndexError):
                            continue
                break  # Found the quarter table

        return GameInfo(
            game_id=game_id,
            season_code=season_code,
            game_type=game_type,
            game_no=game_no,
            game_date=parse_date(date_str),
            home_team_code=home_code,
            away_team_code=away_code,
            home_score=home_score,
            away_score=away_score,
            stadium=stadium,
            q1_home=q1_home,
            q2_home=q2_home,
            q3_home=q3_home,
            q4_home=q4_home,
            ot_home=ot_home,
            q1_away=q1_away,
            q2_away=q2_away,
            q3_away=q3_away,
            q4_away=q4_away,
            ot_away=ot_away,
        )

    except requests.RequestException as e:
        print(f"  Error fetching game {game_no}: {e}")
        return None
    except Exception as e:
        print(f"  Error parsing game {game_no}: {e}")
        return None


def scrape_player_stats(season_code: str, game_type: str, game_no: int, ym: str = None) -> list[PlayerGameStats]:
    """Scrape player statistics from WKBL AJAX endpoint."""
    data = {
        "season_gu": season_code,
        "game_type": game_type,
        "game_no": str(game_no),
    }
    if ym:
        data["ym"] = ym

    stats = []

    try:
        resp = requests.post(PLAYER_STATS_URL, data=data, headers=HEADERS, timeout=REQUEST_TIMEOUT)
        resp.raise_for_status()
        resp.encoding = "utf-8"

        soup = BeautifulSoup(resp.text, "html.parser")

        # Find player stats tables (usually 2: home and away)
        tables = soup.find_all("table")

        game_id = f"{season_code}-{game_type}-{game_no}"

        # First, figure out which team is home and which is away from game result
        game_info = scrape_game_result(season_code, game_type, game_no, ym)
        if not game_info:
            return []

        # Parse stats tables
        for table_idx, table in enumerate(tables):
            # Skip the first comparison table
            headers = [th.get_text(strip=True) for th in table.find_all("th")]
            if "2PM-A" not in headers and "선수" not in headers:
                continue

            # Determine team code based on table position
            # Table 1 = away team, Table 2 = home team (usually)
            if table_idx == 1:
                team_code = game_info.away_team_code
            else:
                team_code = game_info.home_team_code

            rows = table.find_all("tr")
            for row in rows:
                cells = row.find_all("td")
                if len(cells) < 12:
                    continue

                # Parse player row
                try:
                    player_name = cells[0].get_text(strip=True)
                    if not player_name or player_name in ["TEAM", "팀", "합계", "TOTAL"]:
                        continue

                    player_id = None
                    player_link = cells[0].find("a")
                    if player_link:
                        href = player_link.get("href", "")
                        match = re.search(r"pno=(\d+)", href)
                        if match:
                            player_id = match.group(1)

                    position = cells[1].get_text(strip=True)
                    min_str = cells[2].get_text(strip=True)

                    fg_2p_m, fg_2p_a = parse_shot_stats(cells[3].get_text(strip=True))
                    fg_3p_m, fg_3p_a = parse_shot_stats(cells[4].get_text(strip=True))
                    ft_m, ft_a = parse_shot_stats(cells[5].get_text(strip=True))

                    # Rebounds: OFF (6), DEF (7), TOT (8) - but layout varies
                    # Check if cells 6-8 are rebound columns
                    reb_off = int(cells[6].get_text(strip=True) or 0)
                    reb_def = int(cells[7].get_text(strip=True) or 0)
                    reb_tot = int(cells[8].get_text(strip=True) or 0)

                    ast = int(cells[9].get_text(strip=True) or 0)
                    pf = int(cells[10].get_text(strip=True) or 0)
                    stl = int(cells[11].get_text(strip=True) or 0)
                    tov = int(cells[12].get_text(strip=True) or 0)
                    blk = int(cells[13].get_text(strip=True) or 0)
                    pts = int(cells[14].get_text(strip=True) or 0)

                    # Calculate advanced stats
                    ts_pct = calculate_ts_pct(pts, fg_2p_a, fg_3p_a, ft_a)
                    efg_pct = calculate_efg_pct(fg_2p_m, fg_3p_m, fg_2p_a, fg_3p_a)
                    game_score = calculate_game_score(
                        pts, fg_2p_m, fg_3p_m, fg_2p_a, fg_3p_a,
                        ft_m, ft_a, reb_off, reb_def,
                        stl, ast, blk, pf, tov
                    )

                    stat = PlayerGameStats(
                        game_id=game_id,
                        team_code=team_code,
                        player_name=player_name,
                        player_id=player_id,
                        position=position,
                        min_seconds=parse_minutes(min_str),
                        fg_2p_m=fg_2p_m,
                        fg_2p_a=fg_2p_a,
                        fg_3p_m=fg_3p_m,
                        fg_3p_a=fg_3p_a,
                        ft_m=ft_m,
                        ft_a=ft_a,
                        reb_off=reb_off,
                        reb_def=reb_def,
                        reb_tot=reb_tot,
                        ast=ast,
                        pf=pf,
                        stl=stl,
                        tov=tov,
                        blk=blk,
                        pts=pts,
                        ts_pct=ts_pct,
                        efg_pct=efg_pct,
                        game_score=game_score,
                    )
                    stats.append(stat)

                except (ValueError, IndexError) as e:
                    continue

        return stats

    except requests.RequestException as e:
        print(f"  Error fetching player stats for game {game_no}: {e}")
        return []
    except Exception as e:
        print(f"  Error parsing player stats for game {game_no}: {e}")
        return []


def get_player_id_by_name(conn: sqlite3.Connection, player_name: str) -> Optional[str]:
    """Look up player_id by name."""
    cursor = conn.cursor()
    cursor.execute(
        "SELECT player_id FROM players WHERE player_name = ? ORDER BY player_id",
        (player_name,),
    )
    rows = cursor.fetchall()
    if not rows:
        return None
    if len(rows) > 1:
        return None
    return rows[0][0]


def get_existing_game_ids(conn: sqlite3.Connection) -> set[str]:
    """Get set of existing game IDs."""
    cursor = conn.cursor()
    cursor.execute("SELECT game_id FROM games WHERE season_code = ?", (SEASON_CODE,))
    return {row[0] for row in cursor.fetchall()}


def save_game_to_db(conn: sqlite3.Connection, game: GameInfo) -> bool:
    """Save game info to database."""
    cursor = conn.cursor()
    try:
        cursor.execute("""
            INSERT OR REPLACE INTO games
            (game_id, season_code, game_type, game_no, game_date,
             home_team_code, away_team_code, home_score, away_score, stadium,
             q1_home, q2_home, q3_home, q4_home, ot_home,
             q1_away, q2_away, q3_away, q4_away, ot_away)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            game.game_id, game.season_code, game.game_type, game.game_no,
            game.game_date, game.home_team_code, game.away_team_code,
            game.home_score, game.away_score, game.stadium,
            game.q1_home, game.q2_home, game.q3_home, game.q4_home, game.ot_home,
            game.q1_away, game.q2_away, game.q3_away, game.q4_away, game.ot_away
        ))
        return True
    except sqlite3.Error as e:
        print(f"  DB error saving game {game.game_id}: {e}")
        return False


def save_player_stats_to_db(conn: sqlite3.Connection, stats: list[PlayerGameStats]) -> int:
    """Save player stats to database. Returns number of rows inserted."""
    cursor = conn.cursor()
    inserted = 0

    for stat in stats:
        player_id = stat.player_id or get_player_id_by_name(conn, stat.player_name)
        if not player_id:
            cursor.execute(
                "SELECT COUNT(*) FROM players WHERE player_name = ?",
                (stat.player_name,),
            )
            name_count = cursor.fetchone()[0]
            if name_count > 1:
                print(f"    Warning: Ambiguous player name '{stat.player_name}' (multiple IDs). Skipping.")
            else:
                print(f"    Warning: Player '{stat.player_name}' not found in database. Skipping.")
            continue

        try:
            cursor.execute(
                "INSERT OR IGNORE INTO players (player_id, player_name, position) VALUES (?, ?, ?)",
                (player_id, stat.player_name, stat.position or None),
            )
            cursor.execute(
                "UPDATE players SET position = COALESCE(position, ?) WHERE player_id = ?",
                (stat.position or None, player_id),
            )

            # If we previously wrote this same stat line under a different ID (name collision),
            # remove the duplicate to keep boxscores and score fallbacks correct.
            cursor.execute(
                """
                DELETE FROM game_stats
                WHERE game_id = ?
                  AND team_code = ?
                  AND player_id IN (
                    SELECT player_id
                    FROM players
                    WHERE player_name = ?
                      AND player_id != ?
                  )
                  AND min_seconds = ?
                  AND pts = ?
                  AND reb_off = ?
                  AND reb_def = ?
                  AND reb_tot = ?
                  AND ast = ?
                  AND stl = ?
                  AND blk = ?
                  AND tov = ?
                  AND pf = ?
                  AND fg_2p_m = ?
                  AND fg_2p_a = ?
                  AND fg_3p_m = ?
                  AND fg_3p_a = ?
                  AND ft_m = ?
                  AND ft_a = ?
                """,
                (
                    stat.game_id,
                    stat.team_code,
                    stat.player_name,
                    player_id,
                    stat.min_seconds,
                    stat.pts,
                    stat.reb_off,
                    stat.reb_def,
                    stat.reb_tot,
                    stat.ast,
                    stat.stl,
                    stat.blk,
                    stat.tov,
                    stat.pf,
                    stat.fg_2p_m,
                    stat.fg_2p_a,
                    stat.fg_3p_m,
                    stat.fg_3p_a,
                    stat.ft_m,
                    stat.ft_a,
                ),
            )

            cursor.execute("""
                INSERT OR REPLACE INTO game_stats
                (game_id, team_code, player_id, min_seconds,
                 fg_2p_m, fg_2p_a, fg_3p_m, fg_3p_a, ft_m, ft_a,
                 reb_off, reb_def, reb_tot, ast, pf, stl, tov, blk, pts,
                 ts_pct, efg_pct, game_score)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                stat.game_id, stat.team_code, player_id, stat.min_seconds,
                stat.fg_2p_m, stat.fg_2p_a, stat.fg_3p_m, stat.fg_3p_a,
                stat.ft_m, stat.ft_a,
                stat.reb_off, stat.reb_def, stat.reb_tot,
                stat.ast, stat.pf, stat.stl, stat.tov, stat.blk, stat.pts,
                stat.ts_pct, stat.efg_pct, stat.game_score
            ))
            inserted += 1
        except sqlite3.Error as e:
            print(f"    DB error saving stats for {stat.player_name}: {e}")

    return inserted


def scrape_all_games(
    game_numbers: list[int],
    season_code: str = "046",
    game_type: str = "01",
    limit: int = 0,
    verbose: bool = True,
) -> tuple[list[GameInfo], list[PlayerGameStats]]:
    """Scrape all games."""
    all_games = []
    all_stats = []

    total = len(game_numbers) if limit == 0 else min(limit, len(game_numbers))

    for i, game_no in enumerate(game_numbers[:total]):
        if verbose:
            print(f"[{i+1}/{total}] Scraping game #{game_no}...", end=" ")

        # Scrape game result
        game = scrape_game_result(season_code, game_type, game_no)
        if game:
            all_games.append(game)

            if verbose:
                print(f"OK ({game.away_team_code} {game.away_score} @ {game.home_team_code} {game.home_score}, {game.game_date})")

            # Scrape player stats
            time.sleep(REQUEST_DELAY)
            stats = scrape_player_stats(season_code, game_type, game_no)
            all_stats.extend(stats)

            if verbose:
                print(f"      Found {len(stats)} player stat entries")
        else:
            if verbose:
                print("NOT FOUND")

        # Rate limiting
        if i < total - 1:
            time.sleep(REQUEST_DELAY)

    return all_games, all_stats


def main():
    parser = argparse.ArgumentParser(description="Scrape WKBL game results and player stats")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of games to scrape (0=all)")
    parser.add_argument("--dry-run", action="store_true", help="Don't update the database")
    parser.add_argument("--game-no", type=int, help="Scrape a specific game number")
    parser.add_argument("--start-from", type=int, default=1, help="Start from this game number")
    parser.add_argument("--end-at", type=int, default=999, help="End at this game number")
    parser.add_argument("--output", type=Path, default=OUTPUT_JSON, help="Output JSON file")
    parser.add_argument("--quiet", "-q", action="store_true", help="Minimal output")
    args = parser.parse_args()

    print("=" * 60)
    print("WKBL Game Scraper - Season 2025-26")
    print("=" * 60)

    # Connect to database to check existing games
    conn = sqlite3.connect(DB_PATH)
    existing_ids = get_existing_game_ids(conn)
    print(f"Existing games in DB: {len(existing_ids)}")

    # Determine which games to scrape
    if args.game_no:
        game_numbers = [args.game_no]
        print(f"Scraping specific game: #{args.game_no}")
    else:
        print("Fetching game numbers from schedule...")
        all_game_numbers = get_game_numbers_from_schedule()
        print(f"Found {len(all_game_numbers)} games on schedule")

        # Filter by range
        game_numbers = [g for g in all_game_numbers if args.start_from <= g <= args.end_at]

        # Filter out already-scraped games (check if we have stats for them)
        cursor = conn.cursor()
        cursor.execute("SELECT DISTINCT game_id FROM game_stats")
        games_with_stats = {row[0] for row in cursor.fetchall()}

        new_games = []
        for g in game_numbers:
            game_id = f"046-01-{g}"
            if game_id not in games_with_stats:
                new_games.append(g)

        print(f"New games to scrape: {len(new_games)}")
        game_numbers = new_games

    if not game_numbers:
        print("No new games to scrape!")
        conn.close()
        return

    if args.limit:
        print(f"Limiting to {args.limit} games")

    print()
    print("Starting scrape (1 second delay between requests)...")
    print("-" * 60)

    # Scrape games
    games, stats = scrape_all_games(
        game_numbers,
        limit=args.limit,
        verbose=not args.quiet
    )

    print("-" * 60)

    # Statistics
    print(f"\nResults:")
    print(f"  Games scraped: {len(games)}")
    print(f"  Player stat entries: {len(stats)}")

    if games:
        dates = [g.game_date for g in games if g.game_date]
        if dates:
            print(f"  Date range: {min(dates)} to {max(dates)}")

    # Save to JSON
    args.output.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "season_code": SEASON_CODE,
        "games": [g.__dict__ for g in games],
        "player_stats": [s.__dict__ for s in stats],
    }
    args.output.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"\nSaved JSON to: {args.output}")

    # Save to database
    if not args.dry_run:
        games_saved = 0
        stats_saved = 0

        for game in games:
            if save_game_to_db(conn, game):
                games_saved += 1

        stats_saved = save_player_stats_to_db(conn, stats)

        conn.commit()
        print(f"\nSaved to database:")
        print(f"  Games: {games_saved}")
        print(f"  Player stats: {stats_saved}")
    else:
        print("\nDRY RUN - Database not modified")

    conn.close()
    print("\nDone!")


if __name__ == "__main__":
    main()
