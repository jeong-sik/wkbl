#!/usr/bin/env python3
"""
Backfill quarter scores for existing games.

Usage:
    python scripts/backfill_quarter_scores.py           # Backfill all games
    python scripts/backfill_quarter_scores.py --limit 5 # Test with 5 games
    python scripts/backfill_quarter_scores.py --dry-run # Don't update DB
"""

from __future__ import annotations

import argparse
import sqlite3
import time
from pathlib import Path

import requests
from bs4 import BeautifulSoup

# Paths
ROOT_DIR = Path(__file__).resolve().parents[1]
DB_PATH = ROOT_DIR / "data" / "wkbl.db"

# WKBL URLs
GAME_RESULT_URL = "https://www.wkbl.or.kr/game/result.asp"

# Request settings
REQUEST_DELAY = 1.0
REQUEST_TIMEOUT = 15
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7",
}

# Team code to name mapping (normalized without spaces)
TEAM_CODE_TO_NAME = {
    "01": "KB스타즈",
    "03": "삼성생명",
    "05": "우리은행",
    "07": "신한은행",
    "09": "하나은행",
    "11": "BNK썸",
}


def normalize_team_name(name: str) -> str:
    """Normalize team name by removing spaces for comparison."""
    return name.replace(" ", "")


def fetch_quarter_scores(season_code: str, game_type: str, game_no: int, home_name: str, away_name: str) -> dict:
    """Fetch quarter scores from WKBL page."""
    params = {
        "season_gu": season_code,
        "gun": "1",
        "game_type": game_type,
        "game_no": str(game_no),
        "viewType": "2",
    }

    try:
        resp = requests.get(GAME_RESULT_URL, params=params, headers=HEADERS, timeout=REQUEST_TIMEOUT)
        resp.raise_for_status()
        resp.encoding = "utf-8"

        soup = BeautifulSoup(resp.text, "html.parser")

        result = {
            "q1_home": None, "q2_home": None, "q3_home": None, "q4_home": None, "ot_home": 0,
            "q1_away": None, "q2_away": None, "q3_away": None, "q4_away": None, "ot_away": 0,
        }

        # Find the quarter score table
        score_tables = soup.find_all("table")
        for table in score_tables:
            headers = [th.get_text(strip=True) for th in table.find_all("th")]
            if "1Q" in headers and "2Q" in headers:
                rows = table.find_all("tr")
                for row in rows:
                    cells = row.find_all("td")
                    if len(cells) >= 6:
                        team_cell = cells[0].get_text(strip=True)
                        try:
                            q1 = int(cells[1].get_text(strip=True) or 0)
                            q2 = int(cells[2].get_text(strip=True) or 0)
                            q3 = int(cells[3].get_text(strip=True) or 0)
                            q4 = int(cells[4].get_text(strip=True) or 0)
                            ot = int(cells[5].get_text(strip=True) or 0) if len(cells) > 5 else 0

                            # Normalize both for comparison (handles "BNK 썸" vs "BNK썸")
                            normalized_cell = normalize_team_name(team_cell)
                            normalized_away = normalize_team_name(away_name)
                            normalized_home = normalize_team_name(home_name)

                            if normalized_away in normalized_cell:
                                result["q1_away"] = q1
                                result["q2_away"] = q2
                                result["q3_away"] = q3
                                result["q4_away"] = q4
                                result["ot_away"] = ot
                            elif normalized_home in normalized_cell:
                                result["q1_home"] = q1
                                result["q2_home"] = q2
                                result["q3_home"] = q3
                                result["q4_home"] = q4
                                result["ot_home"] = ot
                        except (ValueError, IndexError):
                            continue
                break

        return result

    except Exception as e:
        print(f"  Error: {e}")
        return {}


def main():
    parser = argparse.ArgumentParser(description="Backfill quarter scores for existing games")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of games (0=all)")
    parser.add_argument("--dry-run", action="store_true", help="Don't update DB")
    args = parser.parse_args()

    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    # Find games without quarter scores
    cursor.execute("""
        SELECT game_id, season_code, game_type, game_no, home_team_code, away_team_code
        FROM games
        WHERE q1_home IS NULL AND home_score IS NOT NULL
        ORDER BY game_date DESC
    """)
    games = cursor.fetchall()

    if args.limit:
        games = games[:args.limit]

    print(f"Found {len(games)} games without quarter scores")

    updated = 0
    for i, (game_id, season_code, game_type, game_no, home_code, away_code) in enumerate(games):
        home_name = TEAM_CODE_TO_NAME.get(home_code, home_code)
        away_name = TEAM_CODE_TO_NAME.get(away_code, away_code)

        print(f"[{i+1}/{len(games)}] {game_id}: {away_name} @ {home_name}...", end=" ")

        scores = fetch_quarter_scores(season_code, game_type, game_no, home_name, away_name)

        if scores and scores.get("q1_home") is not None:
            print(f"Q: {scores['q1_home']}-{scores['q2_home']}-{scores['q3_home']}-{scores['q4_home']} vs {scores['q1_away']}-{scores['q2_away']}-{scores['q3_away']}-{scores['q4_away']}")

            if not args.dry_run:
                cursor.execute("""
                    UPDATE games SET
                        q1_home = ?, q2_home = ?, q3_home = ?, q4_home = ?, ot_home = ?,
                        q1_away = ?, q2_away = ?, q3_away = ?, q4_away = ?, ot_away = ?
                    WHERE game_id = ?
                """, (
                    scores["q1_home"], scores["q2_home"], scores["q3_home"], scores["q4_home"], scores["ot_home"],
                    scores["q1_away"], scores["q2_away"], scores["q3_away"], scores["q4_away"], scores["ot_away"],
                    game_id
                ))
            updated += 1
        else:
            print("FAILED")

        time.sleep(REQUEST_DELAY)

    if not args.dry_run:
        conn.commit()

    conn.close()

    print(f"\nUpdated {updated}/{len(games)} games")
    if args.dry_run:
        print("(DRY RUN - DB not modified)")


if __name__ == "__main__":
    main()
