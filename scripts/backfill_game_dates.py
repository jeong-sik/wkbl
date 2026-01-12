#!/usr/bin/env python3
"""
Backfill game_date for all WKBL games in the database.

Scrapes game dates from WKBL official website and updates the games table.

Usage:
    python scripts/backfill_game_dates.py
    python scripts/backfill_game_dates.py --season 046      # Specific season only
    python scripts/backfill_game_dates.py --dry-run         # Don't update DB
    python scripts/backfill_game_dates.py --limit 10        # Test with 10 games
"""

from __future__ import annotations

import argparse
import os
import re
import sqlite3
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import requests
from bs4 import BeautifulSoup
try:
    import psycopg2
except Exception:  # pragma: no cover - optional dependency for Postgres
    psycopg2 = None

# Paths
ROOT_DIR = Path(__file__).resolve().parents[1]
DB_PATH = ROOT_DIR / "data" / "wkbl.db"

# WKBL URLs
GAME_RESULT_URL = "https://www.wkbl.or.kr/game/result.asp"

# Request settings (be respectful to WKBL servers)
REQUEST_DELAY = 0.5  # seconds between requests
REQUEST_TIMEOUT = 15
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7",
}

# Season year-month mapping for URL parameter
# 각 시즌은 특정 연월에 경기가 진행됨
SEASON_YEAR_MONTHS = {
    "046": ["202510", "202511", "202512", "202601", "202602", "202603"],  # 2025-26
    "045": ["202410", "202411", "202412", "202501", "202502", "202503"],  # 2024-25
    "044": ["202310", "202311", "202312", "202401", "202402", "202403"],  # 2023-24
}


@dataclass
class GameDateResult:
    """Result of scraping a game date."""
    game_id: str
    game_date: Optional[str]
    success: bool
    error: Optional[str] = None


def parse_date(date_str: str) -> Optional[str]:
    """Parse Korean date string like '2025년 12월 29일 월요일' to 'YYYY-MM-DD'."""
    if not date_str:
        return None
    match = re.search(r"(\d{4})년\s*(\d{1,2})월\s*(\d{1,2})일", date_str)
    if match:
        year, month, day = match.groups()
        return f"{year}-{int(month):02d}-{int(day):02d}"
    return None


def scrape_game_date(season_code: str, game_type: str, game_no: int) -> GameDateResult:
    """Scrape game date from WKBL result page."""
    game_id = f"{season_code}-{game_type}-{game_no}"

    # Try different year-months for the season
    year_months = SEASON_YEAR_MONTHS.get(season_code, ["202510"])

    for ym in year_months:
        params = {
            "season_gu": season_code,
            "gun": "1",
            "game_type": game_type,
            "game_no": str(game_no),
            "ym": ym,
            "viewType": "2",
        }

        try:
            resp = requests.get(
                GAME_RESULT_URL,
                params=params,
                headers=HEADERS,
                timeout=REQUEST_TIMEOUT
            )
            resp.raise_for_status()
            resp.encoding = "utf-8"

            soup = BeautifulSoup(resp.text, "html.parser")

            # Method 1: Parse from data-kr attribute
            date_elem = soup.select_one("[data-kr*='년'][data-kr*='월'][data-kr*='일']")
            if date_elem:
                date_str = date_elem.get("data-kr", "")
                parsed_date = parse_date(date_str)
                if parsed_date:
                    return GameDateResult(game_id, parsed_date, True)

            # Method 2: Parse from p.info_game span
            info_elem = soup.select_one("p.info_game")
            if info_elem:
                spans = info_elem.find_all("span")
                if spans:
                    date_str = spans[0].get_text(strip=True)
                    parsed_date = parse_date(date_str)
                    if parsed_date:
                        return GameDateResult(game_id, parsed_date, True)

            # Method 3: Search in entire page
            page_text = soup.get_text()
            match = re.search(r"(\d{4})년\s*(\d{1,2})월\s*(\d{1,2})일", page_text)
            if match:
                year, month, day = match.groups()
                parsed_date = f"{year}-{int(month):02d}-{int(day):02d}"
                return GameDateResult(game_id, parsed_date, True)

        except requests.RequestException as e:
            continue
        except Exception as e:
            continue

    return GameDateResult(game_id, None, False, "Date not found in page")


def get_games_without_dates(conn, season_code: Optional[str] = None, *, is_postgres: bool = False) -> list[tuple]:
    """Get games where game_date is NULL."""
    cursor = conn.cursor()
    param = "%s" if is_postgres else "?"

    if season_code:
        cursor.execute(f"""
            SELECT game_id, season_code, game_type, game_no
            FROM games
            WHERE game_date IS NULL AND season_code = {param}
            ORDER BY season_code DESC, game_no
        """, (season_code,))
    else:
        cursor.execute("""
            SELECT game_id, season_code, game_type, game_no
            FROM games
            WHERE game_date IS NULL
            ORDER BY season_code DESC, game_no
        """)

    return cursor.fetchall()


def update_game_date(conn, game_id: str, game_date: str, *, is_postgres: bool = False) -> bool:
    """Update game_date in the database."""
    cursor = conn.cursor()
    param = "%s" if is_postgres else "?"
    try:
        cursor.execute(
            f"UPDATE games SET game_date = {param} WHERE game_id = {param}",
            (game_date, game_id)
        )
        return cursor.rowcount > 0
    except sqlite3.Error as e:
        print(f"  DB error updating {game_id}: {e}")
        return False
    except Exception as e:
        if is_postgres:
            raise
        print(f"  DB error updating {game_id}: {e}")
        return False


def main():
    parser = argparse.ArgumentParser(description="Backfill game dates from WKBL website")
    parser.add_argument("--season", type=str, help="Specific season code (e.g., 046)")
    parser.add_argument("--dry-run", action="store_true", help="Don't update the database")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of games (0=all)")
    parser.add_argument("--delay", type=float, default=REQUEST_DELAY, help="Delay between requests")
    parser.add_argument(
        "--db-url",
        type=str,
        default=os.environ.get("DATABASE_URL"),
        help="Postgres DATABASE_URL (uses SQLite if omitted)",
    )
    args = parser.parse_args()

    print("=" * 60)
    print("WKBL Game Date Backfill")
    print("=" * 60)

    # Connect to database
    is_postgres = bool(args.db_url)
    def connect_postgres():
        if psycopg2 is None:
            raise SystemExit("psycopg2 is required for Postgres. Install it or omit --db-url.")
        return psycopg2.connect(args.db_url)

    if is_postgres:
        if psycopg2 is None:
            raise SystemExit("psycopg2 is required for Postgres. Install it or omit --db-url.")
        conn = connect_postgres()
    else:
        conn = sqlite3.connect(DB_PATH)

    # Get games without dates
    games = get_games_without_dates(conn, args.season, is_postgres=is_postgres)
    print(f"Games without dates: {len(games)}")

    if args.limit:
        games = games[:args.limit]
        print(f"Limited to: {len(games)} games")

    if not games:
        print("No games to process!")
        conn.close()
        return

    print()
    print("Starting scrape...")
    print("-" * 60)

    # Stats
    success_count = 0
    fail_count = 0

    for i, (game_id, season_code, game_type, game_no) in enumerate(games):
        print(f"[{i+1}/{len(games)}] {game_id}", end=" -> ")

        result = scrape_game_date(season_code, game_type, game_no)

        if result.success and result.game_date:
            print(f"{result.game_date}")

            if not args.dry_run:
                try:
                    if update_game_date(conn, game_id, result.game_date, is_postgres=is_postgres):
                        success_count += 1
                    else:
                        fail_count += 1
                except Exception as e:
                    if not is_postgres:
                        print(f"  DB error updating {game_id}: {e}")
                        fail_count += 1
                    else:
                        print(f"  DB error updating {game_id}: {e} (retry)")
                        try:
                            conn.close()
                        except Exception:
                            pass
                        conn = connect_postgres()
                        if update_game_date(conn, game_id, result.game_date, is_postgres=True):
                            success_count += 1
                        else:
                            fail_count += 1
            else:
                success_count += 1
        else:
            print(f"FAILED ({result.error or 'unknown'})")
            fail_count += 1

        # Rate limiting
        if i < len(games) - 1:
            time.sleep(args.delay)

    # Commit changes
    if not args.dry_run:
        conn.commit()

    print("-" * 60)
    print(f"\nResults:")
    print(f"  Success: {success_count}")
    print(f"  Failed: {fail_count}")

    if args.dry_run:
        print("\nDRY RUN - Database not modified")
    else:
        # Verify
        cursor = conn.cursor()
        cursor.execute("SELECT COUNT(*) FROM games WHERE game_date IS NULL")
        remaining = cursor.fetchone()[0]
        print(f"  Remaining NULL dates: {remaining}")

    conn.close()
    print("\nDone!")


if __name__ == "__main__":
    main()
