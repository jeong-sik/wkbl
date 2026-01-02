#!/usr/bin/env python3
"""
Scrape WKBL player profile data (height, weight, birth_date, position, photo).

Fills in missing metadata for the 129 players in our database.

Usage:
    python scripts/scrape_wkbl_players.py
    python scripts/scrape_wkbl_players.py --limit 5  # Test with 5 players
    python scripts/scrape_wkbl_players.py --dry-run  # Don't update DB
"""

from __future__ import annotations

import argparse
import json
import re
import sqlite3
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

import requests
from bs4 import BeautifulSoup

# Paths
ROOT_DIR = Path(__file__).resolve().parents[1]
DB_PATH = ROOT_DIR / "data" / "wkbl.db"
OUTPUT_JSON = ROOT_DIR / "data" / "player_metadata.json"
OUTPUT_SQL = ROOT_DIR / "data" / "player_updates.sql"

# WKBL URLs
PLAYER_DETAIL_URL = "https://www.wkbl.or.kr/player/detail2.asp"
PLAYER_PHOTO_URL = "https://www.wkbl.or.kr/static/images/player/pimg/m_{pno}.jpg"

# Request settings (be respectful to WKBL servers)
REQUEST_DELAY = 1.0  # seconds between requests
REQUEST_TIMEOUT = 15
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7",
}

# Position mapping (Korean to standardized)
POSITION_MAP = {
    "가드": "G",
    "포워드": "F",
    "센터": "C",
    "포인트가드": "PG",
    "슈팅가드": "SG",
    "스몰포워드": "SF",
    "파워포워드": "PF",
    "G": "G",
    "F": "F",
    "C": "C",
    "PG": "PG",
    "SG": "SG",
    "SF": "SF",
    "PF": "PF",
}


def get_players_from_db() -> list[dict]:
    """Get all players from the database."""
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    cursor.execute("""
        SELECT player_id, player_name, position, birth_date, height, weight
        FROM players
        ORDER BY player_name
    """)
    players = [dict(row) for row in cursor.fetchall()]
    conn.close()
    return players


def parse_height(text: str) -> Optional[int]:
    """Parse height from text like '178cm' or '178'."""
    if not text:
        return None
    match = re.search(r"(\d{3})", text)
    if match:
        return int(match.group(1))
    return None


def parse_weight(text: str) -> Optional[int]:
    """Parse weight from text like '68kg' or '68'."""
    if not text:
        return None
    match = re.search(r"(\d{2,3})", text)
    if match:
        weight = int(match.group(1))
        if 40 <= weight <= 150:  # Sanity check
            return weight
    return None


def parse_birth_date(text: str) -> Optional[str]:
    """Parse birth date from various formats to YYYY-MM-DD."""
    if not text:
        return None

    # Try YYYY.MM.DD or YYYY-MM-DD format
    match = re.search(r"(\d{4})[.\-/](\d{1,2})[.\-/](\d{1,2})", text)
    if match:
        year, month, day = match.groups()
        return f"{year}-{int(month):02d}-{int(day):02d}"

    # Try YYYY년 MM월 DD일 format
    match = re.search(r"(\d{4})년\s*(\d{1,2})월\s*(\d{1,2})일", text)
    if match:
        year, month, day = match.groups()
        return f"{year}-{int(month):02d}-{int(day):02d}"

    return None


def parse_position(text: str) -> Optional[str]:
    """Parse and normalize position."""
    if not text:
        return None
    text = text.strip()
    return POSITION_MAP.get(text, text[:2] if len(text) <= 3 else None)


def scrape_player_profile(player_id: str, player_group: str = "11") -> dict:
    """
    Scrape a single player's profile from WKBL.

    Returns dict with: height, weight, birth_date, position, photo_url, raw_html
    """
    result = {
        "player_id": player_id,
        "height": None,
        "weight": None,
        "birth_date": None,
        "position": None,
        "photo_url": PLAYER_PHOTO_URL.format(pno=player_id),
        "scraped_at": datetime.now(timezone.utc).isoformat(),
        "success": False,
        "error": None,
    }

    try:
        # Try different player groups (11=retired/alumni, 12=active, F11=foreign)
        for group in [player_group, "12", "11", "F11"]:
            params = {
                "player_group": group,
                "tcode": "",
                "pno": player_id,
            }

            resp = requests.get(
                PLAYER_DETAIL_URL,
                params=params,
                headers=HEADERS,
                timeout=REQUEST_TIMEOUT,
            )
            resp.raise_for_status()
            resp.encoding = "utf-8"

            soup = BeautifulSoup(resp.text, "html.parser")

            # Check if we got a valid player page (look for player info section)
            player_info = soup.select_one(".player_info, .info_player, .detail_player, .player_detail")
            if not player_info:
                # Try alternative selectors
                player_info = soup.select_one("div[class*='player'], table[class*='player']")

            if player_info or "선수정보" in resp.text or player_id in resp.text:
                # Found the player, parse the data from <li> elements
                # Structure: <li><span>포지션</span> - F</li>
                profile_items = soup.select("ul.list_text li")

                for li in profile_items:
                    text = li.get_text(strip=True)

                    # Extract position (format: "포지션 - F")
                    if "포지션" in text:
                        match = re.search(r"포지션\s*[-–]\s*([A-Z가-힣]+)", text)
                        if match:
                            result["position"] = parse_position(match.group(1))

                    # Extract height (format: "신장 - 177 cm")
                    elif "신장" in text:
                        match = re.search(r"신장\s*[-–]\s*(\d{3})\s*cm", text)
                        if match:
                            height = int(match.group(1))
                            if 150 <= height <= 220:
                                result["height"] = height

                    # Extract weight (format: "체중 - 68 kg")
                    elif "체중" in text:
                        match = re.search(r"체중\s*[-–]\s*(\d{2,3})\s*kg", text)
                        if match:
                            weight = int(match.group(1))
                            if 40 <= weight <= 150:
                                result["weight"] = weight

                    # Extract birth date (format: "생년월일 - 1996.07.20")
                    elif "생년월일" in text:
                        match = re.search(r"생년월일\s*[-–]\s*(\d{4})[.\-/](\d{1,2})[.\-/](\d{1,2})", text)
                        if match:
                            year, month, day = match.groups()
                            result["birth_date"] = f"{year}-{int(month):02d}-{int(day):02d}"

                result["success"] = True
                result["player_group"] = group
                break

        if not result["success"]:
            result["error"] = "Player page not found in any group"

    except requests.RequestException as e:
        result["error"] = f"Request failed: {str(e)}"
    except Exception as e:
        result["error"] = f"Parse error: {str(e)}"

    return result


def scrape_all_players(
    players: list[dict],
    limit: int = 0,
    verbose: bool = True,
) -> list[dict]:
    """Scrape profile data for all players."""
    results = []
    total = len(players) if limit == 0 else min(limit, len(players))

    for i, player in enumerate(players[:total]):
        player_id = player["player_id"]
        player_name = player["player_name"]

        if verbose:
            print(f"[{i+1}/{total}] Scraping {player_name} ({player_id})...", end=" ")

        result = scrape_player_profile(player_id)
        result["player_name"] = player_name
        results.append(result)

        if verbose:
            if result["success"]:
                h = result.get("height") or "?"
                w = result.get("weight") or "?"
                p = result.get("position") or "?"
                b = result.get("birth_date") or "?"
                print(f"OK (H:{h}, W:{w}, P:{p}, B:{b})")
            else:
                print(f"FAILED: {result.get('error', 'Unknown')}")

        # Be nice to WKBL servers
        if i < total - 1:
            time.sleep(REQUEST_DELAY)

    return results


def generate_sql_updates(results: list[dict]) -> str:
    """Generate SQL UPDATE statements for players with scraped data."""
    lines = [
        "-- WKBL Player Metadata Updates",
        f"-- Generated: {datetime.now(timezone.utc).isoformat()}",
        f"-- Total players: {len(results)}",
        "",
        "BEGIN TRANSACTION;",
        "",
    ]

    updated = 0
    for r in results:
        if not r["success"]:
            continue

        updates = []
        if r.get("height"):
            updates.append(f"height = {r['height']}")
        if r.get("weight"):
            updates.append(f"weight = {r['weight']}")
        if r.get("position"):
            updates.append(f"position = '{r['position']}'")
        if r.get("birth_date"):
            updates.append(f"birth_date = '{r['birth_date']}'")

        if updates:
            sql = f"UPDATE players SET {', '.join(updates)} WHERE player_id = '{r['player_id']}';"
            lines.append(f"-- {r.get('player_name', r['player_id'])}")
            lines.append(sql)
            lines.append("")
            updated += 1

    lines.append("COMMIT;")
    lines.append("")
    lines.append(f"-- Updated {updated} players")

    return "\n".join(lines)


def apply_updates_to_db(results: list[dict], dry_run: bool = False) -> int:
    """Apply scraped data to the database."""
    if dry_run:
        print("DRY RUN - No database changes will be made")
        return 0

    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    updated = 0

    for r in results:
        if not r["success"]:
            continue

        updates = []
        values = []

        if r.get("height"):
            updates.append("height = ?")
            values.append(r["height"])
        if r.get("weight"):
            updates.append("weight = ?")
            values.append(r["weight"])
        if r.get("position"):
            updates.append("position = ?")
            values.append(r["position"])
        if r.get("birth_date"):
            updates.append("birth_date = ?")
            values.append(r["birth_date"])

        if updates:
            values.append(r["player_id"])
            sql = f"UPDATE players SET {', '.join(updates)} WHERE player_id = ?"
            cursor.execute(sql, values)
            updated += 1

    conn.commit()
    conn.close()
    return updated


def main():
    parser = argparse.ArgumentParser(description="Scrape WKBL player profile data")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of players to scrape (0=all)")
    parser.add_argument("--dry-run", action="store_true", help="Don't update the database")
    parser.add_argument("--output", type=Path, default=OUTPUT_JSON, help="Output JSON file")
    parser.add_argument("--sql-output", type=Path, default=OUTPUT_SQL, help="Output SQL file")
    parser.add_argument("--quiet", "-q", action="store_true", help="Minimal output")
    args = parser.parse_args()

    print("=" * 60)
    print("WKBL Player Profile Scraper")
    print("=" * 60)

    # Get players from database
    players = get_players_from_db()
    print(f"Found {len(players)} players in database")

    # Check how many are missing data
    missing = sum(1 for p in players if not p.get("height"))
    print(f"Players missing metadata: {missing}")

    if args.limit:
        print(f"Limiting to {args.limit} players")

    print()
    print("Starting scrape (1 second delay between requests)...")
    print("-" * 60)

    # Scrape player profiles
    results = scrape_all_players(players, limit=args.limit, verbose=not args.quiet)

    print("-" * 60)

    # Statistics
    success = sum(1 for r in results if r["success"])
    with_height = sum(1 for r in results if r.get("height"))
    with_weight = sum(1 for r in results if r.get("weight"))
    with_position = sum(1 for r in results if r.get("position"))
    with_birth = sum(1 for r in results if r.get("birth_date"))

    print(f"\nResults:")
    print(f"  Total scraped: {len(results)}")
    print(f"  Successful: {success}")
    print(f"  With height: {with_height}")
    print(f"  With weight: {with_weight}")
    print(f"  With position: {with_position}")
    print(f"  With birth_date: {with_birth}")

    # Save JSON output
    args.output.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "total_players": len(results),
        "successful": success,
        "stats": {
            "with_height": with_height,
            "with_weight": with_weight,
            "with_position": with_position,
            "with_birth_date": with_birth,
        },
        "players": results,
    }
    args.output.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"\nSaved JSON to: {args.output}")

    # Save SQL updates
    sql = generate_sql_updates(results)
    args.sql_output.write_text(sql, encoding="utf-8")
    print(f"Saved SQL to: {args.sql_output}")

    # Apply to database
    if not args.dry_run:
        updated = apply_updates_to_db(results)
        print(f"\nUpdated {updated} players in database")
    else:
        print("\nDRY RUN - Database not modified")

    print("\nDone!")


if __name__ == "__main__":
    main()
