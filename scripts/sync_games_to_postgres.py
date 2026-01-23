#!/usr/bin/env python3
"""
Sync WKBL game schedules to PostgreSQL.
Fetches game list from WKBL website and inserts into games table.
"""

import argparse
import re
import time
import psycopg2
import requests
from bs4 import BeautifulSoup

BASE_URL = "https://www.wkbl.or.kr"
SCHEDULE_URL = f"{BASE_URL}/game/sch/schedule.asp"

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
}

def get_season_years(season_code):
    """Get start and end year for a season.
    Season 046 = 2025-2026, so year = 1979 + season_code
    """
    season_num = int(season_code)
    start_year = 1979 + season_num
    end_year = start_year + 1
    return start_year, end_year

def fetch_games_for_season(session, season_code):
    """Fetch all games for a season from WKBL website."""
    games = []
    seen = set()
    
    start_year, end_year = get_season_years(season_code)
    print(f"  Season {season_code} = {start_year}-{end_year}")
    
    # WKBL 시즌은 보통 10월~4월
    # 시작년도 10,11,12월 + 종료년도 1,2,3,4월
    months_to_try = []
    for month in [10, 11, 12]:
        months_to_try.append((start_year, month))
    for month in [1, 2, 3, 4, 5]:
        months_to_try.append((end_year, month))
    
    for year, month in months_to_try:
        ym = f"{year}{month:02d}"
        url = f"{SCHEDULE_URL}?season_gu={season_code}&game_type=01&ym={ym}&viewType=1"
        
        try:
            resp = session.get(url, headers=HEADERS, timeout=15)
            resp.encoding = 'euc-kr'
            html = resp.text
            
            # Parse game links
            soup = BeautifulSoup(html, "html.parser")
            links = soup.find_all("a", href=True)
            
            for link in links:
                href = link.get("href", "")
                if "game_no=" in href and "result.asp" in href:
                    try:
                        qs = href.split('?')[1]
                        params = {}
                        for part in qs.split('&'):
                            if '=' in part:
                                k, v = part.split('=', 1)
                                params[k] = v
                        
                        game_no = params.get('game_no')
                        game_type = params.get('game_type', '01')
                        
                        if game_no:
                            game_id = f"{season_code}-{game_type}-{game_no}"
                            if game_id not in seen:
                                seen.add(game_id)
                                games.append({
                                    'game_id': game_id,
                                    'season_code': season_code,
                                    'game_type': game_type,
                                    'game_no': int(game_no),
                                    'ym': ym
                                })
                    except Exception:
                        continue
                        
            time.sleep(0.1)
        except Exception as e:
            print(f"  Warning: {ym} failed: {e}")
            continue
    
    return games

def ensure_season(conn, season_code):
    """Ensure season exists in seasons table."""
    start_year, end_year = get_season_years(season_code)
    season_name = f"{start_year}-{end_year}"
    
    cursor = conn.cursor()
    cursor.execute("""
        INSERT INTO seasons (season_code, season_name)
        VALUES (%s, %s)
        ON CONFLICT (season_code) DO NOTHING
    """, (season_code, season_name))
    conn.commit()

def insert_games(conn, games):
    """Insert games into PostgreSQL."""
    cursor = conn.cursor()
    inserted = 0
    
    for game in games:
        try:
            cursor.execute("""
                INSERT INTO games (game_id, season_code, game_type, game_no)
                VALUES (%s, %s, %s, %s)
                ON CONFLICT (game_id) DO NOTHING
            """, (
                game['game_id'],
                game['season_code'],
                game['game_type'],
                game['game_no']
            ))
            if cursor.rowcount > 0:
                inserted += 1
        except Exception as e:
            print(f"  Error inserting {game['game_id']}: {e}")
    
    conn.commit()
    return inserted

def main():
    parser = argparse.ArgumentParser(description="Sync WKBL games to PostgreSQL")
    parser.add_argument("--db-url", required=True, help="PostgreSQL connection URL")
    parser.add_argument("--season", required=True, help="Season code (e.g., 035)")
    parser.add_argument("--dry-run", action="store_true", help="Don't insert, just print")
    args = parser.parse_args()
    
    conn = psycopg2.connect(args.db_url)
    session = requests.Session()
    
    print(f"Fetching games for season {args.season}...")
    games = fetch_games_for_season(session, args.season)
    print(f"Found {len(games)} games")
    
    if args.dry_run:
        for g in games[:10]:
            print(f"  {g['game_id']}")
        if len(games) > 10:
            print(f"  ... and {len(games) - 10} more")
    else:
        ensure_season(conn, args.season)
        inserted = insert_games(conn, games)
        print(f"Inserted {inserted} new games")
    
    conn.close()

if __name__ == "__main__":
    main()
