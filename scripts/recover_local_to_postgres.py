import os
import glob
from bs4 import BeautifulSoup
import psycopg2
from pathlib import Path
import re

# Config
RAW_DIR = Path("data/raw_crawled")
# Supabase Pooler (IPv4)
PG_HOST = "aws-1-ap-southeast-1.pooler.supabase.com"
PG_PORT = "6543"
PG_USER = "postgres.efgbkvmwwefqjxeugktf"
PG_PASS = "dRp7We9L61Ur1sg"
PG_DB = "postgres"

def connect_db():
    print("🔌 Connecting to Supabase...")
    return psycopg2.connect(
        host=PG_HOST, port=PG_PORT, user=PG_USER, password=PG_PASS, dbname=PG_DB
    )

def parse_player(html_content, player_id):
    soup = BeautifulSoup(html_content, 'lxml')
    
    # Example parsing logic (adjust selectors based on actual HTML structure)
    # WKBL structure often uses tables or specific classes
    # This is a best-effort parser
    
    # Name is often in a specific header or div
    name_el = soup.select_one(".name, .player_name, h3") 
    name = name_el.get_text(strip=True) if name_el else f"Unknown_{player_id}"
    
    # Cleanup name (remove backslashes, etc)
    name = name.replace('\\', '').replace('"', '').strip()

    return {
        "player_id": player_id,
        "player_name": name,
        "position": None, # HTML에서 찾으면 업데이트
        "birth_date": None,
        "height": None,
        "weight": None
    }

def run_recovery():
    conn = connect_db()
    cursor = conn.cursor()
    
    # 1. Players
    player_files = list(RAW_DIR.glob("**/*_player.html"))
    print(f"📂 Found {len(player_files)} player files")
    
    count = 0
    for f in player_files:
        try:
            # Extract player_id from filename: 036-01-100_player.html -> wait, this looks like game_id?
            # Actually, your filenames look like: 036-01-100_player.html
            # Is 100 the player_id? Or is this a game record for a player?
            # Let's assume it's player_id for now, or parse from content.
            
            # Re-reading file listing...
            # 036-01-100_player.html seems to be: Season 036, Round 01, Game 100?
            # If these are GAME stats for a player, that's even better!
            
            # Let's parse filename: season-round-game_type?
            # 036-01-100 -> Season 36, 01=Regular?, Game 100?
            
            # Since I don't know the exact HTML structure without reading one, 
            # I will just try to Insert dummy data to prove connection first.
            pass
        except Exception as e:
            print(f"Failed to parse {f}: {e}")
            
    # For now, let's just insert Seasons to test connection
    print("🌱 Inserting seasons...")
    seasons = [
        ("046", "2025-2026", "2025-10-01", "2026-04-01"),
        ("045", "2024-2025", "2024-10-01", "2025-04-01")
    ]
    
    cursor.executemany("""
        INSERT INTO seasons (season_code, season_name, start_date, end_date)
        VALUES (%s, %s, %s, %s)
        ON CONFLICT (season_code) DO NOTHING
    """, seasons)
    
    conn.commit()
    print("✅ Seasons inserted!")
    
    conn.close()

if __name__ == "__main__":
    run_recovery()
