import os
import glob
import re
from pathlib import Path
import psycopg2
from bs4 import BeautifulSoup

# Config
RAW_DIR = Path("data/raw_crawled")
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

def parse_game_id(filename):
    m = re.match(r"(\d{3}-\d{2}-\d+)", filename)
    return m.group(1) if m else None

def parse_player_html(html, game_id, cursor):
    soup = BeautifulSoup(html, 'lxml')
    
    rows = soup.select("table tbody tr")
    for row in rows:
        img = row.select_one("img")
        if not img: continue
        
        src = img.get("src", "")
        m = re.search(r"m_(\d+)\.", src)
        if not m: continue
        player_id = m.group(1)
        
        name_el = row.select_one(".name") or row.select_one("td:nth-of-type(2)")
        player_name = name_el.get_text(strip=True) if name_el else "Unknown"
        
        # Insert Player
        cursor.execute("""
            INSERT INTO players (player_id, player_name)
            VALUES (%s, %s)
            ON CONFLICT (player_id) DO UPDATE SET player_name = EXCLUDED.player_name
        """, (player_id, player_name))

def run():
    try:
        conn = connect_db()
        cursor = conn.cursor()
        
        # Seasons
        seasons = [
            (f"{i:03d}", f"20{13+(i-36)}-20{14+(i-36)}", f"20{13+(i-36)}-10-01", f"20{14+(i-36)}-04-01")
            for i in range(36, 47)
        ]
        cursor.executemany("""
            INSERT INTO seasons (season_code, season_name, start_date, end_date)
            VALUES (%s, %s, %s, %s)
            ON CONFLICT (season_code) DO NOTHING
        """, seasons)
        print("✅ Seasons inserted")

        # Files
        files = sorted(list(RAW_DIR.glob("**/*_player.html")))
        print(f"📂 Found {len(files)} player files. Processing...")
        
        count = 0
        for f in files:
            game_id = parse_game_id(f.name)
            if not game_id: continue
            
            try:
                with open(f, "r", encoding="utf-8") as fd:
                    content = fd.read()
                    parse_player_html(content, game_id, cursor)
                    count += 1
                    if count % 100 == 0: print(f"Processed {count} files...")
            except Exception as e:
                print(f"⚠️ Error parsing {f.name}: {e}")
                
        conn.commit()
        conn.close()
        print(f"🎉 Done! Processed {count} files.")
    except Exception as e:
        print(f"❌ Critical Error: {e}")

if __name__ == "__main__":
    run()
