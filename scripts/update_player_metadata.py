#!/usr/bin/env python3
"""
Update WKBL player metadata (height, weight, position, birth_date) in PostgreSQL.
Scrapes from WKBL website player profile pages.
"""

import argparse
import re
import time
import psycopg2
import requests
from bs4 import BeautifulSoup
from datetime import datetime

PLAYER_DETAIL_URL = "https://www.wkbl.or.kr/player/detail2.asp"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
}

POSITION_MAP = {
    "가드": "G", "포워드": "F", "센터": "C",
    "포인트가드": "PG", "슈팅가드": "SG", 
    "스몰포워드": "SF", "파워포워드": "PF",
    "G": "G", "F": "F", "C": "C",
}

def parse_height(text):
    """Parse height from text like '178cm' or '178'."""
    if not text:
        return None
    match = re.search(r'(\d{2,3})', str(text))
    return int(match.group(1)) if match else None

def parse_weight(text):
    """Parse weight from text like '65kg' or '65'."""
    if not text:
        return None
    match = re.search(r'(\d{2,3})', str(text))
    return int(match.group(1)) if match else None

def parse_birth_date(text):
    """Parse birth date from text like '1995.03.15' or '1995-03-15'."""
    if not text:
        return None
    # Try different formats
    for fmt in ['%Y.%m.%d', '%Y-%m-%d', '%Y/%m/%d']:
        try:
            return datetime.strptime(text.strip(), fmt).date()
        except ValueError:
            continue
    return None

def scrape_player(session, player_id):
    """Scrape player metadata from WKBL website."""
    url = f"{PLAYER_DETAIL_URL}?pno={player_id}"
    try:
        resp = session.get(url, headers=HEADERS, timeout=15)
        resp.encoding = 'utf-8'  # HTML declares charset=utf-8
        soup = BeautifulSoup(resp.text, 'html.parser')
        
        data = {
            'height': None,
            'weight': None,
            'position': None,
            'birth_date': None
        }
        
        # Find player info section
        # WKBL uses various table/div structures, try multiple selectors
        
        # Method 1: Look for specific labels
        for row in soup.find_all(['tr', 'div', 'li']):
            text = row.get_text(strip=True)
            
            # Height
            if '신장' in text or '키' in text:
                data['height'] = parse_height(text)
            
            # Weight  
            if '체중' in text or '몸무게' in text:
                data['weight'] = parse_weight(text)
            
            # Position
            if '포지션' in text:
                for pos_kr, pos_en in POSITION_MAP.items():
                    if pos_kr in text:
                        data['position'] = pos_en
                        break
            
            # Birth date
            if '생년월일' in text or '생일' in text:
                match = re.search(r'(\d{4}[./-]\d{1,2}[./-]\d{1,2})', text)
                if match:
                    data['birth_date'] = parse_birth_date(match.group(1))
        
        # Method 2: Look for common table structure
        for td in soup.find_all('td'):
            text = td.get_text(strip=True)
            if re.match(r'^\d{3}cm$', text):
                data['height'] = parse_height(text)
            elif re.match(r'^\d{2,3}kg$', text):
                data['weight'] = parse_weight(text)
        
        return data
        
    except Exception as e:
        print(f"  Error scraping {player_id}: {e}")
        return None

def main():
    parser = argparse.ArgumentParser(description="Update WKBL player metadata")
    parser.add_argument("--db-url", required=True, help="PostgreSQL connection URL")
    parser.add_argument("--limit", type=int, default=0, help="Limit players to process")
    parser.add_argument("--dry-run", action="store_true", help="Don't update DB")
    args = parser.parse_args()
    
    conn = psycopg2.connect(args.db_url)
    cursor = conn.cursor()
    session = requests.Session()
    
    # Get players without complete metadata
    cursor.execute("""
        SELECT player_id, player_name
        FROM players
        WHERE height IS NULL OR position IS NULL OR weight IS NULL
        ORDER BY player_name
    """)
    players = cursor.fetchall()
    
    if args.limit > 0:
        players = players[:args.limit]
    
    print(f"Processing {len(players)} players...")
    
    updated = 0
    for i, (player_id, player_name) in enumerate(players, 1):
        print(f"[{i}/{len(players)}] {player_name} ({player_id})...", end=" ")
        
        data = scrape_player(session, player_id)
        
        if data and any(v is not None for v in data.values()):
            if not args.dry_run:
                cursor.execute("""
                    UPDATE players 
                    SET height = COALESCE(%s, height),
                        weight = COALESCE(%s, weight),
                        position = COALESCE(%s, position),
                        birth_date = COALESCE(%s, birth_date)
                    WHERE player_id = %s
                """, (data['height'], data['weight'], data['position'], 
                      data['birth_date'], player_id))
                conn.commit()
            
            info = []
            if data['height']: info.append(f"{data['height']}cm")
            if data['weight']: info.append(f"{data['weight']}kg")
            if data['position']: info.append(data['position'])
            print(', '.join(info) if info else "no data")
            updated += 1
        else:
            print("no data")
        
        time.sleep(0.5)  # Be nice to server
    
    print(f"\nUpdated {updated} players")
    conn.close()

if __name__ == "__main__":
    main()
