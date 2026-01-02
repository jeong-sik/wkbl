import requests
from bs4 import BeautifulSoup
import json
import time
import random
import os
from pathlib import Path

# 설정
BASE_URL = "https://www.wkbl.or.kr/game/sch/schedule.asp"
ROOT_DIR = Path(__file__).resolve().parents[1]
META_DIR = ROOT_DIR / "data" / "wkbl" / "meta"
SEASONS_FILE = ROOT_DIR / "scripts" / "wkbl_seasons.json"

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
}

def get_schedule(session, season_code, year_month):
    # viewType=1: 리스트형 보기
    url = f"{BASE_URL}?season_gu={season_code}&game_type=01&ym={year_month}&viewType=1"
    try:
        response = session.get(url, headers=HEADERS, timeout=10)
        if response.status_code == 200:
            # 인코딩 처리
            return response.content.decode('euc-kr', errors='replace')
    except Exception as e:
        print(f"Error fetching schedule {season_code}-{year_month}: {e}")
    return None

def parse_schedule(html):
    soup = BeautifulSoup(html, "html.parser")
    games = []
    
    # 경기 링크 추출
    links = soup.find_all("a", href=True)
    for link in links:
        href = link['href']
        if "game_no=" in href and "result.asp" in href:
            try:
                # 파라미터 파싱
                qs = href.split('?')[1]
                params = dict(x.split('=') for x in qs.split('&'))
                
                game_info = {
                    "season_code": params.get('season_gu'),
                    "game_type": params.get('game_type'),
                    "game_no": params.get('game_no'),
                    "ym": params.get('ym'),
                    "game_id": f"{params.get('season_gu')}-{params.get('game_type')}-{params.get('game_no')}"
                }
                games.append(game_info)
            except Exception:
                continue
                
    return games

def main():
    META_DIR.mkdir(parents=True, exist_ok=True)
    
    with open(SEASONS_FILE, "r", encoding="utf-8") as f:
        seasons = json.load(f)
        
    all_games = []
    session = requests.Session()
    
    # 최근 시즌부터 역순으로 수집 (최신 데이터 우선)
    # seasons.reverse() # json이 이미 역순임(046 -> 001)
    
    for season in seasons:
        season_code = season['code']
        season_name = season['name']
        print(f"Scanning Season: {season_name} ({season_code})")
        
        # 월별 순회 (여름/겨울리그 등 시즌마다 월이 다름 -> 1월~12월 전체 스캔이 안전)
        # 단, 연도를 알아야 함. season_name에서 연도 추정
        # 예: 2024-2025 -> 2024, 2025
        # 예: 2006여름 -> 2006
        
        years = []
        if '-' in season_name:
            start, end = season_name.split('-')
            years = [start, end]
        else:
            # '2006여름' -> '2006'
            import re
            match = re.search(r'\d{4}', season_name)
            if match:
                years = [match.group(0)]
        
        season_games = []
        for year in years:
            for month in range(1, 13):
                year_month = f"{year}{month:02d}"
                
                # 이미 수집한 범위인지 확인은 어려우니 무조건 요청 (빠름)
                html = get_schedule(session, season_code, year_month)
                if html:
                    games = parse_schedule(html)
                    if games:
                        print(f"  Found {len(games)} games in {year_month}")
                        season_games.extend(games)
                
                time.sleep(0.1) # 짧은 딜레이
        
        # 중복 제거
        unique_games = {g['game_id']: g for g in season_games}.values()
        print(f"  => Total {len(unique_games)} unique games in season {season_code}")
        all_games.extend(unique_games)
        
        # 중간 저장 (혹시 끊길까봐)
        with open(META_DIR / "all_games_list.json", "w", encoding="utf-8") as f:
            json.dump(all_games, f, indent=4)

    print(f"\nScanning Complete. Total {len(all_games)} games found.")

if __name__ == "__main__":
    main()
