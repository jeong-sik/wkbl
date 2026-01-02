import requests
import json
import os
import time
import random
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

# 설정
BASE_URL = "https://www.wkbl.or.kr/game/ajax"
ROOT_DIR = Path(__file__).resolve().parents[1]
META_FILE = ROOT_DIR / "data" / "wkbl" / "meta" / "all_games_list.json"
OUTPUT_DIR = ROOT_DIR / "data" / "raw_crawled"

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Referer": "https://www.wkbl.or.kr/game/result.asp",
    "X-Requested-With": "XMLHttpRequest"
}

def fetch_data(url, params):
    try:
        response = requests.post(url, data=params, headers=HEADERS, timeout=10)
        if response.status_code == 200:
            # 인코딩 처리
            try:
                return response.content.decode('euc-kr')
            except UnicodeDecodeError:
                return response.content.decode('utf-8', errors='replace')
    except Exception as e:
        print(f"Error fetching {url}: {e}")
    return None

def process_game(game):
    game_id = game['game_id']
    season_code = game['season_code']
    
    # 시즌별 디렉토리 생성
    season_dir = OUTPUT_DIR / season_code
    season_dir.mkdir(parents=True, exist_ok=True)
    
    player_file = season_dir / f"{game_id}_player.html"
    team_file = season_dir / f"{game_id}_team.html"
    
    # 이미 수집됨?
    if player_file.exists() and team_file.exists():
        return f"Skipped {game_id}"
        
    params = {
        "season_gu": game['season_code'],
        "game_type": game['game_type'],
        "game_no": game['game_no'],
        "ym": game['ym'],
        "h_player": "",
        "a_player": ""
    }
    
    # 1. 선수 기록
    if not player_file.exists():
        player_html = fetch_data(f"{BASE_URL}/ajax_game_result_2.asp", params)
        if player_html:
            with open(player_file, "w", encoding="utf-8") as f:
                f.write(player_html)
        else:
            return f"Failed Player {game_id}"

    # 2. 팀 기록
    if not team_file.exists():
        team_html = fetch_data(f"{BASE_URL}/ajax_game_result_1.asp", params)
        if team_html:
            with open(team_file, "w", encoding="utf-8") as f:
                f.write(team_html)
        else:
            return f"Failed Team {game_id}"
            
    # 매너 딜레이 (병렬 처리 시 각 스레드에서)
    time.sleep(random.uniform(0.2, 0.5))
    return f"Collected {game_id}"

def main():
    if not META_FILE.exists():
        print(f"Meta file not found: {META_FILE}")
        return
        
    with open(META_FILE, "r", encoding="utf-8") as f:
        games = json.load(f)
        
    # 테스트를 위해 최근 200경기만 먼저 수집
    games = games[:200]
        
    print(f"Starting crawl for {len(games)} games...")
    
    # 최신 시즌부터 수집 (역순 정렬)
    # games.sort(key=lambda x: x['season_code'], reverse=True) # 이미 정렬됨
    
    # 워커 수 조절 (서버 부하 고려 5개 정도만)
    max_workers = 5
    
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = [executor.submit(process_game, game) for game in games]
        
        count = 0
        total = len(futures)
        for future in as_completed(futures):
            count += 1
            result = future.result()
            if "Collected" in result:
                print(f"[{count}/{total}] {result}")
            elif "Failed" in result:
                print(f"[{count}/{total}] \033[91m{result}\033[0m")
            # Skipped는 너무 많으면 출력 생략 가능

    print("All tasks completed.")

if __name__ == "__main__":
    main()
