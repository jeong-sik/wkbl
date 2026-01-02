import sqlite3
import os
import re
import json
from pathlib import Path
from bs4 import BeautifulSoup

ROOT_DIR = Path(__file__).resolve().parents[1]
DB_FILE = ROOT_DIR / "data" / "wkbl.db"
RAW_DATA_DIR = ROOT_DIR / "data" / "raw_crawled"
SCHEMA_FILE = ROOT_DIR / "schema.sql"

def init_db():
    conn = sqlite3.connect(DB_FILE)
    
    # 스키마가 없으면 생성
    cursor = conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='games';")
    if not cursor.fetchone():
        with open(SCHEMA_FILE, "r", encoding="utf-8") as f:
            conn.executescript(f.read())
            
    # 시즌 메타데이터 삽입 (없으면)
    cursor.execute("INSERT OR IGNORE INTO seasons (season_code, season_name) VALUES ('046', '2025-2026')")
    cursor.execute("INSERT OR IGNORE INTO seasons (season_code, season_name) VALUES ('045', '2024-2025')")
    cursor.execute("INSERT OR IGNORE INTO seasons (season_code, season_name) VALUES ('044', '2023-2024')")
    
    conn.commit()
    return conn

def clean_text(text):
    return text.strip() if text else ""

def parse_html_data(conn):
    cursor = conn.cursor()
    
    if not RAW_DATA_DIR.exists():
        print(f"No raw data directory: {RAW_DATA_DIR}")
        return

    # 시즌별 디렉토리 순회
    for season_dir in RAW_DATA_DIR.glob("*"):
        if not season_dir.is_dir(): continue
        
        season_code = season_dir.name
        print(f"Processing Season {season_code}...")
        
        # 경기별 파일 처리
        game_files = list(season_dir.glob("*_team.html"))
        for team_file in game_files:
            game_id = team_file.name.replace("_team.html", "")
            player_file = season_dir / f"{game_id}_player.html"
            
            try:
                process_game(cursor, game_id, season_code, team_file, player_file)
            except Exception as e:
                print(f"Error processing {game_id}: {e}")
                
        conn.commit()

def process_game(cursor, game_id, season_code, team_file, player_file):
    # game_id 파싱
    parts = game_id.split('-')
    if len(parts) >= 3:
        game_type, game_no = parts[1], parts[2]
    else:
        game_type, game_no = '01', '0' # Fallback

    # 1. 팀 정보 파싱 (Team HTML)
    with open(team_file, "r", encoding="utf-8") as f:
        soup = BeautifulSoup(f.read(), "html.parser")
        
    wraps = soup.find_all("span", class_=re.compile(r"outcome_wrap (home|away)"))
    home_code = None
    away_code = None
    
    for wrap in wraps:
        is_home = "home" in wrap['class']
        team_p = wrap.find("p")
        if not team_p: continue
        team_name = clean_text(team_p.text)
        
        th = wrap.parent
        img = th.find("img", src=re.compile(r"teamlogo_"))
        team_code = "UNKNOWN"
        if img:
            code_match = re.search(r"teamlogo_(\d+).png", img['src'])
            if code_match:
                team_code = code_match.group(1)
        
        cursor.execute("INSERT OR IGNORE INTO teams (team_code, team_name_kr) VALUES (?, ?)", (team_code, team_name))
        
        if is_home: home_code = team_code
        else: away_code = team_code

    # 게임 정보 삽입 (날짜 정보가 없으면 NULL 허용 필요, 일단 NULL로 넣고 나중에 업데이트)
    cursor.execute("""
        INSERT OR IGNORE INTO games (game_id, season_code, game_type, game_no, home_team_code, away_team_code)
        VALUES (?, ?, ?, ?, ?, ?)
    """, (game_id, season_code, game_type, int(game_no), home_code, away_code))
    
    # 업데이트 (이미 있을 경우 팀 코드 보강)
    if home_code:
        cursor.execute("UPDATE games SET home_team_code = ? WHERE game_id = ?", (home_code, game_id))
    if away_code:
        cursor.execute("UPDATE games SET away_team_code = ? WHERE game_id = ?", (away_code, game_id))


    # 2. 선수 기록 파싱 (Player HTML)
    if not player_file.exists(): return
    
    with open(player_file, "r", encoding="utf-8") as f:
        soup = BeautifulSoup(f.read(), "html.parser")
        
    tables = soup.find_all("div", class_="info_table01 type_record")
    # 팀 타이틀 (순서 중요: 보통 Away, Home 순서거나 반대일 수 있음. h4 태그 확인)
    titles = soup.find_all("h4", class_="tit_area")
    
    for i, title in enumerate(titles):
        # 팀 이름으로 팀 코드 찾기 (DB에서)
        team_name = clean_text(title.text)
        cursor.execute("SELECT team_code FROM teams WHERE team_name_kr = ?", (team_name,))
        row = cursor.fetchone()
        team_code = row[0] if row else None
        
        if not team_code: continue # 팀을 못 찾으면 스킵
        
        if i >= len(tables): break
        table = tables[i].find("table")
        rows = table.find("tbody").find_all("tr")
        
        for tr in rows:
            cols = tr.find_all("td")
            if not cols: continue
            
            # 플레이어
            a_tag = cols[0].find("a")
            if not a_tag: continue
            
            player_name = clean_text(a_tag.text)
            try:
                player_id = a_tag['href'].split("pno=")[1]
            except:
                continue
                
            cursor.execute("INSERT OR IGNORE INTO players (player_id, player_name) VALUES (?, ?)", (player_id, player_name))
            
            # 스탯 추출
            try:
                min_val = clean_text(cols[2].text)
                min_sec = 0
                if ":" in min_val:
                    m, s = map(int, min_val.split(":"))
                    min_sec = m * 60 + s
                
                fg2 = clean_text(cols[3].text).split('-')
                fg3 = clean_text(cols[4].text).split('-')
                ft = clean_text(cols[5].text).split('-')
                
                # 고급 스탯 계산
                pts = int(clean_text(cols[14].text))
                fga = int(fg2[1]) + int(fg3[1])
                fta = int(ft[1])
                fgm = int(fg2[0]) + int(fg3[0])
                fg3m = int(fg3[0])
                
                ts = pts / (2 * (fga + 0.44 * fta)) if (fga + 0.44 * fta) > 0 else 0
                efg = (fgm + 0.5 * fg3m) / fga if fga > 0 else 0
                gmsc = pts + 0.4 * fgm - 0.7 * fga - 0.4*(fta - int(ft[0])) + 0.7 * int(clean_text(cols[6].text)) + 0.3 * int(clean_text(cols[7].text)) + int(clean_text(cols[11].text)) + 0.7 * int(clean_text(cols[9].text)) + 0.7 * int(clean_text(cols[13].text)) - 0.4 * int(clean_text(cols[10].text)) - int(clean_text(cols[12].text))

                cursor.execute("""
                    INSERT OR REPLACE INTO game_stats (
                        game_id, team_code, player_id, min_seconds,
                        fg_2p_m, fg_2p_a, fg_3p_m, fg_3p_a, ft_m, ft_a,
                        reb_off, reb_def, reb_tot,
                        ast, pf, stl, tov, blk, pts,
                        ts_pct, efg_pct, game_score
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    game_id, team_code, player_id, min_sec,
                    int(fg2[0]), int(fg2[1]), int(fg3[0]), int(fg3[1]), int(ft[0]), int(ft[1]),
                    int(clean_text(cols[6].text)), int(clean_text(cols[7].text)), int(clean_text(cols[8].text)),
                    int(clean_text(cols[9].text)), int(clean_text(cols[10].text)), int(clean_text(cols[11].text)),
                    int(clean_text(cols[12].text)), int(clean_text(cols[13].text)), pts,
                    round(ts, 3), round(efg, 3), round(gmsc, 1)
                ))
            except Exception:
                continue

def main():
    if DB_FILE.exists():
        os.remove(DB_FILE) # 깨끗하게 다시 시작 (테스트 단계이므로)
        
    conn = init_db()
    parse_html_data(conn)
    conn.close()
    print("HTML to DB Sync Complete.")

if __name__ == "__main__":
    main()