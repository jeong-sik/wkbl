import sqlite3
import pandas as pd
import os

# Paths
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DB_PATH = os.path.join(BASE_DIR, "data/wkbl.db")
CSV_PATH = os.path.join(BASE_DIR, "data/wkbl/derived/games_summary.csv")

def backfill_scores():
    print(f"Loading data from {CSV_PATH}...")
    try:
        df = pd.read_csv(CSV_PATH)
    except FileNotFoundError:
        print(f"Error: CSV file not found at {CSV_PATH}")
        return

    print(f"Connecting to database at {DB_PATH}...")
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    updated_count = 0
    
    # Check existing data schema (game_id format)
    # CSV game_key: 1-1-199807-1 (season-type-ym-no?) -> DB game_id might be different
    # DB check: 046-01-9 (season-type-no?)
    
    # Let's inspect DB game_id format first
    cursor.execute("SELECT game_id FROM games LIMIT 1")
    row = cursor.fetchone()
    if row:
        print(f"Sample DB game_id: {row[0]}")
    
    # Strategy: Match by season, game_type, game_no (if possible) or try to construct game_id
    # From previous context, DB game_id looks like: 046-01-9 (Season 046, Type 01, Game 9)
    # CSV has: season_gu (001), game_type (1), game_no (1)
    
    print("Starting update...")
    for _, row in df.iterrows():
        season = str(row['season_gu']).zfill(3)
        game_type = str(row['game_type']).zfill(2)
        game_no = str(row['game_no'])
        
        # Construct DB-style game_id
        # Assuming DB format is "{season}-{game_type}-{game_no}" based on previous `sqlite3` output "046-01-9"
        game_id = f"{season}-{game_type}-{game_no}"
        
        pts_a = row['pts_a']
        pts_b = row['pts_b']
        
        # In games_summary.csv: team_a is usually home, team_b is away? Or generic?
        # Let's verify team codes in DB to be safe, but simple update first.
        # Wait, the DB has `home_team_code` and `away_team_code`.
        # We need to map team names to codes to ensure we assign scores to correct columns?
        # Or just assume team_a = home, team_b = away matching the game_id?
        
        # Let's just update based on game_id.
        # But games_summary doesn't strictly say who is home/away in the columns 'team_a', 'team_b'.
        # Usually team_a=Home, team_b=Away in my processing scripts.
        
        try:
            # We need to match team_a/team_b to home/away to assign scores correctly.
            # Get the game from DB to see who is home/away
            cursor.execute("SELECT home_team_code, away_team_code FROM games WHERE game_id = ?", (game_id,))
            db_game = cursor.fetchone()
            
            if db_game:
                home_code, away_code = db_game
                
                # Fetch team names for codes to compare with CSV names
                # Map CSV team names to DB team codes
                # This requires a quick map.
                # Let's build a map from the DB first.
                pass 
            else:
                # print(f"Game not found in DB: {game_id}")
                continue

        except Exception as e:
            print(f"Error checking game {game_id}: {e}")
            continue

    # 2nd Pass: Optimized approach
    # Since we need to match teams, let's load team map first
    team_map = {}
    cursor.execute("SELECT team_code, team_name_kr FROM teams")
    for code, name in cursor.fetchall():
        team_map[name] = code
        # Handle aliases if needed (e.g., BNK 썸)
        if name == "BNK 썸":
            team_map["BNK썸"] = code
        if name == "KB스타즈":
            team_map["KB 스타즈"] = code

    for _, row in df.iterrows():
        season = str(row['season_gu']).zfill(3)
        game_type = str(row['game_type']).zfill(2)
        game_no = str(row['game_no'])
        game_id = f"{season}-{game_type}-{game_no}"
        
        team_a_name = row['team_a']
        team_b_name = row['team_b']
        pts_a = int(row['pts_a'])
        pts_b = int(row['pts_b'])
        
        cursor.execute("SELECT home_team_code, away_team_code FROM games WHERE game_id = ?", (game_id,))
        result = cursor.fetchone()
        
        if not result:
            continue
            
        home_code, away_code = result
        
        # Determine which score belongs to home/away
        home_score = 0
        away_score = 0
        
        # Try to match names to codes
        team_a_code = team_map.get(team_a_name)
        team_b_code = team_map.get(team_b_name)
        
        updated = False
        
        if team_a_code == home_code:
            home_score = pts_a
            if team_b_code == away_code:
                away_score = pts_b
                updated = True
        elif team_a_code == away_code:
            away_score = pts_a
            if team_b_code == home_code:
                home_score = pts_b
                updated = True
        
        # Fallback for name mismatches (fuzzy or just order)
        if not updated:
            # If CSV has strict Home=Team A, Away=Team B convention? 
            # Often games_summary is Home vs Away. Let's assume that if strict match fails.
            # But safer to skip ambiguous ones.
            # Let's try simple alias matching if main failed.
            pass

        if updated:
            cursor.execute(
                "UPDATE games SET home_score = ?, away_score = ? WHERE game_id = ?",
                (home_score, away_score, game_id)
            )
            updated_count += 1

    conn.commit()
    print(f"Successfully updated scores for {updated_count} games.")
    conn.close()

if __name__ == "__main__":
    backfill_scores()
