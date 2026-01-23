#!/usr/bin/env python3
import sqlite3
import os
from pathlib import Path

# Simplified Lineup Aggregator for WKBL
DB_PATH = Path("workspace/yousleepwhen/wkbl/data/wkbl.db")

def run():
    if not DB_PATH.exists():
        print(f"DB not found at {DB_PATH}")
        return

    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()

    print("Re-creating Lineup tables...")
    cursor.executescript("""
    CREATE TABLE IF NOT EXISTS lineups (
        lineup_id TEXT PRIMARY KEY,
        team_code TEXT,
        player1_id TEXT,
        player2_id TEXT,
        player3_id TEXT,
        player4_id TEXT,
        player5_id TEXT
    );

    CREATE TABLE IF NOT EXISTS lineup_stats (
        lineup_id TEXT,
        game_id TEXT,
        minutes_played_seconds INTEGER,
        points_scored INTEGER,
        points_allowed INTEGER,
        possessions INTEGER,
        PRIMARY KEY (lineup_id, game_id)
    );

    DROP VIEW IF EXISTS lineup_net_ratings;
    CREATE VIEW lineup_net_ratings AS
    SELECT
        l.team_code,
        l.lineup_id,
        SUM(ls.minutes_played_seconds) / 60.0 as total_minutes,
        SUM(ls.points_scored) as total_pts_scored,
        SUM(ls.points_allowed) as total_pts_allowed,
        (SUM(ls.points_scored) - SUM(ls.points_allowed)) as total_margin,
        CASE WHEN SUM(ls.possessions) > 0 
        THEN ROUND(100.0 * (SUM(ls.points_scored) - SUM(ls.points_allowed)) / SUM(ls.possessions), 2)
        ELSE 0 END as net_rating
    FROM lineups l
    JOIN lineup_stats ls ON l.lineup_id = ls.lineup_id
    GROUP BY l.team_code, l.lineup_id;
    """)

    print("Aggregating lineup data from PBP...")
    cursor.execute("SELECT DISTINCT team_code FROM game_stats")
    teams = [r[0] for r in cursor.fetchall()]
    
    for team in teams:
        if not team: continue
        cursor.execute("""
            SELECT player_id FROM game_stats 
            WHERE team_code = ? 
            GROUP BY player_id 
            ORDER BY SUM(min_seconds) DESC LIMIT 5
        """, (team,))
        top5 = sorted([str(r[0]) for r in cursor.fetchall()])
        if len(top5) == 5:
            lineup_id = ",".join(top5)
            cursor.execute("INSERT OR IGNORE INTO lineups (lineup_id, team_code, player1_id, player2_id, player3_id, player4_id, player5_id) VALUES (?, ?, ?, ?, ?, ?, ?)",
                           (lineup_id, team, top5[0], top5[1], top5[2], top5[3], top5[4]))
            
            # Add some dummy stats for visualization
            cursor.execute("INSERT OR IGNORE INTO lineup_stats (lineup_id, game_id, minutes_played_seconds, points_scored, points_allowed, possessions) VALUES (?, ?, ?, ?, ?, ?)",
                           (lineup_id, "SAMPLE-01", 1200, 45, 38, 25))

    conn.commit()
    print("Done. Sample lineup data inserted.")
    conn.close()

if __name__ == "__main__":
    run()