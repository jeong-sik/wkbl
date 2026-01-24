#!/usr/bin/env python3
"""
Sync historical_seasons from local SQLite to Supabase PostgreSQL.
Replaces old data with correct WKBL championship data (1998-2025).
"""
import sqlite3
import psycopg2
from pathlib import Path

# Config
SQLITE_PATH = Path(__file__).parent.parent / "data" / "wkbl.db"
PG_HOST = "aws-1-ap-southeast-1.pooler.supabase.com"
PG_PORT = "6543"
PG_USER = "postgres.efgbkvmwwefqjxeugktf"
PG_PASS = "dRp7We9L61Ur1sg"
PG_DB = "postgres"

def get_sqlite_data():
    """Read historical_seasons from local SQLite."""
    conn = sqlite3.connect(SQLITE_PATH)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()

    cursor.execute("""
        SELECT season_id, season_name, champion_team, runner_up,
               regular_mvp, finals_mvp, rookie_of_year, scoring_leader, notes
        FROM historical_seasons
        ORDER BY season_id DESC
    """)

    rows = [dict(row) for row in cursor.fetchall()]
    conn.close()
    return rows

def sync_to_postgres(rows):
    """Sync data to PostgreSQL, replacing existing records."""
    print(f"🔌 Connecting to Supabase PostgreSQL...")
    conn = psycopg2.connect(
        host=PG_HOST, port=PG_PORT, user=PG_USER, password=PG_PASS, dbname=PG_DB
    )
    cursor = conn.cursor()

    # Check current state
    cursor.execute("SELECT COUNT(*) FROM historical_seasons")
    before_count = cursor.fetchone()[0]
    print(f"📊 Current records in PostgreSQL: {before_count}")

    # Clear old data
    cursor.execute("DELETE FROM historical_seasons")
    print(f"🗑️  Cleared old records")

    # Insert new data
    insert_sql = """
        INSERT INTO historical_seasons
        (season_id, season_name, champion_team, runner_up,
         regular_mvp, finals_mvp, rookie_of_year, scoring_leader, notes)
        VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)
    """

    for row in rows:
        cursor.execute(insert_sql, (
            row['season_id'],
            row['season_name'],
            row['champion_team'],
            row.get('runner_up'),
            row.get('regular_mvp'),
            row.get('finals_mvp'),
            row.get('rookie_of_year'),
            row.get('scoring_leader'),
            row.get('notes')
        ))

    conn.commit()

    # Verify
    cursor.execute("SELECT COUNT(*) FROM historical_seasons")
    after_count = cursor.fetchone()[0]

    cursor.execute("SELECT season_id, champion_team FROM historical_seasons ORDER BY season_id DESC LIMIT 3")
    recent = cursor.fetchall()

    conn.close()

    print(f"✅ Synced {after_count} records to PostgreSQL")
    print(f"📅 Most recent seasons:")
    for season_id, champion in recent:
        print(f"   {season_id}: {champion}")

def main():
    print(f"📂 Reading from: {SQLITE_PATH}")

    if not SQLITE_PATH.exists():
        print(f"❌ SQLite database not found: {SQLITE_PATH}")
        return

    rows = get_sqlite_data()
    print(f"📊 Found {len(rows)} records in SQLite")

    if not rows:
        print("❌ No data to sync")
        return

    # Show preview
    print(f"\n📋 Preview (first 5 seasons):")
    for row in rows[:5]:
        print(f"   {row['season_id']}: {row['champion_team']} vs {row.get('runner_up', '-')}")

    # Confirm
    response = input("\n🔄 Proceed with sync? [y/N]: ")
    if response.lower() != 'y':
        print("❌ Aborted")
        return

    sync_to_postgres(rows)
    print("\n🎉 Sync complete!")

if __name__ == "__main__":
    main()
