import sqlite3
import pandas as pd
from sqlalchemy import create_engine, text
import os

# Configuration
SQLITE_DB_PATH = "data/wkbl.db"
POSTGRES_USER = "postgres"
# Tip: If password contains special characters, they need to be URL encoded.
POSTGRES_PASSWORD = "***REMOVED***" 
POSTGRES_HOST = "db.efgbkvmwwefqjxeugktf.supabase.co"
POSTGRES_PORT = "6543"
POSTGRES_DB = "postgres"

# Connection Strings
sqlite_url = f"sqlite:///{SQLITE_DB_PATH}"
postgres_url = f"postgresql://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}:{POSTGRES_PORT}/{POSTGRES_DB}"

def migrate():
    print("🚀 Starting migration from SQLite to Postgres...")
    
    # 1. Connect to SQLite
    print(f"📂 Reading from {SQLITE_DB_PATH}...")
    sqlite_engine = create_engine(sqlite_url)
    
    # Get all table names
    with sqlite_engine.connect() as conn:
        result = conn.execute(text("SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%';"))
        tables = [row[0] for row in result]
    
    print(f"📋 Found tables: {tables}")
    
    # 2. Connect to Postgres
    print("🔌 Connecting to Postgres...")
    pg_engine = create_engine(postgres_url)
    
    # 3. Migrate each table
    for table in tables:
        print(f"🔄 Migrating table: {table}...", end=" ", flush=True)
        try:
            # Read from SQLite
            df = pd.read_sql_table(table, sqlite_engine)
            
            # Fix specific column types if needed (e.g. date handling)
            # Postgres is strict about dates, but pandas handles it mostly well.
            
            # Write to Postgres (replace = drop & create)
            # 'method="multi"' speeds up insertion
            df.to_sql(table, pg_engine, if_exists='replace', index=False, method='multi', chunksize=1000)
            print("✅ Done!")
        except Exception as e:
            print(f"❌ Failed: {e}")

    print("🎉 Migration completed!")

if __name__ == "__main__":
    # Ensure we are in the right directory
    if not os.path.exists(SQLITE_DB_PATH):
        print(f"❌ Error: Cannot find {SQLITE_DB_PATH}. Please run this script from the project root.")
    else:
        migrate()
