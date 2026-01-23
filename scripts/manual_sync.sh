#!/bin/bash
# WKBL 수동 데이터 동기화 (GitHub Actions 대체)
set -e

cd "$(dirname "$0")/.."
DB_URL="postgresql://postgres.efgbkvmwwefqjxeugktf:dRp7We9L61Ur1sg@aws-1-ap-southeast-1.pooler.supabase.com:6543/postgres"

echo "🏀 WKBL 데이터 동기화 시작..."

echo "1️⃣ 경기 데이터 수집..."
python scripts/wkbl_ajax_collector.py 2>&1 | tail -3

echo "2️⃣ 통계 집계..."
python scripts/wkbl_aggregate_stats.py 2>&1 | tail -1

echo "3️⃣ 새 경기 DB 동기화..."
python scripts/sync_games_to_postgres.py --db-url "$DB_URL" --season 046 2>&1 | tail -2

echo "4️⃣ 경기 날짜 백필..."
python scripts/backfill_game_dates.py --db-url "$DB_URL" --season 046 2>&1 | grep -E "Success|Remaining|Done"

echo "5️⃣ Materialized View 리프레시..."
python3 -c "
import psycopg2
conn = psycopg2.connect('$DB_URL')
conn.autocommit = True
conn.cursor().execute('REFRESH MATERIALIZED VIEW games_calc')
print('✅ games_calc 리프레시 완료')
conn.close()
"

echo ""
echo "🎉 동기화 완료! https://wkbl.win/games 확인하세요"
