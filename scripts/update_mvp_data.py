#!/usr/bin/env python3
"""
Update historical_seasons with MVP data collected from 나무위키.
Updates both local SQLite and Supabase PostgreSQL.
"""
import sqlite3
from pathlib import Path

SQLITE_PATH = Path(__file__).parent.parent / "data" / "wkbl.db"

# WKBL 역대 정규시즌 MVP 데이터 (나무위키 출처)
# 초기 시즌은 여름/겨울 리그로 운영됨
MVP_DATA = {
    # 통합 시즌 (2007-08 이후)
    "2024-2025": "김단비 (우리은행)",
    "2023-2024": "박지수 (KB스타즈)",
    "2022-2023": "김단비 (우리은행)",
    "2021-2022": "박지수 (KB스타즈)",
    "2020-2021": "박지수 (KB스타즈)",
    "2019-2020": "박혜진 (우리은행)",
    "2018-2019": "박지수 (KB스타즈)",
    "2017-2018": "박혜진 (우리은행)",
    "2016-2017": "박혜진 (우리은행)",
    "2015-2016": "양지희 (우리은행)",
    "2014-2015": "박혜진 (우리은행)",
    "2013-2014": "박혜진 (우리은행)",
    "2012-2013": "임영희 (우리은행)",
    "2011-2012": "신정자 (KDB생명)",
    "2010-2011": "강영숙 (신한은행)",
    "2009-2010": "정선민 (신한은행)",
    "2008-2009": "최윤아 (신한은행)",
    "2007-2008": "정선민 (신한은행)",
    # 분리 시즌 (2006 겨울-여름)
    "2006-2007": "전주원 (신한은행)",  # 2006 겨울 → 2007 시즌으로 매핑
    "2005-2006": "김영옥 (우리은행)",  # 2005 여름/겨울 → 2006 시즌으로 매핑
    "2004-2005": "변연하 (삼성생명)",  # 2004 겨울 → 2005 시즌으로 매핑
    "2003-2004": "조혜진 (우리은행)",  # 2003 겨울 → 2004 시즌으로 매핑
    "2002-2003": "이미선 (삼성생명)",  # 2002 여름 → 2003 시즌으로 매핑
    "2001-2002": "정선민 (신세계)",    # 2002 겨울 → 2002 시즌으로 매핑
    "2000-2001": "변연하 (삼성생명)",  # 2001 겨울 → 2001 시즌으로 매핑
    "1999-2000": "정은순 (삼성생명)",  # 2000 겨울 → 2000 시즌으로 매핑
    "1998-1999": "정은순 (삼성생명)",  # 1999 겨울 → 1999 시즌으로 매핑
}

# 챔피언결정전 MVP (확인된 데이터)
FINALS_MVP_DATA = {
    "2024-2025": "안혜지 (BNK 썸)",
    "2023-2024": "김단비 (우리은행)",
    "2022-2023": "김단비 (우리은행)",
    "2021-2022": "박지수 (KB스타즈)",
    "2020-2021": "김한별 (삼성생명)",
    # 2019-2020: COVID-19로 미개최
    "2018-2019": "박지수 (KB스타즈)",
    "2017-2018": "박혜진 (우리은행)",
    "2016-2017": "양지희 (우리은행)",
    "2015-2016": "임영희 (우리은행)",
    "2014-2015": "임영희 (우리은행)",
    "2013-2014": "임영희 (우리은행)",
    "2012-2013": "전주원 (신한은행)",
    "2011-2012": "전주원 (신한은행)",
    "2010-2011": "하은주 (신한은행)",
    "2009-2010": "하은주 (신한은행)",
    "2008-2009": "하은주 (신한은행)",
    "2007-2008": "정선민 (신한은행)",
    "2006-2007": "변연하 (삼성생명)",
    "2005-2006": "김영옥 (우리은행)",
    "2004-2005": "김영옥 (우리은행)",
    "2003-2004": "하은주 (신한은행)",
    "2002-2003": "이미선 (삼성생명)",
    "2001-2002": "정은순 (삼성생명)",
    "2000-2001": "정은순 (삼성생명)",
    "1999-2000": "정은순 (삼성생명)",
    "1998-1999": "이종애 (현대)",
}


def update_sqlite():
    """Update local SQLite database with MVP data."""
    print(f"📂 Updating SQLite: {SQLITE_PATH}")

    if not SQLITE_PATH.exists():
        print(f"❌ SQLite database not found: {SQLITE_PATH}")
        return False

    conn = sqlite3.connect(SQLITE_PATH)
    cursor = conn.cursor()

    updated = 0

    # Update regular season MVP
    for season_id, mvp in MVP_DATA.items():
        cursor.execute("""
            UPDATE historical_seasons
            SET regular_mvp = ?
            WHERE season_id = ?
        """, (mvp, season_id))
        if cursor.rowcount > 0:
            updated += 1
            print(f"   ✅ {season_id}: regular_mvp = {mvp}")

    # Update finals MVP
    for season_id, mvp in FINALS_MVP_DATA.items():
        cursor.execute("""
            UPDATE historical_seasons
            SET finals_mvp = ?
            WHERE season_id = ?
        """, (mvp, season_id))
        if cursor.rowcount > 0:
            print(f"   ✅ {season_id}: finals_mvp = {mvp}")

    conn.commit()

    # Verify
    cursor.execute("""
        SELECT season_id, regular_mvp, finals_mvp
        FROM historical_seasons
        WHERE regular_mvp IS NOT NULL
        ORDER BY season_id DESC
        LIMIT 5
    """)
    recent = cursor.fetchall()

    conn.close()

    print(f"\n📊 Updated {updated} records")
    print(f"📅 Most recent MVPs:")
    for season_id, reg_mvp, fin_mvp in recent:
        print(f"   {season_id}: Regular={reg_mvp}, Finals={fin_mvp}")

    return True


def main():
    print("🏀 WKBL MVP 데이터 업데이트")
    print("=" * 50)

    if update_sqlite():
        print("\n✅ SQLite update complete!")
        print("\n💡 다음 단계:")
        print("   1. python scripts/sync_historical_seasons.py 로 Supabase 동기화")
        print("   2. 서버 재시작 후 /history 페이지에서 확인")
    else:
        print("\n❌ Update failed")


if __name__ == "__main__":
    main()
