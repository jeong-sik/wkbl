#!/bin/bash

BASE_URL="http://localhost:8000"
EXIT_CODE=0

echo "🔍 Starting WKBL Self-Test..."

check_page() {
    local url="$1"
    local name="$2"
    
    # HTTP 상태 코드와 본문을 모두 가져옴
    response=$(curl -s -w "\n%{http_code}" "$BASE_URL$url")
    body=$(echo "$response" | head -n -1)
    status=$(echo "$response" | tail -n 1)
    
    if [ "$status" -ne 200 ]; then
        echo "❌ [FAIL] $name ($url): Status $status"
        EXIT_CODE=1
        return
    fi

        # 본문 내 에러 키워드 검사 (Internal Server Error 등)
    if echo "$body" | grep -q "Something went wrong"; then
        echo "❌ [FAIL] $name ($url): 'Something went wrong' found in body"
        # 에러 메시지 추출 (괄호 안의 내용)
        error_msg=$(echo "$body" | grep -o "(Db.*)" | head -n 1)
        echo "   -> $error_msg"
        EXIT_CODE=1
        return
    fi

    if echo "$body" | grep -q "Db.QueryFailed"; then
        echo "❌ [FAIL] $name ($url): DB Query Failed"
        EXIT_CODE=1
        return
    fi

    echo "✅ [PASS] $name ($url)"
}

# 1. 메인 페이지
check_page "/" "Home"

# 2. 목록 페이지들
check_page "/players" "Players List"
check_page "/teams" "Teams List"
check_page "/standings" "Standings"
check_page "/games" "Games List"
check_page "/boxscores" "Boxscores List"
check_page "/leaders" "Leaders"

# 3. 상세 페이지 (실제 ID 사용)
# Player: 김단비 (095035 - 추정, DB에서 확인 필요하지만 일단 고정값 시도)
# Team: KB스타즈 (URL 인코딩 주의)
# Game: 임의의 ID
# DB에서 유효한 ID 하나씩 가져와서 테스트하면 더 좋음

# DB에서 실제 데이터 하나씩 가져오기
DB_PATH="../data/wkbl.db"
if [ -f "$DB_PATH" ]; then
    PID=$(sqlite3 "$DB_PATH" "SELECT player_id FROM players LIMIT 1;")
    TNAME=$(sqlite3 "$DB_PATH" "SELECT team_name_kr FROM teams LIMIT 1;")
    GID=$(sqlite3 "$DB_PATH" "SELECT game_id FROM games LIMIT 1;")
    
    # URL Encoding for Team Name
    # mac/linux curl handles unicode, but url encoding is safer. 
    # Here assuming simple curl request works or using python for encoding if needed.
    # For now, relying on curl to handle the string, or skipping exact encoding.
    # Actually, let's use Python for quick encoding
    TNAME_ENC=$(python3 -c "import urllib.parse; print(urllib.parse.quote('$TNAME'))")

    check_page "/player/$PID" "Player Profile ($PID)"
    check_page "/team/$TNAME_ENC" "Team Profile ($TNAME)"
    check_page "/boxscore/$GID" "Boxscore ($GID)"
    
    # H2H (PID vs PID) - Get another player
    PID2=$(sqlite3 "$DB_PATH" "SELECT player_id FROM players WHERE player_id != '$PID' LIMIT 1;")
    
    # 이름이 필요하므로... DB에서 이름 가져오기
    PNAME1=$(sqlite3 "$DB_PATH" "SELECT player_name FROM players WHERE player_id='$PID';")
    PNAME2=$(sqlite3 "$DB_PATH" "SELECT player_name FROM players WHERE player_id='$PID2';")
    PNAME1_ENC=$(python3 -c "import urllib.parse; print(urllib.parse.quote('$PNAME1'))")
    PNAME2_ENC=$(python3 -c "import urllib.parse; print(urllib.parse.quote('$PNAME2'))")
    
    check_page "/compare?p1=$PNAME1_ENC&p2=$PNAME2_ENC" "Compare ($PNAME1 vs $PNAME2)"
else
    echo "⚠️ DB not found at $DB_PATH, skipping detailed checks."
fi

if [ $EXIT_CODE -eq 0 ]; then
    echo "🎉 All checks passed!"
else
    echo "💥 Some checks failed."
fi

exit $EXIT_CODE
