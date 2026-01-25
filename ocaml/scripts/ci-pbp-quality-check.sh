#!/bin/bash
# PBP Data Quality CI Check
# Usage: ./scripts/ci-pbp-quality-check.sh [threshold] [server_url]
# Returns exit code 1 if quality below threshold

set -e

THRESHOLD=${1:-50}
SERVER_URL=${2:-"https://wkbl.win"}

echo "🔍 PBP Data Quality Check"
echo "   Threshold: ${THRESHOLD}%"
echo "   Server: ${SERVER_URL}"
echo ""

RESPONSE=$(curl -sf "${SERVER_URL}/qa/pbp?ci=1&threshold=${THRESHOLD}" 2>/dev/null)

if [ $? -ne 0 ]; then
    echo "❌ Failed to reach server"
    exit 1
fi

# Check for error in response
if echo "$RESPONSE" | jq -e '.error' > /dev/null 2>&1; then
    VERIFIED=$(echo "$RESPONSE" | jq -r '.verified_pct')
    echo "❌ Quality check FAILED"
    echo "   Verified: ${VERIFIED}% (below ${THRESHOLD}%)"
    echo ""
    echo "Details:"
    echo "$RESPONSE" | jq '.details'
    exit 1
else
    VERIFIED=$(echo "$RESPONSE" | jq -r '.verified_pct')
    T2_HOME=$(echo "$RESPONSE" | jq -r '.t2_home_verified')
    TOTAL=$(echo "$RESPONSE" | jq -r '.total_pbp_games')
    echo "✅ Quality check PASSED"
    echo "   Verified: ${VERIFIED}% (threshold: ${THRESHOLD}%)"
    echo "   T2=HOME pattern: ${T2_HOME}/${TOTAL} games"
    exit 0
fi
