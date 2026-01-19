#!/usr/bin/env bash
# WKBL Data Refresh Script with improved error handling and monitoring
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
ME_ROOT="${ME_ROOT:-$HOME/me}"
LOG_DIR="$ME_ROOT/logs"
LOG_FILE="$LOG_DIR/wkbl_refresh.log"
SLACK_WEBHOOK="${WKBL_SLACK_WEBHOOK:-}"

mkdir -p "$LOG_DIR"

# Rotate log if too large (> 10MB)
if [[ -f "$LOG_FILE" ]] && [[ $(stat -f%z "$LOG_FILE" 2>/dev/null || stat -c%s "$LOG_FILE" 2>/dev/null) -gt 10485760 ]]; then
    mv "$LOG_FILE" "${LOG_FILE}.old"
fi

send_alert() {
    local message="$1"
    if [[ -n "$SLACK_WEBHOOK" ]]; then
        curl -sS -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"$message\"}" \
            "$SLACK_WEBHOOK" >/dev/null 2>&1 || true
    fi
}

log() {
    echo "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] $1"
}

# Main refresh function
run_refresh() {
    local start_time
    start_time=$(date +%s)

    log "=== WKBL refresh START ==="
    log "Working directory: $ROOT_DIR"

    cd "$ROOT_DIR"

    # Run the main refresh pipeline
    if python3 scripts/wkbl_refresh_all.py --update-roster --audit-photos -- --years 10; then
        local end_time duration
        end_time=$(date +%s)
        duration=$((end_time - start_time))
        log "=== WKBL refresh SUCCESS (${duration}s) ==="

        # Run health check after success
        if python3 "$SCRIPT_DIR/wkbl_health_check.py" --quiet; then
            log "Health check: PASSED"
        else
            log "Health check: ISSUES DETECTED"
            send_alert "⚠️ WKBL refresh completed but health check found issues"
        fi
    else
        local exit_code=$?
        log "=== WKBL refresh FAILED (exit code: $exit_code) ==="
        send_alert "🚨 WKBL data refresh failed with exit code $exit_code"
        return $exit_code
    fi
}

# Execute with logging
{
    run_refresh
} >> "$LOG_FILE" 2>&1
