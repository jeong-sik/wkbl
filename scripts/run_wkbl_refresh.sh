#!/usr/bin/env bash
# WKBL Data Refresh Script with improved error handling and monitoring
set -euo pipefail

# Source environment variables (for launchd which doesn't inherit shell env)
# shellcheck source=/dev/null
[[ -f "$HOME/.zshenv" ]] && source "$HOME/.zshenv"

# Initialize opam environment (required for dune)
if command -v opam &>/dev/null; then
    eval "$(opam env --switch=5.4.0 --set-switch)"
fi

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

    cd "$ROOT_DIR/ocaml"

    log "Building scraper tool..."
    dune build bin/scraper_tool.exe

    log "Syncing schedule and results..."
    if dune exec bin/scraper_tool.exe sync schedule && dune exec bin/scraper_tool.exe sync boxscore; then
        local end_time duration
        end_time=$(date +%s)
        duration=$((end_time - start_time))
        log "=== WKBL refresh SUCCESS (${duration}s) ==="

        # Run health check
        log "Running PBP quality check..."
        curl -s "http://localhost:8080/qa/pbp?ci=1" || log "Web server not reachable for health check, skipping."
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
