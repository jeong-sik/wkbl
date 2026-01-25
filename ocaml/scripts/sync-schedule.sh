#!/bin/bash
# WKBL Schedule Sync Script
# Runs daily to update game results in Supabase

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
LOG_DIR="$PROJECT_DIR/logs"
LOG_FILE="$LOG_DIR/sync-$(date +%Y%m%d).log"

# Ensure log directory exists
mkdir -p "$LOG_DIR"

cd "$PROJECT_DIR"

echo "=== WKBL Schedule Sync: $(date) ===" >> "$LOG_FILE"

# Run sync with dune exec
if dune exec wkbl-scraper -- sync schedule >> "$LOG_FILE" 2>&1; then
    echo "Sync completed successfully" >> "$LOG_FILE"
else
    echo "Sync failed with exit code $?" >> "$LOG_FILE"
fi

echo "" >> "$LOG_FILE"

# Keep only last 30 days of logs
find "$LOG_DIR" -name "sync-*.log" -mtime +30 -delete 2>/dev/null || true
