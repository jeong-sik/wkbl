#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ME_ROOT="${ME_ROOT:-$HOME/me}"
LOG_DIR="$ME_ROOT/logs"
LOG_FILE="$LOG_DIR/wkbl_refresh.log"

mkdir -p "$LOG_DIR"

{
  echo "=== WKBL refresh start: $(date -u +%Y-%m-%dT%H:%M:%SZ) ==="
  cd "$ROOT_DIR"
  python3 scripts/wkbl_refresh_all.py --update-roster --audit-photos -- --years 10
  echo "=== WKBL refresh end: $(date -u +%Y-%m-%dT%H:%M:%SZ) ==="
} >> "$LOG_FILE" 2>&1
