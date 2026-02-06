#!/usr/bin/env bash
set -euo pipefail

WORKFLOW_FILE="refresh-data.yml"
REF="main"
SEASON=""
RUN_BOXSCORE="true"

usage() {
  cat <<'USAGE'
Usage: scripts/trigger_github_refresh.sh [--ref BRANCH] [--season CODE] [--no-boxscore]

Examples:
  scripts/trigger_github_refresh.sh
  scripts/trigger_github_refresh.sh --ref main --season 046
  scripts/trigger_github_refresh.sh --no-boxscore
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --ref)
      REF="$2"
      shift 2
      ;;
    --season)
      SEASON="$2"
      shift 2
      ;;
    --no-boxscore)
      RUN_BOXSCORE="false"
      shift 1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if ! command -v gh >/dev/null 2>&1; then
  echo "Error: gh CLI not found. Install and run 'gh auth login' first." >&2
  exit 1
fi

cmd=(gh workflow run "$WORKFLOW_FILE" --ref "$REF")

if [[ -n "$SEASON" ]]; then
  cmd+=(-f "season=$SEASON")
fi

cmd+=(-f "run_boxscore=$RUN_BOXSCORE")

printf "Running: %s\n" "${cmd[*]}"
exec "${cmd[@]}"
