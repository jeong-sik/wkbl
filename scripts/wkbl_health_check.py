#!/usr/bin/env python3
"""
WKBL Data Pipeline Health Check
- Check data freshness
- Validate data integrity
- Send alerts on issues
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path

ROOT_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT_DIR / "data"

# Slack webhook for alerts (from env)
SLACK_WEBHOOK = os.environ.get("WKBL_SLACK_WEBHOOK", "")

# Maximum age in hours before data is considered stale
MAX_STALE_HOURS = 48  # 2 days


def get_file_age_hours(path: Path) -> float | None:
    """Get file age in hours, None if file doesn't exist."""
    if not path.exists():
        return None
    mtime = datetime.fromtimestamp(path.stat().st_mtime)
    age = datetime.now() - mtime
    return age.total_seconds() / 3600


def check_data_freshness() -> tuple[bool, list[str]]:
    """Check if key data files are fresh enough."""
    # Check both SQLite DB and key JSON files in data/wkbl/ subdirectory
    critical_files = [
        ("wkbl.db", MAX_STALE_HOURS),  # Main SQLite database
        ("wkbl/roster_db.json", MAX_STALE_HOURS),  # Roster data
        ("wkbl/qa/qa_report.json", MAX_STALE_HOURS * 2),  # QA report can be older
    ]

    issues: list[str] = []
    all_ok = True

    for filename, max_hours in critical_files:
        path = DATA_DIR / filename
        age = get_file_age_hours(path)

        if age is None:
            issues.append(f"Missing: {filename}")
            all_ok = False
        elif age > max_hours:
            issues.append(f"Stale: {filename} ({age:.1f}h > {max_hours}h)")
            all_ok = False

    return all_ok, issues


def check_launchd_status() -> tuple[bool, str]:
    """Check if wkbl-refresh launchd service is running."""
    try:
        result = subprocess.run(
            ["launchctl", "list"],
            capture_output=True,
            text=True,
            timeout=5
        )
        if "com.jeongsik.wkbl-refresh" in result.stdout:
            return True, "launchd service loaded"
        else:
            return False, "launchd service NOT loaded"
    except Exception as e:
        return False, f"launchd check failed: {e}"


def check_db_connection() -> tuple[bool, str]:
    """Quick check if Postgres is reachable (via psql or similar)."""
    db_url = os.environ.get("DATABASE_URL", "")
    if not db_url:
        # Not critical for local dev - SQLite is used locally
        return True, "DATABASE_URL not set (local mode, using SQLite)"

    # Simple connection test using Python
    try:
        import psycopg2
        conn = psycopg2.connect(db_url, connect_timeout=5)
        conn.close()
        return True, "Postgres connected"
    except ImportError:
        return True, "psycopg2 not available (skipped)"
    except Exception as e:
        return False, f"Postgres error: {str(e)[:50]}"


def send_slack_alert(message: str) -> None:
    """Send alert to Slack webhook if configured."""
    if not SLACK_WEBHOOK:
        print("(No SLACK_WEBHOOK configured, skipping alert)")
        return

    try:
        import urllib.request
        data = json.dumps({"text": f"🚨 WKBL Health Check Alert:\n{message}"}).encode()
        req = urllib.request.Request(
            SLACK_WEBHOOK,
            data=data,
            headers={"Content-Type": "application/json"}
        )
        urllib.request.urlopen(req, timeout=10)
        print("Slack alert sent")
    except Exception as e:
        print(f"Failed to send Slack alert: {e}")


def run_health_check() -> int:
    """Run all health checks and return exit code."""
    print(f"=== WKBL Health Check ({datetime.now().isoformat()}) ===\n")

    all_ok = True
    alerts: list[str] = []

    # Check 1: Data freshness
    fresh_ok, fresh_issues = check_data_freshness()
    if fresh_ok:
        print("✅ Data freshness: OK")
    else:
        print("❌ Data freshness: ISSUES")
        for issue in fresh_issues:
            print(f"   - {issue}")
        alerts.extend(fresh_issues)
        all_ok = False

    # Check 2: launchd status
    launchd_ok, launchd_msg = check_launchd_status()
    if launchd_ok:
        print(f"✅ Scheduler: {launchd_msg}")
    else:
        print(f"⚠️  Scheduler: {launchd_msg}")
        # Not critical - might be running manually

    # Check 3: Database
    db_ok, db_msg = check_db_connection()
    if db_ok:
        print(f"✅ Database: {db_msg}")
    else:
        print(f"❌ Database: {db_msg}")
        alerts.append(db_msg)
        all_ok = False

    print()

    # Send alert if issues found
    if not all_ok and alerts:
        send_slack_alert("\n".join(alerts))

    # Summary
    if all_ok:
        print("✅ All checks passed")
        return 0
    else:
        print("❌ Some checks failed")
        return 1


def main() -> None:
    import argparse
    parser = argparse.ArgumentParser(description="WKBL data pipeline health check")
    parser.add_argument("--quiet", "-q", action="store_true", help="Only output on failure")
    parser.add_argument("--json", action="store_true", help="Output as JSON")
    args = parser.parse_args()

    if args.json:
        fresh_ok, fresh_issues = check_data_freshness()
        launchd_ok, launchd_msg = check_launchd_status()
        db_ok, db_msg = check_db_connection()

        result = {
            "timestamp": datetime.now().isoformat(),
            "data_fresh": fresh_ok,
            "data_issues": fresh_issues,
            "launchd_ok": launchd_ok,
            "launchd_msg": launchd_msg,
            "db_ok": db_ok,
            "db_msg": db_msg,
            "overall_ok": fresh_ok and db_ok,
        }
        print(json.dumps(result, indent=2))
        sys.exit(0 if result["overall_ok"] else 1)

    exit_code = run_health_check()

    if args.quiet and exit_code == 0:
        pass  # Silent on success

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
