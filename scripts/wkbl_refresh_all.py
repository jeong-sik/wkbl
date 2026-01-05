#!/usr/bin/env python3
"""
Refresh WKBL data pipeline: collect -> aggregate -> QA.
Use `--` to pass arguments through to the collector.
Example: python3 scripts/wkbl_refresh_all.py -- --years 10
Example (+/-): python3 scripts/wkbl_refresh_all.py --sync-pbp --pbp-season 046 --compute-plus-minus -- --years 1
"""

from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import sys


ROOT_DIR = Path(__file__).resolve().parents[1]


def run_step(cmd: list[str]) -> None:
    print(">>", " ".join(cmd))
    subprocess.run(cmd, check=True, cwd=ROOT_DIR)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--skip-collect", action="store_true", help="Skip data collection step.")
    parser.add_argument("--skip-aggregate", action="store_true", help="Skip aggregation step.")
    parser.add_argument("--skip-qa", action="store_true", help="Skip QA report step.")
    parser.add_argument("--update-roster", action="store_true", help="Refresh roster_db.json from WKBL live lists.")
    parser.add_argument("--audit-photos", action="store_true", help="Build photo blacklist for missing images.")
    parser.add_argument("--include-special", action="store_true", help="Include all-star/international games in aggregates.")
    parser.add_argument("--sync-pbp", action="store_true", help="Sync text play-by-play (PBP) into SQLite.")
    parser.add_argument(
        "--pbp-season",
        default="",
        help="Season code for PBP sync (e.g. 046). Empty = all seasons.",
    )
    parser.add_argument(
        "--pbp-limit",
        type=int,
        default=0,
        help="Limit number of games processed for PBP sync (0=all).",
    )
    parser.add_argument("--compute-plus-minus", action="store_true", help="Compute per-game +/- from stored PBP.")
    parser.add_argument("--qa-meta-file", default="", help="Optional meta file path for QA.")
    parser.add_argument("collector_args", nargs=argparse.REMAINDER, help="Args passed to wkbl_ajax_collector.py.")
    args = parser.parse_args()

    python = sys.executable or "python3"
    collector_args = list(args.collector_args)
    if collector_args and collector_args[0] == "--":
        collector_args = collector_args[1:]

    if not args.skip_collect:
        cmd = [python, "scripts/wkbl_ajax_collector.py"]
        if collector_args:
            cmd.extend(collector_args)
        run_step(cmd)

    if args.sync_pbp:
        cmd = [python, "scripts/wkbl_pbp_sync.py"]
        if args.pbp_season.strip():
            cmd.extend(["--season", args.pbp_season.strip()])
        if args.pbp_limit and args.pbp_limit > 0:
            cmd.extend(["--limit", str(args.pbp_limit)])
        if args.include_special:
            cmd.append("--include-special")
        run_step(cmd)

    if not args.skip_aggregate:
        cmd = [python, "scripts/wkbl_aggregate_stats.py"]
        if args.include_special:
            cmd.append("--include-special")
        run_step(cmd)

    if args.compute_plus_minus:
        run_step([python, "scripts/wkbl_plus_minus_from_pbp.py"])

    if not args.skip_qa:
        cmd = [python, "scripts/wkbl_data_qa.py"]
        if args.qa_meta_file:
            cmd.extend(["--meta-file", args.qa_meta_file])
        run_step(cmd)

    if args.update_roster:
        run_step([python, "scripts/wkbl_roster_live.py"])

    if args.audit_photos:
        run_step([python, "scripts/wkbl_photo_audit.py"])


if __name__ == "__main__":
    main()
