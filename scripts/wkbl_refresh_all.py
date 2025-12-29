#!/usr/bin/env python3
"""
Refresh WKBL data pipeline: collect -> aggregate -> QA.
Use `--` to pass arguments through to the collector.
Example: python3 scripts/wkbl_refresh_all.py -- --years 10
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

    if not args.skip_aggregate:
        run_step([python, "scripts/wkbl_aggregate_stats.py"])

    if not args.skip_qa:
        cmd = [python, "scripts/wkbl_data_qa.py"]
        if args.qa_meta_file:
            cmd.extend(["--meta-file", args.qa_meta_file])
        run_step(cmd)

    if args.update_roster:
        run_step([python, "scripts/wkbl_roster_live.py"])


if __name__ == "__main__":
    main()
