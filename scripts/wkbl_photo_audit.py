#!/usr/bin/env python3
"""
Validate WKBL player photo URLs and build a blacklist for missing images.
"""

from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
import json
from pathlib import Path
from typing import Iterable

import requests


ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_ROSTER = ROOT_DIR / "data" / "wkbl" / "roster_db.json"
DEFAULT_OUTPUT = ROOT_DIR / "data" / "wkbl" / "photo_blacklist.json"
BASE_URL = "https://www.wkbl.or.kr/static/images/player/pimg/m_{pno}.jpg?ver=0.3"


def load_pnos(roster_path: Path) -> list[str]:
    data = json.loads(roster_path.read_text(encoding="utf-8"))
    pnos = set()
    for value in data.get("photo_by_name_team", {}).values():
        pnos.add(str(value))
    for value in data.get("photo_by_name", {}).values():
        pnos.add(str(value))
    return sorted(pnos)


def check_photo(pno: str, timeout: int) -> dict:
    url = BASE_URL.format(pno=pno)
    try:
        resp = requests.head(url, timeout=timeout, allow_redirects=True)
        content_type = resp.headers.get("content-type", "")
        ok = resp.status_code == 200 and content_type.startswith("image/")
        if not ok and resp.status_code == 200:
            # Some servers respond 200 with HTML; retry GET to confirm.
            resp = requests.get(url, timeout=timeout, stream=True)
            content_type = resp.headers.get("content-type", "")
            ok = resp.status_code == 200 and content_type.startswith("image/")
    except Exception as exc:
        return {"pno": pno, "ok": False, "status": "error", "content_type": "", "error": str(exc)}

    status = "ok" if ok else str(resp.status_code)
    return {"pno": pno, "ok": ok, "status": status, "content_type": content_type}


def iter_chunks(items: list[str], size: int) -> Iterable[list[str]]:
    for idx in range(0, len(items), size):
        yield items[idx : idx + size]


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--roster", type=Path, default=DEFAULT_ROSTER)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--timeout", type=int, default=10)
    parser.add_argument("--workers", type=int, default=12)
    parser.add_argument("--limit", type=int, default=0)
    args = parser.parse_args()

    if not args.roster.exists():
        raise SystemExit(f"roster not found: {args.roster}")

    pnos = load_pnos(args.roster)
    if args.limit:
        pnos = pnos[: args.limit]

    results = []
    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = {pool.submit(check_photo, pno, args.timeout): pno for pno in pnos}
        for future in as_completed(futures):
            results.append(future.result())

    invalid = [row for row in results if not row.get("ok")]
    invalid_pnos = sorted({row["pno"] for row in invalid})
    payload = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "total": len(results),
        "valid": len(results) - len(invalid_pnos),
        "invalid": len(invalid_pnos),
        "invalid_pnos": invalid_pnos,
        "invalid_samples": invalid[:50],
    }

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"saved photo blacklist to {args.output} (invalid={len(invalid_pnos)})")


if __name__ == "__main__":
    main()
