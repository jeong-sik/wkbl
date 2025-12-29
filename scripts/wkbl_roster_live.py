#!/usr/bin/env python3
"""
Build roster_db.json from WKBL live player list pages.
"""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
from datetime import datetime, timezone
import json
import re
from pathlib import Path
from typing import Iterable

import requests
from bs4 import BeautifulSoup


ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_OUTPUT = ROOT_DIR / "data" / "wkbl" / "roster_db.json"
BASE_URL = "https://www.wkbl.or.kr/player/player_list.asp"
DEFAULT_GROUPS = ["12", "11", "F11"]  # active, retired, foreign

TEAM_ALIAS = {
    "BNK 썸": "BNK썸",
    "BNK  썸": "BNK썸",
    "KB 스타즈": "KB스타즈",
    "우리 은행": "우리은행",
    "하나 은행": "하나은행",
    "신한 은행": "신한은행",
    "삼성 생명": "삼성생명",
}


def normalize_team(team: str) -> str:
    team = str(team).strip()
    return TEAM_ALIAS.get(team, team)


def split_name_team(text: str) -> tuple[str, str]:
    value = str(text).strip()
    if not value:
        return "", ""
    if "[" in value and value.endswith("]"):
        name, team = value.rsplit("[", 1)
        return name.strip(), normalize_team(team.rstrip("]").strip())
    return value, ""


def fetch_group(group: str, timeout: int = 20) -> list[tuple[str, str, str]]:
    params = {"player_group": group, "tcode": ""}
    resp = requests.get(BASE_URL, params=params, timeout=timeout)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "html.parser")
    players: list[tuple[str, str, str]] = []
    for link in soup.select("ul.list_player a.link_detail"):
        href = link.get("href", "")
        match = re.search(r"pno=(\d+)", href)
        if not match:
            continue
        name, team = split_name_team(link.get_text(strip=True))
        if not name:
            continue
        players.append((team, name, match.group(1)))
    return players


def seed_counts_from_existing(
    payload: dict,
    name_team_counts: dict[str, Counter],
    name_counts: dict[str, Counter],
    team_counts: dict[str, Counter],
) -> None:
    for key, pno in payload.get("photo_by_name_team", {}).items():
        name_team_counts[key][pno] += 1
        if "|" in key:
            name, team = key.split("|", 1)
            team_counts[name][team] += 1
    for name, pno in payload.get("photo_by_name", {}).items():
        name_counts[name][pno] += 1
    for name, team in payload.get("team_by_name", {}).items():
        team_counts[name][team] += 1


def build_payload(players: Iterable[tuple[str, str, str]], output: Path, merge_existing: bool) -> dict:
    name_team_counts: dict[str, Counter] = defaultdict(Counter)
    name_counts: dict[str, Counter] = defaultdict(Counter)
    team_counts: dict[str, Counter] = defaultdict(Counter)

    if merge_existing and output.exists():
        try:
            existing = json.loads(output.read_text(encoding="utf-8"))
            if isinstance(existing, dict):
                seed_counts_from_existing(existing, name_team_counts, name_counts, team_counts)
        except Exception:
            pass

    for team, name, pno in players:
        key = f"{name}|{team}" if team else name
        if team:
            name_team_counts[key][pno] += 1
            team_counts[name][team] += 1
        name_counts[name][pno] += 1

    photo_by_name_team = {
        key: counter.most_common(1)[0][0] for key, counter in name_team_counts.items()
    }
    photo_by_name = {
        name: counter.most_common(1)[0][0] for name, counter in name_counts.items()
    }
    team_by_name = {
        name: counter.most_common(1)[0][0] for name, counter in team_counts.items()
    }

    return {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "photo_by_name_team": photo_by_name_team,
        "photo_by_name": photo_by_name,
        "team_by_name": team_by_name,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--groups", nargs="*", default=DEFAULT_GROUPS)
    parser.add_argument("--no-merge", action="store_true", help="Do not merge with existing roster_db.json.")
    parser.add_argument("--timeout", type=int, default=20)
    args = parser.parse_args()

    players: list[tuple[str, str, str]] = []
    for group in args.groups:
        players.extend(fetch_group(group, timeout=args.timeout))

    if not players:
        raise SystemExit("no players parsed from live list")

    payload = build_payload(players, args.output, merge_existing=not args.no_merge)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"saved roster db to {args.output} (players={len(players)})")


if __name__ == "__main__":
    main()
