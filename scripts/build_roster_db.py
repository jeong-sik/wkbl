#!/usr/bin/env python3
"""
Build roster_db.json from cached WKBL player HTML.
"""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import json
import re
from pathlib import Path

from bs4 import BeautifulSoup


ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_INPUT = ROOT_DIR / "data" / "raw_crawled"
DEFAULT_OUTPUT = ROOT_DIR / "data" / "wkbl" / "roster_db.json"

TEAM_ALIAS = {
    "BNK 썸": "BNK썸",
    "BNK  썸": "BNK썸",
    "KB 스타즈": "KB스타즈",
    "우리 은행": "우리은행",
    "하나 은행": "하나은행",
    "신한 은행": "신한은행",
    "삼성 생명": "삼성생명",
}


def clean_text(text: str) -> str:
    return str(text).strip()


def normalize_team(team: str) -> str:
    team = clean_text(team)
    return TEAM_ALIAS.get(team, team)


def parse_player_file(path: Path) -> list[tuple[str, str, str]]:
    html = path.read_text(encoding="utf-8", errors="ignore")
    soup = BeautifulSoup(html, "html.parser")
    teams = [normalize_team(t.get_text()) for t in soup.find_all("h4", class_="tit_area")]
    tables = soup.find_all("div", class_="info_table01 type_record")

    players: list[tuple[str, str, str]] = []
    for idx, team in enumerate(teams):
        if not team or idx >= len(tables):
            continue
        table = tables[idx].find("table")
        if not table:
            continue
        rows = table.select("tbody tr")
        for row in rows:
            link = row.find("a", href=re.compile(r"pno="))
            if not link:
                continue
            name = clean_text(link.get_text())
            if not name or name in {"선수", "합계", "Total", "팀합계"}:
                continue
            match = re.search(r"pno=(\d+)", link.get("href", ""))
            if not match:
                continue
            pno = match.group(1)
            players.append((team, name, pno))
    return players


def collect_players(input_dir: Path) -> list[tuple[str, str, str]]:
    players: list[tuple[str, str, str]] = []
    for path in sorted(input_dir.rglob("*_player.html")):
        players.extend(parse_player_file(path))
    return players


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-dir", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()

    if not args.input_dir.exists():
        raise SystemExit(f"input dir not found: {args.input_dir}")

    players = collect_players(args.input_dir)
    if not players:
        raise SystemExit("no players parsed")

    name_team_counts: dict[str, Counter] = defaultdict(Counter)
    name_counts: dict[str, Counter] = defaultdict(Counter)
    team_counts: dict[str, Counter] = defaultdict(Counter)

    for team, name, pno in players:
        key = f"{name}|{team}"
        name_team_counts[key][pno] += 1
        name_counts[name][pno] += 1
        team_counts[name][team] += 1

    photo_by_name_team = {
        key: counter.most_common(1)[0][0] for key, counter in name_team_counts.items()
    }
    photo_by_name = {
        name: counter.most_common(1)[0][0] for name, counter in name_counts.items()
    }
    team_by_name = {
        name: counter.most_common(1)[0][0] for name, counter in team_counts.items()
    }

    payload = {
        "team_by_name": team_by_name,
        "photo_by_name_team": photo_by_name_team,
        "photo_by_name": photo_by_name,
    }

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    print(f"saved roster db to {args.output}")


if __name__ == "__main__":
    main()
