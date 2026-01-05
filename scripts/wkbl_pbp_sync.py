#!/usr/bin/env python3
from __future__ import annotations

import argparse
import shutil
import sqlite3
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable
from xml.etree import ElementTree as ET

import requests

ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_DB_PATH = ROOT_DIR / "data" / "wkbl.db"

PBP_URL = "https://www.wkbl.or.kr/live11/path_live_sms.asp"
REQUEST_TIMEOUT_SEC = 15
REQUEST_DELAY_SEC = 0.5
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Accept": "*/*",
    "Accept-Language": "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7",
}

PERIOD_CODES = ("Q1", "Q2", "Q3", "Q4", "X1", "X2", "X3", "X4")


@dataclass(frozen=True)
class GameKey:
    game_id: str
    season_gu0: str
    game_type0: str
    game_no0: str


@dataclass(frozen=True)
class PbpEvent:
    period_code: str
    team_side: int
    description: str
    team1_score: int | None
    team2_score: int | None
    clock: str


def ensure_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS play_by_play_events (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          game_id TEXT NOT NULL,
          period_code TEXT NOT NULL,
          event_index INTEGER NOT NULL,
          team_side INTEGER NOT NULL,
          description TEXT NOT NULL,
          team1_score INTEGER,
          team2_score INTEGER,
          clock TEXT NOT NULL,
          UNIQUE (game_id, period_code, event_index)
        );

        CREATE INDEX IF NOT EXISTS idx_pbp_game_id ON play_by_play_events(game_id);
        CREATE INDEX IF NOT EXISTS idx_pbp_game_period ON play_by_play_events(game_id, period_code);
        """
    )


def parse_playhistory_text(text: str) -> PbpEvent | None:
    parts = text.split("|")
    if len(parts) != 6:
        return None
    period_code, side_str, desc, t1_str, t2_str, clock = parts
    try:
        team_side = int(side_str)
    except ValueError:
        return None

    team1_score = int(t1_str) if t1_str.strip() else None
    team2_score = int(t2_str) if t2_str.strip() else None
    return PbpEvent(
        period_code=period_code.strip(),
        team_side=team_side,
        description=desc.strip(),
        team1_score=team1_score,
        team2_score=team2_score,
        clock=clock.strip(),
    )


def fetch_period_events(game: GameKey, period_code: str) -> list[PbpEvent]:
    params = {
        "season_gu0": game.season_gu0,
        "game_type0": game.game_type0,
        "game_no0": game.game_no0,
        "quarter_gu0": period_code,
        "seq0": "0",
    }
    resp = requests.get(PBP_URL, params=params, headers=HEADERS, timeout=REQUEST_TIMEOUT_SEC)
    resp.raise_for_status()
    resp.encoding = "utf-8"

    root = ET.fromstring(resp.text)
    events: list[PbpEvent] = []
    for ph in root.findall("./playhistory"):
        if ph.text is None:
            continue
        parsed = parse_playhistory_text(ph.text)
        if parsed is None:
            continue
        events.append(parsed)
    return events


def iter_games(
    conn: sqlite3.Connection,
    *,
    season: str | None,
    game_id: str | None,
    include_special: bool,
) -> Iterable[GameKey]:
    cursor = conn.cursor()
    where = []
    params: list[object] = []

    if game_id:
        where.append("g.game_id = ?")
        params.append(game_id)

    if season:
        where.append("g.season_code = ?")
        params.append(season)

    if not include_special:
        where.append("g.game_type != '10'")

    where_sql = ("WHERE " + " AND ".join(where)) if where else ""
    cursor.execute(
        f"""
        SELECT g.game_id, g.season_code, g.game_type, g.game_no
        FROM games g
        {where_sql}
        ORDER BY g.game_date DESC, g.game_id DESC
        """,
        params,
    )
    for game_id_row, season_code, game_type, game_no in cursor.fetchall():
        yield GameKey(
            game_id=str(game_id_row),
            season_gu0=str(season_code),
            game_type0=str(game_type),
            game_no0=str(game_no),
        )


def main() -> int:
    parser = argparse.ArgumentParser(description="Sync WKBL play-by-play (text live) into SQLite.")
    parser.add_argument("--db", type=Path, default=DEFAULT_DB_PATH)
    parser.add_argument("--season", default="", help="Season code (e.g. 046). Empty = all.")
    parser.add_argument("--game-id", default="", help="Single game_id (e.g. 046-01-37).")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of games processed (0=all).")
    parser.add_argument("--dry-run", action="store_true", help="Fetch and report without writing DB.")
    parser.add_argument("--backup", action="store_true", help="Create a .bak copy next to the DB before edits.")
    parser.add_argument("--include-special", action="store_true", help="Include all-star/international games.")
    args = parser.parse_args()

    if not args.db.exists():
        raise SystemExit(f"DB not found: {args.db}")

    if args.backup and not args.dry_run:
        backup_path = args.db.with_suffix(args.db.suffix + ".bak")
        shutil.copy2(args.db, backup_path)
        print(f"Backup created: {backup_path}")

    season = args.season.strip() or None
    game_id = args.game_id.strip() or None

    conn = sqlite3.connect(args.db)
    try:
        ensure_schema(conn)

        games = list(iter_games(conn, season=season, game_id=game_id, include_special=args.include_special))
        if args.limit and args.limit > 0:
            games = games[: args.limit]

        if not games:
            print("No games found.")
            return 0

        for idx, g in enumerate(games, start=1):
            print(f"[{idx}/{len(games)}] {g.game_id} ...", end=" ")
            inserted = 0

            if not args.dry_run:
                conn.execute("DELETE FROM play_by_play_events WHERE game_id = ?", (g.game_id,))

            for period in PERIOD_CODES:
                try:
                    events = fetch_period_events(g, period)
                except Exception:
                    continue

                if not events:
                    continue

                if not args.dry_run:
                    conn.executemany(
                        """
                        INSERT OR REPLACE INTO play_by_play_events(
                          game_id, period_code, event_index, team_side, description, team1_score, team2_score, clock
                        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                        """,
                        [
                            (
                                g.game_id,
                                e.period_code,
                                i,
                                e.team_side,
                                e.description,
                                e.team1_score,
                                e.team2_score,
                                e.clock,
                            )
                            for i, e in enumerate(events)
                        ],
                    )
                inserted += len(events)

                time.sleep(REQUEST_DELAY_SEC)

            if not args.dry_run:
                conn.commit()

            print(f"events={inserted}")

        return 0
    finally:
        conn.close()


if __name__ == "__main__":
    raise SystemExit(main())

