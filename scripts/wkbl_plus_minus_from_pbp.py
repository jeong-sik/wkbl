#!/usr/bin/env python3
"""
Compute per-game +/- from stored PBP (play-by-play) data.
Supports both SQLite (local) and PostgreSQL (production).
"""
from __future__ import annotations

import argparse
import os
import shutil
import sqlite3
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Protocol, Sequence

# PostgreSQL support (optional)
try:
    import psycopg2
    HAS_PSYCOPG2 = True
except ImportError:
    HAS_PSYCOPG2 = False

ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_DB_PATH = ROOT_DIR / "data" / "wkbl.db"


class DbConnection(Protocol):
    """Database connection protocol for SQLite/PostgreSQL abstraction."""
    def cursor(self) -> Any: ...
    def execute(self, sql: str, params: Sequence = ()) -> Any: ...
    def executemany(self, sql: str, params_list: Sequence) -> Any: ...
    def commit(self) -> None: ...
    def close(self) -> None: ...


class SqliteWrapper:
    """SQLite connection wrapper."""
    def __init__(self, path: Path):
        self.conn = sqlite3.connect(path)
        self.placeholder = "?"

    def cursor(self):
        return self.conn.cursor()

    def execute(self, sql: str, params: Sequence = ()):
        return self.conn.execute(sql, params)

    def executemany(self, sql: str, params_list: Sequence):
        return self.conn.executemany(sql, params_list)

    def executescript(self, sql: str):
        return self.conn.executescript(sql)

    def commit(self):
        self.conn.commit()

    def close(self):
        self.conn.close()


class PostgresWrapper:
    """PostgreSQL connection wrapper."""
    def __init__(self, db_url: str):
        if not HAS_PSYCOPG2:
            raise ImportError("psycopg2 is required for PostgreSQL support: pip install psycopg2-binary")
        self.conn = psycopg2.connect(db_url)
        self.placeholder = "%s"

    def cursor(self):
        return self.conn.cursor()

    def execute(self, sql: str, params: Sequence = ()):
        cursor = self.conn.cursor()
        cursor.execute(sql, params)
        return cursor

    def executemany(self, sql: str, params_list: Sequence):
        cursor = self.conn.cursor()
        cursor.executemany(sql, params_list)
        return cursor

    def executescript(self, sql: str):
        """Execute multiple SQL statements."""
        cursor = self.conn.cursor()
        for stmt in sql.split(';'):
            stmt = stmt.strip()
            if stmt:
                cursor.execute(stmt)
        return cursor

    def commit(self):
        self.conn.commit()

    def close(self):
        self.conn.close()


def q(sql: str, placeholder: str) -> str:
    """Convert SQLite placeholder (?) to target placeholder (%s for PostgreSQL)."""
    if placeholder == "?":
        return sql
    return sql.replace("?", placeholder)


PERIOD_ORDER = {
    "Q1": 1,
    "Q2": 2,
    "Q3": 3,
    "Q4": 4,
    "X1": 5,
    "X2": 6,
    "X3": 7,
    "X4": 8,
}


@dataclass(frozen=True)
class PbpRow:
    period_code: str
    event_index: int
    team_side: int
    description: str
    team1_score: int | None
    team2_score: int | None
    clock: str


def ensure_schema(conn: SqliteWrapper | PostgresWrapper) -> None:
    """Ensure player_plus_minus table exists."""
    conn.executescript(
        """
        CREATE TABLE IF NOT EXISTS player_plus_minus (
          game_id TEXT NOT NULL,
          player_id TEXT NOT NULL,
          plus_minus INTEGER NOT NULL,
          PRIMARY KEY (game_id, player_id)
        );

        CREATE INDEX IF NOT EXISTS idx_player_plus_minus_player_id ON player_plus_minus(player_id);
        """
    )


def load_game_teams(conn: SqliteWrapper | PostgresWrapper, game_id: str) -> tuple[str, str]:
    cursor = conn.cursor()
    cursor.execute(
        q("SELECT home_team_code, away_team_code FROM games WHERE game_id = ?", conn.placeholder),
        (game_id,),
    )
    row = cursor.fetchone()
    if row is None:
        raise ValueError(f"Game not found: {game_id}")
    home_team_code, away_team_code = row
    return str(home_team_code), str(away_team_code)

def load_official_final_scores(conn: SqliteWrapper | PostgresWrapper, game_id: str) -> tuple[int, int]:
    cursor = conn.cursor()
    cursor.execute(
        q("""
        SELECT
          COALESCE(
            g.away_score,
            (SELECT SUM(s2.pts) FROM game_stats s2 WHERE s2.game_id = g.game_id AND s2.team_code = g.away_team_code)
          ) AS away_score,
          COALESCE(
            g.home_score,
            (SELECT SUM(s2.pts) FROM game_stats s2 WHERE s2.game_id = g.game_id AND s2.team_code = g.home_team_code)
          ) AS home_score
        FROM games g
        WHERE g.game_id = ?
        """, conn.placeholder),
        (game_id,),
    )
    row = cursor.fetchone()
    if row is None:
        raise ValueError(f"Game not found: {game_id}")
    away_score, home_score = row
    if away_score is None or home_score is None:
        raise ValueError(f"Official score missing for game: {game_id}")
    return int(away_score), int(home_score)


def load_team_player_ids(conn: SqliteWrapper | PostgresWrapper, *, game_id: str, team_code: str) -> list[str]:
    cursor = conn.cursor()
    cursor.execute(
        q("""
        SELECT s.player_id
        FROM game_stats s
        WHERE s.game_id = ? AND s.team_code = ?
        """, conn.placeholder),
        (game_id, team_code),
    )
    return [str(pid) for (pid,) in cursor.fetchall()]


def load_team_minutes(conn: SqliteWrapper | PostgresWrapper, *, game_id: str, team_code: str) -> dict[str, int]:
    cursor = conn.cursor()
    cursor.execute(
        q("""
        SELECT player_id, COALESCE(min_seconds, 0)
        FROM game_stats
        WHERE game_id = ? AND team_code = ?
        """, conn.placeholder),
        (game_id, team_code),
    )
    return {str(pid): int(mins or 0) for pid, mins in cursor.fetchall()}


def load_name_to_player_id(conn: SqliteWrapper | PostgresWrapper, *, game_id: str, team_code: str) -> dict[str, str]:
    cursor = conn.cursor()
    cursor.execute(
        q("""
        SELECT p.player_id, p.player_name, COALESCE(SUM(s.min_seconds), 0) AS total_min_seconds
        FROM game_stats s
        JOIN players p ON p.player_id = s.player_id
        WHERE s.game_id = ? AND s.team_code = ?
        GROUP BY p.player_id, p.player_name
        """, conn.placeholder),
        (game_id, team_code),
    )

    # If multiple player_ids share the same name in a game/team, keep the one with more minutes.
    best: dict[str, tuple[str, int]] = {}
    for player_id, player_name, total_min_seconds in cursor.fetchall():
        name = str(player_name).strip()
        pid = str(player_id)
        mins = int(total_min_seconds or 0)
        prev = best.get(name)
        if prev is None or mins > prev[1] or (mins == prev[1] and pid > prev[0]):
            best[name] = (pid, mins)
    return {name: pid for name, (pid, _mins) in best.items()}


def load_pbp_rows(conn: SqliteWrapper | PostgresWrapper, game_id: str) -> list[PbpRow]:
    cursor = conn.cursor()
    cursor.execute(
        q("""
        SELECT period_code, event_index, team_side, description, team1_score, team2_score, clock
        FROM play_by_play_events
        WHERE game_id = ?
        """, conn.placeholder),
        (game_id,),
    )
    rows: list[PbpRow] = []
    for period_code, event_index, team_side, description, t1, t2, clock in cursor.fetchall():
        rows.append(
            PbpRow(
                period_code=str(period_code),
                event_index=int(event_index),
                team_side=int(team_side),
                description=str(description),
                team1_score=int(t1) if t1 is not None else None,
                team2_score=int(t2) if t2 is not None else None,
                clock=str(clock),
            )
        )
    rows.sort(key=lambda r: (PERIOD_ORDER.get(r.period_code, 999), r.event_index))
    return rows


def extract_player_name(description: str, known_names: list[str]) -> str | None:
    if not description:
        return None
    # Player name is a prefix of the description, but can contain spaces
    # (e.g. "사카이 사라 3점슛성공"). Prefer longest match.
    for name in known_names:
        if description.startswith(name + " "):
            return name
    return None


def compute_plus_minus_for_game(
    conn: SqliteWrapper | PostgresWrapper,
    *,
    game_id: str,
) -> dict[str, int] | None:
    try:
        home_team_code, away_team_code = load_game_teams(conn, game_id)
    except ValueError as exc:
        print(f"WARNING {game_id}: {exc}; skipping +/-")
        return None

    if not home_team_code.strip() or not away_team_code.strip() or home_team_code == away_team_code:
        print(
            f"WARNING {game_id}: invalid teams home={home_team_code!r} away={away_team_code!r}; skipping +/-"
        )
        return None

    # Live PBP uses team_side=1 as "left" (away), team_side=2 as "right" (home).
    team1_code = away_team_code
    team2_code = home_team_code

    team1_players = load_team_player_ids(conn, game_id=game_id, team_code=team1_code)
    team2_players = load_team_player_ids(conn, game_id=game_id, team_code=team2_code)
    if len(team1_players) < 5 or len(team2_players) < 5:
        print(
            f"WARNING {game_id}: roster incomplete team1={len(team1_players)} team2={len(team2_players)}; skipping +/-"
        )
        return None

    minutes_1 = load_team_minutes(conn, game_id=game_id, team_code=team1_code)
    minutes_2 = load_team_minutes(conn, game_id=game_id, team_code=team2_code)
    minutes_all = {**minutes_1, **minutes_2}
    priority_1 = sorted(team1_players, key=lambda pid: (-minutes_1.get(pid, 0), pid), reverse=False)
    priority_2 = sorted(team2_players, key=lambda pid: (-minutes_2.get(pid, 0), pid), reverse=False)
    name_to_id_1 = load_name_to_player_id(conn, game_id=game_id, team_code=team1_code)
    name_to_id_2 = load_name_to_player_id(conn, game_id=game_id, team_code=team2_code)
    known_names_1 = sorted(name_to_id_1.keys(), key=len, reverse=True)
    known_names_2 = sorted(name_to_id_2.keys(), key=len, reverse=True)

    pbp = load_pbp_rows(conn, game_id)
    if not pbp:
        return None
    if not pbp:
        raise ValueError(f"No PBP rows found for game: {game_id}")

    # Starter inference from early non-IN events.
    starters1: list[str] = []
    starters2: list[str] = []
    for row in pbp:
        if "교체(IN)" in row.description:
            continue
        player_name = (
            extract_player_name(row.description, known_names_1)
            if row.team_side == 1
            else extract_player_name(row.description, known_names_2)
            if row.team_side == 2
            else None
        )
        if player_name is None:
            continue
        if row.team_side == 1 and len(starters1) < 5:
            pid = name_to_id_1.get(player_name)
            if pid and pid not in starters1:
                starters1.append(pid)
        elif row.team_side == 2 and len(starters2) < 5:
            pid = name_to_id_2.get(player_name)
            if pid and pid not in starters2:
                starters2.append(pid)
        if len(starters1) == 5 and len(starters2) == 5:
            break

    def fill_with_most_minutes(starters: list[str], *, team_code: str) -> list[str]:
        if len(starters) >= 5:
            return starters
        cursor = conn.cursor()
        cursor.execute(
            """
            SELECT s.player_id, COALESCE(s.min_seconds, 0)
            FROM game_stats s
            WHERE s.game_id = ? AND s.team_code = ?
            ORDER BY COALESCE(s.min_seconds, 0) DESC, s.player_id DESC
            """,
            (game_id, team_code),
        )
        for pid, _mins in cursor.fetchall():
            p = str(pid)
            if p not in starters:
                starters.append(p)
            if len(starters) >= 5:
                break
        return starters

    starters1 = fill_with_most_minutes(starters1, team_code=team1_code)
    starters2 = fill_with_most_minutes(starters2, team_code=team2_code)

    lineup1 = set(starters1[:5])
    lineup2 = set(starters2[:5])

    def choose_remove_candidate(lineup: set[str], seen: set[str], minutes: dict[str, int]) -> str:
        candidates = [pid for pid in lineup if pid not in seen]
        if not candidates:
            candidates = list(lineup)
        return min(candidates, key=lambda pid: (minutes.get(pid, 0), pid))

    def normalize_lineup(lineup: set[str], seen: set[str], *, priority: list[str], minutes: dict[str, int]) -> None:
        while len(lineup) > 5:
            lineup.remove(choose_remove_candidate(lineup, seen, minutes))
        if len(lineup) < 5:
            for pid in priority:
                if pid not in lineup:
                    lineup.add(pid)
                if len(lineup) >= 5:
                    break

    def swap_in_player(lineup: set[str], seen: set[str], pid: str, *, priority: list[str], minutes: dict[str, int]) -> None:
        if pid in lineup:
            return
        if len(lineup) < 5:
            lineup.add(pid)
            return
        # Quarter breaks may omit substitution logs. Replace a low-minute player that hasn't appeared in this period.
        lineup.remove(choose_remove_candidate(lineup, seen, minutes))
        lineup.add(pid)

    plus_minus: dict[str, int] = {pid: 0 for pid in set(team1_players + team2_players)}
    score1 = 0
    score2 = 0

    current_period: str | None = None
    seen_1: set[str] = set()
    seen_2: set[str] = set()

    for row in pbp:
        if row.period_code != current_period:
            current_period = row.period_code
            seen_1 = set()
            seen_2 = set()

        # Substitutions
        player_name = (
            extract_player_name(row.description, known_names_1)
            if row.team_side == 1
            else extract_player_name(row.description, known_names_2)
            if row.team_side == 2
            else None
        )
        if player_name is not None:
            is_in = "교체(IN)" in row.description
            is_out = "교체(OUT)" in row.description
            if row.team_side == 1:
                pid = name_to_id_1.get(player_name)
                if pid:
                    seen_1.add(pid)
                    if is_out:
                        lineup1.discard(pid)
                    elif is_in:
                        lineup1.add(pid)
                    else:
                        swap_in_player(lineup1, seen_1, pid, priority=priority_1, minutes=minutes_all)
            elif row.team_side == 2:
                pid = name_to_id_2.get(player_name)
                if pid:
                    seen_2.add(pid)
                    if is_out:
                        lineup2.discard(pid)
                    elif is_in:
                        lineup2.add(pid)
                    else:
                        swap_in_player(lineup2, seen_2, pid, priority=priority_2, minutes=minutes_all)

        # Score deltas (running score; blanks mean "unchanged")
        new_score1 = score1 if row.team1_score is None else row.team1_score
        new_score2 = score2 if row.team2_score is None else row.team2_score
        delta1 = new_score1 - score1
        delta2 = new_score2 - score2
        score1, score2 = new_score1, new_score2

        if delta1 == 0 and delta2 == 0:
            continue

        # Ensure we always apply +/- to 5-on-5 (normalize around quarter breaks / missing subs).
        normalize_lineup(lineup1, seen_1, priority=priority_1, minutes=minutes_all)
        normalize_lineup(lineup2, seen_2, priority=priority_2, minutes=minutes_all)

        net = delta1 - delta2
        for pid in lineup1:
            plus_minus[pid] = plus_minus.get(pid, 0) + net
        for pid in lineup2:
            plus_minus[pid] = plus_minus.get(pid, 0) - net

    # Quick sanity check: sum(team +/-) should be 5 * margin if we tracked 5-on-5 consistently.
    game_margin = score1 - score2
    sum_team1 = sum(plus_minus.get(pid, 0) for pid in team1_players)
    sum_team2 = sum(plus_minus.get(pid, 0) for pid in team2_players)
    expected = 5 * game_margin
    if sum_team1 != expected or sum_team2 != -expected:
        print(
            f"WARNING {game_id}: sum_pm team1={sum_team1} team2={sum_team2} expected team1={expected} team2={-expected}"
        )
        return None

    # Validate final score against official box score. If mismatched, plus/minus can be wrong
    # (missing scoring events or post-game corrections). Better to omit than mislead.
    try:
        official_away, official_home = load_official_final_scores(conn, game_id)
    except ValueError as exc:
        print(f"WARNING {game_id}: {exc}; skipping +/-")
        return None
    if score1 != official_away or score2 != official_home:
        print(
            f"WARNING {game_id}: PBP final {score1}-{score2} != official {official_away}-{official_home}; skipping +/-"
        )
        return None

    return plus_minus


def main() -> int:
    parser = argparse.ArgumentParser(description="Compute per-game +/- from stored PBP (text live) data.")
    parser.add_argument("--db", type=Path, default=DEFAULT_DB_PATH, help="SQLite DB path (default)")
    parser.add_argument("--db-url", default="", help="PostgreSQL connection URL (overrides --db)")
    parser.add_argument("--game-id", default="", help="Single game_id (e.g. 046-01-37). Empty = all games with PBP.")
    parser.add_argument("--dry-run", action="store_true", help="Compute and report without writing DB.")
    parser.add_argument("--backup", action="store_true", help="Create a .bak copy next to the DB before edits (SQLite only).")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of games to process (0 = all)")
    args = parser.parse_args()

    # Determine connection type
    db_url = args.db_url or os.environ.get("WKBL_DATABASE_URL", "")

    if db_url:
        print(f"Using PostgreSQL: {db_url[:50]}...")
        conn: SqliteWrapper | PostgresWrapper = PostgresWrapper(db_url)
        use_postgres = True
    else:
        if not args.db.exists():
            raise SystemExit(f"DB not found: {args.db}")

        if args.backup and not args.dry_run:
            backup_path = args.db.with_suffix(args.db.suffix + ".bak")
            shutil.copy2(args.db, backup_path)
            print(f"Backup created: {backup_path}")

        print(f"Using SQLite: {args.db}")
        conn = SqliteWrapper(args.db)
        use_postgres = False

    try:
        ensure_schema(conn)

        cursor = conn.cursor()
        if args.game_id.strip():
            game_ids = [args.game_id.strip()]
        else:
            cursor.execute("SELECT DISTINCT game_id FROM play_by_play_events ORDER BY game_id DESC")
            game_ids = [str(gid) for (gid,) in cursor.fetchall()]

        if not game_ids:
            print("No games with PBP data found.")
            return 0

        if args.limit > 0:
            game_ids = game_ids[:args.limit]

        print(f"Processing {len(game_ids)} games...")

        for i, game_id in enumerate(game_ids, start=1):
            print(f"[{i}/{len(game_ids)}] {game_id}")
            plus_minus = compute_plus_minus_for_game(conn, game_id=game_id)

            if not args.dry_run:
                # Delete existing data
                ph = conn.placeholder
                conn.execute(q("DELETE FROM player_plus_minus WHERE game_id = ?", ph), (game_id,))

                if plus_minus is not None:
                    if use_postgres:
                        # PostgreSQL: use ON CONFLICT
                        for pid, pm in plus_minus.items():
                            conn.execute(
                                """INSERT INTO player_plus_minus(game_id, player_id, plus_minus)
                                   VALUES (%s, %s, %s)
                                   ON CONFLICT (game_id, player_id) DO UPDATE SET plus_minus = EXCLUDED.plus_minus""",
                                (game_id, pid, pm),
                            )
                    else:
                        # SQLite: use INSERT OR REPLACE
                        conn.executemany(
                            "INSERT OR REPLACE INTO player_plus_minus(game_id, player_id, plus_minus) VALUES (?, ?, ?)",
                            [(game_id, pid, pm) for pid, pm in plus_minus.items()],
                        )
                conn.commit()

        return 0
    finally:
        conn.close()


if __name__ == "__main__":
    raise SystemExit(main())
