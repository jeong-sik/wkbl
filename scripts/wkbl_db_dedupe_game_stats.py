#!/usr/bin/env python3
from __future__ import annotations

import argparse
import shutil
import sqlite3
from pathlib import Path

ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_DB_PATH = ROOT_DIR / "data" / "wkbl.db"

STAT_COLS = (
    "min_seconds",
    "pts",
    "reb_off",
    "reb_def",
    "reb_tot",
    "ast",
    "stl",
    "blk",
    "tov",
    "pf",
    "fg_2p_m",
    "fg_2p_a",
    "fg_3p_m",
    "fg_3p_a",
    "ft_m",
    "ft_a",
)


def load_total_minutes(conn: sqlite3.Connection) -> dict[str, int]:
    cursor = conn.cursor()
    cursor.execute("SELECT player_id, COALESCE(SUM(min_seconds), 0) FROM game_stats GROUP BY player_id")
    return {player_id: int(total or 0) for player_id, total in cursor.fetchall()}


def find_duplicate_name_rows(conn: sqlite3.Connection) -> list[tuple[str, str, str, int]]:
    cursor = conn.cursor()
    cursor.execute(
        """
        SELECT s.game_id, s.team_code, p.player_name, COUNT(*) as cnt
        FROM game_stats s
        JOIN players p ON p.player_id = s.player_id
        GROUP BY s.game_id, s.team_code, p.player_name
        HAVING cnt > 1
        ORDER BY cnt DESC, s.game_id DESC
        """
    )
    return [(g, t, n, int(c)) for g, t, n, c in cursor.fetchall()]


def fetch_group_rows(
    conn: sqlite3.Connection,
    *,
    game_id: str,
    team_code: str,
    player_name: str,
) -> list[tuple]:
    cursor = conn.cursor()
    cols = ", ".join(f"s.{c}" for c in STAT_COLS)
    cursor.execute(
        f"""
        SELECT s.id, s.player_id, {cols}
        FROM game_stats s
        JOIN players p ON p.player_id = s.player_id
        WHERE s.game_id = ? AND s.team_code = ? AND p.player_name = ?
        ORDER BY s.id
        """,
        (game_id, team_code, player_name),
    )
    return cursor.fetchall()


def stat_line(row: tuple) -> tuple:
    # row: (id, player_id, ...STAT_COLS)
    return row[2:]


def choose_keep_row(rows: list[tuple], total_minutes: dict[str, int]) -> tuple:
    # Keep the player_id with more total minutes in DB (likely the real one).
    # Tie-breaker: lexical player_id (stable), then smallest row id.
    return max(
        rows,
        key=lambda r: (
            total_minutes.get(str(r[1]), 0),
            str(r[1]),
            -int(r[0]),
        ),
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Dedupe duplicate game_stats rows caused by name collisions.")
    parser.add_argument("--db", type=Path, default=DEFAULT_DB_PATH)
    parser.add_argument("--dry-run", action="store_true", help="Print actions without modifying the DB.")
    parser.add_argument("--backup", action="store_true", help="Create a .bak copy next to the DB before edits.")
    args = parser.parse_args()

    if not args.db.exists():
        raise SystemExit(f"DB not found: {args.db}")

    if args.backup and not args.dry_run:
        backup_path = args.db.with_suffix(args.db.suffix + ".bak")
        shutil.copy2(args.db, backup_path)
        print(f"Backup created: {backup_path}")

    conn = sqlite3.connect(args.db)
    try:
        total_minutes = load_total_minutes(conn)
        dup_groups = find_duplicate_name_rows(conn)
        if not dup_groups:
            print("No duplicate (game_id, team_code, player_name) groups found.")
            return 0

        deleted = 0
        for game_id, team_code, name, cnt in dup_groups:
            rows = fetch_group_rows(conn, game_id=game_id, team_code=team_code, player_name=name)
            if len(rows) < 2:
                continue

            # Only delete when the stat line is exactly identical (very strong signal of bad mapping).
            first = stat_line(rows[0])
            if not all(stat_line(r) == first for r in rows[1:]):
                continue

            keep = choose_keep_row(rows, total_minutes)
            keep_id = int(keep[0])
            keep_player_id = str(keep[1])
            drop_ids = [int(r[0]) for r in rows if int(r[0]) != keep_id]

            print(
                f"[{game_id} team={team_code}] '{name}' x{cnt} -> keep {keep_player_id}, delete rows {drop_ids}"
            )

            if not args.dry_run:
                conn.executemany("DELETE FROM game_stats WHERE id = ?", [(rid,) for rid in drop_ids])
                deleted += len(drop_ids)

        if not args.dry_run:
            conn.commit()
            print(f"Deleted rows: {deleted}")
        else:
            print("Dry run: no changes committed.")

        return 0
    finally:
        conn.close()


if __name__ == "__main__":
    raise SystemExit(main())

