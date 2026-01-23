#!/usr/bin/env python3
"""
Backfill player_id in play_by_play_events table.
Extracts player name from description and matches against players table.
Uses the same logic as wkbl_plus_minus_from_pbp.py for consistency.
"""
from __future__ import annotations

import argparse
import os
from dataclasses import dataclass
from typing import Iterator

try:
    import psycopg2
except ImportError:
    psycopg2 = None


@dataclass(frozen=True)
class GameInfo:
    game_id: str
    home_team_code: str
    away_team_code: str


def get_connection(db_url: str):
    """Create PostgreSQL connection."""
    if psycopg2 is None:
        raise ImportError("psycopg2 is required: pip install psycopg2-binary")
    return psycopg2.connect(db_url)


def iter_games_with_pbp(conn, season: str | None = None) -> Iterator[GameInfo]:
    """Iterate over games that have PBP data."""
    cursor = conn.cursor()
    where_clauses = ["g.home_team_code IS NOT NULL", "g.away_team_code IS NOT NULL"]
    params = []

    if season:
        where_clauses.append("g.season_code = %s")
        params.append(season)

    where_sql = " AND ".join(where_clauses)
    cursor.execute(
        f"""
        SELECT DISTINCT g.game_id, g.home_team_code, g.away_team_code
        FROM games g
        JOIN play_by_play_events p ON p.game_id = g.game_id
        WHERE {where_sql}
        ORDER BY g.game_id
        """,
        params,
    )

    for game_id, home_code, away_code in cursor.fetchall():
        yield GameInfo(
            game_id=str(game_id),
            home_team_code=str(home_code),
            away_team_code=str(away_code),
        )


def load_name_to_player_id(conn, game_id: str, team_code: str) -> dict[str, str]:
    """
    Load player_name -> player_id mapping for a team in a game.
    Prefers player with more minutes if names collide.
    """
    cursor = conn.cursor()
    cursor.execute(
        """
        SELECT p.player_id, p.player_name, COALESCE(SUM(s.min_seconds), 0) AS total_min_seconds
        FROM game_stats s
        JOIN players p ON p.player_id = s.player_id
        WHERE s.game_id = %s AND s.team_code = %s
        GROUP BY p.player_id, p.player_name
        ORDER BY total_min_seconds DESC
        """,
        (game_id, team_code),
    )

    result: dict[str, str] = {}
    for player_id, player_name, _ in cursor.fetchall():
        name = str(player_name).strip()
        if name and name not in result:
            result[name] = str(player_id)

    return result


def extract_player_name(description: str, known_names: list[str]) -> str | None:
    """
    Extract player name from description prefix.
    Known names are sorted by length (longest first) to handle overlapping names.
    """
    if not description:
        return None

    for name in known_names:
        if description.startswith(name + " "):
            return name
    return None


def backfill_player_ids_for_game(conn, game: GameInfo, *, dry_run: bool = False) -> tuple[int, int]:
    """
    Backfill player_id for all PBP events in a game.
    Returns (updated_count, skipped_count).

    Note: team_side=1 is "left" (away), team_side=2 is "right" (home).
    """
    # Build name mappings for both teams
    team1_code = game.away_team_code  # team_side=1 = away
    team2_code = game.home_team_code  # team_side=2 = home

    name_to_id_1 = load_name_to_player_id(conn, game.game_id, team1_code)
    name_to_id_2 = load_name_to_player_id(conn, game.game_id, team2_code)

    # Sort by name length (longest first) for prefix matching
    known_names_1 = sorted(name_to_id_1.keys(), key=len, reverse=True)
    known_names_2 = sorted(name_to_id_2.keys(), key=len, reverse=True)

    # Load PBP events
    cursor = conn.cursor()
    cursor.execute(
        """
        SELECT id, team_side, description
        FROM play_by_play_events
        WHERE game_id = %s AND player_id IS NULL
        """,
        (game.game_id,),
    )

    updates = []
    for pbp_id, team_side, description in cursor.fetchall():
        if team_side == 1:
            known_names = known_names_1
            name_to_id = name_to_id_1
        elif team_side == 2:
            known_names = known_names_2
            name_to_id = name_to_id_2
        else:
            continue  # team_side=0 is system event

        player_name = extract_player_name(str(description), known_names)
        if player_name and player_name in name_to_id:
            updates.append((name_to_id[player_name], pbp_id))

    if not dry_run and updates:
        cursor = conn.cursor()
        cursor.executemany(
            "UPDATE play_by_play_events SET player_id = %s WHERE id = %s",
            updates,
        )
        conn.commit()

    # Count already filled
    cursor = conn.cursor()
    cursor.execute(
        """
        SELECT COUNT(*) FROM play_by_play_events
        WHERE game_id = %s AND player_id IS NOT NULL
        """,
        (game.game_id,),
    )
    already_filled = cursor.fetchone()[0]

    return len(updates), already_filled


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Backfill player_id in play_by_play_events table."
    )
    parser.add_argument(
        "--db-url",
        type=str,
        default=os.environ.get("DATABASE_URL") or os.environ.get("SUPABASE_POSTGRES_URL"),
        help="PostgreSQL DATABASE_URL",
    )
    parser.add_argument("--season", default="", help="Season code (e.g. 047). Empty = all.")
    parser.add_argument("--dry-run", action="store_true", help="Report without writing DB.")
    parser.add_argument("--limit", type=int, default=0, help="Limit number of games (0=all).")
    args = parser.parse_args()

    if not args.db_url:
        raise SystemExit("DATABASE_URL or SUPABASE_POSTGRES_URL is required")

    conn = get_connection(args.db_url)
    try:
        season = args.season.strip() or None
        games = list(iter_games_with_pbp(conn, season))

        if args.limit > 0:
            games = games[: args.limit]

        if not games:
            print("No games with PBP data found.")
            return 0

        total_updated = 0
        total_already = 0
        status = "dry-run" if args.dry_run else "updated"

        for idx, game in enumerate(games, start=1):
            updated, already = backfill_player_ids_for_game(conn, game, dry_run=args.dry_run)
            total_updated += updated
            total_already += already
            print(f"[{idx}/{len(games)}] {game.game_id}: {status}={updated}, already_filled={already}")

        print(f"\nTotal: {status}={total_updated}, already_filled={total_already}")
        return 0
    finally:
        conn.close()


if __name__ == "__main__":
    raise SystemExit(main())
