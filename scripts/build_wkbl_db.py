#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sqlite3
import sys
from pathlib import Path

import pandas as pd

ROOT_DIR = Path(__file__).resolve().parent.parent
DEFAULT_DATA_DIR = ROOT_DIR / "data" / "wkbl"
DEFAULT_OUTPUT = ROOT_DIR / "data" / "wkbl.db"


def is_missing(value: object) -> bool:
    if value is None:
        return True
    if isinstance(value, float) and pd.isna(value):
        return True
    if isinstance(value, str) and value.strip().lower() in {"", "nan"}:
        return True
    return False


def to_str(value: object) -> str:
    if is_missing(value):
        return ""
    return str(value).strip()


def to_str_int(value: object) -> str:
    if is_missing(value):
        return ""
    try:
        return str(int(float(str(value))))
    except Exception:
        return to_str(value)


def to_int(value: object, default: int = 0) -> int:
    if is_missing(value):
        return default
    try:
        return int(float(str(value)))
    except Exception:
        return default


def to_float(value: object, default: float = 0.0) -> float:
    if is_missing(value):
        return default
    try:
        return float(str(value))
    except Exception:
        return default


def minutes_to_seconds(min_dec: object, min_text: object) -> float:
    if not is_missing(min_dec):
        return to_float(min_dec) * 60.0
    text = to_str(min_text)
    if not text:
        return 0.0
    if ":" in text:
        minutes, seconds = text.split(":", 1)
        return to_int(minutes) * 60 + to_int(seconds)
    return to_float(text) * 60.0


def create_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE seasons (
          season_code TEXT PRIMARY KEY,
          season_name TEXT NOT NULL
        );

        CREATE TABLE teams (
          team_code TEXT PRIMARY KEY,
          team_name_kr TEXT NOT NULL
        );

        CREATE TABLE players (
          player_id INTEGER PRIMARY KEY,
          player_name TEXT NOT NULL
        );

        CREATE TABLE games (
          game_id INTEGER PRIMARY KEY,
          game_key TEXT UNIQUE NOT NULL,
          season_code TEXT NOT NULL,
          game_type TEXT,
          ym TEXT,
          game_no TEXT,
          home_team_code TEXT NOT NULL,
          away_team_code TEXT NOT NULL,
          home_score INTEGER,
          away_score INTEGER
        );

        CREATE TABLE game_stats (
          game_id INTEGER NOT NULL,
          player_id INTEGER NOT NULL,
          team_code TEXT NOT NULL,
          min_seconds REAL,
          pts INTEGER,
          reb_off INTEGER,
          reb_def INTEGER,
          reb_tot INTEGER,
          ast INTEGER,
          stl INTEGER,
          blk INTEGER,
          tov INTEGER,
          fg_2p_m INTEGER,
          fg_2p_a INTEGER,
          fg_3p_m INTEGER,
          fg_3p_a INTEGER,
          ft_m INTEGER,
          ft_a INTEGER,
          game_score REAL
        );

        CREATE INDEX idx_game_stats_game_id ON game_stats(game_id);
        CREATE INDEX idx_game_stats_player_id ON game_stats(player_id);
        CREATE INDEX idx_game_stats_team_code ON game_stats(team_code);
        CREATE INDEX idx_games_season_code ON games(season_code);
        """
    )


def chunked(items: list[tuple], size: int):
    for start in range(0, len(items), size):
        yield items[start : start + size]


def build_db(data_dir: Path, output: Path, force: bool, chunk_size: int) -> int:
    derived_dir = data_dir / "derived"
    players_path = derived_dir / "players_games.csv"
    teams_path = derived_dir / "team_games.csv"

    if not players_path.exists():
        print(f"players_games.csv not found: {players_path}", file=sys.stderr)
        return 1
    if not teams_path.exists():
        print(f"team_games.csv not found: {teams_path}", file=sys.stderr)
        return 1

    if output.exists():
        if not force:
            print(f"output already exists: {output} (use --force to overwrite)", file=sys.stderr)
            return 1
        output.unlink()

    output.parent.mkdir(parents=True, exist_ok=True)

    teams_df = pd.read_csv(teams_path, dtype=str)
    players_df = pd.read_csv(players_path, dtype=str)
    players_df = players_df.rename(columns={"to": "tov", "min": "min_text"})

    team_names = set()
    for col in ("team", "opponent"):
        if col not in teams_df.columns:
            continue
        for value in teams_df[col].tolist():
            name = to_str(value)
            if name:
                team_names.add(name)
    if "team" in players_df.columns:
        for value in players_df["team"].tolist():
            name = to_str(value)
            if name:
                team_names.add(name)
    team_rows = sorted(team_names)

    season_codes = set()
    for df in (teams_df, players_df):
        if "season_gu" not in df.columns:
            continue
        for value in df["season_gu"].tolist():
            code = to_str_int(value)
            if code:
                season_codes.add(code)
    season_rows = sorted(season_codes, key=lambda x: int(x) if x.isdigit() else x)

    player_names = []
    if "name" in players_df.columns:
        for value in players_df["name"].tolist():
            name = to_str(value)
            if name:
                player_names.append(name)
    player_names = sorted(set(player_names))
    player_id_map = {name: idx + 1 for idx, name in enumerate(player_names)}

    game_rows: list[tuple] = []
    game_id_map: dict[str, int] = {}
    if "game_key" in teams_df.columns:
        for row in teams_df.itertuples(index=False):
            game_key = to_str(getattr(row, "game_key", ""))
            if not game_key or game_key in game_id_map:
                continue
            team = to_str(getattr(row, "team", ""))
            opponent = to_str(getattr(row, "opponent", ""))
            if not team or not opponent:
                continue
            season_code = to_str_int(getattr(row, "season_gu", "")) or "UNKNOWN"
            game_type = to_str_int(getattr(row, "game_type", ""))
            ym = to_str_int(getattr(row, "ym", ""))
            game_no = to_str_int(getattr(row, "game_no", ""))
            home_score = to_int(getattr(row, "pts_for", 0))
            away_score = to_int(getattr(row, "pts_against", 0))
            game_id = len(game_rows) + 1
            game_id_map[game_key] = game_id
            game_rows.append(
                (
                    game_id,
                    game_key,
                    season_code,
                    game_type,
                    ym,
                    game_no,
                    team,
                    opponent,
                    home_score,
                    away_score,
                )
            )

    conn = sqlite3.connect(output)
    try:
        create_schema(conn)
        conn.executemany(
            "INSERT INTO seasons (season_code, season_name) VALUES (?, ?)",
            [(code, f"Season {code}") for code in season_rows],
        )
        conn.executemany(
            "INSERT INTO teams (team_code, team_name_kr) VALUES (?, ?)",
            [(name, name) for name in team_rows],
        )
        conn.executemany(
            "INSERT INTO players (player_id, player_name) VALUES (?, ?)",
            [(player_id_map[name], name) for name in player_names],
        )
        conn.executemany(
            """
            INSERT INTO games (
              game_id, game_key, season_code, game_type, ym, game_no,
              home_team_code, away_team_code, home_score, away_score
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            game_rows,
        )

        stats_rows: list[tuple] = []
        missing_games = 0
        for row in players_df.itertuples(index=False):
            game_key = to_str(getattr(row, "game_key", ""))
            game_id = game_id_map.get(game_key)
            if not game_id:
                missing_games += 1
                continue
            player_name = to_str(getattr(row, "name", ""))
            player_id = player_id_map.get(player_name)
            if not player_id:
                continue
            team_code = to_str(getattr(row, "team", ""))
            min_seconds = minutes_to_seconds(
                getattr(row, "min_dec", None),
                getattr(row, "min_text", None),
            )
            stats_rows.append(
                (
                    game_id,
                    player_id,
                    team_code,
                    min_seconds,
                    to_int(getattr(row, "pts", 0)),
                    to_int(getattr(row, "reb_off", 0)),
                    to_int(getattr(row, "reb_def", 0)),
                    to_int(getattr(row, "reb", 0)),
                    to_int(getattr(row, "ast", 0)),
                    to_int(getattr(row, "stl", 0)),
                    to_int(getattr(row, "blk", 0)),
                    to_int(getattr(row, "tov", 0)),
                    to_int(getattr(row, "fg2_m", 0)),
                    to_int(getattr(row, "fg2_a", 0)),
                    to_int(getattr(row, "fg3_m", 0)),
                    to_int(getattr(row, "fg3_a", 0)),
                    to_int(getattr(row, "ft_m", 0)),
                    to_int(getattr(row, "ft_a", 0)),
                    to_float(getattr(row, "eff", 0.0)),
                )
            )
            if len(stats_rows) >= chunk_size:
                conn.executemany(
                    """
                    INSERT INTO game_stats (
                      game_id, player_id, team_code, min_seconds, pts,
                      reb_off, reb_def, reb_tot, ast, stl, blk, tov,
                      fg_2p_m, fg_2p_a, fg_3p_m, fg_3p_a, ft_m, ft_a, game_score
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    stats_rows,
                )
                stats_rows = []

        if stats_rows:
            conn.executemany(
                """
                INSERT INTO game_stats (
                  game_id, player_id, team_code, min_seconds, pts,
                  reb_off, reb_def, reb_tot, ast, stl, blk, tov,
                  fg_2p_m, fg_2p_a, fg_3p_m, fg_3p_a, ft_m, ft_a, game_score
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                stats_rows,
            )
        conn.commit()
    finally:
        conn.close()

    print(f"built database: {output}")
    print(f"seasons: {len(season_rows)}, teams: {len(team_rows)}, players: {len(player_names)}")
    print(f"games: {len(game_rows)}")
    if missing_games:
        print(f"skipped players rows without game: {missing_games}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Build wkbl.db from derived CSV data.")
    parser.add_argument("--data-dir", type=Path, default=DEFAULT_DATA_DIR)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--force", action="store_true", help="overwrite output if exists")
    parser.add_argument("--chunk-size", type=int, default=10000)
    args = parser.parse_args()
    return build_db(args.data_dir, args.output, args.force, args.chunk_size)


if __name__ == "__main__":
    raise SystemExit(main())
