#!/usr/bin/env python3
"""
Aggregate WKBL box score CSVs into season-level datasets.
"""

from __future__ import annotations

import argparse
from pathlib import Path
import re

import pandas as pd


ROOT_DIR = Path(__file__).resolve().parents[1]
DEFAULT_INPUT = ROOT_DIR / "data" / "wkbl" / "box"
DEFAULT_OUTPUT = ROOT_DIR / "data" / "wkbl" / "derived"

TEAM_MAP = {
    "BNK 썸": "BNK썸",
    "BNK  썸": "BNK썸",
    "KB 스타즈": "KB스타즈",
    "우리 은행": "우리은행",
    "하나 은행": "하나은행",
    "신한 은행": "신한은행",
    "삼성 생명": "삼성생명",
}

NUMERIC_COLS = [
    "reb_off",
    "reb_def",
    "reb",
    "ast",
    "pf",
    "stl",
    "to",
    "blk",
    "pts",
]


def parse_minutes(value: str) -> float:
    text = str(value).strip()
    if not text or text == "nan":
        return 0.0
    if ":" in text:
        minutes, seconds = text.split(":", 1)
        return to_int(minutes) + (to_int(seconds) / 60)
    try:
        return float(text)
    except Exception:
        return 0.0


def to_int(value: str) -> int:
    try:
        return int(str(value).strip())
    except Exception:
        return 0


def parse_made_att(value: str) -> tuple[int, int]:
    text = str(value).strip()
    if "-" not in text:
        return 0, 0
    made, att = text.split("-", 1)
    return to_int(made), to_int(att)


def normalize_team(name: str) -> str:
    text = str(name).strip()
    return TEAM_MAP.get(text, text)


def add_shooting(df: pd.DataFrame) -> pd.DataFrame:
    df["fg_m"] = df["fg2_m"] + df["fg3_m"]
    df["fg_a"] = df["fg2_a"] + df["fg3_a"]

    df["fg_pct"] = df.apply(lambda r: pct(r["fg_m"], r["fg_a"]), axis=1)
    df["fg2_pct"] = df.apply(lambda r: pct(r["fg2_m"], r["fg2_a"]), axis=1)
    df["fg3_pct"] = df.apply(lambda r: pct(r["fg3_m"], r["fg3_a"]), axis=1)
    df["ft_pct"] = df.apply(lambda r: pct(r["ft_m"], r["ft_a"]), axis=1)
    df["efg_pct"] = df.apply(lambda r: pct(r["fg_m"] + 0.5 * r["fg3_m"], r["fg_a"]), axis=1)

    def ts(row: pd.Series) -> float:
        denom = 2 * (row["fg_a"] + 0.44 * row["ft_a"])
        if denom <= 0:
            return 0.0
        return round((row["pts"] / denom) * 100, 1)

    df["ts_pct"] = df.apply(ts, axis=1)
    return df


def add_eff(df: pd.DataFrame) -> pd.DataFrame:
    df["eff"] = (
        df["pts"]
        + df["reb"]
        + df["ast"]
        + df["stl"]
        + df["blk"]
        - df["to"]
        - ((df["fg_a"] - df["fg_m"]) + (df["ft_a"] - df["ft_m"]))
    )
    return df


def pct(made: float, att: float) -> float:
    if att <= 0:
        return 0.0
    return round((made / att) * 100, 1)


def mode_or_empty(series: pd.Series) -> str:
    if series.empty:
        return ""
    mode = series.mode()
    return str(mode.iloc[0]) if not mode.empty else str(series.iloc[0])


def load_games(input_dir: Path) -> pd.DataFrame:
    rows = []
    for file in sorted(input_dir.rglob("*.csv")):
        df = pd.read_csv(file)
        df["team"] = df["team"].apply(normalize_team)
        df["name"] = df["name"].astype(str).str.strip()
        df["pos"] = df["pos"].astype(str).str.strip()
        df["min_dec"] = df["min"].apply(parse_minutes)

        fg2 = df["fg2"].apply(parse_made_att)
        fg3 = df["fg3"].apply(parse_made_att)
        ft = df["ft"].apply(parse_made_att)
        df["fg2_m"] = fg2.apply(lambda x: x[0])
        df["fg2_a"] = fg2.apply(lambda x: x[1])
        df["fg3_m"] = fg3.apply(lambda x: x[0])
        df["fg3_a"] = fg3.apply(lambda x: x[1])
        df["ft_m"] = ft.apply(lambda x: x[0])
        df["ft_a"] = ft.apply(lambda x: x[1])

        df["game_key"] = (
            df["season_gu"].astype(str)
            + "-"
            + df["game_type"].astype(str)
            + "-"
            + df["ym"].astype(str)
            + "-"
            + df["game_no"].astype(str)
        )

        rows.append(df)

    if not rows:
        return pd.DataFrame()

    games = pd.concat(rows, ignore_index=True)
    for col in NUMERIC_COLS:
        games[col] = pd.to_numeric(games[col], errors="coerce").fillna(0).astype(int)

    games = add_shooting(games)
    games = add_eff(games)
    games["game_no"] = games["game_no"].astype(int)
    games["season_gu"] = games["season_gu"].astype(str).str.zfill(3)
    games["game_type"] = games["game_type"].astype(str)
    games["ym"] = games["ym"].astype(str)

    teams_by_game = games.groupby("game_key")["team"].unique().to_dict()
    opponent_map = {}
    for game_key, teams in teams_by_game.items():
        if len(teams) == 2:
            opponent_map[(game_key, teams[0])] = teams[1]
            opponent_map[(game_key, teams[1])] = teams[0]

    games["opponent"] = games.apply(
        lambda r: opponent_map.get((r["game_key"], r["team"]), ""), axis=1
    )

    return games


def aggregate_players(games: pd.DataFrame) -> pd.DataFrame:
    group = games.groupby(["season_gu", "team", "name"], as_index=False)
    totals = group.agg(
        gp=("game_key", "nunique"),
        pos=("pos", mode_or_empty),
        min_total=("min_dec", "sum"),
        fg2_m=("fg2_m", "sum"),
        fg2_a=("fg2_a", "sum"),
        fg3_m=("fg3_m", "sum"),
        fg3_a=("fg3_a", "sum"),
        ft_m=("ft_m", "sum"),
        ft_a=("ft_a", "sum"),
        reb_off=("reb_off", "sum"),
        reb_def=("reb_def", "sum"),
        reb=("reb", "sum"),
        ast=("ast", "sum"),
        pf=("pf", "sum"),
        stl=("stl", "sum"),
        to=("to", "sum"),
        blk=("blk", "sum"),
        pts=("pts", "sum"),
    )

    totals = add_shooting(totals)
    totals = add_eff(totals)
    totals["min_per_game"] = totals.apply(
        lambda r: round(r["min_total"] / r["gp"], 1) if r["gp"] else 0.0, axis=1
    )

    per_game = totals.copy()
    rate_cols = [
        "min_total",
        "fg2_m",
        "fg2_a",
        "fg3_m",
        "fg3_a",
        "ft_m",
        "ft_a",
        "reb_off",
        "reb_def",
        "reb",
        "ast",
        "pf",
        "stl",
        "to",
        "blk",
        "pts",
        "fg_m",
        "fg_a",
    ]
    for col in rate_cols:
        per_game[col] = per_game.apply(
            lambda r: round(r[col] / r["gp"], 2) if r["gp"] else 0.0, axis=1
        )
    per_game = add_shooting(per_game)
    per_game = add_eff(per_game)

    per_36 = totals.copy()
    for col in rate_cols:
        per_36[col] = per_36.apply(
            lambda r: round((r[col] / r["min_total"]) * 36, 2) if r["min_total"] else 0.0,
            axis=1,
        )
    per_36 = add_shooting(per_36)
    per_36 = add_eff(per_36)

    totals["scope"] = "totals"
    per_game["scope"] = "per_game"
    per_36["scope"] = "per_36"
    return pd.concat([totals, per_game, per_36], ignore_index=True)


def build_team_games(games: pd.DataFrame) -> pd.DataFrame:
    group = games.groupby(
        ["season_gu", "game_type", "ym", "game_no", "game_key", "team"], as_index=False
    )
    team_games = group.agg(
        min_total=("min_dec", "sum"),
        fg2_m=("fg2_m", "sum"),
        fg2_a=("fg2_a", "sum"),
        fg3_m=("fg3_m", "sum"),
        fg3_a=("fg3_a", "sum"),
        ft_m=("ft_m", "sum"),
        ft_a=("ft_a", "sum"),
        reb_off=("reb_off", "sum"),
        reb_def=("reb_def", "sum"),
        reb=("reb", "sum"),
        ast=("ast", "sum"),
        pf=("pf", "sum"),
        stl=("stl", "sum"),
        to=("to", "sum"),
        blk=("blk", "sum"),
        pts=("pts", "sum"),
    )
    team_games = add_shooting(team_games)
    team_games = add_eff(team_games)

    team_games = team_games.rename(columns={"pts": "pts_for"})
    scores = team_games[["game_key", "team", "pts_for"]]
    opponents = scores.rename(columns={"team": "opponent", "pts_for": "pts_against"})
    team_games = team_games.merge(opponents, on="game_key", how="left")
    team_games = team_games[team_games["team"] != team_games["opponent"]]
    team_games["margin"] = team_games["pts_for"] - team_games["pts_against"]
    team_games["win"] = team_games["margin"] > 0
    team_games["pts"] = team_games["pts_for"]
    return team_games


def build_games_summary(team_games: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for _, group in team_games.groupby("game_key"):
        if len(group) != 2:
            continue
        left, right = group.iloc[0], group.iloc[1]
        team_rows = sorted(
            [left, right],
            key=lambda r: str(r["team"]),
        )
        team_a, team_b = team_rows
        pts_a = int(team_a["pts_for"])
        pts_b = int(team_b["pts_for"])
        rows.append(
            {
                "season_gu": team_a["season_gu"],
                "game_type": team_a["game_type"],
                "ym": team_a["ym"],
                "game_no": int(team_a["game_no"]),
                "game_key": team_a["game_key"],
                "team_a": team_a["team"],
                "team_b": team_b["team"],
                "pts_a": pts_a,
                "pts_b": pts_b,
                "margin": pts_a - pts_b,
                "winner": team_a["team"] if pts_a >= pts_b else team_b["team"],
            }
        )
    if not rows:
        return pd.DataFrame()
    summary = pd.DataFrame(rows)
    summary["game_no"] = summary["game_no"].astype(int)
    summary["season_gu"] = summary["season_gu"].astype(str).str.zfill(3)
    summary["game_type"] = summary["game_type"].astype(str)
    summary["ym"] = summary["ym"].astype(str)
    return summary


def aggregate_teams(games: pd.DataFrame, team_games: pd.DataFrame) -> pd.DataFrame:
    group = games.groupby(["season_gu", "team"], as_index=False)
    totals = group.agg(
        gp=("game_key", "nunique"),
        min_total=("min_dec", "sum"),
        fg2_m=("fg2_m", "sum"),
        fg2_a=("fg2_a", "sum"),
        fg3_m=("fg3_m", "sum"),
        fg3_a=("fg3_a", "sum"),
        ft_m=("ft_m", "sum"),
        ft_a=("ft_a", "sum"),
        reb_off=("reb_off", "sum"),
        reb_def=("reb_def", "sum"),
        reb=("reb", "sum"),
        ast=("ast", "sum"),
        pf=("pf", "sum"),
        stl=("stl", "sum"),
        to=("to", "sum"),
        blk=("blk", "sum"),
        pts=("pts", "sum"),
    )
    totals = add_shooting(totals)
    totals = add_eff(totals)
    totals["min_per_game"] = totals.apply(
        lambda r: round(r["min_total"] / r["gp"], 1) if r["gp"] else 0.0, axis=1
    )

    per_game = totals.copy()
    rate_cols = [
        "min_total",
        "fg2_m",
        "fg2_a",
        "fg3_m",
        "fg3_a",
        "ft_m",
        "ft_a",
        "reb_off",
        "reb_def",
        "reb",
        "ast",
        "pf",
        "stl",
        "to",
        "blk",
        "pts",
        "fg_m",
        "fg_a",
    ]
    for col in rate_cols:
        per_game[col] = per_game.apply(
            lambda r: round(r[col] / r["gp"], 2) if r["gp"] else 0.0, axis=1
        )
    per_game = add_shooting(per_game)
    per_game = add_eff(per_game)

    margin_df = (
        team_games.groupby(["season_gu", "team"], as_index=False)
        .agg(gp=("game_key", "nunique"), pts_against=("pts_against", "sum"), margin_total=("margin", "sum"))
    )
    margin_df["margin_per_game"] = margin_df.apply(
        lambda r: round(r["margin_total"] / r["gp"], 2) if r["gp"] else 0.0, axis=1
    )
    margin_df["pts_against_per_game"] = margin_df.apply(
        lambda r: round(r["pts_against"] / r["gp"], 2) if r["gp"] else 0.0, axis=1
    )
    totals = totals.merge(margin_df, on=["season_gu", "team"], how="left")
    per_game = per_game.merge(margin_df, on=["season_gu", "team"], how="left")
    totals = totals.rename(columns={"gp_x": "gp"}).drop(columns=["gp_y"], errors="ignore")
    per_game = per_game.rename(columns={"gp_x": "gp"}).drop(columns=["gp_y"], errors="ignore")
    totals["margin"] = totals["margin_total"].fillna(0)
    per_game["margin"] = per_game["margin_per_game"].fillna(0)
    totals["pts_against"] = totals["pts_against"].fillna(0)
    per_game["pts_against"] = per_game["pts_against_per_game"].fillna(0)

    totals["scope"] = "totals"
    per_game["scope"] = "per_game"
    return pd.concat([totals, per_game], ignore_index=True)


def save(df: pd.DataFrame, output_dir: Path, name: str) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    path = output_dir / name
    df.to_csv(path, index=False)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-dir", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()

    games = load_games(args.input_dir)
    if games.empty:
        raise SystemExit("no game data found")

    team_games = build_team_games(games)
    games_summary = build_games_summary(team_games)
    players = aggregate_players(games)
    teams = aggregate_teams(games, team_games)

    save(games, args.output_dir, "players_games.csv")
    if not team_games.empty:
        save(team_games, args.output_dir, "team_games.csv")
    if not games_summary.empty:
        save(games_summary, args.output_dir, "games_summary.csv")
    save(players, args.output_dir, "players_aggregate.csv")
    save(teams, args.output_dir, "teams_aggregate.csv")

    print(
        f"saved {len(games)} game rows, {len(players)} player rows, "
        f"{len(teams)} team rows, {len(team_games)} team-game rows"
    )


if __name__ == "__main__":
    main()
