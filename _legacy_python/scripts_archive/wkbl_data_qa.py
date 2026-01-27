#!/usr/bin/env python3
"""
Run data QA checks for WKBL boxscore-derived datasets.
"""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
from pathlib import Path

import pandas as pd


ROOT_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT_DIR / "data" / "wkbl"
DERIVED_DIR = DATA_DIR / "derived"
META_FILE = DATA_DIR / "meta" / "all_games_list.json"
OUTPUT_DIR = DATA_DIR / "qa"

ABS_PTS_HIGH = 100
ABS_PTS_LOW = 40
ABS_MARGIN_HIGH = 40


def normalize_key(season, game_type, ym, game_no) -> str | None:
    try:
        return f"{int(season)}-{int(game_type)}-{int(ym)}-{int(game_no)}"
    except Exception:
        return None


def load_csv(path: Path) -> pd.DataFrame:
    if not path.exists():
        raise SystemExit(f"missing required file: {path}")
    return pd.read_csv(path)


def build_expected_games(meta_file: Path | None) -> tuple[set[str], dict[str, int], bool]:
    if not meta_file or not meta_file.exists():
        return set(), {}, False
    data = json.loads(meta_file.read_text(encoding="utf-8"))
    expected = set()
    by_season: dict[str, int] = {}
    for game in data:
        key = normalize_key(game.get("season_code"), game.get("game_type"), game.get("ym"), game.get("game_no"))
        if not key:
            continue
        expected.add(key)
        season = str(int(game.get("season_code")))
        by_season[season] = by_season.get(season, 0) + 1
    return expected, by_season, True


def build_actual_games(players_games: pd.DataFrame) -> tuple[set[str], dict[str, int]]:
    actual = set()
    by_season: dict[str, set[str]] = {}
    for _, row in players_games[["season_gu", "game_type", "ym", "game_no"]].dropna().iterrows():
        key = normalize_key(row["season_gu"], row["game_type"], row["ym"], row["game_no"])
        if not key:
            continue
        actual.add(key)
        season = str(int(row["season_gu"]))
        by_season.setdefault(season, set()).add(key)
    counts = {season: len(keys) for season, keys in by_season.items()}
    return actual, counts


def find_team_count_anomalies(team_games: pd.DataFrame) -> list[dict]:
    counts = team_games.groupby("game_key")["team"].nunique().reset_index(name="team_count")
    anomalies = counts[counts["team_count"] != 2].sort_values("team_count", ascending=False)
    return anomalies.head(50).to_dict(orient="records")


def find_duplicate_players(players_games: pd.DataFrame) -> list[dict]:
    dupes = (
        players_games.groupby(["game_key", "team", "name"], as_index=False)
        .size()
        .rename(columns={"size": "row_count"})
    )
    dupes = dupes[dupes["row_count"] > 1].sort_values("row_count", ascending=False)
    return dupes.head(50).to_dict(orient="records")


def build_pts_outliers(team_games: pd.DataFrame) -> dict:
    for col in ("pts_for", "pts_against", "margin"):
        team_games[col] = pd.to_numeric(team_games[col], errors="coerce")

    iqr_outliers: list[dict] = []
    for season, group in team_games.groupby("season_gu"):
        pts = group["pts_for"].dropna()
        if pts.empty:
            continue
        q1 = pts.quantile(0.25)
        q3 = pts.quantile(0.75)
        iqr = q3 - q1
        if iqr == 0:
            continue
        high = q3 + 1.5 * iqr
        low = max(0, q1 - 1.5 * iqr)
        flagged = group[(group["pts_for"] > high) | (group["pts_for"] < low)].copy()
        for _, row in flagged.iterrows():
            iqr_outliers.append(
                {
                    "season": str(int(season)),
                    "game_key": row["game_key"],
                    "team": row["team"],
                    "pts_for": float(row["pts_for"]),
                    "pts_against": float(row["pts_against"]),
                    "margin": float(row["margin"]),
                    "low_threshold": round(float(low), 2),
                    "high_threshold": round(float(high), 2),
                }
            )

    abs_outliers = team_games[
        (team_games["pts_for"] >= ABS_PTS_HIGH) | (team_games["pts_for"] <= ABS_PTS_LOW)
    ].copy()
    abs_outliers = abs_outliers.sort_values("pts_for", ascending=False)
    abs_outliers = abs_outliers.head(50).to_dict(orient="records")

    margin_outliers = team_games[team_games["margin"].abs() >= ABS_MARGIN_HIGH].copy()
    margin_outliers = margin_outliers.sort_values("margin", ascending=False)
    margin_outliers = margin_outliers.head(50).to_dict(orient="records")

    return {
        "iqr": iqr_outliers[:50],
        "absolute_pts": abs_outliers,
        "absolute_margin": margin_outliers,
    }


def write_markdown(report: dict, output_path: Path) -> None:
    coverage = report["coverage"]
    lines = [
        "# WKBL Data QA Report",
        "",
        f"Generated: {report['generated_at']}",
        "",
        "## Summary",
        f"- Meta file available: {coverage['meta_available']}",
        f"- Expected games: {coverage['expected_games']}",
        f"- Actual games: {coverage['actual_games']}",
        f"- Missing games: {coverage['missing_games']}",
        f"- Coverage: {coverage['coverage_pct']}%",
        "",
        "## Coverage by Season",
        "",
        "| Season | Expected | Actual | Missing | Coverage |",
        "| --- | ---: | ---: | ---: | ---: |",
    ]
    for row in coverage["by_season"]:
        lines.append(
            f"| {row['season']} | {row['expected']} | {row['actual']} | "
            f"{row['missing']} | {row['coverage_pct']}% |"
        )

    lines += [
        "",
        "## Missing Games (sample)",
        "",
        "```\n" + "\n".join(coverage["missing_sample"]) + "\n```",
        "",
        "## Team Count Anomalies (game_key where team count != 2)",
        "",
        "```\n" + "\n".join(
            f"{row['game_key']} teams={row['team_count']}" for row in report["duplicates"]["team_count_anomalies"]
        ) + "\n```",
        "",
        "## Duplicate Player Rows (sample)",
        "",
        "```\n" + "\n".join(
            f"{row['game_key']} {row['team']} {row['name']} x{row['row_count']}" for row in report["duplicates"]["player_duplicates"]
        ) + "\n```",
        "",
        "## Point Outliers (IQR)",
        "",
        "```\n" + "\n".join(
            f"{row['season']} {row['game_key']} {row['team']} pts={row['pts_for']}" for row in report["outliers"]["iqr"]
        ) + "\n```",
        "",
        "## Point Outliers (Absolute Thresholds)",
        "",
        f"Thresholds: pts_for <= {ABS_PTS_LOW} or >= {ABS_PTS_HIGH}",
        "",
        "```\n" + "\n".join(
            f"{row['game_key']} {row['team']} pts={row['pts_for']}" for row in report["outliers"]["absolute_pts"]
        ) + "\n```",
        "",
        "## Margin Outliers (Absolute Thresholds)",
        "",
        f"Threshold: |margin| >= {ABS_MARGIN_HIGH}",
        "",
        "```\n" + "\n".join(
            f"{row['game_key']} {row['team']} margin={row['margin']}" for row in report["outliers"]["absolute_margin"]
        ) + "\n```",
    ]
    output_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", type=Path, default=OUTPUT_DIR)
    parser.add_argument("--meta-file", type=Path, default=META_FILE)
    args = parser.parse_args()

    players_games = load_csv(DERIVED_DIR / "players_games.csv")
    team_games = load_csv(DERIVED_DIR / "team_games.csv")

    expected_games, expected_by_season, meta_available = build_expected_games(args.meta_file)
    actual_games, actual_by_season = build_actual_games(players_games)

    if meta_available:
        missing = sorted(expected_games - actual_games)
        extra = sorted(actual_games - expected_games)
    else:
        missing = []
        extra = []

    seasons = sorted({*expected_by_season.keys(), *actual_by_season.keys()}, key=lambda s: int(s))
    by_season = []
    for season in seasons:
        expected = expected_by_season.get(season, 0)
        actual = actual_by_season.get(season, 0)
        missing_count = max(expected - actual, 0) if expected else 0
        coverage = round((actual / expected * 100), 1) if expected else 0.0
        by_season.append(
            {
                "season": season,
                "expected": expected,
                "actual": actual,
                "missing": missing_count,
                "coverage_pct": coverage,
            }
        )

    report = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "sources": {
            "meta_file": str(META_FILE),
            "players_games": str(DERIVED_DIR / "players_games.csv"),
            "team_games": str(DERIVED_DIR / "team_games.csv"),
        },
        "coverage": {
            "meta_available": meta_available,
            "expected_games": len(expected_games),
            "actual_games": len(actual_games),
            "missing_games": len(missing),
            "extra_games": len(extra),
            "coverage_pct": round((len(actual_games) / len(expected_games) * 100), 1) if expected_games else 0.0,
            "by_season": by_season,
            "missing_sample": missing[:50],
            "extra_sample": extra[:50],
        },
        "duplicates": {
            "team_count_anomalies": find_team_count_anomalies(team_games),
            "player_duplicates": find_duplicate_players(players_games),
        },
        "outliers": build_pts_outliers(team_games),
    }

    args.output_dir.mkdir(parents=True, exist_ok=True)
    json_path = args.output_dir / "qa_report.json"
    md_path = args.output_dir / "qa_report.md"
    json_path.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    write_markdown(report, md_path)
    print(f"saved {json_path} and {md_path}")


if __name__ == "__main__":
    main()
