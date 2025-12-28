#!/usr/bin/env python3
"""
Collect WKBL game box scores via AJAX endpoints.
"""

import argparse
import re
import time
from io import StringIO
from pathlib import Path
from urllib.parse import urljoin, urlparse, parse_qs

import pandas as pd
import requests
from bs4 import BeautifulSoup

BASE_URL = "https://www.wkbl.or.kr"
SCHEDULE_URL = f"{BASE_URL}/game/sch/schedule1.asp"
AJAX_RESULT_URL = f"{BASE_URL}/game/ajax/ajax_game_result_2.asp"

ROOT_DIR = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT_DIR / "data" / "wkbl" / "box"

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    )
}


def fetch(url: str, session: requests.Session) -> str:
    res = session.get(url, headers=HEADERS)
    res.raise_for_status()
    res.encoding = res.apparent_encoding
    return res.text


def post(url: str, data: dict, session: requests.Session) -> str:
    res = session.post(url, data=data, headers=HEADERS)
    res.raise_for_status()
    res.encoding = res.apparent_encoding
    return res.text


def get_season_and_months(session: requests.Session) -> tuple[str, list[str]]:
    html = fetch(SCHEDULE_URL, session)
    soup = BeautifulSoup(html, "html.parser")
    season_opt = soup.select_one("#season_gu option[selected]")
    season_gu = season_opt.get("value") if season_opt else ""
    months = [opt.get("value") for opt in soup.select("#ym option")]
    months = [m for m in months if m and m.isdigit()]
    return season_gu, sorted(set(months))


def get_schedule_links(season_gu: str, ym: str, session: requests.Session) -> list[str]:
    url = (
        f"{BASE_URL}/game/sch/schedule1.asp"
        f"?gun=1&season_gu={season_gu}&viewType=2&ym={ym}"
    )
    html = fetch(url, session)
    links = re.findall(r"/game/result\.asp\?[^\"']+", html)
    return [urljoin(BASE_URL, link) for link in links]


def parse_game_params(url: str) -> dict:
    qs = parse_qs(urlparse(url).query)
    def pick(key: str) -> str:
        return qs.get(key, [""])[0]
    return {
        "season_gu": pick("season_gu"),
        "gun": pick("gun"),
        "game_type": pick("game_type"),
        "game_no": pick("game_no"),
        "ym": pick("ym"),
        "viewType": pick("viewType"),
    }


def normalize_player_table(df: pd.DataFrame) -> pd.DataFrame:
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = [col[1] or col[0] for col in df.columns]

    df = df.rename(columns={
        "선수": "name",
        "POS": "pos",
        "MIN": "min",
        "2PM-A": "fg2",
        "3PM-A": "fg3",
        "FTM-A": "ft",
        "OFF": "reb_off",
        "DEF": "reb_def",
        "TOT": "reb",
        "AST": "ast",
        "PF": "pf",
        "ST": "stl",
        "TO": "to",
        "BS": "blk",
        "PTS": "pts",
    })

    df = df[df["name"].astype(str).str.strip().ne("선수")]
    return df


def fetch_game_players(params: dict, session: requests.Session) -> list[pd.DataFrame]:
    payload = {
        "season_gu": params["season_gu"],
        "game_type": params["game_type"],
        "game_no": params["game_no"],
        "ym": params["ym"],
        "h_player": "",
        "a_player": "",
    }
    html = post(AJAX_RESULT_URL, payload, session)
    dfs = pd.read_html(StringIO(html))
    if len(dfs) < 3:
        return []
    team_left = str(dfs[0].columns[0]).strip()
    team_right = str(dfs[0].columns[-1]).strip()

    left_df = normalize_player_table(dfs[1])
    right_df = normalize_player_table(dfs[2])

    for df, team in [(left_df, team_left), (right_df, team_right)]:
        df.insert(0, "team", team)
        df.insert(1, "game_no", params["game_no"])
        df.insert(2, "season_gu", params["season_gu"])
        df.insert(3, "ym", params["ym"])
        df.insert(4, "game_type", params["game_type"])

    return [left_df, right_df]


def collect(season_gu: str, months: list[str], sleep: float) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    session = requests.Session()
    seen = set()
    saved = 0

    for ym in months:
        links = get_schedule_links(season_gu, ym, session)
        for link in links:
            params = parse_game_params(link)
            key = (params["season_gu"], params["game_type"], params["game_no"], params["ym"])
            if key in seen:
                continue
            seen.add(key)

            try:
                tables = fetch_game_players(params, session)
            except Exception as exc:
                print(f"skip game_no={params['game_no']} ym={params['ym']} error={exc}")
                continue

            if not tables:
                print(f"no data game_no={params['game_no']} ym={params['ym']}")
                continue

            for df in tables:
                team_slug = re.sub(r"\s+", "_", df["team"].iloc[0])
                out = OUT_DIR / f"game_{params['game_no']}_{team_slug}.csv"
                df.to_csv(out, index=False)
                saved += 1

            time.sleep(sleep)

    print(f"saved {saved} files to {OUT_DIR}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--season", default="", help="season_gu override")
    parser.add_argument("--months", default="", help="comma-separated ym list")
    parser.add_argument("--sleep", type=float, default=0.2, help="delay between requests")
    args = parser.parse_args()

    session = requests.Session()
    season_gu, months = get_season_and_months(session)

    if args.season:
        season_gu = args.season
    if args.months:
        months = [m.strip() for m in args.months.split(",") if m.strip()]

    if not season_gu or not months:
        raise SystemExit("season_gu/months not found")

    collect(season_gu, months, args.sleep)


if __name__ == "__main__":
    main()
