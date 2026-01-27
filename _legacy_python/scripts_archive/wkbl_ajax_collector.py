#!/usr/bin/env python3
"""
Collect WKBL game box scores via AJAX endpoints.
"""

import argparse
import json
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
REQUEST_TIMEOUT = (5, 20)
REQUEST_RETRIES = 3
REQUEST_BACKOFF = 0.6

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    )
}


def request_with_retry(method: str, url: str, session: requests.Session, **kwargs) -> str:
    last_exc = None
    for attempt in range(1, REQUEST_RETRIES + 1):
        try:
            res = session.request(method, url, timeout=REQUEST_TIMEOUT, **kwargs)
            res.raise_for_status()
            res.encoding = res.apparent_encoding
            return res.text
        except requests.RequestException as exc:
            last_exc = exc
            if attempt == REQUEST_RETRIES:
                raise
            time.sleep(REQUEST_BACKOFF * attempt)
    if last_exc:
        raise last_exc
    raise RuntimeError("request failed without exception")


def fetch(url: str, session: requests.Session) -> str:
    return request_with_retry("GET", url, session, headers=HEADERS)


def post(url: str, data: dict, session: requests.Session) -> str:
    return request_with_retry("POST", url, session, data=data, headers=HEADERS)


def get_available_seasons(session: requests.Session) -> list[str]:
    html = fetch(SCHEDULE_URL, session)
    soup = BeautifulSoup(html, "html.parser")
    seasons = [opt.get("value") for opt in soup.select("#season_gu option")]
    seasons = [s for s in seasons if s and s.isdigit()]
    return seasons


def get_current_season(session: requests.Session) -> str:
    html = fetch(SCHEDULE_URL, session)
    soup = BeautifulSoup(html, "html.parser")
    season_opt = soup.select_one("#season_gu option[selected]")
    return season_opt.get("value") if season_opt else ""


def get_months_for_season(season_gu: str, session: requests.Session) -> list[str]:
    url = f"{SCHEDULE_URL}?gun=1&season_gu={season_gu}&viewType=2"
    html = fetch(url, session)
    soup = BeautifulSoup(html, "html.parser")
    months = [opt.get("value") for opt in soup.select("#ym option")]
    months = [m for m in months if m and m.isdigit()]
    return sorted(set(months))


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


def normalize_season_code(value: str) -> str:
    text = str(value).strip()
    if not text:
        return ""
    try:
        return str(int(text)).zfill(3)
    except Exception:
        return text.zfill(3)


def normalize_game_type(value: str) -> str:
    text = str(value).strip()
    if not text:
        return ""
    try:
        return str(int(text)).zfill(2)
    except Exception:
        return text.zfill(2)


def normalize_game_no(value: str) -> str:
    text = str(value).strip()
    if not text:
        return ""
    try:
        return str(int(text))
    except Exception:
        return text


def normalize_params(params: dict) -> dict:
    season = normalize_season_code(params.get("season_gu", ""))
    game_type = normalize_game_type(params.get("game_type", ""))
    game_no = normalize_game_no(params.get("game_no", ""))
    ym = str(params.get("ym", "")).strip()
    return {
        **params,
        "season_gu": season,
        "game_type": game_type,
        "game_no": game_no,
        "ym": ym,
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
    if len(dfs) >= 3:
        team_left = str(dfs[0].columns[0]).strip()
        team_right = str(dfs[0].columns[-1]).strip()
        left_df = normalize_player_table(dfs[1])
        right_df = normalize_player_table(dfs[2])
    elif len(dfs) >= 2:
        soup = BeautifulSoup(html, "html.parser")
        titles = [t.get_text(strip=True) for t in soup.select("h4.tit_area")]
        if len(titles) < 2:
            return []
        team_left, team_right = titles[:2]
        left_df = normalize_player_table(dfs[0])
        right_df = normalize_player_table(dfs[1])
    else:
        return []

    for df, team in [(left_df, team_left), (right_df, team_right)]:
        df.insert(0, "team", team)
        df.insert(1, "game_no", params["game_no"])
        df.insert(2, "season_gu", params["season_gu"])
        df.insert(3, "ym", params["ym"])
        df.insert(4, "game_type", params["game_type"])

    return [left_df, right_df]


def collect(
    season_gu: str,
    months: list[str],
    sleep: float,
    overwrite: bool,
    output_root: Path,
) -> None:
    season_dir = output_root / normalize_season_code(season_gu)
    season_dir.mkdir(parents=True, exist_ok=True)

    session = requests.Session()
    seen = set()
    saved = 0
    skipped = 0
    processed = 0

    print(f"[{season_gu}] months={len(months)}", flush=True)

    for ym in months:
        links = get_schedule_links(season_gu, ym, session)
        for link in links:
            params = normalize_params(parse_game_params(link))
            key = (params["season_gu"], params["game_type"], params["game_no"], params["ym"])
            if key in seen:
                continue
            seen.add(key)

            existing = list(
                season_dir.glob(f"game_{params['game_no']}_type{params['game_type']}_*.csv")
            )
            if existing and len(existing) >= 2 and not overwrite:
                skipped += len(existing)
                processed += 1
                continue

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
                out = season_dir / f"game_{params['game_no']}_type{params['game_type']}_{team_slug}.csv"
                if out.exists() and not overwrite:
                    skipped += 1
                    continue
                df.to_csv(out, index=False)
                saved += 1

            time.sleep(sleep)
            processed += 1
            if processed % 25 == 0:
                print(
                    f"[{season_gu}] processed {processed} games (saved {saved}, skipped {skipped})",
                    flush=True,
                )

    print(f"saved {saved} files to {season_dir} (skipped {skipped})")


def collect_from_meta(
    meta_file: Path,
    sleep: float,
    overwrite: bool,
    output_root: Path,
    seasons_filter: list[str],
    before_season: int | None,
    after_season: int | None,
    years: int,
    limit: int,
) -> None:
    data = json.loads(meta_file.read_text(encoding="utf-8"))
    seasons_numeric = sorted(
        {
            int(str(game.get("season_code") or game.get("season_gu") or 0))
            for game in data
            if str(game.get("season_code") or game.get("season_gu") or "").isdigit()
        }
    )
    allowed_by_years = set(seasons_numeric[-years:]) if years else None

    selected = []
    for game in data:
        season_raw = game.get("season_code") or game.get("season_gu")
        if season_raw is None:
            continue
        season_str = normalize_season_code(season_raw)
        try:
            season_int = int(season_raw)
        except Exception:
            season_int = None
        if seasons_filter and season_str not in seasons_filter:
            continue
        if before_season and season_int and season_int > before_season:
            continue
        if after_season and season_int and season_int < after_season:
            continue
        if allowed_by_years and season_int not in allowed_by_years:
            continue
        selected.append(game)

    if limit:
        selected = selected[:limit]

    grouped: dict[str, list[dict]] = {}
    for game in selected:
        season_str = normalize_season_code(game.get("season_code") or game.get("season_gu"))
        grouped.setdefault(season_str, []).append(game)

    session = requests.Session()
    for season_str, games in sorted(grouped.items(), key=lambda x: int(x[0])):
        season_dir = output_root / season_str
        season_dir.mkdir(parents=True, exist_ok=True)
        print(f"[{season_str}] games={len(games)}", flush=True)
        saved = 0
        skipped = 0
        processed = 0

        def sort_key(item: dict) -> tuple[int, int]:
            ym = item.get("ym") or 0
            game_no = item.get("game_no") or 0
            try:
                return int(ym), int(game_no)
            except Exception:
                return 0, 0

        for game in sorted(games, key=sort_key):
            params = normalize_params(
                {
                    "season_gu": season_str,
                    "game_type": game.get("game_type", ""),
                    "game_no": game.get("game_no", ""),
                    "ym": game.get("ym", ""),
                }
            )
            existing = list(
                season_dir.glob(f"game_{params['game_no']}_type{params['game_type']}_*.csv")
            )
            if existing and len(existing) >= 2 and not overwrite:
                skipped += len(existing)
                processed += 1
                continue

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
                out = season_dir / f"game_{params['game_no']}_type{params['game_type']}_{team_slug}.csv"
                if out.exists() and not overwrite:
                    skipped += 1
                    continue
                df.to_csv(out, index=False)
                saved += 1

            time.sleep(sleep)
            processed += 1
            if processed % 25 == 0:
                print(
                    f"[{season_str}] processed {processed} games (saved {saved}, skipped {skipped})",
                    flush=True,
                )

        print(f"saved {saved} files to {season_dir} (skipped {skipped})")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--meta-file", type=Path, default=None, help="JSON meta list (season_code/game_type/ym/game_no)")
    parser.add_argument("--season", default="", help="season_gu override (single season)")
    parser.add_argument("--seasons", default="", help="comma-separated season_gu list")
    parser.add_argument("--years", type=int, default=0, help="latest N seasons")
    parser.add_argument("--months", default="", help="comma-separated ym list")
    parser.add_argument("--before-season", type=int, default=0, help="include seasons <= N (meta only)")
    parser.add_argument("--after-season", type=int, default=0, help="include seasons >= N (meta only)")
    parser.add_argument("--limit", type=int, default=0, help="limit number of games (meta only)")
    parser.add_argument("--overwrite", action="store_true", help="overwrite existing files")
    parser.add_argument("--sleep", type=float, default=0.2, help="delay between requests")
    args = parser.parse_args()

    seasons_filter = []
    if args.season:
        seasons_filter = [normalize_season_code(args.season)]
    elif args.seasons:
        seasons_filter = [normalize_season_code(s.strip()) for s in args.seasons.split(",") if s.strip()]

    if args.meta_file:
        if not args.meta_file.exists():
            raise SystemExit(f"meta file not found: {args.meta_file}")
        collect_from_meta(
            meta_file=args.meta_file,
            sleep=args.sleep,
            overwrite=args.overwrite,
            output_root=OUT_DIR,
            seasons_filter=seasons_filter,
            before_season=args.before_season or None,
            after_season=args.after_season or None,
            years=args.years,
            limit=args.limit,
        )
        return

    session = requests.Session()
    available_seasons = get_available_seasons(session)

    seasons: list[str] = []
    if seasons_filter:
        seasons = seasons_filter
    elif args.years:
        seasons = sorted(set(available_seasons), key=lambda s: int(s))[-args.years :]
    else:
        current = get_current_season(session)
        seasons = [current] if current else []

    months_override = [m.strip() for m in args.months.split(",") if m.strip()] if args.months else []

    if not seasons:
        raise SystemExit("season_gu not found")

    for season_gu in seasons:
        months = months_override or get_months_for_season(season_gu, session)
        if not months:
            print(f"skip season_gu={season_gu}: no months")
            continue
        collect(season_gu, months, args.sleep, args.overwrite, OUT_DIR)


if __name__ == "__main__":
    main()
