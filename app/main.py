from fastapi import FastAPI, Request, Response
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates
from pathlib import Path
from urllib.parse import quote, urlencode
import json
import os
import re
import math
import mimetypes
import pandas as pd

try:
    import app.db as db
except Exception:
    db = None

app = FastAPI()
templates = Jinja2Templates(directory="app/templates")

BASE_DIR = Path(__file__).resolve().parent
ME_ROOT = Path(os.environ.get("ME_ROOT", BASE_DIR.parent)).resolve()
DATA_DIR = BASE_DIR.parent / "data" / "wkbl"
if not DATA_DIR.exists():
    fallback = ME_ROOT / "data" / "wkbl"
    if fallback.exists():
        DATA_DIR = fallback
BOX_DIR = DATA_DIR / "box"
DERIVED_DIR = DATA_DIR / "derived"
STATIC_DIR = BASE_DIR / "static"
ROSTER_PATH = DATA_DIR / "roster_db.json"

PLAYER_DB = {}
if ROSTER_PATH.exists():
    try:
        with ROSTER_PATH.open("r", encoding="utf-8") as f:
            PLAYER_DB = json.load(f)
    except Exception:
        PLAYER_DB = {}

TEAM_META = {
    "우리은행": {"nick": "WON", "logo": "/static/images/team_05.png", "color": "#005BAA"},
    "삼성생명": {"nick": "BLU", "logo": "/static/images/team_03.png", "color": "#007AFF"},
    "신한은행": {"nick": "S-BIRDS", "logo": "/static/images/team_07.png", "color": "#2B3990"},
    "KB스타즈": {"nick": "STARS", "logo": "/static/images/team_01.png", "color": "#FFCC00"},
    "하나은행": {"nick": "1Q", "logo": "/static/images/team_09.png", "color": "#009490"},
    "BNK썸": {"nick": "SUM", "logo": "/static/images/team_11.png", "color": "#D6001C"},
}
DEFAULT_TEAM_META = {"nick": "???", "logo": "", "color": "#ccc"}
PLAYER_PNO = {"김단비": "095226", "이명관": "095778", "조수아": "095912", "변하정": "095104", "강이슬": "095263"}
SEASON_BASE_YEAR = 1979
DEFAULT_PAGE_SIZE = 50
MAX_PAGE_SIZE = 200
MAX_DB_LIMIT = 5000

# 직접 이미지 서빙 (Raw Response)
@app.get("/static/{file_path:path}")
def serve_static(file_path: str):
    try:
        full_path = (STATIC_DIR / file_path).resolve()
    except Exception:
        return Response(content="Not Found", status_code=404)
    if STATIC_DIR.resolve() not in full_path.parents and full_path != STATIC_DIR.resolve():
        return Response(content="Not Found", status_code=404)
    if full_path.exists() and full_path.is_file():
        with full_path.open("rb") as f:
            content = f.read()
        media_type, _ = mimetypes.guess_type(full_path.name)
        return Response(content=content, media_type=media_type or "application/octet-stream")
    return Response(content="Not Found", status_code=404)

def c_i(value) -> int:
    try:
        return int(str(value).strip())
    except Exception:
        return 0

def parse_minutes(value) -> float:
    text = str(value).strip()
    if not text or text == "nan":
        return 0.0
    if ":" in text:
        minutes, seconds = text.split(":", 1)
        return c_i(minutes) + (c_i(seconds) / 60)
    try:
        return float(text)
    except Exception:
        return 0.0

def parse_made_att(value) -> tuple[int, int]:
    text = str(value).strip()
    if "-" not in text:
        return 0, 0
    made, att = text.split("-", 1)
    return c_i(made), c_i(att)


def get_team_meta(team: str) -> dict:
    return TEAM_META.get(team, DEFAULT_TEAM_META)


def season_label(code: str) -> str:
    if not code:
        return ""
    if str(code) == "ALL":
        return "All Seasons"
    try:
        year = int(str(code))
        start = SEASON_BASE_YEAR + year
        end = (start + 1) % 100
        return f"{start}-{str(end).zfill(2)}"
    except Exception:
        return str(code)


def season_options(include_all: bool = True) -> list[dict]:
    seasons = get_season_list()
    options = [{"value": code, "label": season_label(code)} for code in seasons]
    if include_all:
        return [{"value": "ALL", "label": "All Seasons"}] + options
    return options


def build_team_url(team: str, season: str = "") -> str:
    if not team:
        return ""
    url = f"/team?name={quote(str(team), safe='')}"
    if season and season != "ALL":
        url += f"&season={quote(str(season), safe='')}"
    return url


def build_player_key(name: str, team: str, season: str = "") -> str:
    if not name or not team:
        return ""
    if season:
        return f"{name}|{team}|{season}"
    return f"{name}|{team}"


def build_player_url(name: str, team: str, season: str = "") -> str:
    if not name or not team:
        return ""
    url = f"/player?name={quote(str(name), safe='')}&team={quote(str(team), safe='')}"
    if season and season != "ALL":
        url += f"&season={quote(str(season), safe='')}"
    return url


def build_compare_player_url(name: str, team: str, season: str = "") -> str:
    key = build_player_key(name, team, season)
    if not key:
        return ""
    url = f"/compare?mode=players&player_a={quote(key, safe='')}"
    if season and season != "ALL":
        url += f"&season={quote(str(season), safe='')}"
    return url


def build_compare_team_url(team: str, season: str = "") -> str:
    if not team:
        return ""
    url = f"/compare?mode=teams&team_a={quote(str(team), safe='')}"
    if season and season != "ALL":
        url += f"&season={quote(str(season), safe='')}"
    return url


def get_player_photo(name: str) -> str | None:
    pno = PLAYER_PNO.get(name)
    return f"/static/images/player_{pno}.png" if pno else None


def get_team_list(season: str = "ALL") -> list[str]:
    if DERIVED_DIR.exists():
        path = DERIVED_DIR / "players_aggregate.csv"
        if path.exists():
            df = pd.read_csv(path)
            if season and season != "ALL":
                df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
            teams = sorted(df["team"].dropna().unique().tolist())
            return teams
    return sorted(["우리은행", "삼성생명", "신한은행", "KB스타즈", "하나은행", "BNK썸"])


def get_season_list() -> list[str]:
    candidates = [
        DERIVED_DIR / "players_aggregate.csv",
        DERIVED_DIR / "teams_aggregate.csv",
        DERIVED_DIR / "players_games.csv",
    ]
    for path in candidates:
        if path.exists():
            df = pd.read_csv(path)
            if "season_gu" not in df.columns:
                continue
            seasons = {str(s).zfill(3) for s in df["season_gu"].dropna().unique().tolist()}
            return sorted(seasons, key=lambda x: int(x))
    return []


def resolve_season(season: str, allow_all: bool = False) -> str:
    if season == "ALL" and allow_all:
        return "ALL"
    if season and season != "ALL":
        return str(season).zfill(3)
    seasons = get_season_list()
    return seasons[-1] if seasons else "ALL"


def scope_label(scope: str) -> str:
    labels = {
        "totals": "Totals",
        "per_game": "Per Game",
        "per_36": "Per 36",
    }
    return labels.get(scope, "Per Game")


def normalize_int(value: str | int, default: int, min_value: int, max_value: int) -> int:
    try:
        parsed = int(str(value).strip())
    except Exception:
        return default
    if parsed < min_value:
        return min_value
    if parsed > max_value:
        return max_value
    return parsed


def normalize_page(value: str | int) -> int:
    return normalize_int(value, 1, 1, 1_000_000)


def normalize_page_size(value: str | int) -> int:
    return normalize_int(value, DEFAULT_PAGE_SIZE, 1, MAX_PAGE_SIZE)


def build_page_url(path: str, params: dict) -> str:
    filtered = {key: value for key, value in params.items() if value not in (None, "")}
    query = urlencode(filtered, doseq=True)
    return f"{path}?{query}" if query else path


def paginate_items(
    items: list[dict],
    page: str | int,
    page_size: str | int,
    base_path: str,
    table_path: str,
    params: dict,
) -> tuple[list[dict], dict]:
    page = normalize_page(page)
    page_size = normalize_page_size(page_size)
    total_count = len(items)
    total_pages = max(1, math.ceil(total_count / page_size)) if total_count else 1
    if page > total_pages:
        page = total_pages
    start = (page - 1) * page_size
    end = min(start + page_size, total_count)
    pagination = {
        "page": page,
        "page_size": page_size,
        "total_count": total_count,
        "total_pages": total_pages,
        "start": start + 1 if total_count else 0,
        "end": end,
    }
    if total_count and page > 1:
        prev_params = params.copy()
        prev_params.update({"page": page - 1, "page_size": page_size})
        pagination["prev"] = {
            "href": build_page_url(base_path, prev_params),
            "hx": build_page_url(table_path, prev_params),
        }
    if total_count and page < total_pages:
        next_params = params.copy()
        next_params.update({"page": page + 1, "page_size": page_size})
        pagination["next"] = {
            "href": build_page_url(base_path, next_params),
            "hx": build_page_url(table_path, next_params),
        }
    return items[start:end], pagination


def load_players_aggregate(
    season: str = "ALL",
    scope: str = "per_game",
    team_filter: str = "ALL",
    search_query: str = "",
    sort_by: str = "pts",
    order: str = "desc",
) -> list[dict]:
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[df["scope"] == scope]
    if team_filter != "ALL":
        df = df[df["team"] == team_filter]
    if search_query:
        df = df[df["name"].str.contains(search_query, case=False, na=False)]

    allowed = {"pts", "reb", "ast", "stl", "blk", "to", "fg_pct", "fg3_pct", "ts_pct", "efg_pct", "eff", "min_total"}
    if sort_by not in allowed:
        sort_by = "pts"
    df = df.sort_values(by=sort_by, ascending=(order == "asc"))
    rows = df.to_dict(orient="records")
    for row in rows:
        name = str(row.get("name", "")).strip()
        team = str(row.get("team", "")).strip()
        season_val = str(row.get("season_gu", "")).zfill(3)
        row["player_url"] = build_player_url(name, team, season_val)
        row["team_url"] = build_team_url(team, season_val)
        row["compare_url"] = build_compare_player_url(name, team, season_val)
    return rows


def db_available() -> bool:
    if db is None:
        return False
    try:
        if not hasattr(db, "query_players_aggregate"):
            return False
        db_file = getattr(db, "DB_FILE", None)
        return db_file is not None and Path(db_file).exists()
    except Exception:
        return False


def normalize_db_players(rows: list[dict], season: str = "ALL") -> list[dict]:
    for row in rows:
        name = str(row.get("name", "")).strip()
        team = str(row.get("team", "")).strip()
        row["player_url"] = build_player_url(name, team, season)
        row["team_url"] = build_team_url(team, season)
        row["compare_url"] = build_compare_player_url(name, team, season)
        row["pos"] = row.get("pos") or ""
        gp = c_i(row.get("gp", 0))
        row["gp"] = gp
        min_total = float(row.get("min_total") or 0)
        row["min_total"] = min_total
        min_avg = row.get("min_avg")
        if min_avg not in (None, ""):
            row["min_per_game"] = float(min_avg)
        else:
            row["min_per_game"] = min_total / gp if gp else 0.0
        for key in ("pts", "reb", "ast", "stl", "blk", "to", "eff"):
            row[key] = float(row.get(key) or 0)
        for key in ("fg_pct", "fg3_pct", "ft_pct", "efg_pct", "ts_pct"):
            row[key] = float(row.get(key) or 0)
    return rows


def should_use_db_players(season: str) -> bool:
    if season and season != "ALL":
        return False
    if not db_available():
        return False
    return not (DERIVED_DIR / "players_aggregate.csv").exists()


def load_players_db(
    scope: str,
    team_filter: str,
    search_query: str,
    sort_by: str,
    season: str,
    limit: int = MAX_DB_LIMIT,
) -> list[dict]:
    if db is None:
        return []
    try:
        rows = db.query_players_aggregate(
            scope=scope,
            team_filter=team_filter,
            search_query=search_query,
            sort_by=sort_by,
            limit=limit,
        )
    except Exception:
        return []
    return normalize_db_players(rows or [], season)


def load_teams_aggregate(
    season: str = "ALL",
    scope: str = "per_game",
    sort_by: str = "pts",
    order: str = "desc",
) -> list[dict]:
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[df["scope"] == scope]
    if "margin" not in df.columns:
        df["margin"] = 0
    if "pts_against" not in df.columns:
        df["pts_against"] = 0

    allowed = {"pts", "reb", "ast", "stl", "blk", "to", "fg_pct", "fg3_pct", "ts_pct", "efg_pct", "eff", "min_total", "margin"}
    if sort_by not in allowed:
        sort_by = "pts"
    df = df.sort_values(by=sort_by, ascending=(order == "asc"))
    rows = df.to_dict(orient="records")
    for row in rows:
        team = str(row.get("team", "")).strip()
        season_val = str(row.get("season_gu", "")).zfill(3)
        row["team_url"] = build_team_url(team, season_val)
        row["compare_url"] = build_compare_team_url(team, season_val)
    return rows


def load_players_games(
    season: str = "ALL",
    team_filter: str = "ALL",
    search_query: str = "",
    game_no: str = "",
    sort_by: str = "game_no",
    order: str = "desc",
) -> list[dict]:
    path = DERIVED_DIR / "players_games.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    if team_filter != "ALL":
        df = df[df["team"] == team_filter]
    if search_query:
        df = df[df["name"].str.contains(search_query, case=False, na=False)]
    if game_no:
        try:
            game_val = int(str(game_no).replace("#", ""))
            df = df[df["game_no"] == game_val]
        except Exception:
            pass

    df["game_label"] = df["game_no"].apply(lambda x: f"#{int(x)}")
    df["min_display"] = df["min"].fillna("0:00")
    if "game_key" in df.columns:
        df["boxscore_url"] = df["game_key"].apply(lambda x: f"/boxscore?game_key={quote(str(x), safe='')}")

    allowed = {"game_no", "pts", "reb", "ast", "eff", "ts_pct"}
    if sort_by not in allowed:
        sort_by = "game_no"
    df = df.sort_values(by=sort_by, ascending=(order == "asc"))
    return df.to_dict(orient="records")


def load_player_scopes(name: str, team: str, season: str) -> dict:
    if not name or not team:
        return {}
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return {}
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[(df["name"] == name) & (df["team"] == team)]
    if df.empty:
        return {}
    scopes = {}
    for key in ("per_game", "per_36", "totals"):
        row = df[df["scope"] == key]
        if row.empty:
            continue
        record = row.iloc[0].to_dict()
        record["scope_key"] = key
        record["scope_label"] = scope_label(key)
        scopes[key] = record
    return scopes


def load_player_game_log(name: str, team: str, season: str, limit: int = 12) -> list[dict]:
    if not name or not team:
        return []
    path = DERIVED_DIR / "players_games.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[(df["name"] == name) & (df["team"] == team)]
    if df.empty:
        return []
    df = df.sort_values(by=["ym", "game_no"], ascending=False)
    df["game_label"] = df["game_no"].apply(lambda x: f"#{int(x)}")
    df["min_display"] = df["min"].fillna("0:00")
    return df.head(limit).to_dict(orient="records")


def load_team_scopes(team: str, season: str) -> dict:
    if not team:
        return {}
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return {}
    df = pd.read_csv(path)
    if "margin" not in df.columns:
        df["margin"] = 0
    if "pts_against" not in df.columns:
        df["pts_against"] = 0
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[df["team"] == team]
    if df.empty:
        return {}
    scopes = {}
    for key in ("per_game", "totals"):
        row = df[df["scope"] == key]
        if row.empty:
            continue
        record = row.iloc[0].to_dict()
        record["scope_key"] = key
        record["scope_label"] = scope_label(key)
        scopes[key] = record
    return scopes


def load_team_leaders(team: str, season: str) -> dict:
    players = load_players_aggregate(season=season, scope="per_game", team_filter=team, sort_by="pts")
    if not players:
        return {}
    leaders = {}
    for key in ("pts", "reb", "ast"):
        leaders[key] = max(players, key=lambda row: row.get(key, 0))
    return leaders


def load_game_summary(
    season: str = "ALL",
    team_filter: str = "ALL",
    search_query: str = "",
    sort_by: str = "game_no",
    order: str = "desc",
) -> list[dict]:
    path = DERIVED_DIR / "games_summary.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    if team_filter != "ALL":
        df = df[(df["team_a"] == team_filter) | (df["team_b"] == team_filter)]
    if search_query:
        df = df[
            df["team_a"].str.contains(search_query, case=False, na=False)
            | df["team_b"].str.contains(search_query, case=False, na=False)
        ]

    allowed = {"game_no", "margin", "pts_a", "pts_b"}
    if sort_by not in allowed:
        sort_by = "game_no"
    df = df.sort_values(by=sort_by, ascending=(order == "asc"))

    df["game_label"] = df["game_no"].apply(lambda x: f"#{int(x)}")
    df["boxscore_url"] = df["game_key"].apply(lambda x: f"/boxscore?game_key={quote(str(x), safe='')}")
    return df.to_dict(orient="records")


def load_standings(season: str) -> list[dict]:
    path = DERIVED_DIR / "standings.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if "season_gu" not in df.columns:
        return []
    df["season_gu"] = df["season_gu"].astype(str).str.zfill(3)
    if season and season != "ALL":
        df = df[df["season_gu"] == str(season)]
    if df.empty:
        return []
    df = df.sort_values(by=["win_pct", "margin_per_game", "pts_for"], ascending=[False, False, False])
    leader = df.iloc[0]
    df["gb"] = df.apply(
        lambda r: round(((leader["wins"] - r["wins"]) + (r["losses"] - leader["losses"])) / 2, 1),
        axis=1,
    )
    df["rank"] = range(1, len(df) + 1)
    rows = df.to_dict(orient="records")
    for row in rows:
        team = str(row.get("team", "")).strip()
        season_val = str(row.get("season_gu", "")).zfill(3)
        row["team_url"] = build_team_url(team, season_val)
    return rows


def load_boxscore(game_key: str) -> dict:
    if not game_key:
        return {}
    path = DERIVED_DIR / "players_games.csv"
    if not path.exists():
        return {}
    df = pd.read_csv(path)
    df = df[df["game_key"].astype(str) == str(game_key)]
    if df.empty:
        return {}

    df["min_display"] = df["min"].fillna("0:00")
    df["season_gu"] = df["season_gu"].astype(str).str.zfill(3)
    df["game_label"] = df["game_no"].apply(lambda x: f"#{int(x)}")

    team_rows = []
    for team, group in df.groupby("team"):
        totals = {
            "min_total": round(group["min_dec"].sum(), 1),
            "pts": int(group["pts"].sum()),
            "reb": int(group["reb"].sum()),
            "ast": int(group["ast"].sum()),
            "stl": int(group["stl"].sum()),
            "blk": int(group["blk"].sum()),
            "to": int(group["to"].sum()),
            "fg2_m": int(group["fg2_m"].sum()),
            "fg2_a": int(group["fg2_a"].sum()),
            "fg3_m": int(group["fg3_m"].sum()),
            "fg3_a": int(group["fg3_a"].sum()),
            "ft_m": int(group["ft_m"].sum()),
            "ft_a": int(group["ft_a"].sum()),
        }
        totals["fg_m"] = totals["fg2_m"] + totals["fg3_m"]
        totals["fg_a"] = totals["fg2_a"] + totals["fg3_a"]
        totals["fg2_display"] = f"{totals['fg2_m']}-{totals['fg2_a']}"
        totals["fg3_display"] = f"{totals['fg3_m']}-{totals['fg3_a']}"
        totals["ft_display"] = f"{totals['ft_m']}-{totals['ft_a']}"
        totals["fg_pct"] = pct(totals["fg_m"], totals["fg_a"])
        totals["fg3_pct"] = pct(totals["fg3_m"], totals["fg3_a"])
        totals["ft_pct"] = pct(totals["ft_m"], totals["ft_a"])
        totals["efg_pct"] = pct(totals["fg_m"] + 0.5 * totals["fg3_m"], totals["fg_a"])
        totals["ts_pct"] = pct(totals["pts"], 2 * (totals["fg_a"] + 0.44 * totals["ft_a"]))

        rows = group.sort_values(by="min_dec", ascending=False).to_dict(orient="records")
        team_rows.append(
            {
                "team": team,
                "meta": get_team_meta(team),
                "rows": rows,
                "totals": totals,
            }
        )

    team_rows.sort(key=lambda r: r["totals"]["pts"], reverse=True)
    top = team_rows[0]
    bottom = team_rows[1] if len(team_rows) > 1 else None

    header = df.iloc[0]
    summary = {
        "season_gu": header["season_gu"],
        "game_label": header["game_label"],
        "game_key": header["game_key"],
        "game_no": int(header["game_no"]),
        "ym": str(header.get("ym", "")),
    }
    summary["season_label"] = season_label(summary["season_gu"])

    if bottom:
        summary["scoreline"] = f"{top['team']} {top['totals']['pts']} - {bottom['totals']['pts']} {bottom['team']}"
        summary["margin"] = top["totals"]["pts"] - bottom["totals"]["pts"]
    else:
        summary["scoreline"] = f"{top['team']} {top['totals']['pts']}"
        summary["margin"] = 0

    return {"summary": summary, "teams": team_rows}


def get_player_options(season: str = "ALL") -> list[dict]:
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[df["scope"] == "totals"].sort_values(["team", "name"])
    options = []
    for _, row in df.iterrows():
        name = str(row["name"]).strip()
        team = str(row["team"]).strip()
        season_val = str(row.get("season_gu", "")).zfill(3)
        value = f"{name}|{team}|{season_val}"
        label = f"{name} ({team}) · {season_val}"
        options.append({"value": value, "label": label})
    return options


def find_player(scope: str, key: str, season: str = "") -> dict | None:
    if not key or "|" not in key:
        return None
    parts = key.split("|")
    if len(parts) == 3:
        name, team, season_key = parts
    else:
        name, team = parts[0], parts[1]
        season_key = season
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return None
    df = pd.read_csv(path)
    if season_key and season_key != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season_key)]
    df = df[(df["scope"] == scope) & (df["name"] == name) & (df["team"] == team)]
    if df.empty:
        return None
    return df.iloc[0].to_dict()


def get_team_options(season: str = "ALL") -> list[dict]:
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    teams = sorted(df["team"].dropna().unique().tolist())
    return [{"value": team, "label": team} for team in teams]


def find_team(scope: str, team: str, season: str = "") -> dict | None:
    if not team:
        return None
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return None
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[(df["scope"] == scope) & (df["team"] == team)]
    if df.empty:
        return None
    return df.iloc[0].to_dict()


def format_metric(value, digits: int = 1) -> str:
    try:
        return f"{float(value):.{digits}f}"
    except Exception:
        return str(value)


def to_float(value) -> float:
    try:
        return float(value)
    except Exception:
        return 0.0


def pct(made: float, att: float) -> float:
    try:
        if att <= 0:
            return 0.0
        return round((made / att) * 100, 1)
    except Exception:
        return 0.0


def build_compare_metrics(left: dict, right: dict, mode: str = "players") -> list[dict]:
    configs = [
        ("GP", "gp", 0, False),
        ("MIN", "min_total", 1, False),
        ("PTS", "pts", 2, False),
        ("REB", "reb", 2, False),
        ("AST", "ast", 2, False),
        ("STL", "stl", 2, False),
        ("BLK", "blk", 2, False),
        ("TO", "to", 2, False),
        ("FG%", "fg_pct", 1, False),
        ("3P%", "fg3_pct", 1, False),
        ("FT%", "ft_pct", 1, False),
        ("eFG%", "efg_pct", 1, False),
        ("TS%", "ts_pct", 1, False),
        ("EFF", "eff", 1, True),
    ]
    if mode == "teams":
        configs.append(("MARGIN", "margin", 1, True))

    metrics = []
    for label, key, digits, abs_bar in configs:
        left_val = to_float(left.get(key, 0))
        right_val = to_float(right.get(key, 0))
        bar_left = abs(left_val) if abs_bar else max(left_val, 0)
        bar_right = abs(right_val) if abs_bar else max(right_val, 0)
        max_val = max(bar_left, bar_right)
        if max_val <= 0:
            left_bar = 0
            right_bar = 0
        else:
            left_bar = round((bar_left / max_val) * 100, 1)
            right_bar = round((bar_right / max_val) * 100, 1)
        metrics.append(
            {
                "label": label,
                "left_display": format_metric(left_val, digits),
                "right_display": format_metric(right_val, digits),
                "left_bar": left_bar,
                "right_bar": right_bar,
            }
        )
    return metrics


def compare_context(
    mode: str,
    scope: str,
    season: str,
    player_a: str = "",
    player_b: str = "",
    team_a: str = "",
    team_b: str = "",
) -> dict:
    mode = "teams" if mode == "teams" else "players"
    season = resolve_season(season)
    seasons = season_options(include_all=False)
    player_scopes = [("per_game", "Per Game"), ("per_36", "Per 36"), ("totals", "Totals")]
    team_scopes = [("per_game", "Per Game"), ("totals", "Totals")]

    player_scope_keys = [value for value, _ in player_scopes]
    team_scope_keys = [value for value, _ in team_scopes]

    if mode == "teams" and scope not in team_scope_keys:
        scope = "per_game"
    if mode == "players" and scope not in player_scope_keys:
        scope = "per_game"

    if mode == "teams":
        options = get_team_options(season)
        left = find_team(scope, team_a, season)
        right = find_team(scope, team_b, season)
        select_label = "Team"
        select_name_a = "team_a"
        select_name_b = "team_b"
        selected_a = team_a
        selected_b = team_b
        compare_title = "Team vs Team"
        compare_desc = "팀 단위 per-game, totals 지표를 비교합니다. 시즌 전반의 생산성을 확인하세요."
        entity_count_label = f"{len(options)} Teams"
        scopes = team_scopes
        left_label = left.get("team") if left else "Team A"
        right_label = right.get("team") if right else "Team B"
    else:
        options = get_player_options(season)
        left = find_player(scope, player_a, season)
        right = find_player(scope, player_b, season)
        select_label = "Player"
        select_name_a = "player_a"
        select_name_b = "player_b"
        selected_a = player_a
        selected_b = player_b
        compare_title = "Player vs Player"
        compare_desc = "basketball-reference 스타일로 기본 지표를 나란히 비교합니다. scope를 바꿔 per-game과 per-36을 확인하세요."
        entity_count_label = f"{len(options)} Players"
        scopes = player_scopes
        left_label = f"{left.get('name')} ({left.get('team')})" if left else "Player A"
        right_label = f"{right.get('name')} ({right.get('team')})" if right else "Player B"

    metrics = build_compare_metrics(left, right, mode) if left and right else []

    player_scope = scope if scope in player_scope_keys else "per_game"
    team_scope = scope if scope in team_scope_keys else "per_game"
    mode_switch = [
        {
            "label": "Players",
            "url": f"/compare?mode=players&scope={player_scope}&season={season}",
            "active": mode == "players",
        },
        {
            "label": "Teams",
            "url": f"/compare?mode=teams&scope={team_scope}&season={season}",
            "active": mode == "teams",
        },
    ]

    return {
        "mode": mode,
        "scope": scope,
        "season": season,
        "seasons": seasons,
        "season_label": season_label(season),
        "scope_label": scope_label(scope),
        "scopes": scopes,
        "options": options,
        "selected_a": selected_a,
        "selected_b": selected_b,
        "select_label": select_label,
        "select_name_a": select_name_a,
        "select_name_b": select_name_b,
        "compare_title": compare_title,
        "compare_desc": compare_desc,
        "entity_count_label": entity_count_label,
        "left_label": left_label,
        "right_label": right_label,
        "metrics": metrics,
        "left": left,
        "right": right,
        "mode_switch": mode_switch,
    }

def load_real_stats(sort_by="eff", team_filter="ALL", pos_filter="ALL", search_query=""):
    sort_aliases = {
        "points": "pts",
        "point": "pts",
        "minutes": "min",
        "minute": "min",
        "turnovers": "to",
    }
    sort_key = sort_aliases.get(sort_by, sort_by)
    stats = []

    data_dir = BOX_DIR if BOX_DIR.exists() else DATA_DIR
    if not data_dir.exists():
        return []
    files = sorted(data_dir.rglob("*.csv"))
    for f in files:
        try:
            df = pd.read_csv(f)
            if "name" not in df.columns:
                # WKBL 특유의 2중 헤더 처리
                df = pd.read_csv(
                    f,
                    skiprows=2,
                    names=["name", "pos", "min", "fg2", "fg3", "ft", "reb_off", "reb_def", "reb", "ast", "pf", "stl", "to", "blk", "pts"],
                    on_bad_lines="skip",
                )
            filename = f.name
            game_match = re.search(r"game_(\d+)", filename)
            game_label = f"#{game_match.group(1)}" if game_match else "#--"
            
            for _, row in df.iterrows():
                name = str(row.get("name", "")).strip()
                if not name or name in ["합계", "Total", "팀합계", "선수", "nan"]:
                    continue
                
                try:
                    # 1. 팀 식별 (CSV 우선)
                    team = str(row.get("team", "")).strip()
                    if not team:
                        team_db = PLAYER_DB.get(name, "")
                        if team_db:
                            team = team_db
                        else:
                            # DB에 없으면 파일명으로 추측 (최후의 수단, Game 33 기준)
                            if "table_2" in filename:
                                team = "우리은행"
                            elif "table_3" in filename:
                                team = "삼성생명"
                            else:
                                team = "Unknown"
                    
                    meta = get_team_meta(team)
                    pno = PLAYER_PNO.get(name)

                    min_raw = str(row.get("min", "")).strip()
                    min_display = min_raw if min_raw and min_raw != "nan" else "0:00"
                    min_value = parse_minutes(min_raw)

                    pts = c_i(row.get("pts"))
                    reb = c_i(row.get("reb"))
                    ast = c_i(row.get("ast"))
                    stl = c_i(row.get("stl"))
                    blk = c_i(row.get("blk"))
                    to = c_i(row.get("to"))

                    fg2_m, fg2_a = parse_made_att(row.get("fg2", "0-0"))
                    fg3_m, fg3_a = parse_made_att(row.get("fg3", "0-0"))
                    ft_m, ft_a = parse_made_att(row.get("ft", "0-0"))

                    fg_made = fg2_m + fg3_m
                    fg_att = fg2_a + fg3_a
                    fg_pct = round((fg_made / fg_att * 100), 1) if fg_att > 0 else 0.0
                    eff = pts + reb + ast + stl + blk - to - ((fg_att - fg_made) + (ft_a - ft_m))
                    eff_bar = min(int(abs(eff) / 40 * 100), 100)

                    stats.append({
                        "game": game_label,
                        "name": name,
                        "team": team,
                        "nick": meta["nick"],
                        "logo": meta["logo"],
                        "color": meta["color"],
                        "photo": f"/static/images/player_{pno}.png" if pno else None,
                        "pos": str(row.get("pos", "")).strip(),
                        "min": min_value,
                        "min_display": min_display,
                        "pts": pts,
                        "reb": reb,
                        "ast": ast,
                        "stl": stl,
                        "blk": blk,
                        "to": to,
                        "fg_pct": fg_pct,
                        "eff": eff,
                        "eff_pos": eff_bar if eff > 0 else 0,
                        "eff_neg": eff_bar if eff < 0 else 0,
                    })
                except: continue
        except: continue
            
    # 필터링
    if team_filter != "ALL": stats = [s for s in stats if s['team'] == team_filter]
    if pos_filter != "ALL": stats = [s for s in stats if s['pos'] == pos_filter]
    if search_query: stats = [s for s in stats if search_query.lower() in s['name'].lower()]

    # 정렬
    stats.sort(key=lambda x: x.get(sort_key, 0), reverse=True)
    return stats

@app.get("/test")
async def test():
    return HTMLResponse("<h1>OK</h1>")

@app.get("/", response_class=HTMLResponse)
async def index(request: Request):
    return await players(request)

@app.get("/refresh", response_class=HTMLResponse)
async def refresh(request: Request, sort: str = "eff", team: str = "ALL", pos: str = "ALL", search: str = ""):
    stats = load_real_stats(sort_by=sort, team_filter=team, pos_filter=pos, search_query=search)
    return templates.TemplateResponse("partials/stats_table.html", {"request": request, "stats": stats})


@app.get("/players", response_class=HTMLResponse)
async def players(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    team: str = "ALL",
    search: str = "",
    sort: str = "pts",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    scopes = [("per_game", "Per Game"), ("per_36", "Per 36"), ("totals", "Totals")]
    sort_options = [
        ("pts", "PTS"),
        ("margin", "Margin"),
        ("reb", "REB"),
        ("ast", "AST"),
        ("stl", "STL"),
        ("blk", "BLK"),
        ("eff", "EFF"),
        ("ts_pct", "TS%"),
        ("fg3_pct", "3P%"),
        ("min_total", "MIN"),
    ]
    season = resolve_season(season, allow_all=True)
    seasons = season_options(include_all=True)
    params = {
        "season": season,
        "scope": scope,
        "team": team,
        "search": search,
        "sort": sort,
    }
    if should_use_db_players(season):
        full_stats = load_players_db(
            scope=scope,
            team_filter=team,
            search_query=search,
            sort_by=sort,
            season=season,
            limit=MAX_DB_LIMIT,
        )
        teams = []
        if db is not None:
            try:
                teams = db.get_all_teams()
            except Exception:
                teams = []
        if not teams:
            teams = get_team_list(season)
    else:
        full_stats = load_players_aggregate(
            season=season,
            scope=scope,
            team_filter=team,
            search_query=search,
            sort_by=sort,
        )
        teams = get_team_list(season)
    leader = full_stats[0] if full_stats else None
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/players",
        table_path="/players/table",
        params=params,
    )
    return templates.TemplateResponse(
        "players.html",
        {
            "request": request,
            "active_page": "players",
            "page_title": "WKBL Players",
            "stats": stats,
            "leader": leader,
            "total_count": pagination["total_count"],
            "season": season,
            "seasons": seasons,
            "season_label": season_label(season),
            "scope": scope,
            "scope_label": scope_label(scope),
            "team": team,
            "teams": teams,
            "search": search,
            "sort": sort,
            "scopes": scopes,
            "sort_options": sort_options,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/players/table", response_class=HTMLResponse)
async def players_table(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    team: str = "ALL",
    search: str = "",
    sort: str = "pts",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    season = resolve_season(season, allow_all=True)
    params = {
        "season": season,
        "scope": scope,
        "team": team,
        "search": search,
        "sort": sort,
    }
    if should_use_db_players(season):
        full_stats = load_players_db(
            scope=scope,
            team_filter=team,
            search_query=search,
            sort_by=sort,
            season=season,
            limit=MAX_DB_LIMIT,
        )
    else:
        full_stats = load_players_aggregate(
            season=season,
            scope=scope,
            team_filter=team,
            search_query=search,
            sort_by=sort,
        )
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/players",
        table_path="/players/table",
        params=params,
    )
    return templates.TemplateResponse(
        "partials/players_table.html",
        {
            "request": request,
            "stats": stats,
            "scope": scope,
            "season": season,
            "team": team,
            "search": search,
            "sort": sort,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/player", response_class=HTMLResponse)
async def player_profile(
    request: Request,
    name: str = "",
    team: str = "",
    season: str = "",
    scope: str = "per_game",
):
    season = resolve_season(season)
    scopes = load_player_scopes(name, team, season)
    primary = scopes.get(scope) or scopes.get("per_game") or next(iter(scopes.values()), None)
    scope_rows = [scopes[key] for key in ("per_game", "per_36", "totals") if key in scopes]
    games = load_player_game_log(name, team, season)
    team_meta = get_team_meta(team)
    return templates.TemplateResponse(
        "player.html",
        {
            "request": request,
            "active_page": "players",
            "page_title": f"{name} | WKBL Player",
            "player": primary,
            "player_photo": get_player_photo(name),
            "team_meta": team_meta,
            "scope_rows": scope_rows,
            "games": games,
            "season": season,
            "season_label": season_label(season),
            "compare_url": build_compare_player_url(name, team, season),
            "team_url": build_team_url(team, season),
            "back_url": f"/players?season={season}&team={quote(str(team), safe='')}" if team else f"/players?season={season}",
        },
    )


@app.get("/teams", response_class=HTMLResponse)
async def teams(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    sort: str = "pts",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    scopes = [("per_game", "Per Game"), ("totals", "Totals")]
    sort_options = [
        ("pts", "PTS"),
        ("reb", "REB"),
        ("ast", "AST"),
        ("stl", "STL"),
        ("blk", "BLK"),
        ("eff", "EFF"),
        ("ts_pct", "TS%"),
        ("fg3_pct", "3P%"),
        ("min_total", "MIN"),
    ]
    season = resolve_season(season, allow_all=True)
    seasons = season_options(include_all=True)
    full_stats = load_teams_aggregate(season=season, scope=scope, sort_by=sort)
    leader = full_stats[0] if full_stats else None
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/teams",
        table_path="/teams/table",
        params={"season": season, "scope": scope, "sort": sort},
    )
    return templates.TemplateResponse(
        "teams.html",
        {
            "request": request,
            "active_page": "teams",
            "page_title": "WKBL Teams",
            "stats": stats,
            "leader": leader,
            "total_count": pagination["total_count"],
            "season": season,
            "seasons": seasons,
            "season_label": season_label(season),
            "scope": scope,
            "scope_label": scope_label(scope),
            "sort": sort,
            "scopes": scopes,
            "sort_options": sort_options,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/team", response_class=HTMLResponse)
async def team_profile(
    request: Request,
    name: str = "",
    season: str = "",
    scope: str = "per_game",
):
    season = resolve_season(season)
    scopes = load_team_scopes(name, season)
    primary = scopes.get(scope) or scopes.get("per_game") or next(iter(scopes.values()), None)
    scope_rows = [scopes[key] for key in ("per_game", "totals") if key in scopes]
    top_players = load_players_aggregate(season=season, scope="per_game", team_filter=name, sort_by="pts")[:8]
    leaders = load_team_leaders(name, season) if name else {}
    team_meta = get_team_meta(name)
    return templates.TemplateResponse(
        "team.html",
        {
            "request": request,
            "active_page": "teams",
            "page_title": f"{name} | WKBL Team",
            "team_name": name,
            "team_meta": team_meta,
            "team": primary,
            "scope_rows": scope_rows,
            "top_players": top_players,
            "leaders": leaders,
            "season": season,
            "season_label": season_label(season),
            "compare_url": build_compare_team_url(name, season),
            "back_url": f"/teams?season={season}",
        },
    )


@app.get("/teams/table", response_class=HTMLResponse)
async def teams_table(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    sort: str = "pts",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    season = resolve_season(season, allow_all=True)
    full_stats = load_teams_aggregate(season=season, scope=scope, sort_by=sort)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/teams",
        table_path="/teams/table",
        params={"season": season, "scope": scope, "sort": sort},
    )
    return templates.TemplateResponse(
        "partials/teams_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "scope": scope,
            "sort": sort,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/standings", response_class=HTMLResponse)
async def standings(
    request: Request,
    season: str = "",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    season = resolve_season(season)
    seasons = season_options(include_all=False)
    full_stats = load_standings(season)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/standings",
        table_path="/standings/table",
        params={"season": season},
    )
    return templates.TemplateResponse(
        "standings.html",
        {
            "request": request,
            "active_page": "standings",
            "page_title": "WKBL Standings",
            "stats": stats,
            "season": season,
            "season_label": season_label(season),
            "seasons": seasons,
            "total_count": pagination["total_count"],
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/standings/table", response_class=HTMLResponse)
async def standings_table(
    request: Request,
    season: str = "",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    season = resolve_season(season)
    full_stats = load_standings(season)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/standings",
        table_path="/standings/table",
        params={"season": season},
    )
    return templates.TemplateResponse(
        "partials/standings_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/games", response_class=HTMLResponse)
async def games(
    request: Request,
    season: str = "",
    team: str = "ALL",
    search: str = "",
    game_no: str = "",
    sort: str = "game_no",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    sort_options = [
        ("game_no", "Game No"),
        ("pts", "PTS"),
        ("reb", "REB"),
        ("ast", "AST"),
        ("eff", "EFF"),
        ("ts_pct", "TS%"),
    ]
    season = resolve_season(season, allow_all=True)
    seasons = season_options(include_all=True)
    full_stats = load_players_games(
        season=season,
        team_filter=team,
        search_query=search,
        game_no=game_no,
        sort_by=sort,
    )
    leader = full_stats[0] if full_stats else None
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/games",
        table_path="/games/table",
        params={
            "season": season,
            "team": team,
            "search": search,
            "game_no": game_no,
            "sort": sort,
        },
    )
    return templates.TemplateResponse(
        "games.html",
        {
            "request": request,
            "active_page": "games",
            "page_title": "WKBL Game Log",
            "stats": stats,
            "leader": leader,
            "total_count": pagination["total_count"],
            "season": season,
            "seasons": seasons,
            "season_label": season_label(season),
            "teams": get_team_list(season),
            "team": team,
            "search": search,
            "game_no": game_no,
            "sort": sort,
            "sort_options": sort_options,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/games/table", response_class=HTMLResponse)
async def games_table(
    request: Request,
    season: str = "",
    team: str = "ALL",
    search: str = "",
    game_no: str = "",
    sort: str = "game_no",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    season = resolve_season(season, allow_all=True)
    full_stats = load_players_games(
        season=season,
        team_filter=team,
        search_query=search,
        game_no=game_no,
        sort_by=sort,
    )
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/games",
        table_path="/games/table",
        params={
            "season": season,
            "team": team,
            "search": search,
            "game_no": game_no,
            "sort": sort,
        },
    )
    return templates.TemplateResponse(
        "partials/games_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "team": team,
            "search": search,
            "game_no": game_no,
            "sort": sort,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/boxscores", response_class=HTMLResponse)
async def boxscores(
    request: Request,
    season: str = "",
    team: str = "ALL",
    search: str = "",
    sort: str = "game_no",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    sort_options = [
        ("game_no", "Game No"),
        ("margin", "Margin"),
        ("pts_a", "Score A"),
        ("pts_b", "Score B"),
    ]
    season = resolve_season(season, allow_all=True)
    seasons = season_options(include_all=True)
    full_stats = load_game_summary(
        season=season,
        team_filter=team,
        search_query=search,
        sort_by=sort,
    )
    leader = full_stats[0] if full_stats else None
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/boxscores",
        table_path="/boxscores/table",
        params={
            "season": season,
            "team": team,
            "search": search,
            "sort": sort,
        },
    )
    return templates.TemplateResponse(
        "boxscores.html",
        {
            "request": request,
            "active_page": "boxscores",
            "page_title": "WKBL Boxscores",
            "stats": stats,
            "leader": leader,
            "total_count": pagination["total_count"],
            "season": season,
            "seasons": seasons,
            "season_label": season_label(season),
            "teams": get_team_list(season),
            "team": team,
            "search": search,
            "sort": sort,
            "sort_options": sort_options,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/boxscores/table", response_class=HTMLResponse)
async def boxscores_table(
    request: Request,
    season: str = "",
    team: str = "ALL",
    search: str = "",
    sort: str = "game_no",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
):
    season = resolve_season(season, allow_all=True)
    full_stats = load_game_summary(
        season=season,
        team_filter=team,
        search_query=search,
        sort_by=sort,
    )
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/boxscores",
        table_path="/boxscores/table",
        params={
            "season": season,
            "team": team,
            "search": search,
            "sort": sort,
        },
    )
    return templates.TemplateResponse(
        "partials/boxscores_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "team": team,
            "search": search,
            "sort": sort,
            "pagination": pagination,
            "page_size": pagination["page_size"],
        },
    )


@app.get("/boxscore", response_class=HTMLResponse)
async def boxscore(
    request: Request,
    game_key: str = "",
):
    data = load_boxscore(game_key)
    return templates.TemplateResponse(
        "boxscore.html",
        {
            "request": request,
            "active_page": "boxscores",
            "page_title": "WKBL Boxscore",
            **data,
        },
    )


@app.get("/compare", response_class=HTMLResponse)
async def compare(
    request: Request,
    mode: str = "players",
    season: str = "",
    scope: str = "per_game",
    player_a: str = "",
    player_b: str = "",
    team_a: str = "",
    team_b: str = "",
):
    context = compare_context(
        mode=mode,
        scope=scope,
        season=season,
        player_a=player_a,
        player_b=player_b,
        team_a=team_a,
        team_b=team_b,
    )
    return templates.TemplateResponse(
        "compare.html",
        {
            "request": request,
            "active_page": "compare",
            "page_title": "WKBL Compare",
            **context,
        },
    )


@app.get("/compare/table", response_class=HTMLResponse)
async def compare_table(
    request: Request,
    mode: str = "players",
    season: str = "",
    scope: str = "per_game",
    player_a: str = "",
    player_b: str = "",
    team_a: str = "",
    team_b: str = "",
):
    context = compare_context(
        mode=mode,
        scope=scope,
        season=season,
        player_a=player_a,
        player_b=player_b,
        team_a=team_a,
        team_b=team_b,
    )
    return templates.TemplateResponse(
        "partials/compare_table.html",
        {"request": request, **context},
    )

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8080)
