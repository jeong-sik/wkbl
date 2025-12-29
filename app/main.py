from fastapi import FastAPI, Request, Response
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.templating import Jinja2Templates
from pathlib import Path
from urllib.parse import quote, urlencode
import hashlib
import html
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
WKBL_BASE_URL = "https://www.wkbl.or.kr"
BOX_DIR = DATA_DIR / "box"
DERIVED_DIR = DATA_DIR / "derived"
STATIC_DIR = BASE_DIR / "static"
ROSTER_PATH = DATA_DIR / "roster_db.json"
QA_DIR = DATA_DIR / "qa"
QA_REPORT_PATH = QA_DIR / "qa_report.json"
QA_REPORT_MD_PATH = QA_DIR / "qa_report.md"
PHOTO_BLACKLIST_PATH = DATA_DIR / "photo_blacklist.json"
PHOTO_OVERRIDE_PATH = DATA_DIR / "photo_overrides.json"

PLAYER_DB = {}
PHOTO_BY_NAME_TEAM = {}
PHOTO_BY_NAME = {}
PHOTO_BLACKLIST: set[str] = set()
PHOTO_OVERRIDE_BY_NAME_TEAM = {}
PHOTO_OVERRIDE_BY_NAME = {}
if ROSTER_PATH.exists():
    try:
        with ROSTER_PATH.open("r", encoding="utf-8") as f:
            roster = json.load(f)
            if isinstance(roster, dict) and "team_by_name" in roster:
                PLAYER_DB = roster.get("team_by_name", {})
                PHOTO_BY_NAME_TEAM = roster.get("photo_by_name_team", {})
                PHOTO_BY_NAME = roster.get("photo_by_name", {})
            elif isinstance(roster, dict):
                PLAYER_DB = roster
    except Exception:
        PLAYER_DB = {}
        PHOTO_BY_NAME_TEAM = {}
        PHOTO_BY_NAME = {}

if PHOTO_BLACKLIST_PATH.exists():
    try:
        data = json.loads(PHOTO_BLACKLIST_PATH.read_text(encoding="utf-8"))
        invalid = data.get("invalid_pnos", []) if isinstance(data, dict) else []
        PHOTO_BLACKLIST = {str(pno) for pno in invalid}
    except Exception:
        PHOTO_BLACKLIST = set()

if PHOTO_OVERRIDE_PATH.exists():
    try:
        data = json.loads(PHOTO_OVERRIDE_PATH.read_text(encoding="utf-8"))
        if isinstance(data, dict):
            PHOTO_OVERRIDE_BY_NAME_TEAM = data.get("photo_by_name_team", {}) or {}
            PHOTO_OVERRIDE_BY_NAME = data.get("photo_by_name", {}) or {}
    except Exception:
        PHOTO_OVERRIDE_BY_NAME_TEAM = {}
        PHOTO_OVERRIDE_BY_NAME = {}

TEAM_META = {
    "우리은행": {"nick": "WON", "logo": "/static/images/team_05.png", "color": "#005BAA"},
    "삼성생명": {"nick": "BLU", "logo": "/static/images/team_03.png", "color": "#007AFF"},
    "신한은행": {"nick": "S-BIRDS", "logo": "/static/images/team_07.png", "color": "#2B3990"},
    "KB스타즈": {"nick": "STARS", "logo": "/static/images/team_01.png", "color": "#FFCC00"},
    "하나은행": {"nick": "1Q", "logo": "/static/images/team_09.png", "color": "#009490"},
    "BNK썸": {"nick": "SUM", "logo": "/static/images/team_11.png", "color": "#D6001C"},
}
DEFAULT_TEAM_META = {"nick": "???", "logo": "", "color": "#ccc"}
TEAM_COLOR_POOL = [
    "#0F4C81",
    "#FF6B2C",
    "#0F9D8A",
    "#6B4EFF",
    "#D64545",
    "#2FBF71",
    "#FFB400",
    "#6B6259",
]
PLAYER_PNO = {"김단비": "095226", "이명관": "095778", "조수아": "095912", "변하정": "095104", "강이슬": "095263"}
TEAM_ALIAS = {
    "BNK 썸": "BNK썸",
    "BNK  썸": "BNK썸",
    "KB 스타즈": "KB스타즈",
    "우리 은행": "우리은행",
    "하나 은행": "하나은행",
    "신한 은행": "신한은행",
    "삼성 생명": "삼성생명",
}
SEASON_BASE_YEAR = 1979
DEFAULT_PAGE_SIZE = 40
MAX_PAGE_SIZE = 200
MAX_DB_LIMIT = 5000
DEFAULT_PAGER_MODE = "bottom"
DEFAULT_PER36_MIN_MINUTES = 100
HOME_PAGE_SIZES = (20, 40, 80)

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


@app.get("/team-logo")
def team_logo(name: str = "", size: int = 64):
    size = max(32, min(int(size or 64), 256))
    label = team_abbr(name)
    color = team_color_from_name(name)
    safe_label = html.escape(label)
    svg = f"""<svg xmlns="http://www.w3.org/2000/svg" width="{size}" height="{size}" viewBox="0 0 64 64" role="img" aria-label="{safe_label} logo">
<rect width="64" height="64" rx="18" fill="{color}"/>
<text x="32" y="38" text-anchor="middle" font-family="Arial, sans-serif" font-size="16" font-weight="700" fill="#ffffff">{safe_label}</text>
</svg>"""
    return Response(content=svg, media_type="image/svg+xml")

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


def tidy_source_path(value: str) -> str:
    if not value:
        return ""
    text = str(value)
    try:
        path = Path(text)
    except Exception:
        return text
    for base in (DATA_DIR, ME_ROOT):
        try:
            return str(path.relative_to(base))
        except Exception:
            continue
    parts = path.parts
    if len(parts) > 4:
        return "/".join(parts[-4:])
    return text


def load_qa_report() -> dict:
    if not QA_REPORT_PATH.exists():
        return {"error": "QA report not found", "path": str(QA_REPORT_PATH)}
    try:
        return json.loads(QA_REPORT_PATH.read_text(encoding="utf-8"))
    except Exception:
        return {"error": "Failed to parse QA report", "path": str(QA_REPORT_PATH)}


def team_abbr(team: str) -> str:
    if not team:
        return "WK"
    ascii_only = "".join(ch for ch in str(team) if ch.isascii() and ch.isalnum())
    if ascii_only:
        return ascii_only[:6].upper()
    return str(team).strip()[:4]


def team_color_from_name(team: str) -> str:
    if not team:
        return DEFAULT_TEAM_META["color"]
    digest = hashlib.md5(str(team).encode("utf-8")).hexdigest()
    idx = int(digest[:2], 16) % len(TEAM_COLOR_POOL)
    return TEAM_COLOR_POOL[idx]


def team_logo_url(team: str) -> str:
    if not team:
        return ""
    return f"/team-logo?name={quote(str(team), safe='')}"


def get_team_meta(team: str) -> dict:
    meta = TEAM_META.get(team)
    if meta:
        return meta
    if not team:
        return DEFAULT_TEAM_META
    return {
        "nick": team_abbr(team),
        "logo": team_logo_url(team),
        "color": team_color_from_name(team),
    }

def normalize_team_name(team: str) -> str:
    text = str(team).strip()
    return TEAM_ALIAS.get(text, text)

def resolve_pager_accent(team: str) -> str | None:
    if not team or team == "ALL":
        return None
    color = get_team_meta(team).get("color")
    return color or None


def resolve_min_minutes(scope: str, value: int) -> int:
    min_minutes = max(c_i(value), 0)
    if min_minutes == 0 and scope == "per_36":
        return DEFAULT_PER36_MIN_MINUTES
    return min_minutes


def filter_min_minutes(rows: list[dict], min_minutes: int) -> list[dict]:
    if min_minutes <= 0:
        return rows
    filtered = []
    for row in rows:
        minutes = float(row.get("min_total") or 0)
        if minutes >= min_minutes:
            filtered.append(row)
    return filtered


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


def resolve_photo_pno_url(pno: str) -> str | None:
    if not pno:
        return None
    pno = str(pno).strip()
    if not pno or pno in PHOTO_BLACKLIST:
        return None
    local = STATIC_DIR / "images" / f"player_{pno}.png"
    if local.exists():
        return f"/static/images/player_{pno}.png"
    return f"{WKBL_BASE_URL}/static/images/player/pimg/m_{pno}.jpg?ver=0.3"


def resolve_photo_override(value) -> str | None:
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if text.isdigit():
        return resolve_photo_pno_url(text)
    if text.startswith("http://") or text.startswith("https://") or text.startswith("/"):
        return text
    return None


def resolve_roster_team(name: str) -> str:
    if not PLAYER_DB:
        return ""
    roster_team = PLAYER_DB.get(name, "")
    return normalize_team_name(roster_team) if roster_team else ""


def allow_name_only_photo(name: str, normalized_team: str) -> bool:
    if not normalized_team:
        return True
    roster_team = resolve_roster_team(name)
    if not roster_team:
        return True
    return roster_team == normalized_team


def resolve_photo_pno(name: str, normalized_team: str) -> str | None:
    if normalized_team:
        pno = PHOTO_BY_NAME_TEAM.get(f"{name}|{normalized_team}")
        if pno:
            return str(pno)
    if allow_name_only_photo(name, normalized_team):
        pno = PHOTO_BY_NAME.get(name)
        if pno:
            return str(pno)
    return str(PLAYER_PNO.get(name)) if PLAYER_PNO.get(name) else None


def get_player_photo(name: str, team: str = "") -> str | None:
    if not name:
        return None
    normalized_team = normalize_team_name(team) if team else ""
    if normalized_team:
        override = PHOTO_OVERRIDE_BY_NAME_TEAM.get(f"{name}|{normalized_team}")
        override_url = resolve_photo_override(override)
        if override_url:
            return override_url
    override = PHOTO_OVERRIDE_BY_NAME.get(name)
    override_url = resolve_photo_override(override)
    if override_url:
        return override_url
    pno = resolve_photo_pno(name, normalized_team)
    return resolve_photo_pno_url(pno)


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

def clamp_page_size(value: int, choices: tuple[int, ...]) -> int:
    if not choices:
        return value
    if value in choices:
        return value
    if DEFAULT_PAGE_SIZE in choices:
        return DEFAULT_PAGE_SIZE
    return choices[0]


def resolve_page_size(request: Request, page_size: int) -> int:
    if "page_size" in request.query_params:
        return normalize_page_size(page_size)
    cookie_value = request.cookies.get("wkbl_page_size")
    if cookie_value:
        return normalize_page_size(cookie_value)
    return normalize_page_size(page_size)


def normalize_pager_mode(value: str | None) -> str:
    mode = str(value or "").lower()
    if mode in {"top", "bottom", "both", "none"}:
        return mode
    return DEFAULT_PAGER_MODE


def resolve_pager_mode(request: Request, pager: str) -> str:
    if "pager" in request.query_params:
        return normalize_pager_mode(pager)
    cookie_value = request.cookies.get("wkbl_pager")
    if cookie_value:
        return normalize_pager_mode(cookie_value)
    return normalize_pager_mode(pager)


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
    page_param: str = "page",
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
        "page_param": page_param,
    }
    if total_count and page > 1:
        prev_params = params.copy()
        prev_params.update({page_param: page - 1, "page_size": page_size})
        pagination["prev"] = {
            "href": build_page_url(base_path, prev_params),
            "hx": build_page_url(table_path, prev_params),
        }
        first_params = params.copy()
        first_params.update({page_param: 1, "page_size": page_size})
        pagination["first"] = {
            "href": build_page_url(base_path, first_params),
            "hx": build_page_url(table_path, first_params),
        }
    if total_count and page < total_pages:
        next_params = params.copy()
        next_params.update({page_param: page + 1, "page_size": page_size})
        pagination["next"] = {
            "href": build_page_url(base_path, next_params),
            "hx": build_page_url(table_path, next_params),
        }
        last_params = params.copy()
        last_params.update({page_param: total_pages, "page_size": page_size})
        pagination["last"] = {
            "href": build_page_url(base_path, last_params),
            "hx": build_page_url(table_path, last_params),
        }
    pagination["pages"] = build_page_buttons(
        base_path=base_path,
        table_path=table_path,
        params=params,
        page=page,
        page_size=page_size,
        total_pages=total_pages,
        page_param=page_param,
    )
    return items[start:end], pagination


def build_page_buttons(
    base_path: str,
    table_path: str,
    params: dict,
    page: int,
    page_size: int,
    total_pages: int,
    page_param: str = "page",
    window: int = 3,
) -> list[dict]:
    if total_pages <= 1:
        return []
    if total_pages <= 7:
        numbers: list[int | None] = list(range(1, total_pages + 1))
    else:
        start = max(2, page - window)
        end = min(total_pages - 1, page + window)
        numbers = [1]
        if start > 2:
            numbers.append(None)
        numbers.extend(range(start, end + 1))
        if end < total_pages - 1:
            numbers.append(None)
        numbers.append(total_pages)
    pages: list[dict] = []
    for number in numbers:
        if number is None:
            pages.append({"ellipsis": True})
            continue
        page_params = params.copy()
        page_params.update({page_param: number, "page_size": page_size})
        pages.append(
            {
                "num": number,
                "is_current": number == page,
                "href": build_page_url(base_path, page_params),
                "hx": build_page_url(table_path, page_params),
            }
        )
    return pages


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
        if key == "per_36":
            min_total = float(record.get("min_total") or 0)
            if min_total < DEFAULT_PER36_MIN_MINUTES:
                continue
        record["scope_key"] = key
        record["scope_label"] = scope_label(key)
        scopes[key] = record
    return scopes


def load_player_game_log(name: str, team: str, season: str) -> list[dict]:
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
    return df.to_dict(orient="records")


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


def paginate_boxscore_teams(
    teams: list[dict],
    page_a: int,
    page_b: int,
    page_size: int,
    pager_mode: str,
    game_key: str,
) -> list[dict]:
    if not teams:
        return teams
    page_params = ["page_a", "page_b"]
    page_values = [page_a, page_b]
    params = {"game_key": game_key, "pager": pager_mode, "page_a": page_a, "page_b": page_b}
    for idx, team in enumerate(teams):
        page_param = page_params[idx] if idx < len(page_params) else page_params[-1]
        page_value = page_values[idx] if idx < len(page_values) else page_values[-1]
        rows = team.get("rows") or []
        sliced, pagination = paginate_items(
            rows,
            page_value,
            page_size,
            base_path="/boxscore",
            table_path="/boxscore/table",
            params=params,
            page_param=page_param,
        )
        other_param = None
        other_page = None
        if len(page_params) > 1:
            other_idx = 1 if idx == 0 else 0
            other_param = page_params[other_idx] if other_idx < len(page_params) else None
            other_page = page_values[other_idx] if other_idx < len(page_values) else None
        pagination["other_param"] = other_param
        pagination["other_page"] = other_page
        team["rows"] = sliced
        team["pagination"] = pagination
    return teams


def get_player_options(season: str = "ALL", min_minutes: int = 0) -> list[dict]:
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    if season and season != "ALL":
        df = df[df["season_gu"].astype(str).str.zfill(3) == str(season)]
    df = df[df["scope"] == "totals"].sort_values(["team", "name"])
    if min_minutes > 0:
        df["min_total"] = pd.to_numeric(df["min_total"], errors="coerce").fillna(0)
        df = df[df["min_total"] >= min_minutes]
    options = []
    for _, row in df.iterrows():
        name = str(row["name"]).strip()
        team = str(row["team"]).strip()
        season_val = str(row.get("season_gu", "")).zfill(3)
        value = f"{name}|{team}|{season_val}"
        label = f"{name} ({team}) · {season_val}"
        options.append({"value": value, "label": label})
    return options


def find_player(scope: str, key: str, season: str = "", min_minutes: int = 0) -> dict | None:
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
    if scope == "per_36" and min_minutes > 0:
        df["min_total"] = pd.to_numeric(df["min_total"], errors="coerce").fillna(0)
        df = df[df["min_total"] >= min_minutes]
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


def build_compare_metrics(left: dict, right: dict, mode: str = "players", scope: str = "per_game") -> list[dict]:
    scope = scope if scope in ("per_game", "per_36", "totals") else "per_game"
    rate_suffix = ""
    if scope == "per_game":
        rate_suffix = "/G"
    elif scope == "per_36":
        rate_suffix = "/36"
    rate_keys = {"pts", "reb", "ast", "stl", "blk", "to"}
    def label_for(label: str, key: str) -> str:
        if key == "min_total":
            return f"MIN{rate_suffix}" if scope == "per_game" else "MIN"
        if key == "margin":
            return f"{label}{rate_suffix}" if scope == "per_game" else label
        if key in rate_keys and rate_suffix:
            return f"{label}{rate_suffix}"
        return label

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
        label = label_for(label, key)
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
        min_minutes = DEFAULT_PER36_MIN_MINUTES if scope == "per_36" else 0
        options = get_player_options(season, min_minutes=min_minutes)
        left = find_player(scope, player_a, season, min_minutes=min_minutes)
        right = find_player(scope, player_b, season, min_minutes=min_minutes)
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

    metrics = build_compare_metrics(left, right, mode, scope) if left and right else []

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

def build_home_stats_from_aggregate(rows: list[dict], season: str) -> list[dict]:
    stats = []
    game_label = season_label(season) or "Per Game"
    for row in rows:
        name = str(row.get("name", "")).strip()
        team = str(row.get("team", "")).strip()
        pos = str(row.get("pos", "")).strip()
        if pos == "nan":
            pos = ""
        meta = get_team_meta(team)
        eff = float(row.get("eff") or 0)
        eff_bar = min(int(abs(eff) / 40 * 100), 100)
        min_per_game = float(row.get("min_per_game") or row.get("min_total") or 0)
        stats.append(
            {
                "game": game_label,
                "name": name,
                "team": team,
                "nick": meta["nick"],
                "logo": meta["logo"],
                "color": meta["color"],
                "photo": get_player_photo(name, team),
                "pos": pos,
                "min": min_per_game,
                "min_display": f"{min_per_game:.1f}",
                "pts": round(float(row.get("pts") or 0), 1),
                "reb": round(float(row.get("reb") or 0), 1),
                "ast": round(float(row.get("ast") or 0), 1),
                "stl": round(float(row.get("stl") or 0), 1),
                "blk": round(float(row.get("blk") or 0), 1),
                "to": round(float(row.get("to") or 0), 1),
                "fg_pct": round(float(row.get("fg_pct") or 0), 1),
                "eff": round(eff, 1),
                "eff_pos": eff_bar if eff > 0 else 0,
                "eff_neg": eff_bar if eff < 0 else 0,
            }
        )
    return stats


def load_real_stats(
    sort_by="eff",
    team_filter="ALL",
    pos_filter="ALL",
    search_query="",
    season: str = "",
    min_games: int = 0,
):
    sort_aliases = {
        "points": "pts",
        "point": "pts",
        "minutes": "min",
        "minute": "min",
        "min": "min",
        "turnovers": "to",
    }
    raw_sort_key = sort_aliases.get(sort_by, sort_by)
    derived_sort_key = {
        **sort_aliases,
        "minutes": "min_total",
        "minute": "min_total",
        "min": "min_total",
    }.get(sort_by, sort_by)

    derived_path = DERIVED_DIR / "players_aggregate.csv"
    if derived_path.exists():
        season = resolve_season(season, allow_all=False)
        rows = load_players_aggregate(
            season=season,
            scope="per_game",
            team_filter=team_filter,
            search_query=search_query,
            sort_by=derived_sort_key,
        )
        if min_games > 0:
            rows = [row for row in rows if c_i(row.get("gp", 0)) >= min_games]
        stats = build_home_stats_from_aggregate(rows, season)
        if pos_filter != "ALL":
            stats = [s for s in stats if s["pos"] == pos_filter]
        label = f"{season_label(season)} · Per Game · Aggregate"
        return stats, label

    stats = []
    data_dir = BOX_DIR if BOX_DIR.exists() else DATA_DIR
    if not data_dir.exists():
        return [], "No data"
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

                    stats.append(
                        {
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
                        }
                    )
                except Exception:
                    continue
        except Exception:
            continue

    # 필터링
    if team_filter != "ALL":
        stats = [s for s in stats if s["team"] == team_filter]
    if pos_filter != "ALL":
        stats = [s for s in stats if s["pos"] == pos_filter]
    if search_query:
        stats = [s for s in stats if search_query.lower() in s["name"].lower()]

    # 정렬
    stats.sort(key=lambda x: x.get(raw_sort_key, 0), reverse=True)
    return stats, "Boxscore rows · Single-game"

@app.get("/test")
async def test():
    return HTMLResponse("<h1>OK</h1>")

@app.get("/", response_class=HTMLResponse)
async def index(
    request: Request,
    season: str = "",
    min_games: int = 10,
    sort: str = "eff",
    team: str = "ALL",
    pos: str = "ALL",
    search: str = "",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    page_size = resolve_page_size(request, page_size)
    page_size = clamp_page_size(page_size, HOME_PAGE_SIZES)
    pager_mode = "both"
    pager_top = True
    pager_bottom = True
    pager_accent = resolve_pager_accent(team)
    season = resolve_season(season, allow_all=False)
    seasons = season_options(include_all=False)
    full_stats, data_label = load_real_stats(
        sort_by=sort,
        team_filter=team,
        pos_filter=pos,
        search_query=search,
        season=season,
        min_games=min_games,
    )
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/",
        table_path="/refresh",
        params={
            "season": season,
            "min_games": min_games,
            "sort": sort,
            "team": team,
            "pos": pos,
            "search": search,
            "pager": pager_mode,
        },
    )
    response = templates.TemplateResponse(
        "index.html",
        {
            "request": request,
            "active_page": "home",
            "page_title": "WKBL Moneyball Lab",
            "stats": stats,
            "total_count": pagination["total_count"],
            "season": season,
            "seasons": seasons,
            "season_label": season_label(season),
            "min_games": min_games,
            "sort": sort,
            "team": team,
            "pos": pos,
            "search": search,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "home_page_sizes": HOME_PAGE_SIZES,
            "data_label": data_label,
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    return response

@app.get("/refresh", response_class=HTMLResponse)
async def refresh(
    request: Request,
    season: str = "",
    min_games: int = 10,
    sort: str = "eff",
    team: str = "ALL",
    pos: str = "ALL",
    search: str = "",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    page_size = resolve_page_size(request, page_size)
    page_size = clamp_page_size(page_size, HOME_PAGE_SIZES)
    pager_mode = "both"
    pager_top = True
    pager_bottom = True
    pager_accent = resolve_pager_accent(team)
    season = resolve_season(season, allow_all=False)
    full_stats, data_label = load_real_stats(
        sort_by=sort,
        team_filter=team,
        pos_filter=pos,
        search_query=search,
        season=season,
        min_games=min_games,
    )
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/",
        table_path="/refresh",
        params={
            "season": season,
            "min_games": min_games,
            "sort": sort,
            "team": team,
            "pos": pos,
            "search": search,
            "pager": pager_mode,
        },
    )
    response = templates.TemplateResponse(
        "partials/stats_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "min_games": min_games,
            "sort": sort,
            "team": team,
            "pos": pos,
            "search": search,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "data_label": data_label,
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    return response


@app.get("/players", response_class=HTMLResponse)
async def players(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    team: str = "ALL",
    search: str = "",
    sort: str = "pts",
    min_minutes: int = 0,
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    scopes = [("per_game", "Per Game"), ("per_36", "Per 36"), ("totals", "Totals")]
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
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
    min_minutes = resolve_min_minutes(scope, min_minutes)
    params = {
        "season": season,
        "scope": scope,
        "team": team,
        "search": search,
        "sort": sort,
        "min_minutes": min_minutes,
        "pager": pager_mode,
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
    full_stats = filter_min_minutes(full_stats, min_minutes)
    leader = full_stats[0] if full_stats else None
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/players",
        table_path="/players/table",
        params=params,
    )
    response = templates.TemplateResponse(
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
            "min_minutes": min_minutes,
            "scopes": scopes,
            "sort_options": sort_options,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/players/table", response_class=HTMLResponse)
async def players_table(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    team: str = "ALL",
    search: str = "",
    sort: str = "pts",
    min_minutes: int = 0,
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season, allow_all=True)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
    min_minutes = resolve_min_minutes(scope, min_minutes)
    params = {
        "season": season,
        "scope": scope,
        "team": team,
        "search": search,
        "sort": sort,
        "min_minutes": min_minutes,
        "pager": pager_mode,
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
    full_stats = filter_min_minutes(full_stats, min_minutes)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/players",
        table_path="/players/table",
        params=params,
    )
    response = templates.TemplateResponse(
        "partials/players_table.html",
        {
            "request": request,
            "stats": stats,
            "scope": scope,
            "season": season,
            "team": team,
            "search": search,
            "sort": sort,
            "min_minutes": min_minutes,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/player", response_class=HTMLResponse)
async def player_profile(
    request: Request,
    name: str = "",
    team: str = "",
    season: str = "",
    scope: str = "per_game",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season)
    scopes = load_player_scopes(name, team, season)
    primary = scopes.get(scope) or scopes.get("per_game") or next(iter(scopes.values()), None)
    scope_rows = [scopes[key] for key in ("per_game", "per_36", "totals") if key in scopes]
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    full_games = load_player_game_log(name, team, season)
    games, pagination = paginate_items(
        full_games,
        page,
        page_size,
        base_path="/player",
        table_path="/player/games/table",
        params={"name": name, "team": team, "season": season, "scope": scope, "pager": pager_mode},
    )
    team_meta = get_team_meta(team)
    pager_accent = team_meta.get("color")
    response = templates.TemplateResponse(
        "player.html",
        {
            "request": request,
            "active_page": "players",
            "page_title": f"{name} | WKBL Player",
            "player": primary,
            "player_photo": get_player_photo(name, team),
            "team_meta": team_meta,
            "scope_rows": scope_rows,
            "games": games,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
            "season": season,
            "season_label": season_label(season),
            "compare_url": build_compare_player_url(name, team, season),
            "team_url": build_team_url(team, season),
            "back_url": f"/players?season={season}&team={quote(str(team), safe='')}" if team else f"/players?season={season}",
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/player/games/table", response_class=HTMLResponse)
async def player_games_table(
    request: Request,
    name: str = "",
    team: str = "",
    season: str = "",
    scope: str = "per_game",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
    full_games = load_player_game_log(name, team, season)
    games, pagination = paginate_items(
        full_games,
        page,
        page_size,
        base_path="/player",
        table_path="/player/games/table",
        params={"name": name, "team": team, "season": season, "scope": scope, "pager": pager_mode},
    )
    response = templates.TemplateResponse(
        "partials/player_games_table.html",
        {
            "request": request,
            "games": games,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
            "name": name,
            "team": team,
            "season": season,
            "scope": scope,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/teams", response_class=HTMLResponse)
async def teams(
    request: Request,
    season: str = "",
    scope: str = "per_game",
    sort: str = "pts",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
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
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = None
    full_stats = load_teams_aggregate(season=season, scope=scope, sort_by=sort)
    leader = full_stats[0] if full_stats else None
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/teams",
        table_path="/teams/table",
        params={"season": season, "scope": scope, "sort": sort, "pager": pager_mode},
    )
    response = templates.TemplateResponse(
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
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


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
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season, allow_all=True)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = None
    full_stats = load_teams_aggregate(season=season, scope=scope, sort_by=sort)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/teams",
        table_path="/teams/table",
        params={"season": season, "scope": scope, "sort": sort, "pager": pager_mode},
    )
    response = templates.TemplateResponse(
        "partials/teams_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "scope": scope,
            "sort": sort,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/standings", response_class=HTMLResponse)
async def standings(
    request: Request,
    season: str = "",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season)
    seasons = season_options(include_all=False)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = None
    full_stats = load_standings(season)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/standings",
        table_path="/standings/table",
        params={"season": season, "pager": pager_mode},
    )
    response = templates.TemplateResponse(
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
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/standings/table", response_class=HTMLResponse)
async def standings_table(
    request: Request,
    season: str = "",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = None
    full_stats = load_standings(season)
    stats, pagination = paginate_items(
        full_stats,
        page,
        page_size,
        base_path="/standings",
        table_path="/standings/table",
        params={"season": season, "pager": pager_mode},
    )
    response = templates.TemplateResponse(
        "partials/standings_table.html",
        {
            "request": request,
            "stats": stats,
            "season": season,
            "pagination": pagination,
            "page_size": pagination["page_size"],
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


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
    pager: str = DEFAULT_PAGER_MODE,
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
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
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
            "pager": pager_mode,
        },
    )
    response = templates.TemplateResponse(
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
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


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
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season, allow_all=True)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
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
            "pager": pager_mode,
        },
    )
    response = templates.TemplateResponse(
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
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/boxscores", response_class=HTMLResponse)
async def boxscores(
    request: Request,
    season: str = "",
    team: str = "ALL",
    search: str = "",
    sort: str = "game_no",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    sort_options = [
        ("game_no", "Game No"),
        ("margin", "Margin"),
        ("pts_a", "Score A"),
        ("pts_b", "Score B"),
    ]
    season = resolve_season(season, allow_all=True)
    seasons = season_options(include_all=True)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
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
            "pager": pager_mode,
        },
    )
    response = templates.TemplateResponse(
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
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/boxscores/table", response_class=HTMLResponse)
async def boxscores_table(
    request: Request,
    season: str = "",
    team: str = "ALL",
    search: str = "",
    sort: str = "game_no",
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    season = resolve_season(season, allow_all=True)
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    pager_accent = resolve_pager_accent(team)
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
            "pager": pager_mode,
        },
    )
    response = templates.TemplateResponse(
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
            "pager_accent": pager_accent,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(pagination["page_size"]), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/boxscore", response_class=HTMLResponse)
async def boxscore(
    request: Request,
    game_key: str = "",
    page_a: int = 1,
    page_b: int = 1,
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    if (
        "page_a" not in request.query_params
        and "page_b" not in request.query_params
        and "page" in request.query_params
    ):
        page_a = page
        page_b = page
    data = load_boxscore(game_key)
    teams = data.get("teams") or []
    teams = paginate_boxscore_teams(teams, page_a, page_b, page_size, pager_mode, game_key)
    response = templates.TemplateResponse(
        "boxscore.html",
        {
            "request": request,
            "active_page": "boxscores",
            "page_title": "WKBL Boxscore",
            **data,
            "teams": teams,
            "page_size": page_size,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
        },
    )
    response.set_cookie("wkbl_page_size", str(page_size), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/boxscore/table", response_class=HTMLResponse)
async def boxscore_table(
    request: Request,
    game_key: str = "",
    page_a: int = 1,
    page_b: int = 1,
    page: int = 1,
    page_size: int = DEFAULT_PAGE_SIZE,
    pager: str = DEFAULT_PAGER_MODE,
):
    page_size = resolve_page_size(request, page_size)
    pager_mode = resolve_pager_mode(request, pager)
    pager_top = pager_mode in ("top", "both")
    pager_bottom = pager_mode in ("bottom", "both")
    if (
        "page_a" not in request.query_params
        and "page_b" not in request.query_params
        and "page" in request.query_params
    ):
        page_a = page
        page_b = page
    data = load_boxscore(game_key)
    teams = data.get("teams") or []
    teams = paginate_boxscore_teams(teams, page_a, page_b, page_size, pager_mode, game_key)
    response = templates.TemplateResponse(
        "partials/boxscore_tables.html",
        {
            "request": request,
            "teams": teams,
            "summary": data.get("summary"),
            "page_size": page_size,
            "pager_mode": pager_mode,
            "pager_top": pager_top,
            "pager_bottom": pager_bottom,
            "game_key": game_key,
        },
    )
    response.set_cookie("wkbl_page_size", str(page_size), max_age=31536000, samesite="lax")
    response.set_cookie("wkbl_pager", pager_mode, max_age=31536000, samesite="lax")
    return response


@app.get("/qa", response_class=HTMLResponse)
async def qa_report(request: Request):
    report = load_qa_report()
    error = report.get("error") if isinstance(report, dict) else "QA report unavailable"
    coverage = report.get("coverage", {}) if not error else {}
    duplicates = report.get("duplicates", {}) if not error else {}
    outliers = report.get("outliers", {}) if not error else {}
    sources = report.get("sources", {}) if not error else {}

    summary = {
        "coverage_pct": coverage.get("coverage_pct", 0),
        "expected_games": coverage.get("expected_games", 0),
        "actual_games": coverage.get("actual_games", 0),
        "missing_games": coverage.get("missing_games", 0),
        "extra_games": coverage.get("extra_games", 0),
        "meta_available": coverage.get("meta_available", False),
    }

    coverage_by_season = []
    by_season = coverage.get("by_season", [])
    try:
        by_season = sorted(by_season, key=lambda r: int(r.get("season", 0)))
    except Exception:
        by_season = coverage.get("by_season", [])
    for row in by_season:
        try:
            season_code = str(row.get("season", ""))
            label = season_label(season_code)
        except Exception:
            label = str(row.get("season", ""))
        coverage_by_season.append({**row, "label": label})

    source_labels = {
        "meta_file": "Meta list",
        "players_games": "Players game log",
        "team_games": "Team game log",
    }
    source_items = []
    for key, path in sources.items():
        label = source_labels.get(key, key.replace("_", " ").title())
        source_items.append({"label": label, "path": tidy_source_path(path)})

    duplicates_team = duplicates.get("team_count_anomalies", []) if duplicates else []
    duplicates_players = duplicates.get("player_duplicates", []) if duplicates else []
    outliers_iqr = outliers.get("iqr", []) if outliers else []
    outliers_abs_pts = outliers.get("absolute_pts", []) if outliers else []
    outliers_abs_margin = outliers.get("absolute_margin", []) if outliers else []

    counts = {
        "duplicates_team": len(duplicates_team),
        "duplicates_players": len(duplicates_players),
        "outliers_iqr": len(outliers_iqr),
        "outliers_abs_pts": len(outliers_abs_pts),
        "outliers_abs_margin": len(outliers_abs_margin),
    }

    return templates.TemplateResponse(
        "qa.html",
        {
            "request": request,
            "active_page": "qa",
            "page_title": "WKBL Data QA",
            "error": error if error else None,
            "error_path": report.get("path") if error else None,
            "summary": summary,
            "generated_at": report.get("generated_at", ""),
            "sources": source_items,
            "coverage_by_season": coverage_by_season,
            "missing_sample": coverage.get("missing_sample", []),
            "extra_sample": coverage.get("extra_sample", []),
            "duplicates_team": duplicates_team,
            "duplicates_players": duplicates_players,
            "outliers_iqr": outliers_iqr,
            "outliers_abs_pts": outliers_abs_pts,
            "outliers_abs_margin": outliers_abs_margin,
            "counts": counts,
        },
    )


@app.get("/qa.json")
async def qa_report_json():
    report = load_qa_report()
    status_code = 200
    if isinstance(report, dict) and report.get("error"):
        status_code = 404
    return JSONResponse(report, status_code=status_code)


@app.get("/qa.md")
async def qa_report_markdown():
    if not QA_REPORT_MD_PATH.exists():
        return Response(content="QA report not found", status_code=404)
    return Response(
        content=QA_REPORT_MD_PATH.read_text(encoding="utf-8"),
        media_type="text/markdown; charset=utf-8",
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
