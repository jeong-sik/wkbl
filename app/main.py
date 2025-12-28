from fastapi import FastAPI, Request, Response
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates
from pathlib import Path
from urllib.parse import quote
import json
import os
import re
import mimetypes
import pandas as pd

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


def build_team_url(team: str) -> str:
    if not team:
        return ""
    return f"/team?name={quote(str(team), safe='')}"


def build_player_key(name: str, team: str) -> str:
    if not name or not team:
        return ""
    return f"{name}|{team}"


def build_player_url(name: str, team: str) -> str:
    if not name or not team:
        return ""
    return f"/player?name={quote(str(name), safe='')}&team={quote(str(team), safe='')}"


def build_compare_player_url(name: str, team: str) -> str:
    key = build_player_key(name, team)
    if not key:
        return ""
    return f"/compare?mode=players&player_a={quote(key, safe='')}"


def build_compare_team_url(team: str) -> str:
    if not team:
        return ""
    return f"/compare?mode=teams&team_a={quote(str(team), safe='')}"


def get_player_photo(name: str) -> str | None:
    pno = PLAYER_PNO.get(name)
    return f"/static/images/player_{pno}.png" if pno else None


def get_team_list() -> list[str]:
    if DERIVED_DIR.exists():
        path = DERIVED_DIR / "players_aggregate.csv"
        if path.exists():
            df = pd.read_csv(path)
            teams = sorted(df["team"].dropna().unique().tolist())
            return teams
    return sorted(["우리은행", "삼성생명", "신한은행", "KB스타즈", "하나은행", "BNK썸"])


def scope_label(scope: str) -> str:
    labels = {
        "totals": "Totals",
        "per_game": "Per Game",
        "per_36": "Per 36",
    }
    return labels.get(scope, "Per Game")


def load_players_aggregate(
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
        row["player_url"] = build_player_url(name, team)
        row["team_url"] = build_team_url(team)
        row["compare_url"] = build_compare_player_url(name, team)
    return rows


def load_teams_aggregate(
    scope: str = "per_game",
    sort_by: str = "pts",
    order: str = "desc",
) -> list[dict]:
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    df = df[df["scope"] == scope]

    allowed = {"pts", "reb", "ast", "stl", "blk", "to", "fg_pct", "fg3_pct", "ts_pct", "efg_pct", "eff", "min_total"}
    if sort_by not in allowed:
        sort_by = "pts"
    df = df.sort_values(by=sort_by, ascending=(order == "asc"))
    rows = df.to_dict(orient="records")
    for row in rows:
        team = str(row.get("team", "")).strip()
        row["team_url"] = build_team_url(team)
        row["compare_url"] = build_compare_team_url(team)
    return rows


def load_players_games(
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

    allowed = {"game_no", "pts", "reb", "ast", "eff", "ts_pct"}
    if sort_by not in allowed:
        sort_by = "game_no"
    df = df.sort_values(by=sort_by, ascending=(order == "asc"))
    return df.to_dict(orient="records")


def load_player_scopes(name: str, team: str) -> dict:
    if not name or not team:
        return {}
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return {}
    df = pd.read_csv(path)
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


def load_player_game_log(name: str, team: str, limit: int = 12) -> list[dict]:
    if not name or not team:
        return []
    path = DERIVED_DIR / "players_games.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    df = df[(df["name"] == name) & (df["team"] == team)]
    if df.empty:
        return []
    df = df.sort_values(by="game_no", ascending=False)
    df["game_label"] = df["game_no"].apply(lambda x: f"#{int(x)}")
    df["min_display"] = df["min"].fillna("0:00")
    return df.head(limit).to_dict(orient="records")


def load_team_scopes(team: str) -> dict:
    if not team:
        return {}
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return {}
    df = pd.read_csv(path)
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


def load_team_leaders(team: str) -> dict:
    players = load_players_aggregate(scope="per_game", team_filter=team, sort_by="pts")
    if not players:
        return {}
    leaders = {}
    for key in ("pts", "reb", "ast"):
        leaders[key] = max(players, key=lambda row: row.get(key, 0))
    return leaders


def get_player_options() -> list[dict]:
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    df = df[df["scope"] == "totals"].sort_values(["team", "name"])
    options = []
    for _, row in df.iterrows():
        name = str(row["name"]).strip()
        team = str(row["team"]).strip()
        value = f"{name}|{team}"
        label = f"{name} ({team})"
        options.append({"value": value, "label": label})
    return options


def find_player(scope: str, key: str) -> dict | None:
    if not key or "|" not in key:
        return None
    name, team = key.split("|", 1)
    path = DERIVED_DIR / "players_aggregate.csv"
    if not path.exists():
        return None
    df = pd.read_csv(path)
    df = df[(df["scope"] == scope) & (df["name"] == name) & (df["team"] == team)]
    if df.empty:
        return None
    return df.iloc[0].to_dict()


def get_team_options() -> list[dict]:
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return []
    df = pd.read_csv(path)
    teams = sorted(df["team"].dropna().unique().tolist())
    return [{"value": team, "label": team} for team in teams]


def find_team(scope: str, team: str) -> dict | None:
    if not team:
        return None
    path = DERIVED_DIR / "teams_aggregate.csv"
    if not path.exists():
        return None
    df = pd.read_csv(path)
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


def build_compare_metrics(left: dict, right: dict) -> list[dict]:
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
    player_a: str = "",
    player_b: str = "",
    team_a: str = "",
    team_b: str = "",
) -> dict:
    mode = "teams" if mode == "teams" else "players"
    player_scopes = [("per_game", "Per Game"), ("per_36", "Per 36"), ("totals", "Totals")]
    team_scopes = [("per_game", "Per Game"), ("totals", "Totals")]

    player_scope_keys = [value for value, _ in player_scopes]
    team_scope_keys = [value for value, _ in team_scopes]

    if mode == "teams" and scope not in team_scope_keys:
        scope = "per_game"
    if mode == "players" and scope not in player_scope_keys:
        scope = "per_game"

    if mode == "teams":
        options = get_team_options()
        left = find_team(scope, team_a)
        right = find_team(scope, team_b)
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
        options = get_player_options()
        left = find_player(scope, player_a)
        right = find_player(scope, player_b)
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

    metrics = build_compare_metrics(left, right) if left and right else []

    player_scope = scope if scope in player_scope_keys else "per_game"
    team_scope = scope if scope in team_scope_keys else "per_game"
    mode_switch = [
        {
            "label": "Players",
            "url": f"/compare?mode=players&scope={player_scope}",
            "active": mode == "players",
        },
        {
            "label": "Teams",
            "url": f"/compare?mode=teams&scope={team_scope}",
            "active": mode == "teams",
        },
    ]

    return {
        "mode": mode,
        "scope": scope,
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
    files = sorted(data_dir.glob("*.csv"))
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
    stats = load_real_stats()
    return templates.TemplateResponse(
        "index.html",
        {
            "request": request,
            "stats": stats,
            "active_page": "home",
            "page_title": "WKBL Moneyball Lab",
        },
    )

@app.get("/refresh", response_class=HTMLResponse)
async def refresh(request: Request, sort: str = "eff", team: str = "ALL", pos: str = "ALL", search: str = ""):
    stats = load_real_stats(sort_by=sort, team_filter=team, pos_filter=pos, search_query=search)
    return templates.TemplateResponse("partials/stats_table.html", {"request": request, "stats": stats})


@app.get("/players", response_class=HTMLResponse)
async def players(
    request: Request,
    scope: str = "per_game",
    team: str = "ALL",
    search: str = "",
    sort: str = "pts",
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
    stats = load_players_aggregate(scope=scope, team_filter=team, search_query=search, sort_by=sort)
    leader = stats[0] if stats else None
    return templates.TemplateResponse(
        "players.html",
        {
            "request": request,
            "active_page": "players",
            "page_title": "WKBL Players",
            "stats": stats,
            "leader": leader,
            "total_count": len(stats),
            "scope": scope,
            "scope_label": scope_label(scope),
            "team": team,
            "teams": get_team_list(),
            "search": search,
            "sort": sort,
            "scopes": scopes,
            "sort_options": sort_options,
        },
    )


@app.get("/players/table", response_class=HTMLResponse)
async def players_table(
    request: Request,
    scope: str = "per_game",
    team: str = "ALL",
    search: str = "",
    sort: str = "pts",
):
    stats = load_players_aggregate(scope=scope, team_filter=team, search_query=search, sort_by=sort)
    return templates.TemplateResponse(
        "partials/players_table.html",
        {"request": request, "stats": stats, "scope": scope},
    )


@app.get("/player", response_class=HTMLResponse)
async def player_profile(
    request: Request,
    name: str = "",
    team: str = "",
    scope: str = "per_game",
):
    scopes = load_player_scopes(name, team)
    primary = scopes.get(scope) or scopes.get("per_game") or next(iter(scopes.values()), None)
    scope_rows = [scopes[key] for key in ("per_game", "per_36", "totals") if key in scopes]
    games = load_player_game_log(name, team)
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
            "compare_url": build_compare_player_url(name, team),
            "team_url": build_team_url(team),
            "back_url": f"/players?team={quote(str(team), safe='')}" if team else "/players",
        },
    )


@app.get("/teams", response_class=HTMLResponse)
async def teams(
    request: Request,
    scope: str = "per_game",
    sort: str = "pts",
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
    stats = load_teams_aggregate(scope=scope, sort_by=sort)
    leader = stats[0] if stats else None
    return templates.TemplateResponse(
        "teams.html",
        {
            "request": request,
            "active_page": "teams",
            "page_title": "WKBL Teams",
            "stats": stats,
            "leader": leader,
            "total_count": len(stats),
            "scope": scope,
            "scope_label": scope_label(scope),
            "sort": sort,
            "scopes": scopes,
            "sort_options": sort_options,
        },
    )


@app.get("/team", response_class=HTMLResponse)
async def team_profile(
    request: Request,
    name: str = "",
    scope: str = "per_game",
):
    scopes = load_team_scopes(name)
    primary = scopes.get(scope) or scopes.get("per_game") or next(iter(scopes.values()), None)
    scope_rows = [scopes[key] for key in ("per_game", "totals") if key in scopes]
    top_players = load_players_aggregate(scope="per_game", team_filter=name, sort_by="pts")[:8]
    leaders = load_team_leaders(name) if name else {}
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
            "compare_url": build_compare_team_url(name),
            "back_url": "/teams",
        },
    )


@app.get("/teams/table", response_class=HTMLResponse)
async def teams_table(
    request: Request,
    scope: str = "per_game",
    sort: str = "pts",
):
    stats = load_teams_aggregate(scope=scope, sort_by=sort)
    return templates.TemplateResponse(
        "partials/teams_table.html",
        {"request": request, "stats": stats},
    )


@app.get("/games", response_class=HTMLResponse)
async def games(
    request: Request,
    team: str = "ALL",
    search: str = "",
    game_no: str = "",
    sort: str = "game_no",
):
    sort_options = [
        ("game_no", "Game No"),
        ("pts", "PTS"),
        ("reb", "REB"),
        ("ast", "AST"),
        ("eff", "EFF"),
        ("ts_pct", "TS%"),
    ]
    stats = load_players_games(team_filter=team, search_query=search, game_no=game_no, sort_by=sort)
    leader = stats[0] if stats else None
    return templates.TemplateResponse(
        "games.html",
        {
            "request": request,
            "active_page": "games",
            "page_title": "WKBL Game Log",
            "stats": stats,
            "leader": leader,
            "total_count": len(stats),
            "teams": get_team_list(),
            "team": team,
            "search": search,
            "game_no": game_no,
            "sort": sort,
            "sort_options": sort_options,
        },
    )


@app.get("/games/table", response_class=HTMLResponse)
async def games_table(
    request: Request,
    team: str = "ALL",
    search: str = "",
    game_no: str = "",
    sort: str = "game_no",
):
    stats = load_players_games(team_filter=team, search_query=search, game_no=game_no, sort_by=sort)
    return templates.TemplateResponse(
        "partials/games_table.html",
        {"request": request, "stats": stats},
    )


@app.get("/compare", response_class=HTMLResponse)
async def compare(
    request: Request,
    mode: str = "players",
    scope: str = "per_game",
    player_a: str = "",
    player_b: str = "",
    team_a: str = "",
    team_b: str = "",
):
    context = compare_context(
        mode=mode,
        scope=scope,
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
    scope: str = "per_game",
    player_a: str = "",
    player_b: str = "",
    team_a: str = "",
    team_b: str = "",
):
    context = compare_context(
        mode=mode,
        scope=scope,
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
