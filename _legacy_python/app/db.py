import sqlite3
from contextlib import contextmanager
from pathlib import Path

ROOT_DIR = Path(__file__).resolve().parents[1]
DB_FILE = ROOT_DIR / "data" / "wkbl.db"

@contextmanager
def get_db():
    conn = sqlite3.connect(DB_FILE)
    conn.row_factory = sqlite3.Row  # 컬럼명으로 접근 가능하게
    try:
        yield conn
    finally:
        conn.close()

def query_players_aggregate(scope="per_game", team_filter="ALL", search_query="", sort_by="pts", order="desc", limit=100):
    # scope에 따라 집계 방식 변경 (AVG vs SUM)
    agg_func = "AVG" if scope in ["per_game", "per_36"] else "SUM"
    
    # 정렬 컬럼 매핑
    sort_map = {
        "pts": f"{agg_func}(s.pts)",
        "reb": f"{agg_func}(s.reb_tot)",
        "ast": f"{agg_func}(s.ast)",
        "eff": f"{agg_func}(s.game_score)", # game_score를 EFF 대용으로 사용
        "ts_pct": "AVG(s.ts_pct)", # 비율은 항상 AVG
        "min_total": "SUM(s.min_seconds)"
    }
    order_col = sort_map.get(sort_by, sort_map["pts"])
    
    where_clause = "WHERE 1=1"
    params = []
    
    if team_filter != "ALL":
        where_clause += " AND t.team_name_kr = ?"
        params.append(team_filter)
        
    if search_query:
        where_clause += " AND p.player_name LIKE ?"
        params.append(f"%{search_query}%")

    sql = f"""
        SELECT 
            p.player_name as name,
            t.team_name_kr as team,
            COUNT(*) as gp,
            SUM(s.min_seconds) / 60.0 as min_total,
            AVG(s.min_seconds) / 60.0 as min_avg,
            {agg_func}(s.pts) as pts,
            {agg_func}(s.reb_tot) as reb,
            {agg_func}(s.ast) as ast,
            {agg_func}(s.stl) as stl,
            {agg_func}(s.blk) as blk,
            {agg_func}(s.tov) as 'to',
            {agg_func}(s.game_score) as eff,
            AVG(s.ts_pct) as ts_pct,
            AVG(s.efg_pct) as efg_pct
        FROM game_stats s
        JOIN players p ON s.player_id = p.player_id
        JOIN teams t ON s.team_code = t.team_code
        {where_clause}
        GROUP BY p.player_id
        ORDER BY {order_col} {order.upper()}
        LIMIT ?
    """
    params.append(limit)
    
    with get_db() as conn:
        cursor = conn.cursor()
        cursor.execute(sql, params)
        return [dict(row) for row in cursor.fetchall()]

def get_all_teams():
    with get_db() as conn:
        cursor = conn.cursor()
        cursor.execute("SELECT team_name_kr FROM teams ORDER BY team_name_kr")
        return [row['team_name_kr'] for row in cursor.fetchall()]
