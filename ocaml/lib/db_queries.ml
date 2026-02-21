(** SQL query definitions.

    Extracted from db.ml module Queries.
    All queries use oneshot mode via Db_request for PgBouncer compatibility.
*)

open Db_request
open Db_types
open Caqti_type

let ensure_legend_players_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS legend_players (
    player_name TEXT PRIMARY KEY,
    career_years TEXT NOT NULL,
    teams TEXT NOT NULL,
    championships INTEGER NOT NULL DEFAULT 0,
    mvp_count INTEGER NOT NULL DEFAULT 0,
    all_star_count INTEGER NOT NULL DEFAULT 0,
    career_points INTEGER NOT NULL DEFAULT 0,
    career_rebounds INTEGER NOT NULL DEFAULT 0,
    career_assists INTEGER NOT NULL DEFAULT 0
  )
|}

let seed_legend_players = (unit ->. unit) {|
  INSERT INTO legend_players (player_name, career_years, teams, championships, mvp_count, all_star_count, career_points, career_rebounds, career_assists)
  VALUES
  ('정선민', '1998-2012', '신세계, KB스타즈, 신한은행', 9, 7, 12, 8140, 3142, 1777),
  ('박정은', '1998-2013', '삼성생명', 5, 0, 11, 6540, 2664, 1776),
  ('변연하', '1999-2016', '삼성생명, KB스타즈', 5, 3, 13, 7863, 2227, 2407),
  ('이미선', '1998-2016', '삼성생명', 6, 0, 12, 5323, 2664, 2264),
  ('신정자', '1999-2016', '국민은행, 금호생명, 신한은행', 6, 1, 11, 5970, 4502, 1753),
  ('김지윤', '1998-2013', '국민은행, 금호생명, 신세계, 하나외환', 0, 1, 12, 7020, 1867, 2733),
  ('전주원', '1998-2011', '현대, 신한은행', 7, 1, 10, 4325, 1546, 2164),
  ('김단비', '2007-Present', '신한은행, 우리은행', 6, 1, 13, 7000, 3000, 2500),
  ('박혜진', '2008-Present', '우리은행', 8, 5, 10, 5000, 2000, 1500),
  ('강이슬', '2012-Present', '하나은행, KB스타즈', 1, 0, 8, 4500, 1500, 800)
  ON CONFLICT (player_name) DO NOTHING
|}

(* Seed historical/defunct teams and All-Star/special event teams so FK updates can use them. *)
let seed_historical_teams = (unit ->. unit) {|
  INSERT INTO teams (team_code, team_name_kr, team_name_en)
  VALUES
    ('02', '금호생명', 'Kumho Life'),
    ('04', '신세계', 'Shinsegae'),
    ('06', '현대', 'Hyundai'),
    ('12', 'LG', 'LG'),
    ('13', '한화', 'Hanwha'),
    ('81', '팀 유니블', 'Team Unibble'),
    ('82', '팀 포니블', 'Team Ponibble'),
    ('83', '한국 올스타', 'Korea All-Star'),
    ('84', '일본 올스타', 'Japan All-Star'),
    ('87', '핑크스타', 'Pink Star'),
    ('88', '블루스타', 'Blue Star'),
    ('91', '남부선발', 'South Select'),
    ('92', '중부선발', 'Central Select')
  ON CONFLICT (team_code) DO NOTHING
|}

let ensure_player_plus_minus_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS player_plus_minus (
    game_id TEXT NOT NULL,
    player_id TEXT NOT NULL,
    plus_minus INTEGER NOT NULL,
    PRIMARY KEY (game_id, player_id)
  )
|}
let ensure_player_plus_minus_index = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_player_plus_minus_player_id ON player_plus_minus(player_id)
|}
let ensure_play_by_play_events_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS play_by_play_events (
    id SERIAL PRIMARY KEY,
    game_id TEXT NOT NULL,
    period_code TEXT NOT NULL,
    event_index INTEGER NOT NULL,
    team_side INTEGER NOT NULL,
    description TEXT NOT NULL,
    team1_score INTEGER,
    team2_score INTEGER,
    clock TEXT NOT NULL,
    player_id TEXT,
    UNIQUE (game_id, period_code, event_index)
  )
|}

(* Migration: Add player_id column if missing (for existing databases) *)
let ensure_play_by_play_events_player_id_column = (unit ->. unit) {|
  ALTER TABLE play_by_play_events ADD COLUMN IF NOT EXISTS player_id TEXT
|}
let ensure_play_by_play_events_index_game = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_pbp_game_id ON play_by_play_events(game_id)
|}
let ensure_play_by_play_events_index_game_period = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_pbp_game_period ON play_by_play_events(game_id, period_code)
|}
let ensure_play_by_play_events_index_player = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_pbp_player_id ON play_by_play_events(player_id) WHERE player_id IS NOT NULL
|}
let ensure_player_drafts_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS player_drafts (
    player_id TEXT PRIMARY KEY,
    draft_year INTEGER,
    draft_round INTEGER,
    pick_in_round INTEGER,
    overall_pick INTEGER,
    draft_team TEXT,
    raw_text TEXT NOT NULL,
    source_url TEXT NOT NULL,
    scraped_at TEXT NOT NULL
  )
|}
let ensure_player_drafts_index_year = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_player_drafts_year ON player_drafts(draft_year)
|}
let ensure_official_trade_events_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS official_trade_events (
    id SERIAL PRIMARY KEY,
    event_date TEXT NOT NULL,
    event_year INTEGER NOT NULL,
    event_text TEXT NOT NULL,
    event_text_norm TEXT NOT NULL,
    source_url TEXT NOT NULL,
    scraped_at TEXT NOT NULL,
    UNIQUE(event_date, event_text)
  )
|}
let ensure_official_trade_events_index_date = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_trade_events_date ON official_trade_events(event_date)
|}
let ensure_official_trade_events_index_year = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_trade_events_year ON official_trade_events(event_year)
|}
let ensure_player_external_links_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS player_external_links (
    player_id TEXT NOT NULL,
    link_type TEXT NOT NULL,
    url TEXT NOT NULL,
    source_url TEXT,
    scraped_at TEXT NOT NULL,
    PRIMARY KEY (player_id, link_type)
  )
|}
let ensure_player_external_links_index_player = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_player_external_links_player_id ON player_external_links(player_id)
|}
let ensure_game_stats_index_game = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_game_stats_game_id ON game_stats(game_id)
|}
let ensure_game_stats_index_player = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_game_stats_player_id ON game_stats(player_id)
|}
let ensure_game_stats_index_team = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_game_stats_team_code ON game_stats(team_code)
|}
let ensure_game_stats_index_game_team = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_game_stats_game_team ON game_stats(game_id, team_code)
|}
let ensure_games_index_season_type = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_games_season_type ON games (season_code, game_type)
|}

let ensure_games_index_date = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_games_date ON games (game_date)
|}

let ensure_legend_players_index_name = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_legend_players_name ON legend_players (player_name)
|}

let ensure_games_index_season_date = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_games_season_date ON games(season_code, game_date)
|}

(* Player identity normalization.
   Some players appear in the official WKBL dataset under multiple player_id values.
   We treat rows with the same (player_name, birth_date) as the same person and map them to a
   single canonical_player_id (min player_id for stability). *)
let ensure_player_identities_view = (unit ->. unit) {|
  CREATE OR REPLACE VIEW player_identities AS
  WITH groups AS (
    SELECT
      player_name,
      birth_date,
      MIN(player_id) AS canonical_player_id
    FROM players
    WHERE birth_date IS NOT NULL
    GROUP BY player_name, birth_date
  )
  SELECT
    p.player_id,
    COALESCE(g.canonical_player_id, p.player_id) AS canonical_player_id
  FROM players p
  LEFT JOIN groups g
    ON g.player_name = p.player_name
   AND g.birth_date = p.birth_date
|}

(* Data quality overrides.
   WKBL official data sometimes contains obvious wrong rows (e.g., a star player listed under the wrong team for 0:38).
   We keep the raw row in game_stats, but exclude it from user-facing aggregates via a clean view. *)
let ensure_game_stats_exclusions_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS game_stats_exclusions (
    game_id TEXT NOT NULL REFERENCES games(game_id) ON DELETE CASCADE,
    player_id TEXT NOT NULL REFERENCES players(player_id) ON DELETE CASCADE,
    reason TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (game_id, player_id)
  )
|}

let ensure_game_stats_clean_view = (unit ->. unit) {|
  CREATE OR REPLACE VIEW game_stats_clean AS
  SELECT gs.*
  FROM game_stats gs
  WHERE NOT EXISTS (
    SELECT 1
    FROM game_stats_exclusions e
    WHERE e.game_id = gs.game_id
      AND e.player_id = gs.player_id
  )
|}

let upsert_game_stats_exclusion = (t2 string (t2 string string) ->. unit) {|
  INSERT INTO game_stats_exclusions (game_id, player_id, reason)
  VALUES (?, ?, ?)
  ON CONFLICT (game_id, player_id) DO UPDATE
    SET reason = EXCLUDED.reason
|}

let delete_game_stats_exclusion = (t2 string string ->. unit) {|
  DELETE FROM game_stats_exclusions
  WHERE game_id = ?
    AND player_id = ?
|}

let qa_stat_exclusions = (t2 string string ->* qa_stat_exclusion) {|
  SELECT
    e.game_id,
    COALESCE(g.game_date::text, 'Unknown') AS game_date,
    COALESCE(t.team_name_kr, '') AS team_name,
    e.player_id,
    COALESCE(p.player_name, '') AS player_name,
    COALESCE(gs.min_seconds, 0)::int AS min_seconds,
    COALESCE(gs.pts, 0)::int AS pts,
    COALESCE(e.reason, '') AS reason,
    COALESCE(e.created_at::text, '') AS created_at
  FROM game_stats_exclusions e
  JOIN games g ON g.game_id = e.game_id
  LEFT JOIN game_stats gs
    ON gs.game_id = e.game_id
   AND gs.player_id = e.player_id
  LEFT JOIN teams t ON t.team_code = gs.team_code
  LEFT JOIN players p ON p.player_id = e.player_id
  WHERE (? = 'ALL' OR g.season_code = ?)
  ORDER BY e.created_at DESC, e.game_id DESC, e.player_id ASC
  LIMIT 500
|}

let qa_stat_anomaly_candidates = (t2 string string ->* qa_stat_anomaly) {|
  WITH player_team AS (
    SELECT
      g.season_code,
      s.player_id,
      s.team_code,
      COUNT(*)::int AS gp,
      SUM(COALESCE(s.min_seconds, 0))::int AS min_seconds
    FROM game_stats_clean s
    JOIN games g ON g.game_id = s.game_id
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    GROUP BY g.season_code, s.player_id, s.team_code
  ),
  primary_team AS (
    SELECT DISTINCT ON (season_code, player_id)
      season_code,
      player_id,
      team_code AS primary_team_code,
      gp AS primary_gp,
      min_seconds AS primary_min_seconds
    FROM player_team
    ORDER BY season_code, player_id, min_seconds DESC, gp DESC, team_code
  ),
  suspects AS (
    SELECT
      pt.season_code,
      pt.player_id,
      pt.team_code,
      pt.gp,
      pt.min_seconds,
      prim.primary_team_code,
      prim.primary_gp,
      prim.primary_min_seconds
    FROM player_team pt
    JOIN primary_team prim
      ON prim.season_code = pt.season_code
     AND prim.player_id = pt.player_id
    WHERE pt.team_code <> prim.primary_team_code
      AND pt.gp = 1
      AND pt.min_seconds <= 120
      AND prim.primary_gp >= 10
  ),
  suspect_rows AS (
    SELECT
      gs.game_id,
      COALESCE(g.game_date::text, 'Unknown') AS game_date,
      t.team_name_kr AS team_name,
      gs.player_id,
      p.player_name,
      COALESCE(gs.min_seconds, 0)::int AS min_seconds,
      COALESCE(gs.pts, 0)::int AS pts,
      tprim.team_name_kr AS primary_team_name,
      s.primary_gp,
      s.primary_min_seconds
    FROM suspects s
    JOIN game_stats_clean gs
      ON gs.player_id = s.player_id
     AND gs.team_code = s.team_code
    JOIN games g
      ON g.game_id = gs.game_id
     AND g.season_code = s.season_code
    JOIN players p ON p.player_id = gs.player_id
    JOIN teams t ON t.team_code = gs.team_code
    JOIN teams tprim ON tprim.team_code = s.primary_team_code
  )
  SELECT
    game_id,
    game_date,
    team_name,
    player_id,
    player_name,
    min_seconds,
    pts,
    primary_team_name,
    primary_gp,
    primary_min_seconds
  FROM suspect_rows
  ORDER BY game_date DESC, min_seconds ASC, game_id DESC
  LIMIT 200
|}

(* Drop old MATERIALIZED VIEW to allow schema change *)
let drop_leaders_base_cache = (unit ->. unit) {|
  DROP MATERIALIZED VIEW IF EXISTS leaders_base_cache CASCADE
|}
let ensure_leaders_base_cache_view = (unit ->. unit) {|
  CREATE MATERIALIZED VIEW IF NOT EXISTS leaders_base_cache AS
  WITH player_team_games AS (
    -- Count games per player per team to find primary team
    SELECT pi.canonical_player_id AS player_id, s.team_code, COUNT(*) as game_count
    FROM game_stats_clean s
    JOIN player_identities pi ON pi.player_id = s.player_id
    JOIN games g ON g.game_id = s.game_id
    WHERE g.game_type != '10'
    GROUP BY pi.canonical_player_id, s.team_code
  ),
  primary_team AS (
    -- Select team with most games for each player
    SELECT DISTINCT ON (player_id) player_id, team_code
    FROM player_team_games
    ORDER BY player_id, game_count DESC
  )
  SELECT
    g.season_code,
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(*)::int AS gp,
    COALESCE(SUM(s.min_seconds), 0)::int AS min_seconds,
    COALESCE(SUM(s.pts), 0)::int AS pts,
    COALESCE(SUM(s.reb_tot), 0)::int AS reb,
    COALESCE(SUM(s.ast), 0)::int AS ast,
    COALESCE(SUM(s.stl), 0)::int AS stl,
    COALESCE(SUM(s.blk), 0)::int AS blk,
    COALESCE(SUM(s.tov), 0)::int AS tov,
    COALESCE(SUM(s.game_score), 0)::float8 AS eff,
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0)::int AS fg_m,
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0)::int AS fg_a,
    COALESCE(SUM(s.fg_3p_m), 0)::int AS fg3_m,
    COALESCE(SUM(s.fg_3p_a), 0)::int AS fg3_a,
    COALESCE(SUM(s.ft_m), 0)::int AS ft_m,
    COALESCE(SUM(s.ft_a), 0)::int AS ft_a
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN games g ON g.game_id = s.game_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  JOIN primary_team pt ON pt.player_id = pi.canonical_player_id
  JOIN teams t ON t.team_code = pt.team_code
  WHERE g.game_type != '10'
  GROUP BY g.season_code, p.player_id, p.player_name, t.team_name_kr
|}
let ensure_leaders_base_cache_unique = (unit ->. unit) {|
  CREATE UNIQUE INDEX IF NOT EXISTS idx_leaders_base_cache_unique
  ON leaders_base_cache(season_code, player_id, team_name_kr)
|}
let ensure_leaders_base_cache_index_season = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_leaders_base_cache_season
  ON leaders_base_cache(season_code)
|}
let refresh_leaders_base_cache = (unit ->. unit) {|
  REFRESH MATERIALIZED VIEW leaders_base_cache
|}
(* Migration: Drop old VIEWs before creating MATERIALIZED VIEWs *)
let drop_score_mismatch_view = (unit ->. unit) {|
  DROP MATERIALIZED VIEW IF EXISTS score_mismatch_games CASCADE
|}
(* Materialized View: score_mismatch_games - cached for performance *)
let ensure_score_mismatch_matview = (unit ->. unit) {|
  CREATE MATERIALIZED VIEW IF NOT EXISTS score_mismatch_games AS
  WITH sums AS (
    SELECT game_id, team_code, SUM(pts) AS pts_sum
    FROM game_stats_clean
    GROUP BY game_id, team_code
  ),
  joined AS (
    SELECT
      g.game_id,
      g.home_score AS home_score,
      g.away_score AS away_score,
      sh.pts_sum AS home_sum,
      sa.pts_sum AS away_sum
    FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
    WHERE g.game_type != '10'
  )
  SELECT game_id
  FROM joined
  WHERE
    (home_score IS NOT NULL AND home_sum IS NOT NULL AND home_score != home_sum)
    OR
    (away_score IS NOT NULL AND away_sum IS NOT NULL AND away_score != away_sum)
|}
let ensure_score_mismatch_index = (unit ->. unit) {|
  CREATE UNIQUE INDEX IF NOT EXISTS idx_score_mismatch_game_id ON score_mismatch_games(game_id)
|}
let refresh_score_mismatch = (unit ->. unit) {|
  REFRESH MATERIALIZED VIEW CONCURRENTLY score_mismatch_games
|}
let drop_games_calc_view = (unit ->. unit) {|
  DO $$
  DECLARE v_relkind char;
  BEGIN
    SELECT relkind INTO v_relkind FROM pg_class
    WHERE relname = 'games_calc'
      AND relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = 'public');
    IF v_relkind = 'v' THEN
      EXECUTE 'DROP VIEW games_calc CASCADE';
    ELSIF v_relkind = 'm' THEN
      EXECUTE 'DROP MATERIALIZED VIEW games_calc CASCADE';
    END IF;
  END $$
|}
  (* Materialized View: games_calc_v3 - fixed zero score issue *)
  let ensure_games_calc_matview = (unit ->. unit) {|
    CREATE MATERIALIZED VIEW IF NOT EXISTS games_calc_v3 AS
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats_clean
      GROUP BY game_id, team_code
    )
    SELECT
      g.*,
      sh.pts_sum AS home_sum,
      sa.pts_sum AS away_sum,
      COALESCE(NULLIF(g.home_score, 0), sh.pts_sum) AS home_score_calc,
      COALESCE(NULLIF(g.away_score, 0), sa.pts_sum) AS away_score_calc
    FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
  |}

  let ensure_games_calc_index = (unit ->. unit) {|
    CREATE UNIQUE INDEX IF NOT EXISTS idx_games_calc_v3_game_id ON games_calc_v3(game_id)
  |}

  let ensure_games_calc_season_index = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_games_calc_v3_season ON games_calc_v3(season_code)
  |}

  (* Backward compatibility: some stale workers still reference games_calc. *)
  let ensure_games_calc_compat_view = (unit ->. unit) {|
    CREATE OR REPLACE VIEW games_calc AS
    SELECT * FROM games_calc_v3
  |}

  let refresh_games_calc = (unit ->. unit) {|
    REFRESH MATERIALIZED VIEW CONCURRENTLY games_calc_v3
  |}
let pbp_periods_by_game = (string ->* string) {|
  SELECT period_code
  FROM (
    SELECT DISTINCT period_code,
      CASE period_code
        WHEN 'Q1' THEN 1
        WHEN 'Q2' THEN 2
        WHEN 'Q3' THEN 3
        WHEN 'Q4' THEN 4
        WHEN 'X1' THEN 5
        WHEN 'X2' THEN 6
        WHEN 'X3' THEN 7
        WHEN 'X4' THEN 8
        ELSE 99
      END AS sort_key
    FROM play_by_play_events
    WHERE game_id = ?
  ) t
  ORDER BY t.sort_key, t.period_code
|}
let pbp_events_by_game_period = (t2 string string ->* pbp_event) {|
  SELECT
    period_code,
    event_index,
    team_side,
    description,
    team1_score,
    team2_score,
    clock
  FROM play_by_play_events
  WHERE game_id = ?
    AND period_code = ?
  ORDER BY event_index ASC
|}
let all_teams = (unit ->* team_info) "SELECT team_code, team_name_kr FROM teams ORDER BY team_name_kr"
let all_seasons = (unit ->* season_info) {|
  SELECT DISTINCT
    g.season_code,
    COALESCE(s.season_name, g.season_code) as season_name
  FROM games g
  LEFT JOIN seasons s ON s.season_code = g.season_code
  WHERE g.season_code IS NOT NULL
  ORDER BY g.season_code
|}

(** Latest game date for data freshness display *)
let latest_game_date = (unit ->? string)
  "SELECT MAX(game_date) FROM games_calc_v3 WHERE game_type != '10' AND home_score_calc IS NOT NULL AND away_score_calc IS NOT NULL"

(** Historical seasons with champion and MVP data *)
let all_historical_seasons = (unit ->* historical_season) {|
  SELECT
    season_id,
    season_name,
    champion_team,
    runner_up,
    regular_mvp,
    finals_mvp,
    rookie_of_year,
    scoring_leader,
    notes
  FROM historical_seasons
  ORDER BY season_id DESC
|}

(** Legend players *)
let all_legend_players = (unit ->* legend_player) {|
  SELECT
    player_name,
    career_years,
    teams,
    championships,
    mvp_count,
    all_star_count,
    career_points,
    career_rebounds,
    career_assists,
    notable_achievements,
    is_hall_of_fame
  FROM legend_players
  ORDER BY championships DESC, mvp_count DESC, career_points DESC
|}

(** All coaches *)
let all_coaches = (unit ->* coach) {|
  SELECT
    coach_name,
    team,
    tenure_start,
    tenure_end,
    championships,
    regular_season_wins,
    playoff_wins,
    former_player,
    player_career_years,
    notable_achievements
  FROM coaches
  ORDER BY championships DESC, tenure_start DESC
|}

(** Player career history by player name *)
let player_career_by_name = (string ->* player_career_entry) {|
  SELECT
    player_name,
    season_id,
    team,
    jersey_number,
    games_played,
    points_per_game,
    rebounds_per_game,
    assists_per_game,
    is_allstar,
    awards
  FROM player_career_history
  WHERE player_name = ?
  ORDER BY season_id DESC
|}

let player_stats_base = (t2 (t2 string string) int ->* player_aggregate) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(*) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(AVG(s.pts), 0),
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0),
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0)
	    FROM game_stats_clean s
	    JOIN games_calc_v3 g ON g.game_id = s.game_id
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    WHERE g.game_type != '10'
	      AND (? = 'ALL' OR t.team_name_kr = ?)
	    GROUP BY p.player_id, p.player_name, t.team_name_kr
  ORDER BY AVG(s.game_score) DESC
  LIMIT ?
|}
let player_stats_by_season_base = (t2 string (t2 string int) ->* player_base) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(*) as gp,
    COALESCE(SUM(s.min_seconds), 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(AVG(s.pts), 0),
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0),
    COALESCE(
      SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN s.min_seconds
          ELSE 0
        END
      ),
      0
    ) as margin_seconds
	    FROM game_stats_clean s
	    JOIN games_calc_v3 g ON g.game_id = s.game_id
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    WHERE g.game_type != '10'
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    GROUP BY p.player_id, p.player_name, t.team_name_kr
|}
let player_career_aggregate = (string ->? player_aggregate) {|
  WITH pid AS (
    SELECT ? AS player_id
  ),
  canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
      (SELECT player_id FROM pid)
    ) AS canonical_player_id
  )
  SELECT
    p.player_id,
    p.player_name,
    COALESCE(
      (
        SELECT t2.team_name_kr
        FROM game_stats_clean s2
        JOIN player_identities pi2 ON pi2.player_id = s2.player_id
        JOIN games g2 ON g2.game_id = s2.game_id
        JOIN teams t2 ON t2.team_code = s2.team_code
        WHERE pi2.canonical_player_id = p.player_id
          AND g2.game_type != '10'
        ORDER BY g2.game_date DESC, g2.game_id DESC
        LIMIT 1
      ),
      ''
    ) AS team_name_kr,
    COUNT(DISTINCT s.game_id) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(AVG(s.pts), 0),
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0),
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0)
  FROM game_stats_clean s
  JOIN games_calc_v3 g ON g.game_id = s.game_id
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
    AND g.game_type != '10'
  GROUP BY p.player_id, p.player_name
|}

let player_aggregate_by_id = (t2 string (t2 string (t2 string (t2 string string))) ->? player_aggregate) {|
  SELECT
    p.player_id,
    p.player_name,
    COALESCE(
      (
        SELECT t2.team_name_kr
        FROM game_stats_clean s2
        JOIN player_identities pi2 ON pi2.player_id = s2.player_id
        JOIN games g2 ON g2.game_id = s2.game_id
        JOIN teams t2 ON t2.team_code = s2.team_code
        WHERE pi2.canonical_player_id = p.player_id
          AND g2.game_type != '10'
          AND ($1 = 'ALL' OR g2.season_code = $2)
        ORDER BY g2.game_date DESC, g2.game_id DESC
        LIMIT 1
      ),
      ''
    ) AS team_name_kr,
    COUNT(DISTINCT s.game_id) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(AVG(s.pts), 0),
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0),
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0)
  FROM game_stats_clean s
  JOIN games_calc_v3 g ON g.game_id = s.game_id
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  WHERE pi.canonical_player_id = COALESCE(
    (SELECT canonical_player_id FROM player_identities WHERE player_id = $3),
    $3
  )
    AND g.game_type != '10'
    AND ($4 = 'ALL' OR g.season_code = $5)
  GROUP BY p.player_id, p.player_name
|}

(** Player shooting stats aggregated from all games *)
let player_shooting_stats_by_id = (t2 string (t2 string string) ->? player_shooting_stats) {|
  WITH pid AS (
    SELECT $1 AS player_id
  ),
  canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
      (SELECT player_id FROM pid)
    ) AS canonical_player_id
  )
  SELECT
    p.player_id,
    TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')) AS player_name,
    COUNT(DISTINCT s.game_id) AS games,
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0) AS fg_made,
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0) AS fg_attempted,
    CASE
      WHEN SUM(s.fg_2p_a + s.fg_3p_a) > 0
      THEN SUM(s.fg_2p_m + s.fg_3p_m)::float / SUM(s.fg_2p_a + s.fg_3p_a)
      ELSE 0
    END AS fg_pct,
    COALESCE(SUM(s.fg_3p_m), 0) AS fg3_made,
    COALESCE(SUM(s.fg_3p_a), 0) AS fg3_attempted,
    CASE
      WHEN SUM(s.fg_3p_a) > 0
      THEN SUM(s.fg_3p_m)::float / SUM(s.fg_3p_a)
      ELSE 0
    END AS fg3_pct,
    COALESCE(SUM(s.ft_m), 0) AS ft_made,
    COALESCE(SUM(s.ft_a), 0) AS ft_attempted,
    CASE
      WHEN SUM(s.ft_a) > 0
      THEN SUM(s.ft_m)::float / SUM(s.ft_a)
      ELSE 0
    END AS ft_pct
  FROM game_stats_clean s
  JOIN games_calc_v3 g ON g.game_id = s.game_id
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN players p ON p.player_id = pi.canonical_player_id
  WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
    AND g.game_type != '10'
    AND ($2 = 'ALL' OR g.season_code = $3)
  GROUP BY p.player_id, p.player_name
|}

	  let team_totals_by_season = (t2 string (t2 string (t2 string int)) ->* team_totals) {|
	    SELECT
	      CASE WHEN ? = 'ALL' THEN 'ALL' ELSE g.season_code END as season,
	      t.team_name_kr,
	      COUNT(DISTINCT s.game_id) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    COALESCE(SUM(s.fg_2p_m), 0),
    COALESCE(SUM(s.fg_2p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0),
    COALESCE(SUM(s.reb_off), 0),
    COALESCE(SUM(s.reb_def), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
	      COALESCE(SUM(s.pts), 0)
	    FROM game_stats_clean s
	    JOIN games_calc_v3 g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    WHERE g.game_type != '10'
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    GROUP BY season, t.team_code, t.team_name_kr
	    ORDER BY t.team_name_kr ASC
	  |}
	  let team_margin_by_season = (t2 string int ->* team_margin) {|
	    WITH params AS (
	      SELECT ? AS season, ? AS include_mismatch
	    ),
	    filtered AS (
	      SELECT
	        g.season_code,
	        g.home_team_code AS team_code,
	        g.home_score_calc AS pts_for,
	        g.away_score_calc AS pts_against,
	        g.game_id
	      FROM games_calc_v3 g, params p
	      WHERE (p.season = 'ALL' OR g.season_code = p.season)
	        AND g.game_type != '10'
	        AND g.home_score_calc IS NOT NULL
	        AND g.away_score_calc IS NOT NULL
	        AND (p.include_mismatch = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	      UNION ALL
	      SELECT
	        g.season_code,
	        g.away_team_code AS team_code,
	        g.away_score_calc AS pts_for,
	        g.home_score_calc AS pts_against,
	        g.game_id
	      FROM games_calc_v3 g, params p
	      WHERE (p.season = 'ALL' OR g.season_code = p.season)
	        AND g.game_type != '10'
	        AND g.home_score_calc IS NOT NULL
	        AND g.away_score_calc IS NOT NULL
	        AND (p.include_mismatch = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    )
	    SELECT
	      CASE
	        WHEN (SELECT season FROM params) = 'ALL' THEN 'ALL'
	        ELSE season_code
	      END as season,
	      t.team_name_kr,
	      COUNT(*) as gp,
	      COALESCE(SUM(pts_for), 0) as pts_for,
	      COALESCE(SUM(pts_against), 0) as pts_against
	    FROM filtered f
	    JOIN teams t ON t.team_code = f.team_code
	    GROUP BY season, t.team_code, t.team_name_kr
	    ORDER BY t.team_name_kr ASC
	  |}

let team_standings_by_season = (string ->* team_standing) {|
  WITH params AS (
    SELECT ? AS season
  ),
  results AS (
    SELECT
      g.season_code,
      g.home_team_code AS team_code,
      g.home_score_calc AS pts_for,
      g.away_score_calc AS pts_against
    FROM games_calc_v3 g, params p
    WHERE (p.season = 'ALL' OR g.season_code = p.season)
      AND g.game_type != '10'
      AND g.home_score_calc IS NOT NULL
      AND g.away_score_calc IS NOT NULL
    UNION ALL
    SELECT
      g.season_code,
      g.away_team_code AS team_code,
      g.away_score_calc AS pts_for,
      g.home_score_calc AS pts_against
    FROM games_calc_v3 g, params p
    WHERE (p.season = 'ALL' OR g.season_code = p.season)
      AND g.game_type != '10'
      AND g.home_score_calc IS NOT NULL
      AND g.away_score_calc IS NOT NULL
  )
  SELECT
    t.team_name_kr,
    COUNT(*) as gp,
    SUM(CASE WHEN pts_for > pts_against THEN 1 ELSE 0 END) as wins,
    SUM(CASE WHEN pts_for < pts_against THEN 1 ELSE 0 END) as losses,
    AVG(pts_for) as avg_pts,
    AVG(pts_against) as avg_opp_pts
  FROM results r
  JOIN teams t ON t.team_code = r.team_code
  GROUP BY t.team_code
  ORDER BY wins DESC
|}

let all_games_paginated = (t2 (t2 string string) (t2 int int) ->* game_summary) {|
  SELECT
    g.game_id,
    COALESCE(g.game_date::text, 'Unknown'),
    t1.team_name_kr as home_team,
    t2.team_name_kr as away_team,
    g.home_score_calc,
    g.away_score_calc,
    g.game_type
  FROM games_calc_v3 g
  JOIN teams t1 ON g.home_team_code = t1.team_code
  JOIN teams t2 ON g.away_team_code = t2.team_code
  WHERE ($1 = 'ALL' OR g.season_code = $2)
    AND g.game_type != '10'
  ORDER BY g.game_date DESC, g.game_id DESC
  LIMIT $3 OFFSET $4
|}

let scored_games_by_season = (t2 string (t2 string int) ->* game_summary) {|
  SELECT
    g.game_id,
    COALESCE(g.game_date::text, 'Unknown'),
    t1.team_name_kr as home_team,
    t2.team_name_kr as away_team,
    g.home_score_calc,
    g.away_score_calc,
    g.game_type
  FROM games_calc_v3 g
  JOIN teams t1 ON g.home_team_code = t1.team_code
  JOIN teams t2 ON g.away_team_code = t2.team_code
  WHERE (? = 'ALL' OR g.season_code = ?)
    AND g.game_type != '10'
    AND g.home_score_calc IS NOT NULL
    AND g.away_score_calc IS NOT NULL
    AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
  ORDER BY g.game_date ASC, g.game_id ASC
|}

(** Small helper queries for prediction context (schedule/roster). *)
let game_season_by_id = (string ->? string) "SELECT season_code FROM games WHERE game_id = ?"

let team_core_player_ids =
  (let t = t2 string (t2 (t2 string string) int) in
   t ->* string)
  {|
    SELECT s.player_id
    FROM game_stats_clean s
    JOIN games g ON g.game_id = s.game_id
    JOIN teams t ON t.team_code = s.team_code
    WHERE t.team_name_kr = ?
      AND (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    GROUP BY s.player_id
    ORDER BY SUM(s.min_seconds) DESC
    LIMIT ?
  |}

let team_active_player_ids =
  (t2 string string ->* string)
  {|
    SELECT s.player_id
    FROM game_stats_clean s
    JOIN teams t ON t.team_code = s.team_code
    WHERE t.team_name_kr = ?
      AND s.game_id = ?
      AND COALESCE(s.min_seconds, 0) > 0
    ORDER BY s.min_seconds DESC
  |}

(** DB QA (data quality) queries. *)
let qa_games_total = (unit ->? int) {|
  SELECT COUNT(*)
  FROM games
  WHERE game_type != '10'
|}

let qa_finished_games_total = (unit ->? int) {|
  SELECT COUNT(*)
  FROM games_calc_v3 g
  WHERE g.game_type != '10'
    AND g.home_score_calc IS NOT NULL
    AND g.away_score_calc IS NOT NULL
|}

let qa_games_with_stats = (unit ->? int) {|
  SELECT COUNT(DISTINCT s.game_id)
  FROM game_stats_clean s
  JOIN games g ON g.game_id = s.game_id
  WHERE g.game_type != '10'
|}

let qa_pbp_games = (unit ->? int) {|
  SELECT COUNT(DISTINCT e.game_id)
  FROM play_by_play_events e
  JOIN games_calc_v3 g ON g.game_id = e.game_id
  WHERE g.game_type != '10'
    AND g.home_score_calc IS NOT NULL
    AND g.away_score_calc IS NOT NULL
|}

let qa_plus_minus_games = (unit ->? int) {|
  SELECT COUNT(DISTINCT pm.game_id)
  FROM player_plus_minus pm
  JOIN games_calc_v3 g ON g.game_id = pm.game_id
  WHERE g.game_type != '10'
    AND g.home_score_calc IS NOT NULL
    AND g.away_score_calc IS NOT NULL
|}

let qa_finished_games_total_by_season = (string ->? int) {|
  SELECT COUNT(*)
  FROM games_calc_v3 g
  WHERE g.game_type != '10'
    AND g.season_code = ?
    AND g.home_score_calc IS NOT NULL
    AND g.away_score_calc IS NOT NULL
|}

let qa_pbp_games_by_season = (string ->? int) {|
  SELECT COUNT(DISTINCT e.game_id)
  FROM play_by_play_events e
  JOIN games_calc_v3 g ON g.game_id = e.game_id
  WHERE g.game_type != '10'
    AND g.season_code = ?
    AND g.home_score_calc IS NOT NULL
    AND g.away_score_calc IS NOT NULL
|}

let qa_pbp_missing_games_sample_by_season = (string ->* qa_pbp_missing_game) {|
  WITH finished AS (
    SELECT
      game_id,
      game_date,
      home_team_code,
      away_team_code,
      home_score_calc::int as home_score,
      away_score_calc::int as away_score
    FROM games_calc_v3
    WHERE game_type != '10'
      AND season_code = ?
      AND home_score_calc IS NOT NULL
      AND away_score_calc IS NOT NULL
  )
  SELECT
    f.game_id,
    f.game_date::text,
    th.team_name_kr as home_team,
    ta.team_name_kr as away_team,
    f.home_score,
    f.away_score
  FROM finished f
  JOIN teams th ON th.team_code = f.home_team_code
  JOIN teams ta ON ta.team_code = f.away_team_code
  LEFT JOIN play_by_play_events e ON e.game_id = f.game_id
  WHERE e.game_id IS NULL
  ORDER BY f.game_date DESC, f.game_id DESC
  LIMIT 200
|}

let qa_schedule_total = (unit ->? int) {|
  SELECT COUNT(*) FROM schedule
|}

let qa_schedule_completed = (unit ->? int) {|
  SELECT COUNT(*) FROM schedule WHERE status = 'completed'
|}

let qa_schedule_missing_game_count = (unit ->? int) {|
  WITH completed AS (
    SELECT s.game_date, s.season_code, s.home_team_code, s.away_team_code
    FROM schedule s
    WHERE s.status = 'completed'
  )
  SELECT COUNT(*)
  FROM completed s
  LEFT JOIN games g
    ON g.season_code = s.season_code
   AND g.game_date::text = s.game_date
   AND g.home_team_code = s.home_team_code
   AND g.away_team_code = s.away_team_code
   AND g.game_type != '10'
  WHERE g.game_id IS NULL
|}

let qa_schedule_missing_game_sample = (unit ->* qa_schedule_missing_game) {|
  SELECT
    s.game_date,
    s.season_code,
    th.team_name_kr,
    ta.team_name_kr
  FROM schedule s
  JOIN teams th ON s.home_team_code = th.team_code
  JOIN teams ta ON s.away_team_code = ta.team_code
  LEFT JOIN games g
    ON g.season_code = s.season_code
   AND g.game_date::text = s.game_date
   AND g.home_team_code = s.home_team_code
   AND g.away_team_code = s.away_team_code
   AND g.game_type != '10'
  WHERE s.status = 'completed'
    AND g.game_id IS NULL
  ORDER BY s.game_date DESC, s.season_code DESC
  LIMIT 50
|}

let qa_schedule_missing_stats_count = (unit ->? int) {|
  WITH completed AS (
    SELECT s.game_date, s.season_code, s.home_team_code, s.away_team_code
    FROM schedule s
    WHERE s.status = 'completed'
  ),
  matched AS (
    SELECT s.game_date, s.season_code, s.home_team_code, s.away_team_code, g.game_id
    FROM completed s
    JOIN games g
      ON g.season_code = s.season_code
     AND g.game_date::text = s.game_date
     AND g.home_team_code = s.home_team_code
     AND g.away_team_code = s.away_team_code
     AND g.game_type != '10'
  ),
  stats AS (
    SELECT game_id, COUNT(*) AS stat_rows
    FROM game_stats_clean
    GROUP BY game_id
  )
  SELECT COUNT(*)
  FROM matched m
  LEFT JOIN stats st ON st.game_id = m.game_id
  WHERE st.stat_rows IS NULL OR st.stat_rows = 0
|}

let qa_schedule_missing_stats_sample = (unit ->* qa_schedule_missing_stats) {|
  WITH completed AS (
    SELECT s.game_date, s.season_code, s.home_team_code, s.away_team_code
    FROM schedule s
    WHERE s.status = 'completed'
  ),
  matched AS (
    SELECT s.game_date, s.season_code, s.home_team_code, s.away_team_code, g.game_id
    FROM completed s
    JOIN games g
      ON g.season_code = s.season_code
     AND g.game_date::text = s.game_date
     AND g.home_team_code = s.home_team_code
     AND g.away_team_code = s.away_team_code
     AND g.game_type != '10'
  ),
  stats AS (
    SELECT game_id, COUNT(*) AS stat_rows
    FROM game_stats_clean
    GROUP BY game_id
  )
  SELECT
    m.game_id,
    m.game_date,
    th.team_name_kr,
    ta.team_name_kr
  FROM matched m
  JOIN teams th ON m.home_team_code = th.team_code
  JOIN teams ta ON m.away_team_code = ta.team_code
  LEFT JOIN stats st ON st.game_id = m.game_id
  WHERE st.stat_rows IS NULL OR st.stat_rows = 0
  ORDER BY m.game_date DESC, m.game_id DESC
  LIMIT 50
|}

let qa_schedule_coverage = (unit ->* qa_schedule_coverage) {|
  WITH completed AS (
    SELECT s.season_code, s.game_date, s.home_team_code, s.away_team_code
    FROM schedule s
    WHERE s.status = 'completed'
  ),
  matched AS (
    SELECT
      c.season_code,
      COUNT(*) AS schedule_completed,
      COUNT(g.game_id) AS matched
    FROM completed c
    LEFT JOIN games g
      ON g.season_code = c.season_code
     AND g.game_date::text = c.game_date
     AND g.home_team_code = c.home_team_code
     AND g.away_team_code = c.away_team_code
     AND g.game_type != '10'
    GROUP BY c.season_code
  ),
  games_by_season AS (
    SELECT
      season_code,
      COUNT(*) AS games_total,
      COUNT(*) FILTER (
        WHERE home_team_code IS NULL OR home_team_code = '' OR away_team_code IS NULL OR away_team_code = ''
      ) AS games_missing_team
    FROM games
    WHERE game_type != '10'
    GROUP BY season_code
  )
  SELECT
    m.season_code,
    m.schedule_completed,
    COALESCE(g.games_total, 0) AS games_total,
    m.matched,
    (m.schedule_completed - m.matched) AS missing,
    CASE
      WHEN m.schedule_completed = 0 THEN 0
      ELSE ROUND(100.0 * m.matched / m.schedule_completed, 1)
    END AS coverage_pct,
    COALESCE(g.games_total, 0) = 0 AS season_uningested,
    COALESCE(g.games_missing_team, 0) > 0 AS games_missing_team
  FROM matched m
  LEFT JOIN games_by_season g ON g.season_code = m.season_code
  ORDER BY m.season_code
|}

let qa_score_mismatch_count = (unit ->? int) {|
  WITH sums AS (
    SELECT game_id, team_code, SUM(pts) AS pts_sum
    FROM game_stats_clean
    GROUP BY game_id, team_code
  ),
  joined AS (
    SELECT
      g.game_id,
      g.home_score AS home_score,
      g.away_score AS away_score,
      sh.pts_sum AS home_sum,
      sa.pts_sum AS away_sum
    FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
    WHERE g.game_type != '10'
  )
  SELECT COUNT(*)
  FROM joined
  WHERE
    (home_score IS NOT NULL AND home_sum IS NOT NULL AND home_score != home_sum)
    OR
    (away_score IS NOT NULL AND away_sum IS NOT NULL AND away_score != away_sum)
|}

let qa_score_mismatch_sample = (unit ->* qa_score_mismatch) {|
  WITH sums AS (
    SELECT game_id, team_code, SUM(pts) AS pts_sum
    FROM game_stats_clean
    GROUP BY game_id, team_code
  )
  SELECT
    g.game_id,
    COALESCE(g.game_date::text, 'Unknown'),
    t1.team_name_kr,
    t2.team_name_kr,
    g.home_score,
    g.away_score,
    sh.pts_sum,
    sa.pts_sum
  FROM games g
  JOIN teams t1 ON g.home_team_code = t1.team_code
  JOIN teams t2 ON g.away_team_code = t2.team_code
  LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
  LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
  WHERE g.game_type != '10'
    AND (
      (g.home_score IS NOT NULL AND sh.pts_sum IS NOT NULL AND g.home_score != sh.pts_sum)
      OR
      (g.away_score IS NOT NULL AND sa.pts_sum IS NOT NULL AND g.away_score != sa.pts_sum)
    )
  ORDER BY g.game_date DESC, g.game_id DESC
  LIMIT 50
|}

let qa_team_count_anomaly_count = (unit ->? int) {|
  SELECT COUNT(*)
  FROM (
    SELECT s.game_id
    FROM game_stats_clean s
    JOIN games g ON g.game_id = s.game_id
    WHERE g.game_type != '10'
    GROUP BY s.game_id
    HAVING COUNT(DISTINCT s.team_code) != 2
  )
|}

let qa_team_count_anomaly_sample = (unit ->* qa_team_count_anomaly) {|
  SELECT
    s.game_id,
    COUNT(DISTINCT s.team_code) AS team_count
  FROM game_stats_clean s
  JOIN games g ON g.game_id = s.game_id
  WHERE g.game_type != '10'
  GROUP BY s.game_id
  HAVING COUNT(DISTINCT s.team_code) != 2
  ORDER BY team_count DESC, s.game_id DESC
  LIMIT 50
|}

let qa_duplicate_player_row_count = (unit ->? int) {|
  SELECT COUNT(*)
  FROM (
    SELECT s.game_id, s.team_code, s.player_id
    FROM game_stats_clean s
    JOIN games g ON g.game_id = s.game_id
    WHERE g.game_type != '10'
    GROUP BY s.game_id, s.team_code, s.player_id
    HAVING COUNT(*) > 1
  )
|}

	  let qa_duplicate_player_row_sample = (unit ->* qa_duplicate_player_row) {|
	    SELECT
	      s.game_id,
	      s.team_code,
	      t.team_name_kr,
	      s.player_id,
	      TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')) AS player_name,
	      COUNT(*) AS row_count
	    FROM game_stats_clean s
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    JOIN players p ON p.player_id = s.player_id
  WHERE g.game_type != '10'
  GROUP BY
    s.game_id,
    s.team_code,
    t.team_name_kr,
    s.player_id,
    TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', ''))
  HAVING COUNT(*) > 1
  ORDER BY row_count DESC, s.game_id DESC
  LIMIT 50
|}

	  let qa_duplicate_player_name_count = (unit ->? int) {|
	    SELECT COUNT(*)
	    FROM (
	      SELECT TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')) AS normalized_name
	      FROM players
	      GROUP BY TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', ''))
	      HAVING COUNT(DISTINCT player_id) > 1
	    )
	  |}
	
	  let qa_duplicate_player_name_sample = (unit ->* qa_duplicate_player_name_row) {|
	    SELECT
	      TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')) AS normalized_name,
	      COUNT(DISTINCT player_id) AS id_count,
	      STRING_AGG(DISTINCT player_id, ',') AS player_ids
	    FROM players
	    GROUP BY TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', ''))
	    HAVING COUNT(DISTINCT player_id) > 1
	    ORDER BY id_count DESC, normalized_name ASC
	    LIMIT 50
	  |}

(** Schedule vs Games QA:
    - Missing schedule rows (completed schedule not found in games)
    - Split into: ingested seasons vs un-ingested seasons
    - Reason taxonomy for ingested seasons only *)

let qa_schedule_missing_summary = (unit ->? qa_schedule_missing_summary) {|
  WITH sc AS (
    SELECT season_code, game_date::date AS game_date, home_team_code, away_team_code
    FROM schedule
    WHERE status = 'completed'
  ),
  gk AS (
    SELECT season_code, game_date, home_team_code, away_team_code
    FROM games
  ),
  joined AS (
    SELECT sc.season_code,
           CASE WHEN gk.season_code IS NULL THEN 1 ELSE 0 END AS is_missing
    FROM sc
    LEFT JOIN gk
      ON gk.season_code = sc.season_code
     AND gk.game_date = sc.game_date
     AND gk.home_team_code = sc.home_team_code
     AND gk.away_team_code = sc.away_team_code
  )
  SELECT
    COALESCE(SUM(is_missing) FILTER (WHERE season_code IN (SELECT DISTINCT season_code FROM games)), 0)::int AS missing_ingested,
    COALESCE(SUM(is_missing) FILTER (WHERE season_code NOT IN (SELECT DISTINCT season_code FROM games)), 0)::int AS missing_uningested,
    COALESCE(SUM(is_missing), 0)::int AS missing_total
  FROM joined
|}

let qa_schedule_missing_reason_counts = (unit ->* t2 string int) {|
  WITH sc AS (
    SELECT season_code, game_date::date AS game_date, home_team_code, away_team_code
    FROM schedule
    WHERE status = 'completed'
  ),
  missing AS (
    SELECT sc.*
    FROM sc
    LEFT JOIN games g
      ON g.season_code = sc.season_code
     AND g.game_date = sc.game_date
     AND g.home_team_code = sc.home_team_code
     AND g.away_team_code = sc.away_team_code
    WHERE g.game_id IS NULL
      AND sc.season_code IN (SELECT DISTINCT season_code FROM games)
  ),
  games_by_date AS (
    SELECT
      season_code,
      game_date,
      COUNT(*)::int AS games_on_date,
      BOOL_OR(home_team_code = 'UNKNOWN' OR away_team_code = 'UNKNOWN') AS has_unknown
    FROM games
    GROUP BY season_code, game_date
  ),
  missing_enriched AS (
    SELECT
      m.season_code,
      m.game_date,
      m.home_team_code,
      m.away_team_code,
      COALESCE(gbd.games_on_date, 0) AS games_on_date,
      COALESCE(gbd.has_unknown, false) AS has_unknown,
      (m.home_team_code !~ '^[0-9]{2}$' OR m.away_team_code !~ '^[0-9]{2}$') AS schedule_alpha_code,
      (m.home_team_code = 'AS' OR m.away_team_code = 'AS') AS schedule_is_as,
      EXISTS (
        SELECT 1
        FROM games g2
        WHERE g2.season_code = m.season_code
          AND g2.game_date = m.game_date
          AND g2.home_team_code = m.away_team_code
          AND g2.away_team_code = m.home_team_code
      ) AS swapped_exists
    FROM missing m
    LEFT JOIN games_by_date gbd
      ON gbd.season_code = m.season_code
     AND gbd.game_date = m.game_date
  )
  SELECT
    CASE
      WHEN schedule_alpha_code AND schedule_is_as THEN 'schedule_allstar_AS'
      WHEN schedule_alpha_code THEN 'schedule_alpha_code'
      WHEN games_on_date = 0 THEN 'no_game_on_date'
      WHEN swapped_exists THEN 'home_away_swapped'
      WHEN has_unknown THEN 'games_UNKNOWN_team_code'
      ELSE 'team_code_mismatch'
    END AS reason,
    COUNT(*)::int AS n
  FROM missing_enriched
  GROUP BY reason
  ORDER BY n DESC, reason
|}

let qa_schedule_missing_samples = (unit ->* qa_schedule_missing_sample) {|
  WITH sc AS (
    SELECT season_code, game_date::date AS game_date, game_date AS game_date_raw, home_team_code, away_team_code
    FROM schedule
    WHERE status = 'completed'
  ),
  missing AS (
    SELECT sc.*
    FROM sc
    LEFT JOIN games g
      ON g.season_code = sc.season_code
     AND g.game_date = sc.game_date
     AND g.home_team_code = sc.home_team_code
     AND g.away_team_code = sc.away_team_code
    WHERE g.game_id IS NULL
      AND sc.season_code IN (SELECT DISTINCT season_code FROM games)
  ),
  games_by_date AS (
    SELECT
      season_code,
      game_date,
      COUNT(*)::int AS games_on_date,
      BOOL_OR(home_team_code = 'UNKNOWN' OR away_team_code = 'UNKNOWN') AS has_unknown,
      STRING_AGG(
        g.game_id || CASE
          WHEN g.home_team_code IS NULL OR g.away_team_code IS NULL THEN ''
          ELSE '(' || g.home_team_code || '-' || g.away_team_code || ')'
        END,
        ', ' ORDER BY g.game_id
      ) AS games_info
    FROM games g
    GROUP BY season_code, game_date
  ),
  missing_enriched AS (
    SELECT
      m.season_code,
      m.game_date,
      m.game_date_raw,
      m.home_team_code,
      m.away_team_code,
      COALESCE(gbd.games_on_date, 0) AS games_on_date,
      COALESCE(gbd.has_unknown, false) AS has_unknown,
      gbd.games_info AS games_info,
      (m.home_team_code !~ '^[0-9]{2}$' OR m.away_team_code !~ '^[0-9]{2}$') AS schedule_alpha_code,
      (m.home_team_code = 'AS' OR m.away_team_code = 'AS') AS schedule_is_as,
      EXISTS (
        SELECT 1
        FROM games g2
        WHERE g2.season_code = m.season_code
          AND g2.game_date = m.game_date
          AND g2.home_team_code = m.away_team_code
          AND g2.away_team_code = m.home_team_code
      ) AS swapped_exists
    FROM missing m
    LEFT JOIN games_by_date gbd
      ON gbd.season_code = m.season_code
     AND gbd.game_date = m.game_date
  ),
  reasoned AS (
    SELECT
      season_code,
      game_date,
      game_date_raw,
      home_team_code,
      away_team_code,
      CASE
        WHEN schedule_alpha_code AND schedule_is_as THEN 'schedule_allstar_AS'
        WHEN schedule_alpha_code THEN 'schedule_alpha_code'
        WHEN games_on_date = 0 THEN 'no_game_on_date'
        WHEN swapped_exists THEN 'home_away_swapped'
        WHEN has_unknown THEN 'games_UNKNOWN_team_code'
        ELSE 'team_code_mismatch'
      END AS reason,
      games_on_date,
      games_info
    FROM missing_enriched
  ),
  ranked AS (
    SELECT *,
           ROW_NUMBER() OVER (PARTITION BY reason ORDER BY season_code, game_date) AS rn
    FROM reasoned
  )
  SELECT
    season_code,
    game_date_raw,
    home_team_code,
    away_team_code,
    reason,
    games_on_date,
    games_info
  FROM ranked
  WHERE rn <= 20
  ORDER BY reason, season_code, game_date
|}

let qa_duplicate_player_identity_count = (unit ->? int) {|
  SELECT COUNT(*)
  FROM (
    SELECT
      TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')) AS normalized_name,
      birth_date
    FROM players
    WHERE birth_date IS NOT NULL
    GROUP BY
      TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')),
      birth_date
    HAVING COUNT(DISTINCT player_id) > 1
  )
|}

let qa_duplicate_player_identity_sample = (unit ->* qa_duplicate_player_identity_row) {|
  SELECT
    TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')) AS normalized_name,
    TO_CHAR(birth_date, 'YYYY-MM-DD') AS birth_date,
    COUNT(DISTINCT player_id) AS id_count,
    STRING_AGG(DISTINCT player_id, ',' ORDER BY player_id) AS player_ids
  FROM players
  WHERE birth_date IS NOT NULL
  GROUP BY
    TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')),
    birth_date
  HAVING COUNT(DISTINCT player_id) > 1
  ORDER BY id_count DESC, normalized_name ASC, birth_date ASC
  LIMIT 50
|}
let game_info_by_id = (string ->? game_info) {|
  WITH sums AS (
    SELECT game_id, team_code, SUM(pts) AS pts_sum
    FROM game_stats_clean
    GROUP BY game_id, team_code
  ),
	    scored_games AS (
	      SELECT
	        g.*,
	        sh.pts_sum AS home_sum,
	        sa.pts_sum AS away_sum,
	        COALESCE(NULLIF(g.home_score, 0), sh.pts_sum) AS home_score_calc,
	        COALESCE(NULLIF(g.away_score, 0), sa.pts_sum) AS away_score_calc,
	        CASE
	          WHEN NULLIF(g.home_score, 0) IS NOT NULL
	            AND NULLIF(g.away_score, 0) IS NOT NULL
	            AND sh.pts_sum IS NOT NULL
	            AND sa.pts_sum IS NOT NULL
	            AND NULLIF(g.home_score, 0) = sh.pts_sum
	            AND NULLIF(g.away_score, 0) = sa.pts_sum
	            THEN 2
	          WHEN NULLIF(g.home_score, 0) IS NOT NULL
	            AND NULLIF(g.away_score, 0) IS NOT NULL
	            AND sh.pts_sum IS NOT NULL
	            AND sa.pts_sum IS NOT NULL
	            AND (NULLIF(g.home_score, 0) != sh.pts_sum OR NULLIF(g.away_score, 0) != sa.pts_sum)
	            THEN 0
	          ELSE 1
	        END AS score_quality
	      FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
  )
  SELECT
    g.game_id,
    COALESCE(g.game_date::text, 'Unknown'),
    g.home_team_code,
    t1.team_name_kr,
    g.away_team_code,
    t2.team_name_kr,
    COALESCE(g.home_score_calc, 0),
    COALESCE(g.away_score_calc, 0),
    COALESCE(g.score_quality, 1)
  FROM scored_games g
  JOIN teams t1 ON g.home_team_code = t1.team_code
  JOIN teams t2 ON g.away_team_code = t2.team_code
  WHERE g.game_id = ?
|}
let boxscore_stats_by_game_id = (string ->* boxscore_player_stat) {|
  WITH player_total_minutes AS (
    SELECT
      pi.canonical_player_id AS player_id,
      SUM(s.min_seconds) AS total_min_seconds
    FROM game_stats_clean s
    JOIN player_identities pi ON pi.player_id = s.player_id
    GROUP BY pi.canonical_player_id
  ),
  ranked AS (
    SELECT
      pi.canonical_player_id AS player_id,
      TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')) AS player_name,
      p.position,
      t.team_code,
	        t.team_name_kr,
	        s.min_seconds,
	        s.pts,
	        pm.plus_minus,
	        s.reb_tot,
	        s.ast,
	        s.stl,
	        s.blk,
	        s.tov,
      s.fg_2p_m,
      s.fg_2p_a,
      s.fg_3p_m,
      s.fg_3p_a,
      s.ft_m,
      s.ft_a,
      ROW_NUMBER() OVER (
        PARTITION BY
          s.game_id,
          s.team_code,
          TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')),
          CAST(ROUND(s.min_seconds / 6.0) AS INT),
          s.pts,
          s.reb_tot,
          s.ast,
          s.stl,
          s.blk,
          s.tov,
          s.fg_2p_m,
          s.fg_2p_a,
          s.fg_3p_m,
          s.fg_3p_a,
          s.ft_m,
          s.ft_a
	          ORDER BY
            (pm.plus_minus IS NOT NULL) DESC,
            (p.position IS NOT NULL AND TRIM(p.position) <> '') DESC,
            COALESCE(ptm.total_min_seconds, 0) DESC,
            s.min_seconds DESC,
            p.player_id DESC
	        ) AS rn
	      FROM game_stats_clean s
	      JOIN player_identities pi ON pi.player_id = s.player_id
	      JOIN players p ON p.player_id = pi.canonical_player_id
	      JOIN teams t ON s.team_code = t.team_code
	      LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	      LEFT JOIN player_total_minutes ptm ON ptm.player_id = pi.canonical_player_id
	      WHERE s.game_id = ?
	    )
	    SELECT
    player_id,
    player_name,
    position,
    team_code,
    team_name_kr,
	      min_seconds,
	      pts,
	      plus_minus,
	      reb_tot,
	      ast,
	      stl,
	      blk,
    tov,
    fg_2p_m,
    fg_2p_a,
    fg_3p_m,
    fg_3p_a,
    ft_m,
    ft_a
  FROM ranked
  WHERE rn = 1
  ORDER BY min_seconds DESC
|}
let top_players = (int ->* player_aggregate) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(*) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(AVG(s.pts), 0),
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0),
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0)
  FROM game_stats_clean s
  JOIN games_calc_v3 g ON g.game_id = s.game_id
  JOIN players p ON s.player_id = p.player_id
  JOIN teams t ON s.team_code = t.team_code
  WHERE g.game_type != '10'
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  ORDER BY AVG(s.game_score) DESC
  LIMIT ?
|}
let players_by_team = (t2 string (t2 string (t2 string int)) ->* player_aggregate) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(*) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(AVG(s.pts), 0),
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0),
    COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
    COALESCE(SUM(s.fg_3p_m), 0),
    COALESCE(SUM(s.fg_3p_a), 0),
    COALESCE(SUM(s.ft_m), 0),
    COALESCE(SUM(s.ft_a), 0)
	    FROM game_stats_clean s
	    JOIN games_calc_v3 g ON g.game_id = s.game_id
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    WHERE t.team_name_kr = ?
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	    GROUP BY p.player_id, p.player_name, t.team_name_kr
  ORDER BY AVG(s.game_score) DESC
  LIMIT ?
|}
	  let leaders_base_stats = (t2 string string ->* leader_base) {|
	    SELECT
	      p.player_id,
	      p.player_name,
	      t.team_name_kr,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds), 0),
	      COALESCE(SUM(s.pts), 0),
	      COALESCE(SUM(s.reb_tot), 0),
	      COALESCE(SUM(s.ast), 0),
	      COALESCE(SUM(s.stl), 0),
	      COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
	      COALESCE(SUM(s.game_score) * 1.0, 0),
	      COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
	      COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
	      COALESCE(SUM(s.fg_3p_m), 0),
	      COALESCE(SUM(s.fg_3p_a), 0),
	      COALESCE(SUM(s.ft_m), 0),
	      COALESCE(SUM(s.ft_a), 0)
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games g ON g.game_id = s.game_id
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	    GROUP BY p.player_id, p.player_name, t.team_name_kr
	  |}
let leaders_base_cached = (string ->* leader_base) {|
  SELECT
    player_id,
    player_name,
    team_name_kr,
    gp,
    min_seconds,
    pts,
    reb,
    ast,
    stl,
    blk,
    tov,
    eff,
    fg_m,
    fg_a,
    fg3_m,
    fg3_a,
    ft_m,
    ft_a
  FROM leaders_base_cache
  WHERE season_code = ?
|}
let leaders_base_cached_all = (unit ->* leader_base) {|
  SELECT
    player_id,
    player_name,
    team_name_kr,
    COALESCE(SUM(gp), 0)::int,
    COALESCE(SUM(min_seconds), 0)::int,
    COALESCE(SUM(pts), 0)::int,
    COALESCE(SUM(reb), 0)::int,
    COALESCE(SUM(ast), 0)::int,
    COALESCE(SUM(stl), 0)::int,
    COALESCE(SUM(blk), 0)::int,
    COALESCE(SUM(tov), 0)::int,
    COALESCE(SUM(eff), 0)::float8,
    COALESCE(SUM(fg_m), 0)::int,
    COALESCE(SUM(fg_a), 0)::int,
    COALESCE(SUM(fg3_m), 0)::int,
    COALESCE(SUM(fg3_a), 0)::int,
    COALESCE(SUM(ft_m), 0)::int,
    COALESCE(SUM(ft_a), 0)::int
  FROM leaders_base_cache
  GROUP BY player_id, player_name, team_name_kr
|}

let leaders_pts = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.pts) DESC LIMIT 5 |}
let leaders_pts_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.pts) / SUM(s.min_seconds)) DESC LIMIT 5 |}
let leaders_reb = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.reb_tot) DESC LIMIT 5 |}
let leaders_reb_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.reb_tot) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.reb_tot) / SUM(s.min_seconds)) DESC LIMIT 5 |}
let leaders_ast = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.ast) DESC LIMIT 5 |}
let leaders_ast_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ast) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.ast) / SUM(s.min_seconds)) DESC LIMIT 5 |}
let leaders_stl = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.stl) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.stl) DESC LIMIT 5 |}
let leaders_stl_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.stl) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.stl) / SUM(s.min_seconds)) DESC LIMIT 5 |}
let leaders_blk = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.blk) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.blk) DESC LIMIT 5 |}
let leaders_blk_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.blk) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.blk) / SUM(s.min_seconds)) DESC LIMIT 5 |}

(* Leaders - extended (basketball-reference style) *)
let leaders_gp = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, COUNT(*) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY COUNT(*) DESC LIMIT 5 |}
let leaders_min = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.min_seconds) / 60.0 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.min_seconds) DESC LIMIT 5 |}
let leaders_min_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.min_seconds) / 60.0 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY SUM(s.min_seconds) DESC LIMIT 5 |}

let leaders_pts_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.pts) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.pts) DESC LIMIT 5 |}
let leaders_reb_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.reb_tot) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.reb_tot) DESC LIMIT 5 |}
let leaders_ast_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.ast) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.ast) DESC LIMIT 5 |}
let leaders_stl_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.stl) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.stl) DESC LIMIT 5 |}
let leaders_blk_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.blk) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.blk) DESC LIMIT 5 |}

let leaders_tov = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.tov) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.tov) DESC LIMIT 5 |}
let leaders_tov_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.tov) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.tov) / SUM(s.min_seconds)) DESC LIMIT 5 |}
let leaders_tov_totals = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.tov) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.tov) DESC LIMIT 5 |}

let leaders_eff = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.game_score) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.game_score) DESC LIMIT 5 |}
let leaders_eff_per36 = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds)) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.game_score) / SUM(s.min_seconds)) DESC LIMIT 5 |}

let leaders_fg_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY ((SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}
let leaders_fg3_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_3p_a) >= 20 ORDER BY ((SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0)) DESC LIMIT 5 |}
let leaders_ft_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.ft_a) >= 20 ORDER BY ((SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0)) DESC LIMIT 5 |}
let leaders_ts_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING (SUM(s.fg_2p_a + s.fg_3p_a) + SUM(s.ft_a)) >= 50 ORDER BY ((SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0)) DESC LIMIT 5 |}
let leaders_efg_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, ((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY (((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}
let leaders_usg_pct = (t2 string string ->* leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(u.usg_pct) FROM player_usg_stats u JOIN player_identities pi ON pi.player_id = u.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON u.team_code = t.team_code JOIN games g ON g.game_id = u.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' AND u.usg_pct IS NOT NULL GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 AND SUM(u.min_seconds) >= 6000 ORDER BY AVG(u.usg_pct) DESC LIMIT 5 |}

(** Position-filtered leader queries
    Position values: 'G' (Guard), 'F' (Forward), 'C' (Center), 'ALL' (no filter)
*)
	  let leaders_pts_by_position = (t2 (t2 string string) string ->* leader_entry) {|
	    SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts)
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games g ON g.game_id = s.game_id
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
    AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.pts) DESC
  LIMIT 10
|}
	  let leaders_reb_by_position = (t2 (t2 string string) string ->* leader_entry) {|
	    SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot)
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games g ON g.game_id = s.game_id
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
    AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.reb_tot) DESC
  LIMIT 10
|}
	  let leaders_ast_by_position = (t2 (t2 string string) string ->* leader_entry) {|
	    SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast)
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games g ON g.game_id = s.game_id
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
    AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.ast) DESC
  LIMIT 10
|}
	  let leaders_eff_by_position = (t2 (t2 string string) string ->* leader_entry) {|
	    SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.game_score)
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games g ON g.game_id = s.game_id
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
    AND (? = 'ALL' OR p.position LIKE ? || '%')
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.game_score) DESC
  LIMIT 10
|}

(** Stat Awards (unofficial) *)
	  let stat_mvp_eff = (t2 string (t2 string int) ->* leader_entry) {|
	    SELECT
	      p.player_id,
	      p.player_name,
	      t.team_name_kr,
	      AVG(s.game_score)
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN players p ON p.player_id = pi.canonical_player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games g ON g.game_id = s.game_id
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
    AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(*) >= 5
  ORDER BY AVG(s.game_score) DESC
  LIMIT 5
|}

	  let stat_mip_eff_delta = (t2 string (t2 int (t2 string int)) ->* leader_entry) {|
	    WITH cur AS (
	      SELECT
	        pi.canonical_player_id AS player_id,
	        AVG(s.game_score) AS eff_cur,
	        COUNT(*) AS gp_cur,
	        MAX(t.team_name_kr) AS team_name
	      FROM game_stats_clean s
	      JOIN player_identities pi ON pi.player_id = s.player_id
	      JOIN games g ON g.game_id = s.game_id
	      JOIN teams t ON t.team_code = s.team_code
	      WHERE g.season_code = ?
	        AND g.game_type != '10'
	        AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	      GROUP BY pi.canonical_player_id
	    ),
	    prev AS (
	      SELECT
	        pi.canonical_player_id AS player_id,
	        AVG(s.game_score) AS eff_prev,
	        COUNT(*) AS gp_prev
	      FROM game_stats_clean s
	      JOIN player_identities pi ON pi.player_id = s.player_id
	      JOIN games g ON g.game_id = s.game_id
	      WHERE g.season_code = ?
	        AND g.game_type != '10'
	        AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	      GROUP BY pi.canonical_player_id
	    )
	    SELECT
	      p.player_id,
	      p.player_name,
	      cur.team_name,
	      (cur.eff_cur - prev.eff_prev) AS delta
	    FROM cur
	    JOIN prev ON prev.player_id = cur.player_id
	    JOIN players p ON p.player_id = cur.player_id
	    WHERE cur.gp_cur >= 10
	      AND prev.gp_prev >= 10
	    ORDER BY delta DESC
	    LIMIT 5
	  |}
let player_info = (string ->? player_info) {|
  SELECT
    p.player_id,
    p.player_name,
    p.position,
    p.birth_date,
    p.height,
    p.weight
  FROM players p
  WHERE p.player_id = COALESCE(
    (SELECT canonical_player_id FROM player_identities WHERE player_id = $1),
    $1
  )
|}
let all_player_info = (unit ->* Db_types.player_info)
  "SELECT player_id, player_name, position, birth_date, height, weight FROM players"

let player_draft_by_player_id = (string ->? player_draft) {|
  WITH base AS (
    SELECT canonical_player_id AS canonical_id
    FROM player_identities
    WHERE player_id = $1
  ),
  alias_ids AS (
    SELECT player_id
    FROM player_identities
    WHERE canonical_player_id = (SELECT canonical_id FROM base)
  )
  SELECT
    (SELECT canonical_id FROM base) AS player_id,
    draft_year,
    draft_round,
    pick_in_round,
    overall_pick,
    draft_team,
    raw_text,
    source_url,
    scraped_at
  FROM player_drafts
  WHERE player_id IN (SELECT player_id FROM alias_ids)
  ORDER BY player_id ASC
  LIMIT 1
|}

let draft_years = (unit ->* int) {|
  SELECT DISTINCT draft_year
  FROM player_drafts
  WHERE draft_year IS NOT NULL
  ORDER BY draft_year DESC
|}

let draft_picks_filtered = (t2 (t2 int int) (t2 string string) ->* draft_pick_row) {|
  SELECT
    d.player_id,
    p.player_name,
    d.draft_year,
    d.draft_round,
    d.pick_in_round,
    d.overall_pick,
    d.draft_team,
    d.raw_text,
    d.source_url,
    d.scraped_at
  FROM player_drafts d
  JOIN players p ON p.player_id = d.player_id
  WHERE (? = 0 OR d.draft_year = ?)
    AND (p.player_name LIKE ? OR d.raw_text LIKE ?)
  ORDER BY
    COALESCE(d.draft_year, 0) DESC,
    COALESCE(d.overall_pick, 999) ASC,
    p.player_name ASC
  LIMIT 300
|}

let official_trade_events_by_player_name_norm = (string ->* official_trade_event) {|
  SELECT
    event_date,
    event_year,
    event_text,
    source_url,
    scraped_at
  FROM official_trade_events
  WHERE event_text_norm LIKE ?
  ORDER BY event_date DESC, id DESC
  LIMIT 30
|}

let official_trade_years = (unit ->* int) {|
  SELECT DISTINCT event_year
  FROM official_trade_events
  ORDER BY event_year DESC
|}

let official_trade_events_filtered = (t2 (t2 int int) string ->* official_trade_event) {|
  SELECT
    event_date,
    event_year,
    event_text,
    source_url,
    scraped_at
  FROM official_trade_events
  WHERE (? = 0 OR event_year = ?)
    AND event_text_norm LIKE ?
  ORDER BY event_date DESC, id DESC
  LIMIT 300
|}

let player_external_links_by_player_id = (string ->* player_external_link) {|
  WITH base AS (
    SELECT canonical_player_id AS canonical_id
    FROM player_identities
    WHERE player_id = $1
  ),
  alias_ids AS (
    SELECT player_id
    FROM player_identities
    WHERE canonical_player_id = (SELECT canonical_id FROM base)
  )
  SELECT
    (SELECT canonical_id FROM base) AS player_id,
    link_type,
    url,
    source_url,
    scraped_at
  FROM player_external_links
  WHERE player_id IN (SELECT player_id FROM alias_ids)
  ORDER BY link_type ASC
|}

(** Season stats SQL helper — generates query for per_game/totals/per_36 modes.
    Eliminates duplication across 3 nearly-identical 85-line queries. *)
let make_season_stats_sql mode =
  let stat_cols = match mode with
    | `Per_game ->
      {|COALESCE(AVG(s.pts), 0),
    COALESCE(AVG(s.reb_tot), 0),
    COALESCE(AVG(s.ast), 0),
    COALESCE(AVG(s.stl), 0),
    COALESCE(AVG(s.blk), 0),
    COALESCE(AVG(s.tov), 0),
    COALESCE(AVG(s.game_score), 0)|}
    | `Totals ->
      {|COALESCE(SUM(s.pts), 0),
    COALESCE(SUM(s.reb_tot), 0),
    COALESCE(SUM(s.ast), 0),
    COALESCE(SUM(s.stl), 0),
    COALESCE(SUM(s.blk), 0),
    COALESCE(SUM(s.tov), 0),
    COALESCE(SUM(s.game_score), 0)|}
    | `Per_36 ->
      {|(SUM(s.pts) * 2160.0 / SUM(s.min_seconds)),
    (SUM(s.reb_tot) * 2160.0 / SUM(s.min_seconds)),
    (SUM(s.ast) * 2160.0 / SUM(s.min_seconds)),
    (SUM(s.stl) * 2160.0 / SUM(s.min_seconds)),
    (SUM(s.blk) * 2160.0 / SUM(s.min_seconds)),
    (SUM(s.tov) * 2160.0 / SUM(s.min_seconds)),
    (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds))|}
  in
  let having_clause = match mode with
    | `Per_36 -> "\n    HAVING SUM(s.min_seconds) > 0"
    | `Per_game | `Totals -> ""
  in
  Printf.sprintf {sql|
  WITH pid AS (
    SELECT ? AS player_id
  ),
  canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
      (SELECT player_id FROM pid)
    ) AS canonical_player_id
  )
  SELECT
    g.season_code,
    se.season_name,
    COALESCE((SELECT t.team_name_kr FROM teams t
      WHERE t.team_code = (
        SELECT sub.team_code FROM game_stats_clean sub
        JOIN player_identities sub_pi ON sub_pi.player_id = sub.player_id
        JOIN games_calc_v3 sub_g ON sub.game_id = sub_g.game_id
        WHERE sub_pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
          AND sub_g.season_code = g.season_code
          AND sub_g.game_type != '10'
        GROUP BY sub.team_code
        ORDER BY COUNT(*) DESC
        LIMIT 1
      )), '') as team_name,
    COUNT(DISTINCT s.game_id) as gp,
    COALESCE(SUM(s.min_seconds) / 60.0, 0),
    %s,
    COALESCE(
      (SUM(
        CASE
          WHEN g.home_score_calc IS NOT NULL
            AND g.away_score_calc IS NOT NULL
            AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
          THEN (
            (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
            -
            (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
          ) * s.min_seconds
          ELSE 0
        END
      ) * 1.0) / NULLIF(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ),
      0
    ) as margin,
    CASE WHEN SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0)) > 0
      THEN CAST(SUM(COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0)) AS REAL) / SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0))
      ELSE 0.0 END as fg_pct,
    CASE WHEN SUM(COALESCE(s.fg_3p_a, 0)) > 0
      THEN CAST(SUM(COALESCE(s.fg_3p_m, 0)) AS REAL) / SUM(COALESCE(s.fg_3p_a, 0))
      ELSE 0.0 END as fg3_pct,
    CASE WHEN SUM(COALESCE(s.ft_a, 0)) > 0
      THEN CAST(SUM(COALESCE(s.ft_m, 0)) AS REAL) / SUM(COALESCE(s.ft_a, 0))
      ELSE 0.0 END as ft_pct,
    CASE WHEN SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0)) + 0.44 * SUM(COALESCE(s.ft_a, 0)) > 0
      THEN CAST(SUM(s.pts) AS REAL) / (2.0 * (SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0)) + 0.44 * SUM(COALESCE(s.ft_a, 0))))
      ELSE 0.0 END as ts_pct,
    CASE WHEN SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0)) > 0
      THEN (CAST(SUM(COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0)) AS REAL) + 0.5 * SUM(COALESCE(s.fg_3p_m, 0))) / SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0))
      ELSE 0.0 END as efg_pct,
    CASE WHEN SUM(s.min_seconds) > 0
      THEN (SUM(COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0)) + 0.44 * SUM(COALESCE(s.ft_a, 0)) + SUM(COALESCE(s.tov, 0))) * 2160.0 / SUM(s.min_seconds)
      ELSE 0.0 END as usg_pct
  FROM game_stats_clean s
  JOIN player_identities pi ON pi.player_id = s.player_id
  JOIN games_calc_v3 g ON s.game_id = g.game_id
  JOIN seasons se ON g.season_code = se.season_code
  WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
    AND g.game_type != '10'
  GROUP BY g.season_code, se.season_name%s
  ORDER BY g.season_code DESC
|sql} stat_cols having_clause

(** Queries for Per Game (Average) *)
let player_seasons_per_game = (string ->* season_stats) (make_season_stats_sql `Per_game)

(** Queries for Totals (Sum) *)
let player_seasons_totals = (string ->* season_stats) (make_season_stats_sql `Totals)

(** Queries for Per 36 Minutes (Normalized) *)
let player_seasons_per36 = (string ->* season_stats) (make_season_stats_sql `Per_36)

	  let player_recent_games = (string ->* player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    ),
	    sums AS (
	      SELECT game_id, team_code, SUM(pts) AS pts_sum
	      FROM game_stats_clean
	      GROUP BY game_id, team_code
	    )
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.ft_m, 0),
		      COALESCE(s.ft_a, 0),
		      s.pts,
		      s.reb_tot,
		      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      pm.plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
	    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
	    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY g.game_date DESC
	    LIMIT 10
	  |}
	  let player_game_logs = (t2 string (t2 string (t2 string int)) ->* player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    ),
	    sums AS (
	      SELECT game_id, team_code, SUM(pts) AS pts_sum
	      FROM game_stats_clean
	      GROUP BY game_id, team_code
	    )
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.ft_m, 0),
		      COALESCE(s.ft_a, 0),
		      s.pts,
		      s.reb_tot,
		      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      pm.plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
	    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
	    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    ORDER BY g.game_date DESC
	  |}
	  (** Batch query for multiple players - returns player_id with each row *)
	  let batch_player_game_logs = (t2 string (t2 string string) ->* player_game_stat_with_id) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats_clean
		      GROUP BY game_id, team_code
	    )
	    SELECT
	      pi.canonical_player_id,
	      g.game_id,
	      g.game_date,
	      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.ft_m, 0),
		      COALESCE(s.ft_a, 0),
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
	      s.tov,
	      pm.plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
	    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
	    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	    WHERE pi.canonical_player_id = ANY(string_to_array(?, ','))
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	      AND g.game_id NOT IN (SELECT game_id FROM score_mismatch_games)
	    ORDER BY pi.canonical_player_id, g.game_date DESC
	  |}
	  let player_all_star_games = (string ->* player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    ),
	    sums AS (
	      SELECT game_id, team_code, SUM(pts) AS pts_sum
	      FROM game_stats_clean
	      GROUP BY game_id, team_code
	    )
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.fg_3p_m, 0),
		      COALESCE(s.fg_3p_a, 0),
		      COALESCE(s.ft_m, 0),
		      COALESCE(s.ft_a, 0),
		      s.pts,
		      s.reb_tot,
		      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      pm.plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
	    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
	    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type = '10'
	    ORDER BY g.game_date DESC
	  |}
	  let player_team_games = (string ->* (t2 string string)) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    )
	    SELECT
	      g.game_date,
	      t.team_name_kr
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY g.game_date ASC
	  |}

(** Career Highs Queries (best single game per category) *)
	  let career_high_points_game = (string ->? player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    )
	    SELECT
	      g.game_id,
	      COALESCE(g.game_date::text, 'Unknown'),
	      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
	      NULL as team_score,
	      NULL as opponent_score,
	      1 as score_quality,
	      COALESCE(s.min_seconds, 0) / 60.0,
	      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.ft_m, 0),
	      COALESCE(s.ft_a, 0),
	      s.pts,
	      s.reb_tot,
	      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      NULL as plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY s.pts DESC, g.game_date DESC, g.game_id DESC
	    LIMIT 1
	  |}
	  let career_high_rebounds_game = (string ->? player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    )
	    SELECT
	      g.game_id,
	      COALESCE(g.game_date::text, 'Unknown'),
	      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
	      NULL as team_score,
	      NULL as opponent_score,
	      1 as score_quality,
	      COALESCE(s.min_seconds, 0) / 60.0,
	      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.ft_m, 0),
	      COALESCE(s.ft_a, 0),
	      s.pts,
	      s.reb_tot,
	      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      NULL as plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY s.reb_tot DESC, g.game_date DESC, g.game_id DESC
	    LIMIT 1
	  |}
	  let career_high_assists_game = (string ->? player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    )
	    SELECT
	      g.game_id,
	      COALESCE(g.game_date::text, 'Unknown'),
	      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
	      NULL as team_score,
	      NULL as opponent_score,
	      1 as score_quality,
	      COALESCE(s.min_seconds, 0) / 60.0,
	      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.ft_m, 0),
	      COALESCE(s.ft_a, 0),
	      s.pts,
	      s.reb_tot,
	      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      NULL as plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY s.ast DESC, g.game_date DESC, g.game_id DESC
	    LIMIT 1
	  |}
	  let career_high_steals_game = (string ->? player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    )
	    SELECT
	      g.game_id,
	      COALESCE(g.game_date::text, 'Unknown'),
	      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
	      NULL as team_score,
	      NULL as opponent_score,
	      1 as score_quality,
	      COALESCE(s.min_seconds, 0) / 60.0,
	      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.ft_m, 0),
	      COALESCE(s.ft_a, 0),
	      s.pts,
	      s.reb_tot,
	      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      NULL as plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY s.stl DESC, g.game_date DESC, g.game_id DESC
	    LIMIT 1
	  |}
	  let career_high_blocks_game = (string ->? player_game_stat) {|
	    WITH pid AS (
	      SELECT ? AS player_id
	    ),
	    canon AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM pid)),
	        (SELECT player_id FROM pid)
	      ) AS canonical_player_id
	    )
	    SELECT
	      g.game_id,
	      COALESCE(g.game_date::text, 'Unknown'),
	      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
	      NULL as team_score,
	      NULL as opponent_score,
	      1 as score_quality,
	      COALESCE(s.min_seconds, 0) / 60.0,
	      COALESCE(s.fg_2p_m, 0) + COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_2p_a, 0) + COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.fg_3p_m, 0),
	      COALESCE(s.fg_3p_a, 0),
	      COALESCE(s.ft_m, 0),
	      COALESCE(s.ft_a, 0),
	      s.pts,
	      s.reb_tot,
	      s.ast,
	      s.stl,
	      s.blk,
	      s.tov,
	      NULL as plus_minus
	    FROM game_stats_clean s
	    JOIN player_identities pi ON pi.player_id = s.player_id
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
	      AND g.game_type != '10'
	    ORDER BY s.blk DESC, g.game_date DESC, g.game_id DESC
	    LIMIT 1
	  |}

		  let team_recent_games = (let t = t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string string)))))))) in t ->* team_game_result) {|
		    SELECT
		      g.game_id,
		      g.game_date,
	      CASE WHEN t1.team_name_kr = ? THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN t1.team_name_kr = ? THEN 1 ELSE 0 END as is_home,
	      CASE WHEN t1.team_name_kr = ? THEN g.home_score_calc ELSE g.away_score_calc END as team_score,
	      CASE WHEN t1.team_name_kr = ? THEN g.away_score_calc ELSE g.home_score_calc END as opp_score,
	      CASE
	        WHEN (t1.team_name_kr = ? AND g.home_score_calc > g.away_score_calc)
	          OR (t2.team_name_kr = ? AND g.away_score_calc > g.home_score_calc)
	        THEN 1
	        ELSE 0
	      END as is_win
	    FROM games_calc_v3 g
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE (t1.team_name_kr = ? OR t2.team_name_kr = ?)
	      AND g.game_type != '10'
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.home_score_calc IS NOT NULL
	      AND g.away_score_calc IS NOT NULL
		    ORDER BY g.game_date DESC
		    LIMIT 10
		  |}
		  let team_games = (let t = t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string string)))))))) in t ->* team_game_result) {|
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN t1.team_name_kr = ? THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN t1.team_name_kr = ? THEN 1 ELSE 0 END as is_home,
		      CASE WHEN t1.team_name_kr = ? THEN g.home_score_calc ELSE g.away_score_calc END as team_score,
		      CASE WHEN t1.team_name_kr = ? THEN g.away_score_calc ELSE g.home_score_calc END as opp_score,
		      CASE
		        WHEN (t1.team_name_kr = ? AND g.home_score_calc > g.away_score_calc)
		          OR (t2.team_name_kr = ? AND g.away_score_calc > g.home_score_calc)
		        THEN 1
		        ELSE 0
		      END as is_win
		    FROM games_calc_v3 g
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    WHERE (t1.team_name_kr = ? OR t2.team_name_kr = ?)
		      AND g.game_type != '10'
		      AND (? = 'ALL' OR g.season_code = ?)
		      AND g.home_score_calc IS NOT NULL
		      AND g.away_score_calc IS NOT NULL
		    ORDER BY g.game_date ASC, g.game_id ASC
		  |}
		  let player_by_name = (t2 string (t2 string string) ->? player_aggregate) {|
		    SELECT
		      p.player_id,
		      p.player_name,
	      t.team_name_kr,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      COALESCE(SUM(s.pts), 0),
	      COALESCE(SUM(s.reb_tot), 0),
	      COALESCE(SUM(s.ast), 0),
	      COALESCE(SUM(s.stl), 0),
	      COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
	      COALESCE(AVG(s.pts), 0),
	      COALESCE(
	        (SUM(
	          CASE
	            WHEN g.home_score_calc IS NOT NULL
	              AND g.away_score_calc IS NOT NULL
	              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
	            THEN (
	              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
	              -
	              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
	            ) * s.min_seconds
	            ELSE 0
	          END
	        ) * 1.0) / NULLIF(
	          SUM(
	            CASE
	              WHEN g.home_score_calc IS NOT NULL
	                AND g.away_score_calc IS NOT NULL
	                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
	              THEN s.min_seconds
	              ELSE 0
	            END
	          ),
	          0
	        ),
	        0
	      ) as margin,
	      COALESCE(AVG(s.reb_tot), 0),
	      COALESCE(AVG(s.ast), 0),
	      COALESCE(AVG(s.stl), 0),
	      COALESCE(AVG(s.blk), 0),
	      COALESCE(AVG(s.tov), 0),
	      COALESCE(AVG(s.game_score), 0),
	      COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
	      COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
	      COALESCE(SUM(s.fg_3p_m), 0),
	      COALESCE(SUM(s.fg_3p_a), 0),
	      COALESCE(SUM(s.ft_m), 0),
	      COALESCE(SUM(s.ft_a), 0)
	    FROM game_stats_clean s
	    JOIN players p ON s.player_id = p.player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games_calc_v3 g ON g.game_id = s.game_id
	    WHERE p.player_name = ?
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	    GROUP BY p.player_id
	  |}
let player_h2h_games = (t2 (t2 string string) (t2 string string) ->* h2h_game) {|
	    WITH p1 AS (
	      SELECT ? AS player_id
	    ),
	    p2 AS (
	      SELECT ? AS player_id
	    ),
	    canon1 AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM p1)),
	        (SELECT player_id FROM p1)
	      ) AS canonical_player_id
	    ),
	    canon2 AS (
	      SELECT COALESCE(
	        (SELECT canonical_player_id FROM player_identities WHERE player_id = (SELECT player_id FROM p2)),
	        (SELECT player_id FROM p2)
	      ) AS canonical_player_id
	    ),
	    scored_games AS (
	      SELECT
	        g.*,
	        COALESCE(
	          NULLIF(g.home_score, 0),
	          (SELECT SUM(sx.pts) FROM game_stats_clean sx WHERE sx.game_id = g.game_id AND sx.team_code = g.home_team_code)
	        ) AS home_score_calc,
	        COALESCE(
	          NULLIF(g.away_score, 0),
	          (SELECT SUM(sx.pts) FROM game_stats_clean sx WHERE sx.game_id = g.game_id AND sx.team_code = g.away_team_code)
	        ) AS away_score_calc
	      FROM games g
	    )
  SELECT
    g.game_id,
    g.game_date,
    t1.team_name_kr as p1_team,
    t2.team_name_kr as p2_team,
    s1.pts as p1_pts,
    s1.reb_tot as p1_reb,
    s1.ast as p1_ast,
    COALESCE(s1.stl, 0) as p1_stl,
    COALESCE(s1.blk, 0) as p1_blk,
    s2.pts as p2_pts,
    s2.reb_tot as p2_reb,
    s2.ast as p2_ast,
    COALESCE(s2.stl, 0) as p2_stl,
    COALESCE(s2.blk, 0) as p2_blk,
    CASE WHEN g.home_score_calc > g.away_score_calc THEN t1.team_name_kr ELSE t2.team_name_kr END as winner,
    COALESCE(ABS(g.home_score_calc - g.away_score_calc), 0) as diff
	    FROM scored_games g
	    JOIN game_stats_clean s1 ON g.game_id = s1.game_id
	    JOIN game_stats_clean s2 ON g.game_id = s2.game_id
	    JOIN player_identities pi1 ON pi1.player_id = s1.player_id
	    JOIN player_identities pi2 ON pi2.player_id = s2.player_id
	    JOIN teams t1 ON s1.team_code = t1.team_code
	    JOIN teams t2 ON s2.team_code = t2.team_code
	    WHERE pi1.canonical_player_id = (SELECT canonical_player_id FROM canon1)
	      AND pi2.canonical_player_id = (SELECT canonical_player_id FROM canon2)
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	      AND g.home_score_calc IS NOT NULL
	      AND g.away_score_calc IS NOT NULL
	    ORDER BY g.game_date DESC
|}

(* Team H2H: games between two specific teams *)
let team_h2h_games = (t2 (t2 (t2 string string) (t2 string string)) (t2 string string) ->* game_info) {|
	    WITH scored_games AS (
	      SELECT
	        g.*,
	        COALESCE(
	          NULLIF(g.home_score, 0),
	          (SELECT SUM(sx.pts) FROM game_stats_clean sx WHERE sx.game_id = g.game_id AND sx.team_code = g.home_team_code)
	        ) AS home_score_calc,
	        COALESCE(
	          NULLIF(g.away_score, 0),
	          (SELECT SUM(sx.pts) FROM game_stats_clean sx WHERE sx.game_id = g.game_id AND sx.team_code = g.away_team_code)
	        ) AS away_score_calc,
	        CASE
	          WHEN NULLIF(g.home_score, 0) IS NOT NULL AND NULLIF(g.away_score, 0) IS NOT NULL THEN 2
	          WHEN EXISTS (SELECT 1 FROM game_stats_clean gs WHERE gs.game_id = g.game_id) THEN 1
	          ELSE 0
	        END AS score_quality
	      FROM games g
	    )
  SELECT
    g.game_id,
    g.game_date,
    g.home_team_code,
    th.team_name_kr AS home_team_name,
    g.away_team_code,
    ta.team_name_kr AS away_team_name,
    COALESCE(g.home_score_calc, 0) AS home_score,
    COALESCE(g.away_score_calc, 0) AS away_score,
    g.score_quality
  FROM scored_games g
  JOIN teams th ON g.home_team_code = th.team_code
  JOIN teams ta ON g.away_team_code = ta.team_code
  WHERE (
    (g.home_team_code = ? AND g.away_team_code = ?)
    OR (g.home_team_code = ? AND g.away_team_code = ?)
  )
  AND (? = 'ALL' OR g.season_code = ?)
  AND g.game_type != '10'
  AND g.home_score_calc IS NOT NULL
  AND g.away_score_calc IS NOT NULL
  ORDER BY g.game_date DESC
|}

(* ===== Schedule Table Queries ===== *)
let ensure_schedule_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS schedule (
    schedule_id SERIAL PRIMARY KEY,
    game_date TEXT NOT NULL,
    game_time TEXT,
    season_code TEXT NOT NULL,
    home_team_code TEXT NOT NULL,
    away_team_code TEXT NOT NULL,
    venue TEXT,
    status TEXT DEFAULT 'scheduled',
    game_id INTEGER,
    UNIQUE(game_date, home_team_code, away_team_code)
  )
|}
let ensure_schedule_index_date = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_schedule_date ON schedule(game_date)
|}
let ensure_schedule_index_status = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_schedule_status ON schedule(status)
|}
let ensure_schedule_index_season = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_schedule_season ON schedule(season_code)
|}

let schedule_entry_row =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (schedule_id, (game_date, (game_time, (season_code, (home_team_code, (away_team_code, (home_team_name, (away_team_name, (venue, status))))))))) =
    Ok {
      Domain.sch_id = schedule_id;
      sch_game_date = game_date;
      sch_game_time = game_time;
      sch_season_code = season_code;
      sch_home_team_code = home_team_code;
      sch_away_team_code = away_team_code;
      sch_home_team_name = home_team_name;
      sch_away_team_name = away_team_name;
      sch_venue = venue;
      sch_status = status;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t int
      (t string
        (t (option string)
          (t string
            (t string
              (t string
                (t (option string)
                  (t (option string)
                    (t (option string) string)))))))))

let get_upcoming_schedule =
  (t2 string int ->* schedule_entry_row)
  {|
  SELECT
    s.schedule_id,
    s.game_date,
    s.game_time,
    s.season_code,
    s.home_team_code,
    s.away_team_code,
    th.team_name_kr AS home_team_name,
    ta.team_name_kr AS away_team_name,
    s.venue,
    s.status
  FROM schedule s
  LEFT JOIN teams th ON s.home_team_code = th.team_code
  LEFT JOIN teams ta ON s.away_team_code = ta.team_code
  WHERE s.status = ?
    AND s.game_date >= to_char(CURRENT_DATE, 'YYYY-MM-DD')
  ORDER BY s.game_date ASC, s.game_time ASC
  LIMIT ?
|}

let get_schedule_by_date_range =
  let params = t2 string (t2 string (t2 string string)) in
  (params ->* schedule_entry_row)
  {|
  SELECT
    s.schedule_id,
    s.game_date,
    s.game_time,
    s.season_code,
    s.home_team_code,
    s.away_team_code,
    th.team_name_kr AS home_team_name,
    ta.team_name_kr AS away_team_name,
    s.venue,
    s.status
  FROM schedule s
  LEFT JOIN teams th ON s.home_team_code = th.team_code
  LEFT JOIN teams ta ON s.away_team_code = ta.team_code
  WHERE s.game_date >= ?
    AND s.game_date <= ?
    AND (? = 'ALL' OR s.status = ?)
  ORDER BY s.game_date ASC, s.game_time ASC
|}

(** MVP Race query - combines player stats with team standings *)
let mvp_race_candidates =
  let params = t2 string string in
  (params ->* mvp_candidate)
  {|
  WITH params AS (
    SELECT ? AS season
  ),
  team_records AS (
    SELECT
      CASE
        WHEN g.home_score > g.away_score THEN t_home.team_name_kr
        ELSE t_away.team_name_kr
      END AS winner,
      CASE
        WHEN g.home_score < g.away_score THEN t_home.team_name_kr
        ELSE t_away.team_name_kr
      END AS loser
    FROM games g
    JOIN teams t_home ON g.home_team_code = t_home.team_code
    JOIN teams t_away ON g.away_team_code = t_away.team_code
    CROSS JOIN params p
    WHERE g.home_score IS NOT NULL AND g.away_score IS NOT NULL
      AND (p.season = 'ALL' OR g.season_code = p.season)
  ),
  team_standings AS (
    SELECT
      team_name,
      SUM(CASE WHEN is_win THEN 1 ELSE 0 END) AS wins,
      SUM(CASE WHEN NOT is_win THEN 1 ELSE 0 END) AS losses
    FROM (
      SELECT winner AS team_name, TRUE AS is_win FROM team_records
      UNION ALL
      SELECT loser AS team_name, FALSE AS is_win FROM team_records
    ) combined
    GROUP BY team_name
  ),
  player_aggs AS (
    SELECT
      gs.player_id,
      p.player_name,
      t.team_name_kr AS team_name,
      COUNT(DISTINCT gs.game_id) AS gp,
      ROUND(CAST(SUM(gs.pts) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS ppg,
      ROUND(CAST(SUM(gs.reb_tot) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS rpg,
      ROUND(CAST(SUM(gs.ast) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS apg,
      ROUND(CAST(SUM(gs.stl) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS spg,
      ROUND(CAST(SUM(gs.blk) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS bpg,
      ROUND(
        CAST(SUM(gs.pts + gs.reb_tot + gs.ast + gs.stl + gs.blk - (gs.fg_2p_a + gs.fg_3p_a - gs.fg_2p_m - gs.fg_3p_m) - (gs.ft_a - gs.ft_m) - gs.tov) AS NUMERIC)
        / NULLIF(COUNT(DISTINCT gs.game_id), 0),
        1
      ) AS efficiency
    FROM game_stats_clean gs
    JOIN players p ON gs.player_id = p.player_id
    JOIN teams t ON gs.team_code = t.team_code
    JOIN games g ON gs.game_id = g.game_id
    CROSS JOIN params pp
    WHERE (pp.season = 'ALL' OR g.season_code = pp.season)
      AND gs.min_seconds > 0
    GROUP BY gs.player_id, p.player_name, t.team_name_kr
    HAVING COUNT(DISTINCT gs.game_id) >= 5
  )
  SELECT
    pa.player_id,
    pa.player_name,
    pa.team_name,
    pa.gp,
    pa.ppg,
    pa.rpg,
    pa.apg,
    pa.spg,
    pa.bpg,
    pa.efficiency,
    COALESCE(ts.wins, 0) AS team_wins,
    COALESCE(ts.losses, 0) AS team_losses,
    ROUND(
      CASE WHEN (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0)) > 0
        THEN CAST(COALESCE(ts.wins, 0) AS NUMERIC) / (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0))
        ELSE 0
      END,
      3
    ) AS team_win_pct
  FROM player_aggs pa
  LEFT JOIN team_standings ts ON pa.team_name = ts.team_name
  WHERE pa.gp >= ?
  ORDER BY
    (pa.ppg * 2 + pa.rpg * 1.2 + pa.apg * 1.5 + pa.spg * 2 + pa.bpg * 2 + pa.efficiency * 0.5
     + CASE WHEN (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0)) > 0
         THEN (CAST(COALESCE(ts.wins, 0) AS NUMERIC) / (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0))) * 20
         ELSE 0
       END) DESC
  LIMIT 20
|}

(** Get all Q4 PBP events for clutch time analysis
    Clutch time = Q4 + clock <= 5:00 + score diff <= 5 *)
let clutch_pbp_events = (string ->* pbp_event) {|
  SELECT
    period_code, event_index, team_side, description,
    team1_score, team2_score, clock
  FROM play_by_play_events p
  JOIN games g ON p.game_id = g.game_id
  WHERE period_code = 'Q4'
    AND (
      CASE
        WHEN clock ~ '^\d+:\d+$' THEN
          CAST(SPLIT_PART(clock, ':', 1) AS INTEGER) * 60 +
          CAST(SPLIT_PART(clock, ':', 2) AS INTEGER)
        ELSE 600
      END
    ) <= 300
    AND ABS(COALESCE(team1_score, 0) - COALESCE(team2_score, 0)) <= 5
    AND ($1 = 'ALL' OR g.season_code = $1)
  ORDER BY p.game_id, event_index
|}

(** Aggregated clutch time statistics per player
    Parses description for scoring events:
    - "N점슛성공" = field goal made (N=2 or 3)
    - "N점슛실패" = field goal attempted but missed
    - "자유투성공" = free throw made
    - "자유투실패" = free throw missed *)
	  let clutch_stats_by_season = (string ->* clutch_stats) {|
	    WITH clutch_events AS (
	      SELECT
	        COALESCE(pi.canonical_player_id, p.player_id) AS player_id,
	        p.game_id,
	        p.description,
	        CASE
	          WHEN p.description ~ '2점슛성공' THEN 2
        WHEN p.description ~ '3점슛성공' THEN 3
        WHEN p.description ~ '자유투성공' THEN 1
        ELSE 0
      END AS points_scored,
      CASE WHEN p.description ~ '[23]점슛성공' THEN 1 ELSE 0 END AS fg_made,
      CASE WHEN p.description ~ '[23]점슛' THEN 1 ELSE 0 END AS fg_att,
	        CASE WHEN p.description ~ '3점슛성공' THEN 1 ELSE 0 END AS three_made,
	        CASE WHEN p.description ~ '자유투성공' THEN 1 ELSE 0 END AS ft_made,
	        CASE WHEN p.description ~ '자유투' THEN 1 ELSE 0 END AS ft_att
	      FROM play_by_play_events p
	      LEFT JOIN player_identities pi ON pi.player_id = p.player_id
	      JOIN games g ON p.game_id = g.game_id
	      WHERE p.period_code = 'Q4'
	        AND p.player_id IS NOT NULL
      AND (
        CASE
          WHEN p.clock ~ '^\d+:\d+$' THEN
            CAST(SPLIT_PART(p.clock, ':', 1) AS INTEGER) * 60 +
            CAST(SPLIT_PART(p.clock, ':', 2) AS INTEGER)
          ELSE 600
        END
      ) <= 300
      AND ABS(COALESCE(p.team1_score, 0) - COALESCE(p.team2_score, 0)) <= 5
      AND ($1 = 'ALL' OR g.season_code = $1)
      AND g.game_type != '10'
  )
  SELECT
    ce.player_id,
    pl.player_name,
    COALESCE(t.team_name_kr, 'Unknown') AS team_name,
    COUNT(DISTINCT ce.game_id) AS clutch_games,
    SUM(ce.points_scored) AS clutch_points,
    SUM(ce.fg_made) AS clutch_fg_made,
    SUM(ce.fg_att) AS clutch_fg_att,
    CASE
      WHEN SUM(ce.fg_att) > 0
      THEN CAST(SUM(ce.fg_made) AS FLOAT) / SUM(ce.fg_att)
      ELSE 0.0
    END AS clutch_fg_pct,
    SUM(ce.ft_made) AS clutch_ft_made,
    SUM(ce.ft_att) AS clutch_ft_att,
    SUM(ce.three_made) AS clutch_3p_made
	    FROM clutch_events ce
	    JOIN players pl ON ce.player_id = pl.player_id
	    LEFT JOIN (
	      SELECT DISTINCT ON (pi2.canonical_player_id)
	        pi2.canonical_player_id AS player_id,
	        gs.team_code
	      FROM game_stats_clean gs
	      JOIN player_identities pi2 ON pi2.player_id = gs.player_id
	      JOIN games g2 ON gs.game_id = g2.game_id
	      WHERE ($1 = 'ALL' OR g2.season_code = $1)
	      ORDER BY pi2.canonical_player_id, g2.game_date DESC, g2.game_id DESC
	    ) latest_team ON ce.player_id = latest_team.player_id
	    LEFT JOIN teams t ON latest_team.team_code = t.team_code
	    GROUP BY ce.player_id, pl.player_name, t.team_name_kr
	    HAVING SUM(ce.points_scored) > 0 OR SUM(ce.fg_att) > 0 OR SUM(ce.ft_att) > 0
	    ORDER BY SUM(ce.points_scored) DESC, clutch_fg_pct DESC
	    LIMIT 50
	  |}

(* On/Off Impact Queries *)
let on_off_impact_stats = (t2 string string ->* (t2 string (t2 string (t2 string (t2 int (t2 int (t2 int int))))))) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(DISTINCT s.game_id) as games_played,
    COALESCE(SUM(s.min_seconds), 0) as total_min_seconds,
    COALESCE(SUM(pm.plus_minus), 0) as total_plus_minus,
    COUNT(pm.plus_minus) as games_with_pm
  FROM game_stats_clean s
  JOIN players p ON s.player_id = p.player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
  WHERE ($1 = 'ALL' OR g.season_code = $1)
    AND g.game_type != '10'
    AND s.min_seconds > 0
  GROUP BY p.player_id, p.player_name, t.team_name_kr
  HAVING COUNT(DISTINCT s.game_id) >= 5
    AND COALESCE(SUM(s.min_seconds), 0) / 60.0 >= 50
  ORDER BY
    CASE WHEN COUNT(pm.plus_minus) > 0
      THEN CAST(COALESCE(SUM(pm.plus_minus), 0) AS FLOAT) / COUNT(pm.plus_minus)
      ELSE -999
    END DESC
  LIMIT CASE WHEN $2 = '' THEN 50 ELSE CAST($2 AS INTEGER) END
|}

let on_off_impact_for_player = (t2 string string ->? (t2 string (t2 string (t2 string (t2 int (t2 int (t2 int int))))))) {|
  SELECT
    p.player_id,
    p.player_name,
    t.team_name_kr,
    COUNT(DISTINCT s.game_id) as games_played,
    COALESCE(SUM(s.min_seconds), 0) as total_min_seconds,
    COALESCE(SUM(pm.plus_minus), 0) as total_plus_minus,
    COUNT(pm.plus_minus) as games_with_pm
  FROM game_stats_clean s
  JOIN players p ON s.player_id = p.player_id
  JOIN teams t ON s.team_code = t.team_code
  JOIN games g ON g.game_id = s.game_id
  LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
  WHERE s.player_id = $1
    AND ($2 = 'ALL' OR g.season_code = $2)
    AND g.game_type != '10'
    AND s.min_seconds > 0
  GROUP BY p.player_id, p.player_name, t.team_name_kr
|}

(* Shot chart: Get aggregated shot stats by zone for a player *)
let player_shot_stats = (t2 string string ->* t4 string int int float) {|
  WITH canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = $1),
      $1
    ) AS canonical_player_id
  )
  SELECT
    CASE
      WHEN description LIKE '%페인트존%' THEN 'paint'
      WHEN description LIKE '%3점슛%' THEN 'three'
      WHEN description LIKE '%2점슛%' THEN 'mid'
      ELSE 'other'
    END as zone,
    SUM(CASE WHEN description LIKE '%성공%' THEN 1 ELSE 0 END)::int as made,
    COUNT(*)::int as attempts,
    ROUND(
      SUM(CASE WHEN description LIKE '%성공%' THEN 1 ELSE 0 END)::numeric /
      NULLIF(COUNT(*), 0) * 100, 1
    )::float as pct
  FROM play_by_play_events p
  LEFT JOIN player_identities pi ON pi.player_id = p.player_id
  JOIN games g ON p.game_id = g.game_id
  WHERE COALESCE(pi.canonical_player_id, p.player_id) = (SELECT canonical_player_id FROM canon)
    AND p.description LIKE '%슛%'
    AND ($2 = 'ALL' OR g.season_code = $2)
  GROUP BY 1
  ORDER BY 1
|}

(* Shot chart: Get player info for shot chart header *)
let player_shot_info = (string ->? t3 string string string) {|
  WITH canon AS (
    SELECT COALESCE(
      (SELECT canonical_player_id FROM player_identities WHERE player_id = $1),
      $1
    ) AS canonical_player_id
  ),
  latest_gs AS (
    SELECT
      pi.canonical_player_id AS player_id,
      gs.team_code,
      g.season_code,
      g.game_date,
      g.game_id
    FROM game_stats_clean gs
    JOIN player_identities pi ON pi.player_id = gs.player_id
    JOIN games g ON gs.game_id = g.game_id
    WHERE pi.canonical_player_id = (SELECT canonical_player_id FROM canon)
    ORDER BY g.season_code DESC, g.game_date DESC, g.game_id DESC
    LIMIT 1
  )
  SELECT
    p.player_id,
    p.player_name,
    COALESCE(t.team_name_kr, 'Unknown')
  FROM players p
  LEFT JOIN latest_gs ON p.player_id = latest_gs.player_id
  LEFT JOIN teams t ON latest_gs.team_code = t.team_code
  WHERE p.player_id = (SELECT canonical_player_id FROM canon)
  LIMIT 1
|}

(** Quarter scores from PBP for game flow analysis
    Note: In PBP data, team2 = HOME, team1 = AWAY (verified pattern) *)
let quarter_scores_by_game = (string ->* t3 string int int) {|
  SELECT period_code,
         COALESCE(MAX(team2_score), 0) as home_score,
         COALESCE(MAX(team1_score), 0) as away_score
  FROM play_by_play_events
  WHERE game_id = ? AND team1_score IS NOT NULL
  GROUP BY period_code
  ORDER BY period_code
|}

(** PBP data quality verification
    Returns: (pattern, count) where pattern is T2=HOME, SCORE_MISSING, or MISMATCH
    Note: Properly handles NULL scores (common in older seasons) *)
let pbp_data_quality = (unit ->* t2 string int) {|
  WITH pbp_final AS (
    SELECT game_id, MAX(team1_score) as t1, MAX(team2_score) as t2
    FROM play_by_play_events WHERE team1_score IS NOT NULL
    GROUP BY game_id
  ),
  comparison AS (
    SELECT
      CASE
        WHEN g.home_score IS NULL OR g.away_score IS NULL
             OR g.home_score = 0 OR g.away_score = 0 THEN 'SCORE_MISSING'
        WHEN g.home_score = p.t2 AND g.away_score = p.t1 THEN 'T2=HOME'
        ELSE 'MISMATCH'
      END as pattern
    FROM games g JOIN pbp_final p ON g.game_id = p.game_id
  )
  SELECT pattern, COUNT(*)::int as cnt FROM comparison GROUP BY pattern ORDER BY cnt DESC
|}

(** Games with incomplete PBP data (for re-scraping) *)
let games_with_incomplete_pbp = (unit ->* t4 string string string string) {|
  WITH sums AS (
    SELECT game_id, team_code, SUM(pts) AS pts_sum
    FROM game_stats_clean
    GROUP BY game_id, team_code
  ),
  pbp_final AS (
    SELECT game_id, MAX(team1_score) as t1, MAX(team2_score) as t2
    FROM play_by_play_events WHERE team1_score IS NOT NULL
    GROUP BY game_id
  ),
  scored_games AS (
    SELECT
      g.game_id,
      g.season_code,
      g.game_date,
      COALESCE(NULLIF(g.home_score, 0), sh.pts_sum) AS home_score_calc,
      COALESCE(NULLIF(g.away_score, 0), sa.pts_sum) AS away_score_calc
    FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
  )
  SELECT g.game_id, g.season_code, g.game_date::text,
         CASE
           WHEN g.home_score_calc IS NULL OR g.away_score_calc IS NULL THEN 'SCORE_MISSING'
           ELSE 'PBP_INCOMPLETE'
         END as issue_type
  FROM scored_games g JOIN pbp_final p ON g.game_id = p.game_id
  WHERE NOT (g.home_score_calc = p.t2 AND g.away_score_calc = p.t1)
    AND g.home_score_calc > 0 AND g.away_score_calc > 0
  ORDER BY g.season_code DESC, g.game_date DESC
  LIMIT 100
|}

(** UPSERT schedule entry - inserts or updates based on unique constraint *)
let upsert_schedule =
  (t2 string                    (* game_date *)
    (t2 (option string)         (* game_time *)
      (t2 string                (* season_code *)
        (t2 string              (* home_team_code *)
          (t2 string            (* away_team_code *)
            (t2 (option string) (* venue *)
              string            (* status: 'scheduled' or 'completed' *)
            ))))) ->. unit) {|
  INSERT INTO schedule (game_date, game_time, season_code, home_team_code, away_team_code, venue, status)
  VALUES ($1, $2, $3, $4, $5, $6, $7)
  ON CONFLICT (game_date, home_team_code, away_team_code)
  DO UPDATE SET
    game_time = COALESCE(EXCLUDED.game_time, schedule.game_time),
    venue = COALESCE(EXCLUDED.venue, schedule.venue),
    status = EXCLUDED.status
|}

(** UPSERT season info *)
let upsert_season = (t2 string string ->. unit) {|
  INSERT INTO seasons (season_code, season_name)
  VALUES ($1, $2)
  ON CONFLICT (season_code)
  DO UPDATE SET season_name = EXCLUDED.season_name
|}

(** UPSERT game row from schedule/results scrape *)
let upsert_game =
  (t2 string                    (* game_id *)
    (t2 string                  (* season_code *)
      (t2 string                (* game_type *)
        (t2 int                 (* game_no *)
          (t2 (option string)   (* game_date: YYYY-MM-DD *)
            (t2 string          (* home_team_code *)
              (t2 string        (* away_team_code *)
                (t2 (option int) (* home_score *)
                  (t2 (option int) (* away_score *)
                    (t2 (option string) (* stadium *)
                      (option int) (* attendance *)
                    ))))))))) ->. unit) {|
  INSERT INTO games
    (game_id, season_code, game_type, game_no, game_date, home_team_code, away_team_code, home_score, away_score, stadium, attendance)
  VALUES
    ($1, $2, $3, $4, $5::date, $6, $7, $8, $9, $10, $11)
  ON CONFLICT (game_id)
  DO UPDATE SET
    season_code = EXCLUDED.season_code,
    game_type = EXCLUDED.game_type,
    game_no = EXCLUDED.game_no,
    game_date = COALESCE(EXCLUDED.game_date, games.game_date),
    home_team_code = COALESCE(EXCLUDED.home_team_code, games.home_team_code),
    away_team_code = COALESCE(EXCLUDED.away_team_code, games.away_team_code),
    -- Treat 0 as a placeholder score (schedule pages often show 0-0).
    home_score = COALESCE(NULLIF(EXCLUDED.home_score, 0), NULLIF(games.home_score, 0)),
    away_score = COALESCE(NULLIF(EXCLUDED.away_score, 0), NULLIF(games.away_score, 0)),
    stadium = COALESCE(EXCLUDED.stadium, games.stadium),
    attendance = COALESCE(EXCLUDED.attendance, games.attendance)
|}
(** Count schedule entries by season and status *)
let count_schedule_by_status = (t2 string string ->? int) {|
  SELECT COUNT(*) FROM schedule WHERE season_code = $1 AND status = $2
|}

(** Delete schedule entries for a season (derived data, safe to rebuild) *)
let delete_schedule_by_season = (string ->. unit) {|
  DELETE FROM schedule WHERE season_code = $1
|}

(* ===== Awards Table Queries ===== *)

let ensure_awards_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS awards (
    id SERIAL PRIMARY KEY,
    season_name TEXT NOT NULL,
    category TEXT NOT NULL,
    award_type TEXT NOT NULL,
    player_name TEXT NOT NULL,
    stat_value TEXT,
    votes TEXT,
    UNIQUE(season_name, category, player_name)
  )
|}

let ensure_awards_index_season = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_awards_season ON awards(season_name)
|}

let ensure_awards_index_category = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_awards_category ON awards(category)
|}

let ensure_awards_index_player = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_awards_player ON awards(player_name)
|}

let upsert_award = (t2 string (t2 string (t2 string (t2 string (t2 (option string) (option string))))) ->. unit) {|
  INSERT INTO awards (season_name, category, award_type, player_name, stat_value, votes)
  VALUES (?, ?, ?, ?, ?, ?)
  ON CONFLICT (season_name, category, player_name)
  DO UPDATE SET
    award_type = EXCLUDED.award_type,
    stat_value = COALESCE(EXCLUDED.stat_value, awards.stat_value),
    votes = COALESCE(EXCLUDED.votes, awards.votes)
|}

(** Query awards by category *)
let awards_by_category = (string ->* t2 string (t2 string (t2 string (t2 (option string) (option string))))) {|
  SELECT season_name, category, player_name, stat_value, votes
  FROM awards
  WHERE category = ?
  ORDER BY season_name DESC
|}

(** Query awards by season *)
let awards_by_season = (string ->* t2 string (t2 string (t2 string (t2 (option string) (option string))))) {|
  SELECT season_name, category, player_name, stat_value, votes
  FROM awards
  WHERE season_name = ?
  ORDER BY category
|}

(** Query awards by player name (partial match) *)
let awards_by_player = (string ->* t2 string (t2 string (t2 string (t2 (option string) (option string))))) {|
  SELECT season_name, category, player_name, stat_value, votes
  FROM awards
  WHERE player_name LIKE '%' || ? || '%'
  ORDER BY season_name DESC, category
|}

(** Count total awards *)
let count_awards = (unit ->? int) {|
  SELECT COUNT(*) FROM awards
|}

(** Get distinct award winners with win counts *)
let award_leaders = (string ->* t2 string int) {|
  SELECT player_name, COUNT(*) as cnt
  FROM awards
  WHERE category = ?
  GROUP BY player_name
  ORDER BY cnt DESC
  LIMIT 20
|}
