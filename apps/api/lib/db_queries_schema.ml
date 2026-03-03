(** Auto-generated from db_queries.ml split. *)

open Db_request
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


let ensure_coaches_table = (unit ->. unit) {|
  CREATE TABLE IF NOT EXISTS coaches (
    coach_id SERIAL PRIMARY KEY,
    coach_name VARCHAR(50) NOT NULL,
    team VARCHAR(50),
    tenure_start INTEGER,
    tenure_end INTEGER,
    championships INTEGER DEFAULT 0,
    regular_season_wins INTEGER DEFAULT 0,
    playoff_wins INTEGER DEFAULT 0,
    former_player BOOLEAN DEFAULT FALSE,
    player_career_years VARCHAR(50),
    notable_achievements TEXT,
    created_at TIMESTAMP DEFAULT NOW()
  )
|}


let ensure_coaches_index_team = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_coaches_team ON coaches(team)
|}


let ensure_coaches_index_name = (unit ->. unit) {|
  CREATE INDEX IF NOT EXISTS idx_coaches_name ON coaches(coach_name)
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
  DO $$
  DECLARE v_relkind char;
  BEGIN
    SELECT relkind INTO v_relkind FROM pg_class
    WHERE relname = 'score_mismatch_games'
      AND relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = 'public');
    IF v_relkind = 'v' THEN
      EXECUTE 'DROP VIEW score_mismatch_games CASCADE';
    ELSIF v_relkind = 'm' THEN
      EXECUTE 'DROP MATERIALIZED VIEW score_mismatch_games CASCADE';
    END IF;
  END $$
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

