(** Auto-generated from db_queries.ml split. *)

open Db_request
open Db_types
open Caqti_type

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

