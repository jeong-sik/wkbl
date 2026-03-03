(** Auto-generated from db_queries.ml split. *)

open Db_request
open Db_types
open Caqti_type

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


let team_available_seasons = (t4 string string string string ->* season_info) {|
  SELECT DISTINCT
    g.season_code,
    COALESCE(s.season_name, g.season_code) as season_name
  FROM games_calc_v3 g
  LEFT JOIN seasons s ON s.season_code = g.season_code
  WHERE g.game_type != '10'
    AND (
      g.home_team_code = ?
      OR g.away_team_code = ?
      OR g.home_team_name = ?
      OR g.away_team_name = ?
    )
  ORDER BY g.season_code DESC
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
  ORDER BY
    (SUM(CASE WHEN pts_for > pts_against THEN 1 ELSE 0 END)::float / NULLIF(COUNT(*), 0)) DESC,
    SUM(CASE WHEN pts_for > pts_against THEN 1 ELSE 0 END) DESC,
    (AVG(pts_for) - AVG(pts_against)) DESC
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


let draft_dataset_status = (unit ->? t3 int (option string) (option string)) {|
  SELECT
    COUNT(*)::int AS row_count,
    MAX(scraped_at) AS last_scraped_at,
    CASE
      WHEN COUNT(*) = 0 THEN 'missing_data'
      ELSE NULL
    END AS reason
  FROM player_drafts
|}


let trade_dataset_status = (unit ->? t3 int (option string) (option string)) {|
  SELECT
    COUNT(*)::int AS row_count,
    MAX(scraped_at) AS last_scraped_at,
    CASE
      WHEN COUNT(*) = 0 THEN 'missing_data'
      ELSE NULL
    END AS reason
  FROM official_trade_events
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

