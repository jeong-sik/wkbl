(** Database Layer using Caqti

    Principles:
    - Type-safe queries
    - Connection pooling via Caqti
    - All errors as Result, never exceptions
*)

open Domain

(** Database error type - explicit, not string *)
type db_error =
  | ConnectionFailed of string
  | QueryFailed of string
  | ParseError of string
[@@deriving show]

(** Result type alias for convenience *)
type 'a db_result = ('a, db_error) result Lwt.t

(** DB QA (data quality) report types. *)
type qa_score_mismatch = {
  qsm_game_id: string;
  qsm_game_date: string;
  qsm_home_team: string;
  qsm_away_team: string;
  qsm_home_score: int option;
  qsm_away_score: int option;
  qsm_home_sum: int option;
  qsm_away_sum: int option;
}

type qa_team_count_anomaly = {
  qtca_game_id: string;
  qtca_team_count: int;
}

type qa_duplicate_player_row = {
  qdpr_game_id: string;
  qdpr_team_code: string;
  qdpr_team_name: string;
  qdpr_player_id: string;
  qdpr_player_name: string;
  qdpr_row_count: int;
}

type qa_duplicate_player_name = {
  qdpn_player_name: string;
  qdpn_id_count: int;
  qdpn_player_ids: string list;
}

type qa_db_report = {
  qdr_generated_at: string;
  qdr_games_total: int;
  qdr_games_with_stats: int;
  qdr_plus_minus_games: int;
  qdr_plus_minus_coverage_pct: float;
  qdr_score_mismatch_count: int;
  qdr_score_mismatch_sample: qa_score_mismatch list;
  qdr_team_count_anomaly_count: int;
  qdr_team_count_anomaly_sample: qa_team_count_anomaly list;
  qdr_duplicate_player_row_count: int;
  qdr_duplicate_player_row_sample: qa_duplicate_player_row list;
  qdr_duplicate_player_name_count: int;
  qdr_duplicate_player_name_sample: qa_duplicate_player_name list;
}

(** Database connection URI *)
let default_db_path = "../data/wkbl.db"

let normalize_search_pattern search =
  let trimmed = String.trim search in
  if trimmed = "" then "%" else "%" ^ trimmed ^ "%"

module MarginKey = struct
  type t = string * string
  let compare = compare
end

module MarginMap = Map.Make (MarginKey)

(** Caqti type definitions for our domain *)
module Types = struct
  open Caqti_type

  (** Custom type for player_aggregate *)
  let player_aggregate =
    let encode _ = assert false in  (* We only decode *)
    let decode (player_id, rest) =
      let (name, rest) = rest in
      let (team_name, rest) = rest in
      let (games_played, rest) = rest in
      let (total_minutes, rest) = rest in
      let (total_points, rest) = rest in
      let (total_rebounds, rest) = rest in
      let (total_assists, rest) = rest in
      let (total_steals, rest) = rest in
      let (total_blocks, rest) = rest in
      let (total_turnovers, rest) = rest in
      let (avg_points, rest) = rest in
      let (avg_margin, rest) = rest in
      let (avg_rebounds, rest) = rest in
      let (avg_assists, rest) = rest in
      let (avg_steals, rest) = rest in
      let (avg_blocks, rest) = rest in
      let (avg_turnovers, efficiency) = rest in
      Ok {
        player_id;
        name;
        team_name;
        games_played;
        total_minutes;
        total_points;
        total_rebounds;
        total_assists;
        total_steals;
        total_blocks;
        total_turnovers;
        avg_points;
        avg_margin;
        avg_rebounds;
        avg_assists;
        avg_steals;
        avg_blocks;
        avg_turnovers;
        efficiency;
      }
    in
    let t = tup2 in
    custom ~encode ~decode
      (t string
        (t string
          (t string
            (t int
              (t float
                (t int
                  (t int
                    (t int
                      (t int
                        (t int
                          (t int
                            (t float
                              (t float
                                (t float
                                  (t float
                                    (t float
                                      (t float
                                        (t float float))))))))))))))))))

  let season_info =
    let encode _ = assert false in
    let decode (code, name) = Ok { code; name } in
    custom ~encode ~decode (tup2 string string)

  let team_totals =
    let encode _ = assert false in
    let decode
        ( season,
          ( team,
            ( gp,
              ( min_total,
                ( fg2_m,
                  ( fg2_a,
                    ( fg3_m,
                      ( fg3_a,
                        ( ft_m,
                          ( ft_a,
                            ( reb_off,
                              ( reb_def,
                                ( reb,
                                  ( ast,
                                    ( stl,
                                      ( blk,
                                        ( turnovers,
                                          pts )) )) )) )) )) )) )) )) )
      =
      Ok {
        season; team; gp; min_total;
        fg2_m; fg2_a; fg3_m; fg3_a; ft_m; ft_a;
        reb_off; reb_def; reb; ast; stl; blk; turnovers; pts;
      }
    in
    let t = tup2 in
    custom ~encode ~decode
      (t string (t string (t int (t float (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int int)))))))))))))))))

  let team_margin =
    let encode _ = assert false in
    let decode (season, (team, (gp, (pts_for, pts_against)))) =
      Ok { season; team; gp; pts_for; pts_against }
    in
    custom ~encode ~decode (tup2 string (tup2 string (tup2 int (tup2 int int))))

  let team_standing =
    let encode _ = assert false in
    let decode (team_name, (games_played, (wins, (losses, (avg_pts, avg_opp_pts))))) =
      let win_pct = if games_played = 0 then 0.0 else Stdlib.float wins /. Stdlib.float games_played in
      let diff = avg_pts -. avg_opp_pts in
      Ok { team_name; games_played; wins; losses; win_pct; gb = 0.0; avg_pts; avg_opp_pts; diff }
    in
    custom ~encode ~decode (tup2 string (tup2 int (tup2 int (tup2 int (tup2 float float)))))

  let game_summary =
    let encode _ = assert false in
    let decode (game_id, (game_date, (home_team, (away_team, (home_score, (away_score, game_type)))))) =
      Ok { game_id; game_date; home_team; away_team; home_score; away_score; game_type }
    in
    custom ~encode ~decode
      (tup2 string (tup2 string (tup2 string (tup2 string (tup2 (option int) (tup2 (option int) string))))))

  let qa_score_mismatch =
    let encode _ = assert false in
    let decode (game_id, (game_date, (home_team, (away_team, (home_score, (away_score, (home_sum, away_sum))))))) =
      Ok { qsm_game_id = game_id;
           qsm_game_date = game_date;
           qsm_home_team = home_team;
           qsm_away_team = away_team;
           qsm_home_score = home_score;
           qsm_away_score = away_score;
           qsm_home_sum = home_sum;
           qsm_away_sum = away_sum;
      }
    in
    let t = tup2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t string
                  (t (option int)
                     (t (option int)
                        (t (option int) (option int))))))))

  let qa_team_count_anomaly =
    let encode _ = assert false in
    let decode (game_id, team_count) = Ok { qtca_game_id = game_id; qtca_team_count = team_count } in
    custom ~encode ~decode (tup2 string int)

  let qa_duplicate_player_row =
    let encode _ = assert false in
    let decode (game_id, (team_code, (team_name, (player_id, (player_name, row_count))))) =
      Ok { qdpr_game_id = game_id;
           qdpr_team_code = team_code;
           qdpr_team_name = team_name;
           qdpr_player_id = player_id;
           qdpr_player_name = player_name;
           qdpr_row_count = row_count;
      }
    in
    let t = tup2 in
    custom ~encode ~decode (t string (t string (t string (t string (t string int)))))

  let qa_duplicate_player_name_row =
    let encode _ = assert false in
    let decode (player_name, (id_count, player_ids_csv)) =
      Ok (player_name, id_count, player_ids_csv)
    in
    custom ~encode ~decode (tup2 string (tup2 int string))

  let boxscore_player_stat =
    let encode _ = assert false in
    let decode (player_id, (player_name, (position, (team_code, (team_name, (min_seconds, (pts, (plus_minus, (reb, (ast, (stl, (blk, (tov, (fg2_m, (fg2_a, (fg3_m, (fg3_a, (ft_m, ft_a)))))))))))))))))) =
      let min = Stdlib.float min_seconds /. 60.0 in
      let fg_m = fg2_m + fg3_m in
      let fg_a = fg2_a + fg3_a in
      let pct m a = if a = 0 then 0.0 else Stdlib.float m /. Stdlib.float a *. 100.0 in
      Ok {
        bs_player_id = player_id;
        bs_player_name = player_name;
        bs_position = position;
        bs_team_code = team_code;
        bs_team_name = team_name;
        bs_minutes = min; bs_pts = pts; bs_plus_minus = plus_minus; bs_reb = reb; bs_ast = ast; bs_stl = stl; bs_blk = blk; bs_tov = tov;
        bs_fg_made = fg_m; bs_fg_att = fg_a; bs_fg_pct = pct fg_m fg_a;
        bs_fg3_made = fg3_m; bs_fg3_att = fg3_a; bs_fg3_pct = pct fg3_m fg3_a;
        bs_ft_made = ft_m; bs_ft_att = ft_a; bs_ft_pct = pct ft_m ft_a;
      }
    in
    let t = tup2 in
    let stats_tail =
      t int
        (t int
           (t int
              (t int
                 (t int
                    (t int
                       (t int
                          (t int
                             (t int
                                (t int int)))))))))
    in
    custom ~encode ~decode
      (t string
         (t string
            (t (option string)
               (t string
                  (t string
                     (t int
                        (t int
                           (t (option int) stats_tail))))))))

  let game_info =
    let encode _ = assert false in
    let decode (game_id, (game_date, (home_code, (home_name, (away_code, (away_name, (home_score, (away_score, score_quality_int)))))))) =
      Ok {
        gi_game_id = game_id; gi_game_date = game_date;
        gi_home_team_code = home_code; gi_home_team_name = home_name;
        gi_away_team_code = away_code; gi_away_team_name = away_name;
        gi_home_score = home_score;
        gi_away_score = away_score;
        gi_score_quality = game_score_quality_of_int score_quality_int;
      }
    in
    let t = tup2 in
    custom ~encode ~decode
      (t string (t string (t string (t string (t string (t string (t int (t int int))))))))

  let leader_entry =
    let encode _ = assert false in
    let decode (player_id, (player_name, (team_name, stat_value))) =
      Ok { le_player_id = player_id; le_player_name = player_name; le_team_name = team_name; le_stat_value = stat_value }
    in
    custom ~encode ~decode (tup2 string (tup2 string (tup2 string float)))

  let player_info =
    let encode _ = assert false in
    let decode (id, (name, (position, (birth_date, (height, weight))))) =
      Ok { id; name; position; birth_date; height; weight }
    in
    let t = tup2 in
    custom ~encode ~decode (t string (t string (t (option string) (t (option string) (t (option int) (option int))))))

  let season_stats =
    let encode _ = assert false in
    let decode (season_code, rest) =
      let (season_name, rest) = rest in
      let (gp, rest) = rest in
      let (min, rest) = rest in
      let (pts, rest) = rest in
      let (reb, rest) = rest in
      let (ast, rest) = rest in
      let (stl, rest) = rest in
      let (blk, rest) = rest in
      let (tov, rest) = rest in
      let (eff, rest) = rest in
      let (margin, (ts, efg)) = rest in
      Ok {
        ss_season_code = season_code;
        ss_season_name = season_name;
        ss_games_played = gp;
        ss_total_minutes = min;
        ss_avg_points = pts;
        ss_avg_rebounds = reb;
        ss_avg_assists = ast;
        ss_avg_steals = stl;
        ss_avg_blocks = blk;
        ss_avg_turnovers = tov;
        ss_efficiency = eff;
        ss_margin = margin;
        ss_ts_pct = ts;
        ss_efg_pct = efg;
      }
    in
    let t = tup2 in
    custom ~encode ~decode
      (t string (t string (t int (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float float)))))))))))))

  let player_game_stat =
    let encode _ = assert false in
    let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus)))))))))))))) =
      let is_home = is_home_int = 1 in
      Ok { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; pts; reb; ast; stl; blk; tov; plus_minus }
    in
    let t = tup2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t int
                  (t (option int)
                     (t (option int)
                        (t int
                           (t float
                              (t int (t int (t int (t int (t int (t int (option int)))))))))))))))

  let team_game_result =
    let encode _ = assert false in
    let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opp_score, is_win_int)))))) =
      Ok { tgr_game_id = game_id; tgr_game_date = game_date; tgr_opponent = opponent; tgr_is_home = is_home_int = 1; tgr_team_score = team_score; tgr_opponent_score = opp_score; tgr_is_win = is_win_int = 1 }
    in
    let t = tup2 in
    custom ~encode ~decode (t string (t string (t string (t int (t int (t int int))))))

  let h2h_game =
    let encode _ = assert false in
    let decode (game_id, (game_date, (p1_team, (p2_team, (p1_pts, (p1_reb, (p1_ast, (p2_pts, (p2_reb, (p2_ast, (winner, score_diff))))))))))) =
      Ok { game_id; game_date; player1_team = p1_team; player2_team = p2_team; player1_pts = p1_pts; player1_reb = p1_reb; player1_ast = p1_ast; player2_pts = p2_pts; player2_reb = p2_reb; player2_ast = p2_ast; winner_team = winner; score_diff }
    in
    let t = tup2 in
    custom ~encode ~decode (t string (t string (t string (t string (t int (t int (t int (t int (t int (t int (t string int)))))))))))
end

(** SQL Queries *)
module Queries = struct
  open Caqti_request.Infix
  open Caqti_type
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
  let ensure_score_mismatch_view = (unit ->. unit) {|
    CREATE VIEW IF NOT EXISTS score_mismatch_games AS
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
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
  let ensure_games_calc_view = (unit ->. unit) {|
    CREATE VIEW IF NOT EXISTS games_calc AS
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    )
    SELECT
      g.*,
      sh.pts_sum AS home_sum,
      sa.pts_sum AS away_sum,
      COALESCE(g.home_score, sh.pts_sum) AS home_score_calc,
      COALESCE(g.away_score, sa.pts_sum) AS away_score_calc
    FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
  |}
  let all_teams = (unit ->* string) "SELECT DISTINCT team_name_kr FROM teams ORDER BY team_name_kr"
  let all_seasons = (unit ->* Types.season_info) "SELECT season_code, season_name FROM seasons ORDER BY season_code"
  let player_stats_base = (tup2 (tup2 string string) int ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE g.game_type != '10'
      AND (? = 'ALL' OR t.team_name_kr = ?)
    GROUP BY p.player_id
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let player_career_aggregate = (string ->? Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      COALESCE(
        (
          SELECT t2.team_name_kr
          FROM game_stats s2
          JOIN games g2 ON g2.game_id = s2.game_id
          JOIN teams t2 ON t2.team_code = s2.team_code
          WHERE s2.player_id = p.player_id AND g2.game_type != '10'
          ORDER BY g2.game_date DESC
          LIMIT 1
        ),
        ''
      ) AS team_name_kr,
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    WHERE s.player_id = ?
      AND g.game_type != '10'
    GROUP BY p.player_id
  |}

  let player_aggregate_by_id = (tup2 string (tup2 string (tup2 string (tup2 string string))) ->? Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      COALESCE(
        (
          SELECT t2.team_name_kr
          FROM game_stats s2
          JOIN games g2 ON g2.game_id = s2.game_id
          JOIN teams t2 ON t2.team_code = s2.team_code
          WHERE s2.player_id = p.player_id
            AND g2.game_type != '10'
            AND (? = 'ALL' OR g2.season_code = ?)
          ORDER BY g2.game_date DESC, g2.game_id DESC
          LIMIT 1
        ),
        ''
      ) AS team_name_kr,
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    WHERE s.player_id = ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
    GROUP BY p.player_id
  |}
	  let team_totals_by_season = (tup2 string (tup2 string (tup2 string int)) ->* Types.team_totals) {|
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
	    FROM game_stats s
	    JOIN games_calc g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    WHERE g.game_type != '10'
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    GROUP BY season, t.team_code
	    ORDER BY t.team_name_kr ASC
	  |}
	  let team_margin_by_season = (tup2 string (tup2 string (tup2 string int)) ->* Types.team_margin) {|
	    SELECT
	      CASE WHEN ? = 'ALL' THEN 'ALL' ELSE g.season_code END as season,
	      t.team_name_kr,
	      COUNT(*) as gp,
	      COALESCE(SUM(CASE WHEN g.home_team_code = t.team_code THEN g.home_score_calc ELSE g.away_score_calc END), 0) as pts_for,
	      COALESCE(SUM(CASE WHEN g.home_team_code = t.team_code THEN g.away_score_calc ELSE g.home_score_calc END), 0) as pts_against
	    FROM games_calc g
	    JOIN teams t ON t.team_code = g.home_team_code OR t.team_code = g.away_team_code
	    WHERE (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	      AND g.home_score_calc IS NOT NULL
	      AND g.away_score_calc IS NOT NULL
	      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    GROUP BY season, t.team_code
	    ORDER BY t.team_name_kr ASC
	  |}

  let team_standings_by_season = (tup2 string string ->* Types.team_standing) {|
    SELECT
      t.team_name_kr,
      COUNT(*) as gp,
      SUM(
        CASE
          WHEN (g.home_team_code = t.team_code AND g.home_score_calc > g.away_score_calc)
            OR (g.away_team_code = t.team_code AND g.away_score_calc > g.home_score_calc)
          THEN 1
          ELSE 0
        END
      ) as wins,
      SUM(
        CASE
          WHEN (g.home_team_code = t.team_code AND g.home_score_calc < g.away_score_calc)
            OR (g.away_team_code = t.team_code AND g.away_score_calc < g.home_score_calc)
          THEN 1
          ELSE 0
        END
      ) as losses,
      AVG(CASE WHEN g.home_team_code = t.team_code THEN g.home_score_calc ELSE g.away_score_calc END) as avg_pts,
      AVG(CASE WHEN g.home_team_code = t.team_code THEN g.away_score_calc ELSE g.home_score_calc END) as avg_opp_pts
    FROM games_calc g
    JOIN teams t ON t.team_code = g.home_team_code OR t.team_code = g.away_team_code
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
      AND g.home_score_calc IS NOT NULL
      AND g.away_score_calc IS NOT NULL
    GROUP BY t.team_code
    ORDER BY wins DESC
  |}

  let all_games_by_season = (tup2 string string ->* Types.game_summary) {|
    SELECT
      g.game_id,
      COALESCE(g.game_date, 'Unknown'),
      t1.team_name_kr as home_team,
      t2.team_name_kr as away_team,
      g.home_score_calc,
      g.away_score_calc,
      g.game_type
    FROM games_calc g
    JOIN teams t1 ON g.home_team_code = t1.team_code
    JOIN teams t2 ON g.away_team_code = t2.team_code
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    ORDER BY g.game_date DESC, g.game_id DESC
    LIMIT 100
  |}

  let scored_games_by_season = (tup2 string (tup2 string int) ->* Types.game_summary) {|
    SELECT
      g.game_id,
      COALESCE(g.game_date, 'Unknown'),
      t1.team_name_kr as home_team,
      t2.team_name_kr as away_team,
      g.home_score_calc,
      g.away_score_calc,
      g.game_type
    FROM games_calc g
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
    (let t = tup2 string (tup2 (tup2 string string) int) in
     t ->* string)
    {|
      SELECT s.player_id
      FROM game_stats s
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
    (tup2 string string ->* string)
    {|
      SELECT s.player_id
      FROM game_stats s
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

  let qa_games_with_stats = (unit ->? int) {|
    SELECT COUNT(DISTINCT s.game_id)
    FROM game_stats s
    JOIN games g ON g.game_id = s.game_id
    WHERE g.game_type != '10'
  |}

  let qa_plus_minus_games = (unit ->? int) {|
    SELECT COUNT(DISTINCT pm.game_id)
    FROM player_plus_minus pm
    JOIN games g ON g.game_id = pm.game_id
    WHERE g.game_type != '10'
  |}

  let qa_score_mismatch_count = (unit ->? int) {|
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
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

  let qa_score_mismatch_sample = (unit ->* Types.qa_score_mismatch) {|
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    )
    SELECT
      g.game_id,
      COALESCE(g.game_date, 'Unknown'),
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
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.game_type != '10'
      GROUP BY s.game_id
      HAVING COUNT(DISTINCT s.team_code) != 2
    )
  |}

  let qa_team_count_anomaly_sample = (unit ->* Types.qa_team_count_anomaly) {|
    SELECT
      s.game_id,
      COUNT(DISTINCT s.team_code) AS team_count
    FROM game_stats s
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
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.game_type != '10'
      GROUP BY s.game_id, s.team_code, s.player_id
      HAVING COUNT(*) > 1
    )
  |}

	  let qa_duplicate_player_row_sample = (unit ->* Types.qa_duplicate_player_row) {|
	    SELECT
	      s.game_id,
	      s.team_code,
	      t.team_name_kr,
	      s.player_id,
	      TRIM(REPLACE(REPLACE(p.player_name, char(92), ''), '"', '')) AS player_name,
	      COUNT(*) AS row_count
	    FROM game_stats s
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    JOIN players p ON p.player_id = s.player_id
    WHERE g.game_type != '10'
    GROUP BY s.game_id, s.team_code, s.player_id
    HAVING COUNT(*) > 1
    ORDER BY row_count DESC, s.game_id DESC
    LIMIT 50
  |}

	  let qa_duplicate_player_name_count = (unit ->? int) {|
	    SELECT COUNT(*)
	    FROM (
	      SELECT TRIM(REPLACE(REPLACE(player_name, char(92), ''), '"', '')) AS normalized_name
	      FROM players
	      GROUP BY TRIM(REPLACE(REPLACE(player_name, char(92), ''), '"', ''))
	      HAVING COUNT(DISTINCT player_id) > 1
	    )
	  |}
	
	  let qa_duplicate_player_name_sample = (unit ->* Types.qa_duplicate_player_name_row) {|
	    SELECT
	      TRIM(REPLACE(REPLACE(player_name, char(92), ''), '"', '')) AS normalized_name,
	      COUNT(DISTINCT player_id) AS id_count,
	      GROUP_CONCAT(DISTINCT player_id) AS player_ids
	    FROM players
	    GROUP BY TRIM(REPLACE(REPLACE(player_name, char(92), ''), '"', ''))
	    HAVING COUNT(DISTINCT player_id) > 1
	    ORDER BY id_count DESC, normalized_name ASC
	    LIMIT 50
	  |}

  let game_info_by_id = (string ->? Types.game_info) {|
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    ),
    scored_games AS (
      SELECT
        g.*,
        sh.pts_sum AS home_sum,
        sa.pts_sum AS away_sum,
        COALESCE(g.home_score, sh.pts_sum) AS home_score_calc,
        COALESCE(g.away_score, sa.pts_sum) AS away_score_calc,
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
        END AS score_quality
      FROM games g
      LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
      LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
    )
    SELECT
      g.game_id,
      COALESCE(g.game_date, 'Unknown'),
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
  let boxscore_stats_by_game_id = (string ->* Types.boxscore_player_stat) {|
    WITH player_total_minutes AS (
      SELECT player_id, SUM(min_seconds) AS total_min_seconds
      FROM game_stats
      GROUP BY player_id
    ),
    ranked AS (
      SELECT
        p.player_id,
        TRIM(REPLACE(REPLACE(p.player_name, char(92), ''), '"', '')) AS player_name,
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
            TRIM(REPLACE(REPLACE(p.player_name, char(92), ''), '"', '')),
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
	      FROM game_stats s
	      JOIN players p ON s.player_id = p.player_id
	      JOIN teams t ON s.team_code = t.team_code
	      LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	      LEFT JOIN player_total_minutes ptm ON ptm.player_id = p.player_id
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
  let top_players = (int ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE g.game_type != '10'
    GROUP BY p.player_id
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let player_stats_by_points = (tup2 string (tup2 string (tup2 string (tup2 int int))) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE p.player_name LIKE ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    ORDER BY AVG(s.pts) DESC
    LIMIT ?
  |}
  let player_stats_by_margin = (tup2 string (tup2 string (tup2 string (tup2 int int))) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE p.player_name LIKE ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    HAVING SUM(
      CASE
        WHEN g.home_score_calc IS NOT NULL
          AND g.away_score_calc IS NOT NULL
          AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
        THEN s.min_seconds
        ELSE 0
      END
    ) >= 6000
    ORDER BY margin DESC
    LIMIT ?
  |}
  let player_stats_by_rebounds = (tup2 string (tup2 string (tup2 string (tup2 int int))) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE p.player_name LIKE ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    ORDER BY AVG(s.reb_tot) DESC
    LIMIT ?
  |}
  let player_stats_by_assists = (tup2 string (tup2 string (tup2 string (tup2 int int))) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE p.player_name LIKE ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    ORDER BY AVG(s.ast) DESC
    LIMIT ?
  |}
  let player_stats_by_efficiency = (tup2 string (tup2 string (tup2 string (tup2 int int))) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE p.player_name LIKE ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let player_stats_by_minutes = (tup2 string (tup2 string (tup2 string (tup2 int int))) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE p.player_name LIKE ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    ORDER BY SUM(s.min_seconds) DESC
    LIMIT ?
  |}
  let players_by_team = (tup2 string (tup2 string (tup2 string int)) ->* Types.player_aggregate) {|
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
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE t.team_name_kr = ?
      AND (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    GROUP BY p.player_id
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let leaders_pts = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING COUNT(*) >= 5 ORDER BY AVG(s.pts) DESC LIMIT 5 |}
  let leaders_pts_per36 = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.pts) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_reb = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING COUNT(*) >= 5 ORDER BY AVG(s.reb_tot) DESC LIMIT 5 |}
  let leaders_reb_per36 = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.reb_tot) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.reb_tot) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_ast = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING COUNT(*) >= 5 ORDER BY AVG(s.ast) DESC LIMIT 5 |}
  let leaders_ast_per36 = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ast) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.ast) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_stl = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.stl) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING COUNT(*) >= 5 ORDER BY AVG(s.stl) DESC LIMIT 5 |}
  let leaders_stl_per36 = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.stl) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.stl) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_blk = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.blk) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING COUNT(*) >= 5 ORDER BY AVG(s.blk) DESC LIMIT 5 |}
  let leaders_blk_per36 = (tup2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.blk) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.blk) / SUM(s.min_seconds)) DESC LIMIT 5 |}

  (** Stat Awards (unofficial) *)
  let stat_mvp_eff = (tup2 string (tup2 string int) ->* Types.leader_entry) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      AVG(s.game_score)
    FROM game_stats s
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    JOIN games g ON g.game_id = s.game_id
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id
    HAVING COUNT(*) >= 5
    ORDER BY AVG(s.game_score) DESC
    LIMIT 5
  |}

  let stat_mip_eff_delta = (tup2 string (tup2 int (tup2 string int)) ->* Types.leader_entry) {|
    WITH cur AS (
      SELECT
        s.player_id AS player_id,
        AVG(s.game_score) AS eff_cur,
        COUNT(*) AS gp_cur,
        MAX(t.team_name_kr) AS team_name
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      JOIN teams t ON t.team_code = s.team_code
      WHERE g.season_code = ?
        AND g.game_type != '10'
        AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
      GROUP BY s.player_id
    ),
    prev AS (
      SELECT
        s.player_id AS player_id,
        AVG(s.game_score) AS eff_prev,
        COUNT(*) AS gp_prev
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.season_code = ?
        AND g.game_type != '10'
        AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
      GROUP BY s.player_id
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
  let player_info = (string ->? Types.player_info) "SELECT player_id, player_name, position, birth_date, height, weight FROM players WHERE player_id = ?"
  
  (** Queries for Per Game (Average) *)
	  let player_seasons_per_game = (string ->* Types.season_stats) {|
	    SELECT
	      g.season_code,
	      se.season_name,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      COALESCE(AVG(s.pts), 0),
	      COALESCE(AVG(s.reb_tot), 0),
	      COALESCE(AVG(s.ast), 0),
	      COALESCE(AVG(s.stl), 0),
	      COALESCE(AVG(s.blk), 0),
	      COALESCE(AVG(s.tov), 0),
		      COALESCE(AVG(s.game_score), 0),
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
	      0.0 as ts_pct,
	      0.0 as efg_pct
	    FROM game_stats s
		    JOIN games_calc g ON s.game_id = g.game_id
		    JOIN seasons se ON g.season_code = se.season_code
		    WHERE s.player_id = ?
		      AND g.game_type != '10'
		    GROUP BY g.season_code
		    ORDER BY g.season_code DESC
		  |}

  (** Queries for Totals (Sum) *)
	  let player_seasons_totals = (string ->* Types.season_stats) {|
	    SELECT
	      g.season_code,
	      se.season_name,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      COALESCE(SUM(s.pts), 0),
	      COALESCE(SUM(s.reb_tot), 0),
	      COALESCE(SUM(s.ast), 0),
	      COALESCE(SUM(s.stl), 0),
	      COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
		      COALESCE(SUM(s.game_score), 0),
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
	      0.0 as ts_pct,
	      0.0 as efg_pct
	    FROM game_stats s
		    JOIN games_calc g ON s.game_id = g.game_id
		    JOIN seasons se ON g.season_code = se.season_code
		    WHERE s.player_id = ?
		      AND g.game_type != '10'
		    GROUP BY g.season_code
		    ORDER BY g.season_code DESC
		  |}

  (** Queries for Per 36 Minutes (Normalized) *)
	  let player_seasons_per36 = (string ->* Types.season_stats) {|
	    SELECT
	      g.season_code,
	      se.season_name,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      (SUM(s.pts) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.reb_tot) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.ast) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.stl) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.blk) * 2160.0 / SUM(s.min_seconds)),
		      (SUM(s.tov) * 2160.0 / SUM(s.min_seconds)),
		      (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds)),
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
	      0.0 as ts_pct,
	      0.0 as efg_pct
	    FROM game_stats s
		    JOIN games_calc g ON s.game_id = g.game_id
		    JOIN seasons se ON g.season_code = se.season_code
		    WHERE s.player_id = ?
		      AND g.game_type != '10'
		    GROUP BY g.season_code
		    HAVING SUM(s.min_seconds) > 0
		    ORDER BY g.season_code DESC
		  |}

		  let player_recent_games = (string ->* Types.player_game_stat) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
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
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ? AND g.game_type != '10'
		    ORDER BY g.game_date DESC
		    LIMIT 10
		  |}
		  let player_game_logs = (tup2 string (tup2 string (tup2 string int)) ->* Types.player_game_stat) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
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
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ?
		      AND (? = 'ALL' OR g.season_code = ?)
		      AND g.game_type != '10'
		      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
		    ORDER BY g.game_date DESC
		  |}
		  let player_all_star_games = (string ->* Types.player_game_stat) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
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
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ? AND g.game_type = '10'
		    ORDER BY g.game_date DESC
		  |}
		  let player_team_games = (string ->* (tup2 string string)) {| SELECT g.game_date, t.team_name_kr FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t ON t.team_code = s.team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY g.game_date ASC |}

  (** Career Highs Queries (best single game per category) *)
	  let career_high_points_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.pts DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_rebounds_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.reb_tot DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_assists_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.ast DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_steals_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.stl DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_blocks_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.blk DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}

	  let team_recent_games = (let t = tup2 string (tup2 string (tup2 string (tup2 string (tup2 string (tup2 string (tup2 string (tup2 string (tup2 string string)))))))) in t ->* Types.team_game_result) {|
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
	    FROM games_calc g
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
	  let player_by_name = (tup2 string (tup2 string string) ->? Types.player_aggregate) {|
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
	      COALESCE(AVG(s.game_score), 0)
	    FROM game_stats s
	    JOIN players p ON s.player_id = p.player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games_calc g ON g.game_id = s.game_id
	    WHERE p.player_name = ?
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	    GROUP BY p.player_id
	  |}
  let player_h2h_games = (tup2 (tup2 string string) (tup2 string string) ->* Types.h2h_game) {|
    WITH scored_games AS (
      SELECT
        g.*,
        COALESCE(
          g.home_score,
          (SELECT SUM(sx.pts) FROM game_stats sx WHERE sx.game_id = g.game_id AND sx.team_code = g.home_team_code)
        ) AS home_score_calc,
        COALESCE(
          g.away_score,
          (SELECT SUM(sx.pts) FROM game_stats sx WHERE sx.game_id = g.game_id AND sx.team_code = g.away_team_code)
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
      s2.pts as p2_pts,
      s2.reb_tot as p2_reb,
      s2.ast as p2_ast,
      CASE WHEN g.home_score_calc > g.away_score_calc THEN t1.team_name_kr ELSE t2.team_name_kr END as winner,
      COALESCE(ABS(g.home_score_calc - g.away_score_calc), 0) as diff
    FROM scored_games g
    JOIN game_stats s1 ON g.game_id = s1.game_id
    JOIN game_stats s2 ON g.game_id = s2.game_id
    JOIN teams t1 ON s1.team_code = t1.team_code
    JOIN teams t2 ON s2.team_code = t2.team_code
    WHERE s1.player_id = ?
      AND s2.player_id = ?
      AND (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
      AND g.home_score_calc IS NOT NULL
      AND g.away_score_calc IS NOT NULL
    ORDER BY g.game_date DESC
  |}
end

(** Database operations *)
  module Repo = struct
    let ensure_schema (module Db : Caqti_lwt.CONNECTION) =
      let open Lwt_result.Syntax in
      let* () = Db.exec Queries.ensure_player_plus_minus_table () in
      let* () = Db.exec Queries.ensure_player_plus_minus_index () in
      let* () = Db.exec Queries.ensure_score_mismatch_view () in
      Db.exec Queries.ensure_games_calc_view ()
  let get_teams (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.all_teams ()
  let get_seasons (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.all_seasons ()
  let get_player_by_name ~name ~season (module Db : Caqti_lwt.CONNECTION) = let s = if season = "" then "ALL" else season in Db.find_opt Queries.player_by_name (name, (s, s))
  let get_player_aggregate_by_id ~player_id ~season (module Db : Caqti_lwt.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt Queries.player_aggregate_by_id (s, (s, (player_id, (s, s))))
  let query_for_sort = function | ByPoints -> Queries.player_stats_by_points | ByMargin -> Queries.player_stats_by_margin | ByRebounds -> Queries.player_stats_by_rebounds | ByAssists -> Queries.player_stats_by_assists | ByEfficiency -> Queries.player_stats_by_efficiency | ByMinutes -> Queries.player_stats_by_minutes
  let get_players_filtered ~season ~sort ~search ~limit ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let pattern = normalize_search_pattern search in
    let s = if String.trim season = "" then "ALL" else season in
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list (query_for_sort sort) (pattern, (s, (s, (include_int, limit))))
  let get_top_players ?(team_name="ALL") ~limit (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.player_stats_base ((team_name, team_name), limit)
  let get_players_by_team ~team_name ~season ~limit (module Db : Caqti_lwt.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.players_by_team (team_name, (s, (s, limit)))
  let get_team_totals ~season ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.team_totals_by_season (season, (season, (season, include_int)))

  let get_team_margins ~season ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.team_margin_by_season (season, (season, (season, include_int)))
  let get_standings ~season (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.team_standings_by_season (season, season)
  let get_games ~season (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.all_games_by_season (season, season)
  let get_scored_games ~season ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.scored_games_by_season (season, (season, include_int))
  let get_game_season_code ~game_id (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.game_season_by_id game_id
  let get_team_core_player_ids ~season ~team_name ~limit (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.team_core_player_ids (team_name, ((season, season), limit))
  let get_team_active_player_ids ~team_name ~game_id (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.team_active_player_ids (team_name, game_id)

  let qa_games_total (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_games_total ()
  let qa_games_with_stats (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_games_with_stats ()
  let qa_plus_minus_games (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_plus_minus_games ()
  let qa_score_mismatch_count (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_score_mismatch_count ()
  let qa_score_mismatch_sample (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.qa_score_mismatch_sample ()
  let qa_team_count_anomaly_count (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_team_count_anomaly_count ()
  let qa_team_count_anomaly_sample (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.qa_team_count_anomaly_sample ()
  let qa_duplicate_player_row_count (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_row_count ()
  let qa_duplicate_player_row_sample (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_row_sample ()
  let qa_duplicate_player_name_count (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_name_count ()
  let qa_duplicate_player_name_sample (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_name_sample ()
  let get_game_info ~game_id (module Db : Caqti_lwt.CONNECTION) = Db.find_opt Queries.game_info_by_id game_id
  let get_boxscore_stats ~game_id (module Db : Caqti_lwt.CONNECTION) = Db.collect_list Queries.boxscore_stats_by_game_id game_id
  let get_leaders ~category ~scope ~season (module Db : Caqti_lwt.CONNECTION) = let q = match (category, scope) with | "pts", "per_36" -> Queries.leaders_pts_per36 | "pts", _ -> Queries.leaders_pts | "reb", "per_36" -> Queries.leaders_reb_per36 | "reb", _ -> Queries.leaders_reb | "ast", "per_36" -> Queries.leaders_ast_per36 | "ast", _ -> Queries.leaders_ast | "stl", "per_36" -> Queries.leaders_stl_per36 | "stl", _ -> Queries.leaders_stl | "blk", "per_36" -> Queries.leaders_blk_per36 | "blk", _ -> Queries.leaders_blk | _ -> Queries.leaders_pts in Db.collect_list q (season, season)

  let get_stat_mvp_eff ~season ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.stat_mvp_eff (s, (s, include_int))

  let get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.stat_mip_eff_delta (season, (include_int, (prev_season, include_int)))
  
	  let get_player_profile ~player_id (module Db : Caqti_lwt.CONNECTION) = 
	    let open Lwt.Syntax in 
	    let* info = Db.find_opt Queries.player_info player_id in 
	    match info with 
	    | Error e -> Lwt.return (Error e) 
		    | Ok None -> Lwt.return (Ok None) 
		    | Ok (Some p) -> 
		        let team_stints_of_games (games: (string * string) list) =
		          let rec loop current acc = function
		            | [] ->
		                (match current with
		                | None -> List.rev acc
		                | Some c -> List.rev (c :: acc))
		            | (game_date, team_name) :: rest ->
		                (match current with
		                | None ->
		                    loop
		                      (Some
		                        {
		                          pts_team_name = team_name;
		                          pts_start_date = game_date;
		                          pts_end_date = game_date;
		                          pts_games_played = 1;
		                        })
		                      acc
		                      rest
		                | Some stint when stint.pts_team_name = team_name ->
		                    loop
		                      (Some
		                        {
		                          stint with
		                          pts_end_date = game_date;
		                          pts_games_played = stint.pts_games_played + 1;
		                        })
		                      acc
		                      rest
		                | Some stint ->
		                    loop
		                      (Some
		                        {
		                          pts_team_name = team_name;
		                          pts_start_date = game_date;
		                          pts_end_date = game_date;
		                          pts_games_played = 1;
		                        })
		                      (stint :: acc)
		                      rest)
		          in
		          loop None [] games
		        in
		        let* seasons = Db.collect_list Queries.player_seasons_per_game player_id in 
		        let* recent = Db.collect_list Queries.player_recent_games player_id in 
		        let* all_star = Db.collect_list Queries.player_all_star_games player_id in
		        let* team_games = Db.collect_list Queries.player_team_games player_id in
		        let* averages_res = Db.find_opt Queries.player_career_aggregate player_id in
		        let* ch_points = Db.find_opt Queries.career_high_points_game player_id in
		        let* ch_rebounds = Db.find_opt Queries.career_high_rebounds_game player_id in
		        let* ch_assists = Db.find_opt Queries.career_high_assists_game player_id in
		        let* ch_steals = Db.find_opt Queries.career_high_steals_game player_id in
		        let* ch_blocks = Db.find_opt Queries.career_high_blocks_game player_id in
		        let dummy_avg = { player_id = p.id; name = p.name; team_name = ""; games_played = 0; total_minutes = 0.0; total_points = 0; total_rebounds = 0; total_assists = 0; total_steals = 0; total_blocks = 0; total_turnovers = 0; avg_points = 0.0; avg_margin = 0.0; avg_rebounds = 0.0; avg_assists = 0.0; avg_steals = 0.0; avg_blocks = 0.0; avg_turnovers = 0.0; efficiency = 0.0; } in 
		        match seasons, recent, all_star, team_games, averages_res, ch_points, ch_rebounds, ch_assists, ch_steals, ch_blocks with 
		        | Ok ss, Ok rg, Ok asg, Ok tgs, Ok avg_opt, Ok pts_g, Ok reb_g, Ok ast_g, Ok stl_g, Ok blk_g -> 
		            let add_opt opt mk acc = match opt with | None -> acc | Some v -> mk v :: acc in
		            let items =
		              []
		              |> add_opt pts_g (fun (g: player_game_stat) -> { chi_label = "Points"; chi_value = g.pts; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt reb_g (fun g -> { chi_label = "Rebounds"; chi_value = g.reb; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ast_g (fun g -> { chi_label = "Assists"; chi_value = g.ast; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt stl_g (fun g -> { chi_label = "Steals"; chi_value = g.stl; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt blk_g (fun g -> { chi_label = "Blocks"; chi_value = g.blk; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> List.rev
		            in
		            let career_highs = match items with | [] -> None | _ -> Some items in
		            let averages = match avg_opt with Some a -> a | None -> dummy_avg in
		            let team_stints = team_stints_of_games tgs in
		            Lwt.return (Ok (Some { player = p; averages; recent_games = rg; all_star_games = asg; team_stints; season_breakdown = ss; career_highs })) 
		        | Error e, _, _, _, _, _, _, _, _, _
		        | _, Error e, _, _, _, _, _, _, _, _
		        | _, _, Error e, _, _, _, _, _, _, _
		        | _, _, _, Error e, _, _, _, _, _, _
		        | _, _, _, _, Error e, _, _, _, _, _
		        | _, _, _, _, _, Error e, _, _, _, _
		        | _, _, _, _, _, _, Error e, _, _, _
		        | _, _, _, _, _, _, _, Error e, _, _
		        | _, _, _, _, _, _, _, _, Error e, _
		        | _, _, _, _, _, _, _, _, _, Error e ->
		            Lwt.return (Error e)

  let get_player_season_stats ~player_id ~scope (module Db : Caqti_lwt.CONNECTION) = let q = match scope with | "totals" -> Queries.player_seasons_totals | "per_36" -> Queries.player_seasons_per36 | _ -> Queries.player_seasons_per_game in Db.collect_list q player_id
  let get_player_game_logs ~player_id ~season ~include_mismatch (module Db : Caqti_lwt.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.player_game_logs (player_id, (s, (s, include_int)))
  let get_team_recent_games ~team_name ~season (module Db : Caqti_lwt.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    let t =
      (team_name,
       (team_name,
        (team_name,
         (team_name,
          (team_name,
           (team_name,
            (team_name,
             (team_name,
              (s, s)))))))))
    in
    Db.collect_list Queries.team_recent_games t
  let get_player_h2h ~p1_id ~p2_id ~season (module Db : Caqti_lwt.CONNECTION) = let s = if season = "" then "ALL" else season in Db.collect_list Queries.player_h2h_games ((p1_id, p2_id), (s, s))
end

(** Connection pool *)
let pool_ref : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t option ref = ref None
let init_pool db_path = let uri = Uri.of_string ("sqlite3:" ^ db_path) in match Caqti_lwt.connect_pool uri with | Ok pool -> pool_ref := Some pool; Ok () | Error e -> Error (ConnectionFailed (Caqti_error.show e))
let with_db f = let open Lwt.Syntax in match !pool_ref with | None -> Lwt.return (Error (ConnectionFailed "Pool not initialized")) | Some pool -> let* result = Caqti_lwt.Pool.use f pool in match result with | Ok v -> Lwt.return (Ok v) | Error e -> Lwt.return (Error (QueryFailed (Caqti_error.show e)))

let iso8601_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let split_csv_ids (ids : string) =
  ids
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(** Public API *)
let ensure_schema () = with_db (fun db -> Repo.ensure_schema db)
let get_all_teams () = with_db (fun db -> Repo.get_teams db)
let get_seasons () = with_db (fun db -> Repo.get_seasons db)
let get_player_by_name ?(season="ALL") name = with_db (fun db -> Repo.get_player_by_name ~name ~season db)
let get_player_aggregate_by_id ~player_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_aggregate_by_id ~player_id ~season db)
let get_players ?(season="ALL") ?(limit=50) ?(search="") ?(sort=ByEfficiency) ?(include_mismatch=false) () =
  with_db (fun db -> Repo.get_players_filtered ~season ~sort ~search ~limit ~include_mismatch db)
let get_players_by_team ~team_name ?(season="ALL") ?(limit=20) () =
  with_db (fun db -> Repo.get_players_by_team ~team_name ~season ~limit db)
let build_margin_map margins : team_margin MarginMap.t =
  List.fold_left
    (fun acc (row: team_margin) ->
      let key = (row.season, row.team) in
      MarginMap.add key row acc)
    MarginMap.empty
    margins

let sort_team_stats sort_field (items : team_stats list) =
  let metric = function
    | TeamByPoints -> (fun (row: team_stats) -> row.pts)
    | TeamByRebounds -> (fun row -> row.reb)
    | TeamByAssists -> (fun row -> row.ast)
    | TeamBySteals -> (fun row -> row.stl)
    | TeamByBlocks -> (fun row -> row.blk)
    | TeamByEfficiency -> (fun row -> row.eff)
    | TeamByTsPct -> (fun row -> row.ts_pct)
    | TeamByFg3Pct -> (fun row -> row.fg3_pct)
    | TeamByMinutes -> (fun row -> row.min_total)
  in
  let getter = metric sort_field in
  List.sort
    (fun a b ->
      let primary = compare (getter b) (getter a) in
      if primary <> 0 then primary else String.compare a.team b.team)
    items

let dedupe_team_stats (items : team_stats list) =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  items
  |> List.filter (fun (row: team_stats) ->
      let team = normalize_label row.team in
      let key =
        match team_code_of_string team with
        | Some code -> code
        | None -> team |> String.uppercase_ascii
      in
      if Hashtbl.mem seen key then
        false
      else (
        Hashtbl.add seen key ();
        true
      ))

let get_team_stats ?(season="ALL") ?(scope=PerGame) ?(sort=TeamByPoints) ?(include_mismatch=false) () =
  let open Lwt.Syntax in
  let* totals_result = with_db (fun db -> Repo.get_team_totals ~season ~include_mismatch db) in
  let* margins_result = with_db (fun db -> Repo.get_team_margins ~season ~include_mismatch db) in
  match totals_result, margins_result with
  | Ok totals, Ok margins ->
      let margin_map = build_margin_map margins in
      let stats =
        totals
        |> List.map (fun (row: team_totals) ->
            let key = (row.season, row.team) in
            let margin = MarginMap.find_opt key margin_map in
            Stats.team_stats_of_totals ~scope ~margin row)
        |> sort_team_stats sort
        |> dedupe_team_stats
      in
      Lwt.return (Ok stats)
  | Error err, _ -> Lwt.return (Error err)
  | _, Error err -> Lwt.return (Error err)
let calculate_gb (standings : team_standing list) = match standings with | [] -> [] | leader :: others -> let calc s = let wins_diff = Stdlib.float (leader.wins - s.wins) in let losses_diff = Stdlib.float (s.losses - leader.losses) in (wins_diff +. losses_diff) /. 2.0 in leader :: List.map (fun s -> { s with gb = calc s }) others
let get_standings ?(season = "ALL") () = let open Lwt.Syntax in let* result = with_db (fun db -> Repo.get_standings ~season db) in match result with | Ok standings -> Lwt.return (Ok (calculate_gb standings)) | Error err -> Lwt.return (Error err)
let get_games ?(season = "ALL") () = with_db (fun db -> Repo.get_games ~season db)
let get_scored_games ?(season = "ALL") ?(include_mismatch=false) () =
  with_db (fun db -> Repo.get_scored_games ~season ~include_mismatch db)
let get_game_season_code ~game_id () = with_db (fun db -> Repo.get_game_season_code ~game_id db)
let get_team_core_player_ids ~season ~team_name ?(limit=7) () =
  with_db (fun db -> Repo.get_team_core_player_ids ~season ~team_name ~limit db)
let get_team_active_player_ids ~team_name ~game_id () =
  with_db (fun db -> Repo.get_team_active_player_ids ~team_name ~game_id db)
let get_boxscore ~game_id () = let open Lwt.Syntax in let* game_info_result = with_db (fun db -> Repo.get_game_info ~game_id db) in let* stats_result = with_db (fun db -> Repo.get_boxscore_stats ~game_id db) in match game_info_result, stats_result with | Ok (Some game_info), Ok stats -> let home_players = List.filter (fun s -> s.bs_team_code = game_info.gi_home_team_code) stats in let away_players = List.filter (fun s -> s.bs_team_code = game_info.gi_away_team_code) stats in Lwt.return (Ok { boxscore_game = game_info; boxscore_home_players = home_players; boxscore_away_players = away_players }) | Ok None, _ -> Lwt.return (Error (QueryFailed "Game not found")) | Error err, _ -> Lwt.return (Error err) | _, Error err -> Lwt.return (Error err)
let get_leaders ?(season="ALL") ?(scope="per_game") category = with_db (fun db -> Repo.get_leaders ~category ~scope ~season db)

let get_stat_mvp_eff ?(season="ALL") ?(include_mismatch=false) () =
  with_db (fun db -> Repo.get_stat_mvp_eff ~season ~include_mismatch db)

let get_stat_mip_eff_delta ~season ~prev_season ?(include_mismatch=false) () =
  with_db (fun db -> Repo.get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch db)

let get_player_profile ~player_id () = with_db (fun db -> Repo.get_player_profile ~player_id db)
let get_player_season_stats ~player_id ~scope () = with_db (fun db -> Repo.get_player_season_stats ~player_id ~scope db)
let get_player_game_logs ~player_id ?(season="ALL") ?(include_mismatch=false) () =
  with_db (fun db -> Repo.get_player_game_logs ~player_id ~season ~include_mismatch db)
let get_team_full_detail ~team_name ?(season="ALL") () =
  let open Lwt.Syntax in
  let* standing_res = get_standings ~season () in
  let* roster_res = get_players_by_team ~team_name ~season () in
  let* games_res = with_db (fun db -> Repo.get_team_recent_games ~team_name ~season db) in
  match standing_res, roster_res, games_res with
  | Ok standings, Ok roster, Ok games ->
      let standing = List.find_opt (fun (s: team_standing) -> s.team_name = team_name) standings in
      Lwt.return (Ok { tfd_team_name = team_name; tfd_standing = standing; tfd_roster = roster; tfd_recent_games = games })
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Lwt.return (Error e)
let get_player_h2h_data ~p1_id ~p2_id ?(season="ALL") () = with_db (fun db -> Repo.get_player_h2h ~p1_id ~p2_id ~season db)

let get_db_quality_report () : qa_db_report db_result =
  let open Lwt_result.Syntax in
  let int_or_zero = function | None -> 0 | Some v -> v in
  let round1 v = Float.round (v *. 10.0) /. 10.0 in
  let* games_total_opt = with_db (fun db -> Repo.qa_games_total db) in
  let* games_with_stats_opt = with_db (fun db -> Repo.qa_games_with_stats db) in
  let* plus_minus_games_opt = with_db (fun db -> Repo.qa_plus_minus_games db) in
  let* mismatch_count_opt = with_db (fun db -> Repo.qa_score_mismatch_count db) in
  let* mismatch_sample = with_db (fun db -> Repo.qa_score_mismatch_sample db) in
  let* team_count_anomaly_count_opt = with_db (fun db -> Repo.qa_team_count_anomaly_count db) in
  let* team_count_anomaly_sample = with_db (fun db -> Repo.qa_team_count_anomaly_sample db) in
  let* dup_row_count_opt = with_db (fun db -> Repo.qa_duplicate_player_row_count db) in
  let* dup_row_sample = with_db (fun db -> Repo.qa_duplicate_player_row_sample db) in
  let* dup_name_count_opt = with_db (fun db -> Repo.qa_duplicate_player_name_count db) in
  let* dup_name_rows = with_db (fun db -> Repo.qa_duplicate_player_name_sample db) in
  let games_total = int_or_zero games_total_opt in
  let games_with_stats = int_or_zero games_with_stats_opt in
  let plus_minus_games = int_or_zero plus_minus_games_opt in
  let plus_minus_coverage_pct =
    if games_total <= 0 then 0.0
    else round1 ((float_of_int plus_minus_games /. float_of_int games_total) *. 100.0)
  in
  let dup_name_sample =
    dup_name_rows
    |> List.map (fun (name, id_count, ids_csv) ->
        { qdpn_player_name = name;
          qdpn_id_count = id_count;
          qdpn_player_ids = split_csv_ids ids_csv;
        })
  in
  Lwt_result.return
    { qdr_generated_at = iso8601_utc ();
      qdr_games_total = games_total;
      qdr_games_with_stats = games_with_stats;
      qdr_plus_minus_games = plus_minus_games;
      qdr_plus_minus_coverage_pct = plus_minus_coverage_pct;
      qdr_score_mismatch_count = int_or_zero mismatch_count_opt;
      qdr_score_mismatch_sample = mismatch_sample;
      qdr_team_count_anomaly_count = int_or_zero team_count_anomaly_count_opt;
      qdr_team_count_anomaly_sample = team_count_anomaly_sample;
      qdr_duplicate_player_row_count = int_or_zero dup_row_count_opt;
      qdr_duplicate_player_row_sample = dup_row_sample;
      qdr_duplicate_player_name_count = int_or_zero dup_name_count_opt;
      qdr_duplicate_player_name_sample = dup_name_sample;
    }
