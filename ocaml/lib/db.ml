(** Database Layer using Caqti

    Principles:
    - Type-safe queries
    - Connection pooling via Caqti
    - All errors as Result, never exceptions
    - Memory caching for performance (TTL-based)
*)

open Domain

(** Database error type - explicit, not string *)
type db_error =
  | ConnectionFailed of string
  | QueryFailed of string
  | ParseError of string
[@@deriving show]

(** Result type alias for convenience - Direct style with Eio *)
type 'a db_result = ('a, db_error) result

(** Helper to extract game info safely - solves record field shadowing issues in views *)
let extract_game_info (g: game_summary) =
  (g.game_id, g.game_date, g.home_team, g.away_team, g.home_score, g.away_score)

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

type qa_duplicate_player_identity = {
  qdpi_player_name: string;
  qdpi_birth_date: string;
  qdpi_id_count: int;
  qdpi_player_ids: string list;
}

type qa_schedule_missing_game = {
  qsmg_game_date: string;
  qsmg_season_code: string;
  qsmg_home_team: string;
  qsmg_away_team: string;
}

type qa_schedule_missing_stats = {
  qsms_game_id: string;
  qsms_game_date: string;
  qsms_home_team: string;
  qsms_away_team: string;
}

type qa_schedule_coverage = {
  qsc_season_code: string;
  qsc_schedule_completed: int;
  qsc_games_total: int;
  qsc_matched: int;
  qsc_missing: int;
  qsc_coverage_pct: float;
  qsc_season_uningested: bool;
  qsc_games_missing_team: bool;
}

type qa_pbp_missing_game = {
  qpmg_game_id: string;
  qpmg_game_date: string;
  qpmg_home_team: string;
  qpmg_away_team: string;
  qpmg_home_score: int;
  qpmg_away_score: int;
}

type qa_pbp_missing_report = {
  qpmr_generated_at: string;
  qpmr_season_code: string;
  qpmr_finished_games_total: int;
  qpmr_pbp_games: int;
  qpmr_missing_games: int;
  qpmr_coverage_pct: float;
  qpmr_missing_sample: qa_pbp_missing_game list;
}

type qa_db_report = {
  qdr_generated_at: string;
  qdr_games_total: int;
  qdr_finished_games_total: int;
  qdr_games_with_stats: int;
  qdr_pbp_games: int;
  qdr_pbp_missing_games: int;
  qdr_pbp_coverage_pct: float;
  qdr_plus_minus_games: int;
  qdr_plus_minus_coverage_pct: float;
  qdr_schedule_total: int;
  qdr_schedule_completed: int;
  qdr_schedule_missing_game_count: int;
  qdr_schedule_missing_game_pct: float;
  qdr_schedule_missing_game_sample: qa_schedule_missing_game list;
  qdr_schedule_missing_stats_count: int;
  qdr_schedule_missing_stats_pct: float;
  qdr_schedule_missing_stats_sample: qa_schedule_missing_stats list;
  qdr_schedule_coverage: qa_schedule_coverage list;
  qdr_score_mismatch_count: int;
  qdr_score_mismatch_sample: qa_score_mismatch list;
  qdr_team_count_anomaly_count: int;
  qdr_team_count_anomaly_sample: qa_team_count_anomaly list;
  qdr_duplicate_player_row_count: int;
  qdr_duplicate_player_row_sample: qa_duplicate_player_row list;
  qdr_duplicate_player_name_count: int;
  qdr_duplicate_player_name_sample: qa_duplicate_player_name list;
  qdr_duplicate_player_identity_count: int;
  qdr_duplicate_player_identity_sample: qa_duplicate_player_identity list;
}

type qa_schedule_missing_summary = {
  qsms_missing_ingested: int;
  qsms_missing_uningested: int;
  qsms_missing_total: int;
}

type qa_schedule_missing_sample = {
  qsmp_season_code: string;
  qsmp_game_date: string;
  qsmp_home_team_code: string;
  qsmp_away_team_code: string;
  qsmp_reason: string;
  qsmp_games_on_date: int;
  qsmp_games_info: string option;
}

type qa_schedule_missing_report = {
  qsmr_generated_at: string;
  qsmr_summary: qa_schedule_missing_summary;
  qsmr_reason_counts: (string * int) list;
  qsmr_samples: qa_schedule_missing_sample list;
}

type qa_stat_anomaly = {
  qsa_game_id: string;
  qsa_game_date: string;
  qsa_team_name: string;
  qsa_player_id: string;
  qsa_player_name: string;
  qsa_min_seconds: int;
  qsa_pts: int;
  qsa_primary_team_name: string;
  qsa_primary_gp: int;
  qsa_primary_min_seconds: int;
}

type qa_stat_exclusion = {
  qse_game_id: string;
  qse_game_date: string;
  qse_team_name: string;
  qse_player_id: string;
  qse_player_name: string;
  qse_min_seconds: int;
  qse_pts: int;
  qse_reason: string;
  qse_created_at: string;
}

type leader_base = {
  lb_player_id: string;
  lb_player_name: string;
  lb_team_name: string;
  lb_gp: int;
  lb_min_seconds: int;
  lb_pts: int;
  lb_reb: int;
  lb_ast: int;
  lb_stl: int;
  lb_blk: int;
  lb_tov: int;
  lb_eff: float;
  lb_fg_m: int;
  lb_fg_a: int;
  lb_fg3_m: int;
  lb_fg3_a: int;
  lb_ft_m: int;
  lb_ft_a: int;
}

type player_base = {
  pb_player_id: string;
  pb_player_name: string;
  pb_team_name: string;
  pb_gp: int;
  pb_min_seconds: int;
  pb_pts: int;
  pb_reb: int;
  pb_ast: int;
  pb_stl: int;
  pb_blk: int;
  pb_tov: int;
  pb_avg_pts: float;
  pb_margin: float;
  pb_avg_reb: float;
  pb_avg_ast: float;
  pb_avg_stl: float;
  pb_avg_blk: float;
  pb_avg_tov: float;
  pb_eff: float;
  pb_margin_seconds: int;
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
    let encode _ = Error "Encode not supported: read-only type" in
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
      let (avg_turnovers, rest) = rest in
      let (efficiency, rest) = rest in
      let (total_fg_made, rest) = rest in
      let (total_fg_att, rest) = rest in
      let (total_fg3_made, rest) = rest in
      let (total_fg3_att, rest) = rest in
      let (total_ft_made, total_ft_att) = rest in
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
        total_fg_made;
        total_fg_att;
        total_fg3_made;
        total_fg3_att;
        total_ft_made;
        total_ft_att;
      }
    in
    let t = t2 in
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
                                        (t float
                                          (t float
                                            (t int
                                              (t int
                                                (t int
                                                  (t int
                                                    (t int int))))))))))))))))))))))))

  let player_base =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (name, rest) = rest in
      let (team_name, rest) = rest in
      let (games_played, rest) = rest in
      let (min_seconds, rest) = rest in
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
      let (avg_turnovers, rest) = rest in
      let (efficiency, margin_seconds) = rest in
      Ok {
        pb_player_id = player_id;
        pb_player_name = name;
        pb_team_name = team_name;
        pb_gp = games_played;
        pb_min_seconds = min_seconds;
        pb_pts = total_points;
        pb_reb = total_rebounds;
        pb_ast = total_assists;
        pb_stl = total_steals;
        pb_blk = total_blocks;
        pb_tov = total_turnovers;
        pb_avg_pts = avg_points;
        pb_margin = avg_margin;
        pb_avg_reb = avg_rebounds;
        pb_avg_ast = avg_assists;
        pb_avg_stl = avg_steals;
        pb_avg_blk = avg_blocks;
        pb_avg_tov = avg_turnovers;
        pb_eff = efficiency;
        pb_margin_seconds = margin_seconds;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
        (t string
          (t string
            (t int
              (t int
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
                                        (t float
                                          (t float int)))))))))))))))))))

  let season_info =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (code, name) = Ok { code; name } in
    custom ~encode ~decode (t2 string string)

  let team_info =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (team_code, team_name) = Ok { team_code; team_name } in
    custom ~encode ~decode (t2 string string)

  let team_totals =
    let encode _ = Error "Encode not supported: read-only type" in
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
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t int (t float (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int int)))))))))))))))))

  let team_margin =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (season, (team, (gp, (pts_for, pts_against)))) =
      Ok { season; team; gp; pts_for; pts_against }
    in
    custom ~encode ~decode (t2 string (t2 string (t2 int (t2 int int))))

  let team_standing =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (team_name, (games_played, (wins, (losses, (avg_pts, avg_opp_pts))))) =
      let win_pct = if games_played = 0 then 0.0 else Stdlib.float wins /. Stdlib.float games_played in
      let diff = avg_pts -. avg_opp_pts in
      Ok { team_name; games_played; wins; losses; win_pct; gb = 0.0; avg_pts; avg_opp_pts; diff }
    in
    custom ~encode ~decode (t2 string (t2 int (t2 int (t2 int (t2 float float)))))

  let game_summary =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (home_team, (away_team, (home_score, (away_score, game_type)))))) =
      Ok { game_id; game_date; home_team; away_team; home_score; away_score; game_type }
    in
    custom ~encode ~decode
      (t2 string (t2 string (t2 string (t2 string (t2 (option int) (t2 (option int) string))))))

  let qa_score_mismatch =
    let encode _ = Error "Encode not supported: read-only type" in
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
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t string
                  (t (option int)
                     (t (option int)
                        (t (option int) (option int))))))))

  let qa_team_count_anomaly =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, team_count) = Ok { qtca_game_id = game_id; qtca_team_count = team_count } in
    custom ~encode ~decode (t2 string int)

  let qa_duplicate_player_row =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (team_code, (team_name, (player_id, (player_name, row_count))))) =
      Ok { qdpr_game_id = game_id;
           qdpr_team_code = team_code;
           qdpr_team_name = team_name;
           qdpr_player_id = player_id;
           qdpr_player_name = player_name;
           qdpr_row_count = row_count;
      }
    in
    let t = t2 in
    custom ~encode ~decode (t string (t string (t string (t string (t string int)))))

  let qa_duplicate_player_name_row =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_name, (id_count, player_ids_csv)) =
      Ok (player_name, id_count, player_ids_csv)
    in
    custom ~encode ~decode (t2 string (t2 int string))

  let qa_duplicate_player_identity_row =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_name, (birth_date, (id_count, player_ids_csv))) =
      Ok (player_name, birth_date, id_count, player_ids_csv)
    in
    custom ~encode ~decode (t2 string (t2 string (t2 int string)))

  let qa_schedule_missing_game =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_date, (season_code, (home_team, away_team))) =
      Ok {
        qsmg_game_date = game_date;
        qsmg_season_code = season_code;
        qsmg_home_team = home_team;
        qsmg_away_team = away_team;
      }
    in
    custom ~encode ~decode (t2 string (t2 string (t2 string string)))

  let qa_schedule_missing_stats =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (home_team, away_team))) =
      Ok {
        qsms_game_id = game_id;
        qsms_game_date = game_date;
        qsms_home_team = home_team;
        qsms_away_team = away_team;
      }
    in
    custom ~encode ~decode (t2 string (t2 string (t2 string string)))

  let qa_pbp_missing_game =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (home_team, (away_team, (home_score, away_score))))) =
      Ok {
        qpmg_game_id = game_id;
        qpmg_game_date = game_date;
        qpmg_home_team = home_team;
        qpmg_away_team = away_team;
        qpmg_home_score = home_score;
        qpmg_away_score = away_score;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t string
                  (t int int)))))

  let qa_schedule_coverage =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (season_code, (schedule_completed, (games_total, (matched, (missing, (coverage_pct, (season_uningested, games_missing_team))))))) =
      Ok {
        qsc_season_code = season_code;
        qsc_schedule_completed = schedule_completed;
        qsc_games_total = games_total;
        qsc_matched = matched;
        qsc_missing = missing;
        qsc_coverage_pct = coverage_pct;
        qsc_season_uningested = season_uningested;
        qsc_games_missing_team = games_missing_team;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t int
            (t int
               (t int
                  (t int
                     (t float
                        (t bool bool)))))))

  let qa_schedule_missing_summary =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (missing_ingested, (missing_uningested, missing_total)) =
      Ok {
        qsms_missing_ingested = missing_ingested;
        qsms_missing_uningested = missing_uningested;
        qsms_missing_total = missing_total;
      }
    in
    custom ~encode ~decode (t2 int (t2 int int))

  let qa_schedule_missing_sample =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (season_code, (game_date, (home_team_code, (away_team_code, (reason, (games_on_date, games_info)))))) =
      Ok {
        qsmp_season_code = season_code;
        qsmp_game_date = game_date;
        qsmp_home_team_code = home_team_code;
        qsmp_away_team_code = away_team_code;
        qsmp_reason = reason;
        qsmp_games_on_date = games_on_date;
        qsmp_games_info = games_info;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
        (t string
          (t string
            (t string
              (t string
                (t int (option string)))))))

  let qa_stat_anomaly =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (team_name, (player_id, (player_name, (min_seconds, (pts, (primary_team_name, (primary_gp, primary_min_seconds))))))))) =
      Ok {
        qsa_game_id = game_id;
        qsa_game_date = game_date;
        qsa_team_name = team_name;
        qsa_player_id = player_id;
        qsa_player_name = player_name;
        qsa_min_seconds = min_seconds;
        qsa_pts = pts;
        qsa_primary_team_name = primary_team_name;
        qsa_primary_gp = primary_gp;
        qsa_primary_min_seconds = primary_min_seconds;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t string
                  (t string
                     (t int
                        (t int
                           (t string
                              (t int int)))))))))

  let qa_stat_exclusion =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (team_name, (player_id, (player_name, (min_seconds, (pts, (reason, created_at)))))))) =
      Ok {
        qse_game_id = game_id;
        qse_game_date = game_date;
        qse_team_name = team_name;
        qse_player_id = player_id;
        qse_player_name = player_name;
        qse_min_seconds = min_seconds;
        qse_pts = pts;
        qse_reason = reason;
        qse_created_at = created_at;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t string
                  (t string
                     (t int
                        (t int
                           (t string string))))))))
  let boxscore_player_stat =
    let encode _ = Error "Encode not supported: read-only type" in
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
    let t = t2 in
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
    let encode _ = Error "Encode not supported: read-only type" in
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
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t string (t string (t string (t string (t int (t int int))))))))

  let leader_entry =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, (player_name, (team_name, stat_value))) =
      Ok { le_player_id = player_id; le_player_name = player_name; le_team_name = team_name; le_stat_value = stat_value }
    in
    custom ~encode ~decode (t2 string (t2 string (t2 string float)))

  let leader_base =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (player_name, rest) = rest in
      let (team_name, rest) = rest in
      let (gp, rest) = rest in
      let (min_seconds, rest) = rest in
      let (pts, rest) = rest in
      let (reb, rest) = rest in
      let (ast, rest) = rest in
      let (stl, rest) = rest in
      let (blk, rest) = rest in
      let (tov, rest) = rest in
      let (eff, rest) = rest in
      let (fg_m, rest) = rest in
      let (fg_a, rest) = rest in
      let (fg3_m, rest) = rest in
      let (fg3_a, rest) = rest in
      let (ft_m, ft_a) = rest in
      Ok {
        lb_player_id = player_id;
        lb_player_name = player_name;
        lb_team_name = team_name;
        lb_gp = gp;
        lb_min_seconds = min_seconds;
        lb_pts = pts;
        lb_reb = reb;
        lb_ast = ast;
        lb_stl = stl;
        lb_blk = blk;
        lb_tov = tov;
        lb_eff = eff;
        lb_fg_m = fg_m;
        lb_fg_a = fg_a;
        lb_fg3_m = fg3_m;
        lb_fg3_a = fg3_a;
        lb_ft_m = ft_m;
        lb_ft_a = ft_a;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t int
                  (t int
                     (t int
                        (t int
                           (t int
                              (t int
                                 (t int
                                    (t int
                                       (t float
         (t int
            (t int
               (t int
                  (t int
                     (t int int)))))))))))))))))

  let player_info =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (id, (name, (position, (birth_date, (height, weight))))) =
      Ok { id; name; position; birth_date; height; weight }
    in
    let t = t2 in
    custom ~encode ~decode (t string (t string (t (option string) (t (option string) (t (option int) (option int))))))

  let player_draft =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (draft_year, rest) = rest in
      let (draft_round, rest) = rest in
      let (pick_in_round, rest) = rest in
      let (overall_pick, rest) = rest in
      let (draft_team, rest) = rest in
      let (raw_text, rest) = rest in
      let (source_url, scraped_at) = rest in
      Ok {
        pd_player_id = player_id;
        pd_draft_year = draft_year;
        pd_draft_round = draft_round;
        pd_pick_in_round = pick_in_round;
        pd_overall_pick = overall_pick;
        pd_draft_team = draft_team;
        pd_raw_text = raw_text;
        pd_source_url = source_url;
        pd_scraped_at = scraped_at;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t (option int)
            (t (option int)
               (t (option int)
                  (t (option int)
                     (t (option string) (t string (t string string))))))))

  let draft_pick_row =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (player_name, rest) = rest in
      let (draft_year, rest) = rest in
      let (draft_round, rest) = rest in
      let (pick_in_round, rest) = rest in
      let (overall_pick, rest) = rest in
      let (draft_team, rest) = rest in
      let (raw_text, rest) = rest in
      let (source_url, scraped_at) = rest in
      Ok {
        dpr_player_id = player_id;
        dpr_player_name = player_name;
        dpr_draft_year = draft_year;
        dpr_draft_round = draft_round;
        dpr_pick_in_round = pick_in_round;
        dpr_overall_pick = overall_pick;
        dpr_draft_team = draft_team;
        dpr_raw_text = raw_text;
        dpr_source_url = source_url;
        dpr_scraped_at = scraped_at;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t (option int)
               (t (option int)
                  (t (option int)
                     (t (option int)
                        (t (option string) (t string (t string string)))))))))

	  let official_trade_event =
	    let encode _ = Error "Encode not supported: read-only type" in
	    let decode (event_date, rest) =
	      let (event_year, rest) = rest in
	      let (event_text, rest) = rest in
	      let (source_url, scraped_at) = rest in
	      Ok {
	        ote_event_date = event_date;
	        ote_event_year = event_year;
	        ote_event_text = event_text;
	        ote_source_url = source_url;
	        ote_scraped_at = scraped_at;
	      }
	    in
	    let t = t2 in
	    custom ~encode ~decode (t string (t int (t string (t string string))))

	  let player_external_link =
	    let encode _ = Error "Encode not supported: read-only type" in
	    let decode (player_id, rest) =
	      let (link_type, rest) = rest in
	      let (url, rest) = rest in
	      let (source_url, scraped_at) = rest in
	      Ok {
	        pel_player_id = player_id;
	        pel_link_type = link_type;
	        pel_url = url;
	        pel_source_url = source_url;
	        pel_scraped_at = scraped_at;
	      }
	    in
	    let t = t2 in
	    custom ~encode ~decode (t string (t string (t string (t (option string) string))))

	  let season_stats =
	    let encode _ = Error "Encode not supported: read-only type" in
	    let decode (season_code, rest) =
	      let (season_name, rest) = rest in
      let (team_name, rest) = rest in
      let (gp, rest) = rest in
      let (min, rest) = rest in
      let (pts, rest) = rest in
      let (reb, rest) = rest in
      let (ast, rest) = rest in
      let (stl, rest) = rest in
      let (blk, rest) = rest in
      let (tov, rest) = rest in
      let (eff, rest) = rest in
      let (margin, rest) = rest in
      let (fg_pct, rest) = rest in
      let (fg3_pct, rest) = rest in
      let (ft_pct, rest) = rest in
      let (ts, rest) = rest in
      let (efg, usg) = rest in
      Ok {
        ss_season_code = season_code;
        ss_season_name = season_name;
        ss_team_name = team_name;
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
        ss_fg_pct = fg_pct;
        ss_fg3_pct = fg3_pct;
        ss_ft_pct = ft_pct;
        ss_ts_pct = ts;
        ss_efg_pct = efg;
        ss_usg_pct = usg;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t string (t int (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float float))))))))))))))))))

  let player_game_stat =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (fg_made, (fg_att, (fg3_made, (fg3_att, (ft_made, (ft_att, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus)))))))))))))))))))) =
      let is_home = is_home_int = 1 in
      Ok { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; fg_made; fg_att; fg3_made; fg3_att; ft_made; ft_att; pts; reb; ast; stl; blk; tov; plus_minus }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t int
                  (t (option int)
                     (t (option int)
                        (t int
                           (t float
                              (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (option int)))))))))))))))))))))

  let player_game_stat_with_id =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (fg_made, (fg_att, (fg3_made, (fg3_att, (ft_made, (ft_att, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus))))))))))))))))))))) =
      let is_home = is_home_int = 1 in
      let stat = { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; fg_made; fg_att; fg3_made; fg3_att; ft_made; ft_att; pts; reb; ast; stl; blk; tov; plus_minus } in
      Ok { pgs_player_id = player_id; pgs_stat = stat }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string
         (t string
            (t string
               (t string
                  (t int
                     (t (option int)
                        (t (option int)
                           (t int
                              (t float
                                 (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (option int))))))))))))))))))))))

  let team_game_result =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opp_score, is_win_int)))))) =
      Ok { tgr_game_id = game_id; tgr_game_date = game_date; tgr_opponent = opponent; tgr_is_home = is_home_int = 1; tgr_team_score = team_score; tgr_opponent_score = opp_score; tgr_is_win = is_win_int = 1 }
    in
    let t = t2 in
    custom ~encode ~decode (t string (t string (t string (t int (t int (t int int))))))

  let h2h_game =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (p1_team, (p2_team, (p1_pts, (p1_reb, (p1_ast, (p1_stl, (p1_blk, (p2_pts, (p2_reb, (p2_ast, (p2_stl, (p2_blk, (winner, score_diff))))))))))))))) =
      Ok { hg_game_id = game_id; hg_game_date = game_date; player1_team = p1_team; player2_team = p2_team;
           player1_pts = p1_pts; player1_reb = p1_reb; player1_ast = p1_ast;
           player1_stl = p1_stl; player1_blk = p1_blk;
           player2_pts = p2_pts; player2_reb = p2_reb; player2_ast = p2_ast;
           player2_stl = p2_stl; player2_blk = p2_blk;
           winner_team = winner; score_diff }
    in
    let t = t2 in
    custom ~encode ~decode (t string (t string (t string (t string (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t string int)))))))))))))))

  let pbp_event =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (period_code, rest) =
      let (event_index, rest) = rest in
      let (team_side, rest) = rest in
      let (description, rest) = rest in
      let (team1_score, rest) = rest in
      let (team2_score, clock) = rest in
      Ok {
        pe_period_code = period_code;
        pe_event_index = event_index;
        pe_team_side = team_side;
        pe_description = description;
        pe_team1_score = team1_score;
        pe_team2_score = team2_score;
        pe_clock = clock;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t int (t int (t string (t (option int) (t (option int) string))))))

  (** Historical season type *)
  let historical_season =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (season_id, rest) =
      let (season_name, rest) = rest in
      let (champion_team, rest) = rest in
      let (runner_up, rest) = rest in
      let (regular_mvp, rest) = rest in
      let (finals_mvp, rest) = rest in
      let (rookie_of_year, rest) = rest in
      let (scoring_leader, notes) = rest in
      Ok {
        hs_season_id = season_id;
        hs_season_name = season_name;
        hs_champion_team = champion_team;
        hs_runner_up = runner_up;
        hs_regular_mvp = regular_mvp;
        hs_finals_mvp = finals_mvp;
        hs_rookie_of_year = rookie_of_year;
        hs_scoring_leader = scoring_leader;
        hs_notes = notes;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t (option string) (t (option string) (t (option string) (t (option string) (t (option string) (t (option string) (option string)))))))))

  (** Legend player type *)
  let legend_player =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_name, rest) =
      let (career_years, rest) = rest in
      let (teams, rest) = rest in
      let (championships, rest) = rest in
      let (mvp_count, rest) = rest in
      let (all_star_count, rest) = rest in
      let (career_points, rest) = rest in
      let (career_rebounds, rest) = rest in
      let (career_assists, rest) = rest in
      let (notable_achievements, is_hall_of_fame) = rest in
      Ok {
        lp_player_name = player_name;
        lp_career_years = career_years;
        lp_teams = teams;
        lp_championships = championships;
        lp_mvp_count = mvp_count;
        lp_all_star_count = all_star_count;
        lp_career_points = career_points;
        lp_career_rebounds = career_rebounds;
        lp_career_assists = career_assists;
        lp_notable_achievements = notable_achievements;
        lp_is_hall_of_fame = is_hall_of_fame;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t (option string) (t (option string) (t int (t int (t int (t int (t int (t int (t (option string) bool))))))))))

  (** Coach type *)
  let coach =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (coach_name, rest) =
      let (team, rest) = rest in
      let (tenure_start, rest) = rest in
      let (tenure_end, rest) = rest in
      let (championships, rest) = rest in
      let (regular_season_wins, rest) = rest in
      let (playoff_wins, rest) = rest in
      let (former_player, rest) = rest in
      let (player_career_years, notable_achievements) = rest in
      Ok {
        c_coach_name = coach_name;
        c_team = team;
        c_tenure_start = tenure_start;
        c_tenure_end = tenure_end;
        c_championships = championships;
        c_regular_season_wins = regular_season_wins;
        c_playoff_wins = playoff_wins;
        c_former_player = former_player;
        c_player_career_years = player_career_years;
        c_notable_achievements = notable_achievements;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t (option string) (t (option int) (t (option int) (t int (t int (t int (t bool (t (option string) (option string))))))))))

  (** Player career entry type *)
  let player_career_entry =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_name, rest) =
      let (season_id, rest) = rest in
      let (team, rest) = rest in
      let (jersey_number, rest) = rest in
      let (games_played, rest) = rest in
      let (points_per_game, rest) = rest in
      let (rebounds_per_game, rest) = rest in
      let (assists_per_game, rest) = rest in
      let (is_allstar, awards) = rest in
      Ok {
        pce_player_name = player_name;
        pce_season_id = season_id;
        pce_team = team;
        pce_jersey_number = jersey_number;
        pce_games_played = games_played;
        pce_points_per_game = points_per_game;
        pce_rebounds_per_game = rebounds_per_game;
        pce_assists_per_game = assists_per_game;
        pce_is_allstar = is_allstar;
        pce_awards = awards;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t string (t (option int) (t (option int) (t (option float) (t (option float) (t (option float) (t bool (option string))))))))))

  (** MVP candidate type for MVP Race feature *)
  let mvp_candidate =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (player_name, rest) = rest in
      let (team_name, rest) = rest in
      let (games_played, rest) = rest in
      let (ppg, rest) = rest in
      let (rpg, rest) = rest in
      let (apg, rest) = rest in
      let (spg, rest) = rest in
      let (bpg, rest) = rest in
      let (efficiency, rest) = rest in
      let (team_wins, rest) = rest in
      let (team_losses, team_win_pct) = rest in
      let team_code = team_code_of_string team_name in
      let (base_score, win_bonus, final_score) = calculate_mvp_score ~ppg ~rpg ~apg ~spg ~bpg ~efficiency ~win_pct:team_win_pct in
      Ok {
        mvp_rank = 0;  (* Will be set during list processing *)
        mvp_player_id = player_id;
        mvp_player_name = player_name;
        mvp_team_name = team_name;
        mvp_team_code = team_code;
        mvp_games_played = games_played;
        mvp_ppg = ppg;
        mvp_rpg = rpg;
        mvp_apg = apg;
        mvp_spg = spg;
        mvp_bpg = bpg;
        mvp_efficiency = efficiency;
        mvp_team_wins = team_wins;
        mvp_team_losses = team_losses;
        mvp_team_win_pct = team_win_pct;
        mvp_base_score = base_score;
        mvp_win_bonus = win_bonus;
        mvp_final_score = final_score;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t string (t int (t float (t float (t float (t float (t float (t float (t int (t int float))))))))))))

  (** Clutch time statistics type
      Aggregates scoring events from Q4 last 5 minutes with score diff <= 5 *)
  let clutch_stats =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (player_name, rest) = rest in
      let (team_name, rest) = rest in
      let (clutch_games, rest) = rest in
      let (clutch_points, rest) = rest in
      let (clutch_fg_made, rest) = rest in
      let (clutch_fg_att, rest) = rest in
      let (clutch_fg_pct, rest) = rest in
      let (clutch_ft_made, rest) = rest in
      let (clutch_ft_att, clutch_3p_made) = rest in
      Ok {
        cs_player_id = player_id;
        cs_player_name = player_name;
        cs_team_name = team_name;
        cs_clutch_games = clutch_games;
        cs_clutch_points = clutch_points;
        cs_clutch_fg_made = clutch_fg_made;
        cs_clutch_fg_att = clutch_fg_att;
        cs_clutch_fg_pct = clutch_fg_pct;
        cs_clutch_ft_made = clutch_ft_made;
        cs_clutch_ft_att = clutch_ft_att;
        cs_clutch_3p_made = clutch_3p_made;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t string (t int (t int (t int (t int (t float (t int (t int int))))))))))

  (** Player shooting stats for shot distribution chart *)
  let player_shooting_stats =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, rest) =
      let (name, rest) = rest in
      let (games, rest) = rest in
      let (fg_made, rest) = rest in
      let (fg_attempted, rest) = rest in
      let (fg_pct, rest) = rest in
      let (fg3_made, rest) = rest in
      let (fg3_attempted, rest) = rest in
      let (fg3_pct, rest) = rest in
      let (ft_made, rest) = rest in
      let (ft_attempted, ft_pct) = rest in
      Ok {
        pss_player_id = player_id;
        pss_name = name;
        pss_games = games;
        pss_fg_made = fg_made;
        pss_fg_attempted = fg_attempted;
        pss_fg_pct = fg_pct;
        pss_fg3_made = fg3_made;
        pss_fg3_attempted = fg3_attempted;
        pss_fg3_pct = fg3_pct;
        pss_ft_made = ft_made;
        pss_ft_attempted = ft_attempted;
        pss_ft_pct = ft_pct;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t int (t int (t int (t float (t int (t int (t float (t int (t int float)))))))))))
end

(** Use oneshot queries to avoid prepared-statement conflicts with PgBouncer. *)
module Request_oneshot = struct
  include Caqti_request.Infix

  let ( ->. ) pt rt ?(oneshot = true) s =
    Caqti_request.Infix.( ->. ) pt rt ~oneshot s

  let ( ->? ) pt rt ?(oneshot = true) s =
    Caqti_request.Infix.( ->? ) pt rt ~oneshot s

  let ( ->* ) pt rt ?(oneshot = true) s =
    Caqti_request.Infix.( ->* ) pt rt ~oneshot s
end

(** SQL Queries *)
module Queries = struct
  open Request_oneshot
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
      ('82', '올스타A', 'All-Star A'),
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

  let qa_stat_exclusions = (t2 string string ->* Types.qa_stat_exclusion) {|
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

  let qa_stat_anomaly_candidates = (t2 string string ->* Types.qa_stat_anomaly) {|
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
    DROP MATERIALIZED VIEW IF EXISTS games_calc CASCADE
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
  let pbp_events_by_game_period = (t2 string string ->* Types.pbp_event) {|
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
  let all_teams = (unit ->* Types.team_info) "SELECT team_code, team_name_kr FROM teams ORDER BY team_name_kr"
  let all_seasons = (unit ->* Types.season_info) {|
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
  let all_historical_seasons = (unit ->* Types.historical_season) {|
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
  let all_legend_players = (unit ->* Types.legend_player) {|
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
  let all_coaches = (unit ->* Types.coach) {|
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
  let player_career_by_name = (string ->* Types.player_career_entry) {|
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

  let player_stats_base = (t2 (t2 string string) int ->* Types.player_aggregate) {|
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
  let player_stats_by_season_base = (t2 string (t2 string int) ->* Types.player_base) {|
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
  let player_career_aggregate = (string ->? Types.player_aggregate) {|
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

  let player_aggregate_by_id = (t2 string (t2 string (t2 string (t2 string string))) ->? Types.player_aggregate) {|
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
  let player_shooting_stats_by_id = (t2 string (t2 string string) ->? Types.player_shooting_stats) {|
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

	  let team_totals_by_season = (t2 string (t2 string (t2 string int)) ->* Types.team_totals) {|
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
	  let team_margin_by_season = (t2 string int ->* Types.team_margin) {|
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

  let team_standings_by_season = (string ->* Types.team_standing) {|
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

  let all_games_paginated = (t2 (t2 string string) (t2 int int) ->* Types.game_summary) {|
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

  let scored_games_by_season = (t2 string (t2 string int) ->* Types.game_summary) {|
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

  let qa_pbp_missing_games_sample_by_season = (string ->* Types.qa_pbp_missing_game) {|
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

  let qa_schedule_missing_game_sample = (unit ->* Types.qa_schedule_missing_game) {|
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

  let qa_schedule_missing_stats_sample = (unit ->* Types.qa_schedule_missing_stats) {|
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

  let qa_schedule_coverage = (unit ->* Types.qa_schedule_coverage) {|
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

  let qa_score_mismatch_sample = (unit ->* Types.qa_score_mismatch) {|
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

  let qa_team_count_anomaly_sample = (unit ->* Types.qa_team_count_anomaly) {|
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

	  let qa_duplicate_player_row_sample = (unit ->* Types.qa_duplicate_player_row) {|
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
	
	  let qa_duplicate_player_name_sample = (unit ->* Types.qa_duplicate_player_name_row) {|
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

  let qa_schedule_missing_summary = (unit ->? Types.qa_schedule_missing_summary) {|
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

  let qa_schedule_missing_samples = (unit ->* Types.qa_schedule_missing_sample) {|
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

  let qa_duplicate_player_identity_sample = (unit ->* Types.qa_duplicate_player_identity_row) {|
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
  let game_info_by_id = (string ->? Types.game_info) {|
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
  let boxscore_stats_by_game_id = (string ->* Types.boxscore_player_stat) {|
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
  let players_by_team = (t2 string (t2 string (t2 string int)) ->* Types.player_aggregate) {|
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
	  let leaders_base_stats = (t2 string string ->* Types.leader_base) {|
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
  let leaders_base_cached = (string ->* Types.leader_base) {|
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
  let leaders_base_cached_all = (unit ->* Types.leader_base) {|
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

  let leaders_pts = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.pts) DESC LIMIT 5 |}
  let leaders_pts_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.pts) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_reb = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.reb_tot) DESC LIMIT 5 |}
  let leaders_reb_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.reb_tot) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.reb_tot) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_ast = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.ast) DESC LIMIT 5 |}
  let leaders_ast_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ast) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.ast) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_stl = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.stl) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.stl) DESC LIMIT 5 |}
  let leaders_stl_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.stl) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.stl) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_blk = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.blk) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.blk) DESC LIMIT 5 |}
  let leaders_blk_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.blk) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.blk) / SUM(s.min_seconds)) DESC LIMIT 5 |}

  (* Leaders - extended (basketball-reference style) *)
  let leaders_gp = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, COUNT(*) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY COUNT(*) DESC LIMIT 5 |}
  let leaders_min = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.min_seconds) / 60.0 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.min_seconds) DESC LIMIT 5 |}
  let leaders_min_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.min_seconds) / 60.0 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY SUM(s.min_seconds) DESC LIMIT 5 |}

  let leaders_pts_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.pts) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.pts) DESC LIMIT 5 |}
  let leaders_reb_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.reb_tot) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.reb_tot) DESC LIMIT 5 |}
  let leaders_ast_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.ast) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.ast) DESC LIMIT 5 |}
  let leaders_stl_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.stl) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.stl) DESC LIMIT 5 |}
  let leaders_blk_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.blk) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.blk) DESC LIMIT 5 |}

  let leaders_tov = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.tov) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.tov) DESC LIMIT 5 |}
  let leaders_tov_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.tov) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.tov) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_tov_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.tov) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.tov) DESC LIMIT 5 |}

  let leaders_eff = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.game_score) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.game_score) DESC LIMIT 5 |}
  let leaders_eff_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds)) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.game_score) / SUM(s.min_seconds)) DESC LIMIT 5 |}

  let leaders_fg_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY ((SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}
  let leaders_fg3_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_3p_a) >= 20 ORDER BY ((SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0)) DESC LIMIT 5 |}
  let leaders_ft_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.ft_a) >= 20 ORDER BY ((SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0)) DESC LIMIT 5 |}
  let leaders_ts_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING (SUM(s.fg_2p_a + s.fg_3p_a) + SUM(s.ft_a)) >= 50 ORDER BY ((SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0)) DESC LIMIT 5 |}
  let leaders_efg_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, ((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats_clean s JOIN player_identities pi ON pi.player_id = s.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY (((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}
  let leaders_usg_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(u.usg_pct) FROM player_usg_stats u JOIN player_identities pi ON pi.player_id = u.player_id JOIN players p ON p.player_id = pi.canonical_player_id JOIN teams t ON u.team_code = t.team_code JOIN games g ON g.game_id = u.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' AND u.usg_pct IS NOT NULL GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 AND SUM(u.min_seconds) >= 6000 ORDER BY AVG(u.usg_pct) DESC LIMIT 5 |}

  (** Position-filtered leader queries
      Position values: 'G' (Guard), 'F' (Forward), 'C' (Center), 'ALL' (no filter)
  *)
	  let leaders_pts_by_position = (t2 (t2 string string) string ->* Types.leader_entry) {|
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
	  let leaders_reb_by_position = (t2 (t2 string string) string ->* Types.leader_entry) {|
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
	  let leaders_ast_by_position = (t2 (t2 string string) string ->* Types.leader_entry) {|
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
	  let leaders_eff_by_position = (t2 (t2 string string) string ->* Types.leader_entry) {|
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
	  let stat_mvp_eff = (t2 string (t2 string int) ->* Types.leader_entry) {|
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

	  let stat_mip_eff_delta = (t2 string (t2 int (t2 string int)) ->* Types.leader_entry) {|
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
  let player_info = (string ->? Types.player_info) {|
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
  let all_player_info = (unit ->* Types.player_info)
    "SELECT player_id, player_name, position, birth_date, height, weight FROM players"

  let player_draft_by_player_id = (string ->? Types.player_draft) {|
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

  let draft_picks_filtered = (t2 (t2 int int) (t2 string string) ->* Types.draft_pick_row) {|
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

  let official_trade_events_by_player_name_norm = (string ->* Types.official_trade_event) {|
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

  let official_trade_events_filtered = (t2 (t2 int int) string ->* Types.official_trade_event) {|
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

  let player_external_links_by_player_id = (string ->* Types.player_external_link) {|
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
  let player_seasons_per_game = (string ->* Types.season_stats) (make_season_stats_sql `Per_game)

  (** Queries for Totals (Sum) *)
  let player_seasons_totals = (string ->* Types.season_stats) (make_season_stats_sql `Totals)

  (** Queries for Per 36 Minutes (Normalized) *)
  let player_seasons_per36 = (string ->* Types.season_stats) (make_season_stats_sql `Per_36)

	  let player_recent_games = (string ->* Types.player_game_stat) {|
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
	  let player_game_logs = (t2 string (t2 string (t2 string int)) ->* Types.player_game_stat) {|
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
	  let batch_player_game_logs = (t2 string (t2 string string) ->* Types.player_game_stat_with_id) {|
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
	  let player_all_star_games = (string ->* Types.player_game_stat) {|
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
	  let career_high_points_game = (string ->? Types.player_game_stat) {|
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
	  let career_high_rebounds_game = (string ->? Types.player_game_stat) {|
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
	  let career_high_assists_game = (string ->? Types.player_game_stat) {|
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
	  let career_high_steals_game = (string ->? Types.player_game_stat) {|
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
	  let career_high_blocks_game = (string ->? Types.player_game_stat) {|
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

		  let team_recent_games = (let t = t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string string)))))))) in t ->* Types.team_game_result) {|
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
		  let team_games = (let t = t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string string)))))))) in t ->* Types.team_game_result) {|
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
		  let player_by_name = (t2 string (t2 string string) ->? Types.player_aggregate) {|
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
  let player_h2h_games = (t2 (t2 string string) (t2 string string) ->* Types.h2h_game) {|
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
  let team_h2h_games = (t2 (t2 (t2 string string) (t2 string string)) (t2 string string) ->* Types.game_info) {|
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
    (params ->* Types.mvp_candidate)
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
  let clutch_pbp_events = (string ->* Types.pbp_event) {|
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
	  let clutch_stats_by_season = (string ->* Types.clutch_stats) {|
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
end

(** Database operations *)
  module Repo = struct
	    let ensure_schema (module Db : Caqti_eio.CONNECTION) =
	      let (let*) = Result.bind in
	      let* () = Db.exec Queries.ensure_legend_players_table () in
	      let* () = Db.exec Queries.seed_legend_players () in
	      let* () = Db.exec Queries.seed_historical_teams () in
	      (* Data-quality overrides must exist before any derived views/materialized views. *)
	      let* () = Db.exec Queries.ensure_game_stats_exclusions_table () in
	      let* () = Db.exec Queries.ensure_game_stats_clean_view () in
	      
	      (* Use games_calc_v3 instead of dropping old view synchronously *)
	      (* let* () = Db.exec Queries.drop_games_calc_view () in *)
	      let* () = Db.exec Queries.ensure_games_calc_matview () in
      let* () = Db.exec Queries.ensure_games_calc_index () in
      let* () = Db.exec Queries.ensure_games_calc_season_index () in

      let* () = Db.exec Queries.ensure_player_plus_minus_table () in
      let* () = Db.exec Queries.ensure_player_plus_minus_index () in
      let* () = Db.exec Queries.ensure_play_by_play_events_table () in
      let* () = Db.exec Queries.ensure_play_by_play_events_player_id_column () in
      let* () = Db.exec Queries.ensure_play_by_play_events_index_game () in
      let* () = Db.exec Queries.ensure_play_by_play_events_index_game_period () in
      let* () = Db.exec Queries.ensure_play_by_play_events_index_player () in
      let* () = Db.exec Queries.ensure_player_drafts_table () in
      let* () = Db.exec Queries.ensure_player_drafts_index_year () in
	      let* () = Db.exec Queries.ensure_official_trade_events_table () in
	      let* () = Db.exec Queries.ensure_official_trade_events_index_date () in
      let* () = Db.exec Queries.ensure_official_trade_events_index_year () in
      let* () = Db.exec Queries.ensure_player_external_links_table () in
      let* () = Db.exec Queries.ensure_player_external_links_index_player () in
      (* Performance indexes for game_stats - critical for fast queries *)
      let* () = Db.exec Queries.ensure_game_stats_index_game () in
      let* () = Db.exec Queries.ensure_game_stats_index_player () in
      let* () = Db.exec Queries.ensure_game_stats_index_team () in
      let* () = Db.exec Queries.ensure_game_stats_index_game_team () in
      let* () = Db.exec Queries.ensure_game_stats_index_player () in
      let* () = Db.exec Queries.ensure_games_index_season_type () in
      let* () = Db.exec Queries.ensure_games_index_date () in
      let* () = Db.exec Queries.ensure_legend_players_index_name () in
      let* () = Db.exec Queries.ensure_player_plus_minus_table () in
      (* Schedule table for upcoming games *)
      let* () = Db.exec Queries.ensure_schedule_table () in
      let* () = Db.exec Queries.ensure_schedule_index_date () in
      let* () = Db.exec Queries.ensure_schedule_index_status () in
      let* () = Db.exec Queries.ensure_schedule_index_season () in
	      (* Canonicalize duplicate player identities (same name + birth_date). *)
	      let* () = Db.exec Queries.ensure_player_identities_view () in
	      (* Leaders base cache MATERIALIZED VIEW - drop first to update schema *)
	      let* () = Db.exec Queries.drop_leaders_base_cache () in
	      let* () = Db.exec Queries.ensure_leaders_base_cache_view () in
	      let* () = Db.exec Queries.ensure_leaders_base_cache_unique () in
      let* () = Db.exec Queries.ensure_leaders_base_cache_index_season () in
      let* () = Db.exec Queries.refresh_leaders_base_cache () in
      (* Migration: Drop old regular VIEWs before creating MATERIALIZED VIEWs *)
      let* () = Db.exec Queries.drop_score_mismatch_view () in
      let* () = Db.exec Queries.drop_games_calc_view () in
      (* Materialized Views for performance - pre-computed and cached *)
      let* () = Db.exec Queries.ensure_score_mismatch_matview () in
      let* () = Db.exec Queries.ensure_score_mismatch_index () in
      let* () = Db.exec Queries.ensure_games_calc_matview () in
      let* () = Db.exec Queries.ensure_games_calc_index () in
      let* () = Db.exec Queries.ensure_games_calc_season_index () in
      (* Awards table for scraped WKBL award data *)
      let* () = Db.exec Queries.ensure_awards_table () in
      let* () = Db.exec Queries.ensure_awards_index_season () in
      let* () = Db.exec Queries.ensure_awards_index_category () in
      Db.exec Queries.ensure_awards_index_player ()

    (* Refresh materialized views - call after data sync *)
    let refresh_matviews (module Db : Caqti_eio.CONNECTION) =
      let (let*) = Result.bind in
      let* () = Db.exec Queries.refresh_leaders_base_cache () in
      let* () = Db.exec Queries.refresh_games_calc () in
      Db.exec Queries.refresh_score_mismatch ()
  let get_teams (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_teams ()
  let get_seasons (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_seasons ()
  let get_all_player_info (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_player_info ()
  let get_latest_game_date (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.latest_game_date ()
  let get_historical_seasons (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_historical_seasons ()
  let get_legend_players (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_legend_players ()
  let get_quarter_scores_raw (module Db : Caqti_eio.CONNECTION) game_id = Db.collect_list Queries.quarter_scores_by_game game_id
  let get_pbp_data_quality (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.pbp_data_quality ()
  let get_games_with_incomplete_pbp (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.games_with_incomplete_pbp ()
  let get_coaches (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_coaches ()
  let get_player_career ~player_name (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.player_career_by_name player_name
  let get_player_by_name ~name ~season (module Db : Caqti_eio.CONNECTION) = let s = if season = "" then "ALL" else season in Db.find_opt Queries.player_by_name (name, (s, s))
  let get_player_aggregate_by_id ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt Queries.player_aggregate_by_id (s, (s, (player_id, (s, s))))

  let get_player_shooting_stats ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt Queries.player_shooting_stats_by_id (player_id, (s, s))

  (* Shot chart data: zone stats + player info *)
  let get_player_shot_chart ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let open Domain in
    let s = if String.trim season = "" then "ALL" else season in
    let (let*) = Result.bind in
    let* zone_rows = Db.collect_list Queries.player_shot_stats (player_id, s) in
    let* player_info = Db.find_opt Queries.player_shot_info player_id in
    let pid, pname, tname = match player_info with
      | Some (id, name, team) -> id, name, team
      | None -> player_id, "Unknown", "Unknown"
    in
    (* Parse zone rows into raw stats.
       PBP data quirk: "페인트존" only appears on successful shots.
       Missed paint shots are tagged as generic "2점슛시도", so we cannot
       compute paint FG%. We merge paint + mid into a single 2PT zone. *)
    let paint_made = ref 0 in
    let mid_made = ref 0 in
    let mid_attempts = ref 0 in
    let three_made = ref 0 in
    let three_attempts = ref 0 in
    List.iter (fun (zone_str, made, attempts, _pct) ->
      match zone_str with
      | "paint" ->
          (* All paint records are successes — attempts = made *)
          paint_made := made
      | "mid" ->
          (* "mid" includes paint zone misses disguised as "2점슛시도" *)
          mid_made := made;
          mid_attempts := attempts
      | "three" ->
          three_made := made;
          three_attempts := attempts
      | _ -> ()
    ) zone_rows;
    (* Combine: 2PT = paint successes + mid successes / paint attempts(=made) + mid attempts *)
    let two_pt_made = !paint_made + !mid_made in
    let two_pt_attempts = !paint_made + !mid_attempts in
    let two_pt_pct = if two_pt_attempts > 0 then
      float_of_int two_pt_made /. float_of_int two_pt_attempts *. 100.0
    else 0.0 in
    let three_pct = if !three_attempts > 0 then
      float_of_int !three_made /. float_of_int !three_attempts *. 100.0
    else 0.0 in
    let total_made = two_pt_made + !three_made in
    let total_attempts = two_pt_attempts + !three_attempts in
    let total_pct = if total_attempts > 0 then (float_of_int total_made /. float_of_int total_attempts) *. 100.0 else 0.0 in
    Ok {
      psc_player_id = pid;
      psc_player_name = pname;
      psc_team_name = tname;
      psc_two_pt = { zs_zone = MidRange; zs_made = two_pt_made; zs_attempts = two_pt_attempts; zs_pct = two_pt_pct };
      psc_three = { zs_zone = ThreePoint; zs_made = !three_made; zs_attempts = !three_attempts; zs_pct = three_pct };
      psc_paint_made = !paint_made;
      psc_total_made = total_made;
      psc_total_attempts = total_attempts;
      psc_total_pct = total_pct;
    }

  let get_draft_years (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.draft_years ()

  let get_draft_picks ~year ~search (module Db : Caqti_eio.CONNECTION) =
    let pattern = normalize_search_pattern (normalize_label search) in
    Db.collect_list Queries.draft_picks_filtered ((year, year), (pattern, pattern))

  let get_official_trade_years (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.official_trade_years ()

  let get_official_trade_events ~year ~search (module Db : Caqti_eio.CONNECTION) =
    let pattern = normalize_search_pattern (normalize_label search) in
    Db.collect_list Queries.official_trade_events_filtered ((year, year), pattern)

  (* Schedule queries *)
  let get_upcoming_schedule ~status ~limit (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.get_upcoming_schedule (status, limit)

  let get_schedule_by_date_range ~start_date ~end_date ~status (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.get_schedule_by_date_range (start_date, (end_date, (status, status)))

  (** Upsert a single schedule entry *)
  let upsert_schedule_entry ~game_date ~game_time ~season_code ~home_team_code ~away_team_code ~venue ~status (module Db : Caqti_eio.CONNECTION) =
    Db.exec Queries.upsert_schedule (game_date, (game_time, (season_code, (home_team_code, (away_team_code, (venue, status))))))

  (** Delete schedule entries for a season (derived data, safe to rebuild) *)
  let delete_schedule_by_season ~season_code (module Db : Caqti_eio.CONNECTION) =
    Db.exec Queries.delete_schedule_by_season season_code

  (** Upsert a single season entry *)
  let upsert_season ~season_code ~season_name (module Db : Caqti_eio.CONNECTION) =
    Db.exec Queries.upsert_season (season_code, season_name)

  (* Awards queries *)
  let upsert_award ~season_name ~category ~award_type ~player_name ~stat_value ~votes (module Db : Caqti_eio.CONNECTION) =
    Db.exec Queries.upsert_award (season_name, (category, (award_type, (player_name, (stat_value, votes)))))

  let get_awards_by_category ~category (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.awards_by_category category

  let get_awards_by_season ~season_name (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.awards_by_season season_name

  let get_awards_by_player ~player_name (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.awards_by_player player_name

  let get_award_count (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt Queries.count_awards ()

  let get_award_leaders ~category (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.award_leaders category

  (** Data-quality overrides: exclude/restore a (game_id, player_id) stat row *)
  let upsert_game_stats_exclusion ~game_id ~player_id ~reason (module Db : Caqti_eio.CONNECTION) =
    Db.exec Queries.upsert_game_stats_exclusion (game_id, (player_id, reason))

  let delete_game_stats_exclusion ~game_id ~player_id (module Db : Caqti_eio.CONNECTION) =
    Db.exec Queries.delete_game_stats_exclusion (game_id, player_id)

  let qa_stat_exclusions ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.qa_stat_exclusions (s, s)

  let qa_stat_anomaly_candidates ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.qa_stat_anomaly_candidates (s, s)

  (** Upsert a single game row *)
  let upsert_game_entry
      ~game_id
      ~season_code
      ~game_type
      ~game_no
      ~game_date
      ~home_team_code
      ~away_team_code
      ~home_score
      ~away_score
      ~stadium
      ~attendance
      (module Db : Caqti_eio.CONNECTION) =
    let game_date_opt =
      match game_date with
      | None -> None
      | Some s ->
          let trimmed = String.trim s in
          if trimmed = "" then None else Some trimmed
    in
    Db.exec Queries.upsert_game
      (game_id,
        (season_code,
          (game_type,
            (game_no,
              (game_date_opt,
                (home_team_code,
                  (away_team_code,
                    (home_score,
                      (away_score,
                        (stadium, attendance))))))))))

  (** Upsert a game record (scores + metadata) *)
  let upsert_game
      ~game_id ~season_code ~game_type ~game_no ~game_date
      ~home_team_code ~away_team_code ~home_score ~away_score ~stadium
      (module Db : Caqti_eio.CONNECTION) =
    upsert_game_entry
      ~game_id
      ~season_code
      ~game_type
      ~game_no
      ~game_date
      ~home_team_code
      ~away_team_code
      ~home_score
      ~away_score
      ~stadium
      ~attendance:None
      (module Db)
  (** Count schedule entries by season and status *)
  let count_schedule_by_status ~season_code ~status (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt Queries.count_schedule_by_status (season_code, status)

  let get_players_base ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.player_stats_by_season_base (s, (s, include_int))
  let get_top_players ?(team_name="ALL") ~limit (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.player_stats_base ((team_name, team_name), limit)
  let get_players_by_team ~team_name ~season ~limit (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.players_by_team (team_name, (s, (s, limit)))
  let get_team_totals ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.team_totals_by_season (season, (season, (season, include_int)))

  let get_team_margins ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.team_margin_by_season (season, include_int)
  let get_standings ~season (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.team_standings_by_season season
  let get_games ?(page = 1) ?(page_size = 50) ~season (module Db : Caqti_eio.CONNECTION) =
    let offset = (page - 1) * page_size in
    Db.collect_list Queries.all_games_paginated ((season, season), (page_size, offset))
  let get_scored_games ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.scored_games_by_season (season, (season, include_int))
  let get_game_season_code ~game_id (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.game_season_by_id game_id
  let get_pbp_periods ~game_id (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.pbp_periods_by_game game_id
  let get_pbp_events ~game_id ~period_code (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.pbp_events_by_game_period (game_id, period_code)
  let get_team_core_player_ids ~season ~team_name ~limit (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.team_core_player_ids (team_name, ((season, season), limit))
  let get_team_active_player_ids ~team_name ~game_id (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.team_active_player_ids (team_name, game_id)

  let qa_games_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_games_total ()
  let qa_finished_games_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_finished_games_total ()
  let qa_games_with_stats (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_games_with_stats ()
  let qa_pbp_games (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_pbp_games ()
  let qa_plus_minus_games (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_plus_minus_games ()
  let qa_finished_games_total_by_season ~season (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt Queries.qa_finished_games_total_by_season season
  let qa_pbp_games_by_season ~season (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt Queries.qa_pbp_games_by_season season
  let qa_pbp_missing_games_sample_by_season ~season (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.qa_pbp_missing_games_sample_by_season season
  let qa_score_mismatch_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_score_mismatch_count ()
  let qa_score_mismatch_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_score_mismatch_sample ()
  let qa_team_count_anomaly_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_team_count_anomaly_count ()
  let qa_team_count_anomaly_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_team_count_anomaly_sample ()
  let qa_duplicate_player_row_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_row_count ()
  let qa_duplicate_player_row_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_row_sample ()
  let qa_duplicate_player_name_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_name_count ()
  let qa_duplicate_player_name_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_name_sample ()
  let qa_duplicate_player_identity_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_identity_count ()
  let qa_duplicate_player_identity_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_identity_sample ()
  let qa_schedule_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_schedule_total ()
  let qa_schedule_completed (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_schedule_completed ()
  let qa_schedule_missing_game_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_schedule_missing_game_count ()
  let qa_schedule_missing_game_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_schedule_missing_game_sample ()
  let qa_schedule_missing_stats_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_schedule_missing_stats_count ()
  let qa_schedule_missing_stats_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_schedule_missing_stats_sample ()
  let qa_schedule_coverage (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_schedule_coverage ()
  let qa_schedule_missing_summary (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_schedule_missing_summary ()
  let qa_schedule_missing_reason_counts (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_schedule_missing_reason_counts ()
  let qa_schedule_missing_samples (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_schedule_missing_samples ()
  let get_game_info ~game_id (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.game_info_by_id game_id
  let get_boxscore_stats ~game_id (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.boxscore_stats_by_game_id game_id
  let get_leaders ~category ~scope ~season (module Db : Caqti_eio.CONNECTION) =
    let q =
      match (category, scope) with
      | "gp", _ -> Queries.leaders_gp
      | "min", "totals" -> Queries.leaders_min_totals
      | "min", _ -> Queries.leaders_min
      | "pts", "totals" -> Queries.leaders_pts_totals
      | "pts", "per_36" -> Queries.leaders_pts_per36
      | "pts", _ -> Queries.leaders_pts
      | "reb", "totals" -> Queries.leaders_reb_totals
      | "reb", "per_36" -> Queries.leaders_reb_per36
      | "reb", _ -> Queries.leaders_reb
      | "ast", "totals" -> Queries.leaders_ast_totals
      | "ast", "per_36" -> Queries.leaders_ast_per36
      | "ast", _ -> Queries.leaders_ast
      | "stl", "totals" -> Queries.leaders_stl_totals
      | "stl", "per_36" -> Queries.leaders_stl_per36
      | "stl", _ -> Queries.leaders_stl
      | "blk", "totals" -> Queries.leaders_blk_totals
      | "blk", "per_36" -> Queries.leaders_blk_per36
      | "blk", _ -> Queries.leaders_blk
      | "tov", "totals" -> Queries.leaders_tov_totals
      | "tov", "per_36" -> Queries.leaders_tov_per36
      | "tov", _ -> Queries.leaders_tov
      | "eff", "per_36" -> Queries.leaders_eff_per36
      | "eff", _ -> Queries.leaders_eff
      | "fg_pct", _ -> Queries.leaders_fg_pct
      | "fg3_pct", _ -> Queries.leaders_fg3_pct
      | "ft_pct", _ -> Queries.leaders_ft_pct
      | "ts_pct", _ -> Queries.leaders_ts_pct
      | "efg_pct", _ -> Queries.leaders_efg_pct
      | "usg_pct", _ -> Queries.leaders_usg_pct
      | _ -> Queries.leaders_pts
    in
    Db.collect_list q (season, season)

  let get_leaders_base ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    if s = "ALL" then
      Db.collect_list Queries.leaders_base_cached_all ()
    else
      Db.collect_list Queries.leaders_base_cached s

  (** Get leaders filtered by position
      @param position "G" (Guard), "F" (Forward), "C" (Center), or "ALL"
      @param stat "pts", "reb", "ast", "eff"
  *)
  let get_leaders_by_position ~season ~position ~stat (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    let pos = if String.trim position = "" then "ALL" else position in
    let q = match stat with
      | "reb" -> Queries.leaders_reb_by_position
      | "ast" -> Queries.leaders_ast_by_position
      | "eff" -> Queries.leaders_eff_by_position
      | _ -> Queries.leaders_pts_by_position
    in
    Db.collect_list q ((s, s), pos)

  let get_stat_mvp_eff ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.stat_mvp_eff (s, (s, include_int))

  let get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.stat_mip_eff_delta (season, (include_int, (prev_season, include_int)))
  
	  let get_player_profile ~player_id (module Db : Caqti_eio.CONNECTION) =
	    let (let*) = Result.bind in
	    let* info = Db.find_opt Queries.player_info player_id in
	    match info with
	    | None -> Ok None
		    | Some p -> 
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
              let* draft_res = Db.find_opt Queries.player_draft_by_player_id player_id in
              let name_norm = normalize_label p.name in
              let trade_pattern = "%" ^ name_norm ^ "%" in
	              let* trade_events_res =
	                if String.trim name_norm = "" then
	                  Ok ([])
	                else
	                  Db.collect_list Queries.official_trade_events_by_player_name_norm trade_pattern
	              in
	              let* external_links_res =
	                Db.collect_list Queries.player_external_links_by_player_id player_id
	              in
				        let* ch_points = Db.find_opt Queries.career_high_points_game player_id in
				        let* ch_rebounds = Db.find_opt Queries.career_high_rebounds_game player_id in
				        let* ch_assists = Db.find_opt Queries.career_high_assists_game player_id in
				        let* ch_steals = Db.find_opt Queries.career_high_steals_game player_id in
				        let* ch_blocks = Db.find_opt Queries.career_high_blocks_game player_id in
				        let dummy_avg = { player_id = p.id; name = p.name; team_name = ""; games_played = 0; total_minutes = 0.0; total_points = 0; total_rebounds = 0; total_assists = 0; total_steals = 0; total_blocks = 0; total_turnovers = 0; avg_points = 0.0; avg_margin = 0.0; avg_rebounds = 0.0; avg_assists = 0.0; avg_steals = 0.0; avg_blocks = 0.0; avg_turnovers = 0.0; efficiency = 0.0; total_fg_made = 0; total_fg_att = 0; total_fg3_made = 0; total_fg3_att = 0; total_ft_made = 0; total_ft_att = 0; } in 
(* All values already unwrapped by let* - use them directly *)
			            let add_opt opt mk acc = match opt with | None -> acc | Some v -> mk v :: acc in
			            let items =
			              []
			              |> add_opt ch_points (fun (g: player_game_stat) -> { chi_label = "Points"; chi_value = g.pts; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_rebounds (fun g -> { chi_label = "Rebounds"; chi_value = g.reb; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_assists (fun g -> { chi_label = "Assists"; chi_value = g.ast; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_steals (fun g -> { chi_label = "Steals"; chi_value = g.stl; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_blocks (fun g -> { chi_label = "Blocks"; chi_value = g.blk; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> List.rev
			            in
			            let career_highs = match items with | [] -> None | _ -> Some items in
			            let averages = match averages_res with Some a -> a | None -> dummy_avg in
				            let team_stints =
				              team_stints_of_games team_games
				              |> normalize_player_team_stints
				            in
				            let career_entries = match Db.collect_list Queries.player_career_by_name p.name with Ok es -> es | Error _ -> [] in
				            Ok ((Some { player = p; averages; recent_games = recent; all_star_games = all_star; draft = draft_res; official_trade_events = trade_events_res; external_links = external_links_res; team_stints; season_breakdown = seasons; career_highs; career_entries }))

  let get_player_season_stats ~player_id ~scope (module Db : Caqti_eio.CONNECTION) = let q = match scope with | "totals" -> Queries.player_seasons_totals | "per_36" -> Queries.player_seasons_per36 | _ -> Queries.player_seasons_per_game in Db.collect_list q player_id
  let get_player_game_logs ~player_id ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.player_game_logs (player_id, (s, (s, include_int)))
  let get_batch_player_game_logs ~player_ids ~season (module Db : Caqti_eio.CONNECTION) =
    let ids_csv = String.concat "," player_ids in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.batch_player_game_logs (ids_csv, (s, s))
	  let get_team_recent_games ~team_name ~season (module Db : Caqti_eio.CONNECTION) =
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
	  let get_team_games ~team_name ~season (module Db : Caqti_eio.CONNECTION) =
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
	    Db.collect_list Queries.team_games t
	  let get_player_h2h ~p1_id ~p2_id ~season (module Db : Caqti_eio.CONNECTION) = let s = if season = "" then "ALL" else season in Db.collect_list Queries.player_h2h_games ((p1_id, p2_id), (s, s))

    (** Get head-to-head games between two teams *)
    let get_team_h2h ~team1 ~team2 ~season (module Db : Caqti_eio.CONNECTION) =
      let s = if String.trim season = "" then "ALL" else season in
      (* Params: (((t1, t2), (t2, t1)), (s, s)) for both home/away scenarios *)
      Db.collect_list Queries.team_h2h_games (((team1, team2), (team2, team1)), (s, s))

    (** Get MVP race candidates for a season *)
    let get_mvp_race_candidates ~season ~min_games (module Db : Caqti_eio.CONNECTION) =
      let s = if String.trim season = "" then "ALL" else season in
      Db.collect_list Queries.mvp_race_candidates (s, min_games)
	end

(** Connection pool - Eio-based *)
let pool_ref : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t option ref = ref None

(** Initialize pool with Eio context - must be called inside Eio_main.run *)
let init_pool ~sw ~stdenv db_url =
  let uri = Uri.of_string db_url in
  match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
  | Ok pool -> pool_ref := Some pool; Ok ()
  | Error e -> Error (ConnectionFailed (Caqti_error.show e))

(** Run a database operation using the pool *)
let with_db f =
  match !pool_ref with
  | None -> Error (ConnectionFailed "Pool not initialized")
  | Some pool ->
      match Caqti_eio.Pool.use f pool with
      | Ok v -> Ok v
      | Error e -> Error (QueryFailed (Caqti_error.show e))

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

(** Cache module - extracted to db_cache.ml *)
module Cache = Db_cache

let cached = Db_cache.cached

let cache_key_text = Db_cache.cache_key_text

let player_sort_key = function
  | ByPoints -> "pts"
  | ByMargin -> "mg"
  | ByRebounds -> "reb"
  | ByAssists -> "ast"
  | ByEfficiency -> "eff"
  | ByMinutes -> "min"

let team_sort_key = function
  | TeamByPoints -> "pts"
  | TeamByRebounds -> "reb"
  | TeamByAssists -> "ast"
  | TeamBySteals -> "stl"
  | TeamByBlocks -> "blk"
  | TeamByEfficiency -> "eff"
  | TeamByTsPct -> "ts"
  | TeamByFg3Pct -> "fg3"
  | TeamByMinutes -> "min"

(* Cache TTL tiers:
   - Hot data (standings, teams stats, players): 10 min, invalidated on sync
   - Warm data (awards, boxscores, leaders): 15 min, invalidated on sync
   - Cold data (seasons, history, legends): 6 hours, rarely changes
   - Live data (schedule): 2 min, needs near-real-time *)
let seasons_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:4
let teams_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:4
let data_freshness_cache = Cache.create ~ttl:300.0 ~max_entries:1
let standings_cache = Cache.create ~ttl:600.0 ~max_entries:16
let games_cache = Cache.create ~ttl:600.0 ~max_entries:16
let scored_games_cache = Cache.create ~ttl:600.0 ~max_entries:16
let team_stats_cache = Cache.create ~ttl:600.0 ~max_entries:48
let players_cache = Cache.create ~ttl:600.0 ~max_entries:128
let players_base_cache = Cache.create ~ttl:600.0 ~max_entries:32
let players_by_team_cache = Cache.create ~ttl:600.0 ~max_entries:128
let player_info_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:4
let team_detail_cache = Cache.create ~ttl:600.0 ~max_entries:64
let player_profile_cache = Cache.create ~ttl:600.0 ~max_entries:256
let player_season_stats_cache = Cache.create ~ttl:600.0 ~max_entries:256
let player_game_logs_cache = Cache.create ~ttl:600.0 ~max_entries:256
let boxscore_cache = Cache.create ~ttl:900.0 ~max_entries:128
let leaders_base_cache = Cache.create ~ttl:600.0 ~max_entries:16
let leaders_cache = Cache.create ~ttl:600.0 ~max_entries:128
let awards_cache = Cache.create ~ttl:900.0 ~max_entries:32
let awards_db_cache = Cache.create ~ttl:900.0 ~max_entries:64
let awards_count_cache = Cache.create ~ttl:900.0 ~max_entries:4
let awards_leaders_cache = Cache.create ~ttl:900.0 ~max_entries:32
let draft_years_cache = Cache.create ~ttl:900.0 ~max_entries:8
let draft_picks_cache = Cache.create ~ttl:900.0 ~max_entries:32
let trade_years_cache = Cache.create ~ttl:900.0 ~max_entries:8
let trade_events_cache = Cache.create ~ttl:900.0 ~max_entries:32
let qa_report_cache = Cache.create ~ttl:300.0 ~max_entries:4
let qa_schedule_missing_report_cache = Cache.create ~ttl:300.0 ~max_entries:4
let qa_pbp_missing_report_cache = Cache.create ~ttl:300.0 ~max_entries:16
let history_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:16
let legends_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:16
let coaches_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:16
let player_career_cache = Cache.create ~ttl:900.0 ~max_entries:128
let schedule_cache = Cache.create ~ttl:120.0 ~max_entries:16

let take n items =
  let rec loop acc count = function
    | _ when count <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> loop (x :: acc) (count - 1) xs
  in
  loop [] n items

let normalize_search_text value =
  value |> normalize_label |> String.lowercase_ascii |> String.trim

let string_contains ~needle ~hay =
  let needle_len = String.length needle in
  if needle_len = 0 then
    true
  else
    let hay_len = String.length hay in
    let rec loop i =
      if i + needle_len > hay_len then
        false
      else if String.sub hay i needle_len = needle then
        true
      else
        loop (i + 1)
    in
    loop 0

let player_base_matches search (b: player_base) =
  let trimmed = String.trim search in
  if trimmed = "" then
    true
  else
    let needle = normalize_search_text trimmed in
    let hay = normalize_search_text b.pb_player_name in
    string_contains ~needle ~hay

let player_base_to_aggregate (b: player_base) =
  { player_id = b.pb_player_id;
    name = b.pb_player_name;
    team_name = b.pb_team_name;
    games_played = b.pb_gp;
    total_minutes = (float_of_int b.pb_min_seconds) /. 60.0;
    total_points = b.pb_pts;
    total_rebounds = b.pb_reb;
    total_assists = b.pb_ast;
    total_steals = b.pb_stl;
    total_blocks = b.pb_blk;
    total_turnovers = b.pb_tov;
    avg_points = b.pb_avg_pts;
    avg_margin = b.pb_margin;
    avg_rebounds = b.pb_avg_reb;
    avg_assists = b.pb_avg_ast;
    avg_steals = b.pb_avg_stl;
    avg_blocks = b.pb_avg_blk;
    avg_turnovers = b.pb_avg_tov;
    efficiency = b.pb_eff;
    total_fg_made = 0;
    total_fg_att = 0;
    total_fg3_made = 0;
    total_fg3_att = 0;
    total_ft_made = 0;
    total_ft_att = 0;
  }

(* Some seasons can contain multiple (player_id, team) aggregates for the same player_id:
   - mid-season team changes
   - data quality issues (e.g., a 0-minute row attached to the wrong team)
   For list pages, we collapse to one row per player_id to avoid duplicate names. *)
type player_base_collapse_acc = {
  player_id: string;
  player_name: string;
  best_team_name: string;
  best_team_min_seconds: int;
  best_team_gp: int;
  gp: int;
  min_seconds: int;
  pts: int;
  reb: int;
  ast: int;
  stl: int;
  blk: int;
  tov: int;
  eff_gp: int;
  eff_sum: float;
  margin_seconds: int;
  margin_weighted_sum: float; (* margin * margin_seconds *)
}

let collapse_player_bases (items : player_base list) : player_base list =
  let add (tbl : (string, player_base_collapse_acc) Hashtbl.t) (b : player_base) =
    let key = b.pb_player_id in
    let gp_contrib = if b.pb_min_seconds > 0 then b.pb_gp else 0 in
    let eff_sum_contrib = b.pb_eff *. float_of_int gp_contrib in
    let margin_sum_contrib = b.pb_margin *. float_of_int b.pb_margin_seconds in
    match Hashtbl.find_opt tbl key with
    | None ->
        Hashtbl.add tbl key {
          player_id = b.pb_player_id;
          player_name = b.pb_player_name;
          best_team_name = b.pb_team_name;
          best_team_min_seconds = b.pb_min_seconds;
          best_team_gp = gp_contrib;
          gp = gp_contrib;
          min_seconds = b.pb_min_seconds;
          pts = b.pb_pts;
          reb = b.pb_reb;
          ast = b.pb_ast;
          stl = b.pb_stl;
          blk = b.pb_blk;
          tov = b.pb_tov;
          eff_gp = gp_contrib;
          eff_sum = eff_sum_contrib;
          margin_seconds = b.pb_margin_seconds;
          margin_weighted_sum = margin_sum_contrib;
        }
    | Some a ->
        let best_team_name, best_team_min_seconds, best_team_gp =
          if b.pb_min_seconds > a.best_team_min_seconds then
            (b.pb_team_name, b.pb_min_seconds, gp_contrib)
          else if b.pb_min_seconds = a.best_team_min_seconds && gp_contrib > a.best_team_gp then
            (b.pb_team_name, b.pb_min_seconds, gp_contrib)
          else
            (a.best_team_name, a.best_team_min_seconds, a.best_team_gp)
        in
        Hashtbl.replace tbl key {
          a with
          player_name = if a.player_name <> "" then a.player_name else b.pb_player_name;
          best_team_name;
          best_team_min_seconds;
          best_team_gp;
          gp = a.gp + gp_contrib;
          min_seconds = a.min_seconds + b.pb_min_seconds;
          pts = a.pts + b.pb_pts;
          reb = a.reb + b.pb_reb;
          ast = a.ast + b.pb_ast;
          stl = a.stl + b.pb_stl;
          blk = a.blk + b.pb_blk;
          tov = a.tov + b.pb_tov;
          eff_gp = a.eff_gp + gp_contrib;
          eff_sum = a.eff_sum +. eff_sum_contrib;
          margin_seconds = a.margin_seconds + b.pb_margin_seconds;
          margin_weighted_sum = a.margin_weighted_sum +. margin_sum_contrib;
        }
  in
  let tbl = Hashtbl.create (max 16 (List.length items)) in
  items |> List.iter (add tbl);
  tbl
  |> Hashtbl.to_seq_values
  |> List.of_seq
  |> List.map (fun a ->
      let gp = a.gp in
      let avg_of_total total =
        if gp > 0 then (float_of_int total) /. float_of_int gp else 0.0
      in
      let avg_pts = avg_of_total a.pts in
      let avg_reb = avg_of_total a.reb in
      let avg_ast = avg_of_total a.ast in
      let avg_stl = avg_of_total a.stl in
      let avg_blk = avg_of_total a.blk in
      let avg_tov = avg_of_total a.tov in
      let eff =
        if a.eff_gp > 0 then a.eff_sum /. float_of_int a.eff_gp else 0.0
      in
      let margin =
        if a.margin_seconds > 0 then
          a.margin_weighted_sum /. float_of_int a.margin_seconds
        else
          0.0
      in
      {
        pb_player_id = a.player_id;
        pb_player_name = a.player_name;
        pb_team_name = a.best_team_name;
        pb_gp = gp;
        pb_min_seconds = a.min_seconds;
        pb_pts = a.pts;
        pb_reb = a.reb;
        pb_ast = a.ast;
        pb_stl = a.stl;
        pb_blk = a.blk;
        pb_tov = a.tov;
        pb_avg_pts = avg_pts;
        pb_margin = margin;
        pb_avg_reb = avg_reb;
        pb_avg_ast = avg_ast;
        pb_avg_stl = avg_stl;
        pb_avg_blk = avg_blk;
        pb_avg_tov = avg_tov;
        pb_eff = eff;
        pb_margin_seconds = a.margin_seconds;
      })

let sort_player_bases sort (items: player_base list) =
  let compare_name a b = String.compare a.pb_player_name b.pb_player_name in
  match sort with
  | ByPoints ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_avg_pts a.pb_avg_pts in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByMargin ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_margin a.pb_margin in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByRebounds ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_avg_reb a.pb_avg_reb in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByAssists ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_avg_ast a.pb_avg_ast in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByEfficiency ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_eff a.pb_eff in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByMinutes ->
      List.sort
        (fun a b ->
          let primary = compare b.pb_min_seconds a.pb_min_seconds in
          if primary <> 0 then primary else compare_name a b)
        items

let filter_player_bases ~search ~sort (items: player_base list) =
  let filtered =
    items
    |> List.filter (player_base_matches search)
  in
  let with_margin_threshold =
    match sort with
    | ByMargin -> List.filter (fun b -> b.pb_margin_seconds >= 6000) filtered
    | _ -> filtered
  in
  sort_player_bases sort with_margin_threshold

let ensure_schema () = with_db (fun db -> Repo.ensure_schema db)
let refresh_matviews () = with_db (fun db -> Repo.refresh_matviews db)
let upsert_game_stats_exclusion ~game_id ~player_id ~reason () =
  with_db (fun db -> Repo.upsert_game_stats_exclusion ~game_id ~player_id ~reason db)
let delete_game_stats_exclusion ~game_id ~player_id () =
  with_db (fun db -> Repo.delete_game_stats_exclusion ~game_id ~player_id db)
let get_stat_exclusions ~season () =
  with_db (fun db -> Repo.qa_stat_exclusions ~season db)
let get_stat_anomaly_candidates ~season () =
  with_db (fun db -> Repo.qa_stat_anomaly_candidates ~season db)
let get_all_teams () =
  cached teams_cache "all" (fun () -> with_db (fun db -> Repo.get_teams db))

(** Resolve a team name (possibly with sub-name like "우리은행 위비") to the
    canonical team_name_kr stored in the teams table (e.g. "우리은행").
    Returns the input unchanged if no match is found. *)
let resolve_team_name name =
  match get_all_teams () with
  | Error _ -> name
  | Ok teams ->
    (* 1. Exact match on team_name_kr *)
    if List.exists (fun (t: Domain.team_info) -> t.team_name = name) teams then name
    else
      let str_contains haystack needle =
        let nlen = String.length needle in
        let hlen = String.length haystack in
        if nlen = 0 || hlen < nlen then false
        else
          let rec loop i =
            if i + nlen > hlen then false
            else if String.sub haystack i nlen = needle then true
            else loop (i + 1)
          in
          loop 0
      in
      (* 2. Find the longest team name that appears as substring of input.
         Longest-match prevents "LG" matching when "삼성생명" is the real team. *)
      let candidates = List.filter (fun (t: Domain.team_info) ->
        String.length t.team_name >= 4 && str_contains name t.team_name
      ) teams in
      match List.sort (fun a b ->
        compare (String.length (b: Domain.team_info).team_name) (String.length a.team_name)
      ) candidates with
      | best :: _ -> best.team_name
      | [] -> name

let get_seasons () =
  cached seasons_cache "all" (fun () -> with_db (fun db -> Repo.get_seasons db))
let get_all_player_info () =
  cached player_info_cache "all" (fun () -> with_db (fun db -> Repo.get_all_player_info db))
let get_latest_game_date () =
  cached data_freshness_cache "latest" (fun () ->
    with_db (fun db -> Repo.get_latest_game_date db))
let get_player_by_name ?(season="ALL") name = with_db (fun db -> Repo.get_player_by_name ~name ~season db)
let get_player_aggregate_by_id ~player_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_aggregate_by_id ~player_id ~season db)
let get_player_shooting_stats ~player_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_shooting_stats ~player_id ~season db)
let get_player_shot_chart ~player_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_shot_chart ~player_id ~season db)
let get_players_base ?(season="ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "season=%s|mismatch=%b" season include_mismatch in
  cached players_base_cache key (fun () ->
    with_db (fun db -> Repo.get_players_base ~season ~include_mismatch db))
let get_players ?(season="ALL") ?(limit=50) ?(search="") ?(sort=ByEfficiency) ?(include_mismatch=false) () =
  let key =
    Printf.sprintf "season=%s|limit=%d|search=%s|sort=%s|mismatch=%b"
      season
      limit
      (cache_key_text search)
      (player_sort_key sort)
      include_mismatch
  in
  cached players_cache key (fun () ->
    let (let*) = Result.bind in
    let* base_raw = get_players_base ~season ~include_mismatch () in
    let base = collapse_player_bases base_raw in
    let filtered = filter_player_bases ~search ~sort base in
    let limited = take limit filtered in
    let result = List.map player_base_to_aggregate limited in
    Ok result)
let get_players_by_team ~team_name ?(season="ALL") ?(limit=20) () =
  let key =
    Printf.sprintf "team=%s|season=%s|limit=%d"
      (cache_key_text team_name)
      season
      limit
  in
  cached players_by_team_cache key (fun () ->
    with_db (fun db -> Repo.get_players_by_team ~team_name ~season ~limit db))

let get_draft_years () =
  cached draft_years_cache "years" (fun () -> with_db (fun db -> Repo.get_draft_years db))

let get_draft_picks ?(year=0) ?(search="") () =
  let key =
    Printf.sprintf "picks|year=%d|search=%s" year (cache_key_text search)
  in
  cached draft_picks_cache key (fun () -> with_db (fun db -> Repo.get_draft_picks ~year ~search db))

let get_official_trade_years () =
  cached trade_years_cache "years" (fun () -> with_db (fun db -> Repo.get_official_trade_years db))

let get_official_trade_events ?(year=0) ?(search="") () =
  let key =
    Printf.sprintf "events|year=%d|search=%s" year (cache_key_text search)
  in
  cached trade_events_cache key (fun () ->
    with_db (fun db -> Repo.get_official_trade_events ~year ~search db))
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
  let key =
    Printf.sprintf "season=%s|scope=%s|sort=%s|mismatch=%b"
      season
      (team_scope_to_string scope)
      (team_sort_key sort)
      include_mismatch
  in
  cached team_stats_cache key (fun () ->
    let (let*) = Result.bind in
    let* totals = with_db (fun db -> Repo.get_team_totals ~season ~include_mismatch db) in
    let* margins = with_db (fun db -> Repo.get_team_margins ~season ~include_mismatch db) in
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
    Ok stats)
let calculate_gb (standings : team_standing list) = match standings with | [] -> [] | leader :: others -> let calc s = let wins_diff = Stdlib.float (leader.wins - s.wins) in let losses_diff = Stdlib.float (s.losses - leader.losses) in (wins_diff +. losses_diff) /. 2.0 in leader :: List.map (fun s -> { s with gb = calc s }) others
let get_standings ?(season = "ALL") () =
  let key = Printf.sprintf "season=%s" season in
  cached standings_cache key (fun () ->
    let (let*) = Result.bind in
    let* standings = with_db (fun db -> Repo.get_standings ~season db) in
    Ok (calculate_gb standings))
let get_games ?(page = 1) ?(page_size = 50) ?(season = "ALL") () =
  let key = Printf.sprintf "season=%s|page=%d|size=%d" season page page_size in
  cached games_cache key (fun () -> with_db (fun db -> Repo.get_games ~page ~page_size ~season db))
let get_scored_games ?(season = "ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "season=%s|mismatch=%b" season include_mismatch in
  cached scored_games_cache key (fun () ->
    with_db (fun db -> Repo.get_scored_games ~season ~include_mismatch db))
let get_game_season_code ~game_id () = with_db (fun db -> Repo.get_game_season_code ~game_id db)
let get_game_info ~game_id () = with_db (fun db -> Repo.get_game_info ~game_id db)
let get_pbp_periods ~game_id () = with_db (fun db -> Repo.get_pbp_periods ~game_id db)
let get_pbp_events ~game_id ~period_code () =
  with_db (fun db -> Repo.get_pbp_events ~game_id ~period_code db)
let get_team_core_player_ids ~season ~team_name ?(limit=7) () =
  with_db (fun db -> Repo.get_team_core_player_ids ~season ~team_name ~limit db)
let get_team_active_player_ids ~team_name ~game_id () =
  with_db (fun db -> Repo.get_team_active_player_ids ~team_name ~game_id db)
let get_boxscore ~game_id () =
  let key = Printf.sprintf "game_id=%s" game_id in
  cached boxscore_cache key (fun () ->
    let (let*) = Result.bind in
    let* game_info_opt = with_db (fun db -> Repo.get_game_info ~game_id db) in
    let* stats = with_db (fun db -> Repo.get_boxscore_stats ~game_id db) in
    match game_info_opt with
    | Some game_info ->
        let home_players = List.filter (fun s -> s.bs_team_code = game_info.gi_home_team_code) stats in
        let away_players = List.filter (fun s -> s.bs_team_code = game_info.gi_away_team_code) stats in
        Ok { boxscore_game = game_info; boxscore_home_players = home_players; boxscore_away_players = away_players }
    | None -> Error (QueryFailed "Game not found"))
let get_leaders_base ?(season="ALL") () =
  let key = cache_key_text season in
  cached leaders_base_cache key (fun () -> with_db (fun db -> Repo.get_leaders_base ~season db))

let leaders_from_base ~scope ~category (bases: leader_base list) =
  let scope_value = scope |> String.trim |> String.lowercase_ascii in
  let category_value = category |> String.trim |> String.lowercase_ascii in
  let to_entry (b: leader_base) value =
    { le_player_id = b.lb_player_id
    ; le_player_name = b.lb_player_name
    ; le_team_name = b.lb_team_name
    ; le_stat_value = value
    }
  in
  let minutes_total (b: leader_base) = (float_of_int b.lb_min_seconds) /. 60.0 in
  let minutes_per_game (b: leader_base) =
    if b.lb_gp = 0 then 0.0 else minutes_total b /. float_of_int b.lb_gp
  in
  let per_game_int v (b: leader_base) =
    if b.lb_gp = 0 then 0.0 else (float_of_int v) /. float_of_int b.lb_gp
  in
  let per_game_float v (b: leader_base) =
    if b.lb_gp = 0 then 0.0 else v /. float_of_int b.lb_gp
  in
  let per_36_int v (b: leader_base) =
    if b.lb_min_seconds <= 0 then 0.0 else (float_of_int v) /. float_of_int b.lb_min_seconds *. 2160.0
  in
  let per_36_float v (b: leader_base) =
    if b.lb_min_seconds <= 0 then 0.0 else v /. float_of_int b.lb_min_seconds *. 2160.0
  in
  let fg_pct (b: leader_base) =
    if b.lb_fg_a = 0 then 0.0 else (float_of_int b.lb_fg_m) /. float_of_int b.lb_fg_a
  in
  let fg3_pct (b: leader_base) =
    if b.lb_fg3_a = 0 then 0.0 else (float_of_int b.lb_fg3_m) /. float_of_int b.lb_fg3_a
  in
  let ft_pct (b: leader_base) =
    if b.lb_ft_a = 0 then 0.0 else (float_of_int b.lb_ft_m) /. float_of_int b.lb_ft_a
  in
  let ts_pct (b: leader_base) =
    let denom = 2.0 *. ((float_of_int b.lb_fg_a) +. 0.44 *. (float_of_int b.lb_ft_a)) in
    if denom <= 0.0 then 0.0 else (float_of_int b.lb_pts) /. denom
  in
  let efg_pct (b: leader_base) =
    if b.lb_fg_a = 0 then 0.0
    else
      ((float_of_int b.lb_fg_m) +. 0.5 *. (float_of_int b.lb_fg3_m)) /. float_of_int b.lb_fg_a
  in
  let min_games (b: leader_base) = b.lb_gp >= 5 in
  let min_minutes (b: leader_base) = b.lb_min_seconds >= 6000 in
  let min_fga (b: leader_base) = b.lb_fg_a >= 50 in
  let min_fg3a (b: leader_base) = b.lb_fg3_a >= 20 in
  let min_fta (b: leader_base) = b.lb_ft_a >= 20 in
  let min_ts (b: leader_base) = (b.lb_fg_a + b.lb_ft_a) >= 50 in
  let value_fn, predicate =
    match scope_value with
    | "totals" ->
        (match category_value with
        | "gp" -> (fun b -> float_of_int b.lb_gp), (fun _ -> true)
        | "min" -> (fun b -> minutes_total b), (fun _ -> true)
        | "pts" -> (fun b -> float_of_int b.lb_pts), min_games
        | "reb" -> (fun b -> float_of_int b.lb_reb), min_games
        | "ast" -> (fun b -> float_of_int b.lb_ast), min_games
        | "stl" -> (fun b -> float_of_int b.lb_stl), min_games
        | "blk" -> (fun b -> float_of_int b.lb_blk), min_games
        | "tov" -> (fun b -> float_of_int b.lb_tov), min_games
        | "fg_pct" -> fg_pct, min_fga
        | "fg3_pct" -> fg3_pct, min_fg3a
        | "ft_pct" -> ft_pct, min_fta
        | "ts_pct" -> ts_pct, min_ts
        | "efg_pct" -> efg_pct, min_fga
        | _ -> (fun _ -> 0.0), (fun _ -> false))
    | "per_36" ->
        (match category_value with
        | "pts" -> (fun b -> per_36_int b.lb_pts b), min_minutes
        | "reb" -> (fun b -> per_36_int b.lb_reb b), min_minutes
        | "ast" -> (fun b -> per_36_int b.lb_ast b), min_minutes
        | "stl" -> (fun b -> per_36_int b.lb_stl b), min_minutes
        | "blk" -> (fun b -> per_36_int b.lb_blk b), min_minutes
        | "tov" -> (fun b -> per_36_int b.lb_tov b), min_minutes
        | "eff" -> (fun b -> per_36_float b.lb_eff b), min_minutes
        | "fg_pct" -> fg_pct, min_fga
        | "fg3_pct" -> fg3_pct, min_fg3a
        | "ft_pct" -> ft_pct, min_fta
        | "ts_pct" -> ts_pct, min_ts
        | "efg_pct" -> efg_pct, min_fga
        | _ -> (fun _ -> 0.0), (fun _ -> false))
    | _ ->
        (match category_value with
        | "pts" -> (fun b -> per_game_int b.lb_pts b), min_games
        | "reb" -> (fun b -> per_game_int b.lb_reb b), min_games
        | "ast" -> (fun b -> per_game_int b.lb_ast b), min_games
        | "stl" -> (fun b -> per_game_int b.lb_stl b), min_games
        | "blk" -> (fun b -> per_game_int b.lb_blk b), min_games
        | "tov" -> (fun b -> per_game_int b.lb_tov b), min_games
        | "min" -> minutes_per_game, min_games
        | "eff" -> (fun b -> per_game_float b.lb_eff b), min_games
        | "fg_pct" -> fg_pct, min_fga
        | "fg3_pct" -> fg3_pct, min_fg3a
        | "ft_pct" -> ft_pct, min_fta
        | "ts_pct" -> ts_pct, min_ts
        | "efg_pct" -> efg_pct, min_fga
        | _ -> (fun _ -> 0.0), (fun _ -> false))
  in
  bases
  |> List.filter predicate
  |> List.map (fun b -> to_entry b (value_fn b))
  |> List.sort (fun a b -> Float.compare b.le_stat_value a.le_stat_value)
  |> take 5

let get_leaders ?(season="ALL") ?(scope="per_game") category =
  let key = Printf.sprintf "season=%s|scope=%s|category=%s" season scope category in
  cached leaders_cache key (fun () ->
    let (let*) = Result.bind in
    let* bases = get_leaders_base ~season () in
    Ok (leaders_from_base ~scope ~category bases))

let get_leaders_by_position ?(season="ALL") ?(position="ALL") stat =
  with_db (fun db -> Repo.get_leaders_by_position ~season ~position ~stat db)

let get_stat_mvp_eff ?(season="ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "mvp|season=%s|mismatch=%b" season include_mismatch in
  cached awards_cache key (fun () ->
    with_db (fun db -> Repo.get_stat_mvp_eff ~season ~include_mismatch db))

let get_stat_mip_eff_delta ~season ~prev_season ?(include_mismatch=false) () =
  let key = Printf.sprintf "mip|season=%s|prev=%s|mismatch=%b" season prev_season include_mismatch in
  cached awards_cache key (fun () ->
    with_db (fun db -> Repo.get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch db))

(* Awards DB queries — separate caches per return type *)
let save_award ~season_name ~category ~award_type ~player_name ?stat_value ?votes () =
  with_db (fun db -> Repo.upsert_award ~season_name ~category ~award_type ~player_name ~stat_value ~votes db)

let get_awards_by_category ~category () =
  let key = Printf.sprintf "awards_cat|%s" category in
  cached awards_db_cache key (fun () ->
    with_db (fun db -> Repo.get_awards_by_category ~category db))

let get_awards_by_season ~season_name () =
  let key = Printf.sprintf "awards_season|%s" season_name in
  cached awards_db_cache key (fun () ->
    with_db (fun db -> Repo.get_awards_by_season ~season_name db))

let get_awards_by_player ~player_name () =
  let key = Printf.sprintf "awards_player|%s" player_name in
  cached awards_db_cache key (fun () ->
    with_db (fun db -> Repo.get_awards_by_player ~player_name db))

let get_award_count () =
  cached awards_count_cache "award_count" (fun () ->
    with_db (fun db -> Repo.get_award_count db))

let get_award_leaders ~category () =
  let key = Printf.sprintf "award_leaders|%s" category in
  cached awards_leaders_cache key (fun () ->
    with_db (fun db -> Repo.get_award_leaders ~category db))

let get_player_profile ~player_id () =
  let key = Printf.sprintf "player_id=%s" player_id in
  cached player_profile_cache key (fun () -> with_db (fun db -> Repo.get_player_profile ~player_id db))
let get_player_season_stats ~player_id ~scope () =
  let key = Printf.sprintf "player_id=%s|scope=%s" player_id scope in
  cached player_season_stats_cache key (fun () ->
    with_db (fun db -> Repo.get_player_season_stats ~player_id ~scope db))
let get_player_game_logs ~player_id ?(season="ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "player_id=%s|season=%s|mismatch=%b" player_id season include_mismatch in
  cached player_game_logs_cache key (fun () ->
    with_db (fun db -> Repo.get_player_game_logs ~player_id ~season ~include_mismatch db))

(** Batch fetch game logs for multiple players in single query.
    Returns a Hashtbl mapping player_id -> player_game_stat list *)
let get_batch_player_game_logs ~player_ids ?(season="ALL") () =
  let (let*) = Result.bind in
  let* rows = with_db (fun db -> Repo.get_batch_player_game_logs ~player_ids ~season db) in
  let tbl = Hashtbl.create (List.length player_ids) in
  List.iter (fun (row: player_game_stat_with_id) ->
    let existing = match Hashtbl.find_opt tbl row.pgs_player_id with
      | Some lst -> lst
      | None -> []
    in
    Hashtbl.replace tbl row.pgs_player_id (row.pgs_stat :: existing)
  ) rows;
  (* Reverse lists since we iterated in DESC order but appended to front *)
  Hashtbl.iter (fun k v -> Hashtbl.replace tbl k (List.rev v)) tbl;
  Ok tbl

let get_team_full_detail ~team_name ?(season="ALL") () =
  let team_name = resolve_team_name team_name in
  let key = Printf.sprintf "team=%s|season=%s" (cache_key_text team_name) season in
  cached team_detail_cache key (fun () ->
    let (let*) = Result.bind in
    let* standings = get_standings ~season () in
    let* roster = get_players_by_team ~team_name ~season () in
    let* games = with_db (fun db -> Repo.get_team_recent_games ~team_name ~season db) in
    let* game_results = with_db (fun db -> Repo.get_team_games ~team_name ~season db) in
    let* all_totals = with_db (fun db -> Repo.get_team_totals ~season ~include_mismatch:false db) in
    let standing = List.find_opt (fun (s: team_standing) -> s.team_name = team_name) standings in
    let team_totals = List.find_opt (fun (t: team_totals) -> t.team = team_name) all_totals in
    Ok { tfd_team_name = team_name; tfd_standing = standing; tfd_standings = standings; tfd_roster = roster; tfd_game_results = game_results; tfd_recent_games = games; tfd_team_totals = team_totals; tfd_all_totals = all_totals })
let get_player_h2h_data ~p1_id ~p2_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_h2h ~p1_id ~p2_id ~season db)

let get_team_h2h_data ~team1 ~team2 ?(season="ALL") () =
  with_db (fun db -> Repo.get_team_h2h ~team1 ~team2 ~season db)

let get_db_quality_report () : qa_db_report db_result =
  cached qa_report_cache "qa_report" (fun () ->
    let (let*) = Result.bind in
    let int_or_zero = Option.value ~default:0 in
    let* games_total_opt = with_db (fun db -> Repo.qa_games_total db) in
    let* finished_games_total_opt = with_db (fun db -> Repo.qa_finished_games_total db) in
    let* games_with_stats_opt = with_db (fun db -> Repo.qa_games_with_stats db) in
    let* pbp_games_opt = with_db (fun db -> Repo.qa_pbp_games db) in
    let* plus_minus_games_opt = with_db (fun db -> Repo.qa_plus_minus_games db) in
    let* schedule_total_opt = with_db (fun db -> Repo.qa_schedule_total db) in
    let* schedule_completed_opt = with_db (fun db -> Repo.qa_schedule_completed db) in
    let* schedule_missing_game_count_opt = with_db (fun db -> Repo.qa_schedule_missing_game_count db) in
    let* schedule_missing_game_sample = with_db (fun db -> Repo.qa_schedule_missing_game_sample db) in
    let* schedule_missing_stats_count_opt = with_db (fun db -> Repo.qa_schedule_missing_stats_count db) in
    let* schedule_missing_stats_sample = with_db (fun db -> Repo.qa_schedule_missing_stats_sample db) in
    let* schedule_coverage = with_db (fun db -> Repo.qa_schedule_coverage db) in
    let* mismatch_count_opt = with_db (fun db -> Repo.qa_score_mismatch_count db) in
    let* mismatch_sample = with_db (fun db -> Repo.qa_score_mismatch_sample db) in
    let* team_count_anomaly_count_opt = with_db (fun db -> Repo.qa_team_count_anomaly_count db) in
    let* team_count_anomaly_sample = with_db (fun db -> Repo.qa_team_count_anomaly_sample db) in
    let* dup_row_count_opt = with_db (fun db -> Repo.qa_duplicate_player_row_count db) in
    let* dup_row_sample = with_db (fun db -> Repo.qa_duplicate_player_row_sample db) in
    let* dup_name_count_opt = with_db (fun db -> Repo.qa_duplicate_player_name_count db) in
    let* dup_name_rows = with_db (fun db -> Repo.qa_duplicate_player_name_sample db) in
    let* dup_identity_count_opt = with_db (fun db -> Repo.qa_duplicate_player_identity_count db) in
    let* dup_identity_rows = with_db (fun db -> Repo.qa_duplicate_player_identity_sample db) in
    let games_total = int_or_zero games_total_opt in
    let finished_games_total = int_or_zero finished_games_total_opt in
    let games_with_stats = int_or_zero games_with_stats_opt in
    let pbp_games = int_or_zero pbp_games_opt in
    let plus_minus_games = int_or_zero plus_minus_games_opt in
    let schedule_total = int_or_zero schedule_total_opt in
    let schedule_completed = int_or_zero schedule_completed_opt in
    let schedule_missing_game_count = int_or_zero schedule_missing_game_count_opt in
    let schedule_missing_stats_count = int_or_zero schedule_missing_stats_count_opt in
    let pbp_missing_games = max 0 (finished_games_total - pbp_games) in
    let pbp_coverage_pct = Db_common.coverage_pct ~total:finished_games_total ~covered:pbp_games in
    let plus_minus_coverage_pct = Db_common.coverage_pct ~total:finished_games_total ~covered:plus_minus_games in
    let schedule_missing_game_pct = Db_common.coverage_pct ~total:schedule_completed ~covered:schedule_missing_game_count in
    let schedule_missing_stats_pct = Db_common.coverage_pct ~total:schedule_completed ~covered:schedule_missing_stats_count in
    let dup_name_sample =
      dup_name_rows
      |> List.map (fun (name, id_count, ids_csv) ->
          { qdpn_player_name = name;
            qdpn_id_count = id_count;
            qdpn_player_ids = split_csv_ids ids_csv;
          })
    in
    let dup_identity_sample =
      dup_identity_rows
      |> List.map (fun (name, birth_date, id_count, ids_csv) ->
          { qdpi_player_name = name;
            qdpi_birth_date = birth_date;
            qdpi_id_count = id_count;
            qdpi_player_ids = split_csv_ids ids_csv;
          })
    in
    Ok { qdr_generated_at = iso8601_utc ();
        qdr_games_total = games_total;
        qdr_finished_games_total = finished_games_total;
        qdr_games_with_stats = games_with_stats;
        qdr_pbp_games = pbp_games;
        qdr_pbp_missing_games = pbp_missing_games;
        qdr_pbp_coverage_pct = pbp_coverage_pct;
        qdr_plus_minus_games = plus_minus_games;
        qdr_plus_minus_coverage_pct = plus_minus_coverage_pct;
        qdr_schedule_total = schedule_total;
        qdr_schedule_completed = schedule_completed;
        qdr_schedule_missing_game_count = schedule_missing_game_count;
        qdr_schedule_missing_game_pct = schedule_missing_game_pct;
        qdr_schedule_missing_game_sample = schedule_missing_game_sample;
        qdr_schedule_missing_stats_count = schedule_missing_stats_count;
        qdr_schedule_missing_stats_pct = schedule_missing_stats_pct;
        qdr_schedule_missing_stats_sample = schedule_missing_stats_sample;
        qdr_schedule_coverage = schedule_coverage;
        qdr_score_mismatch_count = int_or_zero mismatch_count_opt;
        qdr_score_mismatch_sample = mismatch_sample;
        qdr_team_count_anomaly_count = int_or_zero team_count_anomaly_count_opt;
        qdr_team_count_anomaly_sample = team_count_anomaly_sample;
        qdr_duplicate_player_row_count = int_or_zero dup_row_count_opt;
        qdr_duplicate_player_row_sample = dup_row_sample;
        qdr_duplicate_player_name_count = int_or_zero dup_name_count_opt;
        qdr_duplicate_player_name_sample = dup_name_sample;
        qdr_duplicate_player_identity_count = int_or_zero dup_identity_count_opt;
        qdr_duplicate_player_identity_sample = dup_identity_sample;
      })

let get_pbp_missing_report ~season () : qa_pbp_missing_report db_result =
  let s =
    season |> String.trim |> fun v ->
    if v = "" then Seasons_catalog.current_regular_season_code () else v
  in
  let key = Printf.sprintf "season=%s" (cache_key_text s) in
  cached qa_pbp_missing_report_cache key (fun () ->
    let (let*) = Result.bind in
    let int_or_zero = Option.value ~default:0 in
    let* finished_games_total_opt = with_db (fun db -> Repo.qa_finished_games_total_by_season ~season:s db) in
    let* pbp_games_opt = with_db (fun db -> Repo.qa_pbp_games_by_season ~season:s db) in
    let* missing_sample = with_db (fun db -> Repo.qa_pbp_missing_games_sample_by_season ~season:s db) in
    let finished_games_total = int_or_zero finished_games_total_opt in
    let pbp_games = int_or_zero pbp_games_opt in
    let missing_games = max 0 (finished_games_total - pbp_games) in
    let coverage_pct = Db_common.coverage_pct ~total:finished_games_total ~covered:pbp_games in
    Ok {
      qpmr_generated_at = iso8601_utc ();
      qpmr_season_code = s;
      qpmr_finished_games_total = finished_games_total;
      qpmr_pbp_games = pbp_games;
      qpmr_missing_games = missing_games;
      qpmr_coverage_pct = coverage_pct;
      qpmr_missing_sample = missing_sample;
    })

let get_schedule_missing_report () : qa_schedule_missing_report db_result =
  cached qa_schedule_missing_report_cache "qa_schedule_missing_report" (fun () ->
    let (let*) = Result.bind in
    let* summary_opt = with_db (fun db -> Repo.qa_schedule_missing_summary db) in
    let* reason_counts = with_db (fun db -> Repo.qa_schedule_missing_reason_counts db) in
    let* samples = with_db (fun db -> Repo.qa_schedule_missing_samples db) in
    match summary_opt with
    | None -> Error (QueryFailed "Schedule missing summary not found")
    | Some summary ->
        Ok {
          qsmr_generated_at = iso8601_utc ();
          qsmr_summary = summary;
          qsmr_reason_counts = reason_counts;
          qsmr_samples = samples;
        })

(* ===== History & Legends Public API ===== *)

let get_historical_seasons () =
  cached history_cache "all" (fun () ->
    with_db (fun db -> Repo.get_historical_seasons db))

let get_legend_players () =
  cached legends_cache "all" (fun () ->
    with_db (fun db -> Repo.get_legend_players db))

let get_coaches () =
  cached coaches_cache "all" (fun () ->
    with_db (fun db -> Repo.get_coaches db))

let get_player_career ~player_name () =
  let key = Printf.sprintf "player=%s" (cache_key_text player_name) in
  cached player_career_cache key (fun () ->
    with_db (fun db -> Repo.get_player_career ~player_name db))

(* ===== Schedule Public API ===== *)

let get_upcoming_schedule ?(status="scheduled") ?(limit=10) () =
  let key = Printf.sprintf "status=%s,limit=%d" status limit in
  cached schedule_cache key (fun () ->
    with_db (fun db -> Repo.get_upcoming_schedule ~status ~limit db))

let get_schedule_by_date_range ~start_date ~end_date ?(status="ALL") () =
  let key = Printf.sprintf "start=%s,end=%s,status=%s" start_date end_date status in
  cached schedule_cache key (fun () ->
    with_db (fun db -> Repo.get_schedule_by_date_range ~start_date ~end_date ~status db))

(* ===== MVP Race Public API ===== *)

let mvp_race_cache = Cache.create ~ttl:600.0 ~max_entries:16

let get_mvp_race ?(season="ALL") ?(min_games=5) () =
  let key = Printf.sprintf "season=%s,min_games=%d" season min_games in
  let min_games_str = string_of_int min_games in
  let (let*) = Result.bind in
  let* candidates = cached mvp_race_cache key (fun () ->
    with_db (fun db -> Repo.get_mvp_race_candidates ~season ~min_games:min_games_str db)) in
  (* Add rank to each candidate *)
  let ranked = candidates |> List.mapi (fun i c -> { c with mvp_rank = i + 1 }) in
  Ok ranked

(* ===== Clutch Time Stats Public API ===== *)

(** Get clutch time stats for a season.
    Clutch time = Q4 remaining 5 min + score diff <= 5 points.
    Uses play_by_play_events with player_id to aggregate scoring stats. *)
let get_clutch_stats ~season () =
  let (let*) = Result.bind in
  let s = if String.trim season = "" then "ALL" else season in
  let* rows = with_db (fun (module Db : Caqti_eio.CONNECTION) ->
    Db.collect_list Queries.clutch_stats_by_season s
  ) in
  Ok rows

(* ===== On/Off Impact Public API ===== *)

(** Convert DB row to on_off_impact *)
let row_to_on_off_impact (player_id, (player_name, (team_name, (games_played, (total_min_seconds, (total_plus_minus, games_with_pm)))))) : Domain.on_off_impact =
  let total_minutes = float_of_int total_min_seconds /. 60.0 in
  let avg_pm = if games_with_pm > 0 then float_of_int total_plus_minus /. float_of_int games_with_pm else 0.0 in
  {
    ooi_player_id = player_id;
    ooi_player_name = player_name;
    ooi_team_name = team_name;
    ooi_games_played = games_played;
    ooi_total_minutes = total_minutes;
    ooi_on_court = {
      ocs_games = games_with_pm;
      ocs_minutes = total_minutes;
      ocs_team_pts = 0;
      ocs_opp_pts = 0;
      ocs_possessions = 0.0;
    };
    ooi_off_court = {
      ofcs_games = 0;
      ofcs_minutes = 0.0;
      ofcs_team_pts = 0;
      ofcs_opp_pts = 0;
      ofcs_possessions = 0.0;
    };
    ooi_net_rating_on = 0.0;
    ooi_net_rating_off = 0.0;
    ooi_net_rating_diff = 0.0;
    ooi_plus_minus_total = total_plus_minus;
    ooi_plus_minus_avg = avg_pm;
  }

(** Get on/off impact stats for all qualifying players in a season *)
let get_on_off_impact_stats ~season ?(limit=50) () =
  let (let*) = Result.bind in
  let limit_str = string_of_int limit in
  let* rows = with_db (fun (module Db : Caqti_eio.CONNECTION) ->
    Db.collect_list Queries.on_off_impact_stats (season, limit_str)
  ) in
  Ok (List.map row_to_on_off_impact rows)

(** Get on/off impact for a specific player *)
let get_on_off_impact_for_player ~player_id ~season () =
  let (let*) = Result.bind in
  let* row_opt = with_db (fun (module Db : Caqti_eio.CONNECTION) ->
    Db.find_opt Queries.on_off_impact_for_player (player_id, season)
  ) in
  match row_opt with
  | Some row -> Ok (Some (row_to_on_off_impact row))
  | None -> Ok None

(* ===== Lineup Chemistry Public API ===== *)

(** Lineup analysis data - player stats per game for lineup analysis *)
type lineup_game_data = {
  lgd_game_id: string;
  lgd_player_id: string;
  lgd_player_name: string;
  lgd_team_code: string;
  lgd_team_name: string;
  lgd_minutes: float;
  lgd_pts: int;
  lgd_plus_minus: int option;
}

let lineup_cache = Cache.create ~ttl:900.0 ~max_entries:32

(** Get all player game stats for lineup analysis *)
let get_lineup_data ~season ~team_name () =
  let key = Printf.sprintf "season=%s,team=%s" season team_name in
  cached lineup_cache key (fun () ->
    with_db (fun (module Db : Caqti_eio.CONNECTION) ->
      let open Caqti_type in
      let (let*) = Result.bind in
      let query = Request_oneshot.(
        t2 string (t2 string (t2 string string)) ->*
        (t2 string (t2 string (t2 string (t2 string (t2 string (t2 int (t2 int (option int))))))))
      ) {|
        SELECT
          s.game_id,
          s.player_id,
          p.player_name,
          s.team_code,
          t.team_name_kr,
          s.min_seconds,
          s.pts,
          NULL::integer AS plus_minus
        FROM game_stats_clean s
        JOIN games g ON g.game_id = s.game_id
        JOIN players p ON p.player_id = s.player_id
        JOIN teams t ON t.team_code = s.team_code
        WHERE g.game_type != '10'
          AND (g.season_code = ? OR ? = 'ALL')
          AND (s.team_code = ? OR ? = 'ALL')
          AND s.min_seconds > 0
        ORDER BY s.game_id, s.team_code, s.min_seconds DESC
      |} in
      let* data = Db.collect_list query (season, (season, (team_name, team_name))) in
      let parsed = data |> List.map (fun (game_id, (player_id, (player_name, (team_code, (team_name, (min_secs, (pts, plus_minus))))))) ->
        { lgd_game_id = game_id;
          lgd_player_id = player_id;
          lgd_player_name = player_name;
          lgd_team_code = team_code;
          lgd_team_name = team_name;
          lgd_minutes = float_of_int min_secs /. 60.0;
          lgd_pts = pts;
          lgd_plus_minus = plus_minus;
        })
      in
      Ok parsed))

(** Module for lineup analysis calculations *)
module LineupAnalysis = struct
  open Domain

  (** Group game data by (game_id, team_code) *)
  let group_by_game_team (data: lineup_game_data list)
      : (string * string, lineup_game_data list) Hashtbl.t =
    let tbl = Hashtbl.create 256 in
    List.iter (fun d ->
      let key = (d.lgd_game_id, d.lgd_team_code) in
      let existing = try Hashtbl.find tbl key with Not_found -> [] in
      Hashtbl.replace tbl key (d :: existing)
    ) data;
    tbl

  (** Get top 5 players by minutes in a game-team combo *)
  let get_starting_five (players: lineup_game_data list) : lineup_game_data list =
    players
    |> List.sort (fun a b -> compare b.lgd_minutes a.lgd_minutes)
    |> (fun l ->
        let rec take n acc = function
          | [] -> List.rev acc
          | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
        in take 5 [] l)

  (** Create lineup key from players *)
  let players_to_key (players: lineup_game_data list) : string =
    players
    |> List.map (fun p -> p.lgd_player_id)
    |> List.sort String.compare
    |> String.concat ","

  (** Analyze lineups from game data *)
  let analyze_lineups ~team_name (data: lineup_game_data list) : lineup_stats list =
    let grouped = group_by_game_team data in
    (* Collect lineup stats *)
    let lineup_tbl : (string, lineup_stats * lineup_game_data list) Hashtbl.t =
      Hashtbl.create 128 in

    Hashtbl.iter (fun (_game_id, team_code) players ->
      if team_name = "ALL" || team_code = team_name then begin
        let top5 = get_starting_five players in
        if List.length top5 >= 5 then begin
          let key = players_to_key top5 in
          let team = (List.hd top5).lgd_team_name in
          let total_min = List.fold_left (fun acc p -> acc +. p.lgd_minutes) 0.0 top5 in
          let total_pts = List.fold_left (fun acc p -> acc + p.lgd_pts) 0 top5 in
          let total_pm = List.fold_left (fun acc p ->
            acc + (match p.lgd_plus_minus with Some pm -> pm | None -> 0)
          ) 0 top5 in

          let (existing, players_ref) =
            try Hashtbl.find lineup_tbl key
            with Not_found -> (empty_lineup_stats ~team_name:team, []) in

          let updated = {
            existing with
            ls_games_together = existing.ls_games_together + 1;
            ls_total_minutes = existing.ls_total_minutes +. total_min;
            ls_total_pts = existing.ls_total_pts + total_pts;
            ls_plus_minus = existing.ls_plus_minus + total_pm;
          } in
          Hashtbl.replace lineup_tbl key (updated, top5 @ players_ref)
        end
      end
    ) grouped;

    (* Convert to list and calculate averages *)
    Hashtbl.fold (fun _key (stats, players_sample) acc ->
      let player_infos =
        players_sample
        |> List.sort_uniq (fun a b -> String.compare a.lgd_player_id b.lgd_player_id)
        |> (fun l ->
            let rec take n acc = function
              | [] -> List.rev acc
              | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
            in take 5 [] l)
        |> List.map (fun p -> {
            lp_player_id = p.lgd_player_id;
            lp_player_name = p.lgd_player_name;
            lp_position = None;
          })
      in
      let avg_pts_per_min =
        if stats.ls_total_minutes > 0.0
        then float_of_int stats.ls_total_pts /. stats.ls_total_minutes
        else 0.0 in
      let avg_margin_per_min =
        if stats.ls_total_minutes > 0.0
        then float_of_int stats.ls_plus_minus /. stats.ls_total_minutes
        else 0.0 in
      { stats with
        ls_players = player_infos;
        ls_avg_pts_per_min = avg_pts_per_min;
        ls_avg_margin_per_min = avg_margin_per_min;
      } :: acc
    ) lineup_tbl []

  (** Calculate player pair synergies *)
  let analyze_synergies (lineups: lineup_stats list) : lineup_synergy list =
    let pair_tbl : (string * string, int * float * int) Hashtbl.t = Hashtbl.create 256 in

    (* Collect pair data from lineups *)
    List.iter (fun lineup ->
      let players = lineup.ls_players in
      (* Generate all pairs *)
      let rec pairs acc = function
        | [] -> acc
        | p1 :: rest ->
          let new_pairs = List.map (fun p2 -> (p1, p2)) rest in
          pairs (new_pairs @ acc) rest
      in
      let all_pairs = pairs [] players in
      List.iter (fun (p1, p2) ->
        let (id1, id2) =
          if p1.lp_player_id < p2.lp_player_id
          then (p1.lp_player_id, p2.lp_player_id)
          else (p2.lp_player_id, p1.lp_player_id) in
        let key = (id1, id2) in
        let (games, mins, pm) =
          try Hashtbl.find pair_tbl key
          with Not_found -> (0, 0.0, 0) in
        Hashtbl.replace pair_tbl key
          (games + lineup.ls_games_together,
           mins +. lineup.ls_total_minutes,
           pm + lineup.ls_plus_minus)
      ) all_pairs
    ) lineups;

    (* Build player name lookup *)
    let name_tbl = Hashtbl.create 64 in
    List.iter (fun lineup ->
      List.iter (fun p ->
        Hashtbl.replace name_tbl p.lp_player_id p.lp_player_name
      ) lineup.ls_players
    ) lineups;

    (* Convert to synergy list *)
    Hashtbl.fold (fun (id1, id2) (games, mins, pm) acc ->
      let avg_pm = if mins > 0.0 then float_of_int pm /. mins else 0.0 in
      let score = calculate_synergy_score
        ~games_together:games ~total_minutes:mins ~avg_plus_minus:avg_pm in
      let name1 = try Hashtbl.find name_tbl id1 with Not_found -> id1 in
      let name2 = try Hashtbl.find name_tbl id2 with Not_found -> id2 in
      { syn_player1_id = id1;
        syn_player1_name = name1;
        syn_player2_id = id2;
        syn_player2_name = name2;
        syn_games_together = games;
        syn_total_minutes = mins;
        syn_avg_plus_minus = avg_pm;
        syn_synergy_score = score;
      } :: acc
    ) pair_tbl []
    |> List.sort (fun a b -> compare b.syn_synergy_score a.syn_synergy_score)
end

(** Get full lineup chemistry analysis for a team *)
let get_lineup_chemistry ~season ~team_name () : (Domain.lineup_chemistry, db_error) result =
  let (let*) = Result.bind in
  let open Domain in
  let* data = get_lineup_data ~season ~team_name () in
  let lineups = LineupAnalysis.analyze_lineups ~team_name data in
  let synergies = LineupAnalysis.analyze_synergies lineups in

  (* Sort lineups by different criteria *)
  let by_minutes =
    lineups
    |> List.sort (compare_lineup_stats LineupByMinutes)
    |> (fun l -> let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
      in take 10 [] l) in

  let by_plus_minus =
    lineups
    |> List.filter (fun l -> l.ls_total_minutes >= 20.0)  (* Min 20 min filter *)
    |> List.sort (compare_lineup_stats LineupByPlusMinus)
    |> (fun l -> let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
      in take 10 [] l) in

  let top_synergies =
    synergies
    |> List.filter (fun s -> s.syn_games_together >= 2)
    |> (fun l -> let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
      in take 20 [] l) in

  Ok {
    lc_team_name = team_name;
    lc_season = season;
    lc_top_lineups = by_plus_minus;
    lc_frequent_lineups = by_minutes;
    lc_synergies = top_synergies;
  }

(* ========== Play-by-Play / Quarter Scores ========== *)

(** Get quarter scores for a game from PBP data *)
let get_quarter_scores game_id =
  match with_db (fun db -> Repo.get_quarter_scores_raw db game_id) with
  | Error e -> Error e
  | Ok rows ->
      let quarters = List.map (fun (period, home, away) ->
        { qs_period = period; qs_home_score = home; qs_away_score = away }
      ) rows in
      Ok quarters

(** Get game flow summary including lead changes *)
let get_game_flow game_id =
  match get_quarter_scores game_id with
  | Error e -> Error e
  | Ok quarters ->
      (* Calculate lead changes and largest leads from quarter data *)
      let lead_changes = ref 0 in
      let largest_home = ref 0 in
      let largest_away = ref 0 in
      let prev_leader = ref 0 in  (* -1=away, 0=tie, 1=home *)

      List.iter (fun q ->
        let diff = q.qs_home_score - q.qs_away_score in
        let current_leader = if diff > 0 then 1 else if diff < 0 then -1 else 0 in

        (* Track lead changes *)
        if !prev_leader <> 0 && current_leader <> 0 && !prev_leader <> current_leader then
          incr lead_changes;
        if current_leader <> 0 then prev_leader := current_leader;

        (* Track largest leads *)
        if diff > !largest_home then largest_home := diff;
        if diff < 0 && abs diff > !largest_away then largest_away := abs diff;
      ) quarters;

      Ok {
        gf_quarters = quarters;
        gf_lead_changes = !lead_changes;
        gf_largest_lead_home = !largest_home;
        gf_largest_lead_away = !largest_away;
      }

(* ========== PBP Data Quality Verification ========== *)

type pbp_quality_result = {
  pq_t2_home_count: int;      (** Games where T2=HOME pattern verified *)
  pq_incomplete_count: int;   (** Games with missing scores in games table *)
  pq_no_match_count: int;     (** Games where PBP doesn't match (data quality issue) *)
  pq_total_pbp_games: int;    (** Total games with PBP data *)
}

(** Get PBP data quality summary - verifies T2=HOME pattern consistency *)
let get_pbp_data_quality () =
  match with_db (fun db -> Repo.get_pbp_data_quality db) with
  | Error e -> Error e
  | Ok rows ->
      let t2_home = ref 0 in
      let score_missing = ref 0 in
      let mismatch = ref 0 in
      List.iter (fun (pattern, cnt) ->
        match pattern with
        | "T2=HOME" -> t2_home := cnt
        | "SCORE_MISSING" -> score_missing := cnt
        | "MISMATCH" -> mismatch := cnt
        | _ -> ()
      ) rows;
      Ok {
        pq_t2_home_count = !t2_home;
        pq_incomplete_count = !score_missing;  (* games with NULL/0 scores *)
        pq_no_match_count = !mismatch;         (* actual PBP mismatches *)
        pq_total_pbp_games = !t2_home + !score_missing + !mismatch;
      }

type incomplete_game = {
  ig_game_id: string;
  ig_season: string;
  ig_date: string;
  ig_issue: string;
}

(** Get list of games with incomplete PBP data (for re-scraping) *)
let get_incomplete_pbp_games () =
  match with_db (fun db -> Repo.get_games_with_incomplete_pbp db) with
  | Error e -> Error e
  | Ok rows ->
      let games = List.map (fun (gid, season, date, issue) ->
        { ig_game_id = gid; ig_season = season; ig_date = date; ig_issue = issue }
      ) rows in
      Ok games

(** Get schedule progress: (completed_count, total_count) for a season *)
let get_schedule_progress ~season_code () =
  let completed = with_db (fun db ->
    Repo.count_schedule_by_status ~season_code ~status:"completed" db
  ) in
  let scheduled = with_db (fun db ->
    Repo.count_schedule_by_status ~season_code ~status:"scheduled" db
  ) in
  match (completed, scheduled) with
  | Ok (Some c), Ok (Some s) -> Ok (Some (c, c + s))
  | Ok None, Ok (Some s) -> Ok (Some (0, s))
  | Ok (Some c), Ok None -> Ok (Some (c, c))
  | _ -> Ok None

(** Clear in-memory query caches.

    Useful after admin overrides (exclude/restore) when we want the UI to
    reflect DB changes immediately rather than waiting for TTL expiry. *)
let clear_all_caches () =
  let clear_cache (type a) (c : a Cache.t) = Cache.clear c in
  clear_cache seasons_cache;
  clear_cache teams_cache;
  clear_cache data_freshness_cache;
  clear_cache standings_cache;
  clear_cache games_cache;
  clear_cache scored_games_cache;
  clear_cache team_stats_cache;
  clear_cache players_cache;
  clear_cache players_base_cache;
  clear_cache players_by_team_cache;
  clear_cache player_info_cache;
  clear_cache team_detail_cache;
  clear_cache player_profile_cache;
  clear_cache player_season_stats_cache;
  clear_cache player_game_logs_cache;
  clear_cache boxscore_cache;
  clear_cache leaders_base_cache;
  clear_cache leaders_cache;
  clear_cache awards_cache;
  clear_cache awards_db_cache;
  clear_cache awards_count_cache;
  clear_cache awards_leaders_cache;
  clear_cache draft_years_cache;
  clear_cache draft_picks_cache;
  clear_cache trade_years_cache;
  clear_cache trade_events_cache;
  clear_cache qa_report_cache;
  clear_cache qa_schedule_missing_report_cache;
  clear_cache qa_pbp_missing_report_cache;
  clear_cache history_cache;
  clear_cache legends_cache;
  clear_cache coaches_cache;
  clear_cache player_career_cache;
  clear_cache schedule_cache;
  clear_cache mvp_race_cache;
  clear_cache lineup_cache
