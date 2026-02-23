(** Common database utilities and types.

    Extracted from db.ml for modularity.
    Depends on Domain (for extract_game_info).
    No Caqti dependency.
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

(** QA (data quality) report types. *)
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

(** Leader base type for leaderboard queries *)
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

(** Player base type for player lists *)
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

(** Normalize search pattern for LIKE queries *)
let normalize_search_pattern search =
  let trimmed = String.trim search in
  if trimmed = "" then "%" else "%" ^ trimmed ^ "%"

(** Key type for margin lookups (season, team) *)
module MarginKey = struct
  type t = string * string
  let compare = compare
end

module MarginMap = Map.Make (MarginKey)

(** Take first n items from a list *)
let take n items =
  let rec loop acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> loop (x :: acc) (n - 1) xs
  in
  loop [] n items

(** Normalize text for search matching *)
let normalize_search_text value =
  String.lowercase_ascii (String.trim value)

(** Check if needle is contained in hay (case-insensitive) *)
let string_contains ~needle ~hay =
  if String.length needle = 0 then true
  else
    let needle_lower = String.lowercase_ascii needle in
    let hay_lower = String.lowercase_ascii hay in
    try
      let _ = Str.search_forward (Str.regexp_string needle_lower) hay_lower 0 in
      true
    with Not_found -> false

(** ISO8601 UTC timestamp *)
let iso8601_utc () =
  let open Unix in
  let t = gmtime (gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (1900 + t.tm_year)
    (t.tm_mon + 1)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

(** Coverage percentage rounded to 1 decimal. *)
let coverage_pct ~total ~covered =
  if total <= 0 then 0.0
  else
    let pct = (float_of_int covered /. float_of_int total) *. 100.0 in
    Float.round (pct *. 10.0) /. 10.0

(** Split CSV string into list *)
let split_csv_ids (ids : string) =
  ids
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
