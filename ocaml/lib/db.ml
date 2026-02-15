(** Database Layer using Caqti

    Principles:
    - Type-safe queries
    - Connection pooling via Caqti
    - All errors as Result, never exceptions
    - Memory caching for performance (TTL-based)

    db_common.ml provides: db_error, db_result, QA types, utilities.
    This module adds: Types (Caqti encodings), Queries, Repo, Pool, Cache, public API.
*)

open Domain
include Db_common

(* Types, utilities, and QA report types are provided by Db_common via include above. *)

module Types = Db_types

module Request_oneshot = Db_request

module Queries = Db_queries

module Repo = Db_repo

let init_pool = Db_pool.init_pool
let with_db = Db_pool.with_db

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
