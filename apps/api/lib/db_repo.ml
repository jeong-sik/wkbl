(** Database repository operations.
    Functions using (module Db : Caqti_eio.CONNECTION) for actual DB access.
*)

open Domain
open Db_common
open Db_queries

    let ensure_schema (module Db : Caqti_eio.CONNECTION) =
      let (let*) = Result.bind in
      let* () = Db.exec ensure_legend_players_table () in
      let* () = Db.exec seed_legend_players () in
      let* () = Db.exec seed_historical_teams () in
      (* Data-quality overrides must exist before any derived views/materialized views. *)
      let* () = Db.exec ensure_game_stats_exclusions_table () in
      let* () = Db.exec ensure_game_stats_clean_view () in
      
      (* Use games_calc_v3 instead of dropping old view synchronously *)
      (* let* () = Db.exec drop_games_calc_view () in *)
      let* () = Db.exec ensure_games_calc_matview () in
      let* () = Db.exec ensure_games_calc_index () in
      let* () = Db.exec ensure_games_calc_season_index () in

      let* () = Db.exec ensure_player_plus_minus_table () in
      let* () = Db.exec ensure_player_plus_minus_index () in
      let* () = Db.exec ensure_play_by_play_events_table () in
      let* () = Db.exec ensure_play_by_play_events_player_id_column () in
      let* () = Db.exec ensure_play_by_play_events_index_game () in
      let* () = Db.exec ensure_play_by_play_events_index_game_period () in
      let* () = Db.exec ensure_play_by_play_events_index_player () in
      let* () = Db.exec ensure_player_drafts_table () in
      let* () = Db.exec ensure_player_drafts_index_year () in
      let* () = Db.exec ensure_official_trade_events_table () in
      let* () = Db.exec ensure_official_trade_events_index_date () in
      let* () = Db.exec ensure_official_trade_events_index_year () in
      let* () = Db.exec ensure_player_external_links_table () in
      let* () = Db.exec ensure_player_external_links_index_player () in
      let* () = Db.exec ensure_coaches_table () in
      let* () = Db.exec ensure_coaches_index_team () in
      let* () = Db.exec ensure_coaches_index_name () in
      (* Performance indexes for game_stats - critical for fast queries *)
      let* () = Db.exec ensure_game_stats_index_game () in
      let* () = Db.exec ensure_game_stats_index_player () in
      let* () = Db.exec ensure_game_stats_index_team () in
      let* () = Db.exec ensure_game_stats_index_game_team () in
      let* () = Db.exec ensure_game_stats_index_player () in
      let* () = Db.exec ensure_games_index_season_type () in
      let* () = Db.exec ensure_games_index_date () in
      let* () = Db.exec ensure_legend_players_index_name () in
      let* () = Db.exec ensure_player_plus_minus_table () in
      (* Schedule table for upcoming games *)
      let* () = Db.exec ensure_schedule_table () in
      let* () = Db.exec ensure_schedule_index_date () in
      let* () = Db.exec ensure_schedule_index_status () in
      let* () = Db.exec ensure_schedule_index_season () in
      (* Canonicalize duplicate player identities (same name + birth_date). *)
      let* () = Db.exec ensure_player_identities_view () in
      (* Leaders base cache MATERIALIZED VIEW - drop first to update schema *)
      let* () = Db.exec drop_leaders_base_cache () in
      let* () = Db.exec ensure_leaders_base_cache_view () in
      let* () = Db.exec ensure_leaders_base_cache_unique () in
      let* () = Db.exec ensure_leaders_base_cache_index_season () in
      let* () = Db.exec refresh_leaders_base_cache () in
      (* Migration: Drop old regular VIEWs before creating MATERIALIZED VIEWs *)
      let* () = Db.exec drop_score_mismatch_view () in
      let* () = Db.exec drop_games_calc_view () in
      (* Materialized Views for performance - pre-computed and cached *)
      let* () = Db.exec ensure_score_mismatch_matview () in
      let* () = Db.exec ensure_score_mismatch_index () in
      let* () = Db.exec ensure_games_calc_matview () in
      let* () = Db.exec ensure_games_calc_index () in
      let* () = Db.exec ensure_games_calc_season_index () in
      let* () = Db.exec ensure_games_calc_compat_view () in
      (* Awards table for scraped WKBL award data *)
      let* () = Db.exec ensure_awards_table () in
      let* () = Db.exec ensure_awards_index_season () in
      let* () = Db.exec ensure_awards_index_category () in
      Db.exec ensure_awards_index_player ()

    (* Refresh materialized views - call after data sync *)
    let refresh_matviews (module Db : Caqti_eio.CONNECTION) =
      let (let*) = Result.bind in
      let* () = Db.exec refresh_leaders_base_cache () in
      let* () = Db.exec refresh_games_calc () in
      Db.exec refresh_score_mismatch ()
  let get_teams (module Db : Caqti_eio.CONNECTION) = Db.collect_list all_teams ()
  let get_seasons (module Db : Caqti_eio.CONNECTION) = Db.collect_list all_seasons ()
  let get_team_available_seasons ~team_name (module Db : Caqti_eio.CONNECTION) =
    let team_name = normalize_label team_name in
    let team_code =
      match team_code_of_string team_name with
      | Some c -> c
      | None -> "__NO_TEAM_CODE__"
    in
    Db.collect_list team_available_seasons (team_code, team_code, team_name, team_name)
  let get_all_player_info (module Db : Caqti_eio.CONNECTION) = Db.collect_list all_player_info ()
  let get_latest_game_date (module Db : Caqti_eio.CONNECTION) = Db.find_opt latest_game_date ()
  let get_historical_seasons (module Db : Caqti_eio.CONNECTION) = Db.collect_list all_historical_seasons ()
  let get_legend_players (module Db : Caqti_eio.CONNECTION) = Db.collect_list all_legend_players ()
  let get_quarter_scores_raw (module Db : Caqti_eio.CONNECTION) game_id = Db.collect_list quarter_scores_by_game game_id
  let get_pbp_data_quality (module Db : Caqti_eio.CONNECTION) = Db.collect_list pbp_data_quality ()
  let get_games_with_incomplete_pbp (module Db : Caqti_eio.CONNECTION) = Db.collect_list games_with_incomplete_pbp ()
  let get_coaches (module Db : Caqti_eio.CONNECTION) = Db.collect_list all_coaches ()
  let get_player_career ~player_name (module Db : Caqti_eio.CONNECTION) = Db.collect_list player_career_by_name player_name
  let get_player_by_name ~name ~season (module Db : Caqti_eio.CONNECTION) = let s = if season = "" then "ALL" else season in Db.find_opt player_by_name (name, (s, s))
  let get_player_aggregate_by_id ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt player_aggregate_by_id (s, (s, (player_id, (s, s))))

  let get_player_shooting_stats ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt player_shooting_stats_by_id (player_id, (s, s))

  (* Shot chart data: zone stats + player info *)
  let get_player_shot_chart ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let open Domain in
    let s = if String.trim season = "" then "ALL" else season in
    let (let*) = Result.bind in
    let* zone_rows = Db.collect_list player_shot_stats (player_id, s) in
    let* player_info = Db.find_opt player_shot_info player_id in
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
    Db.collect_list draft_years ()

  let get_draft_picks ~year ~search (module Db : Caqti_eio.CONNECTION) =
    let pattern = normalize_search_pattern (normalize_label search) in
    Db.collect_list draft_picks_filtered ((year, year), (pattern, pattern))

  let get_draft_dataset_status (module Db : Caqti_eio.CONNECTION) =
    let contains_ci haystack needle =
      let haystack = String.lowercase_ascii haystack in
      let needle = String.lowercase_ascii needle in
      let hlen = String.length haystack in
      let nlen = String.length needle in
      let rec loop i =
        if i + nlen > hlen then false
        else if String.sub haystack i nlen = needle then true
        else loop (i + 1)
      in
      if nlen = 0 then true else loop 0
    in
    let default_status = { ds_count = 0; ds_last_scraped_at = None; ds_reason = Some "missing_data" } in
    match Db.find_opt draft_dataset_status () with
    | Ok (Some (count, last_scraped_at, reason)) ->
        Ok { ds_count = count; ds_last_scraped_at = last_scraped_at; ds_reason = reason }
    | Ok None -> Ok default_status
    | Error e ->
        let msg = Caqti_error.show e in
        if contains_ci msg "undefined_table"
           || contains_ci msg "relation \"player_drafts\" does not exist"
        then Ok default_status
        else Error e

  let get_official_trade_years (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list official_trade_years ()

  let get_official_trade_events ~year ~search (module Db : Caqti_eio.CONNECTION) =
    let pattern = normalize_search_pattern (normalize_label search) in
    Db.collect_list official_trade_events_filtered ((year, year), pattern)

  let get_trade_dataset_status (module Db : Caqti_eio.CONNECTION) =
    let contains_ci haystack needle =
      let haystack = String.lowercase_ascii haystack in
      let needle = String.lowercase_ascii needle in
      let hlen = String.length haystack in
      let nlen = String.length needle in
      let rec loop i =
        if i + nlen > hlen then false
        else if String.sub haystack i nlen = needle then true
        else loop (i + 1)
      in
      if nlen = 0 then true else loop 0
    in
    let default_status = { ds_count = 0; ds_last_scraped_at = None; ds_reason = Some "missing_data" } in
    match Db.find_opt trade_dataset_status () with
    | Ok (Some (count, last_scraped_at, reason)) ->
        Ok { ds_count = count; ds_last_scraped_at = last_scraped_at; ds_reason = reason }
    | Ok None -> Ok default_status
    | Error e ->
        let msg = Caqti_error.show e in
        if contains_ci msg "undefined_table"
           || contains_ci msg "relation \"official_trade_events\" does not exist"
        then Ok default_status
        else Error e

  (* Schedule queries *)
  let get_upcoming_schedule ~status ~limit (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list get_upcoming_schedule (status, limit)

  let get_schedule_by_date_range ~start_date ~end_date ~status (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list get_schedule_by_date_range (start_date, (end_date, (status, status)))

  (** Upsert a single schedule entry *)
  let upsert_schedule_entry ~game_date ~game_time ~season_code ~home_team_code ~away_team_code ~venue ~status (module Db : Caqti_eio.CONNECTION) =
    Db.exec upsert_schedule (game_date, (game_time, (season_code, (home_team_code, (away_team_code, (venue, status))))))

  (** Delete schedule entries for a season (derived data, safe to rebuild) *)
  let delete_schedule_by_season ~season_code (module Db : Caqti_eio.CONNECTION) =
    Db.exec delete_schedule_by_season season_code

  (** Upsert a single season entry *)
  let upsert_season ~season_code ~season_name (module Db : Caqti_eio.CONNECTION) =
    Db.exec upsert_season (season_code, season_name)

  (* Awards queries *)
  let upsert_award ~season_name ~category ~award_type ~player_name ~stat_value ~votes (module Db : Caqti_eio.CONNECTION) =
    Db.exec upsert_award (season_name, (category, (award_type, (player_name, (stat_value, votes)))))

  let get_awards_by_category ~category (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list awards_by_category category

  let get_awards_by_season ~season_name (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list awards_by_season season_name

  let get_awards_by_player ~player_name (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list awards_by_player player_name

  let get_award_count (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt count_awards ()

  let get_award_leaders ~category (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list award_leaders category

  (** Data-quality overrides: exclude/restore a (game_id, player_id) stat row *)
  let upsert_game_stats_exclusion ~game_id ~player_id ~reason (module Db : Caqti_eio.CONNECTION) =
    Db.exec upsert_game_stats_exclusion (game_id, (player_id, reason))

  let delete_game_stats_exclusion ~game_id ~player_id (module Db : Caqti_eio.CONNECTION) =
    Db.exec delete_game_stats_exclusion (game_id, player_id)

  let qa_stat_exclusions ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list qa_stat_exclusions (s, s)

  let qa_stat_anomaly_candidates ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list qa_stat_anomaly_candidates (s, s)

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
    Db.exec upsert_game
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
    Db.find_opt count_schedule_by_status (season_code, status)

  let get_players_base ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list player_stats_by_season_base (s, (s, include_int))
  let get_top_players ?(team_name="ALL") ~limit (module Db : Caqti_eio.CONNECTION) = Db.collect_list player_stats_base ((team_name, team_name), limit)
  let get_players_by_team ~team_name ~season ~limit (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list players_by_team (team_name, (s, (s, limit)))
  let get_team_totals ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list team_totals_by_season (season, (season, (season, include_int)))

  let get_team_margins ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list team_margin_by_season (season, include_int)
  let get_standings ~season (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list team_standings_by_season season
  let get_games ?(page = 1) ?(page_size = 50) ~season (module Db : Caqti_eio.CONNECTION) =
    let offset = (page - 1) * page_size in
    Db.collect_list all_games_paginated ((season, season), (page_size, offset))
  let get_scored_games ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list scored_games_by_season (season, (season, include_int))
  let get_game_season_code ~game_id (module Db : Caqti_eio.CONNECTION) = Db.find_opt game_season_by_id game_id
  let get_pbp_periods ~game_id (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list pbp_periods_by_game game_id
  let get_pbp_events ~game_id ~period_code (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list pbp_events_by_game_period (game_id, period_code)
  let get_team_core_player_ids ~season ~team_name ~limit (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list team_core_player_ids (team_name, ((season, season), limit))
  let get_team_active_player_ids ~team_name ~game_id (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list team_active_player_ids (team_name, game_id)

  let qa_games_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_games_total ()
  let qa_finished_games_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_finished_games_total ()
  let qa_games_with_stats (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_games_with_stats ()
  let qa_pbp_games (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_pbp_games ()
  let qa_plus_minus_games (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_plus_minus_games ()
  let qa_finished_games_total_by_season ~season (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt qa_finished_games_total_by_season season
  let qa_pbp_games_by_season ~season (module Db : Caqti_eio.CONNECTION) =
    Db.find_opt qa_pbp_games_by_season season
  let qa_pbp_missing_games_sample_by_season ~season (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list qa_pbp_missing_games_sample_by_season season
  let qa_score_mismatch_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_score_mismatch_count ()
  let qa_score_mismatch_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_score_mismatch_sample ()
  let qa_team_count_anomaly_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_team_count_anomaly_count ()
  let qa_team_count_anomaly_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_team_count_anomaly_sample ()
  let qa_duplicate_player_row_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_duplicate_player_row_count ()
  let qa_duplicate_player_row_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_duplicate_player_row_sample ()
  let qa_duplicate_player_name_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_duplicate_player_name_count ()
  let qa_duplicate_player_name_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_duplicate_player_name_sample ()
  let qa_duplicate_player_identity_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_duplicate_player_identity_count ()
  let qa_duplicate_player_identity_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_duplicate_player_identity_sample ()
  let qa_schedule_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_schedule_total ()
  let qa_schedule_completed (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_schedule_completed ()
  let qa_schedule_missing_game_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_schedule_missing_game_count ()
  let qa_schedule_missing_game_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_schedule_missing_game_sample ()
  let qa_schedule_missing_stats_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_schedule_missing_stats_count ()
  let qa_schedule_missing_stats_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_schedule_missing_stats_sample ()
  let qa_schedule_coverage (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_schedule_coverage ()
  let qa_schedule_missing_summary (module Db : Caqti_eio.CONNECTION) = Db.find_opt qa_schedule_missing_summary ()
  let qa_schedule_missing_reason_counts (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_schedule_missing_reason_counts ()
  let qa_schedule_missing_samples (module Db : Caqti_eio.CONNECTION) = Db.collect_list qa_schedule_missing_samples ()
  let get_game_info ~game_id (module Db : Caqti_eio.CONNECTION) = Db.find_opt game_info_by_id game_id
  let get_boxscore_stats ~game_id (module Db : Caqti_eio.CONNECTION) = Db.collect_list boxscore_stats_by_game_id game_id
  let get_leaders ~category ~scope ~season (module Db : Caqti_eio.CONNECTION) =
    let q =
      match (category, scope) with
      | "gp", _ -> leaders_gp
      | "min", "totals" -> leaders_min_totals
      | "min", _ -> leaders_min
      | "pts", "totals" -> leaders_pts_totals
      | "pts", "per_36" -> leaders_pts_per36
      | "pts", _ -> leaders_pts
      | "reb", "totals" -> leaders_reb_totals
      | "reb", "per_36" -> leaders_reb_per36
      | "reb", _ -> leaders_reb
      | "ast", "totals" -> leaders_ast_totals
      | "ast", "per_36" -> leaders_ast_per36
      | "ast", _ -> leaders_ast
      | "stl", "totals" -> leaders_stl_totals
      | "stl", "per_36" -> leaders_stl_per36
      | "stl", _ -> leaders_stl
      | "blk", "totals" -> leaders_blk_totals
      | "blk", "per_36" -> leaders_blk_per36
      | "blk", _ -> leaders_blk
      | "tov", "totals" -> leaders_tov_totals
      | "tov", "per_36" -> leaders_tov_per36
      | "tov", _ -> leaders_tov
      | "eff", "per_36" -> leaders_eff_per36
      | "eff", _ -> leaders_eff
      | "fg_pct", _ -> leaders_fg_pct
      | "fg3_pct", _ -> leaders_fg3_pct
      | "ft_pct", _ -> leaders_ft_pct
      | "ts_pct", _ -> leaders_ts_pct
      | "efg_pct", _ -> leaders_efg_pct
      | "usg_pct", _ -> leaders_usg_pct
      | _ -> leaders_pts
    in
    Db.collect_list q (season, season)

  let get_leaders_base ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    if s = "ALL" then
      Db.collect_list leaders_base_cached_all ()
    else
      Db.collect_list leaders_base_cached s

  (** Get leaders filtered by position
      @param position "G" (Guard), "F" (Forward), "C" (Center), or "ALL"
      @param stat "pts", "reb", "ast", "eff"
  *)
  let get_leaders_by_position ~season ~position ~stat (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    let pos = if String.trim position = "" then "ALL" else position in
    let q = match stat with
      | "reb" -> leaders_reb_by_position
      | "ast" -> leaders_ast_by_position
      | "eff" -> leaders_eff_by_position
      | _ -> leaders_pts_by_position
    in
    Db.collect_list q ((s, s), pos)

  let get_stat_mvp_eff ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list stat_mvp_eff (s, (s, include_int))

  let get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list stat_mip_eff_delta (season, (include_int, (prev_season, include_int)))
  
  let get_player_profile ~player_id (module Db : Caqti_eio.CONNECTION) =
    let (let*) = Result.bind in
    let* info = Db.find_opt player_info player_id in
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
		        let* seasons = Db.collect_list player_seasons_per_game player_id in 
		        let* recent = Db.collect_list player_recent_games player_id in 
		        let* all_star = Db.collect_list player_all_star_games player_id in
		        let* team_games = Db.collect_list player_team_games player_id in
		        let* averages_res = Db.find_opt player_career_aggregate player_id in
              let* draft_res = Db.find_opt player_draft_by_player_id player_id in
              let name_norm = normalize_label p.name in
              let trade_pattern = "%" ^ name_norm ^ "%" in
              let* trade_events_res =
                if String.trim name_norm = "" then
                  Ok ([])
                else
                  Db.collect_list official_trade_events_by_player_name_norm trade_pattern
              in
              let* external_links_res =
                Db.collect_list player_external_links_by_player_id player_id
              in
			        let* ch_points = Db.find_opt career_high_points_game player_id in
			        let* ch_rebounds = Db.find_opt career_high_rebounds_game player_id in
			        let* ch_assists = Db.find_opt career_high_assists_game player_id in
			        let* ch_steals = Db.find_opt career_high_steals_game player_id in
			        let* ch_blocks = Db.find_opt career_high_blocks_game player_id in
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
			            let career_entries = match Db.collect_list player_career_by_name p.name with Ok es -> es | Error _ -> [] in
			            Ok ((Some { player = p; averages; recent_games = recent; all_star_games = all_star; draft = draft_res; official_trade_events = trade_events_res; external_links = external_links_res; team_stints; season_breakdown = seasons; career_highs; career_entries }))

  let get_player_season_stats ~player_id ~scope (module Db : Caqti_eio.CONNECTION) = let q = match scope with | "totals" -> player_seasons_totals | "per_36" -> player_seasons_per36 | _ -> player_seasons_per_game in Db.collect_list q player_id
  let get_player_game_logs ~player_id ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list player_game_logs (player_id, (s, (s, include_int)))
  let get_batch_player_game_logs ~player_ids ~season (module Db : Caqti_eio.CONNECTION) =
    let ids_csv = String.concat "," player_ids in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list batch_player_game_logs (ids_csv, (s, s))
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
    Db.collect_list team_recent_games t
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
    Db.collect_list team_games t
  let get_player_h2h ~p1_id ~p2_id ~season (module Db : Caqti_eio.CONNECTION) = let s = if season = "" then "ALL" else season in Db.collect_list player_h2h_games ((p1_id, p2_id), (s, s))

    (** Get head-to-head games between two teams *)
    let get_team_h2h ~team1 ~team2 ~season (module Db : Caqti_eio.CONNECTION) =
      let s = if String.trim season = "" then "ALL" else season in
      (* Params: (((t1, t2), (t2, t1)), (s, s)) for both home/away scenarios *)
      Db.collect_list team_h2h_games (((team1, team2), (team2, team1)), (s, s))

    (** Get MVP race candidates for a season *)
    let get_mvp_race_candidates ~season ~min_games (module Db : Caqti_eio.CONNECTION) =
      let s = if String.trim season = "" then "ALL" else season in
      Db.collect_list mvp_race_candidates (s, min_games)
