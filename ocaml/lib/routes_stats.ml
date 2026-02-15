(** Stats routes: standings, leaders, clutch, lineups, awards, mvp-race, fantasy, transactions, streaks, on-off. *)

open Domain

let routes =
  [
    (* Standings *)
    Kirin.get "/standings" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_standings ~season () with
          | Ok standings -> Kirin.html (Views.standings_page ~lang ~season ~seasons standings)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    Kirin.get "/standings/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_standings ~season () with
      | Ok standings -> Kirin.html (Views.standings_table ~lang ~season standings)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Leaders *)
    Kirin.get "/leaders" (fun request ->
      let lang = Route_helpers.request_lang request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let categories =
            match String.lowercase_ascii scope with
            | "totals" ->
                [ "gp"; "min"; "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
            | "per_36" ->
                [ "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "eff"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
            | _ ->
                [ "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "min"; "eff"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
          in
          let rec fetch_all acc = function
            | [] -> Ok (List.rev acc)
            | category :: rest ->
                match Db.get_leaders ~season ~scope category with
                | Error e -> Error e
                | Ok leaders -> fetch_all ((category, leaders) :: acc) rest
          in
          match fetch_all [] categories with
          | Ok leaders_by_category ->
              let player_info_map = Backfill.get_player_info_map () in
              Kirin.html (Views.leaders_page ~lang ~player_info_map ~season ~seasons ~scope leaders_by_category)
          | Error e ->
              Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Position-based Leaders *)
    Kirin.get "/leaders/by-position" (fun request ->
      let lang = Route_helpers.request_lang request in
      let position = Kirin.query_opt "position" request |> Option.value ~default:"ALL" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let stats = ["pts"; "reb"; "ast"; "eff"] in
          let rec fetch_all acc = function
            | [] -> Ok (List.rev acc)
            | stat :: rest ->
                match Db.get_leaders_by_position ~season ~position stat with
                | Error e -> Error e
                | Ok leaders -> fetch_all ((stat, leaders) :: acc) rest
          in
          match fetch_all [] stats with
          | Ok leaders ->
              let player_info_map = Backfill.get_player_info_map () in
              Kirin.html (Views.position_leaders_page ~lang ~player_info_map ~season ~seasons ~position leaders)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Clutch Time Leaders *)
    Kirin.get "/clutch" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_clutch_stats ~season () with
          | Ok stats ->
              let sorted_stats = List.sort
                (fun (a: clutch_stats) (b: clutch_stats) ->
                  compare b.cs_clutch_points a.cs_clutch_points)
                stats
              in
              Kirin.html (Views.clutch_page ~lang ~season ~seasons sorted_stats)
          | Error e ->
              Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Lineup Chemistry - Full Page *)
    Kirin.get "/lineups" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons (), Db.get_all_teams () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let team = Kirin.query_opt "team" request |> Option.value ~default:"ALL" in
          match Db.get_lineup_chemistry ~season ~team_name:team () with
          | Ok chemistry ->
              Kirin.html (Views_tools.lineup_chemistry_page
                ~lang ~teams ~seasons ~selected_team:team ~selected_season:season chemistry)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Lineup Chemistry - Table Content (HTMX partial) *)
    Kirin.get "/lineups/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let team = Kirin.query_opt "team" request |> Option.value ~default:"ALL" in
          match Db.get_lineup_chemistry ~season ~team_name:team () with
          | Ok chemistry ->
              Kirin.html (Views_tools.lineup_chemistry_table_content chemistry)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Awards (Stat-based, unofficial) *)
    Kirin.get "/awards" (fun request ->
      let lang = Route_helpers.request_lang request in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let prev_season_code =
            if season = "ALL" then None
            else
              let rec loop prev = function
                | [] -> None
                | (s: season_info) :: rest ->
                    if s.code = season then prev else loop (Some s.code) rest
              in
              loop None seasons
          in
          let prev_season_name =
            match prev_season_code with
            | None -> None
            | Some code ->
                seasons
                |> List.find_opt (fun (s: season_info) -> s.code = code)
                |> Option.map (fun (s: season_info) -> s.name)
          in
          let mvp_res = Db.get_stat_mvp_eff ~season ~include_mismatch () in
          let mip_res =
            match prev_season_code with
            | None -> Ok []
            | Some prev_season -> Db.get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch ()
          in
          (* Official awards from DB *)
          let season_name =
            if season = "ALL" then None
            else
              seasons
              |> List.find_opt (fun (s: season_info) -> s.code = season)
              |> Option.map (fun (s: season_info) -> s.name)
          in
          let official_awards =
            match season_name with
            | Some sn ->
                (match Db.get_awards_by_season ~season_name:sn () with
                 | Ok awards -> awards
                 | Error _ -> [])
            | None -> []
          in
          match mvp_res, mip_res with
          | Ok mvp, Ok mip ->
              let player_info_map = Backfill.get_player_info_map () in
              Kirin.html (Views.awards_page ~lang ~player_info_map ~season ~seasons ~include_mismatch ~prev_season_name ~official_awards ~mvp ~mip ())
          | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* MVP Race - Full Page *)
    Kirin.get "/mvp-race" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_mvp_race ~season () with
          | Ok candidates -> Kirin.html (Views_mvp.mvp_race_page ~lang ~season ~seasons candidates)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* MVP Race - Table HTMX *)
    Kirin.get "/mvp-race/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_mvp_race ~season () with
          | Ok candidates -> Kirin.html (Views_mvp.mvp_race_table candidates)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Fantasy Calculator - Full Page *)
    Kirin.get "/fantasy" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let pts = Option.bind (Kirin.query_opt "pts" request) float_of_string_opt |> Option.value ~default:1.0 in
          let reb = Option.bind (Kirin.query_opt "reb" request) float_of_string_opt |> Option.value ~default:1.2 in
          let ast = Option.bind (Kirin.query_opt "ast" request) float_of_string_opt |> Option.value ~default:1.5 in
          let stl = Option.bind (Kirin.query_opt "stl" request) float_of_string_opt |> Option.value ~default:2.0 in
          let blk = Option.bind (Kirin.query_opt "blk" request) float_of_string_opt |> Option.value ~default:2.0 in
          let tov = Option.bind (Kirin.query_opt "tov" request) float_of_string_opt |> Option.value ~default:(-1.0) in
          let rules : fantasy_scoring_rule = {
            fsr_points = pts;
            fsr_rebounds = reb;
            fsr_assists = ast;
            fsr_steals = stl;
            fsr_blocks = blk;
            fsr_turnovers = tov;
          } in
          match Db.get_players ~season ~search:"" ~sort:ByMinutes () with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok players ->
              let scores = List.map (fantasy_score_of_aggregate ~rules) players in
              Kirin.html (Views_tools.fantasy_calculator_page ~lang ~season ~seasons ~rules ~scores ())
    );

    (* Fantasy Calculator - HTMX Calculate Endpoint *)
    Kirin.get "/fantasy/calculate" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let pts = Option.bind (Kirin.query_opt "pts" request) float_of_string_opt |> Option.value ~default:1.0 in
          let reb = Option.bind (Kirin.query_opt "reb" request) float_of_string_opt |> Option.value ~default:1.2 in
          let ast = Option.bind (Kirin.query_opt "ast" request) float_of_string_opt |> Option.value ~default:1.5 in
          let stl = Option.bind (Kirin.query_opt "stl" request) float_of_string_opt |> Option.value ~default:2.0 in
          let blk = Option.bind (Kirin.query_opt "blk" request) float_of_string_opt |> Option.value ~default:2.0 in
          let tov = Option.bind (Kirin.query_opt "tov" request) float_of_string_opt |> Option.value ~default:(-1.0) in
          let rules : fantasy_scoring_rule = {
            fsr_points = pts;
            fsr_rebounds = reb;
            fsr_assists = ast;
            fsr_steals = stl;
            fsr_blocks = blk;
            fsr_turnovers = tov;
          } in
          match Db.get_players ~season ~search:"" ~sort:ByMinutes () with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok players ->
              let scores = List.map (fantasy_score_of_aggregate ~rules) players in
              Kirin.html (Views_tools.fantasy_results_table scores)
    );

    (* Convenience aliases for transactions *)
    Kirin.get "/draft" (fun request ->
      let year = Kirin.query_opt "year" request |> Option.value ~default:"" in
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      let params = String.concat "&" (List.filter (fun s -> s <> "") [
        "tab=draft";
        (if year <> "" then "year=" ^ year else "");
        (if q <> "" then "q=" ^ q else "")
      ]) in
      Kirin.redirect ("/transactions?" ^ params)
    );
    Kirin.get "/trade" (fun request ->
      let year = Kirin.query_opt "year" request |> Option.value ~default:"" in
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      let params = String.concat "&" (List.filter (fun s -> s <> "") [
        "tab=trade";
        (if year <> "" then "year=" ^ year else "");
        (if q <> "" then "q=" ^ q else "")
      ]) in
      Kirin.redirect ("/transactions?" ^ params)
    );

    (* Draft / Trade (official transactions) *)
    Kirin.get "/transactions" (fun request ->
      let lang = Route_helpers.request_lang request in
      let tab =
        Kirin.query_opt "tab" request
        |> Option.map String.lowercase_ascii
        |> Option.value ~default:"draft"
      in
      let show_ops = Route_helpers.query_bool request "ops" in
      let year =
        let year_str_opt = Kirin.query_opt "year" request in
        match year_str_opt with
        | None -> 0
        | Some s -> (match int_of_string_opt s with Some i -> i | None -> 0)
      in
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      match Db.get_draft_years (), Db.get_official_trade_years () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok draft_years, Ok trade_years ->
          if tab = "trade" then (
            match Db.get_official_trade_events ~year ~search:q () with
            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
            | Ok events ->
                Kirin.html
                  (Views_tools.transactions_page
                     ~lang
                     ~show_ops
                     ~tab
                     ~year
                     ~q
                     ~draft_years
                     ~trade_years
                     ~draft_picks:[]
                     ~trade_events:events
                     ())
          ) else (
            match Db.get_draft_picks ~year ~search:q () with
            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
            | Ok picks ->
                Kirin.html
                  (Views_tools.transactions_page
                     ~lang
                     ~show_ops
                     ~tab:"draft"
                     ~year
                     ~q
                     ~draft_years
                     ~trade_years
                     ~draft_picks:picks
                     ~trade_events:[]
                     ())
          )
    );

    (* Hot Streaks *)
    Kirin.get "/streaks" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_players ~season ~search:"" ~sort:ByEfficiency (), Db.get_all_teams () with
          | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok players, Ok teams ->
              let top_players = List.filteri (fun i _ -> i < 30) players in
              let player_ids = List.map (fun (p: player_aggregate) -> p.player_id) top_players in
              let player_streaks = match Db.get_batch_player_game_logs ~player_ids ~season () with
                | Error _ -> []
                | Ok games_tbl ->
                    List.filter_map (fun (p: player_aggregate) ->
                      match Hashtbl.find_opt games_tbl p.player_id with
                      | Some games when List.length games >= 2 ->
                          Some (Streaks.analyze_player_streaks
                            ~player_id:p.player_id
                            ~player_name:p.name
                            ~team_name:p.team_name
                            games)
                      | _ -> None
                    ) top_players
              in
              let all_player_streaks = List.concat player_streaks in
              let active_player_streaks = Streaks.get_active_streaks all_player_streaks in

              let team_streaks =
                List.filter_map (fun (t: team_info) ->
                  match Db.get_team_full_detail ~team_name:t.team_name ~season () with
                  | Ok detail ->
                      Some (Streaks.calculate_team_win_streaks ~team_name:t.team_name detail.tfd_game_results)
                  | Error _ -> None
                ) teams
              in
              let all_team_streaks = List.concat team_streaks in
              let active_team_streaks = all_team_streaks |> List.filter (fun s -> s.ts_is_active) in

              let best_player_streaks = Streaks.get_best_streaks all_player_streaks in
              let player_records = List.map (Streaks.player_streak_to_record ~season) best_player_streaks in
              let team_records = List.map (Streaks.team_streak_to_record ~season) all_team_streaks in
              let all_time_records =
                (player_records @ team_records)
                |> List.sort (fun a b -> compare b.sr_count a.sr_count)
                |> List.filteri (fun i _ -> i < 20)
              in

              Kirin.html (Views_streaks.streaks_page
                ~lang
                ~season
                ~seasons
                ~active_player_streaks
                ~active_team_streaks
                ~all_time_records
                ())
    );

    (* On/Off Impact *)
    Kirin.get "/on-off" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_on_off_impact_stats ~season () with
          | Ok impacts -> Kirin.html (Views_tools.on_off_impact_page ~lang ~season ~seasons impacts)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );
  ]
