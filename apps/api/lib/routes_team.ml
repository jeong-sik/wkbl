(** Team routes: team list, stats tables, charts, team profile, and H2H comparison. *)

let routes =
  [
    Kirin.get "/teams" (fun request ->
      let lang = Route_helpers.request_lang request in
      let scope_str = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_team_stats ~season ~scope ~sort ~include_mismatch () with
          | Ok stats -> Kirin.html (Views.teams_page ~lang ~season ~seasons ~scope ~sort:sort_str ~include_mismatch stats)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    Kirin.get "/teams/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let scope_str = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope ~sort ~include_mismatch () with
      | Ok stats -> Kirin.html (Views.teams_table ~lang ~season ~scope stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Team Shooting Comparison Chart - HTMX Partial *)
    Kirin.get "/teams/shooting-chart" (fun request ->
      let lang = Route_helpers.request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope:Domain.PerGame ~sort:Domain.TeamByPoints ~include_mismatch () with
      | Ok stats -> Kirin.html (Views_charts.team_shooting_comparison stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Team Radar Chart - HTMX Partial *)
    Kirin.get "/teams/radar-chart" (fun request ->
      let lang = Route_helpers.request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope:Domain.PerGame ~sort:Domain.TeamByEfficiency ~include_mismatch () with
      | Ok stats -> Kirin.html (Views_charts.team_radar_chart stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Team Profile *)
    Kirin.get "/team/:name" (fun request ->
      let lang = Route_helpers.request_lang request in
      let team_name = Kirin.param "name" request |> Uri.pct_decode in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let seasons_for_team =
            match Db.get_team_available_seasons ~team_name () with
            | Ok [] | Error _ -> seasons
            | Ok xs -> xs
          in
          let season = Route_helpers.query_season_or_latest request seasons_for_team in
          (match Db.get_team_full_detail ~team_name ~season () with
          | Ok detail ->
              let normalize_team_key raw_team_name =
                let normalized = Domain.normalize_label raw_team_name in
                match Domain.team_code_of_string normalized with
                | Some code -> "code:" ^ code
                | None -> "name:" ^ (String.uppercase_ascii normalized)
              in
              let team_key = normalize_team_key team_name in
              let coaches =
                match Db.get_coaches () with
                | Ok rows ->
                    rows
                    |> List.filter (fun (c: Domain.coach) ->
                        match c.c_team with
                        | None -> false
                        | Some coach_team -> normalize_team_key coach_team = team_key)
                | Error _ -> []
              in
              let player_info_map = Backfill.get_player_info_map () in
              Kirin.html (Views_team.team_profile_page ~lang ~player_info_map ~coaches detail ~season ~seasons:seasons_for_team)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    (* Team H2H comparison *)
    Kirin.get "/teams/h2h" (fun request ->
      let lang = Route_helpers.request_lang request in
      let team1 = Kirin.query_opt "team1" request |> Option.value ~default:"" in
      let team2 = Kirin.query_opt "team2" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          if team1 = "" || team2 = "" then
            Kirin.html (Views_team.team_h2h_page ~lang ~team1 ~team2 ~season ~seasons [])
          else
            (match Db.get_team_h2h_data ~team1 ~team2 ~season () with
            | Ok games -> Kirin.html (Views_team.team_h2h_page ~lang ~team1 ~team2 ~season ~seasons games)
            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );
  ]
