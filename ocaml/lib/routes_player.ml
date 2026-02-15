(** Player routes: player list, profile, game log, splits, season stats, shot charts, career. *)

let routes ~sw ~env =
  [
    (* Players List *)
    Kirin.get "/players" (fun request ->
      let lang = Route_helpers.request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"eff" in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~sort ~include_mismatch () with
          | Ok p ->
              let player_info_map = Backfill.get_player_info_map () in
              Backfill.backfill_duplicate_player_meta ~sw ~env ~players:p player_info_map;
              Kirin.html (Views.players_page ~lang ~player_info_map ~season ~seasons ~search ~sort:sort_str ~include_mismatch p)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Players Table HTMX Partial *)
    Kirin.get "/players/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"eff" in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~sort ~include_mismatch () with
          | Ok p ->
              let player_info_map = Backfill.get_player_info_map () in
              Backfill.backfill_duplicate_player_meta ~sw ~env ~players:p player_info_map;
              Kirin.html (Views.players_table ~lang ~player_info_map p)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Profile *)
    Kirin.get "/player/:id" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.param "id" request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let show_ops = Route_helpers.query_bool request "ops" in
      match Db.get_player_profile ~player_id () with
      | Ok (Some profile) ->
          let canon_id = profile.player.id in
          (match
             Player_identity.redirect_location ~requested_id:player_id ~canonical_id:canon_id
               (Kirin.Request.uri request)
           with
          | Some location -> Kirin.redirect ~status:`Permanent_redirect location
          | None ->
              let final_profile =
                if scope = "per_game" then profile
                else
                  match Db.get_player_season_stats ~player_id ~scope () with
                  | Ok stats -> { profile with season_breakdown = stats }
                  | Error _ -> profile
              in
              let seasons_catalog =
                match Db.get_seasons () with
                | Ok seasons -> seasons
                | Error _ -> []
              in
              let season_for_leaderboards =
                match seasons_catalog with
                | [] -> "ALL"
                | seasons -> Route_helpers.query_season_or_latest request seasons
              in
              let season_name_for_leaderboards =
                seasons_catalog
                |> List.find_opt (fun (s : Domain.season_info) -> s.code = season_for_leaderboards)
                |> Option.map (fun (s : Domain.season_info) -> s.name)
                |> Option.value ~default:season_for_leaderboards
              in
              let leaderboard_categories =
                match String.lowercase_ascii scope with
                | "totals" ->
                    [
                      "gp";
                      "min";
                      "pts";
                      "reb";
                      "ast";
                      "stl";
                      "blk";
                      "tov";
                      "fg_pct";
                      "fg3_pct";
                      "ft_pct";
                      "ts_pct";
                      "efg_pct";
                    ]
                | "per_36" ->
                    [
                      "pts";
                      "reb";
                      "ast";
                      "stl";
                      "blk";
                      "tov";
                      "eff";
                      "fg_pct";
                      "fg3_pct";
                      "ft_pct";
                      "ts_pct";
                      "efg_pct";
                    ]
                | _ ->
                    [
                      "pts";
                      "reb";
                      "ast";
                      "stl";
                      "blk";
                      "tov";
                      "min";
                      "eff";
                      "fg_pct";
                      "fg3_pct";
                      "ft_pct";
                      "ts_pct";
                      "efg_pct";
                    ]
              in
              let rec fetch_all acc = function
                | [] -> Ok (List.rev acc)
                | category :: rest -> (
                    match Db.get_leaders ~season:season_for_leaderboards ~scope category with
                    | Error e -> Error e
                    | Ok leaders -> fetch_all ((category, leaders) :: acc) rest)
              in
              let leaderboards =
                match fetch_all [] leaderboard_categories with
                | Ok leaders_by_category ->
                    Some (season_for_leaderboards, season_name_for_leaderboards, leaders_by_category)
                | Error _ -> None
              in
              Kirin.html
                (Views_player.player_profile_page ~lang ~leaderboards ~show_ops final_profile ~scope
                   ~seasons_catalog))
      | Ok None -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"선수" ())
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Game Log *)
    Kirin.get "/player/:id/games" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.param "id" request in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      match Db.get_player_profile ~player_id (), Db.get_seasons () with
      | Ok None, _ -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"선수" ())
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok (Some profile), Ok seasons ->
          let canon_id = profile.player.id in
          match
            Player_identity.redirect_location ~requested_id:player_id ~canonical_id:canon_id
              ~suffix:"games" (Kirin.Request.uri request)
          with
          | Some location -> Kirin.redirect ~status:`Permanent_redirect location
          | None ->
              let season = Route_helpers.query_season_or_latest request seasons in
              match Db.get_player_game_logs ~player_id ~season ~include_mismatch () with
              | Ok games ->
                  Kirin.html
                    (Views_player.player_game_logs_page ~lang profile ~season ~seasons
                       ~include_mismatch games)
              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Splits -- Home/Away, per-opponent, per-month *)
    Kirin.get "/player/:id/splits" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.param "id" request in
      match Db.get_player_profile ~player_id (), Db.get_seasons () with
      | Ok None, _ -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"선수" ())
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok (Some profile), Ok seasons ->
          let canon_id = profile.player.id in
          match
            Player_identity.redirect_location ~requested_id:player_id ~canonical_id:canon_id
              ~suffix:"splits" (Kirin.Request.uri request)
          with
          | Some location -> Kirin.redirect ~status:`Permanent_redirect location
          | None ->
              let season = Route_helpers.query_season_or_latest request seasons in
              match Db.get_player_game_logs ~player_id ~season ~include_mismatch:false () with
              | Ok games ->
                  Kirin.html
                    (Views_player.player_splits_page ~lang profile ~season ~seasons games)
              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Season Stats HTMX Partial *)
    Kirin.get "/player/:id/season-stats" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.param "id" request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      match Db.get_player_season_stats ~player_id ~scope () with
      | Ok stats -> Kirin.html (Views_common.player_season_stats_component ~player_id ~scope stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Shot Chart HTMX Partial *)
    Kirin.get "/player/:id/shot-chart" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.param "id" request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_player_shooting_stats ~player_id ~season () with
      | Ok (Some stats) -> Kirin.html (Views_charts.player_shot_chart_html stats)
      | Ok None -> Kirin.html {|<div class="text-slate-400 text-center p-4">슈팅 데이터가 없습니다.</div>|}
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Zone Shot Chart (PBP-based) - Full Page *)
    Kirin.get "/player/:id/shots" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.param "id" request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let is_htmx = Kirin.header "HX-Request" request |> Option.is_some in
      match Db.get_player_shot_chart ~player_id ~season () with
      | Ok chart ->
          if is_htmx then
            Kirin.html (Views_charts.zone_shot_chart_partial chart)
          else
            (match Db.get_player_profile ~player_id (), Db.get_seasons () with
            | Ok (Some profile), Ok seasons ->
                Kirin.html (Views_player.player_shot_chart_page ~lang profile ~season ~seasons chart)
            | Ok None, _ -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"선수" ())
            | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Career *)
    Kirin.get "/player/:id/career" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_name = Kirin.param "id" request |> Uri.pct_decode in
      match Db.get_player_career ~player_name () with
      | Ok entries -> Kirin.html (Views_history.player_career_page ~lang ~player_name entries)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );
  ]
