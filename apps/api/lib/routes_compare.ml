(** Compare routes: player comparison, team comparison, season comparison. *)

open Domain

let routes =
  [
    (* Compare - HTMX table fragment *)
    Kirin.get "/compare/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      let compare_type =
        Kirin.query_opt "type" request
        |> Option.map String.lowercase_ascii
        |> Option.value ~default:"player"
      in
      let left = Route_helpers.query_nonempty request "left" |> Option.value ~default:"" in
      let right = Route_helpers.query_nonempty request "right" |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          if left = "" || right = "" then
            Kirin.html (Views.compare_table_empty ())
          else
            match compare_type with
            | "team" -> (
                match Db.get_team_stats ~season ~scope:PerGame ~sort:TeamByPoints () with
                | Error _ -> Kirin.html (Views.compare_table_empty ())
                | Ok stats ->
                    let code_of name = team_code_of_string name in
                    let find_by_code code =
                      match code with
                      | None -> None
                      | Some c ->
                          List.find_opt (fun (row: team_stats) -> code_of row.team = Some c) stats
                    in
                    let left_row = find_by_code (code_of left) in
                    let right_row = find_by_code (code_of right) in
                    (match left_row, right_row with
                    | Some a, Some b ->
                        let rows = [
                          ("PTS", a.pts, b.pts, false);
                          ("MG", a.margin, b.margin, true);
                          ("REB", a.reb, b.reb, false);
                          ("AST", a.ast, b.ast, false);
                          ("STL", a.stl, b.stl, false);
                          ("BLK", a.blk, b.blk, false);
                          ("TO", a.turnovers, b.turnovers, false);
                          ("eFG%", a.efg_pct, b.efg_pct, false);
                          ("TOV%", a.tov_pct, b.tov_pct, false);
                          ("ORB%", a.orb_pct, b.orb_pct, false);
                          ("FTR", a.ftr, b.ftr, false);
                          ("EFF", a.eff, b.eff, false);
                        ] in
                        Kirin.html
                          (Views.compare_table_fragment
                             ~left_label:(Views_common.normalize_name a.team)
                             ~right_label:(Views_common.normalize_name b.team)
                             rows)
                    | _ -> Kirin.html (Views.compare_table_empty ())))
            | _ -> (
                match Db.get_player_aggregate_by_id ~player_id:left ~season (),
                      Db.get_player_aggregate_by_id ~player_id:right ~season () with
                | Ok (Some a), Ok (Some b) ->
                    let rows = [
                      ("PTS", a.avg_points, b.avg_points, false);
                      ("MG", a.avg_margin, b.avg_margin, true);
                      ("REB", a.avg_rebounds, b.avg_rebounds, false);
                      ("AST", a.avg_assists, b.avg_assists, false);
                      ("STL", a.avg_steals, b.avg_steals, false);
                      ("BLK", a.avg_blocks, b.avg_blocks, false);
                      ("TO", a.avg_turnovers, b.avg_turnovers, false);
                      ("EFF", a.efficiency, b.efficiency, false);
                    ] in
                    Kirin.html
                      (Views.compare_table_fragment
                         ~left_label:(Views_common.normalize_name a.name)
                         ~right_label:(Views_common.normalize_name b.name)
                         rows)
                | _ -> Kirin.html (Views.compare_table_empty ()))
    );

    (* Compare Seasons - Compare a player's performance across different seasons *)
    Kirin.get "/compare/seasons" (fun request ->
      let lang = Route_helpers.request_lang request in
      let player_id = Kirin.query_opt "player" request |> Option.value ~default:"" in
      let s1 = Kirin.query_opt "s1" request |> Option.value ~default:"" in
      let s2 = Kirin.query_opt "s2" request |> Option.value ~default:"" in

      if String.trim player_id = "" then
        Kirin.redirect "/compare"
      else
        match Db.get_seasons () with
        | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
        | Ok seasons ->
            (match Db.get_player_profile ~player_id () with
            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
            | Ok None -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"선수" ())
            | Ok (Some profile) ->
                let player_name = profile.player.name in
                (match Db.get_player_season_stats ~player_id ~scope:"per_game" () with
                | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
                | Ok all_seasons ->
                    let find_season code =
                      List.find_opt (fun (s: season_stats) -> s.ss_season_code = code) all_seasons
                    in
                    let s1_stats = if s1 <> "" then find_season s1 else None in
                    let s2_stats = if s2 <> "" then find_season s2 else None in
                    let error =
                      if s1 <> "" && s2 <> "" && s1 = s2 then Some "Please select two different seasons."
                      else if s1 <> "" && s1_stats = None then Some (Printf.sprintf "Season %s not found for this player." s1)
                      else if s2 <> "" && s2_stats = None then Some (Printf.sprintf "Season %s not found for this player." s2)
                      else None
                    in
                    Kirin.html (Views.compare_seasons_page
                      ~lang
                      ~seasons
                      ~player_id
                      ~player_name
                      ~s1
                      ~s2
                      ~s1_stats
                      ~s2_stats
                      ~all_seasons
                      ~error
                      ())))
    );

    (* Compare - simplified version *)
    Kirin.get "/compare" (fun request ->
      let lang = Route_helpers.request_lang request in
      let p1_query = Kirin.query_opt "p1" request |> Option.value ~default:"" in
      let p2_query = Kirin.query_opt "p2" request |> Option.value ~default:"" in
      let p1_id = Kirin.query_opt "p1_id" request |> Option.value ~default:"" in
      let p2_id = Kirin.query_opt "p2_id" request |> Option.value ~default:"" in
      let p1_id_opt = if String.trim p1_id = "" then None else Some (String.trim p1_id) in
      let p2_id_opt = if String.trim p2_id = "" then None else Some (String.trim p2_id) in

      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          let is_valid_season code =
            code = "ALL" || List.exists (fun (s: season_info) -> s.code = code) seasons
          in
          let p1_season =
            match Route_helpers.query_nonempty request "p1_season" with
            | None -> season
            | Some v -> if is_valid_season v then v else season
          in
          let p2_season =
            match Route_helpers.query_nonempty request "p2_season" with
            | None -> season
            | Some v -> if is_valid_season v then v else season
          in
          let errors : string list ref = ref [] in
          let add_error msg = errors := msg :: !errors in

          if p1_id_opt <> None && p1_id_opt = p2_id_opt then add_error "Players must be different.";

          let p1_sel_res =
            match p1_id_opt with
            | None -> Ok None
            | Some pid -> Db.get_player_aggregate_by_id ~player_id:pid ~season:p1_season ()
          in
          let p2_sel_res =
            match p2_id_opt with
            | None -> Ok None
            | Some pid -> Db.get_player_aggregate_by_id ~player_id:pid ~season:p2_season ()
          in
          let p1_selected =
            match p1_sel_res with
            | Ok v -> v
            | Error e -> add_error (Db.show_db_error e); None
          in
          let p2_selected =
            match p2_sel_res with
            | Ok v -> v
            | Error e -> add_error (Db.show_db_error e); None
          in

          let p1_candidates_res =
            if p1_selected = None && String.trim p1_query <> "" then
              let limit = if p1_id_opt <> None then 30 else 8 in
              Db.get_players ~season:p1_season ~search:p1_query ~sort:ByMinutes ~limit ()
            else
              Ok []
          in
          let p2_candidates_res =
            if p2_selected = None && String.trim p2_query <> "" then
              let limit = if p2_id_opt <> None then 30 else 8 in
              Db.get_players ~season:p2_season ~search:p2_query ~sort:ByMinutes ~limit ()
            else
              Ok []
          in
          let p1_candidates = match p1_candidates_res with Ok xs -> xs | Error _ -> [] in
          let p2_candidates = match p2_candidates_res with Ok xs -> xs | Error _ -> [] in

          let p1_selected, p1_candidates =
            match p1_selected, p1_id_opt with
            | None, Some pid ->
                (match List.find_opt (fun (c: player_aggregate) -> c.player_id = pid) p1_candidates with
                | Some c -> (Some c, [])
                | None -> (None, p1_candidates))
            | _ -> (p1_selected, p1_candidates)
          in
          let p2_selected, p2_candidates =
            match p2_selected, p2_id_opt with
            | None, Some pid ->
                (match List.find_opt (fun (c: player_aggregate) -> c.player_id = pid) p2_candidates with
                | Some c -> (Some c, [])
                | None -> (None, p2_candidates))
            | _ -> (p2_selected, p2_candidates)
          in

          let p1_available_seasons =
            match p1_id_opt, p1_selected with
            | Some pid, None -> (
                match Db.get_player_season_stats ~player_id:pid ~scope:"per_game" () with
                | Ok stats ->
                    let codes =
                      stats
                      |> List.map (fun (s: season_stats) -> s.ss_season_code)
                      |> List.sort_uniq String.compare
                    in
                    Some codes
                | Error _ -> None
              )
            | _ -> None
          in
          let p2_available_seasons =
            match p2_id_opt, p2_selected with
            | Some pid, None -> (
                match Db.get_player_season_stats ~player_id:pid ~scope:"per_game" () with
                | Ok stats ->
                    let codes =
                      stats
                      |> List.map (fun (s: season_stats) -> s.ss_season_code)
                      |> List.sort_uniq String.compare
                    in
                    Some codes
                | Error _ -> None
              )
            | _ -> None
          in

          (match p1_id_opt, p1_selected with
          | Some pid, None ->
              let suffix =
                match p1_available_seasons with
                | Some codes when codes <> [] -> Printf.sprintf " (available: %s)" (String.concat "," codes)
                | _ -> ""
              in
              add_error (Printf.sprintf "No stats for player_id=%s (season=%s)%s" pid p1_season suffix)
          | _ -> ());
          (match p2_id_opt, p2_selected with
          | Some pid, None ->
              let suffix =
                match p2_available_seasons with
                | Some codes when codes <> [] -> Printf.sprintf " (available: %s)" (String.concat "," codes)
                | _ -> ""
              in
              add_error (Printf.sprintf "No stats for player_id=%s (season=%s)%s" pid p2_season suffix)
          | _ -> ());

          let h2h_disabled_reason =
            match p1_selected, p2_selected with
            | Some _, Some _ when p1_season <> p2_season ->
                Some "Match History는 같은 시즌 선택 시만 표시됩니다."
            | _ -> None
          in
          let h2h_res =
            match p1_selected, p2_selected, h2h_disabled_reason with
            | Some a, Some b, None ->
                Db.get_player_h2h_data ~p1_id:a.player_id ~p2_id:b.player_id ~season:p1_season ()
            | _ -> Ok []
          in
          let h2h = match h2h_res with Ok h -> h | Error _ -> [] in
          let p1_season_history =
            match p1_selected with
            | Some p -> (
                match Db.get_player_season_stats ~player_id:p.player_id ~scope:"per_game" () with
                | Ok stats -> stats
                | Error _ -> [])
            | None -> []
          in
          let p2_season_history =
            match p2_selected with
            | Some p -> (
                match Db.get_player_season_stats ~player_id:p.player_id ~scope:"per_game" () with
                | Ok stats -> stats
                | Error _ -> [])
            | None -> []
          in
          let error_opt =
            match List.rev !errors with
            | [] -> None
            | xs -> Some (String.concat " / " xs)
          in
          Kirin.html
            (Views.compare_page
               ~lang
               ~season
               ~seasons
               ~p1_season
               ~p2_season
               ~p1_query
               ~p2_query
               ~p1_id:p1_id_opt
               ~p2_id:p2_id_opt
               ~p1_candidates
               ~p2_candidates
               ~error:error_opt
               ~h2h_disabled_reason
               ~p1_season_history
               ~p2_season_history
               p1_selected
               p2_selected
               h2h)
    );
  ]
