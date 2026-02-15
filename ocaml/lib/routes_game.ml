(** Game routes: games list, boxscores, PBP, game summary, game flow. *)

let routes ~sw ~env =
  [
    (* Games List *)
    Kirin.get "/games" (fun request ->
      let lang = Route_helpers.request_lang request in
      let page = Kirin.query_opt "page" request |> Option.map int_of_string |> Option.value ~default:1 in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_games ~season ~page () with
          | Ok games -> Kirin.html (Views.games_page ~lang ~page ~season ~seasons games)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    (* Games Table HTMX Partial *)
    Kirin.get "/games/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_games ~season () with
          | Ok games -> Kirin.html (Views.games_table ~lang games)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    (* Boxscores List *)
    Kirin.get "/boxscores" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_scored_games ~season ~include_mismatch:true () with
          | Ok games -> Kirin.html (Views.boxscores_page ~lang ~season ~seasons games)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    (* Boxscores Table HTMX Partial *)
    Kirin.get "/boxscores/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_scored_games ~season ~include_mismatch:true () with
      | Ok games -> Kirin.html (Views.boxscores_table ~lang games)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Play-by-Play (PBP) Detail - MUST come before /boxscore/:id *)
    Kirin.get "/boxscore/:id/pbp" (fun request ->
      let lang = Route_helpers.request_lang request in
      let game_id = Kirin.param "id" request in
      let period_opt = Route_helpers.query_nonempty request "period" in
      match Db.get_game_info ~game_id () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok None -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"경기" ())
      | Ok (Some game) ->
          let today_kst = Live.today_str () in
          let periods_res =
            match Db.get_pbp_periods ~game_id () with
            | Ok periods as ok ->
                if Domain.pbp_should_backfill ~today_kst game periods then
                  Backfill.Pbp.maybe_spawn ~sw ~env ~game_id ~tag:"PBP" ();
                ok
            | other -> other
          in

          (match periods_res with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok periods ->
              let selected_period =
                match period_opt with
                | Some p when List.mem p periods -> p
                | _ -> (match periods with | p :: _ -> p | [] -> "Q1")
              in
              let events_res =
                match periods with
                | [] -> Ok []
                | _ -> Db.get_pbp_events ~game_id ~period_code:selected_period ()
              in
              match events_res with
              | Ok events -> Kirin.html (Views.pbp_page ~lang ~game ~periods ~selected_period ~events ())
              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    (* AI Game Summary *)
    Kirin.get "/boxscore/:id/summary" (fun request ->
      let game_id = Kirin.param "id" request in
      match Db.get_boxscore ~game_id () with
      | Error e -> Kirin.json_string (Printf.sprintf {|{"error": "%s"}|} (Db.show_db_error e))
      | Ok bs ->
          let game = bs.Domain.boxscore_game in
          let home_players = bs.Domain.boxscore_home_players in
          let away_players = bs.Domain.boxscore_away_players in
          let calc_eff (p : Domain.boxscore_player_stat) =
            float_of_int (p.bs_pts + p.bs_reb + p.bs_ast + p.bs_stl + p.bs_blk
              - (p.bs_fg_att - p.bs_fg_made) - (p.bs_ft_att - p.bs_ft_made) - p.bs_tov)
          in
          let find_mvp players =
            List.fold_left (fun best p ->
              let eff = calc_eff p in
              match best with
              | None -> Some (p, eff)
              | Some (_, best_eff) -> if eff > best_eff then Some (p, eff) else best
            ) None players
          in
          let home_mvp = find_mvp home_players in
          let away_mvp = find_mvp away_players in
          let mvp = match home_mvp, away_mvp with
            | Some (h, h_eff), Some (_, a_eff) when h_eff >= a_eff -> Some (h, h_eff, true)
            | Some _, Some (a, a_eff) -> Some (a, a_eff, false)
            | Some (h, h_eff), None -> Some (h, h_eff, true)
            | None, Some (a, a_eff) -> Some (a, a_eff, false)
            | None, None -> None
          in
          let winner, loser, w_score, l_score =
            if game.gi_home_score > game.gi_away_score then
              (game.gi_home_team_name, game.gi_away_team_name, game.gi_home_score, game.gi_away_score)
            else
              (game.gi_away_team_name, game.gi_home_team_name, game.gi_away_score, game.gi_home_score)
          in
          let margin = abs (game.gi_home_score - game.gi_away_score) in
          let game_desc =
            if margin >= 20 then "압도적인 경기력으로"
            else if margin >= 10 then "안정적인 경기 운영으로"
            else if margin >= 5 then "치열한 접전 끝에"
            else "손에 땀을 쥐게 하는 초접전 끝에"
          in
          let mvp_desc = match mvp with
            | Some (p, _, is_home) ->
                Printf.sprintf "%s(%s)가 %d득점 %d리바운드 %d어시스트로 맹활약했습니다."
                  p.bs_player_name
                  (if is_home then game.gi_home_team_name else game.gi_away_team_name)
                  p.bs_pts p.bs_reb p.bs_ast
            | None -> ""
          in
          let summary = Printf.sprintf
            "%s이(가) %s %d-%d로 %s을(를) 꺾었습니다. %s"
            winner game_desc w_score l_score loser mvp_desc
          in
          let json = `Assoc [
            ("summary", `String summary);
            ("winner", `String winner);
            ("loser", `String loser);
            ("margin", `Int margin);
            ("mvp", match mvp with
              | Some (p, eff, _) -> `Assoc [
                  ("name", `String p.bs_player_name);
                  ("pts", `Int p.bs_pts);
                  ("reb", `Int p.bs_reb);
                  ("ast", `Int p.bs_ast);
                  ("eff", `Float eff);
                ]
              | None -> `Null);
          ] in
          Kirin.json_string (Yojson.Basic.to_string json)
    );

    (* Game Flow Chart *)
    Kirin.get "/boxscore/:id/flow" (fun request ->
      let lang = Route_helpers.request_lang request in
      let game_id = Kirin.param "id" request in
      match Db.get_game_info ~game_id () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok None -> Kirin.html ~status:`Not_found (Views.not_found_page ~lang ~what:"경기" ())
      | Ok (Some game) ->
          let today_kst = Live.today_str () in
          let periods_res =
            match Db.get_pbp_periods ~game_id () with
            | Ok periods as ok ->
                if Domain.pbp_should_backfill ~today_kst game periods then
                  Backfill.Pbp.maybe_spawn ~sw ~env ~game_id ~tag:"FLOW" ();
                ok
            | other -> other
          in

          (match periods_res with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok periods ->
              let fetch_flow_points (ps : string list) =
                let chunks =
                  ps
                  |> List.filter_map (fun period_code ->
                      match Db.get_pbp_events ~game_id ~period_code () with
                      | Ok events -> Some events
                      | Error _ -> None)
                in
                Domain.extract_score_flow (List.concat chunks)
              in

              let flow_points = fetch_flow_points periods in

              let scores_known = game.gi_home_score > 0 && game.gi_away_score > 0 in
              let is_not_future_game =
                let d = String.trim game.gi_game_date in
                d <> "" && String.compare d today_kst <= 0
              in
              if scores_known && is_not_future_game
                 && not (Domain.score_flow_matches_final ~final_home:game.gi_home_score ~final_away:game.gi_away_score flow_points)
              then Backfill.Pbp.maybe_spawn ~sw ~env ~game_id ~tag:"FLOW" ();

              Kirin.html (Views_tools.game_flow_page ~lang ~game flow_points))
    );

    (* Boxscore Detail - MUST come AFTER more specific /boxscore/:id/* routes *)
    Kirin.get "/boxscore/:id" (fun request ->
      let lang = Route_helpers.request_lang request in
      let game_id = Kirin.param "id" request in
      match Db.get_boxscore ~game_id () with
      | Ok bs ->
          let today_kst = Live.today_str () in
          let g = bs.boxscore_game in
          let periods =
            match Db.get_pbp_periods ~game_id () with
            | Ok ps -> ps
            | Error _ -> []
          in
          let scores_known = g.gi_home_score > 0 && g.gi_away_score > 0 in
          let is_not_future_game =
            let d = String.trim g.gi_game_date in
            d <> "" && String.compare d today_kst <= 0
          in
          let q4_mismatch =
            if scores_known && is_not_future_game then (
              match Db.get_quarter_scores game_id with
              | Error _ -> false
              | Ok qs ->
                  match List.find_opt (fun (q : Domain.quarter_score) -> q.qs_period = "Q4") qs with
                  | None -> false
                  | Some q4 ->
                      q4.qs_home_score <> g.gi_home_score || q4.qs_away_score <> g.gi_away_score
            ) else false
          in
          if Domain.pbp_should_backfill ~today_kst g periods || q4_mismatch then
            Backfill.Pbp.maybe_spawn ~sw ~env ~game_id ~tag:"BOX" ();
          Kirin.html (Views.boxscore_page ~lang bs)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );
  ]
