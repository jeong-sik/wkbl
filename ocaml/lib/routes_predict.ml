(** Predict routes: team vs team prediction page + API + OG image. *)

open Domain

let routes =
  [
    (* Predict (Team vs Team) *)
    Kirin.get "/predict" (fun request ->
      let lang = Route_helpers.request_lang request in
      let home = Kirin.query_opt "home" request |> Option.value ~default:"" in
      let away = Kirin.query_opt "away" request |> Option.value ~default:"" in
      let include_mismatch = Route_helpers.query_bool request "include_mismatch" in
      let context_enabled =
        Kirin.query_opt "context" request
        |> Option.map String.lowercase_ascii
        |> function
        | Some ("1" | "true" | "yes" | "on") -> true
        | _ -> false
      in
      let is_neutral =
        Kirin.query_opt "neutral" request
        |> Option.map String.lowercase_ascii
        |> function
        | Some ("1" | "true" | "yes" | "on") -> true
        | _ -> false
      in

      match Db.get_seasons (), Db.get_all_teams () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let upcoming = match Db.get_upcoming_schedule ~status:"scheduled" ~limit:6 () with Ok u -> u | Error _ -> [] in
          let team_names = List.map (fun (t: team_info) -> t.team_name) teams in
          let season = Route_helpers.query_season_or_latest request seasons in
          let render result error =
            Kirin.html (Views.predict_page ~lang ~season ~seasons ~teams:team_names ~home ~away ~is_neutral ~context_enabled ~include_mismatch ~upcoming ~games:(match Db.get_scored_games ~season ~include_mismatch () with Ok g -> g | _ -> []) result error)
          in

          if String.trim home = "" || String.trim away = "" then
            render None None
          else if normalize_label home = normalize_label away then
            render None (Some "홈 팀과 원정 팀은 달라야 합니다.")
          else
            match Db.get_team_stats ~season ~scope:Totals ~include_mismatch (), Db.get_scored_games ~season ~include_mismatch () with
            | Ok totals, Ok games ->
                let find_team (name : string) =
                  let key = normalize_label name in
                  let key_without_city =
                    if String.starts_with ~prefix:"아산" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"청주" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"인천" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"용인" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"부천" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"부산" key then String.sub key 6 (String.length key - 6)
                    else key
                  in
                  match List.find_opt (fun (row : team_stats) -> normalize_label row.team = key) totals with
                  | Some t -> Some t
                  | None ->
                      List.find_opt (fun (row : team_stats) ->
                        let row_key = normalize_label row.team in
                        row_key = key_without_city
                      ) totals
                in
                let win_pct_of_team (name : string) =
                  let key = normalize_label name in
                  let wins, losses =
                    games
                    |> List.fold_left
                      (fun (w, l) (g : game_summary) ->
                        if normalize_label g.home_team <> key && normalize_label g.away_team <> key then
                          (w, l)
                        else
                          match g.home_score, g.away_score with
                          | Some hs, Some as_ ->
                              if normalize_label g.home_team = key then
                                if hs > as_ then (w + 1, l) else if hs < as_ then (w, l + 1) else (w, l)
                              else if normalize_label g.away_team = key then
                                if as_ > hs then (w + 1, l) else if as_ < hs then (w, l + 1) else (w, l)
                              else
                                (w, l)
                          | _ -> (w, l))
                      (0, 0)
                  in
                  let total = wins + losses in
                  if total <= 0 then 0.5 else (float_of_int wins /. float_of_int total)
                in
                (match find_team home, find_team away with
                | Some home_row, Some away_row ->
                    let output =
                      Prediction.predict_match_nerd
                        ~context:None
                        ~season
                        ~is_neutral
                        ~games
                        ~home:home_row
                        ~away:away_row
                        ~home_win_pct:(win_pct_of_team home)
                        ~away_win_pct:(win_pct_of_team away)
                        ~name_home:home
                        ~name_away:away
                    in
                    render (Some output) None
                | None, _ -> render None (Some (Printf.sprintf "Team not found: %s" home))
                | _, None -> render None (Some (Printf.sprintf "Team not found: %s" away)))
            | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* AI Prediction API: Get match prediction with explanation *)
    Kirin.get "/api/predict" (fun request ->
      let home = Kirin.query_opt "home" request |> Option.value ~default:"" in
      let away = Kirin.query_opt "away" request |> Option.value ~default:"" in
      if home = "" || away = "" then
        Kirin.with_status `Bad_request
        @@ Kirin.json_string {|{"error":"home and away parameters required"}|}
      else
        let season =
          match Db.get_seasons () with
          | Ok seasons -> Route_helpers.query_season_or_latest request seasons
          | Error _ ->
              Route_helpers.query_nonempty request "season"
              |> Option.value ~default:(Seasons_catalog.current_regular_season_code ())
        in
        match Db.get_team_stats ~season (), Db.get_standings ~season (), Db.get_games ~season () with
        | Ok teams, Ok standings, Ok games ->
            let find_stats name = List.find_opt (fun (t: team_stats) -> t.team = name) teams in
            let find_standing name = List.find_opt (fun (s: team_standing) -> s.team_name = name) standings in
            (match find_stats home, find_stats away, find_standing home, find_standing away with
            | Some home_stats, Some away_stats, Some home_st, Some away_st ->
                let output = Prediction.predict_match_nerd
                  ~context:None ~season ~is_neutral:false ~games
                  ~home:home_stats ~away:away_stats
                  ~home_win_pct:home_st.win_pct ~away_win_pct:away_st.win_pct
                  ~name_home:home ~name_away:away
                in
                let explanation = Ai.get_explanation ~home ~away output in
                let r = output.result in
                let json_obj = `Assoc [
                  ("winner", `String r.winner);
                  ("probability", `Float (r.prob_a *. 100.0));
                  ("explanation", `String explanation);
                  ("breakdown", `Assoc [
                    ("elo_home", `Float output.breakdown.pb_elo_home);
                    ("elo_away", `Float output.breakdown.pb_elo_away);
                    ("elo_prob", `Float (output.breakdown.pb_elo_prob *. 100.0));
                  ]);
                ] in
                let json = Yojson.Basic.to_string json_obj in
                Kirin.with_header "Cache-Control" "public, max-age=300, s-maxage=900"
                @@ Kirin.json_string json
            | None, _, _, _ | _, None, _, _ ->
                Kirin.with_status `Not_found
                @@ Kirin.json_string {|{"error":"Team stats not found"}|}
            | _, _, None, _ | _, _, _, None ->
                Kirin.with_status `Not_found
                @@ Kirin.json_string {|{"error":"Team standings not found"}|})
        | Error e, _, _ | _, Error e, _ | _, _, Error e ->
            Kirin.server_error ~body:(Db.show_db_error e) ()
    );

    (* AI Prediction OG Image *)
    Kirin.get "/api/og/predict" (fun request ->
      let home = Kirin.query_opt "home" request |> Option.value ~default:"" in
      let away = Kirin.query_opt "away" request |> Option.value ~default:"" in
      let season =
        match Db.get_seasons () with
        | Ok seasons -> Route_helpers.query_season_or_latest request seasons
        | Error _ ->
            Route_helpers.query_nonempty request "season"
            |> Option.value ~default:(Seasons_catalog.current_regular_season_code ())
      in
      match Db.get_team_stats ~season (), Db.get_standings ~season (), Db.get_games ~season () with
      | Ok teams, Ok standings, Ok games ->
          let find_stats name = List.find_opt (fun (t: team_stats) -> t.team = name) teams in
          let find_standing name = List.find_opt (fun (s: team_standing) -> s.team_name = name) standings in
          (match find_stats home, find_stats away, find_standing home, find_standing away with
          | Some home_stats, Some away_stats, Some home_st, Some away_st ->
              let output = Prediction.predict_match_nerd
                ~context:None ~season ~is_neutral:false ~games
                ~home:home_stats ~away:away_stats
                ~home_win_pct:home_st.win_pct ~away_win_pct:away_st.win_pct
                ~name_home:home ~name_away:away
              in
              let svg = Cards.prediction_card ~home ~away output in
              (match Cards.svg_to_png svg with
              | Some png ->
                  Kirin.with_header "Content-Type" "image/png"
                  @@ Kirin.with_header "Cache-Control" "public, max-age=3600, s-maxage=86400"
                  @@ Kirin.Response.make ~status:`OK (`String png)
              | None -> Kirin.with_header "Content-Type" "image/svg+xml" @@ Kirin.text svg)
          | _ -> Kirin.not_found ~body:"Team data not found" ())
      | _ -> Kirin.server_error ~body:"Failed to generate image" ()
    );
  ]
