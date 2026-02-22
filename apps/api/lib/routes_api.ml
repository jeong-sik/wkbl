(** API routes: search, live scores, push notifications, admin sync, SSE. *)

open Domain

let routes ~sw ~env =
  ignore (sw, env);
  [
    (* Player Search API for Command Palette *)
    Kirin.get "/api/search/players" (fun request ->
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      if String.length q < 1 then
        Kirin.json_string "[]"
      else begin
        match Db.get_seasons () with
        | Error _ -> Kirin.json_string "[]"
        | Ok seasons ->
            let season = Route_helpers.query_season_or_latest request seasons in
            match Db.get_players ~season ~search:q ~limit:8 () with
            | Error _ -> Kirin.json_string "[]"
            | Ok players ->
                let json_items = List.map (fun p ->
                  Printf.sprintf {|{"id":"%s","name":"%s","team":"%s","pts":%.1f}|}
                    (Views_common.escape_html p.player_id)
                    (Views_common.escape_html p.name)
                    (Views_common.escape_html p.team_name)
                    p.avg_points
                ) players in
                Kirin.json_string (Printf.sprintf "[%s]" (String.concat "," json_items))
      end
    );

    (* Unified Search API: players + teams + seasons *)
    Kirin.get "/api/search" (fun request ->
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      let q_trimmed = String.trim q in
      if String.length q_trimmed < 1 then
        Kirin.json_string {|{"players":[],"teams":[],"seasons":[]}|}
      else begin
        let open Db_common in
        let all_seasons = Db.get_seasons () in
        (* Players *)
        let players_json = match all_seasons with
          | Error _ -> "[]"
          | Ok seasons ->
              let season = Route_helpers.query_season_or_latest request seasons in
              (match Db.get_players ~season ~search:q_trimmed ~limit:5 () with
              | Error _ -> "[]"
              | Ok players ->
                  let items = List.map (fun p ->
                    Printf.sprintf {|{"id":"%s","name":"%s","team":"%s","pts":%.1f}|}
                      (Views_common.escape_html p.player_id)
                      (Views_common.escape_html p.name)
                      (Views_common.escape_html p.team_name)
                      p.avg_points
                  ) players in
                  Printf.sprintf "[%s]" (String.concat "," items))
        in
        (* Teams *)
        let teams_json = match Db.get_all_teams () with
          | Error _ -> "[]"
          | Ok teams ->
              let needle = normalize_search_text q_trimmed in
              let matched = teams
                |> List.filter (fun (t: Domain.team_info) ->
                    string_contains ~needle ~hay:t.team_name ||
                    string_contains ~needle ~hay:t.team_code)
                |> take 5
              in
              let items = List.map (fun (t: Domain.team_info) ->
                Printf.sprintf {|{"code":"%s","name":"%s"}|}
                  (Views_common.escape_html t.team_code)
                  (Views_common.escape_html t.team_name)
              ) matched in
              Printf.sprintf "[%s]" (String.concat "," items)
        in
        (* Seasons *)
        let seasons_json = match all_seasons with
          | Error _ -> "[]"
          | Ok seasons ->
              let needle = normalize_search_text q_trimmed in
              let matched = seasons
                |> List.filter (fun (s: Domain.season_info) ->
                    string_contains ~needle ~hay:s.name ||
                    string_contains ~needle ~hay:s.code)
                |> take 5
              in
              let items = List.map (fun (s: Domain.season_info) ->
                Printf.sprintf {|{"code":"%s","name":"%s"}|}
                  (Views_common.escape_html s.code)
                  (Views_common.escape_html s.name)
              ) matched in
              Printf.sprintf "[%s]" (String.concat "," items)
        in
        Kirin.json_string (Printf.sprintf {|{"players":%s,"teams":%s,"seasons":%s}|}
          players_json teams_json seasons_json)
      end
    );

    (* Live Scores API: JSON *)
    Kirin.get "/api/live/status" (fun _ ->
      let current = Live.get_current_games () in
      let games =
        if current <> [] then current
        else
          let today = Live.today_str () in
          match Db.get_seasons () with
          | Error _ -> []
          | Ok seasons ->
              let live_season = Request_params.latest_season_code seasons in
              (match Db.get_games ~season:live_season ~page_size:400 () with
              | Error _ -> []
              | Ok gs ->
                  gs
                  |> List.filter (fun (g: Domain.game_summary) ->
                      String.length g.game_date >= 10 && String.sub g.game_date 0 10 = today)
                  |> List.map (fun (g: Domain.game_summary) ->
                      let has_score =
                        match g.home_score, g.away_score with
                        | Some a, Some b -> not (a = 0 && b = 0)
                        | _ -> false
                      in
                      let quarter = if has_score then "경기종료" else "경기전" in
                      {
                        Domain.lg_game_id = g.game_id;
                        lg_home_team = g.home_team;
                        lg_away_team = g.away_team;
                        lg_home_score = Option.value ~default:0 g.home_score;
                        lg_away_score = Option.value ~default:0 g.away_score;
                        lg_quarter = quarter;
                        lg_time_remaining = "";
                        lg_is_live = false;
                      }))
      in
      Kirin.with_header "Cache-Control" "no-cache"
      @@ Kirin.json_string (Live.status_json_of_games games)
    );

    (* Live Scores API: SSE *)
    Kirin.get "/api/live/sse" (fun request ->
      Live.sse_handler request
    );

    (* Live Scores Widget: HTMX partial *)
    Kirin.get "/api/live/widget" (fun request ->
      let lang = Route_helpers.request_lang request in
      let current = Live.get_current_games () in
      let games =
        if current <> [] then current
        else
          let today = Live.today_str () in
          match Db.get_seasons () with
          | Error _ -> []
          | Ok seasons ->
              let live_season = Request_params.latest_season_code seasons in
              (match Db.get_games ~season:live_season ~page_size:400 () with
              | Error _ -> []
              | Ok gs ->
                  gs
                  |> List.filter (fun (g: Domain.game_summary) ->
                      String.length g.game_date >= 10 && String.sub g.game_date 0 10 = today)
                  |> List.map (fun (g: Domain.game_summary) ->
                      let has_score =
                        match g.home_score, g.away_score with
                        | Some a, Some b -> not (a = 0 && b = 0)
                        | _ -> false
                      in
                      let quarter = if has_score then "경기종료" else "경기전" in
                      {
                        Domain.lg_game_id = g.game_id;
                        lg_home_team = g.home_team;
                        lg_away_team = g.away_team;
                        lg_home_score = Option.value ~default:0 g.home_score;
                        lg_away_score = Option.value ~default:0 g.away_score;
                        lg_quarter = quarter;
                        lg_time_remaining = "";
                        lg_is_live = false;
                      }))
      in
      Kirin.with_header "Cache-Control" "no-cache"
      @@ Kirin.html (Views.live_scores_widget ~lang games)
    );

    (* Push Notification: Subscribe (MVP - just log) *)
    Kirin.post "/api/push/subscribe" (fun request ->
      let body = Kirin.Request.body request in
      Printf.printf "[Push] Subscribe: %s\n%!" body;
      Kirin.json_string {|{"status":"ok","message":"subscribed"}|}
    );

    (* Push Notification: Unsubscribe (MVP - just log) *)
    Kirin.post "/api/push/unsubscribe" (fun _ ->
      Printf.printf "[Push] Unsubscribe\n%!";
      Kirin.json_string {|{"status":"ok","message":"unsubscribed"}|}
    );

    (* Push Notification: Test (send test notification) *)
    Kirin.post "/api/push/test" (fun _ ->
      Printf.printf "[Push] Test notification requested\n%!";
      Kirin.json_string {|{"status":"ok","message":"test notification sent"}|}
    );

    (* Admin: Sync Awards from WKBL website to DB *)
    Kirin.post "/api/admin/sync-awards" (fun request ->
      if not (Admin.is_admin request) then
        Kirin.json_string ~status:`Forbidden {|{"error":"unauthorized"}|}
      else begin
        Printf.printf "[Admin] Starting awards sync...\n%!";
        let (sc, b5c, vc) = Scraper.sync_awards_to_db ~sw ~env () in
        let total = sc + b5c + vc in
        Kirin.json_string (Printf.sprintf
          {|{"status":"ok","stat":%d,"best5":%d,"vote":%d,"total":%d}|}
          sc b5c vc total)
      end
    );

    (* SSE: Live scores endpoint *)
    Kirin.get "/api/live/scores" (fun _request ->
      let today = Live.today_str () in
      let season =
        match Db.get_seasons () with
        | Ok seasons -> Request_params.latest_season_code seasons
        | Error _ -> Seasons_catalog.current_regular_season_code ()
      in
      match Db.get_games ~season ~page_size:400 () with
      | Ok games ->
          let today_games = List.filter (fun (g: Domain.game_summary) ->
            String.length g.game_date >= 10 && String.sub g.game_date 0 10 = today
          ) games in
          let game_to_json (g: Domain.game_summary) =
            let home_score_json, away_score_json, status =
              match g.home_score, g.away_score with
              | Some a, Some b when not (a = 0 && b = 0) -> (`Int a, `Int b, "final")
              | _ -> (`Null, `Null, "scheduled")
            in
            `Assoc [
              ("game_id", `String g.game_id);
              ("home", `String g.home_team);
              ("away", `String g.away_team);
              ("home_score", home_score_json);
              ("away_score", away_score_json);
              ("status", `String status);
            ]
          in
          let json_obj = `Assoc [
            ("timestamp", `String (string_of_float (Unix.time ())));
            ("date", `String today);
            ("games", `List (List.map game_to_json today_games));
          ] in
          let event = Kirin.Sse.event "scores" (Yojson.Basic.to_string json_obj) in
          Kirin.Sse.response_legacy [event]
      | Error e ->
          let err_event = Kirin.Sse.event "error" (Db.show_db_error e) in
          Kirin.Sse.response_legacy [err_event]
    );

    (* Live scores page *)
    Kirin.get "/live" (fun request ->
      let lang = Route_helpers.request_lang request in
      Kirin.html (Views.live_page ~lang ())
    );

    (* GraphQL API endpoint *)
    Kirin.post "/graphql" (fun request ->
      let body = Kirin.Request.body request in
      let respond_json str =
        Kirin.json_string str
        |> Kirin.with_header "Access-Control-Allow-Origin" "*"
      in
      match Yojson.Basic.from_string body with
      | exception Yojson.Json_error msg ->
          respond_json
            (Printf.sprintf {|{"errors":[{"message":"Invalid JSON: %s"}]}|}
              (String.escaped msg))
      | json ->
          let query = match json with
            | `Assoc fields ->
                (match List.assoc_opt "query" fields with
                 | Some (`String q) -> Some q
                 | _ -> None)
            | _ -> None
          in
          let operation_name = match json with
            | `Assoc fields ->
                (match List.assoc_opt "operationName" fields with
                 | Some (`String n) -> Some n
                 | Some `Null -> None
                 | _ -> None)
            | _ -> None
          in
          let variables = match json with
            | `Assoc fields ->
                (match List.assoc_opt "variables" fields with
                 | Some (`Assoc vars) ->
                     let rec yojson_to_const : Yojson.Basic.t -> Graphql_parser.const_value = function
                       | `Null -> `Null
                       | `Bool b -> `Bool b
                       | `Int i -> `Int i
                       | `Float f -> `Float f
                       | `String s -> `String s
                       | `List l -> `List (List.map yojson_to_const l)
                       | `Assoc a -> `Assoc (List.map (fun (k, v) -> (k, yojson_to_const v)) a)
                     in
                     Some (List.map (fun (k, v) -> (k, yojson_to_const v)) vars)
                 | _ -> None)
            | _ -> None
          in
          match query with
          | None ->
              respond_json {|{"errors":[{"message":"Missing 'query' field"}]}|}
          | Some query_str ->
              match Graphql_schema.execute_query ?variables ?operation_name query_str with
              | Ok result ->
                  respond_json (Yojson.Basic.to_string result)
              | Error err ->
                  respond_json
                    (Printf.sprintf {|{"errors":[{"message":%s}]}|}
                      (Yojson.Basic.to_string err))
    );

    (* GraphQL CORS preflight *)
    Kirin.options "/graphql" (fun _request ->
      Kirin.text ""
      |> Kirin.with_header "Access-Control-Allow-Origin" "*"
      |> Kirin.with_header "Access-Control-Allow-Methods" "POST, OPTIONS"
      |> Kirin.with_header "Access-Control-Allow-Headers" "Content-Type"
      |> Kirin.with_header "Access-Control-Max-Age" "86400"
    );
  ]
