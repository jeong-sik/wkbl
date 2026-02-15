(** QA routes: dashboard, data quality reports, admin login, anomaly management. *)

let routes =
  [
    (* QA Dashboard *)
    Kirin.get "/qa" (fun request ->
      let lang = Route_helpers.request_lang request in
      let markdown = Qa.read_markdown_if_exists () in
      match Db.get_db_quality_report () with
      | Ok report -> Kirin.html (Views_tools.qa_dashboard_page report ~lang ~markdown ())
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    Kirin.get "/qa/pbp-missing" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          (match Db.get_pbp_missing_report ~season () with
          | Ok report -> Kirin.html (Views_tools.qa_pbp_missing_page report ~lang ~season ~seasons ())
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    Kirin.get "/qa/schedule-missing" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_schedule_missing_report () with
      | Ok report -> Kirin.html (Views_tools.qa_schedule_missing_page report ~lang ())
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* QA Admin Login: /qa/admin?token=...&next=/qa/anomalies *)
    Kirin.get "/qa/admin" (fun request ->
      match Admin.token_env with
      | None -> Kirin.empty `Not_found
      | Some expected ->
          let token = Kirin.query_opt "token" request |> Option.value ~default:"" |> String.trim in
          if token <> expected then
            Kirin.empty `Not_found
          else
            let next =
              Kirin.query_opt "next" request
              |> Option.value ~default:"/qa/anomalies"
              |> String.trim
            in
            let next = if Route_helpers.is_safe_redirect_path next then next else "/qa/anomalies" in
            let secure =
              match Kirin.header "X-Forwarded-Proto" request with
              | Some p -> String.lowercase_ascii (String.trim p) = "https"
              | None -> false
            in
            Kirin.with_header "Set-Cookie" (Admin.cookie_header ~secure expected)
            @@ Kirin.redirect next
    );

    (* QA Overrides: exclude/restore obviously wrong rows *)
    Kirin.get "/qa/anomalies" (fun request ->
      if not (Admin.is_admin request) then
        Kirin.empty `Not_found
      else
        let lang = Route_helpers.request_lang request in
        match Db.get_seasons () with
        | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
        | Ok seasons ->
            let season = Route_helpers.query_season_or_latest request seasons in
            (match Db.get_stat_anomaly_candidates ~season (), Db.get_stat_exclusions ~season () with
            | Ok candidates, Ok exclusions ->
                Kirin.html (Views_tools.qa_anomalies_page ~lang ~season ~seasons ~candidates ~exclusions ())
            | Error e, _ -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
            | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    Kirin.post "/qa/anomalies/exclude" (fun request ->
      if not (Admin.is_admin request) then
        Kirin.empty `Not_found
      else
        let lang = Route_helpers.request_lang request in
        let fields = Form_urlencoded.parse (Kirin.Request.body request) in
        let season = Form_urlencoded.find fields "season" |> Option.value ~default:"ALL" |> String.trim in
        let game_id = Form_urlencoded.find fields "game_id" |> Option.value ~default:"" |> String.trim in
        let player_id = Form_urlencoded.find fields "player_id" |> Option.value ~default:"" |> String.trim in
        let reason = Form_urlencoded.find fields "reason" |> Option.value ~default:"" |> String.trim in
        if game_id = "" || player_id = "" then
          Kirin.html (Views.error_page ~lang "요청 값이 비어있습니다.")
        else
          (match Db.upsert_game_stats_exclusion ~game_id ~player_id ~reason () with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok () -> (
              match Db.refresh_matviews () with
              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
              | Ok () ->
                  Db.clear_all_caches ();
                  Kirin.redirect (Printf.sprintf "/qa/anomalies?season=%s" (Uri.pct_encode season))
            ))
    );

    Kirin.post "/qa/anomalies/restore" (fun request ->
      if not (Admin.is_admin request) then
        Kirin.empty `Not_found
      else
        let lang = Route_helpers.request_lang request in
        let fields = Form_urlencoded.parse (Kirin.Request.body request) in
        let season = Form_urlencoded.find fields "season" |> Option.value ~default:"ALL" |> String.trim in
        let game_id = Form_urlencoded.find fields "game_id" |> Option.value ~default:"" |> String.trim in
        let player_id = Form_urlencoded.find fields "player_id" |> Option.value ~default:"" |> String.trim in
        if game_id = "" || player_id = "" then
          Kirin.html (Views.error_page ~lang "요청 값이 비어있습니다.")
        else
          (match Db.delete_game_stats_exclusion ~game_id ~player_id () with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok () -> (
              match Db.refresh_matviews () with
              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
              | Ok () ->
                  Db.clear_all_caches ();
                  Kirin.redirect (Printf.sprintf "/qa/anomalies?season=%s" (Uri.pct_encode season))
            ))
    );

    (* PBP Data Quality API *)
    Kirin.get "/qa/pbp" (fun request ->
      match Db.get_pbp_data_quality () with
      | Ok pq ->
          let verified_pct = if pq.pq_total_pbp_games > 0 then
            100.0 *. (float_of_int pq.pq_t2_home_count /. float_of_int pq.pq_total_pbp_games)
          else 0.0 in
          let threshold = match Kirin.query_opt "threshold" request with
            | Some s -> (try float_of_string s with Failure _ -> 50.0)
            | None -> 50.0
          in
          let ci_mode = Kirin.query_opt "ci" request |> Option.is_some in
          let pass = verified_pct >= threshold in
          let health = if pass then "healthy" else "critical" in
          let json = Printf.sprintf
            {|{"status":"%s","t2_home_verified":%d,"incomplete":%d,"no_match":%d,"total_pbp_games":%d,"pattern":"T2=HOME (team2_score = home_score)","verified_pct":%.1f,"threshold":%.1f,"ci_mode":%b}|}
            health
            pq.pq_t2_home_count
            pq.pq_incomplete_count
            pq.pq_no_match_count
            pq.pq_total_pbp_games
            verified_pct
            threshold
            ci_mode
          in
          if ci_mode && not pass then
            Kirin.json_string (Printf.sprintf {|{"error":"PBP quality check failed","verified_pct":%.1f,"threshold":%.1f,"details":%s}|} verified_pct threshold json)
          else
            Kirin.json_string json
      | Error e -> Kirin.json_string (Printf.sprintf {|{"error":"%s"}|} (Db.show_db_error e))
    );
  ]
