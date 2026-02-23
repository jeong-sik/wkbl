(** History routes: historical overview, season summary, legends, coaches. *)

let routes =
  [
    Kirin.get "/history" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_historical_seasons () with
      | Ok seasons -> Kirin.html (Views_history.history_page ~lang seasons)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Season summary page - 6 queries run in parallel via Eio.Fiber.all *)
    Kirin.get "/season/:code" (fun request ->
      let lang = Route_helpers.request_lang request in
      let season = Kirin.param "code" request |> Uri.pct_decode in
      let r_seasons = ref (Error (Db.ConnectionFailed "not computed")) in
      let r_standings = ref (Error (Db.ConnectionFailed "not computed")) in
      let r_leaders = ref (Error (Db.ConnectionFailed "not computed")) in
      let r_histories = ref (Error (Db.ConnectionFailed "not computed")) in
      let r_tpg = ref (Error (Db.ConnectionFailed "not computed")) in
      let r_ttot = ref (Error (Db.ConnectionFailed "not computed")) in
      Eio.Fiber.all [
        (fun () -> r_seasons := Db.get_seasons ());
        (fun () -> r_standings := Db.get_standings ~season ());
        (fun () -> r_leaders := Db.get_leaders_base ~season ());
        (fun () -> r_histories := Db.get_historical_seasons ());
        (fun () -> r_tpg := Db.get_team_stats ~season ~scope:Domain.PerGame ());
        (fun () -> r_ttot := Db.get_team_stats ~season ~scope:Domain.Totals ());
      ];
      match !r_seasons, !r_standings, !r_leaders, !r_histories, !r_tpg, !r_ttot with
      | Ok seasons, Ok standings, Ok leaders, Ok histories, Ok tpg, Ok ttot ->
          Kirin.html (Views_season.season_summary_page ~lang ~season ~seasons
            ~standings ~leaders ~histories ~team_per_game:tpg ~team_totals:ttot ())
      | Error e, _, _, _, _, _ | _, Error e, _, _, _, _
      | _, _, Error e, _, _, _ | _, _, _, Error e, _, _
      | _, _, _, _, Error e, _ | _, _, _, _, _, Error e ->
          Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    Kirin.get "/legends" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_legend_players () with
      | Ok legends -> Kirin.html (Views_history.legends_page ~lang legends)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    Kirin.get "/coaches" (fun request ->
      let lang = Route_helpers.request_lang request in
      match Db.get_coaches () with
      | Ok coaches -> Kirin.html (Views_history.coaches_page ~lang coaches)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );
  ]
