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
      let p_seasons, res_seasons = Eio.Promise.create () in
      let p_standings, res_standings = Eio.Promise.create () in
      let p_leaders, res_leaders = Eio.Promise.create () in
      let p_histories, res_histories = Eio.Promise.create () in
      let p_tpg, res_tpg = Eio.Promise.create () in
      let p_ttot, res_ttot = Eio.Promise.create () in
      Eio.Fiber.all [
        (fun () -> Eio.Promise.resolve res_seasons (Db.get_seasons ()));
        (fun () -> Eio.Promise.resolve res_standings (Db.get_standings ~season ()));
        (fun () -> Eio.Promise.resolve res_leaders (Db.get_leaders_base ~season ()));
        (fun () -> Eio.Promise.resolve res_histories (Db.get_historical_seasons ()));
        (fun () -> Eio.Promise.resolve res_tpg (Db.get_team_stats ~season ~scope:Domain.PerGame ()));
        (fun () -> Eio.Promise.resolve res_ttot (Db.get_team_stats ~season ~scope:Domain.Totals ()));
      ];
      match Eio.Promise.await p_seasons, Eio.Promise.await p_standings, Eio.Promise.await p_leaders, Eio.Promise.await p_histories, Eio.Promise.await p_tpg, Eio.Promise.await p_ttot with
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
