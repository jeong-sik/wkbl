(** Home page routes: landing page, HTMX table, HEAD, index redirect, language toggle. *)

let routes =
  [
    (* Home Page *)
    Kirin.get "/" (fun request ->
      let lang = Route_helpers.request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~limit:20 () with
          | Ok p ->
              let player_info_map = Backfill.get_player_info_map () in
              let live_games =
                let current = Live.get_current_games () in
                if current <> [] then current
                else
                  let today = Live.today_str () in
                  let live_season = Request_params.latest_season_code seasons in
                  match Db.get_games ~season:live_season ~page_size:400 () with
                  | Error _ -> []
                  | Ok games ->
                      games
                      |> List.filter (fun (g : Domain.game_summary) ->
                          String.length g.game_date >= 10 && String.sub g.game_date 0 10 = today)
                      |> List.map (fun (g : Domain.game_summary) ->
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
                          })
              in
              let tr = I18n.t lang in
              let data_as_of =
                match Db.get_latest_game_date () with
                | Ok (Some d) -> d
                | Ok None -> tr { ko = "기록 없음"; en = "No record" }
                | Error _ -> tr { ko = "확인 불가"; en = "Unavailable" }
              in
              Kirin.html (Views.home_page ~lang ~player_info_map ~live_games ~season ~seasons ~data_as_of p)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Home Page Table HTMX *)
    Kirin.get "/home/table" (fun request ->
      let lang = Route_helpers.request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = Route_helpers.query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~limit:20 () with
          | Ok p ->
              let player_info_map = Backfill.get_player_info_map () in
              Kirin.html (Views.players_table ~lang ~player_info_map p)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Handle HEAD requests *)
    Kirin.head "/" (fun _ -> Kirin.empty `OK);

    (* Redirect index.html *)
    Kirin.get "/index.html" (fun _ -> Kirin.redirect "/");

    (* Language toggle: set cookie and redirect back *)
    Kirin.get "/lang/:code" (fun request ->
      let code = Kirin.param "code" request in
      let lang = I18n.lang_of_code code |> Option.value ~default:I18n.Ko in
      let next = Admin.redirect_back_or_home request in
      Kirin.with_header "Set-Cookie" (I18n.set_cookie_header lang)
      @@ Kirin.redirect next
    );
  ]
