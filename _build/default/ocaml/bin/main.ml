(** WKBL Analytics Main Entry Point
    OCaml Edition using Dream framework
*)

open Wkbl
open Wkbl.Domain

let has_prefix ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let query_bool request name =
  Dream.query request name
  |> Option.map (fun v ->
      match String.lowercase_ascii (String.trim v) with
      | "1" | "true" | "yes" | "on" -> true
      | _ -> false)
  |> Option.value ~default:false

let query_nonempty request name =
  match Dream.query request name with
  | None -> None
  | Some v ->
      let trimmed = String.trim v in
      if trimmed = "" then None else Some trimmed

let latest_season_code (seasons : season_info list) =
  match List.rev seasons with
  | s :: _ -> s.code
  | [] -> "ALL"

let query_season_or_latest request (seasons : season_info list) =
  query_nonempty request "season"
  |> Option.value ~default:(latest_season_code seasons)

let rec find_static_path start_dir =
  let has_styles path = Sys.file_exists (Filename.concat path "css/styles.css") in
  let candidates = [ Filename.concat start_dir "static"; Filename.concat start_dir "ocaml/static" ] in
  match List.find_opt has_styles candidates with
  | Some p -> Some p
  | None ->
      let parent = Filename.dirname start_dir in
      if parent = start_dir then None else find_static_path parent

let () =
  (* Resolve runtime config from env. Prioritize WKBL_DATABASE_URL or DATABASE_URL for Postgres/Supabase. *)
  let db_url =
    match Sys.getenv_opt "WKBL_DATABASE_URL" with
    | Some url -> url
    | None -> (
        match Sys.getenv_opt "DATABASE_URL" with
        | Some url -> url
        | None ->
            let path = Sys.getenv_opt "WKBL_DB_PATH" |> Option.value ~default:Db.default_db_path in
            if has_prefix ~prefix:"postgresql://" path || has_prefix ~prefix:"postgres://" path || has_prefix ~prefix:"sqlite3://" path then
              path
            else
              "sqlite3:" ^ path)
  in
  let port =
    match Sys.getenv_opt "PORT" with
    | None -> 8000
    | Some value -> (
        match int_of_string_opt (String.trim value) with
        | Some p when p > 0 -> p
        | _ -> 8000)
  in

  (* Initialize database pool *)
  match Db.init_pool db_url with
  | Error e ->
      Printf.eprintf "Failed to init DB (%s): %s\n" db_url (Db.show_db_error e);
      exit 1
  | Ok () ->

  (* Ensure optional analytics tables exist (e.g., player +/-). *)
  (match Lwt_main.run (Db.ensure_schema ()) with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "Failed to ensure DB schema: %s\n" (Db.show_db_error e);
      exit 1);
  
  (* Determine static path robustly (repo-relative discovery + env override). *)
  let static_path =
    match Sys.getenv_opt "WKBL_STATIC_PATH" with
    | Some p when Sys.file_exists (Filename.concat p "css/styles.css") -> p
    | Some _ ->
        (* Fall back to repo-relative discovery if env var is misconfigured. *)
        let cwd = Sys.getcwd () in
        Option.value (find_static_path cwd) ~default:"static"
    | None ->
        let cwd = Sys.getcwd () in
        Option.value (find_static_path cwd) ~default:"static"
  in
  (* Keep WKBL_STATIC_PATH consistent for view-layer asset checks. *)
  Unix.putenv "WKBL_STATIC_PATH" static_path;
  Printf.printf "Serving static assets from: %s\n%%!" static_path;
  Printf.printf "Using DB path: %s\n%%!" db_url;
  Printf.printf "Listening on: 0.0.0.0:%d\n%%!" port;

  Dream.run ~interface:"0.0.0.0" ~port ~error_handler:Dream.debug_error_handler
  @@ Dream.logger
  (* Force UTF-8 for HTML without breaking static assets. *)
  @@ (fun next_handler request ->
      let open Lwt.Syntax in
      let* response = next_handler request in
      (match Dream.header response "Content-Type" with
      | Some ct when has_prefix ~prefix:"text/html" (String.lowercase_ascii ct) ->
          Dream.set_header response "Content-Type" "text/html; charset=utf-8"
      | _ -> ());
      Lwt.return response)
  @@ Dream.router [

    (* Static Assets - Correctly scoped *)
    Dream.scope "/static" [] [
      Dream.get "**" (fun request ->
        let open Lwt.Syntax in
        let* response = Dream.static static_path request in
        Dream.set_header response "Cache-Control" "public, max-age=31536000, immutable";
        Lwt.return response)
    ];

    (* Home Page *)
    Dream.get "/" (fun _ ->
      let open Lwt.Syntax in
      let* players_res = Db.get_players ~limit:20 () in
      match players_res with
      | Ok p -> Dream.html (Views.home_page p)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );
    
    (* Handle HEAD requests *)
    Dream.head "/" (fun _ -> Dream.empty `OK);

    (* Redirect index.html *)
    Dream.get "/index.html" (fun request -> Dream.redirect request "/");

    (* Players List & Table HTMX *)
    Dream.get "/players" (fun request ->
      let open Lwt.Syntax in
      let search = Dream.query request "search" |> Option.value ~default:"" in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* players_res = Db.get_players ~season ~search ~sort ~include_mismatch () in
          match players_res with
          | Ok p -> Dream.html (Views.players_page ~season ~seasons ~search ~sort:sort_str ~include_mismatch p)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    Dream.get "/players/table" (fun request ->
      let open Lwt.Syntax in
      let search = Dream.query request "search" |> Option.value ~default:"" in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* players_res = Db.get_players ~season ~search ~sort ~include_mismatch () in
          match players_res with
          | Ok p -> Dream.html (Views.players_table p)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Teams Page & Table HTMX *)
    Dream.get "/teams" (fun request ->
      let open Lwt.Syntax in
      let scope_str = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* stats_res = Db.get_team_stats ~season ~scope ~sort ~include_mismatch () in
          (match stats_res with
          | Ok stats -> Dream.html (Views.teams_page ~season ~seasons ~scope ~sort:sort_str ~include_mismatch stats)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    Dream.get "/teams/table" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let scope_str = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      let* stats_res = Db.get_team_stats ~season ~scope ~sort ~include_mismatch () in
      match stats_res with
      | Ok stats -> Dream.html (Views.teams_table ~season ~scope stats)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Standings *)
    Dream.get "/standings" (fun request ->
      let open Lwt.Syntax in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* standings_res = Db.get_standings ~season () in
          (match standings_res with
          | Ok standings -> Dream.html (Views.standings_page ~season ~seasons standings)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    Dream.get "/standings/table" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let* standings_res = Db.get_standings ~season () in
      match standings_res with
      | Ok standings -> Dream.html (Views.standings_table ~season standings)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Games & Boxscores List *)
    Dream.get "/games" (fun request ->
      let open Lwt.Syntax in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* games_res = Db.get_games ~season () in
          (match games_res with
          | Ok games -> Dream.html (Views.games_page ~season ~seasons games)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    Dream.get "/games/table" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let* games_res = Db.get_games ~season () in
      match games_res with
      | Ok games -> Dream.html (Views.games_table games)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Boxscores List *)
    Dream.get "/boxscores" (fun request ->
      let open Lwt.Syntax in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* games_res = Db.get_games ~season () in
          (match games_res with
          | Ok games -> Dream.html (Views.boxscores_page ~season ~seasons games)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    Dream.get "/boxscores/table" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let* games_res = Db.get_games ~season () in
      match games_res with
      | Ok games -> Dream.html (Views.boxscores_table games)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Boxscore Detail *)
    Dream.get "/boxscore/:id" (fun request ->
      let game_id = Dream.param request "id" in
      let open Lwt.Syntax in
      let* boxscore_res = Db.get_boxscore ~game_id () in
      match boxscore_res with
      | Ok bs -> Dream.html (Views.boxscore_page bs)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Play-by-Play (PBP) Detail *)
    Dream.get "/boxscore/:id/pbp" (fun request ->
      let game_id = Dream.param request "id" in
      let period_opt = query_nonempty request "period" in
      let open Lwt.Syntax in
      let* boxscore_res = Db.get_boxscore ~game_id () in
      match boxscore_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok bs ->
          let* periods_res = Db.get_pbp_periods ~game_id () in
          (match periods_res with
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
          | Ok periods ->
              let selected_period =
                match period_opt with
                | Some p when List.mem p periods -> p
                | _ -> (match periods with | p :: _ -> p | [] -> "Q1")
              in
              let* events_res =
                match periods with
                | [] -> Lwt.return (Ok [])
                | _ -> Db.get_pbp_events ~game_id ~period_code:selected_period ()
              in
              match events_res with
              | Ok events -> Dream.html (Views.pbp_page ~game:bs.boxscore_game ~periods ~selected_period ~events)
              | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    (* Leaders *)
    Dream.get "/leaders" (fun request ->
      let open Lwt.Syntax in
      let scope = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let categories =
            match String.lowercase_ascii scope with
            | "totals" ->
                [ "gp"; "min"; "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
            | "per_36" ->
                [ "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "eff"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
            | _ ->
                [ "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "min"; "eff"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
          in
          let rec fetch_all acc = function
            | [] -> Lwt.return (Ok (List.rev acc))
            | category :: rest ->
                let* res = Db.get_leaders ~season ~scope category in
                (match res with
                | Error e -> Lwt.return (Error e)
                | Ok leaders -> fetch_all ((category, leaders) :: acc) rest)
          in
          let* leaders_res = fetch_all [] categories in
          match leaders_res with
          | Ok leaders_by_category ->
              Dream.html (Views.leaders_page ~season ~seasons ~scope leaders_by_category)
          | Error e ->
              Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Awards (Stat-based, unofficial) *)
    Dream.get "/awards" (fun request ->
      let open Lwt.Syntax in
      let include_mismatch = query_bool request "include_mismatch" in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let prev_season_code =
            if season = "ALL" then None
            else
              let rec loop prev = function
                | [] -> None
                | (s: season_info) :: rest ->
                    if s.code = season then prev else loop (Some s.code) rest
              in
              loop None seasons
          in
          let prev_season_name =
            match prev_season_code with
            | None -> None
            | Some code ->
                seasons
                |> List.find_opt (fun (s: season_info) -> s.code = code)
                |> Option.map (fun (s: season_info) -> s.name)
          in
          let* mvp_res = Db.get_stat_mvp_eff ~season ~include_mismatch () in
          let* mip_res =
            match prev_season_code with
            | None -> Lwt.return (Ok [])
            | Some prev_season -> Db.get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch ()
          in
          match mvp_res, mip_res with
          | Ok mvp, Ok mip -> Dream.html (Views.awards_page ~season ~seasons ~include_mismatch ~prev_season_name ~mvp ~mip)
          | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Compare *)
    Dream.get "/compare" (fun request ->
      let open Lwt.Syntax in
      let p1_query = Dream.query request "p1" |> Option.value ~default:"" in
      let p2_query = Dream.query request "p2" |> Option.value ~default:"" in
      let p1_id = Dream.query request "p1_id" |> Option.value ~default:"" in
      let p2_id = Dream.query request "p2_id" |> Option.value ~default:"" in
      let p1_id_opt = if String.trim p1_id = "" then None else Some (String.trim p1_id) in
      let p2_id_opt = if String.trim p2_id = "" then None else Some (String.trim p2_id) in

      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let is_valid_season code =
            code = "ALL" || List.exists (fun (s: season_info) -> s.code = code) seasons
          in
          let p1_season =
            match query_nonempty request "p1_season" with
            | None -> season
            | Some v -> if is_valid_season v then v else season
          in
          let p2_season =
            match query_nonempty request "p2_season" with
            | None -> season
            | Some v -> if is_valid_season v then v else season
          in
          let errors : string list ref = ref [] in
          let add_error msg = errors := msg :: !errors in

          if p1_id_opt <> None && p1_id_opt = p2_id_opt then add_error "Players must be different.";

          let* p1_sel_res =
            match p1_id_opt with
            | None -> Lwt.return (Ok None)
            | Some pid -> Db.get_player_aggregate_by_id ~player_id:pid ~season:p1_season ()
          in
          let* p2_sel_res =
            match p2_id_opt with
            | None -> Lwt.return (Ok None)
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

          let* p1_candidates_res =
            if p1_selected = None && String.trim p1_query <> "" then
              let limit = if p1_id_opt <> None then 30 else 8 in
              Db.get_players ~season:p1_season ~search:p1_query ~sort:ByMinutes ~limit ()
            else
              Lwt.return (Ok [])
          in
          let* p2_candidates_res =
            if p2_selected = None && String.trim p2_query <> "" then
              let limit = if p2_id_opt <> None then 30 else 8 in
              Db.get_players ~season:p2_season ~search:p2_query ~sort:ByMinutes ~limit ()
            else
              Lwt.return (Ok [])
          in
          let p1_candidates = match p1_candidates_res with Ok xs -> xs | Error _ -> [] in
          let p2_candidates = match p2_candidates_res with Ok xs -> xs | Error _ -> [] in

          (* Fallback: if aggregate-by-id fails but name search returns the id, use it. *)
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

          let* p1_available_seasons =
            match p1_id_opt, p1_selected with
            | Some pid, None -> (
                let* res = Db.get_player_season_stats ~player_id:pid ~scope:"per_game" () in
                match res with
                | Ok stats ->
                    let codes =
                      stats
                      |> List.map (fun (s: season_stats) -> s.ss_season_code)
                      |> List.sort_uniq String.compare
                    in
                    Lwt.return (Some codes)
                | Error _ -> Lwt.return None
              )
            | _ -> Lwt.return None
          in
          let* p2_available_seasons =
            match p2_id_opt, p2_selected with
            | Some pid, None -> (
                let* res = Db.get_player_season_stats ~player_id:pid ~scope:"per_game" () in
                match res with
                | Ok stats ->
                    let codes =
                      stats
                      |> List.map (fun (s: season_stats) -> s.ss_season_code)
                      |> List.sort_uniq String.compare
                    in
                    Lwt.return (Some codes)
                | Error _ -> Lwt.return None
              )
            | _ -> Lwt.return None
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
          let* h2h_res =
            match p1_selected, p2_selected, h2h_disabled_reason with
            | Some a, Some b, None ->
                Db.get_player_h2h_data ~p1_id:a.player_id ~p2_id:b.player_id ~season:p1_season ()
            | _ -> Lwt.return (Ok [])
          in
          let h2h = match h2h_res with Ok h -> h | Error _ -> [] in
          let error_opt =
            match List.rev !errors with
            | [] -> None
            | xs -> Some (String.concat " / " xs)
          in
          Dream.html
            (Views.compare_page
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
               p1_selected
               p2_selected
               h2h)
    );

    (* Predict (Team vs Team) *)
    Dream.get "/predict" (fun request ->
      let open Lwt.Syntax in
      let home = Dream.query request "home" |> Option.value ~default:"" in
      let away = Dream.query request "away" |> Option.value ~default:"" in
      let include_mismatch = query_bool request "include_mismatch" in
      let context_enabled =
        Dream.query request "context"
        |> Option.map String.lowercase_ascii
        |> function
        | Some ("1" | "true" | "yes" | "on") -> true
        | _ -> false
      in
      let is_neutral =
        Dream.query request "neutral"
        |> Option.map String.lowercase_ascii
        |> function
        | Some ("1" | "true" | "yes" | "on") -> true
        | _ -> false
      in

      let* seasons_res = Db.get_seasons () in
      let* teams_res = Db.get_all_teams () in
      match seasons_res, teams_res with
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let season = query_season_or_latest request seasons in
          let render result error =
            Dream.html (Views.predict_page ~season ~seasons ~teams ~home ~away ~is_neutral ~context_enabled ~include_mismatch result error)
          in

          if String.trim home = "" || String.trim away = "" then
            render None None
          else if normalize_label home = normalize_label away then
            render None (Some "Home/Away teams must be different.")
          else
            let* totals_res = Db.get_team_stats ~season ~scope:Totals ~include_mismatch () in
            let* games_res = Db.get_scored_games ~season ~include_mismatch () in
            match totals_res, games_res with
            | Ok totals, Ok games ->
                let find_team (name : string) =
                  let key = normalize_label name in
                  List.find_opt (fun (row : team_stats) -> normalize_label row.team = key) totals
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
                let last_game_for_team (name : string) =
                  let key = normalize_label name in
                  games
                  |> List.filter (fun (g : game_summary) ->
                      normalize_label g.home_team = key || normalize_label g.away_team = key)
                  |> List.sort (fun (a : game_summary) (b : game_summary) ->
                      let by_date = String.compare b.game_date a.game_date in
                      if by_date <> 0 then by_date else String.compare b.game_id a.game_id)
                  |> function
                  | [] -> None
                  | g :: _ -> Some g
                in
                let roster_for_team ~team_name ~game_id ~season_code =
                  let open Lwt.Syntax in
                  let* core_res = Db.get_team_core_player_ids ~season:season_code ~team_name () in
                  let* active_res = Db.get_team_active_player_ids ~team_name ~game_id () in
                  match core_res, active_res with
                  | Ok core_ids, Ok active_ids ->
                      let present =
                        core_ids
                        |> List.filter (fun pid -> List.mem pid active_ids)
                        |> List.length
                      in
                      Lwt.return (Some { rcs_present = present; rcs_total = List.length core_ids })
                  | _ -> Lwt.return None
                in
                let season_for_game_id (game_id : string) =
                  if season <> "ALL" then Lwt.return season
                  else
                    let open Lwt.Syntax in
                    let* res = Db.get_game_season_code ~game_id () in
                    match res with
                    | Ok (Some s) -> Lwt.return s
                    | _ -> Lwt.return "ALL"
                in
                (match find_team home, find_team away with
                | Some home_row, Some away_row ->
                    let* context_input =
                      if not context_enabled then Lwt.return None
                      else
                        let* home_roster =
                          match last_game_for_team home with
                          | None -> Lwt.return None
                          | Some g ->
                              let* sc = season_for_game_id g.game_id in
                              roster_for_team ~team_name:home ~game_id:g.game_id ~season_code:sc
                        in
                        let* away_roster =
                          match last_game_for_team away with
                          | None -> Lwt.return None
                          | Some g ->
                              let* sc = season_for_game_id g.game_id in
                              roster_for_team ~team_name:away ~game_id:g.game_id ~season_code:sc
                        in
                        Lwt.return (Some { pci_home_roster = home_roster; pci_away_roster = away_roster })
                    in
                    let output =
                      Prediction.predict_match_nerd
                        ~context:context_input
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
            | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Player Profile *)
    Dream.get "/player/:id" (fun request ->
      let player_id = Dream.param request "id" in
      let scope = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let open Lwt.Syntax in
      let* profile_res = Db.get_player_profile ~player_id () in
      match profile_res with
      | Ok (Some profile) -> 
          (* We only need to overwrite season_breakdown if scope is not per_game (default) *)
          let* final_profile = 
            if scope = "per_game" then Lwt.return profile
            else 
              let* stats_res = Db.get_player_season_stats ~player_id ~scope () in
              match stats_res with
              | Ok stats -> Lwt.return { profile with season_breakdown = stats }
              | Error _ -> Lwt.return profile (* Fallback to default if error *)
          in
          let* seasons_res = Db.get_seasons () in
          let seasons_catalog =
            match seasons_res with
            | Ok seasons -> seasons
            | Error _ -> []
          in
          let season_for_leaderboards =
            match seasons_catalog with
            | [] -> "ALL"
            | seasons -> query_season_or_latest request seasons
          in
          let season_name_for_leaderboards =
            seasons_catalog
            |> List.find_opt (fun (s: season_info) -> s.code = season_for_leaderboards)
            |> Option.map (fun (s: season_info) -> s.name)
            |> Option.value ~default:season_for_leaderboards
          in
          let leaderboard_categories =
            match String.lowercase_ascii scope with
            | "totals" ->
                [ "gp"; "min"; "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
            | "per_36" ->
                [ "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "eff"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
            | _ ->
                [ "pts"; "reb"; "ast"; "stl"; "blk"; "tov"; "min"; "eff"; "fg_pct"; "fg3_pct"; "ft_pct"; "ts_pct"; "efg_pct" ]
          in
          let rec fetch_all acc = function
            | [] -> Lwt.return (Ok (List.rev acc))
            | category :: rest ->
                let* res = Db.get_leaders ~season:season_for_leaderboards ~scope category in
                (match res with
                | Error e -> Lwt.return (Error e)
                | Ok leaders -> fetch_all ((category, leaders) :: acc) rest)
          in
          let* leaderboards_res = fetch_all [] leaderboard_categories in
          let leaderboards =
            match leaderboards_res with
            | Ok leaders_by_category ->
                Some (season_for_leaderboards, season_name_for_leaderboards, leaders_by_category)
            | Error _ ->
                None
          in
          Dream.html (Views.player_profile_page ~leaderboards final_profile ~scope ~seasons_catalog)
      | Ok None -> Dream.html (Views.error_page "Player not found")
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Player Game Log *)
    Dream.get "/player/:id/games" (fun request ->
      let player_id = Dream.param request "id" in
      let include_mismatch = query_bool request "include_mismatch" in
      let open Lwt.Syntax in
      let* profile_res = Db.get_player_profile ~player_id () in
      let* seasons_res = Db.get_seasons () in
      match profile_res, seasons_res with
      | Ok None, _ -> Dream.html (Views.error_page "Player not found")
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok (Some profile), Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* games_res = Db.get_player_game_logs ~player_id ~season ~include_mismatch () in
          (match games_res with
          | Ok games -> Dream.html (Views.player_game_logs_page profile ~season ~seasons ~include_mismatch games)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    (* Player Season Stats HTMX Partial *)
    Dream.get "/player/:id/season-stats" (fun request ->
      let player_id = Dream.param request "id" in
      let scope = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let open Lwt.Syntax in
      let* stats_res = Db.get_player_season_stats ~player_id ~scope () in
      match stats_res with
      | Ok stats -> Dream.html (Views.player_season_stats_component ~player_id ~scope stats)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e)) (* Or minimal error snippet *)
    );

    (* Team Profile *)
    Dream.get "/team/:name" (fun request ->
      let team_name = Dream.param request "name" |> Uri.pct_decode in
      let open Lwt.Syntax in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let* detail_res = Db.get_team_full_detail ~team_name ~season () in
          (match detail_res with
          | Ok detail -> Dream.html (Views.team_profile_page detail ~season ~seasons)
          | Error e -> Dream.html (Views.error_page (Db.show_db_error e)))
    );

    (* Convenience aliases for transactions *)
    Dream.get "/draft" (fun request ->
      let year = Dream.query request "year" |> Option.value ~default:"" in
      let q = Dream.query request "q" |> Option.value ~default:"" in
      let params = String.concat "&" (List.filter (fun s -> s <> "") [
        "tab=draft";
        (if year <> "" then "year=" ^ year else "");
        (if q <> "" then "q=" ^ q else "")
      ]) in
      Dream.redirect request ("/transactions?" ^ params)
    );
    Dream.get "/trade" (fun request ->
      let year = Dream.query request "year" |> Option.value ~default:"" in
      let q = Dream.query request "q" |> Option.value ~default:"" in
      let params = String.concat "&" (List.filter (fun s -> s <> "") [
        "tab=trade";
        (if year <> "" then "year=" ^ year else "");
        (if q <> "" then "q=" ^ q else "")
      ]) in
      Dream.redirect request ("/transactions?" ^ params)
    );

    (* Draft / Trade (official transactions) *)
    Dream.get "/transactions" (fun request ->
      let tab =
        Dream.query request "tab"
        |> Option.map String.lowercase_ascii
        |> Option.value ~default:"draft"
      in
      let year =
        let year_str_opt = Dream.query request "year" in
        match year_str_opt with
        | None -> 0
        | Some s -> (match int_of_string_opt s with Some i -> i | None -> 0)
      in
      let q = Dream.query request "q" |> Option.value ~default:"" in
      let open Lwt.Syntax in
      let* draft_years_res = Db.get_draft_years () in
      let* trade_years_res = Db.get_official_trade_years () in
      match draft_years_res, trade_years_res with
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok draft_years, Ok trade_years ->
          if tab = "trade" then (
            let* events_res = Db.get_official_trade_events ~year ~search:q () in
            match events_res with
            | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
            | Ok events ->
                Dream.html
                  (Views.transactions_page
                     ~tab
                     ~year
                     ~q
                     ~draft_years
                     ~trade_years
                     ~draft_picks:[]
                     ~trade_events:events)
          ) else (
            let* picks_res = Db.get_draft_picks ~year ~search:q () in
            match picks_res with
            | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
            | Ok picks ->
                Dream.html
                  (Views.transactions_page
                     ~tab:"draft"
                     ~year
                     ~q
                     ~draft_years
                     ~trade_years
                     ~draft_picks:picks
                     ~trade_events:[])
          )
    );

    (* QA & System *)
    Dream.get "/qa" (fun _ ->
      let open Lwt.Syntax in
      let markdown = Qa.read_markdown_if_exists () in
      let* report_res = Db.get_db_quality_report () in
      match report_res with
      | Ok report -> Dream.html (Views.qa_dashboard_page report ~markdown ())
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );
    Dream.get "/health" (fun _ -> Dream.json "{\"status\": \"ok\", \"engine\": \"OCaml/Dream\"}");
  ]
