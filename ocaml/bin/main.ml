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

let rec find_static_path start_dir =
  let has_styles path = Sys.file_exists (Filename.concat path "css/styles.css") in
  let candidates = [ Filename.concat start_dir "static"; Filename.concat start_dir "ocaml/static" ] in
  match List.find_opt has_styles candidates with
  | Some p -> Some p
  | None ->
      let parent = Filename.dirname start_dir in
      if parent = start_dir then None else find_static_path parent

let () =
  (* Resolve runtime config from env. *)
  let db_path = Sys.getenv_opt "WKBL_DB_PATH" |> Option.value ~default:Db.default_db_path in
  let port =
    match Sys.getenv_opt "PORT" with
    | None -> 8000
    | Some value -> (
        match int_of_string_opt (String.trim value) with
        | Some p when p > 0 -> p
        | _ -> 8000)
  in

  (* Initialize database pool *)
  match Db.init_pool db_path with
  | Error e ->
      Printf.eprintf "Failed to init DB (%s): %s\n" db_path (Db.show_db_error e);
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
  Printf.printf "Using DB path: %s\n%%!" db_path;
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
      Dream.get "**" (Dream.static static_path)
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
      let* players_res = Db.get_players ~search ~sort ~include_mismatch () in
      match players_res with
      | Ok p -> Dream.html (Views.players_page ~search ~sort:sort_str ~include_mismatch p)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    Dream.get "/players/table" (fun request ->
      let open Lwt.Syntax in
      let search = Dream.query request "search" |> Option.value ~default:"" in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      let* players_res = Db.get_players ~search ~sort ~include_mismatch () in
      match players_res with
      | Ok p -> Dream.html (Views.players_table p)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Teams Page & Table HTMX *)
    Dream.get "/teams" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let scope_str = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      let* seasons_res = Db.get_seasons () in
      let* stats_res = Db.get_team_stats ~season ~scope ~sort ~include_mismatch () in
      match seasons_res, stats_res with
      | Ok seasons, Ok stats -> Dream.html (Views.teams_page ~season ~seasons ~scope ~sort:sort_str ~include_mismatch stats)
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let* seasons_res = Db.get_seasons () in
      let* standings_res = Db.get_standings ~season () in
      match seasons_res, standings_res with
      | Ok seasons, Ok standings -> Dream.html (Views.standings_page ~season ~seasons standings)
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let* seasons_res = Db.get_seasons () in
      let* games_res = Db.get_games ~season () in
      match seasons_res, games_res with
      | Ok seasons, Ok games -> Dream.html (Views.games_page ~season ~seasons games)
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let* seasons_res = Db.get_seasons () in
      let* games_res = Db.get_games ~season () in
      match seasons_res, games_res with
      | Ok seasons, Ok games -> Dream.html (Views.boxscores_page ~season ~seasons games)
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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

    (* Leaders *)
    Dream.get "/leaders" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let scope = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let* seasons = Db.get_seasons () in
      let* pts = Db.get_leaders ~season ~scope "pts" in
      let* reb = Db.get_leaders ~season ~scope "reb" in
      let* ast = Db.get_leaders ~season ~scope "ast" in
      let* stl = Db.get_leaders ~season ~scope "stl" in
      let* blk = Db.get_leaders ~season ~scope "blk" in
      match seasons, pts, reb, ast, stl, blk with
      | Ok s, Ok p, Ok r, Ok a, Ok st, Ok b -> Dream.html (Views.leaders_page ~season ~seasons:s ~scope p r a st b)
      | Error e, _, _, _, _, _ -> Dream.html (Views.error_page (Db.show_db_error e))
      | _, Error e, _, _, _, _ -> Dream.html (Views.error_page (Db.show_db_error e))
      | _, _, Error e, _, _, _ -> Dream.html (Views.error_page (Db.show_db_error e))
      | _, _, _, Error e, _, _ -> Dream.html (Views.error_page (Db.show_db_error e))
      | _, _, _, _, Error e, _ -> Dream.html (Views.error_page (Db.show_db_error e))
      | _, _, _, _, _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Awards (Stat-based, unofficial) *)
    Dream.get "/awards" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let include_mismatch = query_bool request "include_mismatch" in
      let* seasons_res = Db.get_seasons () in
      match seasons_res with
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
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
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
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
          let errors : string list ref = ref [] in
          let add_error msg = errors := msg :: !errors in

          if p1_id_opt <> None && p1_id_opt = p2_id_opt then add_error "Players must be different.";

          let* p1_sel_res =
            match p1_id_opt with
            | None -> Lwt.return (Ok None)
            | Some pid -> Db.get_player_aggregate_by_id ~player_id:pid ~season ()
          in
          let* p2_sel_res =
            match p2_id_opt with
            | None -> Lwt.return (Ok None)
            | Some pid -> Db.get_player_aggregate_by_id ~player_id:pid ~season ()
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
          (match p1_id_opt, p1_selected with
          | Some pid, None -> add_error (Printf.sprintf "No stats for player_id=%s (season=%s)" pid season)
          | _ -> ());
          (match p2_id_opt, p2_selected with
          | Some pid, None -> add_error (Printf.sprintf "No stats for player_id=%s (season=%s)" pid season)
          | _ -> ());

          let* p1_candidates_res =
            if p1_selected = None && String.trim p1_query <> "" then
              Db.get_players ~search:p1_query ~sort:ByMinutes ~limit:8 ()
            else
              Lwt.return (Ok [])
          in
          let* p2_candidates_res =
            if p2_selected = None && String.trim p2_query <> "" then
              Db.get_players ~search:p2_query ~sort:ByMinutes ~limit:8 ()
            else
              Lwt.return (Ok [])
          in
          let p1_candidates = match p1_candidates_res with Ok xs -> xs | Error _ -> [] in
          let p2_candidates = match p2_candidates_res with Ok xs -> xs | Error _ -> [] in

          let* h2h_res =
            match p1_selected, p2_selected with
            | Some a, Some b -> Db.get_player_h2h_data ~p1_id:a.player_id ~p2_id:b.player_id ~season ()
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
               ~p1_query
               ~p2_query
               ~p1_id:p1_id_opt
               ~p2_id:p2_id_opt
               ~p1_candidates
               ~p2_candidates
               ~error:error_opt
               p1_selected
               p2_selected
               h2h)
    );

    (* Predict (Team vs Team) *)
    Dream.get "/predict" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
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

      let render result error =
        match seasons_res, teams_res with
        | Ok seasons, Ok teams ->
            Dream.html (Views.predict_page ~season ~seasons ~teams ~home ~away ~is_neutral ~context_enabled ~include_mismatch result error)
        | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
          Dream.html (Views.player_profile_page final_profile ~scope ~seasons_catalog)
      | Ok None -> Dream.html (Views.error_page "Player not found")
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* Player Game Log *)
    Dream.get "/player/:id/games" (fun request ->
      let player_id = Dream.param request "id" in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let include_mismatch = query_bool request "include_mismatch" in
      let open Lwt.Syntax in
      let* profile_res = Db.get_player_profile ~player_id () in
      let* seasons_res = Db.get_seasons () in
      let* games_res = Db.get_player_game_logs ~player_id ~season ~include_mismatch () in
      match profile_res, seasons_res, games_res with
      | Ok (Some profile), Ok seasons, Ok games ->
          Dream.html (Views.player_game_logs_page profile ~season ~seasons ~include_mismatch games)
      | Ok None, _, _ -> Dream.html (Views.error_page "Player not found")
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let open Lwt.Syntax in
      let* seasons_res = Db.get_seasons () in
      let* detail_res = Db.get_team_full_detail ~team_name ~season () in
      match seasons_res, detail_res with
      | Ok seasons, Ok detail -> Dream.html (Views.team_profile_page detail ~season ~seasons)
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
