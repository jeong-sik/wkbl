(** WKBL Analytics Main Entry Point
    OCaml Edition using Dream framework
*)

open Wkbl
open Wkbl.Domain

let has_prefix ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

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
      let sort = Domain.player_sort_of_string sort_str in
      let* players_res = Db.get_players ~search ~sort () in
      match players_res with
      | Ok p -> Dream.html (Views.players_page ~search ~sort:sort_str p)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    Dream.get "/players/table" (fun request ->
      let open Lwt.Syntax in
      let search = Dream.query request "search" |> Option.value ~default:"" in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"eff" in
      let sort = Domain.player_sort_of_string sort_str in
      let* players_res = Db.get_players ~search ~sort () in
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
      let* seasons_res = Db.get_seasons () in
      let* stats_res = Db.get_team_stats ~season ~scope ~sort () in
      match seasons_res, stats_res with
      | Ok seasons, Ok stats -> Dream.html (Views.teams_page ~season ~seasons ~scope ~sort:sort_str stats)
      | Error e, _ | _, Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    Dream.get "/teams/table" (fun request ->
      let open Lwt.Syntax in
      let season = Dream.query request "season" |> Option.value ~default:"ALL" in
      let scope_str = Dream.query request "scope" |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Dream.query request "sort" |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let* stats_res = Db.get_team_stats ~season ~scope ~sort () in
      match stats_res with
      | Ok stats -> Dream.html (Views.teams_table ~scope stats)
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
      | Ok standings -> Dream.html (Views.standings_table standings)
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

    (* Compare *)
    Dream.get "/compare" (fun request ->
      let open Lwt.Syntax in
      let p1_name = Dream.query request "p1" |> Option.value ~default:"" in
      let p2_name = Dream.query request "p2" |> Option.value ~default:"" in
      if p1_name = "" || p2_name = "" then
        Dream.html (Views.compare_page None None [])
      else
        let* p1_res = Db.get_player_by_name p1_name in
        let* p2_res = Db.get_player_by_name p2_name in
        match p1_res, p2_res with
        | Ok (Some a), Ok (Some b) ->
            let* h2h_res = Db.get_player_h2h_data ~p1_id:a.player_id ~p2_id:b.player_id () in
            let h2h = match h2h_res with Ok h -> h | Error _ -> [] in
            Dream.html (Views.compare_page (Some a) (Some b) h2h)
        | Ok None, _ -> Dream.html (Views.error_page (Printf.sprintf "Player not found: %s" p1_name))
        | _, Ok None -> Dream.html (Views.error_page (Printf.sprintf "Player not found: %s" p2_name))
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
          Dream.html (Views.player_profile_page final_profile ~scope)
      | Ok None -> Dream.html (Views.error_page "Player not found")
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
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
      let* detail_res = Db.get_team_full_detail ~team_name () in
      match detail_res with
      | Ok detail -> Dream.html (Views.team_profile_page detail)
      | Error e -> Dream.html (Views.error_page (Db.show_db_error e))
    );

    (* QA & System *)
    Dream.get "/qa" (fun _ -> 
      let content = 
        match Qa.load_markdown () with
        | Ok md -> Printf.sprintf {html|<div class="prose dark:prose-invert max-w-none">%s</div>|html} md
        | Error _ -> "QA Report not available."
      in
      Dream.html (Views.layout ~title:"QA Report" ~content)
    );
    Dream.get "/health" (fun _ -> Dream.json "{\"status\": \"ok\", \"engine\": \"OCaml/Dream\"}");
  ]
