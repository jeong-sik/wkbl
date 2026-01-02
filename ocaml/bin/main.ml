(** WKBL Web Server

    Dream-based web server with HTMX frontend.
    All handlers are pure functions returning HTML.
*)

open Wkbl
open Domain

let rec find_db_path dir depth =
  if depth < 0 then None
  else
    let candidate = Filename.concat dir "data/wkbl.db" in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_db_path parent (depth - 1)

let resolve_db_path () =
  match Sys.getenv_opt "WKBL_DB" with
  | Some path when Sys.file_exists path -> Some path
  | Some _ -> None
  | None -> find_db_path (Sys.getcwd ()) 4

let query_string request key =
  match Dream.query request key with
  | None -> ""
  | Some value -> String.trim value

let sort_field_of_param value =
  match String.lowercase_ascii value with
  | "pts" | "points" -> ByPoints
  | "reb" | "rebounds" -> ByRebounds
  | "ast" | "assists" -> ByAssists
  | "eff" | "efficiency" -> ByEfficiency
  | "min" | "minutes" -> ByMinutes
  | _ -> ByEfficiency

let team_scope_of_param value =
  team_scope_of_string (String.lowercase_ascii value)

let team_sort_of_param value =
  team_sort_field_of_string (String.lowercase_ascii value)

let latest_season_code seasons =
  let with_codes =
    seasons
    |> List.filter_map (fun s ->
        match int_of_string_opt s.code with
        | None -> None
        | Some code -> Some (code, s.code))
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  match List.rev with_codes with
  | (_, code) :: _ -> code
  | [] -> "ALL"

let resolve_season_param season seasons =
  if season = "" then latest_season_code seasons
  else season

let refresh_requested request =
  match Dream.query request "refresh" with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

(** Initialize database connection *)
let init_db () =
  match resolve_db_path () with
  | None ->
      Dream.log "Database file not found. Set WKBL_DB or place data/wkbl.db.";
      false
  | Some db_path ->
      match Db.init_pool db_path with
      | Ok () ->
          Dream.log "Database connected: %s" db_path;
          true
      | Error e ->
          Dream.log "Database connection failed: %s" (Db.show_db_error e);
          false

(** Home page handler *)
let home_handler _request =
  let open Lwt.Syntax in
  let* result = Db.get_players ~limit:50 () in
  match result with
  | Ok players ->
      Dream.html (Views.home_page players)
  | Error e ->
      Dream.html (Views.error_page (Db.show_db_error e))

(** Players page handler *)
let players_page_handler request =
  let open Lwt.Syntax in
  let sort_param = query_string request "sort" in
  let search = query_string request "search" in
  let sort_by = sort_field_of_param sort_param in
  let* result = Db.get_players ~limit:50 ~search ~sort:sort_by () in
  match result with
  | Ok players ->
      Dream.html (Views.players_page ~search ~sort:sort_param players)
  | Error e ->
      Dream.html (Views.error_page (Db.show_db_error e))

(** Players partial - for HTMX updates *)
let players_table_handler request =
  let open Lwt.Syntax in
  let sort_param = query_string request "sort" in
  let search = query_string request "search" in
  let sort_by = sort_field_of_param sort_param in
  let* result = Db.get_players ~limit:50 ~search ~sort:sort_by () in
  match result with
  | Ok players ->
      Dream.html (Views.players_table players)
  | Error e ->
      Dream.html (Printf.sprintf "<div class='text-red-500'>Error: %s</div>" (Db.show_db_error e))

(** Health check *)
let health_handler _request =
  Dream.json {|{"status": "ok", "runtime": "ocaml"}|}

(** Teams page handler *)
let teams_handler request =
  let open Lwt.Syntax in
  let season = query_string request "season" in
  let scope_param = query_string request "scope" in
  let sort_param = query_string request "sort" in
  let scope = team_scope_of_param scope_param in
  let sort = team_sort_of_param sort_param in
  let* seasons_result = Db.get_seasons () in
  let seasons = match seasons_result with
    | Ok seasons -> seasons
    | Error _ -> []
  in
  let season = resolve_season_param season seasons in
  let* stats_result = Db.get_team_stats ~season ~scope ~sort () in
  match stats_result with
  | Ok stats ->
      Dream.html (Views.teams_page ~season ~seasons ~scope ~sort:sort_param stats)
  | Error e ->
      Dream.html (Views.error_page (Db.show_db_error e))

(** Teams table handler (HTMX) *)
let teams_table_handler request =
  let open Lwt.Syntax in
  let season = query_string request "season" in
  let scope_param = query_string request "scope" in
  let sort_param = query_string request "sort" in
  let scope = team_scope_of_param scope_param in
  let sort = team_sort_of_param sort_param in
  let* seasons_result = Db.get_seasons () in
  let seasons = match seasons_result with
    | Ok seasons -> seasons
    | Error _ -> []
  in
  let season = resolve_season_param season seasons in
  let* stats_result = Db.get_team_stats ~season ~scope ~sort () in
  match stats_result with
  | Ok stats -> Dream.html (Views.teams_table ~scope stats)
  | Error e -> Dream.html (Printf.sprintf "<div class='text-red-500'>Error: %s</div>" (Db.show_db_error e))

(** Team detail handler - HTMX partial *)
let team_detail_handler request =
  let open Lwt.Syntax in
  let team_name = Dream.param request "team" |> Uri.pct_decode in
  let* result = Db.get_players_by_team ~team_name () in
  match result with
  | Ok players ->
      Dream.html (Views.team_detail team_name players)
  | Error e ->
      Dream.html (Printf.sprintf "<div class='text-red-500'>Error: %s</div>" (Db.show_db_error e))

(** QA page handler *)
let qa_handler request =
  let refresh = refresh_requested request in
  let result = Qa.load_report ~refresh () in
  Dream.html (Views.qa_page result)

(** QA JSON handler *)
let qa_json_handler request =
  let refresh = refresh_requested request in
  let json_response =
    match Qa.load_report ~refresh () with
    | Ok report -> (Qa.report_to_yojson report, 200)
    | Error err ->
        let payload =
          `Assoc (
            ("error", `String err.message) ::
            (match err.path with
            | None -> []
            | Some path -> [("path", `String path)])
          )
        in
        (payload, 404)
  in
  let body, code = json_response in
  Dream.json ~code (Yojson.Safe.to_string body)

(** QA Markdown handler *)
let qa_md_handler request =
  let refresh = refresh_requested request in
  let response =
    match Qa.load_markdown ~refresh () with
    | Ok markdown -> Dream.respond ~code:200 ~headers:["Content-Type", "text/markdown; charset=utf-8"] markdown
    | Error err ->
        let message =
          match err.path with
          | None -> err.message
          | Some path -> err.message ^ " / " ^ path
        in
        Dream.respond ~code:404 ~headers:["Content-Type", "text/plain; charset=utf-8"] message
  in
  response

(** Main entry point *)
let () =
  (* Initialize DB *)
  let db_ok = init_db () in
  if not db_ok then
    Dream.log "WARNING: Running without database connection";

  (* Start server *)
  Dream.run
    ~port:8080
    ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" home_handler;
    Dream.get "/players" players_page_handler;
    Dream.get "/players/table" players_table_handler;
    Dream.get "/teams" teams_handler;
    Dream.get "/teams/table" teams_table_handler;
    Dream.get "/teams/:team" team_detail_handler;
    Dream.get "/health" health_handler;
    Dream.get "/qa" qa_handler;
    Dream.get "/qa.json" qa_json_handler;
    Dream.get "/qa.md" qa_md_handler;
  ]
