(** WKBL Web Server

    Dream-based web server with HTMX frontend.
    All handlers are pure functions returning HTML.
*)

open Wkbl

(** Initialize database connection *)
let init_db () =
  let db_path =
    match Sys.getenv_opt "WKBL_DB" with
    | Some path -> path
    | None -> "../data/wkbl.db"
  in
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

(** Players partial - for HTMX updates *)
let players_handler request =
  let open Lwt.Syntax in
  let _sort = Dream.query request "sort" in
  let _search = Dream.query request "search" in
  (* TODO: Apply filters *)
  let* result = Db.get_players ~limit:50 () in
  match result with
  | Ok players ->
      Dream.html (Views.players_table players)
  | Error e ->
      Dream.html (Printf.sprintf "<div class='text-red-500'>Error: %s</div>" (Db.show_db_error e))

(** Health check *)
let health_handler _request =
  Dream.json {|{"status": "ok", "runtime": "ocaml"}|}

(** Teams page handler *)
let teams_handler _request =
  let open Lwt.Syntax in
  let* result = Db.get_all_teams () in
  match result with
  | Ok teams ->
      Dream.html (Views.teams_page teams)
  | Error e ->
      Dream.html (Views.error_page (Db.show_db_error e))

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
    Dream.get "/players" players_handler;
    Dream.get "/teams" teams_handler;
    Dream.get "/teams/:team" team_detail_handler;
    Dream.get "/health" health_handler;
  ]
