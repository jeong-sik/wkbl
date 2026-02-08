(** WKBL Analytics Main Entry Point
    OCaml Edition using Kirin framework
*)

open Wkbl
open Wkbl.Domain

let query_bool request name =
  Kirin.query_opt name request
  |> Option.fold ~none:false ~some:(fun v ->
      List.mem (String.lowercase_ascii (String.trim v)) ["1"; "true"; "yes"; "on"])

let query_nonempty request name =
  match Kirin.query_opt name request with
  | None -> None
  | Some v ->
      let trimmed = String.trim v in
      if trimmed = "" then None else Some trimmed

let query_season_or_latest request (seasons : season_info list) =
  let requested = query_nonempty request "season" |> Option.value ~default:"" in
  Request_params.normalize_season ~seasons ~requested

let is_safe_redirect_path (s : string) =
  let t = String.trim s in
  String.length t > 0
  && String.get t 0 = '/'
  && (String.length t = 1 || String.get t 1 <> '/')
  && not (String.contains t '\n')
  && not (String.contains t '\r')

let request_lang request =
  Option.bind (Kirin.header "Cookie" request) I18n.lang_of_cookie_header
  |> Option.value ~default:I18n.Ko

module Pbp_backfill = struct
  (* Guardrails:
     - Never block HTTP responses on network scraping.
     - Deduplicate concurrent backfills per game_id.
     - Cooldown avoids repeated upstream hits when data is persistently missing. *)
  let inflight : (string, unit) Hashtbl.t = Hashtbl.create 256
  let last_attempt : (string, float) Hashtbl.t = Hashtbl.create 256

  let cooldown_seconds = 60.0
  let timeout_seconds = 10.0

  let parse_game_id (id : string) : (string * string * int) option =
    match String.split_on_char '-' (String.trim id) with
    | [season_gu; game_type; game_no_s] -> (
        match int_of_string_opt (String.trim game_no_s) with
        | Some game_no -> Some (String.trim season_gu, String.trim game_type, game_no)
        | None -> None)
    | _ -> None

  let maybe_spawn ~sw ~env ~(game_id : string) ~(tag : string) () : unit =
    match parse_game_id game_id with
    | None -> ()
    | Some (season_gu, game_type, game_no) ->
        let now = Unix.time () in
        if Hashtbl.mem inflight game_id then ()
        else (
          match Hashtbl.find_opt last_attempt game_id with
          | Some last when now -. last < cooldown_seconds -> ()
          | _ ->
              Hashtbl.replace inflight game_id ();
              Hashtbl.replace last_attempt game_id now;
              let clock = Eio.Stdenv.clock env in
              Eio.Fiber.fork ~sw (fun () ->
                Fun.protect
                  ~finally:(fun () -> Hashtbl.remove inflight game_id)
                  (fun () ->
                    try
                      let events_opt =
                        try
                          Some
                            (Eio.Time.with_timeout_exn clock timeout_seconds (fun () ->
                               Scraper.fetch_game_pbp_events ~sw ~env ~season_gu ~game_type
                                 ~game_no ()))
                        with
                        | Eio.Time.Timeout -> None
                      in
                      (match events_opt with
                      | None ->
                          Printf.eprintf "[%s] backfill fetch timeout (%s)\n%!" tag game_id
                      | Some events ->
                          if events <> [] then (
                            let rows =
                              events |> List.map (fun (e : Domain.pbp_event) -> (e, None))
                            in
                            match Db_sync.replace_pbp_events ~game_id rows with
                            | Ok _n -> ()
                            | Error e ->
                                Printf.eprintf "[%s] backfill DB error (%s): %s\n%!"
                                  tag
                                  game_id
                                  (Db.show_db_error e)
                          ))
                    with
                    | exn ->
                        Printf.eprintf "[%s] backfill error (%s): %s\n%!"
                          tag
                          game_id
                          (Printexc.to_string exn)
                  )
              )
        )
end

module Player_meta_backfill = struct
  (* Guardrails:
     - Never block HTTP responses on network scraping.
     - Deduplicate concurrent fetches per player_id.
     - Cooldown avoids repeated upstream hits when data is persistently missing. *)
  let inflight : (string, unit) Hashtbl.t = Hashtbl.create 256
  let last_attempt : (string, float) Hashtbl.t = Hashtbl.create 256

  let cooldown_seconds = 6.0 *. 60.0 *. 60.0
  let timeout_seconds = 10.0

  let maybe_spawn ~sw ~env ~(player_id : string) ~(tag : string) () : unit =
    let pid = String.trim player_id in
    if pid = "" then ()
    else
      let now = Unix.time () in
      if Hashtbl.mem inflight pid then ()
      else (
        match Hashtbl.find_opt last_attempt pid with
        | Some last when now -. last < cooldown_seconds -> ()
        | _ ->
            Hashtbl.replace inflight pid ();
            Hashtbl.replace last_attempt pid now;
            let clock = Eio.Stdenv.clock env in
            Eio.Fiber.fork ~sw (fun () ->
              Fun.protect
                ~finally:(fun () -> Hashtbl.remove inflight pid)
                (fun () ->
                  try
                    let info_opt =
                      try
                        Eio.Time.with_timeout_exn clock timeout_seconds (fun () ->
                          Scraper.fetch_player_detail ~sw ~env ~player_id:pid)
                      with
                      | Eio.Time.Timeout -> None
                    in
                    match info_opt with
                    | None -> ()
                    | Some info -> (
                        match Db_sync.upsert_player_info info with
                        | Ok () -> ()
                        | Error e ->
                            Printf.eprintf "[%s] player meta DB error (%s): %s\n%!"
                              tag
                              pid
                              (Db.show_db_error e))
                  with
                  | exn ->
                      Printf.eprintf "[%s] player meta error (%s): %s\n%!"
                        tag
                        pid
                        (Printexc.to_string exn)
                )
            )
      )
end

(* Admin guard for QA override tools (exclude/restore).
   This should not be publicly accessible without a token. *)
let admin_cookie_name = "wkbl_admin"

let admin_token_env : string option =
  match Sys.getenv_opt "WKBL_ADMIN_TOKEN" with
  | None -> None
  | Some s ->
      let s = String.trim s in
      if s = "" then None else Some s

let admin_cookie_header ?(secure=false) (token : string) : string =
  (* Short-lived, path-wide; Strict reduces CSRF risk for these admin-only POSTs. *)
  let value = Uri.pct_encode token in
  let secure_attr = if secure then "; Secure" else "" in
  Printf.sprintf
    "%s=%s; Path=/; Max-Age=604800; SameSite=Strict; HttpOnly%s"
    admin_cookie_name
    value
    secure_attr

let is_admin request : bool =
  match (admin_token_env, Kirin.header "Cookie" request) with
  | Some expected, Some cookie_header ->
      (match I18n.cookie_value cookie_header admin_cookie_name with
      | Some got -> Uri.pct_decode (String.trim got) = expected
      | None -> false)
  | _ -> false

let referer_to_path (referer : string) : string option =
  let r = String.trim referer in
  if r = "" then None
  else if String.starts_with ~prefix:"/" r then Some r
  else
    (* Best-effort: extract "/path?query" from an absolute URL. *)
    let len = String.length r in
    let rec find_scheme_slash i =
      if i + 2 >= len then None
      else if r.[i] = ':' && r.[i + 1] = '/' && r.[i + 2] = '/' then Some (i + 3)
      else find_scheme_slash (i + 1)
    in
    match find_scheme_slash 0 with
    | None -> None
    | Some start ->
        let rec find_path i =
          if i >= len then None
          else if r.[i] = '/' then Some i
          else find_path (i + 1)
        in
        (match find_path start with
        | None -> None
        | Some i -> Some (String.sub r i (len - i)))

let redirect_back_or_home request =
  let next_q = Kirin.query_opt "next" request |> Option.value ~default:"" |> String.trim in
  if next_q <> "" && is_safe_redirect_path next_q then
    next_q
  else
    match Option.bind (Kirin.header "Referer" request) referer_to_path with
    | Some p when is_safe_redirect_path p -> p
    | _ -> "/"

let build_player_info_map (infos: player_info list) =
  let map = Hashtbl.create (List.length infos * 2 + 1) in
  infos |> List.iter (fun (p: player_info) ->
    Hashtbl.replace map p.id p
  );
  map

let get_player_info_map () =
  match Db.get_all_player_info () with
  | Ok infos -> Some (build_player_info_map infos)
  | Error _ -> None

let player_meta_missing (info_opt : player_info option) : bool =
  match info_opt with
  | None -> true
  | Some info -> info.position = None && info.birth_date = None && info.height = None

let backfill_duplicate_player_meta ~sw ~env ~(players : player_aggregate list)
    (player_info_map : (string, player_info) Hashtbl.t option) : unit =
  match player_info_map with
  | None -> ()
  | Some map ->
      let name_counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
      players |> List.iter (fun (p : player_aggregate) ->
        let key = Views_common.normalize_name p.name in
        let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
        Hashtbl.replace name_counts key (prev + 1)
      );
      players |> List.iter (fun (p : player_aggregate) ->
        let key = Views_common.normalize_name p.name in
        let is_dup =
          match Hashtbl.find_opt name_counts key with
          | Some c when c > 1 -> true
          | _ -> false
        in
        if is_dup then (
          let info_opt = Hashtbl.find_opt map p.player_id in
          if player_meta_missing info_opt then
            Player_meta_backfill.maybe_spawn ~sw ~env ~player_id:p.player_id ~tag:"PLAYERS" ()
        )
      )

let rec find_static_path start_dir =
  let has_styles path = Sys.file_exists (Filename.concat path "css/styles.css") in
  let candidates = [ Filename.concat start_dir "static"; Filename.concat start_dir "ocaml/static" ] in
  match List.find_opt has_styles candidates with
  | Some p -> Some p
  | None ->
      let parent = Filename.dirname start_dir in
      if parent = start_dir then None else find_static_path parent

(* UTF-8 middleware for HTML responses *)
let utf8_middleware : Kirin.middleware = fun next_handler request ->
  let response = next_handler request in
  (match Kirin.response_header "Content-Type" response with
  | Some ct when String.starts_with ~prefix:"text/html" (String.lowercase_ascii ct) ->
      Kirin.with_header "Content-Type" "text/html; charset=utf-8" response
  | _ -> response)

let woman_win_redirect_middleware : Kirin.middleware = fun next_handler request ->
  match Kirin.header "Host" request with
  | Some host when Canonical_host.should_redirect_to_wkbl host ->
      let request_uri = Kirin.Request.uri request |> Uri.to_string in
      let location = Canonical_host.wkbl_location ~request_uri in
      Kirin.redirect ~status:`Permanent_redirect location
  | _ -> next_handler request

let () =
  (* Initialize RNG for HTTPS/TLS requests (Critical for Live Scraper) *)
  Mirage_crypto_rng_unix.use_default ();
  
  Printf.printf "Starting WKBL Server initialization...\n%!";
  (* Resolve runtime config from env. Prioritize WKBL_DATABASE_URL or DATABASE_URL for Postgres/Supabase. *)
  let db_url =
    match Sys.getenv_opt "WKBL_DATABASE_URL" with
    | Some url -> url
    | None -> (
        match Sys.getenv_opt "DATABASE_URL" with
        | Some url -> url
        | None ->
            let path = Sys.getenv_opt "WKBL_DB_PATH" |> Option.value ~default:Db.default_db_path in
            if String.starts_with ~prefix:"postgresql://" path || String.starts_with ~prefix:"postgres://" path || String.starts_with ~prefix:"sqlite3://" path then
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
  Printf.printf "Serving static assets from: %s\n%!" static_path;
  Printf.printf "Using DB path: %s\n%!" db_url;
  Printf.printf "Listening on: 0.0.0.0:%d\n%!" port;

  (* Run inside Eio context for DB pool initialization *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  
  (* Configure AI module to use the shared Eio context for HTTP calls (no shell-out). *)
  Ai.set_llm_ctx ~sw ~env;

  (* Initialize database pool inside Eio context *)
  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Error e ->
      Printf.eprintf "Failed to init DB (%s): %s\n" db_url (Db.show_db_error e);
      exit 1
  | Ok () -> ());

  (* Ensure optional analytics tables exist (e.g., player +/-). *)
  (match Db.ensure_schema () with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "Failed to ensure DB schema: %s\n" (Db.show_db_error e);
      exit 1);

  (* Ensure season_code -> season_name facts are correct in DB (UI dropdown labels rely on this). *)
  (match Scraper.ensure_seasons_catalog_in_db () with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "Failed to ensure seasons catalog: %s\n%!" (Db.show_db_error e));

  (* Background scheduler: sync schedule/metadata regularly with exponential backoff retry *)
  let one_hour = 60.0 *. 60.0 in
  let sync_interval = one_hour in
  let retry_intervals = [| 5.0 *. 60.0; 15.0 *. 60.0; 45.0 *. 60.0 |] in  (* 5m, 15m, 45m *)

  let sync_missing_boxscores ~season () =
    let take n xs =
      let rec loop acc n = function
        | [] -> List.rev acc
        | _ when n <= 0 -> List.rev acc
        | x :: rest -> loop (x :: acc) (n - 1) rest
      in
      loop [] n xs
    in
    let uniq_by_game_id (games : Db_sync.missing_boxscore_game list) =
      let seen = Hashtbl.create 256 in
      games
      |> List.filter (fun g ->
          if Hashtbl.mem seen g.Db_sync.game_id then false
          else (
            Hashtbl.add seen g.Db_sync.game_id ();
            true))
    in
    let missing_games =
      match Db_sync.get_games_missing_boxscore ~season () with
      | Ok xs -> xs
      | Error e ->
          Printf.eprintf "[Scheduler] boxscore missing query error: %s\n%!"
            (Db.show_db_error e);
          []
    in
    let mismatch_games =
      match Db_sync.get_games_score_mismatch ~season () with
      | Ok xs -> xs
      | Error _ -> []
    in
    let max_games = 12 in
    let games = uniq_by_game_id (missing_games @ mismatch_games) |> take max_games in
    let total = List.length games in
    if total = 0 then ()
    else (
      Printf.printf
        "[Scheduler] Syncing boxscores: %d missing + %d mismatch (running %d)\n%!"
        (List.length missing_games)
        (List.length mismatch_games)
        total;
      let clock = Eio.Stdenv.clock env in
      games |> List.iteri (fun i (g : Db_sync.missing_boxscore_game) ->
        Printf.printf "[Scheduler] [%d/%d] boxscore %s\n%!" (i + 1) total g.Db_sync.game_id;
        let tables =
          Scraper.fetch_game_boxscore
            ~sw ~env
            ~season_gu:g.Db_sync.season_code
            ~game_type:g.Db_sync.game_type
            ~game_no:g.Db_sync.game_no
            ~ym:g.Db_sync.ym
        in
        match tables with
        | [away_stats; home_stats] ->
            let upsert_team team_code stats =
              stats |> List.iter (fun s ->
                match Db_sync.upsert_game_stat s ~game_id:g.Db_sync.game_id ~team_code with
                | Ok () -> ()
                | Error e ->
                    Printf.eprintf "[Scheduler] boxscore upsert error (%s): %s\n%!"
                      g.Db_sync.game_id
                      (Db.show_db_error e))
            in
            upsert_team g.Db_sync.away_team_code away_stats;
            upsert_team g.Db_sync.home_team_code home_stats;
            let sum_pts stats =
              stats |> List.fold_left (fun acc s -> acc + s.Scraper.bs_pts) 0
            in
            let away_pts = sum_pts away_stats in
            let home_pts = sum_pts home_stats in
            (match Db_sync.update_game_scores_if_missing ~game_id:g.Db_sync.game_id ~away_score:away_pts ~home_score:home_pts with
            | Ok () -> ()
            | Error e ->
                Printf.eprintf "[Scheduler] score backfill error (%s): %s\n%!"
                  g.Db_sync.game_id
                  (Db.show_db_error e));
            Eio.Time.sleep clock 0.2
        | _ -> ()
      );
      ignore (Db.refresh_matviews ())
    )
  in

  let sync_missing_pbp ~season () =
    let take n xs =
      let rec loop acc n = function
        | [] -> List.rev acc
        | _ when n <= 0 -> List.rev acc
        | x :: rest -> loop (x :: acc) (n - 1) rest
      in
      loop [] n xs
    in
    let missing_games =
      match Db_sync.get_games_missing_pbp ~season () with
      | Ok xs -> xs
      | Error _ -> []
    in
    let max_games = 8 in
    let games = missing_games |> take max_games in
    let total = List.length games in
    if total = 0 then ()
    else (
      Printf.printf "[Scheduler] Syncing play-by-play for %d games (season=%s)\n%!" total season;
      let clock = Eio.Stdenv.clock env in
      games |> List.iteri (fun i (g : Db_sync.PbpSync.missing_pbp_game) ->
        Printf.printf "[Scheduler] [%d/%d] pbp %s\n%!" (i + 1) total g.game_id;
        try
          let events =
            Eio.Time.with_timeout_exn clock 10.0 (fun () ->
              Scraper.fetch_game_pbp_events ~sw ~env ~season_gu:g.season_code ~game_type:g.game_type ~game_no:g.game_no ())
          in
          if events <> [] then (
            let rows = events |> List.map (fun (e : Domain.pbp_event) -> (e, None)) in
            match Db_sync.replace_pbp_events ~game_id:g.game_id rows with
            | Ok _n -> ()
            | Error e ->
                Printf.eprintf "[Scheduler] pbp DB error (%s): %s\n%!" g.game_id (Db.show_db_error e)
          );
          Eio.Time.sleep clock 0.2
        with
        | Eio.Time.Timeout -> ()
        | exn ->
            Printf.eprintf "[Scheduler] pbp error (%s): %s\n%!" g.game_id (Printexc.to_string exn)
      )
    )
  in

  let sync_with_retry () : bool =
    let rec attempt n =
      Printf.printf "[Scheduler] Sync attempt %d...\n%!" (n + 1);
      let (schedule_synced, games_upserted, errors) =
        Scraper.sync_current_season_schedule ~sw ~env ()
      in
      if Scraper.schedule_sync_success ~schedule_synced ~games_upserted ~errors then (
        Printf.printf
          "[Scheduler] Sync successful: %d schedule, %d games, %d errors\n%!"
          schedule_synced
          games_upserted
          errors;
        true
      )
      else if n < Array.length retry_intervals then begin
        Printf.printf
          "[Scheduler] Sync failed (%d schedule, %d games, %d errors), retrying in %.0f minutes...\n%!"
          schedule_synced
          games_upserted
          errors
          (retry_intervals.(n) /. 60.0);
        Eio.Time.sleep env#clock retry_intervals.(n);
        attempt (n + 1)
      end else
        (Printf.eprintf "[Scheduler] Sync failed after %d retries, giving up until next cycle\n%!"
           (Array.length retry_intervals);
         false)
    in
    attempt 0
  in

  Eio.Fiber.fork ~sw (fun () ->
    Printf.printf "[Scheduler] Background sync started (%.0fm interval with retry)\n%!" (sync_interval /. 60.0);
    (* Initial sync on startup *)
    let ok = sync_with_retry () in
    if ok then (
      let season = Scraper.current_season_code_auto () |> Scraper.main_to_datalab in
      sync_missing_boxscores ~season ();
      sync_missing_pbp ~season ()
    );
    (* Periodic sync *)
    while true do
      Eio.Time.sleep env#clock sync_interval;
      Printf.printf "[Scheduler] Running scheduled sync...\n%!";
      let ok = sync_with_retry () in
      if ok then (
        let season = Scraper.current_season_code_auto () |> Scraper.main_to_datalab in
        sync_missing_boxscores ~season ();
        sync_missing_pbp ~season ()
      )
    done
  );

  (* Live Score Poller: 60s interval *)
  Eio.Fiber.fork ~sw (fun () ->
    Printf.printf "[Poller] Live score polling started (60s interval)\n%!";
    while true do
      (try
        let games = Scraper.fetch_live_games ~sw ~env () in
        Live.update_games games;
        if List.length games > 0 then begin
           Printf.printf "[Poller] Live games updated: %d games\n%!" (List.length games);
           Live.broadcast_scores ()
        end
       with e ->
        Printf.eprintf "[Poller] Error: %s\n%!" (Printexc.to_string e));
      Eio.Time.sleep env#clock 60.0
    done
  );

  Kirin.run ~config:{ Kirin.default_config with port } ~sw ~env
  @@ Kirin.logger
  @@ woman_win_redirect_middleware
  @@ utf8_middleware
  @@ Kirin.static "/static" ~dir:static_path
  @@ Kirin.router [

    (* PWA: manifest.json - serve from static folder *)
    Kirin.get "/manifest.json" (fun _ ->
      let manifest_path = Filename.concat static_path "manifest.json" in
      if Sys.file_exists manifest_path then
        let content = In_channel.with_open_text manifest_path In_channel.input_all in
        Kirin.with_header "Content-Type" "application/manifest+json"
        @@ Kirin.with_header "Cache-Control" "public, max-age=86400"
        @@ Kirin.text content
      else
        Kirin.not_found ~body:"manifest.json not found" ());

    (* PWA: service-worker.js - must be served from root for full scope *)
    Kirin.get "/sw.js" (fun _ ->
      let sw_path = Filename.concat static_path "js/service-worker.js" in
      if Sys.file_exists sw_path then
        let content = In_channel.with_open_text sw_path In_channel.input_all in
        Kirin.with_header "Content-Type" "application/javascript; charset=utf-8"
        @@ Kirin.with_header "Cache-Control" "no-cache"
        @@ Kirin.with_header "Service-Worker-Allowed" "/"
        @@ Kirin.text content
      else
        Kirin.not_found ~body:"service-worker.js not found" ());

    (* Share Cards: Player card PNG *)
    Kirin.get "/card/player/png/:id" (fun request ->
      let player_id = Kirin.param "id" request in
      match Db.get_player_aggregate_by_id ~player_id () with
      | Ok (Some p) ->
          let svg = Cards.player_card p in
          (match Cards.svg_to_png svg with
          | Some png ->
              Kirin.with_header "Content-Type" "image/png"
              @@ Kirin.with_header "Cache-Control" "public, max-age=3600"
              @@ Kirin.Response.make ~status:`OK (`String png)
          | None ->
              (* Fallback: return SVG if PNG conversion fails *)
              Kirin.with_header "Content-Type" "image/svg+xml"
              @@ Kirin.text svg)
      | Ok None -> Kirin.not_found ~body:"Player not found" ()
      | Error e -> Kirin.server_error ~body:(Db.show_db_error e) ());

    (* Share Cards: Player card SVG (for debugging/preview) *)
    Kirin.get "/card/player/svg/:id" (fun request ->
      let player_id = Kirin.param "id" request in
      match Db.get_player_aggregate_by_id ~player_id () with
      | Ok (Some p) ->
          Kirin.with_header "Content-Type" "image/svg+xml"
          @@ Kirin.with_header "Cache-Control" "public, max-age=3600"
          @@ Kirin.text (Cards.player_card p)
      | Ok None -> Kirin.not_found ~body:"Player not found" ()
      | Error e -> Kirin.server_error ~body:(Db.show_db_error e) ());

    (* SEO: robots.txt *)
    Kirin.get "/robots.txt" (fun _ ->
      Kirin.with_header "Content-Type" "text/plain; charset=utf-8"
      @@ Kirin.text {|User-agent: *
Allow: /
Disallow: /qa

Sitemap: https://wkbl.win/sitemap.xml
|});

    (* SEO: sitemap.xml - Dynamic sitemap *)
    Kirin.get "/sitemap.xml" (fun _ ->
      match Db.get_seasons (), Db.get_all_teams () with
      | Ok seasons, Ok teams ->
          let today = Unix.time () |> Unix.gmtime in
          let lastmod = Printf.sprintf "%04d-%02d-%02d" (today.Unix.tm_year + 1900) (today.Unix.tm_mon + 1) today.Unix.tm_mday in
          let static_pages = ["/"; "/players"; "/teams"; "/standings"; "/boxscores"; "/games"; "/leaders"; "/awards"; "/compare"; "/predict"; "/transactions"] in
          let static_urls = static_pages |> List.map (fun path ->
            Printf.sprintf {|  <url><loc>https://wkbl.win%s</loc><lastmod>%s</lastmod><changefreq>daily</changefreq><priority>%s</priority></url>|}
              path lastmod (if path = "/" then "1.0" else "0.8")) |> String.concat "\n" in
          let team_urls = teams |> List.map (fun (t: team_info) ->
            Printf.sprintf {|  <url><loc>https://wkbl.win/team/%s</loc><lastmod>%s</lastmod><changefreq>weekly</changefreq><priority>0.7</priority></url>|}
              t.team_code lastmod) |> String.concat "\n" in
          let season_urls = seasons |> List.map (fun (s: season_info) ->
            Printf.sprintf {|  <url><loc>https://wkbl.win/standings?season=%s</loc><lastmod>%s</lastmod><changefreq>weekly</changefreq><priority>0.6</priority></url>|}
              s.code lastmod) |> String.concat "\n" in
          let sitemap = Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
%s
%s
%s
</urlset>|} static_urls team_urls season_urls in
          Kirin.with_header "Content-Type" "application/xml; charset=utf-8" @@ Kirin.text sitemap
      | _ -> Kirin.server_error ~body:"Failed to generate sitemap" ());

    (* Home Page *)
    Kirin.get "/" (fun request ->
      let lang = request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~limit:20 () with
          | Ok p ->
              let player_info_map = get_player_info_map () in
              (* Home "today games" should not look empty on cold start.
                 Prefer live poller data, but fall back to today's schedule from DB. *)
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
      let lang = request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~limit:20 () with
          | Ok p ->
              let player_info_map = get_player_info_map () in
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
    let next = redirect_back_or_home request in
    Kirin.with_header "Set-Cookie" (I18n.set_cookie_header lang)
    @@ Kirin.redirect next
  );

    (* Players List & Table HTMX *)
    Kirin.get "/players" (fun request ->
      let lang = request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~sort ~include_mismatch () with
          | Ok p ->
              let player_info_map = get_player_info_map () in
              backfill_duplicate_player_meta ~sw ~env ~players:p player_info_map;
              Kirin.html (Views.players_page ~lang ~player_info_map ~season ~seasons ~search ~sort:sort_str ~include_mismatch p)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    Kirin.get "/players/table" (fun request ->
      let lang = request_lang request in
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~sort ~include_mismatch () with
          | Ok p ->
              let player_info_map = get_player_info_map () in
              backfill_duplicate_player_meta ~sw ~env ~players:p player_info_map;
              Kirin.html (Views.players_table ~lang ~player_info_map p)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Teams Page & Table HTMX *)
    Kirin.get "/teams" (fun request ->
      let lang = request_lang request in
      let scope_str = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_team_stats ~season ~scope ~sort ~include_mismatch () with
          | Ok stats -> Kirin.html (Views.teams_page ~lang ~season ~seasons ~scope ~sort:sort_str ~include_mismatch stats)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    Kirin.get "/teams/table" (fun request ->
      let lang = request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let scope_str = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope ~sort ~include_mismatch () with
      | Ok stats -> Kirin.html (Views.teams_table ~lang ~season ~scope stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Team Shooting Comparison Chart - HTMX Partial *)
    Kirin.get "/teams/shooting-chart" (fun request ->
      let lang = request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope:Domain.PerGame ~sort:Domain.TeamByPoints ~include_mismatch () with
      | Ok stats -> Kirin.html (Views_charts.team_shooting_comparison stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Team Radar Chart - HTMX Partial *)
    Kirin.get "/teams/radar-chart" (fun request ->
      let lang = request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope:Domain.PerGame ~sort:Domain.TeamByEfficiency ~include_mismatch () with
      | Ok stats -> Kirin.html (Views_charts.team_radar_chart stats)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Standings *)
    Kirin.get "/standings" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_standings ~season () with
          | Ok standings -> Kirin.html (Views.standings_page ~lang ~season ~seasons standings)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    Kirin.get "/standings/table" (fun request ->
      let lang = request_lang request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_standings ~season () with
      | Ok standings -> Kirin.html (Views.standings_table ~lang ~season standings)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

		    (* Games & Boxscores List *)
		    Kirin.get "/games" (fun request ->
          let lang = request_lang request in
		      let page = Kirin.query_opt "page" request |> Option.map int_of_string |> Option.value ~default:1 in
		      match Db.get_seasons () with
		      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
		      | Ok seasons ->
		          let season = query_season_or_latest request seasons in
		          (match Db.get_games ~season ~page () with
		          | Ok games -> Kirin.html (Views.games_page ~lang ~page ~season ~seasons games)
		          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
		    );

		    Kirin.get "/games/table" (fun request ->
          let lang = request_lang request in
		      match Db.get_seasons () with
		      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
		      | Ok seasons ->
		          let season = query_season_or_latest request seasons in
		          (match Db.get_games ~season () with
		          | Ok games -> Kirin.html (Views.games_table ~lang games)
		          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
		    );

	    (* Boxscores List *)
	    Kirin.get "/boxscores" (fun request ->
	      let lang = request_lang request in
	      match Db.get_seasons () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok seasons ->
	          let season = query_season_or_latest request seasons in
	          (match Db.get_scored_games ~season ~include_mismatch:true () with
	          | Ok games -> Kirin.html (Views.boxscores_page ~lang ~season ~seasons games)
	          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
	    );

	    Kirin.get "/boxscores/table" (fun request ->
	      let lang = request_lang request in
	      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
	      match Db.get_scored_games ~season ~include_mismatch:true () with
	      | Ok games -> Kirin.html (Views.boxscores_table ~lang games)
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	    );

    (* Play-by-Play (PBP) Detail - MUST come before /boxscore/:id *)
	    Kirin.get "/boxscore/:id/pbp" (fun request ->
	      let lang = request_lang request in
	      let game_id = Kirin.param "id" request in
	      let period_opt = query_nonempty request "period" in
	      match Db.get_game_info ~game_id () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok None -> Kirin.html (Views.error_page ~lang "Game not found")
	      | Ok (Some game) ->
	          let today_kst = Live.today_str () in
	          let periods_res =
	            match Db.get_pbp_periods ~game_id () with
	            | Ok periods as ok ->
	                if pbp_should_backfill ~today_kst game periods then
	                  Pbp_backfill.maybe_spawn ~sw ~env ~game_id ~tag:"PBP" ();
	                ok
	            | other -> other
	          in

	          (match periods_res with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok periods ->
              let selected_period =
                match period_opt with
                | Some p when List.mem p periods -> p
                | _ -> (match periods with | p :: _ -> p | [] -> "Q1")
              in
              let events_res =
                match periods with
                | [] -> Ok []
                | _ -> Db.get_pbp_events ~game_id ~period_code:selected_period ()
              in
              match events_res with
              | Ok events -> Kirin.html (Views.pbp_page ~lang ~game ~periods ~selected_period ~events ())
              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
    );

    (* AI Game Summary *)
    Kirin.get "/boxscore/:id/summary" (fun request ->
      let game_id = Kirin.param "id" request in
      match Db.get_boxscore ~game_id () with
      | Error e -> Kirin.json_string (Printf.sprintf {|{"error": "%s"}|} (Db.show_db_error e))
      | Ok bs ->
          let game = bs.Domain.boxscore_game in
          let home_players = bs.Domain.boxscore_home_players in
          let away_players = bs.Domain.boxscore_away_players in
          (* Calculate EFF: PTS + REB + AST + STL + BLK - (FGA-FGM) - (FTA-FTM) - TOV *)
          let calc_eff (p : Domain.boxscore_player_stat) =
            float_of_int (p.bs_pts + p.bs_reb + p.bs_ast + p.bs_stl + p.bs_blk
              - (p.bs_fg_att - p.bs_fg_made) - (p.bs_ft_att - p.bs_ft_made) - p.bs_tov)
          in
          (* Find MVP: player with highest EFF *)
          let find_mvp players =
            List.fold_left (fun best p ->
              let eff = calc_eff p in
              match best with
              | None -> Some (p, eff)
              | Some (_, best_eff) -> if eff > best_eff then Some (p, eff) else best
            ) None players
          in
          let home_mvp = find_mvp home_players in
          let away_mvp = find_mvp away_players in
          let mvp = match home_mvp, away_mvp with
            | Some (h, h_eff), Some (_, a_eff) when h_eff >= a_eff -> Some (h, h_eff, true)
            | Some _, Some (a, a_eff) -> Some (a, a_eff, false)
            | Some (h, h_eff), None -> Some (h, h_eff, true)
            | None, Some (a, a_eff) -> Some (a, a_eff, false)
            | None, None -> None
          in
          (* Generate summary *)
          let winner, loser, w_score, l_score =
            if game.gi_home_score > game.gi_away_score then
              (game.gi_home_team_name, game.gi_away_team_name, game.gi_home_score, game.gi_away_score)
            else
              (game.gi_away_team_name, game.gi_home_team_name, game.gi_away_score, game.gi_home_score)
          in
          let margin = abs (game.gi_home_score - game.gi_away_score) in
          let game_desc =
            if margin >= 20 then "압도적인 경기력으로"
            else if margin >= 10 then "안정적인 경기 운영으로"
            else if margin >= 5 then "치열한 접전 끝에"
            else "손에 땀을 쥐게 하는 초접전 끝에"
          in
          let mvp_desc = match mvp with
            | Some (p, _, is_home) ->
                Printf.sprintf "%s(%s)가 %d득점 %d리바운드 %d어시스트로 맹활약했습니다."
                  p.bs_player_name
                  (if is_home then game.gi_home_team_name else game.gi_away_team_name)
                  p.bs_pts p.bs_reb p.bs_ast
            | None -> ""
          in
          let summary = Printf.sprintf
            "%s이(가) %s %d-%d로 %s을(를) 꺾었습니다. %s"
            winner game_desc w_score l_score loser mvp_desc
          in
          let json = `Assoc [
            ("summary", `String summary);
            ("winner", `String winner);
            ("loser", `String loser);
            ("margin", `Int margin);
            ("mvp", match mvp with
              | Some (p, eff, _) -> `Assoc [
                  ("name", `String p.bs_player_name);
                  ("pts", `Int p.bs_pts);
                  ("reb", `Int p.bs_reb);
                  ("ast", `Int p.bs_ast);
                  ("eff", `Float eff);
                ]
              | None -> `Null);
          ] in
          Kirin.json_string (Yojson.Basic.to_string json)
    );

    (* Game Flow Chart *)
	    Kirin.get "/boxscore/:id/flow" (fun request ->
	      let lang = request_lang request in
	      let game_id = Kirin.param "id" request in
	      match Db.get_game_info ~game_id () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok None -> Kirin.html (Views.error_page ~lang "Game not found")
	      | Ok (Some game) ->
	          let today_kst = Live.today_str () in
	          let periods_res =
	            match Db.get_pbp_periods ~game_id () with
	            | Ok periods as ok ->
	                if pbp_should_backfill ~today_kst game periods then
	                  Pbp_backfill.maybe_spawn ~sw ~env ~game_id ~tag:"FLOW" ();
	                ok
	            | other -> other
	          in

	          (match periods_res with
	          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	          | Ok periods ->
	              let fetch_flow_points (ps : string list) =
	                let chunks =
	                  ps
	                  |> List.filter_map (fun period_code ->
	                      match Db.get_pbp_events ~game_id ~period_code () with
	                      | Ok events -> Some events
	                      | Error _ -> None)
	                in
	                Domain.extract_score_flow (List.concat chunks)
	              in

	              let flow_points = fetch_flow_points periods in

	              (* If the flow doesn't reach the final score for a past game,
	                 schedule a background refresh (never block the HTTP response). *)
		              let scores_known = game.gi_home_score > 0 && game.gi_away_score > 0 in
		              let is_not_future_game =
		                let d = String.trim game.gi_game_date in
		                d <> "" && String.compare d today_kst <= 0
		              in
	              if scores_known && is_not_future_game
	                 && not (score_flow_matches_final ~final_home:game.gi_home_score ~final_away:game.gi_away_score flow_points)
	              then Pbp_backfill.maybe_spawn ~sw ~env ~game_id ~tag:"FLOW" ();

	              Kirin.html (Views_tools.game_flow_page ~lang ~game flow_points))
	    );

	    (* Boxscore Detail - MUST come AFTER more specific /boxscore/:id/* routes *)
	    Kirin.get "/boxscore/:id" (fun request ->
	      let lang = request_lang request in
	      let game_id = Kirin.param "id" request in
	      match Db.get_boxscore ~game_id () with
	      | Ok bs ->
	          let today_kst = Live.today_str () in
	          (* If PBP/quarter scores are clearly incomplete, refresh once so the boxscore page doesn't
	             show nonsense quarter flow. Never block the HTTP response. *)
	          let g = bs.boxscore_game in
		          let periods =
		            match Db.get_pbp_periods ~game_id () with
		            | Ok ps -> ps
		            | Error _ -> []
		          in
		          let scores_known = g.gi_home_score > 0 && g.gi_away_score > 0 in
		          let is_not_future_game =
		            let d = String.trim g.gi_game_date in
		            d <> "" && String.compare d today_kst <= 0
		          in
		          let q4_mismatch =
		            if scores_known && is_not_future_game then (
		              match Db.get_quarter_scores game_id with
		              | Error _ -> false
		              | Ok qs ->
		                  match List.find_opt (fun (q : Domain.quarter_score) -> q.qs_period = "Q4") qs with
		                  | None -> false
	                  | Some q4 ->
	                      q4.qs_home_score <> g.gi_home_score || q4.qs_away_score <> g.gi_away_score
	            ) else false
	          in
	          if pbp_should_backfill ~today_kst g periods || q4_mismatch then
	            Pbp_backfill.maybe_spawn ~sw ~env ~game_id ~tag:"BOX" ();
	          Kirin.html (Views.boxscore_page ~lang bs)
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	    );

    (* Leaders *)
    Kirin.get "/leaders" (fun request ->
      let lang = request_lang request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
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
            | [] -> Ok (List.rev acc)
            | category :: rest ->
                match Db.get_leaders ~season ~scope category with
                | Error e -> Error e
                | Ok leaders -> fetch_all ((category, leaders) :: acc) rest
          in
          match fetch_all [] categories with
          | Ok leaders_by_category ->
              let player_info_map = get_player_info_map () in
              Kirin.html (Views.leaders_page ~lang ~player_info_map ~season ~seasons ~scope leaders_by_category)
          | Error e ->
              Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Position-based Leaders *)
    Kirin.get "/leaders/by-position" (fun request ->
      let lang = request_lang request in
      let position = Kirin.query_opt "position" request |> Option.value ~default:"ALL" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let stats = ["pts"; "reb"; "ast"; "eff"] in
          let rec fetch_all acc = function
            | [] -> Ok (List.rev acc)
            | stat :: rest ->
                match Db.get_leaders_by_position ~season ~position stat with
                | Error e -> Error e
                | Ok leaders -> fetch_all ((stat, leaders) :: acc) rest
          in
          match fetch_all [] stats with
          | Ok leaders ->
              let player_info_map = get_player_info_map () in
              Kirin.html (Views.position_leaders_page ~lang ~player_info_map ~season ~seasons ~position leaders)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Clutch Time Leaders *)
    Kirin.get "/clutch" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_clutch_stats ~season () with
          | Ok stats ->
              let sorted_stats = List.sort
                (fun (a: clutch_stats) (b: clutch_stats) ->
                  compare b.cs_clutch_points a.cs_clutch_points)
                stats
              in
              Kirin.html (Views.clutch_page ~lang ~season ~seasons sorted_stats)
          | Error e ->
              Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Lineup Chemistry - Full Page *)
    Kirin.get "/lineups" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons (), Db.get_all_teams () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let season = query_season_or_latest request seasons in
          let team = Kirin.query_opt "team" request |> Option.value ~default:"ALL" in
          match Db.get_lineup_chemistry ~season ~team_name:team () with
          | Ok chemistry ->
              Kirin.html (Views_tools.lineup_chemistry_page
                ~lang ~teams ~seasons ~selected_team:team ~selected_season:season chemistry)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Lineup Chemistry - Table Content (HTMX partial) *)
    Kirin.get "/lineups/table" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let team = Kirin.query_opt "team" request |> Option.value ~default:"ALL" in
          match Db.get_lineup_chemistry ~season ~team_name:team () with
          | Ok chemistry ->
              Kirin.html (Views_tools.lineup_chemistry_table_content chemistry)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Awards (Stat-based, unofficial) *)
    Kirin.get "/awards" (fun request ->
      let lang = request_lang request in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
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
          let mvp_res = Db.get_stat_mvp_eff ~season ~include_mismatch () in
          let mip_res =
            match prev_season_code with
            | None -> Ok []
            | Some prev_season -> Db.get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch ()
          in
          match mvp_res, mip_res with
          | Ok mvp, Ok mip ->
              let player_info_map = get_player_info_map () in
              Kirin.html (Views.awards_page ~lang ~player_info_map ~season ~seasons ~include_mismatch ~prev_season_name ~mvp ~mip ())
          | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* MVP Race - Full Page *)
    Kirin.get "/mvp-race" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_mvp_race ~season () with
          | Ok candidates -> Kirin.html (Views_mvp.mvp_race_page ~lang ~season ~seasons candidates)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* MVP Race - Table HTMX *)
    Kirin.get "/mvp-race/table" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_mvp_race ~season () with
          | Ok candidates -> Kirin.html (Views_mvp.mvp_race_table candidates)
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Fantasy Calculator - Full Page *)
    Kirin.get "/fantasy" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let pts = Option.bind (Kirin.query_opt "pts" request) float_of_string_opt |> Option.value ~default:1.0 in
          let reb = Option.bind (Kirin.query_opt "reb" request) float_of_string_opt |> Option.value ~default:1.2 in
          let ast = Option.bind (Kirin.query_opt "ast" request) float_of_string_opt |> Option.value ~default:1.5 in
          let stl = Option.bind (Kirin.query_opt "stl" request) float_of_string_opt |> Option.value ~default:2.0 in
          let blk = Option.bind (Kirin.query_opt "blk" request) float_of_string_opt |> Option.value ~default:2.0 in
          let tov = Option.bind (Kirin.query_opt "tov" request) float_of_string_opt |> Option.value ~default:(-1.0) in
          let rules : Domain.fantasy_scoring_rule = {
            fsr_points = pts;
            fsr_rebounds = reb;
            fsr_assists = ast;
            fsr_steals = stl;
            fsr_blocks = blk;
            fsr_turnovers = tov;
          } in
          match Db.get_players ~season ~search:"" ~sort:ByMinutes () with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok players ->
              let scores = List.map (Domain.fantasy_score_of_aggregate ~rules) players in
              Kirin.html (Views_tools.fantasy_calculator_page ~lang ~season ~seasons ~rules ~scores ())
    );

    (* Fantasy Calculator - HTMX Calculate Endpoint *)
    Kirin.get "/fantasy/calculate" (fun request ->
      let lang = request_lang request in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let pts = Option.bind (Kirin.query_opt "pts" request) float_of_string_opt |> Option.value ~default:1.0 in
          let reb = Option.bind (Kirin.query_opt "reb" request) float_of_string_opt |> Option.value ~default:1.2 in
          let ast = Option.bind (Kirin.query_opt "ast" request) float_of_string_opt |> Option.value ~default:1.5 in
          let stl = Option.bind (Kirin.query_opt "stl" request) float_of_string_opt |> Option.value ~default:2.0 in
          let blk = Option.bind (Kirin.query_opt "blk" request) float_of_string_opt |> Option.value ~default:2.0 in
          let tov = Option.bind (Kirin.query_opt "tov" request) float_of_string_opt |> Option.value ~default:(-1.0) in
          let rules : Domain.fantasy_scoring_rule = {
            fsr_points = pts;
            fsr_rebounds = reb;
            fsr_assists = ast;
            fsr_steals = stl;
            fsr_blocks = blk;
            fsr_turnovers = tov;
          } in
          match Db.get_players ~season ~search:"" ~sort:ByMinutes () with
          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
          | Ok players ->
              let scores = List.map (Domain.fantasy_score_of_aggregate ~rules) players in
              Kirin.html (Views_tools.fantasy_results_table scores)
    );

    (* Compare - HTMX table fragment *)
    Kirin.get "/compare/table" (fun request ->
      let lang = request_lang request in
      let compare_type =
        Kirin.query_opt "type" request
        |> Option.map String.lowercase_ascii
        |> Option.value ~default:"player"
      in
      let left = query_nonempty request "left" |> Option.value ~default:"" in
      let right = query_nonempty request "right" |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          if left = "" || right = "" then
            Kirin.html (Views.compare_table_empty ())
          else
            match compare_type with
            | "team" -> (
                match Db.get_team_stats ~season ~scope:PerGame ~sort:TeamByPoints () with
                | Error _ -> Kirin.html (Views.compare_table_empty ())
                | Ok stats ->
                    let code_of name = team_code_of_string name in
                    let find_by_code code =
                      match code with
                      | None -> None
                      | Some c ->
                          List.find_opt (fun (row: team_stats) -> code_of row.team = Some c) stats
                    in
                    let left_row = find_by_code (code_of left) in
                    let right_row = find_by_code (code_of right) in
                    (match left_row, right_row with
                    | Some a, Some b ->
                        let rows = [
                          ("PTS", a.pts, b.pts, false);
                          ("MG", a.margin, b.margin, true);
                          ("REB", a.reb, b.reb, false);
                          ("AST", a.ast, b.ast, false);
                          ("STL", a.stl, b.stl, false);
                          ("BLK", a.blk, b.blk, false);
                          ("TO", a.turnovers, b.turnovers, false);
                          ("EFF", a.eff, b.eff, false);
                        ] in
                        Kirin.html
                          (Views.compare_table_fragment
                             ~left_label:(Views_common.normalize_name a.team)
                             ~right_label:(Views_common.normalize_name b.team)
                             rows)
                    | _ -> Kirin.html (Views.compare_table_empty ())))
            | _ -> (
                match Db.get_player_aggregate_by_id ~player_id:left ~season (),
                      Db.get_player_aggregate_by_id ~player_id:right ~season () with
                | Ok (Some a), Ok (Some b) ->
                    let rows = [
                      ("PTS", a.avg_points, b.avg_points, false);
                      ("MG", a.avg_margin, b.avg_margin, true);
                      ("REB", a.avg_rebounds, b.avg_rebounds, false);
                      ("AST", a.avg_assists, b.avg_assists, false);
                      ("STL", a.avg_steals, b.avg_steals, false);
                      ("BLK", a.avg_blocks, b.avg_blocks, false);
                      ("TO", a.avg_turnovers, b.avg_turnovers, false);
                      ("EFF", a.efficiency, b.efficiency, false);
                    ] in
                    Kirin.html
                      (Views.compare_table_fragment
                         ~left_label:(Views_common.normalize_name a.name)
                         ~right_label:(Views_common.normalize_name b.name)
                         rows)
                | _ -> Kirin.html (Views.compare_table_empty ()))
    );

    (* Compare Seasons - Compare a player's performance across different seasons *)
    Kirin.get "/compare/seasons" (fun request ->
      let lang = request_lang request in
      let player_id = Kirin.query_opt "player" request |> Option.value ~default:"" in
      let s1 = Kirin.query_opt "s1" request |> Option.value ~default:"" in
      let s2 = Kirin.query_opt "s2" request |> Option.value ~default:"" in

      if String.trim player_id = "" then
        Kirin.redirect "/compare"
      else
        match Db.get_seasons () with
        | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
        | Ok seasons ->
            (* Get player profile to get name *)
            (match Db.get_player_profile ~player_id () with
            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
            | Ok None -> Kirin.html (Views.error_page ~lang "Player not found")
            | Ok (Some profile) ->
                let player_name = profile.player.name in
                (* Get all seasons for this player *)
                (match Db.get_player_season_stats ~player_id ~scope:"per_game" () with
                | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
                | Ok all_seasons ->
                    let find_season code =
                      List.find_opt (fun (s: season_stats) -> s.ss_season_code = code) all_seasons
                    in
                    let s1_stats = if s1 <> "" then find_season s1 else None in
                    let s2_stats = if s2 <> "" then find_season s2 else None in
                    let error =
                      if s1 <> "" && s2 <> "" && s1 = s2 then Some "Please select two different seasons."
                      else if s1 <> "" && s1_stats = None then Some (Printf.sprintf "Season %s not found for this player." s1)
                      else if s2 <> "" && s2_stats = None then Some (Printf.sprintf "Season %s not found for this player." s2)
                      else None
                    in
                    Kirin.html (Views.compare_seasons_page
                      ~lang
                      ~seasons
                      ~player_id
                      ~player_name
                      ~s1
                      ~s2
                      ~s1_stats
	                      ~s2_stats
	                      ~all_seasons
	                      ~error
	                      ())))
	    );

    (* Compare - simplified version *)
    Kirin.get "/compare" (fun request ->
      let lang = request_lang request in
      let p1_query = Kirin.query_opt "p1" request |> Option.value ~default:"" in
      let p2_query = Kirin.query_opt "p2" request |> Option.value ~default:"" in
      let p1_id = Kirin.query_opt "p1_id" request |> Option.value ~default:"" in
      let p2_id = Kirin.query_opt "p2_id" request |> Option.value ~default:"" in
      let p1_id_opt = if String.trim p1_id = "" then None else Some (String.trim p1_id) in
      let p2_id_opt = if String.trim p2_id = "" then None else Some (String.trim p2_id) in

      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
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

          let p1_sel_res =
            match p1_id_opt with
            | None -> Ok None
            | Some pid -> Db.get_player_aggregate_by_id ~player_id:pid ~season:p1_season ()
          in
          let p2_sel_res =
            match p2_id_opt with
            | None -> Ok None
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

          let p1_candidates_res =
            if p1_selected = None && String.trim p1_query <> "" then
              let limit = if p1_id_opt <> None then 30 else 8 in
              Db.get_players ~season:p1_season ~search:p1_query ~sort:ByMinutes ~limit ()
            else
              Ok []
          in
          let p2_candidates_res =
            if p2_selected = None && String.trim p2_query <> "" then
              let limit = if p2_id_opt <> None then 30 else 8 in
              Db.get_players ~season:p2_season ~search:p2_query ~sort:ByMinutes ~limit ()
            else
              Ok []
          in
          let p1_candidates = match p1_candidates_res with Ok xs -> xs | Error _ -> [] in
          let p2_candidates = match p2_candidates_res with Ok xs -> xs | Error _ -> [] in

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

          let p1_available_seasons =
            match p1_id_opt, p1_selected with
            | Some pid, None -> (
                match Db.get_player_season_stats ~player_id:pid ~scope:"per_game" () with
                | Ok stats ->
                    let codes =
                      stats
                      |> List.map (fun (s: season_stats) -> s.ss_season_code)
                      |> List.sort_uniq String.compare
                    in
                    Some codes
                | Error _ -> None
              )
            | _ -> None
          in
          let p2_available_seasons =
            match p2_id_opt, p2_selected with
            | Some pid, None -> (
                match Db.get_player_season_stats ~player_id:pid ~scope:"per_game" () with
                | Ok stats ->
                    let codes =
                      stats
                      |> List.map (fun (s: season_stats) -> s.ss_season_code)
                      |> List.sort_uniq String.compare
                    in
                    Some codes
                | Error _ -> None
              )
            | _ -> None
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
          let h2h_res =
            match p1_selected, p2_selected, h2h_disabled_reason with
            | Some a, Some b, None ->
                Db.get_player_h2h_data ~p1_id:a.player_id ~p2_id:b.player_id ~season:p1_season ()
            | _ -> Ok []
          in
          let h2h = match h2h_res with Ok h -> h | Error _ -> [] in
          let p1_season_history =
            match p1_selected with
            | Some p -> (
                match Db.get_player_season_stats ~player_id:p.player_id ~scope:"per_game" () with
                | Ok stats -> stats
                | Error _ -> [])
            | None -> []
          in
          let p2_season_history =
            match p2_selected with
            | Some p -> (
                match Db.get_player_season_stats ~player_id:p.player_id ~scope:"per_game" () with
                | Ok stats -> stats
                | Error _ -> [])
            | None -> []
          in
          let error_opt =
            match List.rev !errors with
            | [] -> None
            | xs -> Some (String.concat " / " xs)
          in
          Kirin.html
            (Views.compare_page
               ~lang
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
               ~p1_season_history
               ~p2_season_history
               p1_selected
               p2_selected
               h2h)
    );

    (* Predict (Team vs Team) *)
    Kirin.get "/predict" (fun request ->
      let lang = request_lang request in
      let home = Kirin.query_opt "home" request |> Option.value ~default:"" in
      let away = Kirin.query_opt "away" request |> Option.value ~default:"" in
      let include_mismatch = query_bool request "include_mismatch" in
      let context_enabled =
        Kirin.query_opt "context" request
        |> Option.map String.lowercase_ascii
        |> function
        | Some ("1" | "true" | "yes" | "on") -> true
        | _ -> false
      in
      let is_neutral =
        Kirin.query_opt "neutral" request
        |> Option.map String.lowercase_ascii
        |> function
        | Some ("1" | "true" | "yes" | "on") -> true
        | _ -> false
      in

      match Db.get_seasons (), Db.get_all_teams () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let upcoming = match Db.get_upcoming_schedule ~status:"scheduled" ~limit:6 () with Ok u -> u | Error _ -> [] in
          let team_names = List.map (fun (t: team_info) -> t.team_name) teams in
          let season = query_season_or_latest request seasons in
          let render result error =
            Kirin.html (Views.predict_page ~lang ~season ~seasons ~teams:team_names ~home ~away ~is_neutral ~context_enabled ~include_mismatch ~upcoming ~games:(match Db.get_scored_games ~season ~include_mismatch () with Ok g -> g | _ -> []) result error)
          in

          if String.trim home = "" || String.trim away = "" then
            render None None
          else if normalize_label home = normalize_label away then
            render None (Some "홈 팀과 원정 팀은 달라야 합니다.")
          else
            match Db.get_team_stats ~season ~scope:Totals ~include_mismatch (), Db.get_scored_games ~season ~include_mismatch () with
            | Ok totals, Ok games ->
                let find_team (name : string) =
                  let key = normalize_label name in
                  let key_without_city = 
                    if String.starts_with ~prefix:"아산" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"청주" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"인천" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"용인" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"부천" key then String.sub key 6 (String.length key - 6)
                    else if String.starts_with ~prefix:"부산" key then String.sub key 6 (String.length key - 6)
                    else key
                  in
                  match List.find_opt (fun (row : team_stats) -> normalize_label row.team = key) totals with
                  | Some t -> Some t
                  | None -> 
                      List.find_opt (fun (row : team_stats) -> 
                        let row_key = normalize_label row.team in
                        row_key = key_without_city
                      ) totals
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
                (match find_team home, find_team away with
                | Some home_row, Some away_row ->
                    let output =
                      Prediction.predict_match_nerd
                        ~context:None
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
	            | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

    (* Player Profile *)
    Kirin.get "/player/:id" (fun request ->
      let lang = request_lang request in
      let player_id = Kirin.param "id" request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let show_ops = query_bool request "ops" in
      match Db.get_player_profile ~player_id () with
      | Ok (Some profile) ->
          let canon_id = profile.player.id in
          (match
             Player_identity.redirect_location ~requested_id:player_id ~canonical_id:canon_id
               (Kirin.Request.uri request)
           with
          | Some location -> Kirin.redirect ~status:`Permanent_redirect location
          | None ->
              let final_profile =
                if scope = "per_game" then profile
                else
                  match Db.get_player_season_stats ~player_id ~scope () with
                  | Ok stats -> { profile with season_breakdown = stats }
                  | Error _ -> profile
              in
              let seasons_catalog =
                match Db.get_seasons () with
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
                |> List.find_opt (fun (s : season_info) -> s.code = season_for_leaderboards)
                |> Option.map (fun (s : season_info) -> s.name)
                |> Option.value ~default:season_for_leaderboards
              in
              let leaderboard_categories =
                match String.lowercase_ascii scope with
                | "totals" ->
                    [
                      "gp";
                      "min";
                      "pts";
                      "reb";
                      "ast";
                      "stl";
                      "blk";
                      "tov";
                      "fg_pct";
                      "fg3_pct";
                      "ft_pct";
                      "ts_pct";
                      "efg_pct";
                    ]
                | "per_36" ->
                    [
                      "pts";
                      "reb";
                      "ast";
                      "stl";
                      "blk";
                      "tov";
                      "eff";
                      "fg_pct";
                      "fg3_pct";
                      "ft_pct";
                      "ts_pct";
                      "efg_pct";
                    ]
                | _ ->
                    [
                      "pts";
                      "reb";
                      "ast";
                      "stl";
                      "blk";
                      "tov";
                      "min";
                      "eff";
                      "fg_pct";
                      "fg3_pct";
                      "ft_pct";
                      "ts_pct";
                      "efg_pct";
                    ]
              in
              let rec fetch_all acc = function
                | [] -> Ok (List.rev acc)
                | category :: rest -> (
                    match Db.get_leaders ~season:season_for_leaderboards ~scope category with
                    | Error e -> Error e
                    | Ok leaders -> fetch_all ((category, leaders) :: acc) rest)
              in
              let leaderboards =
                match fetch_all [] leaderboard_categories with
                | Ok leaders_by_category ->
                    Some (season_for_leaderboards, season_name_for_leaderboards, leaders_by_category)
                | Error _ -> None
              in
              Kirin.html
                (Views_player.player_profile_page ~lang ~leaderboards ~show_ops final_profile ~scope
                   ~seasons_catalog))
      | Ok None -> Kirin.html (Views.error_page ~lang "Player not found")
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

	    (* Player Game Log *)
	    Kirin.get "/player/:id/games" (fun request ->
	      let lang = request_lang request in
	      let player_id = Kirin.param "id" request in
	      let include_mismatch = query_bool request "include_mismatch" in
	      match Db.get_player_profile ~player_id (), Db.get_seasons () with
	      | Ok None, _ -> Kirin.html (Views.error_page ~lang "Player not found")
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
      | Ok (Some profile), Ok seasons ->
          let canon_id = profile.player.id in
          match
            Player_identity.redirect_location ~requested_id:player_id ~canonical_id:canon_id
              ~suffix:"games" (Kirin.Request.uri request)
          with
	          | Some location -> Kirin.redirect ~status:`Permanent_redirect location
	          | None ->
	              let season = query_season_or_latest request seasons in
	              match Db.get_player_game_logs ~player_id ~season ~include_mismatch () with
		              | Ok games ->
		                  Kirin.html
		                    (Views_player.player_game_logs_page ~lang profile ~season ~seasons
		                       ~include_mismatch games)
		              | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
		    );

	    (* Player Season Stats HTMX Partial *)
	    Kirin.get "/player/:id/season-stats" (fun request ->
	      let lang = request_lang request in
	      let player_id = Kirin.param "id" request in
	      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
	      match Db.get_player_season_stats ~player_id ~scope () with
	      | Ok stats -> Kirin.html (Views_common.player_season_stats_component ~player_id ~scope stats)
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	    );

	    (* Player Shot Chart HTMX Partial *)
	    Kirin.get "/player/:id/shot-chart" (fun request ->
	      let lang = request_lang request in
	      let player_id = Kirin.param "id" request in
	      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
	      match Db.get_player_shooting_stats ~player_id ~season () with
	      | Ok (Some stats) -> Kirin.html (Views_charts.player_shot_chart_html stats)
	      | Ok None -> Kirin.html {|<div class="text-slate-400 text-center p-4">슈팅 데이터가 없습니다.</div>|}
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	    );

	    (* Player Zone Shot Chart (PBP-based) - Full Page *)
	    Kirin.get "/player/:id/shots" (fun request ->
	      let lang = request_lang request in
	      let player_id = Kirin.param "id" request in
	      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
	      let is_htmx = Kirin.header "HX-Request" request |> Option.is_some in
	      match Db.get_player_shot_chart ~player_id ~season () with
	      | Ok chart ->
	          if is_htmx then
	            Kirin.html (Views_charts.zone_shot_chart_partial chart)
	          else
	            (match Db.get_seasons () with
	            | Ok seasons ->
	                let season_list = seasons |> List.map (fun s -> (s.code, s.name)) in
	                Kirin.html (Views_charts.zone_shot_chart_page ~lang chart ~seasons:season_list ~current_season:season)
	            | Error _ ->
	                Kirin.html (Views_charts.zone_shot_chart_page ~lang chart ~seasons:[] ~current_season:season))
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	    );

	    (* Team Profile *)
	    Kirin.get "/team/:name" (fun request ->
	      let lang = request_lang request in
	      let team_name = Kirin.param "name" request |> Uri.pct_decode in
	      match Db.get_seasons () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok seasons ->
	          let season = query_season_or_latest request seasons in
	          (match Db.get_team_full_detail ~team_name ~season () with
	          | Ok detail ->
	              let player_info_map = get_player_info_map () in
	              Kirin.html (Views_team.team_profile_page ~lang ~player_info_map detail ~season ~seasons)
	          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
	    );

	    (* Team H2H comparison *)
	    Kirin.get "/teams/h2h" (fun request ->
	      let lang = request_lang request in
	      let team1 = Kirin.query_opt "team1" request |> Option.value ~default:"" in
	      let team2 = Kirin.query_opt "team2" request |> Option.value ~default:"" in
	      match Db.get_seasons () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok seasons ->
	          let season = query_season_or_latest request seasons in
	          if team1 = "" || team2 = "" then
	            (* Show empty form *)
	            Kirin.html (Views_team.team_h2h_page ~lang ~team1 ~team2 ~season ~seasons [])
	          else
	            (match Db.get_team_h2h_data ~team1 ~team2 ~season () with
	            | Ok games -> Kirin.html (Views_team.team_h2h_page ~lang ~team1 ~team2 ~season ~seasons games)
	            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
	    );

    (* Convenience aliases for transactions *)
    Kirin.get "/draft" (fun request ->
      let year = Kirin.query_opt "year" request |> Option.value ~default:"" in
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      let params = String.concat "&" (List.filter (fun s -> s <> "") [
        "tab=draft";
        (if year <> "" then "year=" ^ year else "");
        (if q <> "" then "q=" ^ q else "")
      ]) in
      Kirin.redirect ("/transactions?" ^ params)
    );
    Kirin.get "/trade" (fun request ->
      let year = Kirin.query_opt "year" request |> Option.value ~default:"" in
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      let params = String.concat "&" (List.filter (fun s -> s <> "") [
        "tab=trade";
        (if year <> "" then "year=" ^ year else "");
        (if q <> "" then "q=" ^ q else "")
      ]) in
      Kirin.redirect ("/transactions?" ^ params)
    );

	    (* Draft / Trade (official transactions) *)
	    Kirin.get "/transactions" (fun request ->
	      let lang = request_lang request in
	      let tab =
	        Kirin.query_opt "tab" request
	        |> Option.map String.lowercase_ascii
	        |> Option.value ~default:"draft"
	      in
      let show_ops = query_bool request "ops" in
	      let year =
        let year_str_opt = Kirin.query_opt "year" request in
        match year_str_opt with
        | None -> 0
        | Some s -> (match int_of_string_opt s with Some i -> i | None -> 0)
	      in
	      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
	      match Db.get_draft_years (), Db.get_official_trade_years () with
	      | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok draft_years, Ok trade_years ->
	          if tab = "trade" then (
	            match Db.get_official_trade_events ~year ~search:q () with
	            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	            | Ok events ->
	                Kirin.html
	                  (Views_tools.transactions_page
	                     ~lang
	                     ~show_ops
	                     ~tab
	                     ~year
	                     ~q
	                     ~draft_years
	                     ~trade_years
	                     ~draft_picks:[]
	                     ~trade_events:events
	                     ())
	          ) else (
	            match Db.get_draft_picks ~year ~search:q () with
	            | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	            | Ok picks ->
	                Kirin.html
	                  (Views_tools.transactions_page
	                     ~lang
	                     ~show_ops
	                     ~tab:"draft"
	                     ~year
	                     ~q
	                     ~draft_years
	                     ~trade_years
	                     ~draft_picks:picks
	                     ~trade_events:[]
	                     ())
	          )
	    );

	    (* Hot Streaks *)
	    Kirin.get "/streaks" (fun request ->
	      let lang = request_lang request in
	      match Db.get_seasons () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok seasons ->
	          let season = query_season_or_latest request seasons in
	          match Db.get_players ~season ~search:"" ~sort:ByEfficiency (), Db.get_all_teams () with
	          | Error e, _ | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	          | Ok players, Ok teams ->
              let top_players = List.filteri (fun i _ -> i < 30) players in
              let player_ids = List.map (fun (p: player_aggregate) -> p.player_id) top_players in
              let player_streaks = match Db.get_batch_player_game_logs ~player_ids ~season () with
                | Error _ -> []
                | Ok games_tbl ->
                    List.filter_map (fun (p: player_aggregate) ->
                      match Hashtbl.find_opt games_tbl p.player_id with
                      | Some games when List.length games >= 2 ->
                          Some (Streaks.analyze_player_streaks
                            ~player_id:p.player_id
                            ~player_name:p.name
                            ~team_name:p.team_name
                            games)
                      | _ -> None
                    ) top_players
              in
              let all_player_streaks = List.concat player_streaks in
              let active_player_streaks = Streaks.get_active_streaks all_player_streaks in

              let team_streaks =
                List.filter_map (fun (t: team_info) ->
                  match Db.get_team_full_detail ~team_name:t.team_name ~season () with
                  | Ok detail ->
                      Some (Streaks.calculate_team_win_streaks ~team_name:t.team_name detail.tfd_game_results)
                  | Error _ -> None
                ) teams
              in
              let all_team_streaks = List.concat team_streaks in
              let active_team_streaks = all_team_streaks |> List.filter (fun s -> s.ts_is_active) in

              let best_player_streaks = Streaks.get_best_streaks all_player_streaks in
              let player_records = List.map (Streaks.player_streak_to_record ~season) best_player_streaks in
              let team_records = List.map (Streaks.team_streak_to_record ~season) all_team_streaks in
              let all_time_records =
                (player_records @ team_records)
                |> List.sort (fun a b -> compare b.sr_count a.sr_count)
                |> List.filteri (fun i _ -> i < 20)
              in

	              Kirin.html (Views_streaks.streaks_page
	                ~lang
	                ~season
	                ~seasons
	                ~active_player_streaks
	                ~active_team_streaks
	                ~all_time_records
	                ())
	    );

	    (* On/Off Impact *)
	    Kirin.get "/on-off" (fun request ->
	      let lang = request_lang request in
	      match Db.get_seasons () with
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	      | Ok seasons ->
	          let season = query_season_or_latest request seasons in
	          (match Db.get_on_off_impact_stats ~season () with
	          | Ok impacts -> Kirin.html (Views_tools.on_off_impact_page ~lang ~season ~seasons impacts)
	          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
	    );

		    (* QA & System *)
		    Kirin.get "/qa" (fun request ->
		      let lang = request_lang request in
		      let markdown = Qa.read_markdown_if_exists () in
		      match Db.get_db_quality_report () with
		      | Ok report -> Kirin.html (Views_tools.qa_dashboard_page report ~lang ~markdown ())
		      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
		    );
		    Kirin.get "/qa/pbp-missing" (fun request ->
		      let lang = request_lang request in
		      match Db.get_seasons () with
		      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
		      | Ok seasons ->
		          let season = query_season_or_latest request seasons in
		          (match Db.get_pbp_missing_report ~season () with
		          | Ok report -> Kirin.html (Views_tools.qa_pbp_missing_page report ~lang ~season ~seasons ())
		          | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
		    );
		    Kirin.get "/qa/schedule-missing" (fun request ->
		      let lang = request_lang request in
		      match Db.get_schedule_missing_report () with
		      | Ok report -> Kirin.html (Views_tools.qa_schedule_missing_page report ~lang ())
	      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	    );

	    (* QA Admin Login: /qa/admin?token=...&next=/qa/anomalies *)
	    Kirin.get "/qa/admin" (fun request ->
	      match admin_token_env with
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
	            let next = if is_safe_redirect_path next then next else "/qa/anomalies" in
	            let secure =
	              match Kirin.header "X-Forwarded-Proto" request with
	              | Some p -> String.lowercase_ascii (String.trim p) = "https"
	              | None -> false
	            in
	            Kirin.with_header "Set-Cookie" (admin_cookie_header ~secure expected)
	            @@ Kirin.redirect next
	    );

	    (* QA Overrides: exclude/restore obviously wrong rows *)
	    Kirin.get "/qa/anomalies" (fun request ->
	      if not (is_admin request) then
	        Kirin.empty `Not_found
	      else
	        let lang = request_lang request in
	        match Db.get_seasons () with
	        | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	        | Ok seasons ->
	            let season = query_season_or_latest request seasons in
	            (match Db.get_stat_anomaly_candidates ~season (), Db.get_stat_exclusions ~season () with
	            | Ok candidates, Ok exclusions ->
	                Kirin.html (Views_tools.qa_anomalies_page ~lang ~season ~seasons ~candidates ~exclusions ())
	            | Error e, _ -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
	            | _, Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e)))
	    );

	    Kirin.post "/qa/anomalies/exclude" (fun request ->
	      if not (is_admin request) then
	        Kirin.empty `Not_found
	      else
	        let lang = request_lang request in
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
	      if not (is_admin request) then
	        Kirin.empty `Not_found
	      else
	        let lang = request_lang request in
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
    (* PBP Data Quality API - verifies T2=HOME pattern
       Usage: /qa/pbp              - normal check (200 always)
              /qa/pbp?ci=1         - CI mode (500 if below 50% threshold)
              /qa/pbp?ci=1&threshold=60 - CI mode with custom threshold *)
    Kirin.get "/qa/pbp" (fun request ->
      match Db.get_pbp_data_quality () with
      | Ok pq ->
          let verified_pct = if pq.pq_total_pbp_games > 0 then
            100.0 *. (float_of_int pq.pq_t2_home_count /. float_of_int pq.pq_total_pbp_games)
          else 0.0 in
          let threshold = match Kirin.query_opt "threshold" request with
            | Some s -> (try float_of_string s with _ -> 50.0)
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
          (* CI mode: return error status in JSON if below threshold *)
          if ci_mode && not pass then
            Kirin.json_string (Printf.sprintf {|{"error":"PBP quality check failed","verified_pct":%.1f,"threshold":%.1f,"details":%s}|} verified_pct threshold json)
          else
            Kirin.json_string json
      | Error e -> Kirin.json_string (Printf.sprintf {|{"error":"%s"}|} (Db.show_db_error e))
    );
    Kirin.get "/health" (fun _ -> Kirin.json_string "{\"status\": \"ok\", \"engine\": \"OCaml/Kirin\"}");

    (* Player Search API for Command Palette *)
    Kirin.get "/api/search/players" (fun request ->
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      if String.length q < 1 then
        Kirin.json_string "[]"
      else begin
        match Db.get_seasons () with
        | Error _ -> Kirin.json_string "[]"
        | Ok seasons ->
            let season = query_season_or_latest request seasons in
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

    (* Live Scores API: JSON *)
    Kirin.get "/api/live/status" (fun _ ->
      let current = Live.get_current_games () in
      let games =
        if current <> [] then current
	        else
	          (* Fallback to today's schedule so the UI doesn't look empty if live polling fails. *)
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
	      let lang = request_lang request in
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
      (* In production, this would trigger actual push notifications *)
      Printf.printf "[Push] Test notification requested\n%!";
      Kirin.json_string {|{"status":"ok","message":"test notification sent"}|}
    );

    (* History & Legends *)
    Kirin.get "/history" (fun request ->
      let lang = request_lang request in
      match Db.get_historical_seasons () with
      | Ok seasons -> Kirin.html (Views_history.history_page ~lang seasons)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );
    Kirin.get "/legends" (fun request ->
      let lang = request_lang request in
      match Db.get_legend_players () with
      | Ok legends -> Kirin.html (Views_history.legends_page ~lang legends)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );
    Kirin.get "/coaches" (fun request ->
      let lang = request_lang request in
      match Db.get_coaches () with
      | Ok coaches -> Kirin.html (Views_history.coaches_page ~lang coaches)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );
    Kirin.get "/player/:id/career" (fun request ->
      let lang = request_lang request in
      let player_name = Kirin.param "id" request |> Uri.pct_decode in
      match Db.get_player_career ~player_name () with
      | Ok entries -> Kirin.html (Views_history.player_career_page ~lang ~player_name entries)
      | Error e -> Kirin.html (Views.error_page ~lang (Db.show_db_error e))
    );

	    (* AI Prediction API: Get match prediction with explanation *)
	    Kirin.get "/api/predict" (fun request ->
	      let home = Kirin.query_opt "home" request |> Option.value ~default:"" in
	      let away = Kirin.query_opt "away" request |> Option.value ~default:"" in
	      if home = "" || away = "" then
	        Kirin.with_status `Bad_request
	        @@ Kirin.json_string {|{"error":"home and away parameters required"}|}
	      else
	        let season =
	          match Db.get_seasons () with
	          | Ok seasons -> query_season_or_latest request seasons
	          | Error _ ->
	              query_nonempty request "season"
	              |> Option.value ~default:(Seasons_catalog.current_regular_season_code ())
	        in
	        match Db.get_team_stats ~season (), Db.get_standings ~season (), Db.get_games ~season () with
	        | Ok teams, Ok standings, Ok games ->
	            let find_stats name = List.find_opt (fun (t: Domain.team_stats) -> t.team = name) teams in
	            let find_standing name = List.find_opt (fun (s: Domain.team_standing) -> s.team_name = name) standings in
            (match find_stats home, find_stats away, find_standing home, find_standing away with
            | Some home_stats, Some away_stats, Some home_st, Some away_st ->
                let output = Prediction.predict_match_nerd
                  ~context:None ~season ~is_neutral:false ~games
                  ~home:home_stats ~away:away_stats
                  ~home_win_pct:home_st.win_pct ~away_win_pct:away_st.win_pct
                  ~name_home:home ~name_away:away
                in
                let explanation = Ai.get_explanation ~home ~away output in
                let r = output.result in
                let json_obj = `Assoc [
                  ("winner", `String r.winner);
                  ("probability", `Float (r.prob_a *. 100.0));
                  ("explanation", `String explanation);
                  ("breakdown", `Assoc [
                    ("elo_home", `Float output.breakdown.pb_elo_home);
                    ("elo_away", `Float output.breakdown.pb_elo_away);
                    ("elo_prob", `Float (output.breakdown.pb_elo_prob *. 100.0));
                  ]);
                ] in
                let json = Yojson.Basic.to_string json_obj in
                Kirin.with_header "Cache-Control" "public, max-age=300"
                @@ Kirin.json_string json
            | None, _, _, _ | _, None, _, _ ->
                Kirin.with_status `Not_found
                @@ Kirin.json_string {|{"error":"Team stats not found"}|}
            | _, _, None, _ | _, _, _, None ->
                Kirin.with_status `Not_found
                @@ Kirin.json_string {|{"error":"Team standings not found"}|})
        | Error e, _, _ | _, Error e, _ | _, _, Error e ->
            Kirin.server_error ~body:(Db.show_db_error e) ()
    );

	    (* AI Prediction OG Image *)
	    Kirin.get "/api/og/predict" (fun request ->
	      let home = Kirin.query_opt "home" request |> Option.value ~default:"" in
	      let away = Kirin.query_opt "away" request |> Option.value ~default:"" in
	      let season =
	        match Db.get_seasons () with
	        | Ok seasons -> query_season_or_latest request seasons
	        | Error _ ->
	            query_nonempty request "season"
	            |> Option.value ~default:(Seasons_catalog.current_regular_season_code ())
	      in
	      match Db.get_team_stats ~season (), Db.get_standings ~season (), Db.get_games ~season () with
	      | Ok teams, Ok standings, Ok games ->
	          let find_stats name = List.find_opt (fun (t: Domain.team_stats) -> t.team = name) teams in
	          let find_standing name = List.find_opt (fun (s: Domain.team_standing) -> s.team_name = name) standings in
          (match find_stats home, find_stats away, find_standing home, find_standing away with
          | Some home_stats, Some away_stats, Some home_st, Some away_st ->
              let output = Prediction.predict_match_nerd
                ~context:None ~season ~is_neutral:false ~games
                ~home:home_stats ~away:away_stats
                ~home_win_pct:home_st.win_pct ~away_win_pct:away_st.win_pct
                ~name_home:home ~name_away:away
              in
              let svg = Cards.prediction_card ~home ~away output in
              (match Cards.svg_to_png svg with
              | Some png -> 
                  Kirin.with_header "Content-Type" "image/png"
                  @@ Kirin.with_header "Cache-Control" "public, max-age=3600"
                  @@ Kirin.Response.make ~status:`OK (`String png)
              | None -> Kirin.with_header "Content-Type" "image/svg+xml" @@ Kirin.text svg)
          | _ -> Kirin.not_found ~body:"Team data not found" ())
      | _ -> Kirin.server_error ~body:"Failed to generate image" ()
    );

	    (* SSE: Live scores endpoint *)
	    Kirin.get "/api/live/scores" (fun _request ->
	      let today = Live.today_str () in
		      let season =
		        match Db.get_seasons () with
		        | Ok seasons -> Request_params.latest_season_code seasons
		        | Error _ -> Seasons_catalog.current_regular_season_code ()
		      in
	      (* Use a larger page size so "today" isn't dropped due to future games. *)
	      match Db.get_games ~season ~page_size:400 () with
	      | Ok games ->
	          (* Filter today's games *)
	          let today_games = List.filter (fun (g: Domain.game_summary) ->
	            String.length g.game_date >= 10 && String.sub g.game_date 0 10 = today
	          ) games in
	          (* Convert to JSON *)
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
          (* Return as SSE event *)
          let event = Kirin.Sse.event "scores" (Yojson.Basic.to_string json_obj) in
          Kirin.Sse.response_legacy [event]
      | Error e ->
          let err_event = Kirin.Sse.event "error" (Db.show_db_error e) in
          Kirin.Sse.response_legacy [err_event]
    );

    (* Live scores page *)
    Kirin.get "/live" (fun request ->
      let lang = request_lang request in
      Kirin.html (Views.live_page ~lang ())
    );
  ]
