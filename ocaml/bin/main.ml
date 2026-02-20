(** WKBL Analytics Main Entry Point
    OCaml Edition using Kirin framework
*)

open Wkbl

(* Runtime config from environment variables *)
let poller_interval =
  Sys.getenv_opt "POLLER_INTERVAL_SECS"
  |> Option.map float_of_string
  |> Option.value ~default:60.0

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

(* Cache-Control middleware for HTML page responses.
   Adds cache headers based on route pattern when none are already set.

   Two-tier TTL:
   - max-age: browser cache (short, user sees fresh on refresh)
   - s-maxage: CDN/edge cache (longer, Cloudflare serves from edge)
   - stale-while-revalidate: serve stale while fetching fresh in background

   Route tiers:
   - /static/*: browser 1d, CDN 7d (assets rarely change)
   - /api/*: browser 60s, CDN 120s (polling endpoints)
   - /season/* past: browser 1d, CDN 7d (historical, immutable)
   - /season/* current: browser 5min, CDN 15min (live scores possible)
   - Other HTML: browser 2min, CDN 10min (general pages)
*)
let cache_control_middleware : Kirin.middleware = fun next_handler request ->
  let response = next_handler request in
  (* Skip if Cache-Control already set by route handler *)
  match Kirin.response_header "Cache-Control" response with
  | Some _ -> response
  | None ->
    let status = Kirin.response_status response in
    (* Never cache error responses in CDN — prevents 404 cache poisoning *)
    if status >= 400 then
      Kirin.with_header "Cache-Control" "no-store" response
    else
      let path = Kirin.Request.uri request |> Uri.path in
      (* (max_age, s_maxage, stale_while_revalidate) *)
      let cache_params =
        if String.starts_with ~prefix:"/static/" path then
          Some (86400, 604800, 86400)
        else if String.starts_with ~prefix:"/api/" path then
          Some (60, 120, 30)
        else if String.starts_with ~prefix:"/season/" path then
          let code = String.sub path 8 (min 3 (String.length path - 8)) in
          let current = Scraper.current_season_code_auto () |> Scraper.main_to_datalab in
          if code < current then Some (86400, 604800, 86400)
          else Some (300, 900, 300)
        else
          match Kirin.response_header "Content-Type" response with
          | Some ct when String.starts_with ~prefix:"text/html" (String.lowercase_ascii ct) ->
            Some (120, 600, 120)
          | _ -> None
      in
      (match cache_params with
      | Some (ma, sma, swr) ->
        Kirin.with_header "Cache-Control"
          (Printf.sprintf "public, max-age=%d, s-maxage=%d, stale-while-revalidate=%d" ma sma swr)
          response
      | None -> response)

let security_headers_middleware : Kirin.middleware = fun next_handler request ->
  let response = next_handler request in
  response
  |> Kirin.with_header "X-Content-Type-Options" "nosniff"
  |> Kirin.with_header "X-Frame-Options" "DENY"
  |> Kirin.with_header "Referrer-Policy" "strict-origin-when-cross-origin"
  |> Kirin.with_header "Permissions-Policy" "camera=(), microphone=(), geolocation=()"

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
      sync_missing_pbp ~season ();
      Db.clear_all_caches ();
      Printf.printf "[Scheduler] Caches invalidated after sync\n%!"
    );
    (* Periodic sync *)
    while true do
      Eio.Time.sleep env#clock sync_interval;
      Printf.printf "[Scheduler] Running scheduled sync...\n%!";
      let ok = sync_with_retry () in
      if ok then (
        let season = Scraper.current_season_code_auto () |> Scraper.main_to_datalab in
        sync_missing_boxscores ~season ();
        sync_missing_pbp ~season ();
        Db.clear_all_caches ();
        Printf.printf "[Scheduler] Caches invalidated after sync\n%!"
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
           Live.broadcast_scores ();
           Db.clear_all_caches ()
        end
       with e ->
        Printf.eprintf "[Poller] Error: %s\n%s\n%!"
          (Printexc.to_string e) (Printexc.get_backtrace ()));
      Eio.Time.sleep env#clock poller_interval
    done
  );

  Kirin.run ~config:{ Kirin.default_config with port } ~sw ~env
  @@ Kirin.logger
  @@ Kirin.compress
  @@ security_headers_middleware
  @@ woman_win_redirect_middleware
  @@ cache_control_middleware
  @@ utf8_middleware
  @@ Kirin.static "/static" ~dir:static_path
  @@ Kirin.router (
    Routes_static.routes ~static_path
    @ Routes_home.routes
    @ Routes_team.routes
    @ Routes_history.routes
    @ Routes_player.routes ~sw ~env
    @ Routes_game.routes ~sw ~env
    @ Routes_stats.routes
    @ Routes_compare.routes
    @ Routes_predict.routes
    @ Routes_qa.routes
    @ Routes_api.routes ~sw ~env
    @ [
    (* Catch-all 404 *)
    Kirin.get "/*" (fun request ->
      let lang = Route_helpers.request_lang request in
      Kirin.html ~status:`Not_found (Views.not_found_page ~lang ()));
  ])
