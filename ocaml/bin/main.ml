(** WKBL Analytics Main Entry Point
    OCaml Edition using Kirin framework (migrated from Dream)
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

(* UTF-8 middleware for HTML responses *)
let utf8_middleware : Kirin.middleware = fun next_handler request ->
  let response = next_handler request in
  (match Kirin.response_header "Content-Type" response with
  | Some ct when String.starts_with ~prefix:"text/html" (String.lowercase_ascii ct) ->
      Kirin.with_header "Content-Type" "text/html; charset=utf-8" response
  | _ -> response)

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

  Kirin.run ~config:{ Kirin.default_config with port } ~sw ~env
  @@ Kirin.logger
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
              @@ Kirin.Response.make ~status:`OK png
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
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~limit:20 () with
          | Ok p -> Kirin.html (Views.home_page ~season ~seasons p)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Home Page Table HTMX *)
    Kirin.get "/home/table" (fun request ->
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~limit:20 () with
          | Ok p -> Kirin.html (Views.players_table p)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Handle HEAD requests *)
    Kirin.head "/" (fun _ -> Kirin.empty `OK);

    (* Redirect index.html *)
    Kirin.get "/index.html" (fun _ -> Kirin.redirect "/");

    (* Players List & Table HTMX *)
    Kirin.get "/players" (fun request ->
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~sort ~include_mismatch () with
          | Ok p -> Kirin.html (Views.players_page ~season ~seasons ~search ~sort:sort_str ~include_mismatch p)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    Kirin.get "/players/table" (fun request ->
      let search = Kirin.query_opt "search" request |> Option.value ~default:"" in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"eff" in
      let include_mismatch = query_bool request "include_mismatch" in
      let sort = Domain.player_sort_of_string sort_str in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search ~sort ~include_mismatch () with
          | Ok p -> Kirin.html (Views.players_table p)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Teams Page & Table HTMX *)
    Kirin.get "/teams" (fun request ->
      let scope_str = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_team_stats ~season ~scope ~sort ~include_mismatch () with
          | Ok stats -> Kirin.html (Views.teams_page ~season ~seasons ~scope ~sort:sort_str ~include_mismatch stats)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    Kirin.get "/teams/table" (fun request ->
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let scope_str = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      let scope = Domain.team_scope_of_string scope_str in
      let sort_str = Kirin.query_opt "sort" request |> Option.value ~default:"pts" in
      let sort = Domain.team_sort_of_string sort_str in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope ~sort ~include_mismatch () with
      | Ok stats -> Kirin.html (Views.teams_table ~season ~scope stats)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Team Shooting Comparison Chart - HTMX Partial *)
    Kirin.get "/teams/shooting-chart" (fun request ->
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_team_stats ~season ~scope:Domain.PerGame ~sort:Domain.TeamByPoints ~include_mismatch () with
      | Ok stats -> Kirin.html (Views_charts.team_shooting_comparison stats)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Standings *)
    Kirin.get "/standings" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_standings ~season () with
          | Ok standings -> Kirin.html (Views.standings_page ~season ~seasons standings)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    Kirin.get "/standings/table" (fun request ->
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_standings ~season () with
      | Ok standings -> Kirin.html (Views.standings_table ~season standings)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Games & Boxscores List *)
    Kirin.get "/games" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_games ~season () with
          | Ok games -> Kirin.html (Views.games_page ~season ~seasons games)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    Kirin.get "/games/table" (fun request ->
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_games ~season () with
      | Ok games -> Kirin.html (Views.games_table games)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Boxscores List *)
    Kirin.get "/boxscores" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_games ~season () with
          | Ok games -> Kirin.html (Views.boxscores_page ~season ~seasons games)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    Kirin.get "/boxscores/table" (fun request ->
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_games ~season () with
      | Ok games -> Kirin.html (Views.boxscores_table games)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Boxscore Detail *)
    Kirin.get "/boxscore/:id" (fun request ->
      let game_id = Kirin.param "id" request in
      match Db.get_boxscore ~game_id () with
      | Ok bs -> Kirin.html (Views.boxscore_page bs)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Play-by-Play (PBP) Detail *)
    Kirin.get "/boxscore/:id/pbp" (fun request ->
      let game_id = Kirin.param "id" request in
      let period_opt = query_nonempty request "period" in
      match Db.get_boxscore ~game_id () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok bs ->
          (match Db.get_pbp_periods ~game_id () with
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
              | Ok events -> Kirin.html (Views.pbp_page ~game:bs.boxscore_game ~periods ~selected_period ~events)
              | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    (* Game Flow Chart *)
    Kirin.get "/boxscore/:id/flow" (fun request ->
      let game_id = Kirin.param "id" request in
      match Db.get_boxscore ~game_id () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok bs ->
          (* Get all periods for this game *)
          (match Db.get_pbp_periods ~game_id () with
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
          | Ok periods ->
              (* Fetch PBP events for all periods *)
              let all_events =
                List.fold_left (fun acc period_code ->
                  match Db.get_pbp_events ~game_id ~period_code () with
                  | Ok events -> acc @ events
                  | Error _ -> acc
                ) [] periods
              in
              (* Extract score flow and render chart *)
              let flow_points = Domain.extract_score_flow all_events in
              Kirin.html (Views_tools.game_flow_page ~game:bs.boxscore_game flow_points))
    );

    (* Leaders *)
    Kirin.get "/leaders" (fun request ->
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
              Kirin.html (Views.leaders_page ~season ~seasons ~scope leaders_by_category)
          | Error e ->
              Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Clutch Time Leaders *)
    Kirin.get "/clutch" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_clutch_stats ~season () with
          | Ok stats ->
              let sorted_stats = List.sort
                (fun (a: clutch_stats) (b: clutch_stats) ->
                  compare b.cs_clutch_points a.cs_clutch_points)
                stats
              in
              Kirin.html (Views.clutch_page ~season ~seasons sorted_stats)
          | Error e ->
              Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Lineup Chemistry - Full Page *)
    Kirin.get "/lineups" (fun request ->
      match Db.get_seasons (), Db.get_all_teams () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let season = query_season_or_latest request seasons in
          let team = Kirin.query_opt "team" request |> Option.value ~default:"ALL" in
          match Db.get_lineup_chemistry ~season ~team_name:team () with
          | Ok chemistry ->
              Kirin.html (Views_tools.lineup_chemistry_page
                ~teams ~seasons ~selected_team:team ~selected_season:season chemistry)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Lineup Chemistry - Table Content (HTMX partial) *)
    Kirin.get "/lineups/table" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          let team = Kirin.query_opt "team" request |> Option.value ~default:"ALL" in
          match Db.get_lineup_chemistry ~season ~team_name:team () with
          | Ok chemistry ->
              Kirin.html (Views_tools.lineup_chemistry_table_content chemistry)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Awards (Stat-based, unofficial) *)
    Kirin.get "/awards" (fun request ->
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
          | Ok mvp, Ok mip -> Kirin.html (Views.awards_page ~season ~seasons ~include_mismatch ~prev_season_name ~mvp ~mip ())
          | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* MVP Race - Full Page *)
    Kirin.get "/mvp-race" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_mvp_race ~season () with
          | Ok candidates -> Kirin.html (Views_mvp.mvp_race_page ~season ~seasons candidates)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* MVP Race - Table HTMX *)
    Kirin.get "/mvp-race/table" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_mvp_race ~season () with
          | Ok candidates -> Kirin.html (Views_mvp.mvp_race_table candidates)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Fantasy Calculator - Full Page *)
    Kirin.get "/fantasy" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
          | Ok players ->
              let scores = List.map (Domain.fantasy_score_of_aggregate ~rules) players in
              Kirin.html (Views_tools.fantasy_calculator_page ~season ~seasons ~rules ~scores)
    );

    (* Fantasy Calculator - HTMX Calculate Endpoint *)
    Kirin.get "/fantasy/calculate" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
          | Ok players ->
              let scores = List.map (Domain.fantasy_score_of_aggregate ~rules) players in
              Kirin.html (Views_tools.fantasy_results_table scores)
    );

    (* Compare - simplified version *)
    Kirin.get "/compare" (fun request ->
      let p1_query = Kirin.query_opt "p1" request |> Option.value ~default:"" in
      let p2_query = Kirin.query_opt "p2" request |> Option.value ~default:"" in
      let p1_id = Kirin.query_opt "p1_id" request |> Option.value ~default:"" in
      let p2_id = Kirin.query_opt "p2_id" request |> Option.value ~default:"" in
      let p1_id_opt = if String.trim p1_id = "" then None else Some (String.trim p1_id) in
      let p2_id_opt = if String.trim p2_id = "" then None else Some (String.trim p2_id) in

      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons, Ok teams ->
          let upcoming = match Db.get_upcoming_schedule ~status:"scheduled" ~limit:6 () with Ok u -> u | Error _ -> [] in
          let team_names = List.map (fun (t: team_info) -> t.team_name) teams in
          let season = query_season_or_latest request seasons in
          let render result error =
            Kirin.html (Views.predict_page ~season ~seasons ~teams:team_names ~home ~away ~is_neutral ~context_enabled ~include_mismatch ~upcoming result error)
          in

          if String.trim home = "" || String.trim away = "" then
            render None None
          else if normalize_label home = normalize_label away then
            render None (Some "Home/Away teams must be different.")
          else
            match Db.get_team_stats ~season ~scope:Totals ~include_mismatch (), Db.get_scored_games ~season ~include_mismatch () with
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
            | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Player Profile *)
    Kirin.get "/player/:id" (fun request ->
      let player_id = Kirin.param "id" request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      match Db.get_player_profile ~player_id () with
      | Ok (Some profile) ->
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
            | [] -> Ok (List.rev acc)
            | category :: rest ->
                match Db.get_leaders ~season:season_for_leaderboards ~scope category with
                | Error e -> Error e
                | Ok leaders -> fetch_all ((category, leaders) :: acc) rest
          in
          let leaderboards =
            match fetch_all [] leaderboard_categories with
            | Ok leaders_by_category ->
                Some (season_for_leaderboards, season_name_for_leaderboards, leaders_by_category)
            | Error _ ->
                None
          in
          Kirin.html (Views_player.player_profile_page ~leaderboards final_profile ~scope ~seasons_catalog)
      | Ok None -> Kirin.html (Views.error_page "Player not found")
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Player Game Log *)
    Kirin.get "/player/:id/games" (fun request ->
      let player_id = Kirin.param "id" request in
      let include_mismatch = query_bool request "include_mismatch" in
      match Db.get_player_profile ~player_id (), Db.get_seasons () with
      | Ok None, _ -> Kirin.html (Views.error_page "Player not found")
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok (Some profile), Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_player_game_logs ~player_id ~season ~include_mismatch () with
          | Ok games -> Kirin.html (Views_player.player_game_logs_page profile ~season ~seasons ~include_mismatch games)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    (* Player Season Stats HTMX Partial *)
    Kirin.get "/player/:id/season-stats" (fun request ->
      let player_id = Kirin.param "id" request in
      let scope = Kirin.query_opt "scope" request |> Option.value ~default:"per_game" in
      match Db.get_player_season_stats ~player_id ~scope () with
      | Ok stats -> Kirin.html (Views_common.player_season_stats_component ~player_id ~scope stats)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Player Shot Chart HTMX Partial *)
    Kirin.get "/player/:id/shot-chart" (fun request ->
      let player_id = Kirin.param "id" request in
      let season = Kirin.query_opt "season" request |> Option.value ~default:"ALL" in
      match Db.get_player_shooting_stats ~player_id ~season () with
      | Ok (Some stats) -> Kirin.html (Views_charts.player_shot_chart_html stats)
      | Ok None -> Kirin.html {|<div class="text-slate-400 text-center p-4">슈팅 데이터가 없습니다.</div>|}
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );

    (* Team Profile *)
    Kirin.get "/team/:name" (fun request ->
      let team_name = Kirin.param "name" request |> Uri.pct_decode in
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_team_full_detail ~team_name ~season () with
          | Ok detail -> Kirin.html (Views_team.team_profile_page detail ~season ~seasons)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
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
      let tab =
        Kirin.query_opt "tab" request
        |> Option.map String.lowercase_ascii
        |> Option.value ~default:"draft"
      in
      let year =
        let year_str_opt = Kirin.query_opt "year" request in
        match year_str_opt with
        | None -> 0
        | Some s -> (match int_of_string_opt s with Some i -> i | None -> 0)
      in
      let q = Kirin.query_opt "q" request |> Option.value ~default:"" in
      match Db.get_draft_years (), Db.get_official_trade_years () with
      | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok draft_years, Ok trade_years ->
          if tab = "trade" then (
            match Db.get_official_trade_events ~year ~search:q () with
            | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
            | Ok events ->
                Kirin.html
                  (Views_tools.transactions_page
                     ~tab
                     ~year
                     ~q
                     ~draft_years
                     ~trade_years
                     ~draft_picks:[]
                     ~trade_events:events)
          ) else (
            match Db.get_draft_picks ~year ~search:q () with
            | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
            | Ok picks ->
                Kirin.html
                  (Views_tools.transactions_page
                     ~tab:"draft"
                     ~year
                     ~q
                     ~draft_years
                     ~trade_years
                     ~draft_picks:picks
                     ~trade_events:[])
          )
    );

    (* Hot Streaks *)
    Kirin.get "/streaks" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          match Db.get_players ~season ~search:"" ~sort:ByEfficiency (), Db.get_all_teams () with
          | Error e, _ | _, Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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
                ~season
                ~seasons
                ~active_player_streaks
                ~active_team_streaks
                ~all_time_records
                ())
    );

    (* On/Off Impact *)
    Kirin.get "/on-off" (fun request ->
      match Db.get_seasons () with
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
      | Ok seasons ->
          let season = query_season_or_latest request seasons in
          (match Db.get_on_off_impact_stats ~season () with
          | Ok impacts -> Kirin.html (Views_tools.on_off_impact_page ~season ~seasons impacts)
          | Error e -> Kirin.html (Views.error_page (Db.show_db_error e)))
    );

    (* QA & System *)
    Kirin.get "/qa" (fun _ ->
      let markdown = Qa.read_markdown_if_exists () in
      match Db.get_db_quality_report () with
      | Ok report -> Kirin.html (Views_tools.qa_dashboard_page report ~markdown ())
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
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

    (* History & Legends *)
    Kirin.get "/history" (fun _ ->
      match Db.get_historical_seasons () with
      | Ok seasons -> Kirin.html (Views_history.history_page seasons)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );
    Kirin.get "/legends" (fun _ ->
      match Db.get_legend_players () with
      | Ok legends -> Kirin.html (Views_history.legends_page legends)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );
    Kirin.get "/coaches" (fun _ ->
      match Db.get_coaches () with
      | Ok coaches -> Kirin.html (Views_history.coaches_page coaches)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );
    Kirin.get "/player/:id/career" (fun request ->
      let player_name = Kirin.param "id" request |> Uri.pct_decode in
      match Db.get_player_career ~player_name () with
      | Ok entries -> Kirin.html (Views_history.player_career_page ~player_name entries)
      | Error e -> Kirin.html (Views.error_page (Db.show_db_error e))
    );
  ]
