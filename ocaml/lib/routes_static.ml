(** Static and utility routes: manifest, service worker, cards, robots, sitemap, health. *)

let routes ~static_path =
  [
    (* PWA: manifest.json - serve from static folder *)
    Kirin.get "/manifest.json" (fun _ ->
      let manifest_path = Filename.concat static_path "manifest.json" in
      if Sys.file_exists manifest_path then
        let content = In_channel.with_open_text manifest_path In_channel.input_all in
        Kirin.with_header "Content-Type" "application/manifest+json"
        @@ Kirin.with_header "Cache-Control" "public, max-age=86400, s-maxage=604800"
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

    (* Static assets: CSS, JS, images, fonts, etc. - wildcard catch-all *)
    Kirin.get "/static/*path" (fun request ->
      let path = Kirin.param "path" request in
      let file_path = Filename.concat static_path path in
      
      (* Security: check file exists and is within static_path *)
      if Sys.file_exists file_path && not (Sys.is_directory file_path) then
        let content = In_channel.with_open_bin file_path In_channel.input_all in
        let content_type = match Filename.extension path with
          | ".css" -> "text/css; charset=utf-8"
          | ".js" | ".mjs" -> "application/javascript; charset=utf-8"
          | ".json" -> "application/json; charset=utf-8"
          | ".png" -> "image/png"
          | ".jpg" | ".jpeg" -> "image/jpeg"
          | ".gif" -> "image/gif"
          | ".svg" -> "image/svg+xml"
          | ".webp" -> "image/webp"
          | ".woff" | ".woff2" -> "font/woff2"
          | ".ttf" -> "font/ttf"
          | ".eot" -> "application/vnd.ms-fontobject"
          | _ -> "application/octet-stream"
        in
        let cache_header = if String.starts_with ~prefix:"css/" path || String.starts_with ~prefix:"js/" path then
          "public, max-age=86400, s-maxage=604800, stale-while-revalidate=86400"
        else
          "public, max-age=2592000, immutable"  (* 30 days for images/fonts *)
        in
        Kirin.with_header "Content-Type" content_type
        @@ Kirin.with_header "Cache-Control" cache_header
        @@ Kirin.Response.make ~status:`OK (`String content)
      else
        Kirin.not_found ~body:(Printf.sprintf "Static file not found: /static/%s" path) ());

    (* Share Cards: Player card PNG *)
    Kirin.get "/card/player/png/:id" (fun request ->
      let player_id = Kirin.param "id" request in
      match Db.get_player_aggregate_by_id ~player_id () with
      | Ok (Some p) ->
          let svg = Cards.player_card p in
          (match Cards.svg_to_png svg with
          | Some png ->
              Kirin.with_header "Content-Type" "image/png"
              @@ Kirin.with_header "Cache-Control" "public, max-age=3600, s-maxage=86400"
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
          @@ Kirin.with_header "Cache-Control" "public, max-age=3600, s-maxage=86400"
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
          let static_pages = ["/"; "/players"; "/teams"; "/standings"; "/boxscores"; "/games"; "/leaders"; "/awards"; "/compare"; "/predict"; "/transactions"; "/history"; "/legends"; "/coaches"; "/streaks"; "/mvp-race"; "/fantasy"; "/lineups"; "/clutch"; "/live"; "/on-off"; "/draft"; "/trade"] in
          let low_priority_pages = ["/history"; "/legends"; "/coaches"; "/draft"; "/trade"] in
          let static_urls = static_pages |> List.map (fun path ->
            let priority = if path = "/" then "1.0"
              else if List.mem path low_priority_pages then "0.5"
              else "0.8" in
            Printf.sprintf {|  <url><loc>https://wkbl.win%s</loc><lastmod>%s</lastmod><changefreq>daily</changefreq><priority>%s</priority></url>|}
              path lastmod priority) |> String.concat "\n" in
          let team_urls = teams |> List.map (fun (t : Domain.team_info) ->
            Printf.sprintf {|  <url><loc>https://wkbl.win/team/%s</loc><lastmod>%s</lastmod><changefreq>weekly</changefreq><priority>0.7</priority></url>|}
              t.team_code lastmod) |> String.concat "\n" in
          let season_urls = seasons |> List.map (fun (s : Domain.season_info) ->
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

    (* Health check *)
    Kirin.get "/health" (fun _ -> Kirin.json_string "{\"status\": \"ok\", \"engine\": \"OCaml/Kirin\"}");
  ]
