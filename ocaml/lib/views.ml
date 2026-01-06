(** HTML Views - HTMX-powered templates

    Design:
    - Pure functions returning strings
    - No side effects
    - Composable components
*)

open Domain

(** Cache-busting asset version (Cloudflare + browser cache). *)
let asset_version =
  Sys.getenv_opt "WKBL_ASSET_VERSION"
  |> Option.map String.trim
  |> function
  | Some v when v <> "" -> v
  | _ -> string_of_int (int_of_float (Unix.time ()))

(** Escape HTML - prevent XSS *)
let escape_html s =
  s
  |> String.to_seq
  |> Seq.map (function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""

(** Player image component with fallback *)
let player_img_tag ?(class_name="w-12 h-12") player_id player_name =
  let local_src = Printf.sprintf "/static/images/player_%s.png" player_id in
  let remote_src = Printf.sprintf "https://www.wkbl.or.kr/static/images/player/pimg/m_%s.jpg" player_id in
  let placeholder_src = "/static/images/player_placeholder.svg" in
  let local_filename = Printf.sprintf "player_%s.png" player_id in
  let has_local_player_image =
    let env_static =
      Sys.getenv_opt "WKBL_STATIC_PATH"
      |> Option.map (fun p -> Filename.concat (Filename.concat p "images") local_filename)
    in
    let candidates =
      [ env_static
      ; Some (Filename.concat "static/images" local_filename)
      ; Some (Filename.concat "ocaml/static/images" local_filename)
      ]
      |> List.filter_map Fun.id
    in
    List.exists Sys.file_exists candidates
  in
  let src = if has_local_player_image then local_src else remote_src in
  Printf.sprintf
    {html|<img src="%s" alt="%s" class="%s rounded-full object-cover bg-slate-800 border border-slate-700 shadow-sm" loading="lazy" data-placeholder="%s">|html}
    (escape_html src)
    (escape_html player_name)
    (escape_html class_name)
    (escape_html placeholder_src)

(** Team logo component *)
let team_logo_tag ?(class_name="w-8 h-8") team_name =
  let logo_file =
    match team_code_of_string team_name with
    | Some code -> team_code_to_logo code
    | None -> None
  in
  match logo_file with
  | Some f -> Printf.sprintf {html|<img src="/static/images/%s" alt="%s" class="%s object-contain">|html} f (escape_html team_name) class_name
  | None ->
      Printf.sprintf
        {html|<div class="%s bg-slate-800 rounded flex items-center justify-center text-xs">🏀</div>|html}
        class_name

(** Team badge component *)
let team_badge team_name =
  let color =
    team_code_of_string team_name
    |> Option.map team_code_to_color
    |> Option.value ~default:"#666"
  in
  Printf.sprintf
    {html|<a href="/team/%s" class="inline-flex items-center gap-1.5 px-2 py-0.5 rounded text-xs font-medium hover:brightness-125 transition" style="background-color: %s20; color: %s; border: 1px solid %s40">%s<span class="truncate max-w-[80px]">%s</span></a>|html}
    (Uri.pct_encode team_name) color color color (team_logo_tag ~class_name:"w-4 h-4" team_name) (escape_html team_name)

(** Stat cell with formatting *)
let stat_cell ?(highlight=false) value =
  let class_name = if highlight then "text-orange-400 font-bold" else "text-slate-300" in
  Printf.sprintf {html|<td class="px-3 py-2 text-right %s font-mono">%.1f</td>|html} class_name value

(** Margin cell (signed, colored) *)
let margin_cell value =
  let class_name =
    if value > 0.0 then "text-sky-400 font-bold"
    else if value < 0.0 then "text-rose-400 font-bold"
    else "text-slate-300 font-bold"
  in
  let value_str =
    if value > 0.0 then Printf.sprintf "+%.1f" value else Printf.sprintf "%.1f" value
  in
  Printf.sprintf {html|<td class="px-3 py-2 text-right %s font-mono">%s</td>|html} class_name (escape_html value_str)

(** Player Season Stats Table Component (Fixed widths for layout stability) *)
let player_season_stats_table ~scope (stats: season_stats list) =
  let total_gp = stats |> List.fold_left (fun acc (s: season_stats) -> acc + s.ss_games_played) 0 in
  let total_minutes = stats |> List.fold_left (fun acc (s: season_stats) -> acc +. s.ss_total_minutes) 0.0 in
  let sum_by f =
    stats |> List.fold_left (fun acc (s: season_stats) -> acc +. f s) 0.0
  in
  let sum_weighted ~weight f =
    stats |> List.fold_left (fun acc (s: season_stats) -> acc +. f s *. weight s) 0.0
  in
  let career_row =
    match stats with
    | [] -> None
    | _ ->
        let margin =
          if total_minutes <= 0.0 then 0.0
          else (sum_weighted ~weight:(fun s -> s.ss_total_minutes) (fun s -> s.ss_margin)) /. total_minutes
        in
        let make_weighted_row ~weight =
          let wsum = stats |> List.fold_left (fun acc s -> acc +. weight s) 0.0 in
          let safe_div x = if wsum <= 0.0 then 0.0 else x /. wsum in
          let pts = safe_div (sum_weighted ~weight (fun s -> s.ss_avg_points)) in
          let reb = safe_div (sum_weighted ~weight (fun s -> s.ss_avg_rebounds)) in
          let ast = safe_div (sum_weighted ~weight (fun s -> s.ss_avg_assists)) in
          let eff = safe_div (sum_weighted ~weight (fun s -> s.ss_efficiency)) in
          (pts, reb, ast, eff)
        in
        let pts, reb, ast, eff =
          match scope with
          | "totals" ->
              (sum_by (fun s -> s.ss_avg_points),
               sum_by (fun s -> s.ss_avg_rebounds),
               sum_by (fun s -> s.ss_avg_assists),
               sum_by (fun s -> s.ss_efficiency))
          | "per_36" ->
              make_weighted_row ~weight:(fun s -> s.ss_total_minutes)
          | _ ->
              make_weighted_row ~weight:(fun s -> float_of_int s.ss_games_played)
        in
        Some {
          ss_season_code = "CAREER";
          ss_season_name = "Career";
          ss_games_played = total_gp;
          ss_total_minutes = total_minutes;
          ss_avg_points = pts;
          ss_avg_rebounds = reb;
          ss_avg_assists = ast;
          ss_avg_steals = 0.0;
          ss_avg_blocks = 0.0;
          ss_avg_turnovers = 0.0;
          ss_efficiency = eff;
          ss_margin = margin;
          ss_ts_pct = 0.0;
          ss_efg_pct = 0.0;
        }
  in
  let row_html ?(highlight=false) (s: season_stats) =
    let margin_class = if s.ss_margin >= 0.0 then "text-sky-400" else "text-rose-400" in
    let margin_str = if s.ss_margin > 0.0 then Printf.sprintf "+%.1f" s.ss_margin else Printf.sprintf "%.1f" s.ss_margin in
    let row_class =
      if highlight then "bg-slate-800/40 border-b border-slate-700/60"
      else "border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors"
    in
    let name_class = if highlight then "text-white font-black" else "text-white font-medium" in
    Printf.sprintf {html|<tr class="%s">
          <td class="px-4 py-3 %s truncate">%s</td>
          <td class="px-4 py-3 text-right text-slate-300 w-[60px] font-mono">%d</td>
          <td class="px-4 py-3 text-right text-slate-300 w-[80px] font-mono">%.1f</td>
          <td class="px-4 py-3 text-right font-bold text-orange-400 w-[80px] font-mono">%.1f</td>
          <td class="px-4 py-3 text-right font-bold %s w-[80px] font-mono">%s</td>
          <td class="px-4 py-3 text-right text-slate-300 w-[80px] font-mono">%.1f</td>
          <td class="px-4 py-3 text-right text-slate-300 w-[80px] font-mono">%.1f</td>
          <td class="px-4 py-3 text-right text-slate-300 w-[80px] font-mono">%.1f</td>
        </tr>|html}
      row_class
      name_class
      (escape_html s.ss_season_name)
      s.ss_games_played
      s.ss_total_minutes
      s.ss_avg_points
      margin_class
      (escape_html margin_str)
      s.ss_avg_rebounds
      s.ss_avg_assists
      s.ss_efficiency
  in
  let rows =
    (match career_row with
     | None -> []
     | Some s -> [row_html ~highlight:true s])
    @ (stats |> List.map row_html)
    |> String.concat "\n"
  in
  Printf.sprintf {html|<div class="bg-slate-900 rounded-xl border border-slate-800 overflow-hidden shadow-lg animate-fade-in"><table class="season-stats-table w-full text-sm font-mono table-fixed">
    <thead class="bg-slate-800/80 text-slate-400 uppercase tracking-wider text-xs">
      <tr>
        <th class="px-4 py-3 text-left font-sans">Season</th>
        <th class="px-4 py-3 text-right w-[60px]">GP</th>
        <th class="px-4 py-3 text-right w-[80px]">MIN</th>
        <th class="px-4 py-3 text-right text-orange-400 w-[80px]">PTS</th>
        <th class="px-4 py-3 text-right w-[80px]">MG</th>
        <th class="px-4 py-3 text-right w-[80px]">REB</th>
        <th class="px-4 py-3 text-right w-[80px]">AST</th>
        <th class="px-4 py-3 text-right w-[80px]">EFF</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
  </table></div>|html} rows

(** Player Season Stats Component (Tabs + Table) *)
let player_season_stats_component ~player_id ~scope (stats: season_stats list) =
  let btn_class active =
    if active then "season-stats-tab px-4 py-2 bg-transparent border-0 border-b-2 border-orange-500 text-white font-medium cursor-default pointer-events-none"
    else "season-stats-tab px-4 py-2 bg-transparent border-0 border-b-2 border-transparent text-slate-400 hover:text-white transition cursor-pointer font-medium"
  in
  let s_per = btn_class (scope = "per_game") in
  let s_tot = btn_class (scope = "totals") in
  let s_36 = btn_class (scope = "per_36") in

  let tabs = Printf.sprintf
    {html|<div id="season-stats-tabs" class="flex items-center gap-4 border-b border-slate-800 mb-4">
      <button type="button" class="%s" hx-get="/player/%s/season-stats?scope=per_game" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Per Game</button>
      <button type="button" class="%s" hx-get="/player/%s/season-stats?scope=totals" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Totals</button>
      <button type="button" class="%s" hx-get="/player/%s/season-stats?scope=per_36" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Per 36</button>
      <span id="season-stats-indicator" class="htmx-indicator season-stats-indicator ml-auto text-xs text-slate-500"><span class="spinner"></span><span>Loading...</span></span>
    </div>|html}
    s_per player_id s_tot player_id s_36 player_id
  in
  let mg_note =
    {html|<p class="text-[11px] text-slate-500 leading-relaxed mb-3"><span class="font-mono text-slate-300">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 <span class="font-mono text-slate-300">+/-</span>는 문자중계(PBP) 기반으로 <span class="font-mono text-slate-300">일부 경기만</span> 제공됩니다. (데이터가 없으면 <span class="font-mono text-slate-300">-</span>)</p>|html}
  in
  let table = player_season_stats_table ~scope stats in
  Printf.sprintf {html|<div id="season-stats-component" class="stats-container animate-fade-in">%s%s%s</div>|html} tabs mg_note table

(** Career Highs Component *)
let career_highs_card (ch: career_high_item list option) =
  match ch with
  | None -> ""
  | Some items ->
      let items_html =
        items
        |> List.map (fun (item: career_high_item) ->
            let matchup_prefix = if item.chi_is_home then "vs " else "@ " in
            let matchup = matchup_prefix ^ item.chi_opponent in
            let game_url = "/boxscore/" ^ Uri.pct_encode item.chi_game_id in
            Printf.sprintf
              {html|<div class="flex justify-between items-center border-b border-slate-800 pb-2 last:border-0 last:pb-0">
                <div class="flex flex-col">
                  <span class="text-slate-400 text-xs uppercase tracking-wider">%s</span>
                  <a href="%s" class="text-xs text-slate-500 hover:text-slate-300 transition-colors font-mono">%s • %s</a>
                </div>
                <span class="text-white font-mono font-bold">%d</span>
              </div>|html}
              (escape_html item.chi_label)
              game_url
              (escape_html item.chi_game_date)
              (escape_html matchup)
              item.chi_value)
        |> String.concat "\n"
      in
      Printf.sprintf {html|<div class="bg-slate-900 rounded-xl border border-slate-800 p-6 shadow-lg h-full"><h3 class="text-orange-400 font-bold uppercase tracking-wider text-xs mb-4 flex items-center gap-2"><span class="text-lg">🚀</span> Career Highs</h3><div class="space-y-3">%s</div></div>|html} items_html

(** Player row component *)
let player_row (rank: int) (p: player_aggregate) =
  Printf.sprintf
    {html|<tr class="border-b border-slate-700/50 hover:bg-slate-800/50 transition-colors">
      <td class="px-3 py-2 text-slate-500 text-sm">%d</td>
      <td class="px-3 py-2 font-medium text-white">
        <div class="flex items-center gap-3 min-w-0">
          %s
          <a href="/player/%s" class="player-name hover:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
        </div>
      </td>
      <td class="px-3 py-2">%s</td>
      <td class="px-3 py-2 text-right text-slate-400">%d</td>
      %s%s%s%s%s%s%s%s
    </tr>|html}
    rank
    (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
    p.player_id
    (escape_html p.name)
    (team_badge p.team_name)
    p.games_played
    (stat_cell ~highlight:true p.avg_points)
    (margin_cell p.avg_margin)
    (stat_cell p.avg_rebounds)
    (stat_cell p.avg_assists)
    (stat_cell p.avg_steals)
    (stat_cell p.avg_blocks)
    (stat_cell p.avg_turnovers)
    (stat_cell ~highlight:true p.efficiency)

(** Players table - HTMX partial *)
let players_table (players: player_aggregate list) =
  let rows =
    players
    |> List.mapi (fun i p -> player_row (i + 1) p)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<table class="w-full">
      <thead class="bg-slate-800/80 sticky top-0 text-slate-400 text-xs uppercase tracking-wider">
        <tr><th class="px-3 py-3 text-left w-12">#</th><th class="px-3 py-3 text-left">Player</th><th class="px-3 py-3 text-left">Team</th><th class="px-3 py-3 text-right">GP</th><th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400" hx-get="/players/table?sort=pts" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">PTS</th><th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400" hx-get="/players/table?sort=mg" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">MG</th><th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400" hx-get="/players/table?sort=reb" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">REB</th><th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400" hx-get="/players/table?sort=ast" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">AST</th><th class="px-3 py-3 text-right">STL</th><th class="px-3 py-3 text-right">BLK</th><th class="px-3 py-3 text-right">TO</th><th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400" hx-get="/players/table?sort=eff" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">EFF</th></tr>
      </thead>
      <tbody id="players-body">%s</tbody>
    </table>|html}
    rows

(** Main layout *)
let layout ~title ~content =
  let v = escape_html asset_version in
  Printf.sprintf
    {html|<!DOCTYPE html>
<html lang="ko" class="dark">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <script src="/static/js/htmx-1.9.10.min.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/player-photo-fallback.js?v=%s" defer data-cfasync="false"></script>
  <script src="https://cdn.tailwindcss.com" data-cfasync="false"></script>
  <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&family=Inter:wght@400;600;800&display=swap" rel="stylesheet">
  <link rel="stylesheet" href="/static/css/styles.css?v=%s">
  <script data-cfasync="false">tailwind.config = { darkMode: 'class', theme: { extend: { fontFamily: { sans: ['Inter', 'sans-serif'], mono: ['JetBrains Mono', 'monospace'] } } } }</script>
</head>
<body class="bg-[#0b0e14] text-slate-200 font-sans antialiased min-h-screen">
  <header class="sticky top-0 z-50 bg-slate-900/95 backdrop-blur border-b border-slate-800 px-4 sm:px-6 py-4">
    <div class="max-w-7xl mx-auto flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3">
      <div class="flex items-center gap-3 shrink-0">
        <span class="text-2xl">🏀</span>
        <h1 class="text-lg font-bold text-white">WKBL <span class="text-orange-500">Analytics</span></h1>
        <span class="hidden sm:inline text-xs text-slate-500 font-mono">OCaml Edition</span>
      </div>
      <nav class="flex flex-wrap justify-end gap-x-4 gap-y-2 text-xs sm:text-sm">
        <a href="/" class="text-slate-400 hover:text-white transition">Home</a>
        <a href="/leaders" class="text-slate-400 hover:text-white transition">Leaders</a>
        <a href="/boxscores" class="text-slate-400 hover:text-white transition">Boxscores</a>
        <a href="/games" class="text-slate-400 hover:text-white transition">Games</a>
        <a href="/standings" class="text-slate-400 hover:text-white transition">Standings</a>
        <a href="/teams" class="text-slate-400 hover:text-white transition">Teams</a>
        <a href="/players" class="text-slate-400 hover:text-white transition">Players</a>
        <a href="/predict" class="text-slate-400 hover:text-white transition">Predict</a>
        <a href="/compare" class="text-slate-400 hover:text-white transition">Compare</a>
        <a href="/qa" class="text-slate-400 hover:text-white transition">QA</a>
      </nav>
    </div>
  </header>
  <main class="max-w-7xl mx-auto px-4 sm:px-6 py-8">%s</main>
  <footer class="border-t border-slate-800 py-6 text-center text-slate-500 text-sm">Built with OCaml + Dream + HTMX</footer>
</body>
</html>|html}
    (escape_html title) v v v content

(** Home page *)
let home_page players =
  let table = players_table players in
  layout ~title:"WKBL Analytics"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex items-center justify-between"><h2 class="text-xl font-bold text-white">Top Players by Efficiency</h2><div class="flex gap-2"><input type="text" placeholder="Search player..." class="bg-slate-800 border border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none" hx-get="/players/table" hx-trigger="keyup changed delay:300ms" hx-target="#players-table" name="search"></div></div><div id="players-table" class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">%s</div></div>|html}
      table)

let players_page ~search ~sort players =
  let sort_value = match String.lowercase_ascii sort with | "pts" | "points" -> "pts" | "mg" | "margin" -> "mg" | "reb" | "rebounds" -> "reb" | "ast" | "assists" -> "ast" | "min" | "minutes" -> "min" | "eff" | "efficiency" -> "eff" | _ -> "eff" in
  let sort_option value label = let selected = if sort_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let table = players_table players in
  let mg_note =
    if sort_value = "mg" then
      {html|<details class="bg-slate-900/50 rounded-lg border border-slate-800/50 p-4 text-xs text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-300 select-none">MG 안내 (왜 몇 명은 안 보이나요?)</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-200">MG</span> = (팀 득점 - 상대 득점)의 출전시간 가중 평균입니다.</div>
          <div>개인 <span class="font-mono text-slate-200">+/-</span>는 문자중계(PBP) 기반이며, 데이터가 없거나 PBP/박스스코어 최종 스코어 불일치 등 품질 이슈가 있으면 <span class="font-mono text-slate-200">-</span>로 표시합니다.</div>
          <div>표본 안정성을 위해 <span class="font-mono text-slate-200">MG</span> 정렬 시 총 출전 <span class="font-mono text-slate-200">100분 이상</span> 선수만 표시합니다.</div>
        </div>
      </details>|html}
    else
      ""
  in
  layout ~title:"WKBL Players"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-white">Players</h2><p class="text-slate-400 text-sm">Filter and sort player aggregates.</p></div><a class="text-orange-400 hover:text-orange-300 text-sm" href="/players">Reset</a></div><form id="players-filter" class="grid grid-cols-1 md:grid-cols-3 gap-3" hx-get="/players/table" hx-target="#players-table" hx-trigger="change, keyup delay:250ms"><input type="text" name="search" placeholder="Search player..." value="%s" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none"><select name="sort" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s</select><div class="text-slate-500 text-xs flex items-center">Sorted by %s</div></form>%s<div id="players-table" class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">%s</div></div>|html}
      (escape_html search) (sort_option "eff" "EFF") (sort_option "pts" "PTS") (sort_option "mg" "MG") (sort_option "reb" "REB") (sort_option "ast" "AST") (sort_option "min" "MIN") (String.uppercase_ascii sort_value) mg_note table)

let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

(** Team stat row component to avoid too many sprintf arguments *)
let team_stat_row (row: team_stats) =
  let name_cell = Printf.sprintf {html|<td class="px-3 py-2 font-medium text-white flex items-center gap-2 whitespace-nowrap">%s<a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a></td>|html} (team_logo_tag ~class_name:"w-5 h-5" row.team) (Uri.pct_encode row.team) (escape_html row.team) in
  let gp_cell = Printf.sprintf {html|<td class="px-3 py-2 text-right text-slate-400">%d</td>|html} row.gp in
  let stats_part1 = Printf.sprintf {html|<td class="px-3 py-2 text-right text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300">%s</td><td class="px-3 py-2 text-right text-slate-300">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden sm:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden sm:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden md:table-cell">%s</td>|html}
    (format_float row.min_total) (format_float row.pts) (format_float row.margin) (format_float row.pts_against) (format_float row.reb) (format_float row.ast) (format_float row.stl) in
  let stats_part2 = Printf.sprintf {html|<td class="px-3 py-2 text-right text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-orange-400 font-bold">%s</td>|html}
    (format_float row.blk) (format_float row.turnovers) (format_float row.fg_pct) (format_float row.fg3_pct) (format_float row.ft_pct) (format_float row.efg_pct) (format_float row.eff)
  in
  Printf.sprintf {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors">%s%s%s%s</tr>|html} name_cell gp_cell stats_part1 stats_part2

let teams_table ~scope (stats: team_stats list) =
  let rows =
    stats
    |> List.map team_stat_row
    |> String.concat "\n"
  in
  let min_label = if scope = PerGame then "MIN/G" else "MIN" in
  Printf.sprintf
    {html|<div class="bg-slate-900 rounded-lg border border-slate-800 overflow-x-auto overflow-y-hidden"><table class="w-full min-w-max text-xs sm:text-sm font-mono tabular-nums"><thead class="bg-slate-800/80 text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-3 py-2 text-left">Team</th><th class="px-3 py-2 text-right">GP</th><th class="px-3 py-2 text-right hidden md:table-cell">%s</th><th class="px-3 py-2 text-right">PTS</th><th class="px-3 py-2 text-right">MG</th><th class="px-3 py-2 text-right hidden md:table-cell">PA</th><th class="px-3 py-2 text-right hidden sm:table-cell">REB</th><th class="px-3 py-2 text-right hidden sm:table-cell">AST</th><th class="px-3 py-2 text-right hidden md:table-cell">STL</th><th class="px-3 py-2 text-right hidden md:table-cell">BLK</th><th class="px-3 py-2 text-right hidden md:table-cell">TO</th><th class="px-3 py-2 text-right hidden lg:table-cell">FG%%</th><th class="px-3 py-2 text-right hidden lg:table-cell">3P%%</th><th class="px-3 py-2 text-right hidden lg:table-cell">FT%%</th><th class="px-3 py-2 text-right hidden lg:table-cell">eFG%%</th><th class="px-3 py-2 text-right text-orange-400">EFF</th></tr></thead><tbody>%s</tbody></table></div>|html}
    min_label rows

let teams_page ~season ~seasons ~scope ~sort stats =
  let scope_value = team_scope_to_string scope in
  let scope_option value label = let selected = if scope_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let sort_option value label = let selected = if String.lowercase_ascii sort = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let season_options = let base = seasons |> List.map (fun s -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = teams_table ~scope stats in
  layout ~title:"WKBL Teams"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-white">Teams</h2><p class="text-slate-400 text-sm">Team aggregates by season and scope.</p></div><a class="text-orange-400 hover:text-orange-300 text-sm" href="/teams">Reset</a></div><form id="teams-filter" class="grid grid-cols-1 md:grid-cols-3 gap-3" hx-get="/teams/table" hx-target="#teams-table" hx-trigger="change"><select name="season" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><select name="scope" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s</select><select name="sort" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s%s%s%s</select></form><div id="teams-table">%s</div></div>|html}
      season_options (scope_option "per_game" "Per Game") (scope_option "totals" "Totals") (sort_option "pts" "PTS") (sort_option "reb" "REB") (sort_option "ast" "AST") (sort_option "stl" "STL") (sort_option "blk" "BLK") (sort_option "eff" "EFF") (sort_option "ts_pct" "TS%") (sort_option "fg3_pct" "3P%") (sort_option "min_total" "MIN") table)

let standings_table (standings : team_standing list) =
  let rows =
    standings
    |> List.mapi (fun i (s : team_standing) ->
        let win_pct_fmt = Printf.sprintf "%.3f" s.win_pct in
        let gb_fmt = if s.gb = 0.0 then "-" else Printf.sprintf "%.1f" s.gb in
        Printf.sprintf
          {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 font-mono text-sm">%d</td><td class="px-4 py-3 font-bold text-white flex items-center gap-2">%s<a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-right text-slate-300">%d</td><td class="px-4 py-3 text-right text-slate-300">%d</td><td class="px-4 py-3 text-right text-slate-300">%d</td><td class="px-4 py-3 text-right text-orange-400 font-bold">%s</td><td class="px-4 py-3 text-right text-slate-400">%s</td><td class="px-4 py-3 text-right text-slate-300 font-mono">%.1f</td><td class="px-4 py-3 text-right text-slate-300 font-mono">%.1f</td><td class="px-4 py-3 text-right %s font-mono font-bold">%.1f</td></tr>|html}
          (i + 1) (team_logo_tag ~class_name:"w-5 h-5" s.team_name) (Uri.pct_encode s.team_name) (escape_html s.team_name) s.games_played s.wins s.losses win_pct_fmt gb_fmt s.avg_pts s.avg_opp_pts (if s.diff >= 0.0 then "text-emerald-400" else "text-rose-400") s.diff)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden shadow-2xl"><table class="w-full text-sm font-mono"><thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider"><tr><th class="px-4 py-3 text-left w-12">Rk</th><th class="px-4 py-3 text-left font-sans">Team</th><th class="px-4 py-3 text-right">GP</th><th class="px-4 py-3 text-right">W</th><th class="px-4 py-3 text-right">L</th><th class="px-4 py-3 text-right">PCT</th><th class="px-4 py-3 text-right">GB</th><th class="px-4 py-3 text-right">PS/G</th><th class="px-4 py-3 text-right">PA/G</th><th class="px-4 py-3 text-right">DIFF</th></tr></thead><tbody id="standings-body">%s</tbody></table></div>|html}
    rows

let standings_page ~season ~seasons standings =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = standings_table standings in
  layout ~title:"WKBL Standings"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-white">Standings</h2><p class="text-slate-400 text-sm">League rank and win percentage.</p></div></div><form id="standings-filter" class="flex gap-3" hx-get="/standings/table" hx-target="#standings-table" hx-trigger="change"><select name="season" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="standings-table">%s</div></div>|html}
      season_options table)

let games_table (games : game_summary list) =
  let rows =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> string_of_int s | None -> "-" in
        let score_b = match g.away_score with Some s -> string_of_int s | None -> "-" in
        let status_class = if g.home_score = None then "text-slate-500" else "text-white" in
        Printf.sprintf
          {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors %s"><td class="px-4 py-3 text-slate-500 font-mono text-sm">%d</td><td class="px-4 py-3 text-slate-400 font-mono text-sm">%s</td><td class="px-4 py-3 font-medium flex items-center gap-2">%s<a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-center font-bold text-orange-400 font-mono">%s - %s</td><td class="px-4 py-3 text-right font-medium"><div class="flex items-center justify-end gap-2"><a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a>%s</div></td><td class="px-4 py-3 text-right"><a href="/boxscore/%s" class="text-xs bg-slate-800 hover:bg-slate-700 px-2 py-1 rounded text-slate-400 hover:text-white transition">Boxscore</a></td></tr>|html}
          status_class (i + 1) (escape_html g.game_date) (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) score_a score_b (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) (escape_html g.game_id))
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden shadow-2xl"><table class="w-full text-sm font-mono"><thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider"><tr><th class="px-4 py-3 text-left w-12">#</th><th class="px-4 py-3 text-left font-sans w-32">Date</th><th class="px-4 py-3 text-left font-sans">Home</th><th class="px-4 py-3 text-center w-32">Score</th><th class="px-4 py-3 text-right font-sans">Away</th><th class="px-4 py-3 text-right font-sans w-24">Action</th></tr></thead><tbody id="games-body">%s</tbody></table></div>|html}
    rows

let games_page ~season ~seasons games =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = games_table games in
  layout ~title:"WKBL Games"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-white">Games</h2><p class="text-slate-400 text-sm">Season schedule and results.</p></div></div><form id="games-filter" class="flex gap-3" hx-get="/games/table" hx-target="#games-table" hx-trigger="change"><select name="season" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="games-table">%s</div></div>|html}
      season_options table)

let boxscores_table (games : game_summary list) =
  let rows =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> s | None -> 0 in
        let score_b = match g.away_score with Some s -> s | None -> 0 in
        let margin = score_a - score_b in
        let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0" in
        let margin_color = if margin > 0 then "text-sky-400" else if margin < 0 then "text-orange-400" else "text-slate-400" in
        if g.home_score = None then ""
        else
          Printf.sprintf
            {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 font-mono text-sm">%d</td><td class="px-4 py-3 text-slate-400 font-mono text-sm">%s</td><td class="px-4 py-3 font-medium flex items-center gap-2">%s<a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-center font-bold text-white font-mono">%d</td><td class="px-4 py-3 text-center font-bold text-white font-mono">%d</td><td class="px-4 py-3 font-medium text-right"><div class="flex items-center justify-end gap-2"><a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a>%s</div></td><td class="px-4 py-3 text-right font-bold font-mono %s">%s</td><td class="px-4 py-3 text-right">
                  <a href="/boxscore/%s" class="text-xs bg-slate-800 hover:bg-slate-700 px-2 py-1 rounded text-slate-400 hover:text-white transition">View</a>
                </td></tr>|html}
            (i + 1) (escape_html g.game_date) (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) score_a score_b (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) margin_color margin_str (escape_html g.game_id))
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden shadow-2xl"><table class="w-full text-sm font-mono"><thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider"><tr><th class="px-4 py-3 text-left w-12">#</th><th class="px-4 py-3 text-left w-32 font-sans">Date</th><th class="px-4 py-3 text-left font-sans">Home</th><th class="px-4 py-3 text-center font-sans">PTS</th><th class="px-4 py-3 text-center font-sans">PTS</th><th class="px-4 py-3 text-right font-sans">Away</th><th class="px-4 py-3 text-right font-sans">Margin</th><th class="px-4 py-3 text-right w-20 font-sans">Link</th></tr></thead><tbody id="boxscores-body">%s</tbody></table></div>|html}
    rows

let boxscores_page ~season ~seasons games =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = boxscores_table games in
  layout ~title:"WKBL Boxscores"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-white">Boxscores</h2><p class="text-slate-400 text-sm">Game results and margins.</p></div></div><form id="boxscores-filter" class="flex gap-3" hx-get="/boxscores/table" hx-target="#boxscores-table" hx-trigger="change"><select name="season" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="boxscores-table">%s</div></div>|html}
      season_options table)

let boxscore_player_table (title: string) (players: boxscore_player_stat list) =
  let rows =
    players
    |> List.map (fun (p: boxscore_player_stat) ->
        let pos_badge =
          match p.bs_position with
          | Some pos when String.trim pos <> "" ->
              Printf.sprintf
                {html|<span class="ml-2 px-1.5 py-0.5 rounded bg-slate-800 text-slate-400 text-[10px] font-sans">%s</span>|html}
                (escape_html pos)
          | _ -> ""
        in
        let pm_class, pm_str =
          match p.bs_plus_minus with
          | None -> ("text-slate-500", "-")
          | Some v ->
              let cls =
                if v > 0 then "text-sky-400"
                else if v < 0 then "text-rose-400"
                else "text-slate-400"
              in
              let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
              (cls, s)
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors"><td class="px-3 py-2 font-medium text-white flex items-center gap-3">%s<a href="/player/%s" class="hover:text-orange-400 transition-colors">%s</a>%s</td><td class="px-3 py-2 text-right text-slate-400 font-mono text-sm w-[60px]">%.1f</td><td class="px-3 py-2 text-right text-white font-bold w-[60px]">%d</td><td class="px-3 py-2 text-right font-mono w-[60px] %s">%s</td><td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-400 text-xs font-mono w-[120px]">%d-%d (%.1f%%)</td><td class="px-3 py-2 text-right text-slate-400 text-xs font-mono w-[120px]">%d-%d (%.1f%%)</td><td class="px-3 py-2 text-right text-slate-400 text-xs font-mono w-[120px]">%d-%d (%.1f%%)</td></tr>|html}
          (player_img_tag ~class_name:"w-6 h-6" p.bs_player_id p.bs_player_name)
          p.bs_player_id
          (escape_html p.bs_player_name)
          pos_badge
          p.bs_minutes
          p.bs_pts
          pm_class
          (escape_html pm_str)
          p.bs_reb
          p.bs_ast
          p.bs_stl
          p.bs_blk
          p.bs_tov
          p.bs_fg_made
          p.bs_fg_att
          p.bs_fg_pct
          p.bs_fg3_made
          p.bs_fg3_att
          p.bs_fg3_pct
          p.bs_ft_made
          p.bs_ft_att
          p.bs_ft_pct)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="space-y-3"><h3 class="text-lg font-bold text-white flex items-center gap-2"><span class="w-1 h-6 bg-orange-500 rounded-full"></span>%s</h3><div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden shadow-lg"><table class="w-full text-sm font-mono table-fixed"><thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider"><tr><th class="px-3 py-2 text-left font-sans">Player</th><th class="px-3 py-2 text-right w-[60px]">MP</th><th class="px-3 py-2 text-right text-orange-400 w-[60px]">PTS</th><th class="px-3 py-2 text-right w-[60px]">+/-</th><th class="px-3 py-2 text-right w-[60px]">TRB</th><th class="px-3 py-2 text-right w-[60px]">AST</th><th class="px-3 py-2 text-right w-[60px]">STL</th><th class="px-3 py-2 text-right w-[60px]">BLK</th><th class="px-3 py-2 text-right w-[60px]">TOV</th><th class="px-3 py-2 text-right w-[120px]">FG</th><th class="px-3 py-2 text-right w-[120px]">3P</th><th class="px-3 py-2 text-right w-[120px]">FT</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
    (escape_html title) rows

let boxscore_page (bs: game_boxscore) =
  let gi = bs.boxscore_game in
  let margin = gi.gi_home_score - gi.gi_away_score in
  let margin_str =
    if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0"
  in
  let margin_class =
    if margin > 0 then "text-sky-400"
    else if margin < 0 then "text-orange-400"
    else "text-slate-400"
  in
  let margin_badge =
    Printf.sprintf
      {html|<span class="px-2 py-1 rounded-full bg-slate-800 border border-slate-700 text-[10px] font-mono tracking-wider %s">MARGIN %s</span>|html}
      margin_class
      (escape_html margin_str)
  in
  let home_table = boxscore_player_table gi.gi_home_team_name bs.boxscore_home_players in
  let away_table = boxscore_player_table gi.gi_away_team_name bs.boxscore_away_players in
  layout ~title:(Printf.sprintf "Boxscore: %s vs %s" gi.gi_home_team_name gi.gi_away_team_name)
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="bg-slate-900 rounded-xl border border-slate-800 p-6 shadow-2xl"><div class="flex flex-col items-center gap-6"><div class="text-slate-500 font-mono text-sm uppercase tracking-widest">%s</div><div class="flex items-center justify-between w-full max-w-2xl"><div class="flex flex-col items-center gap-3"><div class="text-2xl font-black text-white flex items-center gap-3">%s<a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a></div><div class="text-slate-400 text-sm">HOME</div></div><div class="flex items-center gap-8"><div class="text-5xl font-black text-white">%d</div><div class="flex flex-col items-center gap-2"><div class="text-2xl text-slate-700 font-light">vs</div>%s</div><div class="text-5xl font-black text-white">%d</div></div><div class="flex flex-col items-center gap-3"><div class="text-2xl font-black text-white flex items-center gap-3"><a href="/team/%s" class="hover:text-orange-400 transition-colors">%s</a>%s</div><div class="text-slate-400 text-sm">AWAY</div></div></div></div></div><details class="max-w-2xl mx-auto bg-slate-900/50 rounded-lg border border-slate-800 p-4 text-xs text-slate-500"><summary class="cursor-pointer font-bold text-slate-300 select-none">득실마진 / 데이터 안내</summary><div class="mt-2 space-y-1 leading-relaxed"><div><span class="font-mono text-slate-200">MARGIN</span>은 경기 최종 득점(팀-상대)입니다.</div><div>개인 <span class="font-mono text-slate-200">+/-</span>는 문자중계(PBP) 기반이며, 데이터가 없거나 PBP/박스스코어 최종 스코어 불일치 등 품질 이슈가 있으면 <span class="font-mono text-slate-200">-</span>로 표시합니다.</div><div>동명이인 매칭 오류로 동일 스탯 라인이 중복될 수 있어, 동일 라인은 1개만 표시합니다.</div></div></details><div class="grid grid-cols-1 gap-8">%s%s</div><div class="flex justify-center"><a href="/games" class="text-slate-500 hover:text-orange-500 transition text-sm">← Back to Games</a></div></div>|html}
      (escape_html gi.gi_game_date)
      (team_logo_tag ~class_name:"w-10 h-10" gi.gi_home_team_name) (Uri.pct_encode gi.gi_home_team_name) (escape_html gi.gi_home_team_name)
      gi.gi_home_score
      margin_badge
      gi.gi_away_score
      (Uri.pct_encode gi.gi_away_team_name) (escape_html gi.gi_away_team_name) (team_logo_tag ~class_name:"w-10 h-10" gi.gi_away_team_name)
      home_table away_table)

let compare_stat_row ?(signed=false) label val1 val2 =
  let max_val = max (abs_float val1) (abs_float val2) in
  let pct1 = if max_val = 0.0 then 0.0 else abs_float val1 /. max_val *. 100.0 in
  let pct2 = if max_val = 0.0 then 0.0 else abs_float val2 /. max_val *. 100.0 in
  let value_str v =
    if signed && v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v
  in
  Printf.sprintf
    {html|<div class="flex flex-col gap-1"><div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-500"><span>%s</span><span class="text-slate-400">%s</span><span>%s</span></div><div class="flex h-2 bg-slate-800 rounded-full overflow-hidden"><div class="flex justify-end w-1/2 border-r border-slate-700"><div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div><div class="flex justify-start w-1/2"><div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div></div></div>|html}
    (escape_html (value_str val1)) (escape_html label) (escape_html (value_str val2)) pct1 pct2

let h2h_game_row (g: h2h_game) =
  Printf.sprintf
    {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 font-mono transition-colors">
      <td class="px-3 py-2 text-slate-400 text-xs w-[100px] truncate">%s</td>
      <td class="px-3 py-2 text-right text-white w-[60px] font-bold">%d</td>
      <td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-right text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-center text-slate-500 text-xs font-sans w-[40px]">vs</td>
      <td class="px-3 py-2 text-left text-white w-[60px] font-bold">%d</td>
      <td class="px-3 py-2 text-left text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-left text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-right text-xs font-sans w-[80px]"><span class="px-1.5 py-0.5 rounded bg-slate-800 text-slate-400">DIFF %d</span></td>
    </tr>|html}
    (escape_html g.game_date) g.player1_pts g.player1_reb g.player1_ast g.player2_pts g.player2_reb g.player2_ast g.score_diff

let h2h_game_table (p1_name: string) (p2_name: string) (games: h2h_game list) =
  let rows = games |> List.map h2h_game_row |> String.concat "\n" in
  Printf.sprintf {html|<div class="space-y-3 mt-8"><h3 class="text-center text-slate-500 text-sm font-bold uppercase tracking-widest">Match History</h3><div class="bg-slate-900 rounded-xl border border-slate-800 overflow-hidden shadow-xl"><table class="w-full text-sm table-fixed"><thead class="bg-slate-800/80 text-slate-500 uppercase text-[10px] tracking-tighter"><tr><th class="px-3 py-2 text-left font-sans w-[100px]">Date</th><th class="px-3 py-2 text-right font-sans text-orange-400 w-[60px]">%s PTS</th><th class="px-3 py-2 text-right font-sans w-[60px]">REB</th><th class="px-3 py-2 text-right font-sans w-[60px]">AST</th><th class="px-3 py-2 w-[40px]"></th><th class="px-3 py-2 text-left font-sans text-sky-400 w-[60px]">%s PTS</th><th class="px-3 py-2 text-left font-sans w-[60px]">REB</th><th class="px-3 py-2 text-left font-sans w-[60px]">AST</th><th class="px-3 py-2 w-[80px]"></th></tr></thead><tbody>%s</tbody></table></div></div>|html} (escape_html p1_name) (escape_html p2_name) rows

let compare_page (p1: player_aggregate option) (p2: player_aggregate option) (h2h: h2h_game list) =
  let content =
    match p1, p2 with
    | Some a, Some b ->
        Printf.sprintf
          {html|<div class="space-y-8 animate-fade-in"><div class="grid grid-cols-1 md:grid-cols-3 gap-8 items-start"><div class="bg-slate-900 rounded-xl border border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-orange-500">%s<div class="text-center"><div class="text-2xl font-black text-white hover:text-orange-400"><a href="/player/%s">%s</a></div><div class="text-slate-400">%s</div></div></div><div class="bg-slate-900/50 rounded-xl border border-slate-800 p-6 space-y-6"><div class="text-center space-y-2"><h3 class="text-slate-500 text-sm font-bold uppercase">Average Stats</h3><p class="text-[11px] text-slate-500 leading-relaxed"><span class="font-mono">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 +/-는 문자중계(PBP) 기반으로 일부 경기에서만 제공됩니다. (데이터가 없으면 -)</p></div>%s%s%s%s%s%s%s%s</div><div class="bg-slate-900 rounded-xl border border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-sky-500">%s<div class="text-center"><div class="text-2xl font-black text-white hover:text-sky-400"><a href="/player/%s">%s</a></div><div class="text-slate-400">%s</div></div></div></div>%s</div>|html}
          (player_img_tag ~class_name:"w-32 h-32" a.player_id a.name) a.player_id (escape_html a.name) (escape_html a.team_name) (compare_stat_row "Points" a.avg_points b.avg_points) (compare_stat_row ~signed:true "MG" a.avg_margin b.avg_margin) (compare_stat_row "Rebounds" a.avg_rebounds b.avg_rebounds) (compare_stat_row "Assists" a.avg_assists b.avg_assists) (compare_stat_row "Steals" a.avg_steals b.avg_steals) (compare_stat_row "Blocks" a.avg_blocks b.avg_blocks) (compare_stat_row "Turnovers" a.avg_turnovers b.avg_turnovers) (compare_stat_row "Efficiency" a.efficiency b.efficiency) (player_img_tag ~class_name:"w-32 h-32" b.player_id b.name) b.player_id (escape_html b.name) (escape_html b.team_name) (h2h_game_table a.name b.name h2h)
    | _ -> {html|<div class="text-center py-20 bg-slate-900 rounded-xl border border-slate-800"><div class="text-4xl mb-4">🔍</div><h3 class="text-xl font-bold text-white">Compare Players</h3><p class="text-slate-400 mt-2">Enter player names in the URL query (e.g., ?p1=김단비&p2=박지현)</p></div>|html}
  in
  layout ~title:"Player Comparison" ~content:(Printf.sprintf {html|<div class="space-y-8"><div class="flex justify-between items-end"><div><h2 class="text-3xl font-black text-white">Head to Head</h2><p class="text-slate-400">Side-by-side performance analysis.</p></div></div>%s</div>|html} content)

let prediction_result_card ~(home: string) ~(away: string) (output: prediction_output) =
  let pct value = value *. 100.0 in
  let result = output.result in
  let breakdown = output.breakdown in
  let home_pct = pct result.prob_a in
  let away_pct = pct result.prob_b in
  let elo_home_pct = pct breakdown.pb_elo_prob in
  let elo_away_pct = pct (1.0 -. breakdown.pb_elo_prob) in
  let pyth_home_pct = pct breakdown.pb_pyth_prob in
  let pyth_away_pct = pct (1.0 -. breakdown.pb_pyth_prob) in
  let stats_home_pct = pct breakdown.pb_stats_prob in
  let stats_away_pct = pct (1.0 -. breakdown.pb_stats_prob) in
  let context_card_html, context_note_html =
    match breakdown.pb_context with
    | None ->
        ("", {html|<div>부상/로스터/일정/전술/PBP 등의 컨텍스트는 반영하지 않습니다.</div>|html})
    | Some ctx ->
        let delta_pp = ctx.pcb_delta *. 100.0 in
        let delta_cls =
          if delta_pp > 0.0 then "text-sky-400"
          else if delta_pp < 0.0 then "text-rose-400"
          else "text-slate-300"
        in
        let delta_str =
          if delta_pp > 0.0 then Printf.sprintf "+%.1f%%p" delta_pp else Printf.sprintf "%.1f%%p" delta_pp
        in
        let form_home_pct = pct ctx.pcb_form_home in
        let form_away_pct = pct ctx.pcb_form_away in
        let roster_text =
          match ctx.pcb_roster_home, ctx.pcb_roster_away with
          | Some h, Some a ->
              Printf.sprintf "%d/%d vs %d/%d" h.rcs_present h.rcs_total a.rcs_present a.rcs_total
          | _ -> "-"
        in
        let rest_text =
          match ctx.pcb_rest_home_days, ctx.pcb_rest_away_days with
          | Some h, Some a -> Printf.sprintf "%dd vs %dd" h a
          | _ -> "-"
        in
        ( Printf.sprintf
            {html|<div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3">
              <div class="flex items-center justify-between">
                <div class="text-slate-500 font-mono uppercase tracking-widest">CONTEXT</div>
                <div class="text-[10px] font-mono %s">%s</div>
              </div>
              <div class="mt-2 space-y-1 text-[10px] text-slate-500 font-mono">
                <div class="flex justify-between"><span>FORM (L5 win%%)</span><span class="text-slate-300">%.0f%% vs %.0f%%</span></div>
                <div class="flex justify-between"><span>ROSTER (core)</span><span class="text-slate-300">%s</span></div>
                <div class="flex justify-between"><span>REST</span><span class="text-slate-300">%s</span></div>
              </div>
            </div>|html}
            delta_cls
            (escape_html delta_str)
            form_home_pct
            form_away_pct
            (escape_html roster_text)
            (escape_html rest_text),
          {html|<div>컨텍스트(최근 5경기 폼/코어 로스터/휴식)를 Δ로 소폭 반영합니다. (최대 ±8%p)</div><div class="text-slate-500">로스터는 “마지막 경기 출전” 기준으로 추정하며, 부상/전술/PBP 컨텍스트는 여전히 미반영입니다.</div>|html}
        )
  in
  let winner_class =
    if normalize_label result.winner = normalize_label home then "text-orange-400"
    else "text-sky-400"
  in
  Printf.sprintf
    {html|<div class="bg-slate-900 rounded-xl border border-slate-800 p-6 shadow-xl space-y-4">
      <div class="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-4">
        <div class="min-w-0">
          <div class="text-sm text-slate-500 font-mono uppercase tracking-widest">Prediction</div>
          <div class="mt-1 text-2xl font-black text-white">%s</div>
          <div class="mt-1 text-xs text-slate-500 truncate">%s%s vs %s%s</div>
        </div>
        <div class="text-right shrink-0">
          <div class="text-xs text-slate-500">Winner</div>
          <div class="text-lg font-black %s">%s</div>
        </div>
      </div>
      <div class="space-y-2">
        <div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-500">
          <span class="text-slate-300">%s</span>
          <span class="text-slate-300">%s</span>
        </div>
        <div class="flex h-2 bg-slate-800 rounded-full overflow-hidden">
          <div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div>
          <div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div>
        </div>
        <div class="flex justify-between font-mono text-sm">
          <span class="text-orange-400 font-bold">%.1f%%</span>
          <span class="text-sky-400 font-bold">%.1f%%</span>
        </div>
      </div>
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-3 text-xs">
        <div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3">
          <div class="flex items-center justify-between">
            <div class="text-slate-500 font-mono uppercase tracking-widest">ELO</div>
            <div class="text-[10px] text-slate-500 font-mono">%d games</div>
          </div>
          <div class="mt-2 flex items-center justify-between font-mono">
            <div class="text-orange-300">%.0f</div>
            <div class="text-sky-300">%.0f</div>
          </div>
          <div class="mt-1 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-400">%.1f%%</div>
            <div class="text-sky-400">%.1f%%</div>
          </div>
        </div>
        <div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3">
          <div class="text-slate-500 font-mono uppercase tracking-widest">PYTHAG</div>
          <div class="mt-2 flex items-center justify-between font-mono">
            <div class="text-orange-300">%.3f</div>
            <div class="text-sky-300">%.3f</div>
          </div>
          <div class="mt-1 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-400">%.1f%%</div>
            <div class="text-sky-400">%.1f%%</div>
          </div>
        </div>
        <div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3">
          <div class="text-slate-500 font-mono uppercase tracking-widest">STATS</div>
          <div class="mt-2 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-400">%.1f%%</div>
            <div class="text-sky-400">%.1f%%</div>
          </div>
          <div class="mt-1 text-[10px] text-slate-500 font-mono">Win%% + EFF blend</div>
        </div>
        %s
      </div>
      <details class="bg-slate-900/50 rounded-lg border border-slate-800/50 p-4 text-xs text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-300 select-none">예측 안내</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-200">Final</span> = 0.60×Elo + 0.25×Pythag(Log5) + 0.15×Stats.</div>
          <div><span class="font-mono text-slate-200">Elo</span>는 경기 결과(득점차 반영)로 레이팅을 업데이트합니다.</div>
          <div><span class="font-mono text-slate-200">Pythag</span>는 득실 기반 기대 승률이며 Log5로 매치업 확률을 계산합니다.</div>
          <div><span class="font-mono text-slate-200">Stats</span>는 승률과 EFF를 단순 결합합니다.</div>
          <div>중립경기(Neutral)면 홈 어드밴티지는 예측에 반영하지 않습니다.</div>
          %s
        </div>
      </details>
    </div>|html}
    (escape_html (Printf.sprintf "%.1f%% - %.1f%%" home_pct away_pct))
    (escape_html home)
    (if breakdown.pb_is_neutral then " (Neutral)" else " (Home)")
    (escape_html away)
    (if breakdown.pb_is_neutral then " (Neutral)" else " (Away)")
    winner_class
    (escape_html result.winner)
    (escape_html home)
    (escape_html away)
    home_pct
    away_pct
    home_pct
    away_pct
    breakdown.pb_games_used
    breakdown.pb_elo_home
    breakdown.pb_elo_away
    elo_home_pct
    elo_away_pct
    breakdown.pb_pyth_home
    breakdown.pb_pyth_away
    pyth_home_pct
    pyth_away_pct
    stats_home_pct
    stats_away_pct
    context_card_html
    context_note_html

let predict_page ~season ~seasons ~teams ~home ~away ~is_neutral ~context_enabled (result: prediction_output option) (error: string option) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s : season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let team_option current name =
    let selected = if normalize_label current = normalize_label name then "selected" else "" in
    Printf.sprintf {html|<option value="%s" %s>%s</option>|html} (escape_html name) selected (escape_html name)
  in
  let team_options current =
    let base = teams |> List.map (team_option current) |> String.concat "\n" in
    Printf.sprintf {html|<option value="" %s>Select team…</option>%s|html} (if String.trim current = "" then "selected" else "") base
  in
  let result_html =
    match result, error with
    | Some r, _ -> prediction_result_card ~home ~away r
    | None, Some msg ->
        Printf.sprintf
          {html|<div class="bg-rose-500/10 border border-rose-500/30 text-rose-300 rounded-xl p-5">%s</div>|html}
          (escape_html msg)
    | None, None ->
        {html|<div class="text-slate-500 text-sm">Select teams to see a prediction.</div>|html}
  in
  layout ~title:"WKBL Predict"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
          <div>
            <h2 class="text-3xl font-black text-white">Match Prediction</h2>
            <p class="text-slate-400 text-sm">농구광 모드: Elo + Pythagorean + Stats 근거를 함께 보여줍니다.</p>
          </div>
        </div>
        <form action="/predict" method="get" class="grid grid-cols-1 md:grid-cols-3 gap-3 bg-slate-900 rounded-xl border border-slate-800 p-4">
          <select name="season" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <select name="home" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <select name="away" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-400">
            <input type="checkbox" name="context" value="1" class="accent-orange-500" %s>
            컨텍스트 반영 (폼/로스터/휴식)
          </label>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-400">
            <input type="checkbox" name="neutral" value="1" class="accent-orange-500" %s>
            Neutral site (no home advantage)
          </label>
          <div class="md:col-span-3 flex justify-end">
            <button type="submit" class="bg-orange-500 hover:bg-orange-400 text-black font-bold px-4 py-2 rounded text-sm transition">Predict</button>
          </div>
        </form>
        %s
      </div>|html}
      season_options
      (team_options home)
      (team_options away)
      (if context_enabled then "checked" else "")
      (if is_neutral then "checked" else "")
      result_html)

let leader_card title (leaders: leader_entry list) =
  if leaders = [] then ""
  else
    let top = List.hd leaders in
    let others = List.tl leaders in
    let others_rows =
      others
      |> List.mapi (fun i l -> Printf.sprintf {html|<div class="flex items-center justify-between py-2 border-b border-slate-800/60 last:border-0"><div class="flex items-center gap-3"><span class="text-slate-500 font-mono text-sm w-4">%d</span>%s<div class="flex flex-col"><div class="text-sm font-medium text-slate-300"><a href="/player/%s" class="hover:text-orange-400 transition-colors">%s</a></div><div class="text-xs text-slate-500">%s</div></div></div><div class="font-mono font-bold text-slate-400">%.1f</div></div>|html} (i + 2) (player_img_tag ~class_name:"w-8 h-8" l.le_player_id l.le_player_name) l.le_player_id (escape_html l.le_player_name) (escape_html l.le_team_name) l.le_stat_value)
      |> String.concat "\n"
    in
    Printf.sprintf {html|<div class="bg-slate-900 rounded-xl border border-slate-800 p-5 shadow-lg"><h3 class="text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">%s</h3><div class="flex items-center gap-4 mb-6 pb-6 border-b border-slate-800"><div class="relative">%s<div class="absolute -top-2 -right-2 bg-orange-500 text-white text-xs font-bold w-6 h-6 rounded-full flex items-center justify-center border-2 border-slate-900">1</div></div><div><div class="text-3xl font-black text-white">%.1f</div><div class="font-bold text-orange-400"><a href="/player/%s" class="hover:text-white transition-colors">%s</a></div><div class="text-xs text-slate-500">%s</div></div></div><div class="space-y-1">%s</div></div>|html} (escape_html title) (player_img_tag ~class_name:"w-16 h-16" top.le_player_id top.le_player_name) top.le_stat_value top.le_player_id (escape_html top.le_player_name) (escape_html top.le_team_name) others_rows

let leaders_page ~season ~seasons ~scope pts reb ast stl blk =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let scope_options = let opt v l = let sel = if scope = v then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} v sel l in opt "per_game" "Per Game" ^ opt "per_36" "Per 36 Min" in
  let content = Printf.sprintf {html|<div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">%s%s%s%s%s</div>|html} (leader_card "Points" pts) (leader_card "Rebounds" reb) (leader_card "Assists" ast) (leader_card "Steals" stl) (leader_card "Blocks" blk) in
  layout ~title:"WKBL Leaders" ~content:(Printf.sprintf {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-white">League Leaders</h2><p class="text-slate-400">Top performers by category.</p></div><form action="/leaders" method="get" class="flex gap-3"><select name="season" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-40" onchange="this.form.submit()">%s</select><select name="scope" class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-40" onchange="this.form.submit()">%s</select></form></div>%s</div>|html} season_options scope_options content)

let player_profile_page (profile: player_profile) ~scope =
  let p = profile.player in
  let pos = match p.position with Some s -> s | None -> "-" in
  let info_text = Printf.sprintf "%s | %dcm" pos (match p.height with Some h -> h | None -> 0) in
  let avg = profile.averages in
  let career_chips =
    if avg.games_played <= 0 then ""
    else
      Printf.sprintf
        {html|<span class="bg-slate-800/60 text-slate-300 px-3 py-1 rounded text-sm font-mono">GP %d</span><span class="bg-slate-800/60 text-orange-400 px-3 py-1 rounded text-sm font-mono">PTS %.1f</span><span class="bg-slate-800/60 text-slate-300 px-3 py-1 rounded text-sm font-mono">REB %.1f</span><span class="bg-slate-800/60 text-slate-300 px-3 py-1 rounded text-sm font-mono">AST %.1f</span>|html}
        avg.games_played
        avg.avg_points
        avg.avg_rebounds
        avg.avg_assists
  in
  let team_badge_html =
    let t = String.trim avg.team_name in
    if t = "" then "" else team_badge t
  in
  let video_links_html =
    let name = normalize_label p.name in
    let team = String.trim avg.team_name |> normalize_label in
    let base_query = if team = "" then name else Printf.sprintf "%s %s" name team in
    let youtube =
      "https://www.youtube.com/results?search_query="
      ^ Uri.pct_encode (Printf.sprintf "%s WKBL 하이라이트" base_query)
    in
    let instagram =
      "https://www.google.com/search?q="
      ^ Uri.pct_encode (Printf.sprintf "site:instagram.com %s WKBL 인스타" base_query)
    in
    Printf.sprintf
      {html|<a href="%s" target="_blank" rel="noopener noreferrer" class="bg-slate-800/60 text-slate-300 px-3 py-1 rounded text-sm font-mono hover:bg-slate-700/60 hover:text-white transition">▶ 영상</a><a href="%s" target="_blank" rel="noopener noreferrer" class="bg-slate-800/60 text-slate-300 px-3 py-1 rounded text-sm font-mono hover:bg-slate-700/60 hover:text-white transition">인스타</a>|html}
      (escape_html youtube) (escape_html instagram)
  in
  let game_rows games =
    games
    |> List.map (fun (g: player_game_stat) ->
        let res_color = if g.pts >= 20 then "text-orange-400" else "text-slate-300" in
        let pm_class, pm_str =
          match g.plus_minus with
          | None -> ("text-slate-500", "-")
          | Some v ->
              let cls =
                if v > 0 then "text-sky-400"
                else if v < 0 then "text-rose-400"
                else "text-slate-400"
              in
              let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
              (cls, s)
        in
        let margin_badge =
          match g.team_score, g.opponent_score with
          | Some team_score, Some opponent_score ->
              let margin = team_score - opponent_score in
              let cls =
                if margin > 0 then "bg-sky-500/10 text-sky-400 border-sky-500/30"
                else if margin < 0 then "bg-rose-500/10 text-rose-400 border-rose-500/30"
                else "bg-slate-500/10 text-slate-300 border-slate-500/30"
              in
              let label =
                if margin > 0 then Printf.sprintf "W +%d" margin
                else if margin < 0 then Printf.sprintf "L %d" margin
                else "T 0"
              in
              Printf.sprintf
                {html|<span class="inline-flex items-center px-2 py-0.5 rounded border text-[10px] font-mono %s">%s</span>|html}
                cls label
          | _ ->
              {html|<span class="inline-flex items-center px-2 py-0.5 rounded border border-slate-700/60 text-[10px] font-mono text-slate-500">-</span>|html}
        in
        let opponent_label = if g.is_home then "vs " ^ g.opponent else "@ " ^ g.opponent in
        Printf.sprintf
          {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-400 text-sm font-mono whitespace-nowrap"><a href="/boxscore/%s" class="hover:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-white"><div class="flex items-center justify-between gap-3"><span class="truncate">%s</span>%s</div></td><td class="px-4 py-3 text-right font-mono text-slate-400 w-[72px]">%.1f</td><td class="px-4 py-3 text-right font-bold %s w-[72px]">%d</td><td class="px-4 py-3 text-right font-mono w-[72px] %s">%s</td><td class="px-4 py-3 text-right text-slate-300 w-[72px]">%d</td><td class="px-4 py-3 text-right text-slate-300 w-[72px]">%d</td><td class="px-4 py-3 text-right text-slate-300 w-[72px]">%d</td><td class="px-4 py-3 text-right text-slate-300 w-[72px]">%d</td></tr>|html}
          (Uri.pct_encode g.game_id)
          (escape_html g.game_date)
          (escape_html opponent_label)
          margin_badge
          g.min
          res_color
          g.pts
          pm_class
          (escape_html pm_str)
          g.reb
          g.ast
          g.stl
          g.blk)
    |> String.concat "\n"
  in
  let recent_rows = game_rows profile.recent_games in
  let all_star_rows = game_rows profile.all_star_games in
  let season_stats_component = player_season_stats_component ~player_id:p.id ~scope profile.season_breakdown in
  let all_star_section_html =
    if profile.all_star_games = [] then
      ""
    else
      Printf.sprintf
        {html|<div class="space-y-4"><div class="flex flex-col gap-1"><h3 class="text-xl font-bold text-white">All-Star Games</h3><p class="text-[11px] text-slate-500">올스타전(<span class="font-mono text-slate-300">game_type=10</span>) 기록은 시즌/커리어 테이블에서 제외하고 여기서만 보여줍니다.</p></div><div class="bg-slate-900 rounded-xl border border-slate-800 overflow-x-auto shadow-lg"><table class="min-w-[920px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-800/80 text-slate-400 uppercase tracking-wider text-xs"><tr><th class="px-4 py-3 text-left font-sans w-[110px] whitespace-nowrap">Date</th><th class="px-4 py-3 text-left font-sans">Opponent</th><th class="px-4 py-3 text-right w-[72px]">MIN</th><th class="px-4 py-3 text-right text-orange-400 w-[72px]">PTS</th><th class="px-4 py-3 text-right w-[72px]">+/-</th><th class="px-4 py-3 text-right w-[72px]">REB</th><th class="px-4 py-3 text-right w-[72px]">AST</th><th class="px-4 py-3 text-right w-[72px]">STL</th><th class="px-4 py-3 text-right w-[72px]">BLK</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
        all_star_rows
  in

  let team_movement_html =
    match profile.team_stints with
    | [] -> ""
    | stints ->
        let stints_desc = List.rev stints in
        let current_team_html =
          let t = String.trim avg.team_name in
          if t = "" then {html|<span class="text-slate-500">-</span>|html}
          else Printf.sprintf {html|<div class="shrink-0">%s</div>|html} (team_badge t)
        in
        let transfers = max 0 (List.length stints - 1) in
        let last_move_value_html =
          match stints_desc with
          | current :: prev :: _ when prev.pts_team_name <> current.pts_team_name ->
              Printf.sprintf
                {html|<div class="flex items-center gap-2 min-w-0 flex-wrap"><span class="shrink-0">%s</span><span class="text-slate-600">→</span><span class="shrink-0">%s</span><span class="text-slate-500 font-mono text-[11px] whitespace-nowrap shrink-0">(%s~)</span></div>|html}
                (team_badge prev.pts_team_name)
                (team_badge current.pts_team_name)
                (escape_html current.pts_start_date)
          | _ -> {html|<span class="text-slate-500">-</span>|html}
        in
        let stint_rows =
          stints_desc
          |> List.map (fun (s: player_team_stint) ->
              let range =
                if s.pts_start_date = s.pts_end_date then s.pts_start_date
                else Printf.sprintf "%s ~ %s" s.pts_start_date s.pts_end_date
              in
              Printf.sprintf
                {html|<div class="py-3"><div class="flex items-center justify-between gap-3"><div class="shrink-0">%s</div><div class="text-xs text-slate-400 font-mono shrink-0 sm:hidden">GP %d</div></div><div class="mt-2 grid grid-cols-[1fr,auto] items-center gap-3"><div class="text-[11px] text-slate-500 font-mono leading-relaxed break-keep">%s</div><div class="hidden sm:block text-xs text-slate-400 font-mono shrink-0">GP %d</div></div></div>|html}
                (team_badge s.pts_team_name)
                s.pts_games_played
                (escape_html range)
                s.pts_games_played)
          |> String.concat "\n"
        in
        Printf.sprintf
          {html|<div class="bg-slate-900 rounded-xl border border-slate-800 p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><h3 class="text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🔁</span> Team Movement</h3><span class="text-[11px] text-slate-500 font-mono">박스스코어</span></div><div class="grid grid-cols-1 sm:grid-cols-3 gap-3 text-xs"><div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3"><div class="text-slate-500 font-mono uppercase tracking-widest text-[11px]">Current</div><div class="mt-2 text-slate-300">%s</div></div><div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3"><div class="text-slate-500 font-mono uppercase tracking-widest text-[11px]">Transfers</div><div class="mt-2 font-mono text-slate-200 text-lg font-black">%d</div></div><div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-3"><div class="text-slate-500 font-mono uppercase tracking-widest text-[11px]">Latest</div><div class="mt-2 text-slate-300">%s</div></div></div><div class="mt-4 rounded-lg border border-slate-800/60 bg-slate-950/30 divide-y divide-slate-800/60">%s</div><div class="mt-4 pt-3 border-t border-slate-800/60 text-[11px] text-slate-500 leading-relaxed">팀 이동은 공식 이적 기록이 아니라, <span class="font-mono text-slate-300">박스스코어 출전팀</span> 변화로 추정합니다.</div></div>|html}
          current_team_html transfers last_move_value_html stint_rows
  in

  let career_highs_html = career_highs_card profile.career_highs in
  let data_notes_html =
    let seasons = profile.season_breakdown in
    let seasons_value_html, show_backfill =
      match seasons with
      | [] -> ({html|<span class="font-mono text-slate-500">-</span>|html}, false)
      | latest :: _ ->
          let count = List.length seasons in
          let rec last = function
            | [] -> None
            | [x] -> Some x
            | _ :: xs -> last xs
          in
          let oldest = last seasons |> Option.value ~default:latest in
          (Printf.sprintf
             {html|<span class="font-mono text-slate-300">%s</span><span class="mx-1 text-slate-600">→</span><span class="font-mono text-slate-300">%s</span>|html}
             (escape_html oldest.ss_season_name)
             (escape_html latest.ss_season_name),
           count <= 3)
    in
    let backfill_row_html =
      if show_backfill then
        {html|<div class="text-slate-500 leading-relaxed">아직 데이터가 3시즌 정도만 있어요. 더 채우려면 아래를 실행하세요:</div><code class="mt-2 block font-mono text-slate-300 bg-slate-800/40 border border-slate-700/60 px-2 py-1 rounded break-all">python3 scripts/wkbl_refresh_all.py -- --years 10</code>|html}
      else
        ""
    in
    let seasons_card_html =
      Printf.sprintf
        {html|<div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-4"><div class="flex items-center justify-between gap-3"><div class="text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🗓</span> Seasons</div><div class="text-[11px] text-slate-500 font-mono">n=%d</div></div><div class="mt-2 text-slate-200 font-mono">%s</div>%s</div>|html}
        (List.length seasons)
        seasons_value_html
        (if backfill_row_html = "" then "" else Printf.sprintf {html|<details class="mt-3 text-[11px] text-slate-500"><summary class="cursor-pointer select-none text-slate-400 font-bold">Backfill</summary><div class="mt-2">%s</div></details>|html} backfill_row_html)
    in
    let pbp_card_html =
      {html|<div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-4"><div class="flex items-center justify-between gap-3"><div class="text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🎥</span> PBP +/-</div><span class="px-2 py-0.5 rounded bg-slate-800/60 border border-slate-700/60 text-[10px] font-mono text-slate-300">부분</span></div><div class="mt-2 text-slate-400 text-xs leading-relaxed">개인 <span class="font-mono text-slate-300">+/-</span>는 문자중계(PBP) 기반이라 <span class="text-slate-300 font-bold">일부 경기만</span> 제공됩니다. 데이터가 없거나 PBP/박스스코어 최종 스코어 불일치 등 품질 이슈가 있으면 <span class="font-mono text-slate-300">-</span>로 표시합니다.</div></div>|html}
    in
    let draft_card_html =
      {html|<div class="bg-slate-800/40 border border-slate-700/50 rounded-lg p-4"><div class="flex items-center justify-between gap-3"><div class="text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🧩</span> Draft / Trade</div><span class="px-2 py-0.5 rounded bg-slate-800/60 border border-slate-700/60 text-[10px] font-mono text-slate-300">예정</span></div><div class="mt-2 text-slate-400 text-xs leading-relaxed">공식 드래프트/이적 기록은 아직 수집 중입니다. 현재는 <span class="font-mono text-slate-300">박스스코어 출전팀</span> 변화로 팀 이동을 추정합니다. (공식 페이지 기반 추가 수집 필요)</div></div>|html}
    in
    Printf.sprintf
      {html|<div class="bg-slate-900 rounded-xl border border-slate-800 p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><h3 class="text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🧾</span> Data Notes</h3><span class="text-[11px] text-slate-500 font-mono">커버리지</span></div><div class="grid grid-cols-1 sm:grid-cols-2 gap-3">%s%s</div><div class="mt-3">%s</div></div>|html}
      seasons_card_html
      pbp_card_html
      draft_card_html
  in
  let missing_data_html =
    if profile.season_breakdown = [] && profile.recent_games = [] && profile.all_star_games = [] then
      {html|<div class="bg-slate-900/50 rounded-xl border border-slate-800/50 p-6 flex items-center justify-center text-slate-500 text-sm gap-2"><span class="text-xl">🚧</span><div><div class="font-bold">Data Collection in Progress</div><div>Draft info and play-by-play data coming soon.</div></div></div>|html}
    else
      ""
  in

  layout ~title:(p.name ^ " | WKBL Profile")
    ~content:(Printf.sprintf {html|<div class="space-y-8 animate-fade-in"><div class="bg-slate-900 rounded-xl border border-slate-800 p-8 shadow-2xl flex flex-col md:flex-row items-center md:items-start gap-8"><div class="relative">%s<div class="absolute -bottom-3 -right-3 bg-slate-800 border border-slate-700 text-white text-xs font-bold px-3 py-1 rounded-full">%s</div></div><div class="text-center md:text-left space-y-2"><h1 class="text-4xl font-black text-white">%s</h1><div class="text-slate-400 text-lg">%s</div><div class="flex flex-wrap gap-2 justify-center md:justify-start pt-2"><span class="bg-slate-800 text-slate-300 px-3 py-1 rounded text-sm">%s</span>%s%s%s</div></div></div><div class="grid grid-cols-1 lg:grid-cols-4 gap-8"><div class="lg:col-span-3 space-y-8">%s<div class="space-y-4"><div class="flex flex-col gap-1"><h3 class="text-xl font-bold text-white">Recent Games</h3><p class="text-[11px] text-slate-500">개인 <span class="font-mono text-slate-300">+/-</span>는 문자중계(PBP) 기반이며, 데이터가 없거나 PBP/박스스코어 최종 스코어 불일치 등 품질 이슈가 있으면 <span class="font-mono text-slate-300">-</span>로 표시합니다.</p></div><div class="bg-slate-900 rounded-xl border border-slate-800 overflow-x-auto shadow-lg"><table class="min-w-[920px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-800/80 text-slate-400 uppercase tracking-wider text-xs"><tr><th class="px-4 py-3 text-left font-sans w-[110px] whitespace-nowrap">Date</th><th class="px-4 py-3 text-left font-sans">Opponent</th><th class="px-4 py-3 text-right w-[72px]">MIN</th><th class="px-4 py-3 text-right text-orange-400 w-[72px]">PTS</th><th class="px-4 py-3 text-right w-[72px]">+/-</th><th class="px-4 py-3 text-right w-[72px]">REB</th><th class="px-4 py-3 text-right w-[72px]">AST</th><th class="px-4 py-3 text-right w-[72px]">STL</th><th class="px-4 py-3 text-right w-[72px]">BLK</th></tr></thead><tbody>%s</tbody></table></div></div>%s</div><div class="space-y-8"><div class="space-y-4">%s%s%s%s</div></div></div></div>|html}
          (player_img_tag ~class_name:"w-32 h-32 border-4 border-slate-700 shadow-2xl" p.id p.name)
          (escape_html p.id)
          (escape_html p.name)
          info_text
          (match p.birth_date with Some d -> d | None -> "Unknown")
          team_badge_html
          video_links_html
          career_chips
          season_stats_component
          recent_rows
          all_star_section_html
          team_movement_html
          career_highs_html
          data_notes_html
          missing_data_html)

let team_profile_page (detail: team_full_detail) =
  let t = detail.tfd_team_name in
  let s = detail.tfd_standing in
  let standing_info = match s with | Some st -> Printf.sprintf {html|<div class="flex gap-6 text-sm"><div class="flex flex-col"><span>WINS</span><span class="text-2xl font-black text-white">%d</span></div><div class="flex flex-col"><span>LOSSES</span><span class="text-2xl font-black text-white">%d</span></div><div class="flex flex-col"><span>WIN %%</span><span class="text-2xl font-black text-orange-400">%.3f</span></div><div class="flex flex-col"><span>GB</span><span class="text-2xl font-black text-slate-400">%.1f</span></div></div>|html} st.wins st.losses st.win_pct st.gb | None -> "" in
  let roster_rows = detail.tfd_roster |> List.mapi (fun i p -> player_row (i + 1) p) |> String.concat "\n" in
  let game_rows =
    detail.tfd_recent_games
    |> List.map (fun (g: team_game_result) ->
        let res_class = if g.tgr_is_win then "text-sky-400" else "text-rose-400" in
        let res_label = if g.tgr_is_win then "W" else "L" in
        let margin = g.tgr_team_score - g.tgr_opponent_score in
        let margin_class =
          if margin > 0 then "text-sky-400"
          else if margin < 0 then "text-rose-400"
          else "text-slate-400"
        in
        let margin_str =
          if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin
        in
        let opponent_label = if g.tgr_is_home then "vs " ^ g.tgr_opponent else "@ " ^ g.tgr_opponent in
        Printf.sprintf
          {html|<tr class="border-b border-slate-800/60 hover:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-400 text-sm font-mono"><a href="/boxscore/%s" class="hover:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-white">%s</td><td class="px-4 py-3 text-center font-bold %s">%s</td><td class="px-4 py-3 text-right font-mono text-white"><div class="flex items-center justify-end gap-2"><span>%d - %d</span><span class="px-2 py-0.5 rounded bg-slate-800/60 border border-slate-700/60 text-[10px] font-mono %s">%s</span></div></td></tr>|html}
          (Uri.pct_encode g.tgr_game_id)
          (escape_html g.tgr_game_date)
          (escape_html opponent_label)
          res_class
          res_label
          g.tgr_team_score
          g.tgr_opponent_score
          margin_class
          (escape_html margin_str))
    |> String.concat "\n"
  in
  layout ~title:(t ^ " | WKBL Team Profile")
    ~content:(Printf.sprintf {html|<div class="space-y-8 animate-fade-in"><div class="bg-slate-900 rounded-xl border border-slate-800 p-8 shadow-2xl flex flex-col md:flex-row items-center md:items-start gap-8"><div class="w-32 h-32 bg-slate-800 rounded-xl flex items-center justify-center p-4 border-2 border-slate-700 shadow-inner">%s</div><div class="text-center md:text-left space-y-4"><h1 class="text-4xl font-black text-white">%s</h1>%s</div></div><div class="grid grid-cols-1 lg:grid-cols-3 gap-8"><div class="space-y-4 lg:col-span-2"><h3 class="text-xl font-bold text-white">Roster</h3><div class="bg-slate-900 rounded-xl border border-slate-800 overflow-x-auto shadow-lg"><table class="roster-table min-w-[900px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-800/80 text-slate-400 uppercase tracking-wider text-xs"><tr><th class="px-3 py-3 text-left font-sans w-12 whitespace-nowrap">#</th><th class="px-3 py-3 text-left font-sans w-[220px] whitespace-nowrap">Player</th><th class="px-3 py-3 text-left font-sans w-[140px] whitespace-nowrap">Team</th><th class="px-3 py-3 text-right w-[60px] whitespace-nowrap">GP</th><th class="px-3 py-3 text-right text-orange-400 w-[72px] whitespace-nowrap">PTS</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">MG</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">REB</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">AST</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">STL</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">BLK</th><th class="px-3 py-2 text-right w-[72px] whitespace-nowrap">TO</th><th class="px-3 py-2 text-right text-orange-400 w-[72px] whitespace-nowrap">EFF</th></tr></thead><tbody>%s</tbody></table></div></div><div class="space-y-4 lg:col-span-1"><h3 class="text-xl font-bold text-white">Recent Results</h3><div class="bg-slate-900 rounded-xl border border-slate-800 overflow-hidden shadow-lg"><table class="w-full text-sm font-mono"><thead class="bg-slate-800/80 text-slate-400 uppercase tracking-wider text-xs"><tr><th class="px-4 py-3 text-left font-sans">Date</th><th class="px-4 py-3 text-left font-sans">Opponent</th><th class="px-4 py-3 text-center font-sans">Result</th><th class="px-4 py-3 text-right font-sans">Score</th></tr></thead><tbody>%s</tbody></table></div></div></div></div>|html}
          (team_logo_tag ~class_name:"w-24 h-24" t) (escape_html t) standing_info roster_rows game_rows)

(** Error page *)
let error_page message = layout ~title:"Error" ~content:(Printf.sprintf {html|<div class="flex flex-col items-center justify-center py-20"><span class="text-6xl mb-4">😵</span><h2 class="text-xl font-bold text-white mb-2">Something went wrong</h2><p class="text-slate-400">%s</p><a href="/" class="mt-4 text-orange-500 hover:underline">← Back to home</a></div>|html} (escape_html message))
