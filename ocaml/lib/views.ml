(** HTML Views - HTMX-powered templates

    Design:
    - Pure functions returning strings
    - No side effects
    - Composable components
*)

open Domain
open Qa

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

(** Team badge component *)
let team_badge team_name =
  let color =
    team_code_of_string team_name
    |> Option.map team_code_to_color
    |> Option.value ~default:"#666"
  in
  Printf.sprintf
    {|<span class="inline-flex items-center gap-1.5 px-2 py-0.5 rounded text-xs font-medium"
           style="background-color: %s20; color: %s; border: 1px solid %s40">
      <span class="w-2 h-2 rounded-full" style="background-color: %s"></span>
      %s
    </span>|}
    color color color color (escape_html team_name)

(** Stat cell with formatting *)
let stat_cell ?(highlight=false) value =
  let class_name = if highlight then "text-orange-400 font-bold" else "text-slate-300" in
  Printf.sprintf {|<td class="px-3 py-2 text-right %s font-mono">%.1f</td>|} class_name value

(** Player row component *)
let player_row (rank: int) (p: player_aggregate) =
  Printf.sprintf
    {|<tr class="border-b border-slate-700/50 hover:bg-slate-800/50 transition-colors">
      <td class="px-3 py-2 text-slate-500 text-sm">%d</td>
      <td class="px-3 py-2 font-medium text-white">%s</td>
      <td class="px-3 py-2">%s</td>
      <td class="px-3 py-2 text-right text-slate-400">%d</td>
      %s%s%s%s%s%s%s
    </tr>|}
    rank
    (escape_html p.name)
    (team_badge p.team_name)
    p.games_played
    (stat_cell ~highlight:true p.avg_points)
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
    {|<table class="w-full">
      <thead class="bg-slate-800/80 sticky top-0">
        <tr class="text-slate-400 text-xs uppercase tracking-wider">
          <th class="px-3 py-3 text-left w-12">#</th>
          <th class="px-3 py-3 text-left">Player</th>
          <th class="px-3 py-3 text-left">Team</th>
          <th class="px-3 py-3 text-right">GP</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players/table?sort=pts" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">PTS</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players/table?sort=reb" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">REB</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players/table?sort=ast" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">AST</th>
          <th class="px-3 py-3 text-right">STL</th>
          <th class="px-3 py-3 text-right">BLK</th>
          <th class="px-3 py-3 text-right">TO</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players/table?sort=eff" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">EFF</th>
        </tr>
      </thead>
      <tbody id="players-body">
        %s
      </tbody>
    </table>|}
    rows

(** Main layout *)
let layout ~title ~content =
  Printf.sprintf
    {|<!DOCTYPE html>
<html lang="ko" class="dark">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <script src="https://unpkg.com/htmx.org@1.9.10"></script>
  <script src="https://cdn.tailwindcss.com"></script>
  <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&family=Inter:wght@400;600;800&display=swap" rel="stylesheet">
  <script>
    tailwind.config = {
      darkMode: 'class',
      theme: {
        extend: {
          fontFamily: {
            sans: ['Inter', 'sans-serif'],
            mono: ['JetBrains Mono', 'monospace'],
          }
        }
      }
    }
  </script>
  <style>
    body { background: #0b0e14; }
  </style>
</head>
<body class="bg-[#0b0e14] text-slate-200 font-sans antialiased min-h-screen">
  <header class="sticky top-0 z-50 bg-slate-900/95 backdrop-blur border-b border-slate-800 px-6 py-4">
    <div class="max-w-7xl mx-auto flex items-center justify-between">
      <div class="flex items-center gap-3">
        <span class="text-2xl">🏀</span>
        <h1 class="text-lg font-bold text-white">WKBL <span class="text-orange-500">Analytics</span></h1>
        <span class="text-xs text-slate-500 font-mono">OCaml Edition</span>
      </div>
      <nav class="flex gap-4 text-sm">
        <a href="/" class="text-slate-400 hover:text-white transition">Home</a>
        <a href="/teams" class="text-slate-400 hover:text-white transition">Teams</a>
        <a href="/players" class="text-slate-400 hover:text-white transition">Players</a>
        <a href="/qa" class="text-slate-400 hover:text-white transition">QA</a>
      </nav>
    </div>
  </header>

  <main class="max-w-7xl mx-auto px-6 py-8">
    %s
  </main>

  <footer class="border-t border-slate-800 py-6 text-center text-slate-500 text-sm">
    Built with OCaml + Dream + HTMX
  </footer>
</body>
</html>|}
    (escape_html title)
    content

(** Home page *)
let home_page players =
  let table = players_table players in
  layout ~title:"WKBL Analytics"
    ~content:(Printf.sprintf
      {|<div class="space-y-6">
        <div class="flex items-center justify-between">
          <h2 class="text-xl font-bold text-white">Top Players by Efficiency</h2>
          <div class="flex gap-2">
            <input type="text"
                   placeholder="Search player..."
                   class="bg-slate-800 border border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none"
                   hx-get="/players/table"
                   hx-trigger="keyup changed delay:300ms"
                   hx-target="#players-table"
                   name="search">
          </div>
        </div>
        <div id="players-table" class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
          %s
        </div>
      </div>|}
      table)

let players_page ~search ~sort players =
  let sort_value =
    match String.lowercase_ascii sort with
    | "pts" | "points" -> "pts"
    | "reb" | "rebounds" -> "reb"
    | "ast" | "assists" -> "ast"
    | "min" | "minutes" -> "min"
    | "eff" | "efficiency" -> "eff"
    | _ -> "eff"
  in
  let sort_option value label =
    let selected = if sort_value = value then "selected" else "" in
    Printf.sprintf {|<option value="%s" %s>%s</option>|} value selected label
  in
  let table = players_table players in
  layout ~title:"WKBL Players"
    ~content:(Printf.sprintf
      {|<div class="space-y-6">
          <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
            <div>
              <h2 class="text-2xl font-bold text-white">Players</h2>
              <p class="text-slate-400 text-sm">Filter and sort player aggregates.</p>
            </div>
            <a class="text-orange-400 hover:text-orange-300 text-sm" href="/players">Reset</a>
          </div>
          <form id="players-filter"
                class="grid grid-cols-1 md:grid-cols-3 gap-3"
                hx-get="/players/table"
                hx-target="#players-table"
                hx-trigger="change, keyup delay:250ms">
            <input type="text"
                   name="search"
                   placeholder="Search player..."
                   value="%s"
                   class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
            <select name="sort"
                    class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
              %s%s%s%s%s
            </select>
            <div class="text-slate-500 text-xs flex items-center">Sorted by %s</div>
          </form>
          <div id="players-table" class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
            %s
          </div>
        </div>|}
      (escape_html search)
      (sort_option "eff" "EFF")
      (sort_option "pts" "PTS")
      (sort_option "reb" "REB")
      (sort_option "ast" "AST")
      (sort_option "min" "MIN")
      (String.uppercase_ascii sort_value)
      table)

let format_float ?(digits=1) value =
  Printf.sprintf "%.*f" digits value

let teams_table ~scope (stats: team_stats list) =
  let rows =
    stats
    |> List.map (fun (row: team_stats) ->
        Printf.sprintf
          {|<tr class="border-b border-slate-800/60">
              <td class="px-3 py-2 font-medium text-white">%s</td>
              <td class="px-3 py-2 text-right text-slate-400">%d</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
              <td class="px-3 py-2 text-right text-slate-300">%s</td>
            </tr>|}
          (escape_html row.team)
          row.gp
          (format_float row.min_total)
          (format_float row.pts)
          (format_float row.margin)
          (format_float row.reb)
          (format_float row.ast)
          (format_float row.stl)
          (format_float row.blk)
          (format_float row.turnovers)
          (format_float row.fg_pct)
          (format_float row.fg3_pct)
          (format_float row.ft_pct)
          (format_float row.efg_pct)
          (format_float row.ts_pct)
          (format_float row.eff))
    |> String.concat "\n"
  in
  let min_label = if scope = PerGame then "MIN/G" else "MIN" in
  Printf.sprintf
    {|<div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
        <table class="w-full">
          <thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider">
            <tr>
              <th class="px-3 py-2 text-left">Team</th>
              <th class="px-3 py-2 text-right">GP</th>
              <th class="px-3 py-2 text-right">%s</th>
              <th class="px-3 py-2 text-right">PTS</th>
              <th class="px-3 py-2 text-right">Margin</th>
              <th class="px-3 py-2 text-right">REB</th>
              <th class="px-3 py-2 text-right">AST</th>
              <th class="px-3 py-2 text-right">STL</th>
              <th class="px-3 py-2 text-right">BLK</th>
              <th class="px-3 py-2 text-right">TO</th>
              <th class="px-3 py-2 text-right">FG%%</th>
              <th class="px-3 py-2 text-right">3P%%</th>
              <th class="px-3 py-2 text-right">FT%%</th>
              <th class="px-3 py-2 text-right">eFG%%</th>
              <th class="px-3 py-2 text-right">TS%%</th>
              <th class="px-3 py-2 text-right">EFF</th>
            </tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>|}
    min_label rows

let teams_page ~season ~seasons ~scope ~sort stats =
  let scope_value = team_scope_to_string scope in
  let scope_option value label =
    let selected = if scope_value = value then "selected" else "" in
    Printf.sprintf {|<option value="%s" %s>%s</option>|} value selected label
  in
  let sort_option value label =
    let selected = if String.lowercase_ascii sort = value then "selected" else "" in
    Printf.sprintf {|<option value="%s" %s>%s</option>|} value selected label
  in
  let season_options =
    let base =
      seasons
      |> List.map (fun s ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {|<option value="%s" %s>%s</option>|}
            s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {|<option value="ALL" %s>All Seasons</option>%s|}
      (if season = "ALL" then "selected" else "")
      base
  in
  let table = teams_table ~scope stats in
  layout ~title:"WKBL Teams"
    ~content:(Printf.sprintf
      {|<div class="space-y-6">
          <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
            <div>
              <h2 class="text-2xl font-bold text-white">Teams</h2>
              <p class="text-slate-400 text-sm">Team aggregates by season and scope.</p>
            </div>
            <a class="text-orange-400 hover:text-orange-300 text-sm" href="/teams">Reset</a>
          </div>
          <form id="teams-filter"
                class="grid grid-cols-1 md:grid-cols-3 gap-3"
                hx-get="/teams/table"
                hx-target="#teams-table"
                hx-trigger="change">
            <select name="season"
                    class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
              %s
            </select>
            <select name="scope"
                    class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
              %s%s
            </select>
            <select name="sort"
                    class="bg-slate-800 border border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
              %s%s%s%s%s%s%s%s%s
            </select>
          </form>
          <div id="teams-table">%s</div>
        </div>|}
      season_options
      (scope_option "per_game" "Per Game")
      (scope_option "totals" "Totals")
      (sort_option "pts" "PTS")
      (sort_option "reb" "REB")
      (sort_option "ast" "AST")
      (sort_option "stl" "STL")
      (sort_option "blk" "BLK")
      (sort_option "eff" "EFF")
      (sort_option "ts_pct" "TS%")
      (sort_option "fg3_pct" "3P%")
      (sort_option "min_total" "MIN")
      table)

(** Error page *)
let error_page message =
  layout ~title:"Error"
    ~content:(Printf.sprintf
      {|<div class="flex flex-col items-center justify-center py-20">
        <span class="text-6xl mb-4">😵</span>
        <h2 class="text-xl font-bold text-white mb-2">Something went wrong</h2>
        <p class="text-slate-400">%s</p>
        <a href="/" class="mt-4 text-orange-500 hover:underline">← Back to home</a>
      </div>|}
      (escape_html message))

(** Team card component - shows team stats with HTMX drill-down *)
let team_card team_name =
  let color =
    team_code_of_string team_name
    |> Option.map team_code_to_color
    |> Option.value ~default:"#666"
  in
  Printf.sprintf
    {|<div class="bg-slate-800/50 rounded-lg border border-slate-700 p-4 hover:border-orange-500/50 transition-all cursor-pointer"
         hx-get="/teams/%s" hx-target="#team-detail" hx-swap="innerHTML">
      <div class="flex items-center gap-3">
        <div class="w-12 h-12 rounded-full flex items-center justify-center text-2xl"
             style="background-color: %s20; border: 2px solid %s">
          🏀
        </div>
        <div>
          <h3 class="font-bold text-white">%s</h3>
          <p class="text-sm text-slate-400">클릭하여 선수 보기</p>
        </div>
      </div>
    </div>|}
    (Uri.pct_encode team_name) color color (escape_html team_name)

(** Team detail - players list for a specific team *)
let team_detail team_name (players: player_aggregate list) =
  let rows =
    players
    |> List.mapi (fun i p -> player_row (i + 1) p)
    |> String.concat "\n"
  in
  let color =
    team_code_of_string team_name
    |> Option.map team_code_to_color
    |> Option.value ~default:"#666"
  in
  Printf.sprintf
    {|<div class="space-y-4">
      <div class="flex items-center gap-3 mb-4">
        <div class="w-10 h-10 rounded-full flex items-center justify-center text-xl"
             style="background-color: %s20; border: 2px solid %s">
          🏀
        </div>
        <h3 class="text-lg font-bold text-white">%s 선수 목록</h3>
      </div>
      <table class="w-full">
        <thead class="bg-slate-800/80">
          <tr class="text-slate-400 text-xs uppercase tracking-wider">
            <th class="px-3 py-2 text-left w-12">#</th>
            <th class="px-3 py-2 text-left">Player</th>
            <th class="px-3 py-2 text-left">Team</th>
            <th class="px-3 py-2 text-right">GP</th>
            <th class="px-3 py-2 text-right">PTS</th>
            <th class="px-3 py-2 text-right">REB</th>
            <th class="px-3 py-2 text-right">AST</th>
            <th class="px-3 py-2 text-right">STL</th>
            <th class="px-3 py-2 text-right">BLK</th>
            <th class="px-3 py-2 text-right">TO</th>
            <th class="px-3 py-2 text-right">EFF</th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>|}
    color color (escape_html team_name) rows

(** Teams grid page - legacy layout *)
let teams_grid_page (teams: string list) =
  let cards =
    teams
    |> List.map team_card
    |> String.concat "\n"
  in
  layout ~title:"WKBL Teams"
    ~content:(Printf.sprintf
      {|<div class="space-y-6">
        <h2 class="text-xl font-bold text-white">WKBL 팀</h2>
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          %s
        </div>
        <div id="team-detail" class="mt-8 bg-slate-900 rounded-lg border border-slate-800 p-4">
          <p class="text-slate-400 text-center py-8">팀을 선택하면 선수 목록이 표시됩니다</p>
        </div>
      </div>|}
      cards)

let qa_page result =
  let fmt_int value = string_of_int value in
  let fmt_float ?(digits=1) value =
    Printf.sprintf "%.*f" digits value
  in
  match result with
  | Error err ->
      layout ~title:"WKBL QA"
        ~content:(Printf.sprintf
          {|<div class="flex flex-col items-center justify-center py-20">
              <span class="text-6xl mb-4">🧪</span>
              <h2 class="text-xl font-bold text-white mb-2">QA report not available</h2>
              <p class="text-slate-400">%s%s</p>
              <a href="/qa?refresh=1" class="mt-4 text-orange-500 hover:underline">Run QA now</a>
            </div>|}
          (escape_html err.message)
          (match err.path with
           | None -> ""
           | Some path -> " / " ^ escape_html path))
  | Ok report ->
      let coverage = report.coverage in
      let summary_cards =
        Printf.sprintf
          {|<div class="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div class="bg-slate-800/60 rounded-lg border border-slate-700 p-4">
                <div class="text-slate-400 text-xs uppercase tracking-wider">Coverage</div>
                <div class="text-2xl font-bold text-white">%s%%</div>
                <div class="text-slate-500 text-sm">Expected %s / Actual %s</div>
              </div>
              <div class="bg-slate-800/60 rounded-lg border border-slate-700 p-4">
                <div class="text-slate-400 text-xs uppercase tracking-wider">Missing</div>
                <div class="text-2xl font-bold text-white">%s</div>
                <div class="text-slate-500 text-sm">Extra %s</div>
              </div>
              <div class="bg-slate-800/60 rounded-lg border border-slate-700 p-4">
                <div class="text-slate-400 text-xs uppercase tracking-wider">Outliers</div>
                <div class="text-2xl font-bold text-white">%s</div>
                <div class="text-slate-500 text-sm">Abs PTS %s · Margin %s</div>
              </div>
            </div>|}
          (fmt_float coverage.coverage_pct)
          (fmt_int coverage.expected_games)
          (fmt_int coverage.actual_games)
          (fmt_int coverage.missing_games)
          (fmt_int coverage.extra_games)
          (fmt_int (List.length report.outliers.iqr))
          (fmt_int (List.length report.outliers.absolute_pts))
          (fmt_int (List.length report.outliers.absolute_margin))
      in
      let render_samples title items =
        if items = [] then
          Printf.sprintf
            {|<div class="bg-slate-800/40 rounded-lg border border-slate-700 p-4">
                <h3 class="text-sm font-semibold text-white mb-2">%s</h3>
                <div class="text-slate-500 text-sm">None detected</div>
              </div>|}
            title
        else
          let rows =
            items
            |> take 12
            |> List.map (fun item -> Printf.sprintf "<li><code>%s</code></li>" (escape_html item))
            |> String.concat "\n"
          in
          Printf.sprintf
            {|<div class="bg-slate-800/40 rounded-lg border border-slate-700 p-4">
                <h3 class="text-sm font-semibold text-white mb-2">%s</h3>
                <ul class="text-slate-400 text-sm space-y-1">%s</ul>
              </div>|}
            title rows
      in
      let render_team_anomalies =
        if report.duplicates.team_count_anomalies = [] then
          {|<div class="bg-slate-800/40 rounded-lg border border-slate-700 p-4">
              <h3 class="text-sm font-semibold text-white mb-2">Team Count Anomalies</h3>
              <div class="text-slate-500 text-sm">None detected</div>
            </div>|}
        else
          let rows =
            report.duplicates.team_count_anomalies
            |> take 10
            |> List.map (fun (row: Qa.duplicate_team) ->
                Printf.sprintf "<li><code>%s</code> teams=%d</li>"
                  (escape_html row.game_key) row.team_count)
            |> String.concat "\n"
          in
          Printf.sprintf
            {|<div class="bg-slate-800/40 rounded-lg border border-slate-700 p-4">
                <h3 class="text-sm font-semibold text-white mb-2">Team Count Anomalies</h3>
                <ul class="text-slate-400 text-sm space-y-1">%s</ul>
              </div>|}
            rows
      in
      let render_player_dupes =
        if report.duplicates.player_duplicates = [] then
          {|<div class="bg-slate-800/40 rounded-lg border border-slate-700 p-4">
              <h3 class="text-sm font-semibold text-white mb-2">Duplicate Players</h3>
              <div class="text-slate-500 text-sm">None detected</div>
            </div>|}
        else
          let rows =
            report.duplicates.player_duplicates
            |> take 10
            |> List.map (fun (row: Qa.duplicate_player) ->
                Printf.sprintf "<li><code>%s</code> %s %s x%d</li>"
                  (escape_html row.game_key)
                  (escape_html row.team)
                  (escape_html row.name)
                  row.row_count)
            |> String.concat "\n"
          in
          Printf.sprintf
            {|<div class="bg-slate-800/40 rounded-lg border border-slate-700 p-4">
                <h3 class="text-sm font-semibold text-white mb-2">Duplicate Players</h3>
                <ul class="text-slate-400 text-sm space-y-1">%s</ul>
              </div>|}
            rows
      in
      let coverage_rows =
        report.coverage.by_season
        |> List.map (fun (row: Qa.coverage_row) ->
            Printf.sprintf
              {|<tr class="border-b border-slate-800/60">
                  <td class="px-3 py-2">%s</td>
                  <td class="px-3 py-2 text-right">%d</td>
                  <td class="px-3 py-2 text-right">%d</td>
                  <td class="px-3 py-2 text-right">%d</td>
                  <td class="px-3 py-2 text-right">%s%%</td>
                </tr>|}
              (escape_html row.season)
              row.expected
              row.actual
              row.missing
              (fmt_float row.coverage_pct))
        |> String.concat "\n"
      in
      let render_outliers_iqr =
        if report.outliers.iqr = [] then
          {|<tr><td colspan="8" class="px-3 py-3 text-slate-500">No IQR outliers detected.</td></tr>|}
        else
          report.outliers.iqr
          |> take 20
          |> List.map (fun (row: Qa.outlier_iqr) ->
              Printf.sprintf
                {|<tr class="border-b border-slate-800/60">
                    <td class="px-3 py-2">%s</td>
                    <td class="px-3 py-2">%s</td>
                    <td class="px-3 py-2">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                  </tr>|}
                (escape_html row.season)
                (escape_html row.game_key)
                (escape_html row.team)
                (fmt_float row.pts_for)
                (fmt_float row.pts_against)
                (fmt_float row.margin)
                (fmt_float ~digits:2 row.low_threshold)
                (fmt_float ~digits:2 row.high_threshold))
          |> String.concat "\n"
      in
      let render_outliers_table rows empty_label =
        if rows = [] then
          Printf.sprintf
            {|<tr><td colspan="5" class="px-3 py-3 text-slate-500">%s</td></tr>|}
            empty_label
        else
          rows
          |> take 15
          |> List.map (fun (row: Qa.outlier_abs) ->
              Printf.sprintf
                {|<tr class="border-b border-slate-800/60">
                    <td class="px-3 py-2">%s</td>
                    <td class="px-3 py-2">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                    <td class="px-3 py-2 text-right">%s</td>
                  </tr>|}
                (escape_html row.game_key)
                (escape_html row.team)
                (fmt_float row.pts_for)
                (fmt_float row.pts_against)
                (fmt_float row.margin))
          |> String.concat "\n"
      in
      layout ~title:"WKBL Data QA"
        ~content:(Printf.sprintf
          {|<div class="space-y-8">
              <div class="flex flex-col gap-4">
                <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
                  <div>
                    <h2 class="text-2xl font-bold text-white">Data QA</h2>
                    <p class="text-slate-400 text-sm">Coverage, duplicates, and outlier checks for WKBL datasets.</p>
                  </div>
                  <div class="flex gap-2 text-sm">
                    <a class="text-orange-400 hover:text-orange-300" href="/qa.json">QA JSON</a>
                    <a class="text-orange-400 hover:text-orange-300" href="/qa.md">QA MD</a>
                    <a class="text-orange-400 hover:text-orange-300" href="/qa?refresh=1">Refresh</a>
                  </div>
                </div>
                %s
              </div>

              <div class="space-y-3">
                <h3 class="text-lg font-semibold text-white">Coverage by Season</h3>
                <div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
                  <table class="w-full">
                    <thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider">
                      <tr>
                        <th class="px-3 py-2 text-left">Season</th>
                        <th class="px-3 py-2 text-right">Expected</th>
                        <th class="px-3 py-2 text-right">Actual</th>
                        <th class="px-3 py-2 text-right">Missing</th>
                        <th class="px-3 py-2 text-right">Coverage</th>
                      </tr>
                    </thead>
                    <tbody>%s</tbody>
                  </table>
                </div>
              </div>

              <div class="space-y-3">
                <h3 class="text-lg font-semibold text-white">Anomalies</h3>
                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                  %s
                  %s
                  %s
                  %s
                </div>
              </div>

              <div class="space-y-3">
                <h3 class="text-lg font-semibold text-white">Outliers (IQR)</h3>
                <div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
                  <table class="w-full">
                    <thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider">
                      <tr>
                        <th class="px-3 py-2 text-left">Season</th>
                        <th class="px-3 py-2 text-left">Game</th>
                        <th class="px-3 py-2 text-left">Team</th>
                        <th class="px-3 py-2 text-right">PTS</th>
                        <th class="px-3 py-2 text-right">Opp</th>
                        <th class="px-3 py-2 text-right">Margin</th>
                        <th class="px-3 py-2 text-right">Low</th>
                        <th class="px-3 py-2 text-right">High</th>
                      </tr>
                    </thead>
                    <tbody>%s</tbody>
                  </table>
                </div>
              </div>

              <div class="space-y-3">
                <h3 class="text-lg font-semibold text-white">Absolute Points</h3>
                <div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
                  <table class="w-full">
                    <thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider">
                      <tr>
                        <th class="px-3 py-2 text-left">Game</th>
                        <th class="px-3 py-2 text-left">Team</th>
                        <th class="px-3 py-2 text-right">PTS</th>
                        <th class="px-3 py-2 text-right">Opp</th>
                        <th class="px-3 py-2 text-right">Margin</th>
                      </tr>
                    </thead>
                    <tbody>%s</tbody>
                  </table>
                </div>
              </div>

              <div class="space-y-3">
                <h3 class="text-lg font-semibold text-white">Absolute Margin</h3>
                <div class="bg-slate-900 rounded-lg border border-slate-800 overflow-hidden">
                  <table class="w-full">
                    <thead class="bg-slate-800/80 text-slate-400 text-xs uppercase tracking-wider">
                      <tr>
                        <th class="px-3 py-2 text-left">Game</th>
                        <th class="px-3 py-2 text-left">Team</th>
                        <th class="px-3 py-2 text-right">Margin</th>
                        <th class="px-3 py-2 text-right">PTS</th>
                        <th class="px-3 py-2 text-right">Opp</th>
                      </tr>
                    </thead>
                    <tbody>%s</tbody>
                  </table>
                </div>
              </div>
            </div>|}
          summary_cards
          coverage_rows
          (render_samples "Missing Games" coverage.missing_sample)
          (render_samples "Extra Games" coverage.extra_sample)
          render_team_anomalies
          render_player_dupes
          render_outliers_iqr
          (render_outliers_table report.outliers.absolute_pts "No absolute points outliers detected.")
          (render_outliers_table report.outliers.absolute_margin "No absolute margin outliers detected."))
