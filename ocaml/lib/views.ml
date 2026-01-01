(** HTML Views - HTMX-powered templates

    Design:
    - Pure functions returning strings
    - No side effects
    - Composable components
*)

open Domain

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
              hx-get="/players?sort=pts" hx-target="#players-table" hx-swap="innerHTML">PTS</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players?sort=reb" hx-target="#players-table" hx-swap="innerHTML">REB</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players?sort=ast" hx-target="#players-table" hx-swap="innerHTML">AST</th>
          <th class="px-3 py-3 text-right">STL</th>
          <th class="px-3 py-3 text-right">BLK</th>
          <th class="px-3 py-3 text-right">TO</th>
          <th class="px-3 py-3 text-right cursor-pointer hover:text-orange-400"
              hx-get="/players?sort=eff" hx-target="#players-table" hx-swap="innerHTML">EFF</th>
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
                   hx-get="/players"
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

(** Teams page - grid of all teams *)
let teams_page (teams: string list) =
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
