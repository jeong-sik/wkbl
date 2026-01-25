(** Basketball Court Charts - Shot distribution visualization

    Since WKBL doesn't provide shot coordinates, we visualize:
    - Shot distribution by zone (3PT, 2PT, FT)
    - Shooting percentages per zone
    - Made/Missed attempts
*)

open Domain
open Views_common

(** Court dimensions (scaled for SVG) *)
let court_width = 500
let court_height = 470

(** Color utilities *)
let pct_to_color pct =
  if pct >= 50.0 then "#22c55e"      (* Green - Hot *)
  else if pct >= 40.0 then "#f59e0b" (* Yellow - Warm *)
  else if pct >= 30.0 then "#f97316" (* Orange - Cool *)
  else "#ef4444"                      (* Red - Cold *)

let pct_to_opacity pct =
  if pct >= 50.0 then "0.8"
  else if pct >= 40.0 then "0.6"
  else if pct >= 30.0 then "0.5"
  else "0.4"

(** Basketball half-court SVG base *)
let court_base ?(dark=true) () =
  let bg = if dark then "#1e293b" else "#f8fafc" in
  let line_color = if dark then "#475569" else "#cbd5e1" in
  let paint_color = if dark then "#334155" else "#e2e8f0" in
  Printf.sprintf {|
  <!-- Court background -->
  <rect x="0" y="0" width="%d" height="%d" fill="%s" rx="8"/>

  <!-- Paint area (restricted zone) -->
  <rect x="175" y="0" width="150" height="190" fill="%s" stroke="%s" stroke-width="2"/>

  <!-- Free throw circle -->
  <circle cx="250" cy="190" r="60" fill="none" stroke="%s" stroke-width="2"/>

  <!-- 3-point arc -->
  <path d="M 30 0 L 30 140 Q 30 340 250 340 Q 470 340 470 140 L 470 0"
        fill="none" stroke="%s" stroke-width="2"/>

  <!-- Basket -->
  <circle cx="250" cy="45" r="8" fill="none" stroke="#f97316" stroke-width="3"/>
  <rect x="230" y="30" width="40" height="5" fill="#f97316"/>

  <!-- Backboard -->
  <rect x="220" y="25" width="60" height="4" fill="%s"/>

  <!-- Free throw line -->
  <line x1="175" y1="190" x2="325" y2="190" stroke="%s" stroke-width="2"/>

  <!-- Lane lines -->
  <line x1="175" y1="0" x2="175" y2="190" stroke="%s" stroke-width="2"/>
  <line x1="325" y1="0" x2="325" y2="190" stroke="%s" stroke-width="2"/>

  <!-- Half-court line -->
  <line x1="0" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="2"/>
|}
    court_width court_height bg
    paint_color line_color
    line_color
    line_color
    line_color
    line_color line_color line_color
    (court_height - 1) court_width (court_height - 1) line_color

(** Shot zone overlay - highlights a zone with shooting stats *)
let zone_overlay ~zone ~pct ~made ~attempts =
  let color = pct_to_color pct in
  let opacity = pct_to_opacity pct in
  let (zone_path, text_x, text_y) = match zone with
    | "paint" ->
        (* Paint area - below free throw line *)
        (Printf.sprintf {|<rect x="175" y="0" width="150" height="190" fill="%s" opacity="%s"/>|}
           color opacity,
         250, 100)
    | "mid" ->
        (* Mid-range - between paint and 3pt line *)
        (Printf.sprintf {|<path d="M 30 0 L 30 140 Q 30 340 250 340 Q 470 340 470 140 L 470 0 L 325 0 L 325 190
                   Q 325 230 250 230 Q 175 230 175 190 L 175 0 Z" fill="%s" opacity="%s"/>|}
           color opacity,
         100, 250)
    | "three" ->
        (* 3-point zone - outside arc *)
        (Printf.sprintf {|<path d="M 0 0 L 0 %d L %d %d L %d 0 L 470 0 L 470 140 Q 470 340 250 340 Q 30 340 30 140 L 30 0 Z"
                 fill="%s" opacity="%s"/>|}
           court_height court_width court_height court_width color opacity,
         400, 100)
    | _ -> ("", 0, 0)
  in
  Printf.sprintf {|
  %s
  <g transform="translate(%d, %d)">
    <text x="0" y="0" text-anchor="middle" font-family="system-ui" font-size="28" font-weight="700" fill="#ffffff">
      %.1f%%
    </text>
    <text x="0" y="24" text-anchor="middle" font-family="system-ui" font-size="14" fill="#94a3b8">
      %d/%d
    </text>
  </g>
|} zone_path text_x text_y pct made attempts

(** Generate complete shot distribution chart SVG *)
let shot_distribution_chart ~fg2_pct ~fg2_made ~fg2_attempts
                            ~fg3_pct ~fg3_made ~fg3_attempts
                            ~ft_pct ~ft_made ~ft_attempts
                            ~player_name ~dark =
  let court = court_base ~dark () in
  (* Since we don't have paint vs mid-range split, use 2PT for mid-range area *)
  let mid_zone = zone_overlay ~zone:"mid" ~pct:fg2_pct ~made:fg2_made ~attempts:fg2_attempts in
  let three_zone = zone_overlay ~zone:"three" ~pct:fg3_pct ~made:fg3_made ~attempts:fg3_attempts in
  Printf.sprintf {|<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <defs>
    <filter id="glow">
      <feGaussianBlur stdDeviation="2" result="coloredBlur"/>
      <feMerge>
        <feMergeNode in="coloredBlur"/>
        <feMergeNode in="SourceGraphic"/>
      </feMerge>
    </filter>
  </defs>

  %s
  %s
  %s

  <!-- Legend -->
  <g transform="translate(10, %d)">
    <text x="0" y="-10" font-family="system-ui" font-size="12" fill="#94a3b8">
      %s 슛 분포
    </text>
    <rect x="0" y="0" width="12" height="12" fill="#22c55e" rx="2"/>
    <text x="18" y="10" font-family="system-ui" font-size="11" fill="#94a3b8">50%%+</text>
    <rect x="60" y="0" width="12" height="12" fill="#f59e0b" rx="2"/>
    <text x="78" y="10" font-family="system-ui" font-size="11" fill="#94a3b8">40-50%%</text>
    <rect x="130" y="0" width="12" height="12" fill="#f97316" rx="2"/>
    <text x="148" y="10" font-family="system-ui" font-size="11" fill="#94a3b8">30-40%%</text>
    <rect x="200" y="0" width="12" height="12" fill="#ef4444" rx="2"/>
    <text x="218" y="10" font-family="system-ui" font-size="11" fill="#94a3b8">&lt;30%%</text>
  </g>

  <!-- Free Throw stats (separate box) -->
  <g transform="translate(%d, 380)">
    <rect x="-60" y="-20" width="120" height="50" fill="%s" opacity="0.3" rx="8"/>
    <text x="0" y="0" text-anchor="middle" font-family="system-ui" font-size="14" fill="#94a3b8">
      자유투
    </text>
    <text x="0" y="22" text-anchor="middle" font-family="system-ui" font-size="20" font-weight="700" fill="%s">
      %.1f%% (%d/%d)
    </text>
  </g>
</svg>|}
    (court_width + 100) (court_height + 80)
    (court_width + 100) (court_height + 80)
    court
    three_zone
    mid_zone
    (court_height + 40)
    player_name
    (court_width / 2)
    (pct_to_color ft_pct)
    (pct_to_color ft_pct)
    ft_pct ft_made ft_attempts

(** Simplified shot chart for embedding in player page *)
let shot_chart_embedded ~fg2_pct ~fg2_made ~fg2_attempts
                        ~fg3_pct ~fg3_made ~fg3_attempts
                        ~ft_pct ~ft_made ~ft_attempts =
  Printf.sprintf {|
<div class="shot-chart-container bg-slate-800 rounded-xl p-4">
  <h3 class="text-lg font-bold text-white mb-4">🎯 슛 분포</h3>

  <div class="grid grid-cols-3 gap-4">
    <!-- 3점 슛 -->
    <div class="text-center p-3 rounded-lg" style="background-color: %s20">
      <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
      <div class="text-sm text-slate-400">3점슛</div>
      <div class="text-xs text-slate-500">%d/%d</div>
    </div>

    <!-- 2점 슛 -->
    <div class="text-center p-3 rounded-lg" style="background-color: %s20">
      <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
      <div class="text-sm text-slate-400">2점슛</div>
      <div class="text-xs text-slate-500">%d/%d</div>
    </div>

    <!-- 자유투 -->
    <div class="text-center p-3 rounded-lg" style="background-color: %s20">
      <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
      <div class="text-sm text-slate-400">자유투</div>
      <div class="text-xs text-slate-500">%d/%d</div>
    </div>
  </div>

  <!-- Visual bar comparison -->
  <div class="mt-4 space-y-2">
    <div class="flex items-center gap-2">
      <span class="text-xs text-slate-400 w-12">3PT</span>
      <div class="flex-1 h-3 bg-slate-700 rounded-full overflow-hidden">
        <div class="h-full rounded-full" style="width: %.1f%%; background-color: %s"></div>
      </div>
    </div>
    <div class="flex items-center gap-2">
      <span class="text-xs text-slate-400 w-12">2PT</span>
      <div class="flex-1 h-3 bg-slate-700 rounded-full overflow-hidden">
        <div class="h-full rounded-full" style="width: %.1f%%; background-color: %s"></div>
      </div>
    </div>
    <div class="flex items-center gap-2">
      <span class="text-xs text-slate-400 w-12">FT</span>
      <div class="flex-1 h-3 bg-slate-700 rounded-full overflow-hidden">
        <div class="h-full rounded-full" style="width: %.1f%%; background-color: %s"></div>
      </div>
    </div>
  </div>
</div>
|}
    (pct_to_color fg3_pct) (pct_to_color fg3_pct) fg3_pct fg3_made fg3_attempts
    (pct_to_color fg2_pct) (pct_to_color fg2_pct) fg2_pct fg2_made fg2_attempts
    (pct_to_color ft_pct) (pct_to_color ft_pct) ft_pct ft_made ft_attempts
    fg3_pct (pct_to_color fg3_pct)
    fg2_pct (pct_to_color fg2_pct)
    ft_pct (pct_to_color ft_pct)

(** Generate shot chart from player shooting stats *)
let player_shot_chart (p: player_shooting_stats) =
  (* Calculate 2PT stats from total FG and 3PT *)
  let fg2_made = p.pss_fg_made - p.pss_fg3_made in
  let fg2_attempts = p.pss_fg_attempted - p.pss_fg3_attempted in
  let fg2_pct = if fg2_attempts > 0 then
    (float_of_int fg2_made /. float_of_int fg2_attempts) *. 100.0
  else 0.0 in
  let fg3_pct = p.pss_fg3_pct *. 100.0 in
  let ft_pct = p.pss_ft_pct *. 100.0 in
  shot_distribution_chart
    ~fg2_pct ~fg2_made ~fg2_attempts
    ~fg3_pct ~fg3_made:p.pss_fg3_made ~fg3_attempts:p.pss_fg3_attempted
    ~ft_pct ~ft_made:p.pss_ft_made ~ft_attempts:p.pss_ft_attempted
    ~player_name:p.pss_name
    ~dark:true

(** Generate embedded shot chart HTML from player shooting stats *)
let player_shot_chart_html (p: player_shooting_stats) =
  let fg2_made = p.pss_fg_made - p.pss_fg3_made in
  let fg2_attempts = p.pss_fg_attempted - p.pss_fg3_attempted in
  let fg2_pct = if fg2_attempts > 0 then
    (float_of_int fg2_made /. float_of_int fg2_attempts) *. 100.0
  else 0.0 in
  let fg3_pct = p.pss_fg3_pct *. 100.0 in
  let ft_pct = p.pss_ft_pct *. 100.0 in
  shot_chart_embedded
    ~fg2_pct ~fg2_made ~fg2_attempts
    ~fg3_pct ~fg3_made:p.pss_fg3_made ~fg3_attempts:p.pss_fg3_attempted
    ~ft_pct ~ft_made:p.pss_ft_made ~ft_attempts:p.pss_ft_attempted

(** Team shooting comparison bar chart - horizontal stacked bars *)
let team_shooting_comparison (teams: team_stats list) =
  if teams = [] then ""
  else
    (* Sort by True Shooting % descending *)
    let sorted = List.sort (fun a b -> compare b.ts_pct a.ts_pct) teams in
    (* Note: team_stats percentages are already in 0-100 format, not 0.0-1.0 *)
    (* Scale bar width: each bar represents actual percentage (capped at 100%) *)
    let clamp_width pct = min pct 100.0 in

    let team_rows = sorted |> List.map (fun t ->
      let fg_width = clamp_width t.fg_pct in
      let fg3_width = clamp_width t.fg3_pct in
      let ft_width = clamp_width t.ft_pct in
      let ts_width = clamp_width t.ts_pct in
      Printf.sprintf {|
      <div class="team-shooting-row">
        <div class="w-24 text-sm font-medium text-slate-700 dark:text-slate-300 truncate">%s</div>
        <div class="flex-1 space-y-1">
          <div class="flex items-center gap-2">
            <span class="w-8 text-[10px] text-slate-500">FG</span>
            <div class="flex-1 h-2 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden">
              <div class="h-full bg-blue-500 rounded-full transition-all duration-500" style="width: %.1f%%"></div>
            </div>
            <span class="w-12 text-[10px] text-slate-600 dark:text-slate-400 text-right font-mono">%.1f%%</span>
          </div>
          <div class="flex items-center gap-2">
            <span class="w-8 text-[10px] text-slate-500">3PT</span>
            <div class="flex-1 h-2 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden">
              <div class="h-full bg-green-500 rounded-full transition-all duration-500" style="width: %.1f%%"></div>
            </div>
            <span class="w-12 text-[10px] text-slate-600 dark:text-slate-400 text-right font-mono">%.1f%%</span>
          </div>
          <div class="flex items-center gap-2">
            <span class="w-8 text-[10px] text-slate-500">FT</span>
            <div class="flex-1 h-2 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden">
              <div class="h-full bg-amber-500 rounded-full transition-all duration-500" style="width: %.1f%%"></div>
            </div>
            <span class="w-12 text-[10px] text-slate-600 dark:text-slate-400 text-right font-mono">%.1f%%</span>
          </div>
          <div class="flex items-center gap-2 pt-1 border-t border-slate-200 dark:border-slate-700">
            <span class="w-8 text-[10px] text-orange-500 font-bold">TS</span>
            <div class="flex-1 h-3 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden">
              <div class="h-full bg-orange-500 rounded-full transition-all duration-500" style="width: %.1f%%"></div>
            </div>
            <span class="w-12 text-[10px] text-orange-500 text-right font-mono font-bold">%.1f%%</span>
          </div>
        </div>
      </div>|}
      t.team
      fg_width t.fg_pct
      fg3_width t.fg3_pct
      ft_width t.ft_pct
      ts_width t.ts_pct
    ) |> String.concat "\n" in

    Printf.sprintf {|
<div class="team-shooting-comparison bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
  <h3 class="text-lg font-bold text-slate-900 dark:text-white mb-4 flex items-center gap-2">
    <span>📊</span> 팀 슈팅 효율 비교
  </h3>
  <div class="space-y-4">
    %s
  </div>
  <div class="mt-4 pt-3 border-t border-slate-200 dark:border-slate-700">
    <div class="flex flex-wrap gap-3 text-[10px] text-slate-500">
      <span class="flex items-center gap-1"><span class="w-2 h-2 bg-blue-500 rounded-full"></span>FG: 필드골</span>
      <span class="flex items-center gap-1"><span class="w-2 h-2 bg-green-500 rounded-full"></span>3PT: 3점슛</span>
      <span class="flex items-center gap-1"><span class="w-2 h-2 bg-amber-500 rounded-full"></span>FT: 자유투</span>
      <span class="flex items-center gap-1"><span class="w-2 h-2 bg-orange-500 rounded-full"></span>TS: 트루슈팅</span>
    </div>
  </div>
</div>
|} team_rows

(** Team comparison radar chart - overlay multiple teams on one radar *)
let team_radar_chart ?(selected_teams = []) (teams: team_stats list) =
  if teams = [] then ""
  else
    (* WKBL team colors *)
    let team_color name = match name with
      | "KB스타즈" -> ("#FFCC00", "rgba(255,204,0,0.2)")  (* Yellow *)
      | "삼성생명" -> ("#004098", "rgba(0,64,152,0.2)")  (* Blue *)
      | "우리은행" -> ("#003366", "rgba(0,51,102,0.2)")  (* Navy *)
      | "하나원큐" -> ("#00B8A9", "rgba(0,184,169,0.2)")  (* Teal *)
      | "신한은행" -> ("#004AAD", "rgba(0,74,173,0.2)")  (* Blue *)
      | "OK저축은행" -> ("#EE3338", "rgba(238,51,56,0.2)")  (* Red *)
      | _ -> ("#F97316", "rgba(249,115,22,0.2)")  (* Default orange *)
    in

    (* Filter teams if selection provided, otherwise use top 3 by EFF *)
    let display_teams = match selected_teams with
      | [] -> List.sort (fun a b -> compare b.eff a.eff) teams |> (fun l -> try List.filteri (fun i _ -> i < 3) l with _ -> l)
      | names -> List.filter (fun t -> List.mem t.team names) teams
    in

    if display_teams = [] then ""
    else
      (* Chart dimensions *)
      let size = 300.0 in
      let center = size /. 2.0 in
      let radius = 100.0 in

      (* 6 axes: PTS, REB, AST, STL, BLK, EFF *)
      let labels = [|"PTS"; "REB"; "AST"; "STL"; "BLK"; "EFF"|] in
      let n_axes = Array.length labels in
      let angle_step = 2.0 *. Float.pi /. float_of_int n_axes in

      (* Find max values for normalization - team_stats fields are already float *)
      let max_pts = List.fold_left (fun m (t: team_stats) -> max m t.pts) 0.0 teams in
      let max_reb = List.fold_left (fun m (t: team_stats) -> max m t.reb) 0.0 teams in
      let max_ast = List.fold_left (fun m (t: team_stats) -> max m t.ast) 0.0 teams in
      let max_stl = List.fold_left (fun m (t: team_stats) -> max m t.stl) 0.0 teams in
      let max_blk = List.fold_left (fun m (t: team_stats) -> max m t.blk) 0.0 teams in
      let max_eff = List.fold_left (fun m (t: team_stats) -> max m t.eff) 0.0 teams in

      let normalize (t: team_stats) i = match i with
        | 0 -> t.pts /. max (max_pts *. 1.1) 1.0
        | 1 -> t.reb /. max (max_reb *. 1.1) 1.0
        | 2 -> t.ast /. max (max_ast *. 1.1) 1.0
        | 3 -> t.stl /. max (max_stl *. 1.1) 1.0
        | 4 -> t.blk /. max (max_blk *. 1.1) 1.0
        | 5 -> t.eff /. max (max_eff *. 1.1) 1.0
        | _ -> 0.0
      in

      (* Generate grid circles *)
      let grid_circles = [0.2; 0.4; 0.6; 0.8; 1.0] |> List.map (fun level ->
        Printf.sprintf {|<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" stroke="#94a3b8" stroke-width="0.5" opacity="0.3"/>|}
          center center (radius *. level)
      ) |> String.concat "\n" in

      (* Generate axis lines and labels *)
      let axes_and_labels = Array.mapi (fun i label ->
        let angle = float_of_int i *. angle_step -. (Float.pi /. 2.0) in
        let x_end = center +. radius *. cos angle in
        let y_end = center +. radius *. sin angle in
        let x_label = center +. (radius +. 18.0) *. cos angle in
        let y_label = center +. (radius +. 18.0) *. sin angle in
        Printf.sprintf {|<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#94a3b8" stroke-width="0.5" opacity="0.5"/>
<text x="%.1f" y="%.1f" fill="#64748b" font-size="10" text-anchor="middle" dominant-baseline="middle" font-weight="500">%s</text>|}
          center center x_end y_end x_label y_label label
      ) labels |> Array.to_list |> String.concat "\n" in

      (* Generate polygon for each team *)
      let team_polygons = display_teams |> List.mapi (fun idx t ->
        let (stroke_color, fill_color) = team_color t.team in
        let points = Array.mapi (fun i _ ->
          let angle = float_of_int i *. angle_step -. (Float.pi /. 2.0) in
          let value = normalize t i in
          let r = radius *. value in
          let x = center +. r *. cos angle in
          let y = center +. r *. sin angle in
          Printf.sprintf "%.1f,%.1f" x y
        ) labels |> Array.to_list |> String.concat " " in
        Printf.sprintf {|<polygon points="%s" fill="%s" stroke="%s" stroke-width="2" class="radar-polygon-%c" style="animation-delay: %.1fs"/>|}
          points fill_color stroke_color (Char.chr (97 + idx)) (float_of_int idx *. 0.15)
      ) |> String.concat "\n" in

      (* Generate legend *)
      let legend_items = display_teams |> List.map (fun t ->
        let (color, _) = team_color t.team in
        Printf.sprintf {|<span class="flex items-center gap-1"><span class="w-3 h-3 rounded" style="background-color: %s"></span><span class="text-xs text-slate-600 dark:text-slate-400">%s</span></span>|}
          color t.team
      ) |> String.concat "\n" in

      Printf.sprintf {|
<div class="team-radar-chart bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
  <h3 class="text-lg font-bold text-slate-900 dark:text-white mb-4 flex items-center gap-2">
    <span>🎯</span> 팀 능력치 비교
  </h3>
  <div class="flex justify-center">
    <svg viewBox="0 0 %.0f %.0f" class="w-full max-w-[320px] h-auto" aria-label="Team comparison radar chart">
      <style>
        @keyframes radar-draw {
          from { stroke-dashoffset: 1000; opacity: 0; }
          to { stroke-dashoffset: 0; opacity: 1; }
        }
        @keyframes radar-fill {
          from { fill-opacity: 0; }
          to { fill-opacity: 0.2; }
        }
        .radar-polygon-a, .radar-polygon-b, .radar-polygon-c {
          stroke-dasharray: 1000;
          animation: radar-draw 0.8s ease-out forwards, radar-fill 0.5s ease-out 0.5s forwards;
          fill-opacity: 0;
        }
      </style>
      %s
      %s
      %s
    </svg>
  </div>
  <div class="flex flex-wrap justify-center gap-4 mt-4">
    %s
  </div>
  <div class="text-center text-[10px] text-slate-500 mt-2">효율(EFF) 상위 3팀 자동 표시</div>
</div>
|}
      size size grid_circles axes_and_labels team_polygons legend_items

(* ========== Zone-Based Shot Chart (PBP Data) ========== *)

(** Color based on FG% for zone chart *)
let zone_pct_color pct =
  if pct >= 50.0 then "#22c55e"       (* green - excellent *)
  else if pct >= 40.0 then "#84cc16"  (* lime - good *)
  else if pct >= 30.0 then "#eab308"  (* yellow - average *)
  else if pct >= 20.0 then "#f97316"  (* orange - below average *)
  else "#ef4444"                       (* red - poor *)

(** Zone-based shot chart SVG with court outline *)
let zone_shot_chart_svg (chart: player_shot_chart) =
  let paint = chart.psc_paint in
  let mid = chart.psc_mid in
  let three = chart.psc_three in
  Printf.sprintf {html|
<svg viewBox="0 0 470 500" class="w-full max-w-md mx-auto">
  <!-- Court background -->
  <rect x="0" y="0" width="470" height="500" fill="#1e293b" rx="8"/>

  <!-- Court outline -->
  <rect x="10" y="10" width="450" height="440" fill="none" stroke="#475569" stroke-width="2"/>

  <!-- Paint/Key area -->
  <rect x="135" y="330" width="200" height="120" fill="none" stroke="#475569" stroke-width="2"/>

  <!-- Free throw circle (top half) -->
  <ellipse cx="235" cy="330" rx="60" ry="60" fill="none" stroke="#475569" stroke-width="2" stroke-dasharray="5,5"/>

  <!-- Restricted area (골밑) -->
  <ellipse cx="235" cy="420" rx="40" ry="30" fill="none" stroke="#475569" stroke-width="1.5"/>

  <!-- Basket -->
  <circle cx="235" cy="430" r="8" fill="none" stroke="#f97316" stroke-width="2"/>
  <line x1="235" y1="438" x2="235" y2="450" stroke="#f97316" stroke-width="2"/>

  <!-- Three-point line arc -->
  <path d="M 30 450 Q 30 150 235 150 Q 440 150 440 450" fill="none" stroke="#475569" stroke-width="2"/>

  <!-- Three-point corners -->
  <line x1="30" y1="450" x2="30" y2="380" stroke="#475569" stroke-width="2"/>
  <line x1="440" y1="450" x2="440" y2="380" stroke="#475569" stroke-width="2"/>

  <!-- Zone: Paint (center-bottom) -->
  <g transform="translate(235, 395)">
    <circle cx="0" cy="0" r="45" fill="%s" fill-opacity="%s" class="transition-all"/>
    <text x="0" y="-18" text-anchor="middle" fill="white" font-size="11" font-weight="bold">페인트존</text>
    <text x="0" y="2" text-anchor="middle" fill="white" font-size="16" font-weight="bold">%d/%d</text>
    <text x="0" y="20" text-anchor="middle" fill="white" font-size="14">%.1f%%</text>
  </g>

  <!-- Zone: Mid-range (center-middle) -->
  <g transform="translate(235, 270)">
    <circle cx="0" cy="0" r="50" fill="%s" fill-opacity="%s" class="transition-all"/>
    <text x="0" y="-22" text-anchor="middle" fill="white" font-size="11" font-weight="bold">미드레인지</text>
    <text x="0" y="2" text-anchor="middle" fill="white" font-size="16" font-weight="bold">%d/%d</text>
    <text x="0" y="22" text-anchor="middle" fill="white" font-size="14">%.1f%%</text>
  </g>

  <!-- Zone: Three-point (center-top) -->
  <g transform="translate(235, 100)">
    <circle cx="0" cy="0" r="55" fill="%s" fill-opacity="%s" class="transition-all"/>
    <text x="0" y="-25" text-anchor="middle" fill="white" font-size="11" font-weight="bold">3점</text>
    <text x="0" y="0" text-anchor="middle" fill="white" font-size="16" font-weight="bold">%d/%d</text>
    <text x="0" y="22" text-anchor="middle" fill="white" font-size="14">%.1f%%</text>
  </g>

  <!-- Total stats -->
  <g transform="translate(235, 480)">
    <text x="0" y="0" text-anchor="middle" fill="#94a3b8" font-size="12">
      전체: %d/%d (%.1f%%)
    </text>
  </g>
</svg>
  |html}
    (* Paint zone *)
    (zone_pct_color paint.zs_pct) (if paint.zs_attempts > 0 then "0.7" else "0.3")
    paint.zs_made paint.zs_attempts paint.zs_pct
    (* Mid-range zone *)
    (zone_pct_color mid.zs_pct) (if mid.zs_attempts > 0 then "0.7" else "0.3")
    mid.zs_made mid.zs_attempts mid.zs_pct
    (* Three-point zone *)
    (zone_pct_color three.zs_pct) (if three.zs_attempts > 0 then "0.7" else "0.3")
    three.zs_made three.zs_attempts three.zs_pct
    (* Total *)
    chart.psc_total_made chart.psc_total_attempts chart.psc_total_pct

(** Zone-based shot chart page *)
let zone_shot_chart_page (chart: player_shot_chart) ~seasons ~current_season =
  let season_options = seasons |> List.map (fun (code, name) ->
    Printf.sprintf {|<option value="%s"%s>%s</option>|}
      code
      (if code = current_season then " selected" else "")
      name
  ) |> String.concat "\n" in
  let content = Printf.sprintf {html|
<div class="space-y-6">
  <!-- Header -->
  <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
    <div>
      <h1 class="text-2xl font-bold text-slate-900 dark:text-white">
        🎯 %s 슛 차트
      </h1>
      <p class="text-slate-600 dark:text-slate-400">%s · PBP 기반 존 분석</p>
    </div>

    <!-- Season filter -->
    <form hx-get="/player/%s/shots" hx-target="#shot-chart-content" hx-swap="innerHTML" class="flex gap-2">
      <select name="season" class="px-3 py-2 bg-white dark:bg-slate-800 border border-slate-300 dark:border-slate-600 rounded-lg text-sm">
        <option value="ALL"%s>전체 시즌</option>
        %s
      </select>
      <button type="submit" class="px-4 py-2 bg-orange-500 hover:bg-orange-600 text-white rounded-lg text-sm font-medium transition">
        적용
      </button>
    </form>
  </div>

  <!-- Shot Chart -->
  <div id="shot-chart-content" class="bg-white dark:bg-slate-800 rounded-xl p-6 shadow-lg">
    %s

    <!-- Stats breakdown cards -->
    <div class="mt-6 grid grid-cols-3 gap-4 text-center">
      <div class="p-4 bg-slate-50 dark:bg-slate-700/50 rounded-lg">
        <div class="text-sm text-slate-500 dark:text-slate-400">페인트존</div>
        <div class="text-2xl font-bold text-emerald-500">%d</div>
        <div class="text-xs text-slate-400">성공 (미스 미집계)</div>
      </div>
      <div class="p-4 bg-slate-50 dark:bg-slate-700/50 rounded-lg">
        <div class="text-sm text-slate-500 dark:text-slate-400">미드레인지</div>
        <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
        <div class="text-xs text-slate-400">%d/%d</div>
      </div>
      <div class="p-4 bg-slate-50 dark:bg-slate-700/50 rounded-lg">
        <div class="text-sm text-slate-500 dark:text-slate-400">3점</div>
        <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
        <div class="text-xs text-slate-400">%d/%d</div>
      </div>
    </div>

    <!-- Color legend -->
    <div class="mt-4 flex flex-wrap items-center justify-center gap-3 text-xs text-slate-500 dark:text-slate-400">
      <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-lime-500"></span> 40%%+</span>
      <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-yellow-500"></span> 30-40%%</span>
      <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-orange-500"></span> 20-30%%</span>
      <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-red-500"></span> &lt;20%%</span>
    </div>
  </div>

  <!-- Back link -->
  <div class="text-center">
    <a href="/player/%s" class="text-orange-500 hover:text-orange-600 transition text-sm">
      ← 선수 프로필로 돌아가기
    </a>
  </div>
</div>
  |html}
    chart.psc_player_name
    chart.psc_team_name
    chart.psc_player_id
    (if current_season = "ALL" then " selected" else "")
    season_options
    (zone_shot_chart_svg chart)
    (* Paint stats - only show makes since misses aren't tracked separately *)
    chart.psc_paint.zs_made
    (* Mid stats *)
    (zone_pct_color chart.psc_mid.zs_pct) chart.psc_mid.zs_pct
    chart.psc_mid.zs_made chart.psc_mid.zs_attempts
    (* Three stats *)
    (zone_pct_color chart.psc_three.zs_pct) chart.psc_three.zs_pct
    chart.psc_three.zs_made chart.psc_three.zs_attempts
    chart.psc_player_id
  in
  layout
    ~title:(Printf.sprintf "%s 슛 차트 | WKBL Analytics" chart.psc_player_name)
    ~canonical_path:(Printf.sprintf "/player/%s/shots" chart.psc_player_id)
    ~description:(Printf.sprintf "%s(%s)의 존별 슛 분포 - 페인트존, 미드레인지, 3점 성공률" chart.psc_player_name chart.psc_team_name)
    ~content:content ()

(** HTMX partial for zone shot chart (filter results) *)
let zone_shot_chart_partial (chart: player_shot_chart) =
  Printf.sprintf {html|
%s

<!-- Stats breakdown cards -->
<div class="mt-6 grid grid-cols-3 gap-4 text-center">
  <div class="p-4 bg-slate-50 dark:bg-slate-700/50 rounded-lg">
    <div class="text-sm text-slate-500 dark:text-slate-400">페인트존</div>
    <div class="text-2xl font-bold text-emerald-500">%d</div>
    <div class="text-xs text-slate-400">성공 (미스 미집계)</div>
  </div>
  <div class="p-4 bg-slate-50 dark:bg-slate-700/50 rounded-lg">
    <div class="text-sm text-slate-500 dark:text-slate-400">미드레인지</div>
    <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
    <div class="text-xs text-slate-400">%d/%d</div>
  </div>
  <div class="p-4 bg-slate-50 dark:bg-slate-700/50 rounded-lg">
    <div class="text-sm text-slate-500 dark:text-slate-400">3점</div>
    <div class="text-2xl font-bold" style="color: %s">%.1f%%</div>
    <div class="text-xs text-slate-400">%d/%d</div>
  </div>
</div>

<!-- Color legend -->
<div class="mt-4 flex flex-wrap items-center justify-center gap-3 text-xs text-slate-500 dark:text-slate-400">
  <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-lime-500"></span> 40%%+</span>
  <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-yellow-500"></span> 30-40%%</span>
  <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-orange-500"></span> 20-30%%</span>
  <span class="flex items-center gap-1"><span class="w-3 h-3 rounded-full bg-red-500"></span> &lt;20%%</span>
</div>
  |html}
    (zone_shot_chart_svg chart)
    chart.psc_paint.zs_made
    (zone_pct_color chart.psc_mid.zs_pct) chart.psc_mid.zs_pct
    chart.psc_mid.zs_made chart.psc_mid.zs_attempts
    (zone_pct_color chart.psc_three.zs_pct) chart.psc_three.zs_pct
    chart.psc_three.zs_made chart.psc_three.zs_attempts

(* ============================================================================
   Phase 3.3: New Charts
   ============================================================================ *)

(** Team Comparison Stacked Bar Chart - 팀별 공격/수비 스탯 비교 *)
let team_comparison_bar_chart (teams: team_stats list) =
  let width = 600 in
  let height = 320 in
  let bar_height = 28 in
  let padding_left = 80 in
  let padding_top = 40 in
  let chart_width = width - padding_left - 40 in

  (* Sort by efficiency *)
  let sorted = List.sort (fun (a: team_stats) (b: team_stats) -> compare b.eff a.eff) teams in
  let max_pts = List.fold_left (fun acc (t: team_stats) -> Float.max acc t.pts) 0.0 sorted in

  let bars = sorted |> List.mapi (fun i (t: team_stats) ->
    let y = padding_top + (i * (bar_height + 8)) in
    let pts_width = int_of_float (t.pts /. max_pts *. float_of_int chart_width) in
    let ast_width = int_of_float (t.ast /. max_pts *. float_of_int chart_width *. 0.3) in
    let reb_width = int_of_float (t.reb /. max_pts *. float_of_int chart_width *. 0.3) in
    let delay = float_of_int i *. 0.1 in
    Printf.sprintf {|
      <text x="%d" y="%d" class="fill-slate-600 dark:fill-slate-300 text-xs" text-anchor="end">%s</text>
      <g class="bar-group" style="animation: bar-grow 0.6s ease-out forwards; animation-delay: %.2fs; opacity: 0; transform: scaleX(0); transform-origin: left;">
        <rect x="%d" y="%d" width="%d" height="%d" fill="#f97316" rx="4"/>
        <rect x="%d" y="%d" width="%d" height="%d" fill="#22c55e" rx="4" opacity="0.8"/>
        <rect x="%d" y="%d" width="%d" height="%d" fill="#3b82f6" rx="4" opacity="0.8"/>
      </g>
      <text x="%d" y="%d" class="fill-slate-500 dark:fill-slate-400 text-xs">%.1f</text>
    |}
      (padding_left - 8) (y + bar_height / 2 + 4) t.team
      delay
      padding_left y pts_width bar_height
      (padding_left + pts_width + 2) y ast_width bar_height
      (padding_left + pts_width + ast_width + 4) y reb_width bar_height
      (padding_left + pts_width + ast_width + reb_width + 12) (y + bar_height / 2 + 4) t.eff
  ) |> String.concat "\n" in

  Printf.sprintf {|
<svg viewBox="0 0 %d %d" class="w-full h-auto" preserveAspectRatio="xMidYMid meet">
  <style>
    @keyframes bar-grow {
      from { opacity: 0; transform: scaleX(0); }
      to { opacity: 1; transform: scaleX(1); }
    }
  </style>
  <!-- Title -->
  <text x="%d" y="20" class="fill-slate-700 dark:fill-slate-200 text-sm font-medium">팀별 공격력 비교</text>

  <!-- Legend -->
  <g transform="translate(%d, 8)">
    <rect x="0" y="0" width="12" height="12" fill="#f97316" rx="2"/>
    <text x="16" y="10" class="fill-slate-500 dark:fill-slate-400 text-xs">득점</text>
    <rect x="50" y="0" width="12" height="12" fill="#22c55e" rx="2"/>
    <text x="66" y="10" class="fill-slate-500 dark:fill-slate-400 text-xs">어시스트</text>
    <rect x="120" y="0" width="12" height="12" fill="#3b82f6" rx="2"/>
    <text x="136" y="10" class="fill-slate-500 dark:fill-slate-400 text-xs">리바운드</text>
  </g>

  <!-- Bars -->
  %s
</svg>
  |} width height padding_left (width - 200) bars

(** Quarter Performance Heatmap - 쿼터별 퍼포먼스 히트맵
    Takes a list of (quarter, stat_value) pairs *)
type quarter_stat = {
  qs_quarter: string;  (* "Q1", "Q2", "Q3", "Q4", "OT" *)
  qs_pts: float;
  qs_fg_pct: float;
  qs_turnovers: float;
}

let quarter_heatmap (quarters: quarter_stat list) =
  let width = 400 in
  let height = 120 in
  let cell_width = 70 in
  let cell_height = 24 in
  let padding_left = 80 in
  let padding_top = 32 in

  (* Normalize to 0-1 for color intensity *)
  let max_pts = List.fold_left (fun acc q -> Float.max acc q.qs_pts) 1.0 quarters in
  let max_fg = List.fold_left (fun acc q -> Float.max acc q.qs_fg_pct) 1.0 quarters in

  let cells = quarters |> List.mapi (fun i q ->
    let x = padding_left + (i * (cell_width + 4)) in
    (* PTS row *)
    let pts_intensity = q.qs_pts /. max_pts in
    let pts_color = Printf.sprintf "rgba(249, 115, 22, %.2f)" (0.3 +. pts_intensity *. 0.7) in
    (* FG% row *)
    let fg_intensity = q.qs_fg_pct /. max_fg in
    let fg_color = Printf.sprintf "rgba(34, 197, 94, %.2f)" (0.3 +. fg_intensity *. 0.7) in
    Printf.sprintf {|
      <!-- %s header -->
      <text x="%d" y="%d" class="fill-slate-600 dark:fill-slate-300 text-xs font-medium" text-anchor="middle">%s</text>
      <!-- PTS cell -->
      <rect x="%d" y="%d" width="%d" height="%d" fill="%s" rx="4"/>
      <text x="%d" y="%d" class="fill-white text-xs font-medium" text-anchor="middle">%.1f</text>
      <!-- FG%% cell -->
      <rect x="%d" y="%d" width="%d" height="%d" fill="%s" rx="4"/>
      <text x="%d" y="%d" class="fill-white text-xs font-medium" text-anchor="middle">%.1f%%</text>
    |}
      q.qs_quarter
      (x + cell_width / 2) (padding_top - 8) q.qs_quarter
      x padding_top cell_width cell_height pts_color
      (x + cell_width / 2) (padding_top + cell_height / 2 + 4) q.qs_pts
      x (padding_top + cell_height + 4) cell_width cell_height fg_color
      (x + cell_width / 2) (padding_top + cell_height + 4 + cell_height / 2 + 4) q.qs_fg_pct
  ) |> String.concat "\n" in

  Printf.sprintf {|
<svg viewBox="0 0 %d %d" class="w-full h-auto" preserveAspectRatio="xMidYMid meet">
  <!-- Row labels -->
  <text x="%d" y="%d" class="fill-slate-500 dark:fill-slate-400 text-xs" text-anchor="end">득점</text>
  <text x="%d" y="%d" class="fill-slate-500 dark:fill-slate-400 text-xs" text-anchor="end">FG%%</text>

  <!-- Cells -->
  %s
</svg>
  |} width height
    (padding_left - 8) (padding_top + cell_height / 2 + 4)
    (padding_left - 8) (padding_top + cell_height + 4 + cell_height / 2 + 4)
    cells

(** Win Rate Trend Chart - 승률 추이 라인 차트
    Takes a list of (date, win_rate) pairs *)
type win_rate_point = {
  wrp_date: string;
  wrp_win_rate: float;  (* 0.0 to 100.0 *)
  wrp_wins: int;
  wrp_losses: int;
}

let win_rate_trend_chart (points: win_rate_point list) =
  let width = 500.0 in
  let height = 200.0 in
  let padding_left = 50.0 in
  let padding_right = 20.0 in
  let padding_top = 30.0 in
  let padding_bottom = 40.0 in
  let plot_width = width -. padding_left -. padding_right in
  let plot_height = height -. padding_top -. padding_bottom in

  let n = List.length points in
  if n = 0 then "<div class='text-slate-500 text-center p-4'>데이터 없음</div>"
  else
    let x_step = plot_width /. float_of_int (max 1 (n - 1)) in

    (* Generate path and points *)
    let path_points = points |> List.mapi (fun i p ->
      let x = padding_left +. (float_of_int i *. x_step) in
      let y = padding_top +. plot_height -. (p.wrp_win_rate /. 100.0 *. plot_height) in
      (i, x, y, p)
    ) in

    let path_d = path_points
      |> List.map (fun (i, x, y, _) ->
          if i = 0 then Printf.sprintf "M%.2f,%.2f" x y
          else Printf.sprintf " L%.2f,%.2f" x y)
      |> String.concat "" in

    let circles = path_points |> List.map (fun (i, x, y, p) ->
      let delay = 0.8 +. (float_of_int i *. 0.05) in
      Printf.sprintf {|
        <circle cx="%.2f" cy="%.2f" r="5"
                class="fill-orange-500 stroke-white dark:stroke-slate-900 cursor-pointer win-point"
                stroke-width="2" style="animation-delay: %.2fs">
          <title>%s
승률: %.1f%% (%d승 %d패)</title>
        </circle>
      |} x y delay p.wrp_date p.wrp_win_rate p.wrp_wins p.wrp_losses
    ) |> String.concat "\n" in

    (* X-axis labels (show every nth) *)
    let label_step = max 1 (n / 5) in
    let x_labels = path_points
      |> List.filteri (fun i _ -> i mod label_step = 0 || i = n - 1)
      |> List.map (fun (_, x, _, p) ->
          let short_date = if String.length p.wrp_date >= 5
            then String.sub p.wrp_date (String.length p.wrp_date - 5) 5
            else p.wrp_date in
          Printf.sprintf {|<text x="%.2f" y="%.2f" class="fill-slate-500 dark:fill-slate-400 text-xs" text-anchor="middle">%s</text>|}
            x (height -. 10.0) short_date)
      |> String.concat "\n" in

    Printf.sprintf {|
<svg viewBox="0 0 %.0f %.0f" class="w-full h-auto" preserveAspectRatio="xMidYMid meet">
  <style>
    @keyframes win-line-draw {
      from { stroke-dashoffset: 1500; }
      to { stroke-dashoffset: 0; }
    }
    .win-line { stroke-dasharray: 1500; animation: win-line-draw 1s ease-out forwards; }
    .win-point { opacity: 0; animation: fade-in 0.3s ease-out forwards; }
    @keyframes fade-in {
      from { opacity: 0; }
      to { opacity: 1; }
    }
  </style>

  <!-- Y-axis grid lines -->
  <line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" stroke="#e2e8f0" stroke-dasharray="4" class="dark:stroke-slate-700"/>
  <line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" stroke="#e2e8f0" stroke-dasharray="4" class="dark:stroke-slate-700"/>
  <line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" stroke="#f97316" stroke-dasharray="4" opacity="0.5"/>

  <!-- Y-axis labels -->
  <text x="%.2f" y="%.2f" class="fill-slate-500 dark:fill-slate-400 text-xs" text-anchor="end">100%%</text>
  <text x="%.2f" y="%.2f" class="fill-orange-500 text-xs font-medium" text-anchor="end">50%%</text>
  <text x="%.2f" y="%.2f" class="fill-slate-500 dark:fill-slate-400 text-xs" text-anchor="end">0%%</text>

  <!-- Title -->
  <text x="%.2f" y="18" class="fill-slate-700 dark:fill-slate-200 text-sm font-medium">승률 추이</text>

  <!-- Trend line -->
  <path d="%s" fill="none" class="stroke-orange-500 win-line" stroke-width="3" stroke-linecap="round" stroke-linejoin="round"/>

  <!-- Data points -->
  %s

  <!-- X-axis labels -->
  %s
</svg>
    |} width height
    (* Grid lines: 100%, 50%, 0% *)
    padding_left padding_top (width -. padding_right) padding_top
    padding_left (padding_top +. plot_height /. 2.0) (width -. padding_right) (padding_top +. plot_height /. 2.0)
    padding_left (padding_top +. plot_height) (width -. padding_right) (padding_top +. plot_height)
    (* Y labels *)
    (padding_left -. 8.0) (padding_top +. 4.0)
    (padding_left -. 8.0) (padding_top +. plot_height /. 2.0 +. 4.0)
    (padding_left -. 8.0) (padding_top +. plot_height +. 4.0)
    (* Title *)
    padding_left
    path_d circles x_labels
