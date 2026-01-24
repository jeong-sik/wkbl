(** Basketball Court Charts - Shot distribution visualization

    Since WKBL doesn't provide shot coordinates, we visualize:
    - Shot distribution by zone (3PT, 2PT, FT)
    - Shooting percentages per zone
    - Made/Missed attempts
*)

open Domain

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
