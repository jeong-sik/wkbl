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
    | '\'' -> "&#x27;"
    | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""

(** Normalize names for duplicate detection (strip common escape artifacts). *)
let normalize_name s =
  normalize_label s

(** Naive substring search (byte-based). *)
let find_substring_from ~sub s ~from =
  let sub_len = String.length sub in
  let len = String.length s in
  if sub_len = 0 then None
  else
    let rec loop i =
      if i + sub_len > len then None
      else if String.sub s i sub_len = sub then Some i
      else loop (i + 1)
    in
    loop (max 0 from)

(** Best-effort extraction of contract duration from official trade text. *)
let extract_contract_years (text : string) : int option =
  let key = "계약" in
  let year_marker = "년" in
  match find_substring_from ~sub:key text ~from:0 with
  | None -> None
  | Some idx ->
      let start = idx + String.length key in
      let len = String.length text in
      let is_digit c = c >= '0' && c <= '9' in
      let rec scan i =
        if i >= len then None
        else if is_digit text.[i] then (
          let j = ref i in
          while !j < len && is_digit text.[!j] do
            incr j
          done;
          let digits = String.sub text i (!j - i) in
          let k = ref !j in
          while !k < len && (text.[!k] = ' ' || text.[!k] = '\t') do
            incr k
          done;
          if !k + String.length year_marker <= len
             && String.sub text !k (String.length year_marker) = year_marker
          then
            int_of_string_opt digits
          else
            scan (!j + 1)
        ) else
          scan (i + 1)
      in
      scan start

(** Short player id for compact UI (keeps disambiguation readable). *)
let short_player_id player_id =
  let s = String.trim player_id in
  let len = String.length s in
  if len <= 3 then s else String.sub s (len - 3) 3

let player_id_badge player_id =
  let short = short_player_id player_id in
  Printf.sprintf
    {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/50 border border-slate-300 dark:border-slate-700/50 text-[10px] font-mono text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition-colors whitespace-nowrap cursor-help" title="player_id: %s" aria-label="player id %s">ID %s</span>|html}
    (escape_html player_id)
    (escape_html player_id)
    (escape_html short)

(** EFF badge with color-coded value (Gemini UX feedback: MVP 핵심 지표 강조)
    - 20+ purple (elite)
    - 15+ red (star)
    - 10+ green (solid)
    - <10 slate (role player) *)
let eff_badge ?(show_label=false) eff =
  let (bg, text) =
    if eff >= 20.0 then ("bg-purple-500/20 border-purple-500/40", "text-purple-500")
    else if eff >= 15.0 then ("bg-red-500/20 border-red-500/40", "text-red-500")
    else if eff >= 10.0 then ("bg-green-500/20 border-green-500/40", "text-green-500")
    else ("bg-slate-500/10 border-slate-500/20", "text-slate-500")
  in
  let label = if show_label then "EFF " else "" in
  Printf.sprintf
    {html|<span class="inline-flex items-center px-1.5 py-0.5 rounded border text-[10px] font-mono font-bold %s %s" title="Efficiency: %.1f">%s%.1f</span>|html}
    bg text eff label eff

(** Empty state component with icon - consistent UX for no-data scenarios *)
type empty_state_icon =
  | SearchIcon
  | ChartIcon
  | UsersIcon
  | CalendarIcon
  | TableIcon
  | BasketballIcon

let empty_state_svg = function
  | SearchIcon -> {svg|<svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path></svg>|svg}
  | ChartIcon -> {svg|<svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>|svg}
  | UsersIcon -> {svg|<svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"></path></svg>|svg}
  | CalendarIcon -> {svg|<svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>|svg}
  | TableIcon -> {svg|<svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M3 10h18M3 14h18m-9-4v8m-7 0h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v8a2 2 0 002 2z"></path></svg>|svg}
  | BasketballIcon -> {svg|<svg class="empty-state-icon" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="10" stroke-width="1.5"/><path stroke-width="1.5" d="M12 2v20M2 12h20M4.93 4.93l14.14 14.14M19.07 4.93L4.93 19.07"/></svg>|svg}

let empty_state ?(icon=SearchIcon) ?(action="") title description =
  Printf.sprintf
    {html|<div class="empty-state" role="status" aria-live="polite">
  %s
  <h3 class="empty-state-title">%s</h3>
  <p class="empty-state-description">%s</p>
  %s
</div>|html}
    (empty_state_svg icon)
    (escape_html title)
    (escape_html description)
    (if action = "" then "" else Printf.sprintf {html|<div class="empty-state-action">%s</div>|html} action)

(** Radar chart component for player stat comparison

    Creates a hexagonal radar chart SVG with two overlapping polygons.
    Each axis represents a stat category (PTS, REB, AST, STL, BLK, EFF).
    Values are normalized to 0-100 scale for display.

    @param labels List of 6 stat labels
    @param values_a Player A's normalized values (0-100 each)
    @param values_b Player B's normalized values (0-100 each)
    @param color_a Player A's color (hex)
    @param color_b Player B's color (hex)
*)
(** WKBL league average normalized values (based on typical player averages) *)
let league_avg_normalized = [
  40.0;  (* PTS: ~10 ppg / 25 max = 40% *)
  42.0;  (* REB: ~5 rpg / 12 max = 42% *)
  25.0;  (* AST: ~2 apg / 8 max = 25% *)
  33.0;  (* STL: ~1 spg / 3 max = 33% *)
  25.0;  (* BLK: ~0.5 bpg / 2 max = 25% *)
  40.0;  (* EFF: ~10 / 25 max = 40% *)
]

let radar_chart ?(show_league_avg=false) ~labels ~values_a ~values_b ~color_a ~color_b () =
  (* SVG viewBox dimensions *)
  let cx, cy = 150.0, 150.0 in
  let radius = 100.0 in
  let n = 6 in (* 6 axes for hexagon *)
  let pi = Float.pi in

  (* Calculate point coordinates on the radar *)
  let point_at_radius r i =
    let angle = (float_of_int i *. 2.0 *. pi /. float_of_int n) -. (pi /. 2.0) in
    let x = cx +. r *. Float.cos angle in
    let y = cy +. r *. Float.sin angle in
    (x, y)
  in

  (* Generate grid lines (concentric hexagons) *)
  let grid_levels = [0.25; 0.5; 0.75; 1.0] in
  let grid_paths = List.map (fun level ->
    let points = List.init n (fun i ->
      let (x, y) = point_at_radius (radius *. level) i in
      Printf.sprintf "%.1f,%.1f" x y
    ) in
    Printf.sprintf {svg|<polygon points="%s" fill="none" stroke="currentColor" stroke-width="0.5" class="text-slate-300 dark:text-slate-600" opacity="%.1f"/>|svg}
      (String.concat " " points)
      (if level = 1.0 then 1.0 else 0.5)
  ) grid_levels in

  (* Generate axis lines from center *)
  let axis_lines = List.init n (fun i ->
    let (x, y) = point_at_radius radius i in
    Printf.sprintf {svg|<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="currentColor" stroke-width="0.5" class="text-slate-300 dark:text-slate-600"/>|svg}
      cx cy x y
  ) in

  (* Generate axis labels *)
  let axis_labels = List.mapi (fun i label ->
    let (x, y) = point_at_radius (radius +. 20.0) i in
    let anchor =
      if Float.abs (x -. cx) < 5.0 then "middle"
      else if x < cx then "end"
      else "start"
    in
    Printf.sprintf {svg|<text x="%.1f" y="%.1f" text-anchor="%s" dominant-baseline="middle" class="text-xs fill-slate-500 dark:fill-slate-400 font-medium">%s</text>|svg}
      x (y +. 4.0) anchor label
  ) labels in

  (* Generate data polygon for player A *)
  let points_a = List.mapi (fun i v ->
    let normalized = Float.min 100.0 (Float.max 0.0 v) /. 100.0 in
    let (x, y) = point_at_radius (radius *. normalized) i in
    Printf.sprintf "%.1f,%.1f" x y
  ) values_a in

  (* Generate data polygon for player B *)
  let points_b = List.mapi (fun i v ->
    let normalized = Float.min 100.0 (Float.max 0.0 v) /. 100.0 in
    let (x, y) = point_at_radius (radius *. normalized) i in
    Printf.sprintf "%.1f,%.1f" x y
  ) values_b in

  (* Generate league average polygon (dashed line, no fill) *)
  let league_avg_polygon = if show_league_avg then
    let points_avg = List.mapi (fun i v ->
      let normalized = Float.min 100.0 (Float.max 0.0 v) /. 100.0 in
      let (x, y) = point_at_radius (radius *. normalized) i in
      Printf.sprintf "%.1f,%.1f" x y
    ) league_avg_normalized in
    Printf.sprintf {|<polygon points="%s" fill="none" stroke="#64748b" stroke-width="1.5" stroke-dasharray="4,3" opacity="0.6"/>|}
      (String.concat " " points_avg)
  else "" in

  (* Legend for league average *)
  let league_avg_legend = if show_league_avg then
    {|<g transform="translate(150, 285)">
      <line x1="-40" y1="0" x2="-15" y2="0" stroke="#64748b" stroke-width="1.5" stroke-dasharray="4,3" opacity="0.6"/>
      <text x="-8" y="4" class="text-[10px] fill-slate-500 dark:fill-slate-400">리그 평균</text>
    </g>|}
  else "" in

  (* Assemble the SVG with CSS animation *)
  Printf.sprintf
    {svg|<svg viewBox="0 0 300 300" class="w-full max-w-xs mx-auto" role="img" aria-label="선수 스탯 비교 레이더 차트">
  <style>
    @keyframes radar-draw {
      from { stroke-dashoffset: 1000; opacity: 0; }
      to { stroke-dashoffset: 0; opacity: 1; }
    }
    @keyframes radar-fill {
      from { fill-opacity: 0; }
      to { fill-opacity: 0.2; }
    }
    .radar-polygon-a {
      stroke-dasharray: 1000;
      animation: radar-draw 0.8s ease-out forwards, radar-fill 0.5s ease-out 0.5s forwards;
      fill-opacity: 0;
    }
    .radar-polygon-b {
      stroke-dasharray: 1000;
      animation: radar-draw 0.8s ease-out 0.2s forwards, radar-fill 0.5s ease-out 0.6s forwards;
      fill-opacity: 0;
    }
  </style>
  <title>선수 스탯 비교</title>
  <!-- Grid -->
  %s
  <!-- Axes -->
  %s
  <!-- League average (dashed, background) -->
  %s
  <!-- Player B polygon (back) -->
  <polygon class="radar-polygon-b" points="%s" fill="%s" stroke="%s" stroke-width="2"/>
  <!-- Player A polygon (front) -->
  <polygon class="radar-polygon-a" points="%s" fill="%s" stroke="%s" stroke-width="2"/>
  <!-- Labels -->
  %s
  <!-- Legend -->
  %s
</svg>|svg}
    (String.concat "\n  " grid_paths)
    (String.concat "\n  " axis_lines)
    league_avg_polygon
    (String.concat " " points_b) color_b color_b
    (String.concat " " points_a) color_a color_a
    (String.concat "\n  " axis_labels)
    league_avg_legend

(** Normalize stats to 0-100 scale for radar chart display.
    Uses typical WKBL max values as reference points. *)
let normalize_stat_for_radar stat_type value =
  let max_val = match stat_type with
    | `Points -> 25.0      (* ~25 ppg is elite *)
    | `Rebounds -> 12.0    (* ~12 rpg is elite *)
    | `Assists -> 8.0      (* ~8 apg is elite *)
    | `Steals -> 3.0       (* ~3 spg is elite *)
    | `Blocks -> 2.0       (* ~2 bpg is elite *)
    | `Efficiency -> 25.0  (* ~25 EFF is elite *)
  in
  Float.min 100.0 (value /. max_val *. 100.0)

(** Player image component with fallback *)
let player_img_tag ?(class_name="w-12 h-12") player_id _player_name =
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
    {html|<img src="%s" alt="" aria-hidden="true" class="%s rounded-full object-cover bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 shadow-sm" loading="lazy" decoding="async" data-placeholder="%s" onerror="if(!this.dataset.placeholderApplied){this.dataset.placeholderApplied='1';this.src=this.dataset.placeholder;}">|html}
    (escape_html src)
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
  | Some f -> Printf.sprintf {html|<img src="/static/images/%s" alt="" aria-hidden="true" class="%s object-contain" loading="lazy" decoding="async">|html} f class_name
  | None ->
      Printf.sprintf
        {html|<div class="%s bg-slate-100 dark:bg-slate-800 rounded flex items-center justify-center text-xs">🏀</div>|html}
        class_name

(** Team badge component *)
let team_badge ?(max_width="max-w-[130px] sm:max-w-[200px]") team_name =
  let display = normalize_label team_name in
  let color =
    team_code_of_string display
    |> Option.map team_code_to_color
    |> Option.value ~default:"#666"
  in
  Printf.sprintf
    {html|<a href="/team/%s" class="inline-flex min-w-0 items-center gap-1.5 px-1.5 sm:px-2 py-0.5 rounded text-[11px] sm:text-xs font-medium hover:brightness-125 transition" style="background-color: %s20; color: %s; border: 1px solid %s40" title="%s">%s<span class="truncate min-w-0 %s">%s</span></a>|html}
    (Uri.pct_encode team_name)
    color
    color
    color
    (escape_html display)
    (team_logo_tag ~class_name:"w-4 h-4 shrink-0" display)
    max_width
    (escape_html display)


(** Stat cell with formatting - two-line for visual consistency *)
let stat_cell ?(highlight=false) ?(extra_classes="") ?(label="") value =
  let class_name = if highlight then "text-orange-600 dark:text-orange-400 font-bold" else "text-slate-700 dark:text-slate-300" in
  if label = "" then
    Printf.sprintf {html|<td class="px-3 py-2 text-right %s font-mono %s">%.1f</td>|html} class_name extra_classes value
  else
    Printf.sprintf
      {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap">%s</span></div></td>|html}
      extra_classes class_name value label

let format_int_commas n =
  let abs_n = abs n in
  let s = string_of_int abs_n in
  let len = String.length s in
  if len <= 3 then
    if n < 0 then "-" ^ s else s
  else
    let buf = Buffer.create (len + (len / 3)) in
    let first = len mod 3 in
    let first = if first = 0 then 3 else first in
    Buffer.add_substring buf s 0 first;
    let i = ref first in
    while !i < len do
      Buffer.add_char buf ',';
      Buffer.add_substring buf s !i 3;
      i := !i + 3
    done;
    let res = Buffer.contents buf in
    if n < 0 then "-" ^ res else res

(** Compact number format for table cells: 999 -> "999", 1234 -> "1.2K"
    Returns a span with aria-label for accessibility (screen readers get full number) *)
let format_int_compact ?(with_aria=false) n =
  let abs_n = abs n in
  let compact =
    if abs_n < 1000 then string_of_int abs_n
    else Printf.sprintf "%.1fK" (float_of_int abs_n /. 1000.0)
  in
  let display = if n < 0 then "-" ^ compact else compact in
  if with_aria && abs_n >= 1000 then
    (* Wrap in span with aria-label for screen readers and title for hover tooltip *)
    Printf.sprintf {html|<span aria-label="%d" title="%s">%s</span>|html}
      n (Printf.sprintf "%d" n |> fun s ->
         (* Add thousands separator for readability *)
         let rec add_commas acc i s =
           if i < 0 then acc
           else
             let c = String.make 1 s.[i] in
             let pos_from_end = String.length s - 1 - i in
             if pos_from_end > 0 && pos_from_end mod 3 = 0 then
               add_commas ("," ^ c ^ acc) (i - 1) s
             else
               add_commas (c ^ acc) (i - 1) s
         in
         add_commas "" (String.length s - 1) s) display
  else
    display

let stat_total_cell ?(highlight=false) ?(extra_classes="") (avg_value: float) (total_value: int) =
  let class_name = if highlight then "text-orange-600 dark:text-orange-400 font-bold" else "text-slate-700 dark:text-slate-300" in
  let total_str = format_int_compact ~with_aria:true total_value in
  Printf.sprintf
    {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap" title="통산 합계 (Career Total)">Σ%s</span></div></td>|html}
    extra_classes
    class_name
    avg_value
    total_str

(** Points cell with career total *)
let points_total_cell ?(extra_classes="") (avg_points: float) (total_points: int) =
  let total_str = format_int_compact ~with_aria:true total_points in
  Printf.sprintf
    {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="text-orange-600 dark:text-orange-400 font-bold font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap" title="통산 합계 (Career Total)">Σ%s</span></div></td>|html}
    extra_classes
    avg_points
    total_str

(** Margin cell (signed, colored) - two-line for visual consistency *)
let margin_cell ?(extra_classes="") value =
  let class_name =
    if value > 0.0 then "text-sky-600 dark:text-sky-400 font-bold"
    else if value < 0.0 then "text-rose-600 dark:text-rose-400 font-bold"
    else "text-slate-700 dark:text-slate-300 font-bold"
  in
  let value_str =
    if value > 0.0 then Printf.sprintf "+%.1f" value else Printf.sprintf "%.1f" value
  in
  Printf.sprintf
    {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap">MG</span></div></td>|html}
    extra_classes class_name (escape_html value_str)

let wkbl_official_game_result_url (game_id : string) =
  match String.split_on_char '-' (String.trim game_id) with
  | [ season; game_type; game_no ] ->
      Some
        (Printf.sprintf
           "https://www.wkbl.or.kr/game/result.asp?season_gu=%s&game_type=%s&game_no=%s"
           (Uri.pct_encode season)
           (Uri.pct_encode game_type)
           (Uri.pct_encode game_no))
  | _ -> None

let score_quality_badge ?(compact=false) (q: game_score_quality) =
  let label, cls, title =
    match q with
    | Verified ->
        ( "VERIFIED"
        , "bg-slate-100 dark:bg-slate-800/60 text-slate-900 dark:text-slate-200 border-slate-300 dark:border-slate-700/60"
        , "WKBL 공식 스코어(result.asp)와 박스스코어 득점합(ajax_game_result_2.asp)이 일치합니다."
        )
    | Derived ->
        ( "DERIVED"
        , "bg-slate-100 dark:bg-slate-800/60 text-slate-700 dark:text-slate-300 border-slate-300 dark:border-slate-700/60"
        , "스코어가 누락되었거나 교차검증이 불가해 득점합으로 보정(derive)되었습니다."
        )
    | Mismatch ->
        ( "MISMATCH"
        , "bg-amber-500/10 text-amber-400 border-amber-500/30"
        , "WKBL 공식 스코어와 박스스코어 득점합이 불일치합니다. (/qa 참고)"
        )
  in
  let text =
    if compact then
      match q with
      | Verified -> "✓"
      | Derived -> "Σ"
      | Mismatch -> "!"
    else
      label
  in
  Printf.sprintf
    {html|<span class="px-2 py-1 rounded-full border text-[10px] font-mono tracking-wider %s" title="%s" aria-label="%s">%s</span>|html}
    cls
    (escape_html title)
    (escape_html label)
    (escape_html text)

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
    let margin_class = if s.ss_margin >= 0.0 then "text-sky-600 dark:text-sky-400" else "text-rose-600 dark:text-rose-400" in
    let margin_str = if s.ss_margin > 0.0 then Printf.sprintf "+%.1f" s.ss_margin else Printf.sprintf "%.1f" s.ss_margin in
    let row_class =
      if highlight then "bg-slate-100 dark:bg-slate-800/40 border-b border-slate-300 dark:border-slate-700/60"
      else "border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"
    in
    let name_class = if highlight then "text-slate-900 dark:text-slate-200 font-black" else "text-slate-900 dark:text-slate-200 font-medium" in
        Printf.sprintf {html|<tr class="%s">
          <td class="px-3 py-2 %s truncate whitespace-nowrap">%s</td>
          <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] font-mono whitespace-nowrap">%d</td>
          <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap hidden sm:table-cell">%.1f</td>
          <td class="px-3 py-2 text-right font-bold text-orange-600 dark:text-orange-400 w-[80px] font-mono whitespace-nowrap">%.1f</td>
          <td class="px-3 py-2 text-right font-bold %s w-[80px] font-mono whitespace-nowrap hidden sm:table-cell">%s</td>
          <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap">%.1f</td>
          <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap hidden sm:table-cell">%.1f</td>
          <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap">%.1f</td>
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
  Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 scroll-shadow shadow-lg animate-fade-in"><table class="season-stats-table min-w-[520px] sm:min-w-[720px] w-full text-sm font-mono table-fixed" aria-label="시즌별 선수 스탯">
    <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-xs whitespace-nowrap">
      <tr>
        <th scope="col" class="px-3 py-2 text-left font-sans">Season</th>
        <th scope="col" class="px-3 py-2 text-right w-[60px]" title="Games Played">GP</th>
        <th scope="col" class="px-3 py-2 text-right w-[80px] hidden sm:table-cell" title="Minutes">MIN</th>
        <th scope="col" class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 w-[80px]" title="Points">PTS</th>
        <th scope="col" class="px-3 py-2 text-right w-[80px] hidden sm:table-cell" title="Margin">MG</th>
        <th scope="col" class="px-3 py-2 text-right w-[80px]" title="Rebounds">REB</th>
        <th scope="col" class="px-3 py-2 text-right w-[80px] hidden sm:table-cell" title="Assists">AST</th>
        <th scope="col" class="px-3 py-2 text-right w-[80px]" title="Efficiency">EFF</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
  </table></div>|html} rows

(** Player Season Stats Component (Tabs + Table) *)
let player_season_stats_component ~player_id ~scope (stats: season_stats list) =
  let btn_class active =
    if active then "season-stats-tab px-3 sm:px-3 py-2 bg-transparent border-0 border-b-2 border-orange-500 text-slate-900 dark:text-slate-200 font-medium cursor-default pointer-events-none whitespace-nowrap"
    else "season-stats-tab px-3 sm:px-3 py-2 bg-transparent border-0 border-b-2 border-transparent text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition cursor-pointer font-medium whitespace-nowrap"
  in
  let s_per = btn_class (scope = "per_game") in
  let s_tot = btn_class (scope = "totals") in
  let s_36 = btn_class (scope = "per_36") in

  let aria_per = if scope = "per_game" then "true" else "false" in
  let aria_tot = if scope = "totals" then "true" else "false" in
  let aria_36 = if scope = "per_36" then "true" else "false" in
  let tabs = Printf.sprintf
    {html|<div id="season-stats-tabs" role="tablist" aria-label="시즌 스탯 보기 방식" class="flex items-center gap-2 sm:gap-4 border-b border-slate-200 dark:border-slate-800 mb-4 overflow-x-auto no-scrollbar whitespace-nowrap -mx-4 px-4 sm:mx-0 sm:px-0">
      <button type="button" role="tab" aria-selected="%s" class="%s" hx-get="/player/%s/season-stats?scope=per_game" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Per Game</button>
      <button type="button" role="tab" aria-selected="%s" class="%s" hx-get="/player/%s/season-stats?scope=totals" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Totals</button>
      <button type="button" role="tab" aria-selected="%s" class="%s" hx-get="/player/%s/season-stats?scope=per_36" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Per 36</button>
      <span id="season-stats-indicator" class="htmx-indicator season-stats-indicator ml-auto text-xs text-slate-500 dark:text-slate-400" aria-live="polite"><span class="spinner" aria-hidden="true"></span><span>Loading...</span></span>
    </div>|html}
    aria_per s_per player_id aria_tot s_tot player_id aria_36 s_36 player_id
  in
  let mg_note =
    {html|<p class="text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed mb-3"><span class="font-mono text-slate-700 dark:text-slate-300">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계(PBP) 기반으로 <span class="font-mono text-slate-700 dark:text-slate-300">일부 경기만</span> 제공됩니다. (데이터가 없으면 <span class="font-mono text-slate-700 dark:text-slate-300">-</span>)</p>|html}
  in
  let table = player_season_stats_table ~scope stats in
  Printf.sprintf {html|<div id="season-stats-component" class="stats-container animate-fade-in" data-skeleton="table" data-skeleton-count="8" data-skeleton-cols="10">%s%s%s</div>|html} tabs mg_note table

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
              {html|<div class="flex justify-between items-center border-b border-slate-200 dark:border-slate-800 pb-2 last:border-0 last:pb-0">
                <div class="flex flex-col">
                  <span class="text-slate-500 dark:text-slate-400 text-xs uppercase tracking-wider">%s</span>
                  <a href="%s" class="text-xs text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 transition-colors font-mono">%s • %s</a>
                </div>
                <span class="text-slate-900 dark:text-slate-200 font-mono font-bold">%d</span>
              </div>|html}
              (escape_html item.chi_label)
              game_url
              (escape_html item.chi_game_date)
              (escape_html matchup)
              item.chi_value)
        |> String.concat "\n"
      in
      Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg h-full"><h3 class="text-orange-600 dark:text-orange-400 font-bold uppercase tracking-wider text-xs mb-4 flex items-center gap-2"><span class="text-lg">🚀</span> Career Highs</h3><div class="space-y-3">%s</div></div>|html} items_html

(** Career Trajectory Chart - SVG-based line chart for season stats *)
let career_trajectory_chart (stats: season_stats list) =
  match stats with
  | [] | [_] -> ""  (* Need at least 2 seasons for trajectory *)
  | _ ->
      (* Sort by season code (chronological order) *)
      let sorted_stats = List.sort (fun a b -> String.compare a.ss_season_code b.ss_season_code) stats in
      let n = List.length sorted_stats in

      (* Chart dimensions - increased for better text rendering *)
      let chart_width = 400.0 in  (* viewBox units *)
      let chart_height = 220.0 in  (* Increased for rotated x-axis labels *)
      let padding_left = 32.0 in
      let padding_right = 16.0 in
      let padding_top = 24.0 in
      let padding_bottom = 56.0 in  (* Increased for rotated labels *)
      let plot_width = chart_width -. padding_left -. padding_right in
      let plot_height = chart_height -. padding_top -. padding_bottom in

      (* Extract data series *)
      let pts_data = List.map (fun s -> s.ss_avg_points) sorted_stats in
      let reb_data = List.map (fun s -> s.ss_avg_rebounds) sorted_stats in
      let ast_data = List.map (fun s -> s.ss_avg_assists) sorted_stats in
      let eff_data = List.map (fun s -> s.ss_efficiency) sorted_stats in

      (* Calculate bounds for scaling *)
      let all_values = pts_data @ eff_data in  (* Use PTS and EFF for main scale *)
      let max_val = List.fold_left max 0.0 all_values in
      let min_val = 0.0 in  (* Always start from 0 *)
      let range = max 1.0 (max_val -. min_val) in

      (* Secondary scale for REB/AST (smaller values) *)
      let secondary_values = reb_data @ ast_data in
      let max_secondary = List.fold_left max 0.0 secondary_values in
      let secondary_range = max 1.0 max_secondary in

      (* Scale functions *)
      let scale_x i = padding_left +. (float_of_int i /. float_of_int (n - 1)) *. plot_width in
      let scale_y v = padding_top +. plot_height -. ((v -. min_val) /. range *. plot_height) in
      let scale_y_secondary v = padding_top +. plot_height -. (v /. secondary_range *. plot_height *. 0.6) in

      (* Generate path data *)
      let make_path scale_fn data =
        data
        |> List.mapi (fun i v ->
            let x = scale_x i in
            let y = scale_fn v in
            if i = 0 then Printf.sprintf "M%.2f,%.2f" x y
            else Printf.sprintf "L%.2f,%.2f" x y)
        |> String.concat " "
      in

      let pts_path = make_path scale_y pts_data in
      let reb_path = make_path scale_y_secondary reb_data in
      let ast_path = make_path scale_y_secondary ast_data in
      let eff_path = make_path scale_y eff_data in

      (* Find peak season (highest efficiency) *)
      let peak_idx, peak_stats =
        sorted_stats
        |> List.mapi (fun i s -> (i, s))
        |> List.fold_left (fun (best_i, best_s) (i, s) ->
            if s.ss_efficiency > best_s.ss_efficiency then (i, s) else (best_i, best_s))
          (0, List.hd sorted_stats)
      in
      let peak_x = scale_x peak_idx in
      let peak_y = scale_y peak_stats.ss_efficiency in

      (* Season labels (x-axis) *)
      let season_labels =
        sorted_stats
        |> List.mapi (fun i s ->
            let x = scale_x i in
            let label =
              (* Extract short season name like "24-25" from full name *)
              let code = s.ss_season_code in
              if String.length code >= 4 then
                let year = String.sub code 0 4 in
                let y1 = int_of_string_opt year |> Option.value ~default:0 in
                Printf.sprintf "%02d-%02d" (y1 mod 100) ((y1 + 1) mod 100)
              else
                s.ss_season_name
            in
            Printf.sprintf
              {svg|<text x="%.2f" y="%.2f" class="fill-slate-600 dark:fill-slate-400" font-size="10" text-anchor="end" transform="rotate(-45 %.2f %.2f)">%s</text>|svg}
              x (chart_height -. 12.0) x (chart_height -. 12.0) (escape_html label))
        |> String.concat "\n"
      in

      (* Find career highs for main stats (PTS, EFF) *)
      let max_pts = List.fold_left max 0.0 pts_data in
      let max_eff = List.fold_left max 0.0 eff_data in

      (* Data points for PTS with interactive tooltip and career-high marker *)
      let pts_points =
        pts_data
        |> List.mapi (fun i v ->
            let x = scale_x i in
            let y = scale_y v in
            let stats = List.nth sorted_stats i in
            let is_career_high = v = max_pts && v > 0.0 in
            let delay = 0.6 +. (float_of_int i *. 0.08) in (* staggered delay after lines *)
            let marker = if is_career_high then
              Printf.sprintf {svg|<text x="%.2f" y="%.2f" class="fill-orange-600 dark:fill-orange-400 career-point" font-size="8" text-anchor="middle" style="animation-delay: %.2fs">★</text>|svg}
                x (y -. 10.0) delay
            else "" in
            Printf.sprintf
              {svg|%s<circle cx="%.2f" cy="%.2f" r="5" class="fill-orange-500 stroke-white dark:stroke-slate-900 cursor-pointer transition-transform hover:scale-150 career-point" stroke-width="2" style="animation-delay: %.2fs" data-season="%s" data-pts="%.1f" data-reb="%.1f" data-ast="%.1f" data-eff="%.1f"><title>%s
PTS: %.1f | REB: %.1f | AST: %.1f | EFF: %.1f</title></circle>|svg}
              marker x y delay
              (escape_html stats.ss_season_name) stats.ss_avg_points stats.ss_avg_rebounds stats.ss_avg_assists stats.ss_efficiency
              (escape_html stats.ss_season_name) stats.ss_avg_points stats.ss_avg_rebounds stats.ss_avg_assists stats.ss_efficiency)
        |> String.concat "\n"
      in

      (* Data points for EFF with career-high marker *)
      let eff_points =
        eff_data
        |> List.mapi (fun i v ->
            let x = scale_x i in
            let y = scale_y v in
            let stats = List.nth sorted_stats i in
            let is_career_high = v = max_eff && v > 0.0 in
            let delay = 0.5 +. (float_of_int i *. 0.08) in
            let marker = if is_career_high then
              Printf.sprintf {svg|<text x="%.2f" y="%.2f" class="fill-emerald-600 dark:fill-emerald-400 career-point" font-size="8" text-anchor="middle" style="animation-delay: %.2fs">★</text>|svg}
                x (y -. 10.0) delay
            else "" in
            Printf.sprintf
              {svg|%s<circle cx="%.2f" cy="%.2f" r="5" class="fill-emerald-500 stroke-white dark:stroke-slate-900 cursor-pointer transition-transform hover:scale-150 career-point" stroke-width="2" style="animation-delay: %.2fs"><title>%s
PTS: %.1f | REB: %.1f | AST: %.1f | EFF: %.1f</title></circle>|svg}
              marker x y delay
              (escape_html stats.ss_season_name) stats.ss_avg_points stats.ss_avg_rebounds stats.ss_avg_assists stats.ss_efficiency)
        |> String.concat "\n"
      in

      (* Grid lines *)
      let grid_lines =
        let lines = [0.25; 0.5; 0.75] in
        lines
        |> List.map (fun pct ->
            let y = padding_top +. plot_height *. (1.0 -. pct) in
            Printf.sprintf
              {svg|<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" class="stroke-slate-200 dark:stroke-slate-700/50" stroke-width="1" stroke-dasharray="4,4"/>|svg}
              padding_left y (chart_width -. padding_right) y)
        |> String.concat "\n"
      in

      (* Peak season highlight *)
      let peak_highlight =
        Printf.sprintf
          {svg|<g class="animate-pulse">
            <circle cx="%.2f" cy="%.2f" r="12" class="fill-yellow-400/30 dark:fill-yellow-500/20"/>
            <circle cx="%.2f" cy="%.2f" r="7" class="fill-yellow-500 stroke-white dark:stroke-slate-900" stroke-width="2">
              <title>Career Peak: %s - %.1f EFF</title>
            </circle>
          </g>|svg}
          peak_x peak_y peak_x peak_y (escape_html peak_stats.ss_season_name) peak_stats.ss_efficiency
      in

      (* Legend *)
      let legend =
        {html|<div class="flex flex-wrap items-center justify-center gap-x-4 gap-y-2 mt-3 text-[10px] sm:text-xs">
          <div class="flex items-center gap-1.5">
            <span class="w-3 h-0.5 bg-orange-500 rounded"></span>
            <span class="text-slate-600 dark:text-slate-400">PPG</span>
          </div>
          <div class="flex items-center gap-1.5">
            <span class="w-3 h-0.5 bg-emerald-500 rounded"></span>
            <span class="text-slate-600 dark:text-slate-400">EFF</span>
          </div>
          <div class="flex items-center gap-1.5">
            <span class="w-3 h-0.5 bg-sky-400 rounded"></span>
            <span class="text-slate-600 dark:text-slate-400">RPG</span>
          </div>
          <div class="flex items-center gap-1.5">
            <span class="w-3 h-0.5 bg-violet-400 rounded"></span>
            <span class="text-slate-600 dark:text-slate-400">APG</span>
          </div>
          <div class="flex items-center gap-1.5">
            <span class="w-2 h-2 bg-yellow-500 rounded-full"></span>
            <span class="text-slate-600 dark:text-slate-400">Peak</span>
          </div>
        </div>|html}
      in

      (* Stats summary *)
      let first_season = List.hd sorted_stats in
      let last_season = List.hd (List.rev sorted_stats) in
      let pts_trend = last_season.ss_avg_points -. first_season.ss_avg_points in
      let eff_trend = last_season.ss_efficiency -. first_season.ss_efficiency in
      let trend_class sign = if sign > 0.0 then "text-emerald-600 dark:text-emerald-400" else if sign < 0.0 then "text-rose-600 dark:text-rose-400" else "text-slate-500" in
      let trend_arrow sign = if sign > 0.0 then "+" else "" in

      let summary =
        Printf.sprintf
          {html|<div class="grid grid-cols-2 sm:grid-cols-4 gap-2 mt-4 text-xs">
            <div class="bg-slate-100 dark:bg-slate-800/40 rounded-lg p-2 text-center">
              <div class="text-slate-500 dark:text-slate-400 text-[10px] uppercase tracking-wider">Seasons</div>
              <div class="text-slate-900 dark:text-slate-200 font-mono font-bold">%d</div>
            </div>
            <div class="bg-slate-100 dark:bg-slate-800/40 rounded-lg p-2 text-center">
              <div class="text-slate-500 dark:text-slate-400 text-[10px] uppercase tracking-wider">Peak EFF</div>
              <div class="text-yellow-600 dark:text-yellow-400 font-mono font-bold">%.1f</div>
            </div>
            <div class="bg-slate-100 dark:bg-slate-800/40 rounded-lg p-2 text-center">
              <div class="text-slate-500 dark:text-slate-400 text-[10px] uppercase tracking-wider">PPG Trend</div>
              <div class="%s font-mono font-bold">%s%.1f</div>
            </div>
            <div class="bg-slate-100 dark:bg-slate-800/40 rounded-lg p-2 text-center">
              <div class="text-slate-500 dark:text-slate-400 text-[10px] uppercase tracking-wider">EFF Trend</div>
              <div class="%s font-mono font-bold">%s%.1f</div>
            </div>
          </div>|html}
          n
          peak_stats.ss_efficiency
          (trend_class pts_trend) (trend_arrow pts_trend) pts_trend
          (trend_class eff_trend) (trend_arrow eff_trend) eff_trend
      in

      Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg">
          <h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs mb-4 flex items-center gap-2">
            <span class="text-lg">📈</span> Career Trajectory
          </h3>
          <div class="relative">
            <svg viewBox="0 0 %.0f %.0f" class="w-full h-auto" preserveAspectRatio="xMidYMid meet" role="img" aria-label="Career statistics trend chart">
              <style>
                @keyframes line-draw {
                  from { stroke-dashoffset: 2000; }
                  to { stroke-dashoffset: 0; }
                }
                @keyframes fade-in {
                  from { opacity: 0; }
                  to { opacity: 1; }
                }
                .career-line { stroke-dasharray: 2000; animation: line-draw 1.2s ease-out forwards; }
                .career-line-1 { animation-delay: 0s; }
                .career-line-2 { animation-delay: 0.15s; }
                .career-line-3 { animation-delay: 0.3s; }
                .career-line-4 { animation-delay: 0.45s; }
                .career-point { opacity: 0; animation: fade-in 0.3s ease-out forwards; }
              </style>
              <!-- Grid -->
              %s

              <!-- Lines (back to front: AST, REB, EFF, PTS) -->
              <path d="%s" fill="none" class="stroke-violet-400/60 career-line career-line-1" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="%s" fill="none" class="stroke-sky-400/60 career-line career-line-2" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="%s" fill="none" class="stroke-emerald-500 career-line career-line-3" stroke-width="3" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="%s" fill="none" class="stroke-orange-500 career-line career-line-4" stroke-width="3" stroke-linecap="round" stroke-linejoin="round"/>

              <!-- Data points -->
              %s
              %s

              <!-- Peak highlight -->
              %s

              <!-- Season labels -->
              %s
            </svg>
          </div>
          %s
          %s
        </div>|html}
        chart_width chart_height
        grid_lines
        ast_path reb_path eff_path pts_path
        eff_points pts_points
        peak_highlight
        season_labels
        legend
        summary

(** Player row component *)
let player_row ?(show_player_id=false) ?(team_cell_class="px-3 py-2 w-[120px] sm:w-[160px]") ?(include_team=true) (rank: int) (p: player_aggregate) =
  let id_badge =
    if show_player_id then
      player_id_badge p.player_id
    else
      ""
  in
  let display_name = normalize_name p.name in
  (* PER calculation inline - (EFF / MIN) * 48 * pace_factor, normalized *)
  let per =
    if p.total_minutes <= 0.0 then 0.0
    else
      let per_min = p.efficiency /. p.total_minutes in
      let per_48 = per_min *. 48.0 in
      let pace_factor = 40.0 /. 48.0 in
      let normalized = per_48 *. pace_factor *. 1.2 in
      max 0.0 (min 40.0 normalized)
  in
  let team_cell =
    if include_team then
      Printf.sprintf {html|<td class="%s">%s</td>|html} (escape_html team_cell_class) (team_badge p.team_name)
    else
      ""
  in
  Printf.sprintf
    {html|<tr class="group border-b border-slate-200 dark:border-slate-700/50 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-all duration-200 hover:scale-[1.01] hover:shadow-md relative z-0 hover:z-10">
      <td class="px-3 py-2 w-12 text-slate-500 dark:text-slate-500 text-sm whitespace-nowrap">%d</td>
      <td class="px-3 py-2 font-medium text-slate-900 dark:text-white w-[220px] sm:w-[260px]">
        <div class="flex items-center gap-3 min-w-0">
          %s
          <div class="flex items-center gap-2 min-w-0">
            <a href="/player/%s" class="player-name hover:text-orange-600 dark:hover:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
            <span class="%s">%s</span>
          </div>
        </div>
      </td>
      %s
      <td class="px-3 py-2 text-right whitespace-nowrap hidden sm:table-cell"><div class="flex flex-col items-end leading-tight"><span class="text-slate-500 dark:text-slate-400 font-mono">%d</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap">GP</span></div></td>
      %s%s%s%s%s%s%s%s%s
    </tr>|html}
    rank
    (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
    p.player_id
    (escape_html display_name)
    (if show_player_id then "opacity-100 sm:opacity-0 sm:group-hover:opacity-100 transition-opacity" else "hidden")
    id_badge
    team_cell
    p.games_played
    (points_total_cell p.avg_points p.total_points)
    (margin_cell ~extra_classes:"hidden md:table-cell" p.avg_margin)
    (stat_total_cell p.avg_rebounds p.total_rebounds)
    (stat_total_cell ~extra_classes:"hidden md:table-cell" p.avg_assists p.total_assists)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell" p.avg_steals p.total_steals)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell" p.avg_blocks p.total_blocks)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell" p.avg_turnovers p.total_turnovers)
    (stat_cell ~highlight:true ~label:"EFF" p.efficiency)
    (stat_cell ~extra_classes:"hidden sm:table-cell" ~label:"PER" per)

(** Players table - HTMX partial *)
let players_table (players: player_aggregate list) =
  let name_counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
  players
  |> List.iter (fun (p: player_aggregate) ->
      let key = normalize_name p.name in
      let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
      Hashtbl.replace name_counts key (prev + 1));
  let rows =
    players
    |> List.mapi (fun i (p: player_aggregate) ->
        let key = normalize_name p.name in
        let show_player_id =
          match Hashtbl.find_opt name_counts key with
          | Some c when c > 1 -> true
          | _ -> false
        in
        player_row ~show_player_id (i + 1) p)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="overflow-x-auto max-h-[75vh] overflow-y-auto">
    <table class="min-w-[680px] sm:min-w-[860px] lg:min-w-[980px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="선수 스탯 순위">
      <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap">
        <tr>
          <th scope="col" class="px-3 py-2 text-left w-12" title="Rank">#</th>
          <th scope="col" class="px-3 py-2 text-left w-[220px] sm:w-[260px]">Player</th>
          <th scope="col" class="px-3 py-2 text-left w-[120px] sm:w-[160px]">Team</th>
          <th scope="col" class="px-3 py-2 text-right w-[60px] hidden sm:table-cell" title="Games Played">GP</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300" title="Points" hx-get="/players/table?sort=pts" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">PTS</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 hidden md:table-cell" title="MG: 팀 득실마진(출전시간 가중 평균)" hx-get="/players/table?sort=mg" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">MG</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300" title="Rebounds" hx-get="/players/table?sort=reb" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">REB</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 hidden md:table-cell" title="Assists" hx-get="/players/table?sort=ast" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">AST</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] hidden lg:table-cell" title="Steals">STL</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] hidden lg:table-cell" title="Blocks">BLK</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] hidden lg:table-cell" title="Turnovers">TO</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300" title="Efficiency" hx-get="/players/table?sort=eff" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">EFF</th>
          <th scope="col" class="px-3 py-2 text-right w-[72px] hidden sm:table-cell" title="Player Efficiency Rating (분당 효율 정규화)">PER</th>
        </tr>
      </thead>
      <tbody id="players-body">%s</tbody>
    </table></div>|html}
    rows

(** Main layout *)
let layout ~title ?(canonical_path="/") ?(description="") ?(json_ld="")
    ?og_title ?og_description ?og_image ?data_freshness ~content () =
  let v = escape_html asset_version in
  let cf_wa_script =
    match Sys.getenv_opt "CF_WEB_ANALYTICS_TOKEN" |> Option.map String.trim with
    | Some token when token <> "" ->
        Printf.sprintf
          {html|  <script defer src="https://static.cloudflareinsights.com/beacon.min.js" data-cfasync="false" data-cf-beacon='{"token":"%s"}'></script>|html}
          (escape_html token)
    | _ -> ""
  in
  (* SEO: 기본 설명 또는 페이지별 맞춤 설명 사용 *)
  let default_desc = "WKBL 여자농구 통계 분석 - 선수별 효율, 팀 순위, 박스스코어, 드래프트/이적 정보를 basketball-reference 스타일로 제공합니다." in
  let meta_desc = if description = "" then default_desc else description in
  let short_desc = if description = "" then "WKBL 여자농구 통계 분석 - 선수별 효율, 팀 순위, 박스스코어 정보" else description in
  (* SEO: Canonical URL 동적 생성 *)
  let canonical_url = Printf.sprintf "https://wkbl.win%s" canonical_path in
  (* OG 태그: 페이지별 커스터마이징 또는 기본값 사용 *)
  let og_title_val = Option.value og_title ~default:title in
  let og_desc_val = Option.value og_description ~default:short_desc in
  let og_image_val = Option.value og_image ~default:"https://wkbl.win/static/images/og-image.jpeg" in
  (* SEO: JSON-LD 구조화 데이터 - 기본 WebSite 스키마 또는 페이지별 커스텀 *)
  let default_json_ld = Printf.sprintf {|{
  "@context": "https://schema.org",
  "@type": "WebSite",
  "name": "WKBL Analytics",
  "url": "https://wkbl.win",
  "description": "%s",
  "inLanguage": "ko-KR",
  "publisher": {
    "@type": "Organization",
    "name": "WKBL Analytics",
    "url": "https://wkbl.win"
  }
}|} (String.escaped meta_desc) in
  let json_ld_script =
    let ld = if json_ld = "" then default_json_ld else json_ld in
    Printf.sprintf {html|  <script type="application/ld+json">%s</script>|html} ld
  in
  let data_freshness_html =
    let fresh_date = match data_freshness with
      | Some d -> Some d
      | None ->
          (* Fetch from DB if not provided *)
          match Db.get_latest_game_date () with
          | Ok (Some d) -> Some d
          | _ -> None
    in
    (* Get schedule progress from DB *)
    let schedule_progress =
      match Db.get_schedule_progress ~season_code:"046" () with
      | Ok (Some (completed, total)) -> Some (completed, total)
      | _ -> None
    in
    let date_html = match fresh_date with
      | Some date -> Printf.sprintf {html|📊 데이터: %s 경기까지 반영|html} (escape_html date)
      | None -> ""
    in
    let progress_html = match schedule_progress with
      | Some (completed, total) ->
          let pct = if total > 0 then completed * 100 / total else 0 in
          Printf.sprintf {html| · 🏀 시즌: %d/%d경기 (%d%%)|html} completed total pct
      | None -> ""
    in
    if date_html = "" && progress_html = "" then ""
    else Printf.sprintf {html|<div class="mb-2 text-xs text-slate-500 dark:text-slate-500">%s%s</div>|html} date_html progress_html
  in
  Printf.sprintf
    {html|<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="author" content="WKBL Analytics">
  <meta name="robots" content="index, follow">
  <title>%s</title>
  <meta name="description" content="%s">
  <meta name="keywords" content="WKBL, 여자농구, 여자프로농구, 한국여자농구, 통계, 분석, 선수통계, MVP, 팀, 박스스코어, 농구분석">
  <meta property="og:title" content="%s">
  <meta property="og:description" content="%s">
  <meta property="og:type" content="website">
  <meta property="og:site_name" content="WKBL Analytics">
  <meta property="og:locale" content="ko_KR">
  <meta property="og:image" content="%s">
  <meta property="og:url" content="%s">
  <meta name="twitter:card" content="summary_large_image">
  <meta name="twitter:title" content="%s">
  <meta name="twitter:description" content="%s">
  <meta name="twitter:image" content="%s">
  <link rel="canonical" href="%s">
  <!-- Preconnect hints for performance -->
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link rel="preconnect" href="https://cdn.tailwindcss.com">
  <link rel="preconnect" href="https://www.wkbl.or.kr">
  <link rel="dns-prefetch" href="https://www.wkbl.or.kr">
  <link rel="icon" type="image/png" sizes="32x32" href="/static/images/favicon-32.png">
  <link rel="icon" type="image/png" sizes="16x16" href="/static/images/favicon-16.png">
  <link rel="apple-touch-icon" sizes="180x180" href="/static/images/apple-touch-icon.png">
  <!-- PWA Support -->
  <link rel="manifest" href="/manifest.json">
  <meta name="theme-color" content="#f97316" media="(prefers-color-scheme: light)">
  <meta name="theme-color" content="#0b0e14" media="(prefers-color-scheme: dark)">
  <meta name="mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-capable" content="yes">
  <meta name="apple-mobile-web-app-status-bar-style" content="black-translucent">
  <meta name="apple-mobile-web-app-title" content="WKBL">
  <script src="/static/js/theme-toggle.js?v=%s" data-cfasync="false"></script>
  <script src="/static/js/htmx-1.9.10.min.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/page-transitions.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/player-photo-fallback.js?v=%s" data-cfasync="false"></script>
  <script src="/static/js/mobile-nav.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/search-modal.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/skeleton-loader.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/share-utils.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/table-sort.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/a11y-utils.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/notifications.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/scroll-shadow.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/back-to-top.js?v=%s" defer data-cfasync="false"></script>
  <script src="https://cdn.tailwindcss.com" data-cfasync="false"></script>
  <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&family=Inter:wght@400;600;800&display=swap" rel="stylesheet">
  <link rel="stylesheet" href="/static/css/styles.css?v=%s">
  <script data-cfasync="false">tailwind.config = { darkMode: 'class', theme: { extend: { fontFamily: { sans: ['Inter', 'sans-serif'], mono: ['JetBrains Mono', 'monospace'] } } } }</script>
%s
%s
</head>
<body class="bg-slate-50 dark:bg-[#0b0e14] text-slate-900 dark:text-slate-200 font-sans antialiased min-h-screen is-loading">
  <a href="#main-content" class="sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4 focus:z-[60] focus:bg-orange-500 focus:text-white focus:px-4 focus:py-2 focus:rounded-lg focus:font-semibold focus:shadow-lg">본문으로 건너뛰기</a>
  <div id="page-loader" class="page-loader" aria-hidden="true"><div class="page-loader-bar"></div></div>
  <header role="banner" class="sticky top-0 z-[110] bg-white/95 dark:bg-slate-900/95 backdrop-blur border-b border-slate-200 dark:border-slate-800 px-4 sm:px-6 py-2 sm:py-4">
    <div class="max-w-7xl mx-auto flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3">
      <div class="flex items-center justify-between w-full sm:w-auto">
        <a href="/" class="flex items-center gap-3 shrink-0" aria-label="WKBL Analytics 홈으로 이동">
          <span class="text-2xl" aria-hidden="true">🏀</span>
          <span class="text-lg font-bold text-slate-900 dark:text-slate-200">WKBL <span class="text-orange-500">Analytics</span></span>
        </a>
        <div class="flex items-center gap-2 md:hidden">
          <button data-notify-toggle onclick="WKBLNotifications.toggle()" class="p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="알림 설정" aria-pressed="false">
            <span class="notify-icon text-base">🔕</span>
          </button>
          <button onclick="toggleTheme()" class="p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="테마 전환 (라이트/다크)">
            <svg class="w-5 h-5 dark:hidden" fill="currentColor" viewBox="0 0 20 20" aria-hidden="true"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"></path></svg>
            <svg class="w-5 h-5 hidden dark:block" fill="currentColor" viewBox="0 0 20 20" aria-hidden="true"><path d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" fill-rule="evenodd" clip-rule="evenodd"></path></svg>
          </button>
          <button id="mobile-menu-btn" class="p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="메뉴 열기" aria-expanded="false" aria-controls="mobile-menu-panel">
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"></path></svg>
          </button>
        </div>
      </div>
      <div class="hidden md:flex items-center gap-3">
        <button onclick="SearchModal.open()" class="flex items-center gap-2 px-3 py-1.5 text-sm text-slate-600 dark:text-slate-400 bg-slate-100 dark:bg-slate-800 rounded-lg hover:text-slate-900 dark:hover:text-slate-200 transition whitespace-nowrap shrink-0" aria-label="검색 열기">
          <svg class="w-4 h-4 shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path></svg>
          <span class="shrink-0">검색</span>
          <kbd class="px-1.5 py-0.5 text-xs font-mono bg-white dark:bg-slate-700 rounded shadow-sm shrink-0">⌘K</kbd>
        </button>
        <nav aria-label="메인 내비게이션" class="flex items-center gap-x-1 gap-y-2 text-sm flex-wrap justify-end">
          <!-- 핵심 메뉴 -->
          <a href="/" class="px-3 py-1.5 rounded-lg text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800 transition">Home</a>
          <a href="/games" class="px-3 py-1.5 rounded-lg text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800 transition">Games</a>
          <a href="/teams" class="px-3 py-1.5 rounded-lg text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800 transition">Teams</a>
          <a href="/players" class="px-3 py-1.5 rounded-lg text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800 transition">Players</a>
          <a href="/live" class="px-3 py-1.5 rounded-lg text-orange-500 font-medium hover:bg-orange-50 dark:hover:bg-orange-900/20 transition">🔴 Live</a>

          <!-- 분석 드롭다운 -->
          <div class="relative group">
            <button class="px-3 py-1.5 rounded-lg text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800 transition flex items-center gap-1">
              분석
              <svg class="w-3 h-3 transition-transform group-hover:rotate-180" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>
            </button>
            <div class="absolute right-0 top-full pt-1 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all duration-200 z-50">
              <div class="bg-white dark:bg-slate-800 rounded-xl shadow-xl border border-slate-200 dark:border-slate-700 py-2 min-w-[160px]">
                <a href="/leaders" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">📊 Leaders</a>
                <a href="/compare" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">⚖️ Compare</a>
                <a href="/on-off" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">🔄 On/Off</a>
                <a href="/predict" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">🔮 Predict</a>
              </div>
            </div>
          </div>

          <!-- 기록 드롭다운 -->
          <div class="relative group">
            <button class="px-3 py-1.5 rounded-lg text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800 transition flex items-center gap-1">
              기록
              <svg class="w-3 h-3 transition-transform group-hover:rotate-180" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>
            </button>
            <div class="absolute right-0 top-full pt-1 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all duration-200 z-50">
              <div class="bg-white dark:bg-slate-800 rounded-xl shadow-xl border border-slate-200 dark:border-slate-700 py-2 min-w-[160px]">
                <a href="/standings" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">🏆 Standings</a>
                <a href="/boxscores" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">📋 Boxscores</a>
                <a href="/awards" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">🏅 Awards</a>
                <a href="/history" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">📜 History</a>
                <a href="/legends" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">⭐ Legends</a>
                <a href="/transactions" class="block px-4 py-2 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-700 transition">🔄 Draft/Trade</a>
              </div>
            </div>
          </div>
        </nav>
        <button data-notify-toggle onclick="WKBLNotifications.toggle()" class="hidden sm:block p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="알림 설정" aria-pressed="false">
          <span class="notify-icon text-base">🔕</span>
        </button>
        <button onclick="toggleTheme()" class="hidden sm:block p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="테마 전환 (라이트/다크)">
          <svg class="w-5 h-5 dark:hidden" fill="currentColor" viewBox="0 0 20 20" aria-hidden="true"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"></path></svg>
          <svg class="w-5 h-5 hidden dark:block" fill="currentColor" viewBox="0 0 20 20" aria-hidden="true"><path d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" fill-rule="evenodd" clip-rule="evenodd"></path></svg>
        </button>
      </div>
    </div>
  </header>
  <main id="main-content" class="max-w-7xl mx-auto px-4 sm:px-6 py-6 sm:py-8 pb-24 md:pb-8" tabindex="-1">%s</main>
  <footer class="border-t border-slate-200 dark:border-slate-800 py-6 text-center text-slate-600 dark:text-slate-400 text-sm mb-20 md:mb-0">
    %s
    <a href="mailto:contact@wkbl.win" class="hover:text-slate-700 dark:hover:text-slate-300 transition-colors">📧 contact@wkbl.win</a>
  </footer>

  <!-- Mobile Slide Menu Overlay -->
  <div id="mobile-menu-overlay" class="fixed inset-0 z-[100] bg-black/50 backdrop-blur-sm hidden opacity-0 transition-opacity duration-300" aria-hidden="true">
    <div id="mobile-menu-panel" class="absolute right-0 top-0 h-full w-72 max-w-[calc(100vw-3rem)] bg-white dark:bg-slate-900 shadow-2xl transform translate-x-full transition-transform duration-300 overflow-y-auto" role="dialog" aria-modal="true" aria-label="메뉴">
      <div class="flex items-center justify-between p-4 border-b border-slate-200 dark:border-slate-700">
        <span class="font-bold text-slate-900 dark:text-white">메뉴</span>
        <button id="mobile-menu-close" class="p-2 rounded-lg text-slate-500 hover:text-slate-900 dark:hover:text-white transition" aria-label="메뉴 닫기">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg>
        </button>
      </div>
      <nav class="p-4 space-y-4" aria-label="모바일 메뉴">
        <!-- 핵심 -->
        <div class="space-y-1">
          <a href="/" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">🏠</span> Home
          </a>
          <a href="/games" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">📅</span> Games
          </a>
          <a href="/teams" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">👥</span> Teams
          </a>
          <a href="/players" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">🏀</span> Players
          </a>
          <a href="/live" class="flex items-center gap-3 px-3 py-2.5 rounded-lg bg-orange-50 dark:bg-orange-900/20 text-orange-600 dark:text-orange-400 font-medium transition">
            <span class="text-lg">🔴</span> Live
          </a>
        </div>

        <!-- 분석 -->
        <div class="border-t border-slate-200 dark:border-slate-700 pt-4">
          <div class="text-xs font-semibold text-slate-400 dark:text-slate-500 uppercase tracking-wider px-3 mb-2">분석</div>
          <a href="/leaders" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">📊</span> Leaders
          </a>
          <a href="/compare" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">⚖️</span> Compare
          </a>
          <a href="/on-off" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">🔄</span> On/Off
          </a>
          <a href="/predict" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">🔮</span> Predict
          </a>
        </div>

        <!-- 기록 -->
        <div class="border-t border-slate-200 dark:border-slate-700 pt-4">
          <div class="text-xs font-semibold text-slate-400 dark:text-slate-500 uppercase tracking-wider px-3 mb-2">기록</div>
          <a href="/standings" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">🏆</span> Standings
          </a>
          <a href="/boxscores" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">📋</span> Boxscores
          </a>
          <a href="/awards" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">🏅</span> Awards
          </a>
          <a href="/history" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">📜</span> History
          </a>
          <a href="/legends" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">⭐</span> Legends
          </a>
          <a href="/transactions" class="flex items-center gap-3 px-3 py-2.5 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
            <span class="text-lg">📝</span> Draft/Trade
          </a>
        </div>
      </nav>
    </div>
  </div>

  <!-- Mobile Bottom Navigation -->
  <nav data-bottom-nav class="fixed bottom-0 left-0 right-0 z-50 bg-white/95 dark:bg-slate-900/95 backdrop-blur border-t border-slate-200 dark:border-slate-700 md:hidden safe-area-bottom" aria-label="모바일 하단 내비게이션">
    <div class="flex items-center justify-around h-16 max-w-md sm:max-w-lg mx-auto px-2 gap-1">
      <a href="/" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-600 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="홈">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"></path></svg>
        <span class="text-[10px] font-medium">홈</span>
      </a>
      <a href="/leaders" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-600 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="리더">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>
        <span class="text-[10px] font-medium">리더</span>
      </a>
      <a href="/games" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-600 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="경기">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>
        <span class="text-[10px] font-medium">경기</span>
      </a>
      <a href="/players" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-600 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="선수">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"></path></svg>
        <span class="text-[10px] font-medium">선수</span>
      </a>
      <button onclick="MobileNav.open()" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-600 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="더보기 메뉴">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"></path></svg>
        <span class="text-[10px] font-medium">더보기</span>
      </button>
    </div>
  </nav>
  <!-- PWA Service Worker Registration -->
  <script>
    if ('serviceWorker' in navigator) {
      window.addEventListener('load', () => {
        navigator.serviceWorker.register('/sw.js')
          .then(reg => console.log('[PWA] SW registered:', reg.scope))
          .catch(err => console.warn('[PWA] SW registration failed:', err));
      });
    }
  </script>
</body>
</html>|html}
    (escape_html title) (escape_html meta_desc) (escape_html og_title_val) (escape_html og_desc_val) (escape_html og_image_val) (escape_html canonical_url) (escape_html og_title_val) (escape_html og_desc_val) (escape_html og_image_val) (escape_html canonical_url) v v v v v v v v v v v v v v json_ld_script cf_wa_script content data_freshness_html

(** Home page *)
