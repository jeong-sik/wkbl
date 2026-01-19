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
    {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/50 border border-slate-300 dark:border-slate-700/50 text-[10px] font-mono text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition-colors whitespace-nowrap cursor-help" title="player_id: %s" aria-label="player id %s">ID %s</span>|html}
    (escape_html player_id)
    (escape_html player_id)
    (escape_html short)

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
    {html|<img src="%s" alt="" aria-hidden="true" class="%s rounded-full object-cover bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 shadow-sm" loading="lazy" data-placeholder="%s" onerror="if(!this.dataset.placeholderApplied){this.dataset.placeholderApplied='1';this.src=this.dataset.placeholder;}">|html}
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
  | Some f -> Printf.sprintf {html|<img src="/static/images/%s" alt="" aria-hidden="true" class="%s object-contain">|html} f class_name
  | None ->
      Printf.sprintf
        {html|<div class="%s bg-slate-100 dark:bg-slate-800 rounded flex items-center justify-center text-xs">🏀</div>|html}
        class_name

(** Team badge component *)
let team_badge ?(max_width="max-w-[110px] sm:max-w-[180px]") team_name =
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


(** Stat cell with formatting *)
let stat_cell ?(highlight=false) ?(extra_classes="") value =
  let class_name = if highlight then "text-orange-600 dark:text-orange-400 font-bold" else "text-slate-700 dark:text-slate-300" in
  Printf.sprintf {html|<td class="px-3 py-2 text-right %s font-mono %s">%.1f</td>|html} class_name extra_classes value

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

let stat_total_cell ?(highlight=false) ?(extra_classes="") (avg_value: float) (total_value: int) =
  let class_name = if highlight then "text-orange-600 dark:text-orange-400 font-bold" else "text-slate-700 dark:text-slate-300" in
  let total_str = format_int_commas total_value in
  Printf.sprintf
    {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap">TOT %s</span></div></td>|html}
    extra_classes
    class_name
    avg_value
    total_str

(** Points cell with career total *)
let points_total_cell ?(extra_classes="") (avg_points: float) (total_points: int) =
  let total_str = format_int_commas total_points in
  Printf.sprintf
    {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="text-orange-600 dark:text-orange-400 font-bold font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap">TOT %s</span></div></td>|html}
    extra_classes
    avg_points
    total_str

(** Margin cell (signed, colored) *)
let margin_cell ?(extra_classes="") value =
  let class_name =
    if value > 0.0 then "text-sky-600 dark:text-sky-400 font-bold"
    else if value < 0.0 then "text-rose-400 font-bold"
    else "text-slate-700 dark:text-slate-300 font-bold"
  in
  let value_str =
    if value > 0.0 then Printf.sprintf "+%.1f" value else Printf.sprintf "%.1f" value
  in
  Printf.sprintf {html|<td class="px-3 py-2 text-right %s font-mono %s">%s</td>|html} class_name extra_classes (escape_html value_str)

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
    let margin_class = if s.ss_margin >= 0.0 then "text-sky-600 dark:text-sky-400" else "text-rose-400" in
    let margin_str = if s.ss_margin > 0.0 then Printf.sprintf "+%.1f" s.ss_margin else Printf.sprintf "%.1f" s.ss_margin in
    let row_class =
      if highlight then "bg-slate-100 dark:bg-slate-800/40 border-b border-slate-300 dark:border-slate-700/60"
      else "border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"
    in
    let name_class = if highlight then "text-slate-900 dark:text-slate-200 font-black" else "text-slate-900 dark:text-slate-200 font-medium" in
        Printf.sprintf {html|<tr class="%s">
          <td class="px-4 py-3 %s truncate whitespace-nowrap">%s</td>
          <td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] font-mono whitespace-nowrap">%d</td>
          <td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap hidden sm:table-cell">%.1f</td>
          <td class="px-4 py-3 text-right font-bold text-orange-600 dark:text-orange-400 w-[80px] font-mono whitespace-nowrap">%.1f</td>
          <td class="px-4 py-3 text-right font-bold %s w-[80px] font-mono whitespace-nowrap hidden sm:table-cell">%s</td>
          <td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap">%.1f</td>
          <td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap hidden sm:table-cell">%.1f</td>
          <td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[80px] font-mono whitespace-nowrap">%.1f</td>
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
  Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg animate-fade-in"><table class="season-stats-table min-w-[520px] sm:min-w-[720px] w-full text-sm font-mono table-auto">
    <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-xs whitespace-nowrap">
      <tr>
        <th class="px-4 py-3 text-left font-sans">Season</th>
        <th class="px-4 py-3 text-right w-[60px]">GP</th>
        <th class="px-4 py-3 text-right w-[80px] hidden sm:table-cell">MIN</th>
        <th class="px-4 py-3 text-right text-orange-600 dark:text-orange-400 w-[80px]">PTS</th>
        <th class="px-4 py-3 text-right w-[80px] hidden sm:table-cell">MG</th>
        <th class="px-4 py-3 text-right w-[80px]">REB</th>
        <th class="px-4 py-3 text-right w-[80px] hidden sm:table-cell">AST</th>
        <th class="px-4 py-3 text-right w-[80px]">EFF</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
  </table></div>|html} rows

(** Player Season Stats Component (Tabs + Table) *)
let player_season_stats_component ~player_id ~scope (stats: season_stats list) =
  let btn_class active =
    if active then "season-stats-tab px-3 sm:px-4 py-2 bg-transparent border-0 border-b-2 border-orange-500 text-slate-900 dark:text-slate-200 font-medium cursor-default pointer-events-none whitespace-nowrap"
    else "season-stats-tab px-3 sm:px-4 py-2 bg-transparent border-0 border-b-2 border-transparent text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition cursor-pointer font-medium whitespace-nowrap"
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

      (* Chart dimensions *)
      let chart_width = 100.0 in  (* viewBox units *)
      let chart_height = 60.0 in
      let padding_left = 8.0 in
      let padding_right = 4.0 in
      let padding_top = 8.0 in
      let padding_bottom = 12.0 in
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
              {svg|<text x="%.2f" y="%.2f" class="fill-slate-500 dark:fill-slate-400 text-[3px] sm:text-[4px]" text-anchor="middle">%s</text>|svg}
              x (chart_height -. 2.0) (escape_html label))
        |> String.concat "\n"
      in

      (* Data points for PTS (with values on hover) *)
      let pts_points =
        pts_data
        |> List.mapi (fun i v ->
            let x = scale_x i in
            let y = scale_y v in
            let season = (List.nth sorted_stats i).ss_season_name in
            Printf.sprintf
              {svg|<circle cx="%.2f" cy="%.2f" r="1.5" class="fill-orange-500 stroke-white dark:stroke-slate-900" stroke-width="0.5"><title>%s: %.1f PPG</title></circle>|svg}
              x y (escape_html season) v)
        |> String.concat "\n"
      in

      (* Data points for EFF *)
      let eff_points =
        eff_data
        |> List.mapi (fun i v ->
            let x = scale_x i in
            let y = scale_y v in
            let season = (List.nth sorted_stats i).ss_season_name in
            Printf.sprintf
              {svg|<circle cx="%.2f" cy="%.2f" r="1.5" class="fill-emerald-500 stroke-white dark:stroke-slate-900" stroke-width="0.5"><title>%s: %.1f EFF</title></circle>|svg}
              x y (escape_html season) v)
        |> String.concat "\n"
      in

      (* Grid lines *)
      let grid_lines =
        let lines = [0.25; 0.5; 0.75] in
        lines
        |> List.map (fun pct ->
            let y = padding_top +. plot_height *. (1.0 -. pct) in
            Printf.sprintf
              {svg|<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" class="stroke-slate-200 dark:stroke-slate-700/50" stroke-width="0.2" stroke-dasharray="1,1"/>|svg}
              padding_left y (chart_width -. padding_right) y)
        |> String.concat "\n"
      in

      (* Peak season highlight *)
      let peak_highlight =
        Printf.sprintf
          {svg|<g class="animate-pulse">
            <circle cx="%.2f" cy="%.2f" r="3" class="fill-yellow-400/30 dark:fill-yellow-500/20"/>
            <circle cx="%.2f" cy="%.2f" r="1.8" class="fill-yellow-500 stroke-white dark:stroke-slate-900" stroke-width="0.5">
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
              <!-- Grid -->
              %s

              <!-- Lines (back to front: AST, REB, EFF, PTS) -->
              <path d="%s" fill="none" class="stroke-violet-400/60" stroke-width="1" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="%s" fill="none" class="stroke-sky-400/60" stroke-width="1" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="%s" fill="none" class="stroke-emerald-500" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="%s" fill="none" class="stroke-orange-500" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>

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
  let team_cell =
    if include_team then
      Printf.sprintf {html|<td class="%s">%s</td>|html} (escape_html team_cell_class) (team_badge p.team_name)
    else
      ""
  in
  Printf.sprintf
    {html|<tr class="group border-b border-slate-200 dark:border-slate-700/50 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-all duration-200 hover:scale-[1.01] hover:shadow-md relative z-0 hover:z-10">
      <td class="px-3 py-2 text-slate-500 dark:text-slate-500 text-sm whitespace-nowrap">%d</td>
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
      <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 whitespace-nowrap hidden sm:table-cell">%d</td>
      %s%s%s%s%s%s%s%s
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
    (stat_cell ~highlight:true p.efficiency)

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
    {html|<div class="overflow-x-auto overflow-y-hidden">
    <table class="min-w-[680px] sm:min-w-[860px] lg:min-w-[980px] w-full text-xs sm:text-sm font-mono tabular-nums table-auto">
      <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap">
        <tr>
          <th class="px-3 py-3 text-left w-12">#</th>
          <th class="px-3 py-3 text-left w-[220px] sm:w-[260px]">Player</th>
          <th class="px-3 py-3 text-left w-[120px] sm:w-[160px]">Team</th>
          <th class="px-3 py-3 text-right w-[60px] hidden sm:table-cell">GP</th>
          <th class="px-3 py-3 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400" hx-get="/players/table?sort=pts" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">PTS</th>
          <th class="px-3 py-3 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 hidden md:table-cell" title="MG: 팀 득실마진(출전시간 가중 평균)" hx-get="/players/table?sort=mg" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">MG</th>
          <th class="px-3 py-3 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400" hx-get="/players/table?sort=reb" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">REB</th>
          <th class="px-3 py-3 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400 hidden md:table-cell" hx-get="/players/table?sort=ast" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">AST</th>
          <th class="px-3 py-3 text-right w-[72px] hidden lg:table-cell">STL</th>
          <th class="px-3 py-3 text-right w-[72px] hidden lg:table-cell">BLK</th>
          <th class="px-3 py-3 text-right w-[72px] hidden lg:table-cell">TO</th>
          <th class="px-3 py-3 text-right w-[72px] cursor-pointer hover:text-orange-600 dark:text-orange-400" hx-get="/players/table?sort=eff" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">EFF</th>
        </tr>
      </thead>
      <tbody id="players-body">%s</tbody>
    </table></div>|html}
    rows

(** Main layout *)
let layout ~title ?(canonical_path="/") ?(description="") ?(json_ld="")
    ?og_title ?og_description ?og_image ~content () =
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
  <link rel="icon" type="image/png" sizes="32x32" href="/static/images/favicon-32.png">
  <link rel="icon" type="image/png" sizes="16x16" href="/static/images/favicon-16.png">
  <link rel="apple-touch-icon" sizes="180x180" href="/static/images/apple-touch-icon.png">
  <script src="/static/js/theme-toggle.js?v=%s" data-cfasync="false"></script>
  <script src="/static/js/htmx-1.9.10.min.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/page-transitions.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/player-photo-fallback.js?v=%s" data-cfasync="false"></script>
  <script src="/static/js/mobile-nav.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/search-modal.js?v=%s" defer data-cfasync="false"></script>
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
  <header role="banner" class="sticky top-0 z-50 bg-white/95 dark:bg-slate-900/95 backdrop-blur border-b border-slate-200 dark:border-slate-800 px-4 sm:px-6 py-3 sm:py-4">
    <div class="max-w-7xl mx-auto flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3">
      <div class="flex items-center justify-between w-full sm:w-auto">
        <a href="/" class="flex items-center gap-3 shrink-0" aria-label="WKBL Analytics 홈으로 이동">
          <span class="text-2xl" aria-hidden="true">🏀</span>
          <span class="text-lg font-bold text-slate-900 dark:text-slate-200">WKBL <span class="text-orange-500">Analytics</span></span>
        </a>
        <div class="flex items-center gap-2 md:hidden">
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
        <button onclick="SearchModal.open()" class="flex items-center gap-2 px-3 py-1.5 text-sm text-slate-400 bg-slate-100 dark:bg-slate-800 rounded-lg hover:text-slate-600 dark:hover:text-slate-300 transition" aria-label="검색 열기">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path></svg>
          <span>검색</span>
          <kbd class="px-1.5 py-0.5 text-xs font-mono bg-white dark:bg-slate-700 rounded shadow-sm">⌘K</kbd>
        </button>
        <nav aria-label="메인 내비게이션" class="flex items-center gap-x-4 gap-y-2 text-sm flex-wrap justify-end">
          <a href="/" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Home</a>
          <a href="/awards" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Awards</a>
          <a href="/leaders" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Leaders</a>
          <a href="/boxscores" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Boxscores</a>
          <a href="/games" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Games</a>
          <a href="/standings" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Standings</a>
          <a href="/teams" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Teams</a>
          <a href="/players" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Players</a>
          <a href="/predict" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Predict</a>
          <a href="/compare" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Compare</a>
          <a href="/transactions" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Draft/Trade</a>
          <a href="/qa" class="shrink-0 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">QA</a>
        </nav>
        <button onclick="toggleTheme()" class="hidden sm:block p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="테마 전환 (라이트/다크)">
          <svg class="w-5 h-5 dark:hidden" fill="currentColor" viewBox="0 0 20 20" aria-hidden="true"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"></path></svg>
          <svg class="w-5 h-5 hidden dark:block" fill="currentColor" viewBox="0 0 20 20" aria-hidden="true"><path d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" fill-rule="evenodd" clip-rule="evenodd"></path></svg>
        </button>
      </div>
    </div>
  </header>
  <main id="main-content" class="max-w-7xl mx-auto px-4 sm:px-6 py-6 sm:py-8 pb-24 md:pb-8" tabindex="-1">%s</main>
  <footer class="border-t border-slate-200 dark:border-slate-800 py-6 text-center text-slate-500 dark:text-slate-400 text-sm mb-20 md:mb-0">
    <a href="mailto:contact@wkbl.win" class="hover:text-slate-700 dark:hover:text-slate-300 transition-colors">📧 contact@wkbl.win</a>
  </footer>

  <!-- Mobile Slide Menu Overlay -->
  <div id="mobile-menu-overlay" class="fixed inset-0 z-[100] bg-black/50 backdrop-blur-sm hidden opacity-0 transition-opacity duration-300" aria-hidden="true">
    <div id="mobile-menu-panel" class="absolute right-0 top-0 h-full w-72 bg-white dark:bg-slate-900 shadow-2xl transform translate-x-full transition-transform duration-300 overflow-y-auto" role="dialog" aria-modal="true" aria-label="메뉴">
      <div class="flex items-center justify-between p-4 border-b border-slate-200 dark:border-slate-700">
        <span class="font-bold text-slate-900 dark:text-white">메뉴</span>
        <button id="mobile-menu-close" class="p-2 rounded-lg text-slate-500 hover:text-slate-900 dark:hover:text-white transition" aria-label="메뉴 닫기">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg>
        </button>
      </div>
      <nav class="p-4 space-y-1" aria-label="모바일 메뉴">
        <a href="/" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"></path></svg>
          Home
        </a>
        <a href="/awards" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 3v4M3 5h4M6 17v4m-2-2h4m5-16l2.286 6.857L21 12l-5.714 2.143L13 21l-2.286-6.857L5 12l5.714-2.143L13 3z"></path></svg>
          Awards
        </a>
        <a href="/leaders" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>
          Leaders
        </a>
        <a href="/boxscores" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 17v-2m3 2v-4m3 4v-6m2 10H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path></svg>
          Boxscores
        </a>
        <a href="/games" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>
          Games
        </a>
        <a href="/standings" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 10h18M3 14h18m-9-4v8m-7 0h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v8a2 2 0 002 2z"></path></svg>
          Standings
        </a>
        <a href="/teams" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"></path></svg>
          Teams
        </a>
        <a href="/players" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"></path></svg>
          Players
        </a>
        <a href="/predict" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 7h8m0 0v8m0-8l-8 8-4-4-6 6"></path></svg>
          Predict
        </a>
        <a href="/compare" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7h12m0 0l-4-4m4 4l-4 4m0 6H4m0 0l4 4m-4-4l4-4"></path></svg>
          Compare
        </a>
        <a href="/transactions" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7h12m0 0l-4-4m4 4l-4 4m0 6H4m0 0l4 4m-4-4l4-4"></path></svg>
          Draft/Trade
        </a>
        <a href="/qa" class="flex items-center gap-3 px-4 py-3 rounded-lg text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition">
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"></path></svg>
          QA
        </a>
      </nav>
    </div>
  </div>

  <!-- Mobile Bottom Navigation -->
  <nav data-bottom-nav class="fixed bottom-0 left-0 right-0 z-50 bg-white/95 dark:bg-slate-900/95 backdrop-blur border-t border-slate-200 dark:border-slate-700 md:hidden safe-area-bottom" aria-label="모바일 하단 내비게이션">
    <div class="flex items-center justify-around h-16 max-w-md mx-auto px-2">
      <a href="/" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"></path></svg>
        <span class="text-[10px] font-medium">Home</span>
      </a>
      <a href="/leaders" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"></path></svg>
        <span class="text-[10px] font-medium">Leaders</span>
      </a>
      <a href="/games" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"></path></svg>
        <span class="text-[10px] font-medium">Games</span>
      </a>
      <a href="/players" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"></path></svg>
        <span class="text-[10px] font-medium">Players</span>
      </a>
      <button onclick="MobileNav.open()" class="flex flex-col items-center gap-1 px-3 py-2 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="더보기 메뉴">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"></path></svg>
        <span class="text-[10px] font-medium">More</span>
      </button>
    </div>
  </nav>
</body>
</html>|html}
    (escape_html title) (escape_html meta_desc) (escape_html og_title_val) (escape_html og_desc_val) (escape_html og_image_val) (escape_html canonical_url) (escape_html og_title_val) (escape_html og_desc_val) (escape_html og_image_val) (escape_html canonical_url) v v v v v v v json_ld_script cf_wa_script content

(** Home page *)
