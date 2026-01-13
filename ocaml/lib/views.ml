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

  let tabs = Printf.sprintf
    {html|<div id="season-stats-tabs" class="flex items-center gap-2 sm:gap-4 border-b border-slate-200 dark:border-slate-800 mb-4 overflow-x-auto no-scrollbar whitespace-nowrap -mx-4 px-4 sm:mx-0 sm:px-0">
      <button type="button" class="%s" hx-get="/player/%s/season-stats?scope=per_game" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Per Game</button>
      <button type="button" class="%s" hx-get="/player/%s/season-stats?scope=totals" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Totals</button>
      <button type="button" class="%s" hx-get="/player/%s/season-stats?scope=per_36" hx-target="#season-stats-component" hx-swap="outerHTML transition:true" hx-indicator="#season-stats-indicator" hx-disabled-elt="#season-stats-tabs button">Per 36</button>
      <span id="season-stats-indicator" class="htmx-indicator season-stats-indicator ml-auto text-xs text-slate-500 dark:text-slate-400"><span class="spinner"></span><span>Loading...</span></span>
    </div>|html}
    s_per player_id s_tot player_id s_36 player_id
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
let layout ~title ~content =
  let v = escape_html asset_version in
  let cf_wa_script =
    match Sys.getenv_opt "CF_WEB_ANALYTICS_TOKEN" |> Option.map String.trim with
    | Some token when token <> "" ->
        Printf.sprintf
          {html|  <script defer src="https://static.cloudflareinsights.com/beacon.min.js" data-cfasync="false" data-cf-beacon='{"token":"%s"}'></script>|html}
          (escape_html token)
    | _ -> ""
  in
  Printf.sprintf
    {html|<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <script src="/static/js/theme-toggle.js?v=%s" data-cfasync="false"></script>
  <script src="/static/js/htmx-1.9.10.min.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/page-transitions.js?v=%s" defer data-cfasync="false"></script>
  <script src="/static/js/player-photo-fallback.js?v=%s" data-cfasync="false"></script>
  <script src="https://cdn.tailwindcss.com" data-cfasync="false"></script>
  <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&family=Inter:wght@400;600;800&display=swap" rel="stylesheet">
  <link rel="stylesheet" href="/static/css/styles.css?v=%s">
  <script data-cfasync="false">tailwind.config = { darkMode: 'class', theme: { extend: { fontFamily: { sans: ['Inter', 'sans-serif'], mono: ['JetBrains Mono', 'monospace'] } } } }</script>
%s
</head>
<body class="bg-slate-50 dark:bg-[#0b0e14] text-slate-900 dark:text-slate-200 font-sans antialiased min-h-screen is-loading">
  <div id="page-loader" class="page-loader" aria-hidden="true"><div class="page-loader-bar"></div></div>
  <header class="sticky top-0 z-50 bg-white/95 dark:bg-slate-900/95 backdrop-blur border-b border-slate-200 dark:border-slate-800 px-4 sm:px-6 py-3 sm:py-4">
    <div class="max-w-7xl mx-auto flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3">
      <div class="flex items-center justify-between w-full sm:w-auto">
        <div class="flex items-center gap-3 shrink-0">
          <span class="text-2xl">🏀</span>
          <h1 class="text-lg font-bold text-slate-900 dark:text-slate-200">WKBL <span class="text-orange-500">Analytics</span></h1>
        </div>
        <button onclick="toggleTheme()" class="p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition sm:hidden" aria-label="Toggle theme">
          <svg class="w-5 h-5 dark:hidden" fill="currentColor" viewBox="0 0 20 20"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"></path></svg>
          <svg class="w-5 h-5 hidden dark:block" fill="currentColor" viewBox="0 0 20 20"><path d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" fill-rule="evenodd" clip-rule="evenodd"></path></svg>
        </button>
      </div>
      <div class="flex items-center gap-3">
        <nav class="flex items-center gap-3 sm:gap-x-4 sm:gap-y-2 text-xs sm:text-sm overflow-x-auto no-scrollbar whitespace-nowrap -mx-4 px-4 sm:mx-0 sm:px-0 flex-nowrap sm:flex-wrap sm:justify-end sm:overflow-visible flex-1">
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
        <button onclick="toggleTheme()" class="hidden sm:block p-2 rounded-lg bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 transition" aria-label="Toggle theme">
          <svg class="w-5 h-5 dark:hidden" fill="currentColor" viewBox="0 0 20 20"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"></path></svg>
          <svg class="w-5 h-5 hidden dark:block" fill="currentColor" viewBox="0 0 20 20"><path d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z" fill-rule="evenodd" clip-rule="evenodd"></path></svg>
        </button>
      </div>
    </div>
  </header>
  <main class="max-w-7xl mx-auto px-4 sm:px-6 py-6 sm:py-8">%s</main>
  <footer class="border-t border-slate-200 dark:border-slate-800 py-6 text-center text-slate-500 dark:text-slate-400 text-sm"></footer>
</body>
</html>|html}
    (escape_html title) v v v v v cf_wa_script content

(** Home page *)
let home_page players =
  let table = players_table players in
  layout ~title:"WKBL Analytics"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex items-center justify-between"><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">Top Players by Efficiency</h2><div class="flex gap-2"><input type="text" placeholder="Search player..." class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none" hx-get="/players/table" hx-trigger="keyup changed delay:300ms" hx-target="#players-table" name="search"></div></div><div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden">%s</div></div>|html}
      table)

let players_page ~season ~seasons ~search ~sort ~include_mismatch players =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let sort_value = match String.lowercase_ascii sort with | "pts" | "points" -> "pts" | "mg" | "margin" -> "mg" | "reb" | "rebounds" -> "reb" | "ast" | "assists" -> "ast" | "min" | "minutes" -> "min" | "eff" | "efficiency" -> "eff" | _ -> "eff" in
  let sort_option value label = let selected = if sort_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let table = players_table players in
  let include_checked = if include_mismatch then "checked" else "" in
  let mg_note =
    if sort_value = "mg" then
      {html|<details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-500 dark:text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">MG 안내 (왜 몇 명은 안 보이나요?)</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-900 dark:text-slate-200">MG</span> = (팀 득점 - 상대 득점)의 출전시간 가중 평균입니다.</div>
          <div>개인 <span class="font-mono text-slate-900 dark:text-slate-200">+/-</span>는 문자중계(PBP) 기반이며, 데이터가 없거나 PBP/박스스코어 최종 스코어 불일치 등 품질 이슈가 있으면 <span class="font-mono text-slate-900 dark:text-slate-200">-</span>로 표시합니다.</div>
	          <div>표본 안정성을 위해 <span class="font-mono text-slate-900 dark:text-slate-200">MG</span> 정렬 시 <span class="font-mono text-slate-900 dark:text-slate-200">스코어가 있는 경기</span> 기준 총 출전 <span class="font-mono text-slate-900 dark:text-slate-200">100분 이상</span> 선수만 표시합니다.</div>
	        </div>
	      </details>|html}
    else
      ""
  in
  layout ~title:"WKBL Players"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Players</h2><p class="text-slate-500 dark:text-slate-400 text-sm">Season-filtered player aggregates.</p></div><a class="text-orange-600 dark:text-orange-400 hover:text-orange-700 text-sm" href="/players">Reset</a></div><form id="players-filter" class="grid grid-cols-1 md:grid-cols-4 gap-3" hx-get="/players/table" hx-target="#players-table" hx-trigger="change, keyup delay:250ms"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" name="search" placeholder="Search player..." value="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none"><select name="sort" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s</select><div class="flex items-center justify-between gap-3 text-xs"><div class="text-slate-500 dark:text-slate-400 flex items-center">Sorted by %s</div><label class="flex items-center gap-2 text-slate-500 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="Final score != sum(points) 경기 포함"><span>Mismatch 포함</span></label></div></form>%s<div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden">%s</div></div>|html}
      season_options
      (escape_html search)
      (sort_option "eff" "EFF")
      (sort_option "pts" "PTS")
      (sort_option "mg" "MG")
      (sort_option "reb" "REB")
      (sort_option "ast" "AST")
      (sort_option "min" "MIN")
      (String.uppercase_ascii sort_value)
      include_checked
      mg_note
      table)

let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

(** Team stat row component to avoid too many sprintf arguments *)
let team_stat_row ~season (row: team_stats) =
  let team_href =
    if season = "ALL" then
      Printf.sprintf "/team/%s" (Uri.pct_encode row.team)
    else
      Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode row.team) (Uri.pct_encode season)
  in
  let name_cell = Printf.sprintf {html|<td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 flex items-center gap-2 whitespace-nowrap">%s<a href="%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a></td>|html} (team_logo_tag ~class_name:"w-5 h-5" row.team) (escape_html team_href) (escape_html row.team) in
  let gp_cell = Printf.sprintf {html|<td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400">%d</td>|html} row.gp in
  let stats_part1 = Printf.sprintf {html|<td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden sm:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden sm:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden md:table-cell">%s</td>|html}
    (format_float row.min_total) (format_float row.pts) (format_float row.margin) (format_float row.pts_against) (format_float row.reb) (format_float row.ast) (format_float row.stl) in
  let stats_part2 = Printf.sprintf {html|<td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden lg:table-cell">%s</td><td class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 font-bold">%s</td>|html}
    (format_float row.blk) (format_float row.turnovers) (format_float row.fg_pct) (format_float row.fg3_pct) (format_float row.ft_pct) (format_float row.efg_pct) (format_float row.eff)
  in
  Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors">%s%s%s%s</tr>|html} name_cell gp_cell stats_part1 stats_part2

let teams_table ~season ~scope (stats: team_stats list) =
  let rows =
    stats
    |> List.map (team_stat_row ~season)
    |> String.concat "\n"
  in
  let min_label = if scope = PerGame then "MIN/G" else "MIN" in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden"><table class="w-full min-w-max text-xs sm:text-sm font-mono tabular-nums"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-3 py-2 text-left">Team</th><th class="px-3 py-2 text-right">GP</th><th class="px-3 py-2 text-right hidden md:table-cell">%s</th><th class="px-3 py-2 text-right">PTS</th><th class="px-3 py-2 text-right" title="MG: 팀 득실마진(PTS - PA)">MG</th><th class="px-3 py-2 text-right hidden md:table-cell">PA</th><th class="px-3 py-2 text-right hidden sm:table-cell">REB</th><th class="px-3 py-2 text-right hidden sm:table-cell">AST</th><th class="px-3 py-2 text-right hidden md:table-cell">STL</th><th class="px-3 py-2 text-right hidden md:table-cell">BLK</th><th class="px-3 py-2 text-right hidden md:table-cell">TO</th><th class="px-3 py-2 text-right hidden lg:table-cell">FG%%</th><th class="px-3 py-2 text-right hidden lg:table-cell">3P%%</th><th class="px-3 py-2 text-right hidden lg:table-cell">FT%%</th><th class="px-3 py-2 text-right hidden lg:table-cell">eFG%%</th><th class="px-3 py-2 text-right text-orange-600 dark:text-orange-400">EFF</th></tr></thead><tbody>%s</tbody></table></div>|html}
    min_label rows

let teams_page ~season ~seasons ~scope ~sort ~include_mismatch stats =
  let scope_value = team_scope_to_string scope in
  let scope_option value label = let selected = if scope_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let sort_option value label = let selected = if String.lowercase_ascii sort = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let season_options = let base = seasons |> List.map (fun s -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = teams_table ~season ~scope stats in
  let include_checked = if include_mismatch then "checked" else "" in
  layout ~title:"WKBL Teams"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Teams</h2><p class="text-slate-500 dark:text-slate-400 text-sm">Team aggregates by season and scope.</p></div><a class="text-orange-600 dark:text-orange-400 hover:text-orange-700 text-sm" href="/teams">Reset</a></div><form id="teams-filter" class="grid grid-cols-1 md:grid-cols-3 gap-3" hx-get="/teams/table" hx-target="#teams-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><select name="scope" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s</select><select name="sort" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s%s%s%s</select><label class="md:col-span-3 flex items-center justify-end gap-2 text-xs text-slate-500 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="Final score != sum(points) 경기 포함"><span>Mismatch 포함</span></label></form><div id="teams-table">%s</div></div>|html}
      season_options (scope_option "per_game" "Per Game") (scope_option "totals" "Totals") (sort_option "pts" "PTS") (sort_option "reb" "REB") (sort_option "ast" "AST") (sort_option "stl" "STL") (sort_option "blk" "BLK") (sort_option "eff" "EFF") (sort_option "ts_pct" "TS%") (sort_option "fg3_pct" "3P%") (sort_option "min_total" "MIN") include_checked table)

let standings_table ~season (standings : team_standing list) =
  let rows =
    standings
    |> List.mapi (fun i (s : team_standing) ->
        let win_pct_fmt = Printf.sprintf "%.3f" s.win_pct in
        let gb_fmt = if s.gb = 0.0 then "-" else Printf.sprintf "%.1f" s.gb in
        let team_href =
          if season = "ALL" then
            Printf.sprintf "/team/%s" (Uri.pct_encode s.team_name)
          else
            Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode s.team_name) (Uri.pct_encode season)
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono text-sm">%d</td><td class="px-4 py-3 font-bold text-slate-900 dark:text-slate-200 flex items-center gap-2">%s<a href="%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300">%d</td><td class="px-4 py-3 text-right text-orange-600 dark:text-orange-400 font-bold">%s</td><td class="px-4 py-3 text-right text-slate-500 dark:text-slate-400 hidden sm:table-cell">%s</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 font-mono hidden md:table-cell">%.1f</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 font-mono hidden md:table-cell">%.1f</td><td class="px-4 py-3 text-right %s font-mono font-bold hidden sm:table-cell">%.1f</td></tr>|html}
          (i + 1) (team_logo_tag ~class_name:"w-5 h-5" s.team_name) (escape_html team_href) (escape_html s.team_name) s.games_played s.wins s.losses win_pct_fmt gb_fmt s.avg_pts s.avg_opp_pts (if s.diff >= 0.0 then "text-emerald-400" else "text-rose-400") s.diff)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl"><table class="min-w-[560px] sm:min-w-[760px] w-full text-xs sm:text-sm font-mono tabular-nums"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-4 py-3 text-left w-12">Rk</th><th class="px-4 py-3 text-left font-sans">Team</th><th class="px-4 py-3 text-right">GP</th><th class="px-4 py-3 text-right">W</th><th class="px-4 py-3 text-right">L</th><th class="px-4 py-3 text-right">PCT</th><th class="px-4 py-3 text-right hidden sm:table-cell">GB</th><th class="px-4 py-3 text-right hidden md:table-cell">PS/G</th><th class="px-4 py-3 text-right hidden md:table-cell">PA/G</th><th class="px-4 py-3 text-right hidden sm:table-cell">DIFF</th></tr></thead><tbody id="standings-body">%s</tbody></table></div>|html}
    rows

let standings_page ~season ~seasons standings =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = standings_table ~season standings in
  layout ~title:"WKBL Standings"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Standings</h2><p class="text-slate-500 dark:text-slate-400 text-sm">League rank and win percentage.</p></div></div><form id="standings-filter" class="flex gap-3" hx-get="/standings/table" hx-target="#standings-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="standings-table">%s</div></div>|html}
      season_options table)

let games_table (games : game_summary list) =
  let mobile_cards =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> string_of_int s | None -> "-" in
        let score_b = match g.away_score with Some s -> string_of_int s | None -> "-" in
        let status_class = if g.home_score = None then "text-slate-500 dark:text-slate-400" else "text-slate-900 dark:text-slate-200" in
        let action_html =
          if g.home_score = None then
            {html|<span class="text-[10px] text-slate-500 dark:text-slate-400">TBD</span>|html}
          else
            Printf.sprintf
              {html|<a href="/boxscore/%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Boxscore</a>|html}
              (escape_html g.game_id)
        in
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-3 shadow-sm space-y-2">
  <div class="flex items-center justify-between text-[11px] text-slate-500 dark:text-slate-400 font-mono">
    <span>#%d · %s</span>
    %s
  </div>
  <div class="flex items-center justify-between gap-3">
    <div class="flex flex-col gap-1 min-w-0">
      <div class="flex items-center gap-2 text-sm font-medium">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors truncate">%s</a></div>
      <div class="flex items-center gap-2 text-sm font-medium">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors truncate">%s</a></div>
    </div>
    <div class="text-right font-mono text-sm %s whitespace-nowrap">%s - %s</div>
  </div>
</div>|html}
          (i + 1)
          (escape_html g.game_date)
          action_html
          (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
          (Uri.pct_encode g.home_team)
          (escape_html g.home_team)
          (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
          (Uri.pct_encode g.away_team)
          (escape_html g.away_team)
          status_class
          score_a
          score_b)
    |> String.concat "\n"
  in
  let rows =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> string_of_int s | None -> "-" in
        let score_b = match g.away_score with Some s -> string_of_int s | None -> "-" in
        let status_class = if g.home_score = None then "text-slate-500 dark:text-slate-400" else "text-slate-900 dark:text-slate-200" in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors %s"><td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono text-sm hidden sm:table-cell">%d</td><td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono text-sm w-[90px] sm:w-32 whitespace-nowrap">%s</td><td class="px-4 py-3 font-medium flex items-center gap-2">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-center font-bold text-orange-600 dark:text-orange-400 font-mono">%s - %s</td><td class="px-4 py-3 text-right font-medium"><div class="flex items-center justify-end gap-2"><a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s</div></td><td class="px-4 py-3 text-right w-[64px] sm:w-24"><a href="/boxscore/%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Boxscore</a></td></tr>|html}
          status_class (i + 1) (escape_html g.game_date) (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) score_a score_b (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) (escape_html g.game_id))
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="space-y-3 sm:hidden">%s</div><div class="hidden sm:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl"><table class="min-w-[720px] sm:min-w-[860px] w-full text-xs sm:text-sm font-mono tabular-nums"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-4 py-3 text-left w-12 hidden sm:table-cell">#</th><th class="px-4 py-3 text-left font-sans w-[90px] sm:w-32">Date</th><th class="px-4 py-3 text-left font-sans">Home</th><th class="px-4 py-3 text-center w-32">Score</th><th class="px-4 py-3 text-right font-sans">Away</th><th class="px-4 py-3 text-right font-sans w-[64px] sm:w-24">Action</th></tr></thead><tbody id="games-body">%s</tbody></table></div>|html}
    mobile_cards rows

let games_page ~season ~seasons games =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = games_table games in
  layout ~title:"WKBL Games"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Games</h2><p class="text-slate-500 dark:text-slate-400 text-sm">Season schedule and results.</p></div></div><form id="games-filter" class="flex gap-3" hx-get="/games/table" hx-target="#games-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="games-table">%s</div></div>|html}
      season_options table)

let boxscores_table (games : game_summary list) =
  let mobile_cards =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> s | None -> 0 in
        let score_b = match g.away_score with Some s -> s | None -> 0 in
        let margin = score_a - score_b in
        let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0" in
        let margin_color = if margin > 0 then "text-sky-600 dark:text-sky-400" else if margin < 0 then "text-rose-400" else "text-slate-500 dark:text-slate-400" in
        if g.home_score = None then ""
        else
          Printf.sprintf
            {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-3 shadow-sm space-y-2">
  <div class="flex items-center justify-between text-[11px] text-slate-500 dark:text-slate-400 font-mono">
    <span>#%d · %s</span>
    <span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono %s whitespace-nowrap">Δ %s</span>
  </div>
  <div class="flex items-center justify-between gap-3">
    <div class="flex flex-col gap-1 min-w-0 w-full">
      <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
      <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
    </div>
    <a href="/boxscore/%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">View</a>
  </div>
</div>|html}
            (i + 1)
            (escape_html g.game_date)
            margin_color
            margin_str
            (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
            (Uri.pct_encode g.home_team)
            (escape_html g.home_team)
            score_a
            (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
            (Uri.pct_encode g.away_team)
            (escape_html g.away_team)
            score_b
            (escape_html g.game_id))
    |> String.concat "\n"
  in
  let rows =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> s | None -> 0 in
        let score_b = match g.away_score with Some s -> s | None -> 0 in
        let margin = score_a - score_b in
        let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0" in
        let margin_color = if margin > 0 then "text-sky-600 dark:text-sky-400" else if margin < 0 then "text-rose-400" else "text-slate-500 dark:text-slate-400" in
        if g.home_score = None then ""
        else
          Printf.sprintf
            {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono text-sm hidden sm:table-cell">%d</td><td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono text-sm w-[90px] sm:w-32 whitespace-nowrap">%s</td><td class="px-4 py-3 font-medium flex items-center gap-2">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-center font-bold text-slate-900 dark:text-slate-200 font-mono">%d</td><td class="px-4 py-3 text-center font-bold text-slate-900 dark:text-slate-200 font-mono">%d</td><td class="px-4 py-3 font-medium text-right"><div class="flex items-center justify-end gap-2"><a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s</div></td><td class="px-4 py-3 text-right font-bold font-mono %s hidden sm:table-cell">%s</td><td class="px-4 py-3 text-right w-[60px] sm:w-20">
                  <a href="/boxscore/%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">View</a>
                </td></tr>|html}
            (i + 1) (escape_html g.game_date) (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) score_a score_b (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) margin_color margin_str (escape_html g.game_id))
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="space-y-3 sm:hidden">%s</div><div class="hidden sm:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl"><table class="min-w-[720px] sm:min-w-[900px] w-full text-xs sm:text-sm font-mono tabular-nums"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-4 py-3 text-left w-12 hidden sm:table-cell">#</th><th class="px-4 py-3 text-left w-[90px] sm:w-32 font-sans">Date</th><th class="px-4 py-3 text-left font-sans">Home</th><th class="px-4 py-3 text-center font-sans">PTS</th><th class="px-4 py-3 text-center font-sans">PTS</th><th class="px-4 py-3 text-right font-sans">Away</th><th class="px-4 py-3 text-right font-sans hidden sm:table-cell">Margin</th><th class="px-4 py-3 text-right w-[60px] sm:w-20 font-sans">Link</th></tr></thead><tbody id="boxscores-body">%s</tbody></table></div>|html}
    mobile_cards rows

let boxscores_page ~season ~seasons games =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = boxscores_table games in
  layout ~title:"WKBL Boxscores"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Boxscores</h2><p class="text-slate-500 dark:text-slate-400 text-sm">Game results and margins.</p></div></div><form id="boxscores-filter" class="flex gap-3" hx-get="/boxscores/table" hx-target="#boxscores-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="boxscores-table">%s</div></div>|html}
      season_options table)

let boxscore_player_table (title: string) (players: boxscore_player_stat list) =
  (* Dedupe: if the same stat line is duplicated due to name matching, keep 1 row. *)
  let players =
    let key_of (p: boxscore_player_stat) =
      let minutes_key = int_of_float (floor (p.bs_minutes *. 10.0 +. 0.5)) in
      Printf.sprintf
        "%s|%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d"
        (normalize_name p.bs_player_name)
        (String.trim p.bs_team_code)
        minutes_key
        p.bs_pts
        p.bs_reb
        p.bs_ast
        p.bs_stl
        p.bs_blk
        p.bs_tov
        p.bs_fg_made
        p.bs_fg_att
        p.bs_fg3_made
        p.bs_fg3_att
        p.bs_ft_made
        p.bs_ft_att
    in
    let quality_score (p: boxscore_player_stat) =
      let pm_score = if Option.is_some p.bs_plus_minus then 2 else 0 in
      let pos_score =
        match p.bs_position with
        | Some pos when String.trim pos <> "" -> 1
        | _ -> 0
      in
      pm_score + pos_score
    in
    let best_by_key : (string, int * boxscore_player_stat) Hashtbl.t = Hashtbl.create 64 in
    players
    |> List.iteri (fun idx p ->
        let key = key_of p in
        match Hashtbl.find_opt best_by_key key with
        | None -> Hashtbl.add best_by_key key (idx, p)
        | Some (best_idx, best_p) ->
            let s_new = quality_score p in
            let s_best = quality_score best_p in
            if s_new > s_best then
              Hashtbl.replace best_by_key key (idx, p)
            else if s_new = s_best && idx < best_idx then
              Hashtbl.replace best_by_key key (idx, p)
            else
              ())
    ;
    best_by_key
    |> Hashtbl.to_seq_values
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  let name_counts : (string, int) Hashtbl.t = Hashtbl.create 32 in
  players
  |> List.iter (fun (p: boxscore_player_stat) ->
      let key = normalize_name p.bs_player_name in
      let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
      Hashtbl.replace name_counts key (prev + 1));
  let rows =
    players
    |> List.map (fun (p: boxscore_player_stat) ->
        let pos_badge =
          match p.bs_position with
          | Some pos when String.trim pos <> "" ->
              Printf.sprintf
                {html|<span class="ml-2 px-1.5 py-0.5 rounded bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400 text-[10px] font-sans">%s</span>|html}
                (escape_html pos)
          | _ -> ""
        in
        let show_player_id =
          match Hashtbl.find_opt name_counts (normalize_name p.bs_player_name) with
          | Some c when c > 1 -> true
          | _ -> false
        in
        let id_badge =
          if show_player_id then
            Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge p.bs_player_id)
          else
            ""
        in
        let pm_class, pm_str =
          match p.bs_plus_minus with
          | None -> ("text-slate-500 dark:text-slate-400", "-")
          | Some v ->
              let cls =
                if v > 0 then "text-sky-600 dark:text-sky-400"
                else if v < 0 then "text-rose-400"
                else "text-slate-500 dark:text-slate-400"
              in
              let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
              (cls, s)
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 flex items-center gap-3 min-w-[160px]">%s<a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s</td><td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 font-mono text-sm w-[56px] sm:w-[60px] hidden sm:table-cell">%.1f</td><td class="px-3 py-2 text-right text-slate-900 dark:text-slate-200 font-bold w-[56px] sm:w-[60px]">%d</td><td class="px-3 py-2 text-right font-mono w-[56px] sm:w-[60px] %s hidden sm:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[56px] sm:w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[56px] sm:w-[60px]">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[56px] sm:w-[60px] hidden md:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[56px] sm:w-[60px] hidden md:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[56px] sm:w-[60px] hidden md:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-xs font-mono w-[120px] hidden lg:table-cell">%d-%d (%.1f%%)</td><td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-xs font-mono w-[120px] hidden lg:table-cell">%d-%d (%.1f%%)</td><td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-xs font-mono w-[120px] hidden lg:table-cell">%d-%d (%.1f%%)</td></tr>|html}
          (player_img_tag ~class_name:"w-6 h-6" p.bs_player_id p.bs_player_name)
          p.bs_player_id
          (escape_html (normalize_name p.bs_player_name))
          (pos_badge ^ id_badge)
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
    {html|<div class="space-y-3"><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 flex items-center gap-2"><span class="w-1 h-6 bg-orange-500 rounded-full"></span>%s</h3><div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-lg"><table class="min-w-[560px] sm:min-w-[720px] lg:min-w-[980px] w-full text-sm font-mono table-auto"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase tracking-wider"><tr><th class="px-3 py-2 text-left font-sans">Player</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px] hidden sm:table-cell">MP</th><th class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 w-[56px] sm:w-[60px]">PTS</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px] hidden sm:table-cell">+/-</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px]">TRB</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px]">AST</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px] hidden md:table-cell">STL</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px] hidden md:table-cell">BLK</th><th class="px-3 py-2 text-right w-[56px] sm:w-[60px] hidden md:table-cell">TOV</th><th class="px-3 py-2 text-right w-[120px] hidden lg:table-cell">FG</th><th class="px-3 py-2 text-right w-[120px] hidden lg:table-cell">3P</th><th class="px-3 py-2 text-right w-[120px] hidden lg:table-cell">FT</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
    (escape_html title) rows

let boxscore_page (bs: game_boxscore) =
  let gi = bs.boxscore_game in
  let margin = gi.gi_home_score - gi.gi_away_score in
  let margin_str =
    if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0"
  in
  let margin_class =
    if margin > 0 then "text-sky-600 dark:text-sky-400"
    else if margin < 0 then "text-orange-600 dark:text-orange-400"
    else "text-slate-500 dark:text-slate-400"
  in
  let margin_badge =
    Printf.sprintf
      {html|<span class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider %s">MARGIN %s</span>|html}
      margin_class
      (escape_html margin_str)
  in
  let quality_badge = score_quality_badge gi.gi_score_quality in
  let official_link =
    match wkbl_official_game_result_url gi.gi_game_id with
    | None -> ""
    | Some url ->
        Printf.sprintf
          {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
          (escape_html url)
  in
  let pbp_link =
    Printf.sprintf
      {html|<a href="/boxscore/%s/pbp" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">PBP</a>|html}
      (escape_html gi.gi_game_id)
  in
  let data_notes =
    Printf.sprintf
      {html|<details class="max-w-2xl mx-auto bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-xs text-slate-500 dark:text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">출처 / 검증 기준</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-900 dark:text-slate-200">MARGIN</span>은 경기 최종 득점(팀-상대)입니다.</div>
          <div class="pt-1 text-slate-500 dark:text-slate-400 font-bold">출처</div>
          <div>• 스코어: WKBL 공식 경기 결과 페이지(<span class="font-mono text-slate-700 dark:text-slate-300">/game/result.asp</span>) %s</div>
          <div>• 박스스코어: WKBL 공식 박스스코어(AJAX) → <span class="font-mono text-slate-700 dark:text-slate-300">game_stats</span></div>
          <div>• 개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>: WKBL 문자중계(PBP) → <span class="font-mono text-slate-700 dark:text-slate-300">player_plus_minus</span> (일부 경기만)</div>
          <div class="pt-1 text-slate-500 dark:text-slate-400 font-bold">검증 기준</div>
          <div>• <span class="font-mono text-slate-900 dark:text-slate-200">VERIFIED (✓)</span>: <span class="font-mono text-slate-700 dark:text-slate-300">games.home/away_score</span>가 있고, 양 팀 <span class="font-mono text-slate-700 dark:text-slate-300">SUM(game_stats.pts)</span>와 모두 일치</div>
          <div>• <span class="font-mono text-slate-900 dark:text-slate-200">DERIVED (Σ)</span>: 스코어/합계 누락으로 교차검증 불가. 표시 스코어는 <span class="font-mono text-slate-700 dark:text-slate-300">COALESCE(games score, sum pts)</span></div>
          <div>• <span class="font-mono text-slate-900 dark:text-slate-200">MISMATCH (!)</span>: 스코어와 합계가 모두 있는데 값이 다름 (<a href="/qa" class="text-orange-600 dark:text-orange-400 hover:underline">QA</a>)</div>
          <div class="pt-1">※ 이 검증은 “최종 득점”만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 검증이 필요합니다.</div>
          <div>※ 동명이인 매칭 오류로 동일 스탯 라인이 중복될 수 있어, 동일 라인은 1개만 표시합니다.</div>
        </div>
      </details>|html}
      official_link
  in
  let home_table = boxscore_player_table gi.gi_home_team_name bs.boxscore_home_players in
  let away_table = boxscore_player_table gi.gi_away_team_name bs.boxscore_away_players in
  layout ~title:(Printf.sprintf "Boxscore: %s vs %s" gi.gi_home_team_name gi.gi_away_team_name)
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-2xl"><div class="flex flex-col items-center gap-6"><div class="text-slate-500 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div><div class="flex items-center justify-between w-full max-w-2xl gap-3 sm:gap-6"><div class="flex flex-col items-center gap-2 sm:gap-3 min-w-0"><div class="text-lg sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-2 sm:gap-3 min-w-0">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors truncate max-w-[120px] sm:max-w-none whitespace-nowrap">%s</a></div><div class="text-[11px] sm:text-sm text-slate-500 dark:text-slate-400">HOME</div></div><div class="flex items-center gap-4 sm:gap-8"><div class="text-4xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%d</div><div class="flex flex-col items-center gap-1.5 sm:gap-2"><div class="text-lg sm:text-2xl text-slate-700 dark:text-slate-300 font-light">vs</div><div class="flex flex-wrap items-center justify-center gap-1.5 sm:gap-2">%s%s%s</div></div><div class="text-4xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%d</div></div><div class="flex flex-col items-center gap-2 sm:gap-3 min-w-0"><div class="text-lg sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-2 sm:gap-3 min-w-0"><a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors truncate max-w-[120px] sm:max-w-none whitespace-nowrap">%s</a>%s</div><div class="text-[11px] sm:text-sm text-slate-500 dark:text-slate-400">AWAY</div></div></div></div></div>%s<div class="grid grid-cols-1 gap-8">%s%s</div><div class="flex justify-center"><a href="/games" class="text-slate-500 dark:text-slate-400 hover:text-orange-500 transition text-sm">← Back to Games</a></div></div>|html}
      (escape_html gi.gi_game_date)
      (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_home_team_name) (Uri.pct_encode gi.gi_home_team_name) (escape_html gi.gi_home_team_name)
      gi.gi_home_score
      margin_badge
      quality_badge
      pbp_link
      gi.gi_away_score
      (Uri.pct_encode gi.gi_away_team_name) (escape_html gi.gi_away_team_name) (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_away_team_name)
      data_notes
      home_table away_table)

let pbp_period_label = function
  | "Q1" -> "1Q"
  | "Q2" -> "2Q"
  | "Q3" -> "3Q"
  | "Q4" -> "4Q"
  | "X1" -> "OT1"
  | "X2" -> "OT2"
  | "X3" -> "OT3"
  | "X4" -> "OT4"
  | p -> p

let pbp_page ~(game: game_info) ~(periods: string list) ~(selected_period: string) ~(events: pbp_event list) =
  let official_link =
    match wkbl_official_game_result_url game.gi_game_id with
    | None -> ""
    | Some url ->
        Printf.sprintf
          {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
          (escape_html url)
  in
  let tabs =
    match periods with
    | [] -> ""
    | _ ->
        periods
        |> List.map (fun p ->
            let is_selected = p = selected_period in
            let cls =
              if is_selected then "bg-orange-500/15 border-orange-500/30 text-orange-700"
              else "bg-slate-100 dark:bg-slate-800/40 border-slate-300 dark:border-slate-700/40 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200"
            in
            Printf.sprintf
              {html|<a href="/boxscore/%s/pbp?period=%s" class="px-3 py-1.5 rounded-full border text-xs font-mono tracking-wider transition %s">%s</a>|html}
              (escape_html game.gi_game_id)
              (Uri.pct_encode p)
              cls
              (escape_html (pbp_period_label p)))
        |> String.concat "\n"
        |> fun s -> Printf.sprintf {html|<div class="flex flex-wrap items-center justify-center gap-2">%s</div>|html} s
  in
  let rows =
    events
    |> List.map (fun (e: pbp_event) ->
        let team_name, side_label =
          match e.pe_team_side with
          | 1 -> (game.gi_away_team_name, "AWAY")
          | 2 -> (game.gi_home_team_name, "HOME")
          | _ -> ("", "—")
        in
        let side_badge =
          match e.pe_team_side with
          | 1 | 2 ->
              Printf.sprintf
                {html|<div class="flex items-center gap-2">%s<span class="text-[10px] font-mono text-slate-500 dark:text-slate-400 tracking-wider">%s</span></div>|html}
                (team_badge ~max_width:"max-w-[110px] sm:max-w-[140px]" team_name)
                (escape_html side_label)
          | _ ->
              {html|<div class="flex items-center gap-2"><div class="w-6 h-6 rounded bg-slate-100 dark:bg-slate-800 flex items-center justify-center text-xs">🏀</div><span class="text-[10px] font-mono text-slate-500 dark:text-slate-400 tracking-wider">—</span></div>|html}
        in
        let score_badge =
          match e.pe_team1_score, e.pe_team2_score with
          | Some a, Some h ->
              Printf.sprintf
                {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap" title="AWAY %s / HOME %s">A %d - %d H</span>|html}
                (escape_html game.gi_away_team_name)
                (escape_html game.gi_home_team_name)
                a
                h
          | _ -> ""
        in
        let is_score = score_badge <> "" in
        let row_cls =
          if is_score then "bg-slate-100 dark:bg-slate-800/15"
          else "hover:bg-slate-100 dark:bg-slate-800/30"
        in
        Printf.sprintf
          {html|<li class="flex items-start gap-3 px-4 py-3 border-b border-slate-200 dark:border-slate-800/60 last:border-0 %s transition-colors">
            <div class="w-[56px] text-right text-xs font-mono text-slate-500 dark:text-slate-400 pt-1">%s</div>
            <div class="min-w-0 flex-1">
              <div class="flex flex-wrap items-center gap-2">
                %s
                %s
                <span class="text-sm text-slate-900 dark:text-slate-200 break-words">%s</span>
              </div>
            </div>
          </li>|html}
          row_cls
          (escape_html e.pe_clock)
          side_badge
          score_badge
          (escape_html e.pe_description))
    |> String.concat "\n"
  in
  let body =
    match periods with
    | [] ->
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-slate-500 dark:text-slate-400 text-sm">
            <div class="font-bold text-slate-900 dark:text-slate-200 mb-2">PBP 데이터가 없습니다</div>
            <div class="leading-relaxed">
              WKBL 문자중계(PBP)가 제공되지 않았거나 아직 수집되지 않은 경기입니다.
              개인 <span class="font-mono text-slate-900 dark:text-slate-200">+/-</span> 계산도 PBP 기반이라 이 경기에는 표시되지 않을 수 있습니다.
            </div>
          </div>|html}
    | _ ->
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">%s<ul class="divide-y divide-slate-800/60">%s</ul></div>|html}
          tabs
          rows
  in
  layout
    ~title:(Printf.sprintf "PBP: %s vs %s" game.gi_home_team_name game.gi_away_team_name)
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-4">
          <div class="space-y-2">
            <div class="text-slate-500 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div>
            <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Play-by-Play</h2>
            <div class="flex flex-wrap items-center gap-2 text-sm">
              %s
              <span class="text-slate-600">vs</span>
              %s
              <span class="text-slate-500 dark:text-slate-400 font-mono">%d - %d</span>
            </div>
          </div>
          <div class="flex items-center gap-3">
            %s
            <a href="/boxscore/%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">← Boxscore</a>
          </div>
        </div>
        <div class="text-xs text-slate-500 dark:text-slate-400 leading-relaxed">
          <span class="font-mono text-slate-900 dark:text-slate-200">team1/team2</span> 스코어는 WKBL 문자중계(XML) 기준이며, 일반적으로 <span class="font-mono text-slate-900 dark:text-slate-200">A(원정)</span> / <span class="font-mono text-slate-900 dark:text-slate-200">H(홈)</span> 순으로 표시됩니다.
        </div>
        %s
      </div>|html}
      (escape_html game.gi_game_date)
      (team_badge ~max_width:"max-w-[160px]" game.gi_home_team_name)
      (team_badge ~max_width:"max-w-[160px]" game.gi_away_team_name)
      game.gi_home_score
      game.gi_away_score
      official_link
      (escape_html game.gi_game_id)
      body)

let compare_stat_row ?(signed=false) label val1 val2 =
  let max_val = max (abs_float val1) (abs_float val2) in
  let pct1 = if max_val = 0.0 then 0.0 else abs_float val1 /. max_val *. 100.0 in
  let pct2 = if max_val = 0.0 then 0.0 else abs_float val2 /. max_val *. 100.0 in
  let value_str v =
    if signed && v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v
  in
  Printf.sprintf
    {html|<div class="flex flex-col gap-1"><div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-500 dark:text-slate-400"><span>%s</span><span class="text-slate-500 dark:text-slate-400">%s</span><span>%s</span></div><div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden"><div class="flex justify-end w-1/2 border-r border-slate-300 dark:border-slate-700"><div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div><div class="flex justify-start w-1/2"><div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div></div></div>|html}
    (escape_html (value_str val1)) (escape_html label) (escape_html (value_str val2)) pct1 pct2

let h2h_game_row (g: h2h_game) =
  Printf.sprintf
    {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 font-mono transition-colors">
      <td class="px-3 py-2 text-slate-500 dark:text-slate-400 text-xs w-[100px] truncate">%s</td>
      <td class="px-3 py-2 text-right text-slate-900 dark:text-slate-200 w-[60px] font-bold">%d</td>
      <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-center text-slate-500 dark:text-slate-400 text-xs font-sans w-[40px]">vs</td>
      <td class="px-3 py-2 text-left text-slate-900 dark:text-slate-200 w-[60px] font-bold">%d</td>
      <td class="px-3 py-2 text-left text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-left text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-right text-xs font-sans w-[80px]"><span class="px-1.5 py-0.5 rounded bg-slate-100 dark:bg-slate-800 text-slate-500 dark:text-slate-400">DIFF %d</span></td>
    </tr>|html}
    (escape_html g.game_date) g.player1_pts g.player1_reb g.player1_ast g.player2_pts g.player2_reb g.player2_ast g.score_diff

let h2h_game_table (p1_name: string) (p2_name: string) (games: h2h_game list) =
  let rows = games |> List.map h2h_game_row |> String.concat "\n" in
  Printf.sprintf {html|<div class="space-y-3 mt-8"><h3 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest">Match History</h3><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-xl"><table class="w-full text-sm table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase text-[10px] tracking-tighter"><tr><th class="px-3 py-2 text-left font-sans w-[100px]">Date</th><th class="px-3 py-2 text-right font-sans text-orange-600 dark:text-orange-400 w-[60px]">%s PTS</th><th class="px-3 py-2 text-right font-sans w-[60px]">REB</th><th class="px-3 py-2 text-right font-sans w-[60px]">AST</th><th class="px-3 py-2 w-[40px]"></th><th class="px-3 py-2 text-left font-sans text-sky-600 dark:text-sky-400 w-[60px]">%s PTS</th><th class="px-3 py-2 text-left font-sans w-[60px]">REB</th><th class="px-3 py-2 text-left font-sans w-[60px]">AST</th><th class="px-3 py-2 w-[80px]"></th></tr></thead><tbody>%s</tbody></table></div></div>|html} (escape_html p1_name) (escape_html p2_name) rows

let compare_page
    ~season
    ~seasons
    ~p1_season
    ~p2_season
    ~p1_query
    ~p2_query
    ~p1_id
    ~p2_id
    ~p1_candidates
    ~p2_candidates
    ~error
    ~h2h_disabled_reason
    (p1_selected: player_aggregate option)
    (p2_selected: player_aggregate option)
    (h2h: h2h_game list)
  =
  let season_options ~selected =
    let base =
      seasons
      |> List.map (fun (s : season_info) ->
          let is_selected = if s.code = selected then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code is_selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<option value="ALL" %s>All Seasons</option>%s|html}
      (if selected = "ALL" then "selected" else "")
      base
  in
  let season_label code =
    if code = "ALL" then "All Seasons"
    else
      seasons
      |> List.find_opt (fun (s: season_info) -> s.code = code)
      |> Option.map (fun (s: season_info) -> s.name)
      |> Option.value ~default:code
  in
  let season_badge code =
    Printf.sprintf
      {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</span>|html}
      (escape_html (season_label code))
  in
  let query_display (query : string) (selected : player_aggregate option) =
    if String.trim query <> "" then query
    else
      match selected with
      | None -> ""
      | Some p -> normalize_name p.name
  in
  let p1_display = query_display p1_query p1_selected in
  let p2_display = query_display p2_query p2_selected in
  let compare_url ~season ~p1_season ~p2_season ~p1 ~p2 ~p1_id ~p2_id =
    let params =
      [ ("season", season); ("p1_season", p1_season); ("p2_season", p2_season); ("p1", p1); ("p2", p2) ]
      @ (match p1_id with None -> [] | Some id -> [ ("p1_id", id) ])
      @ (match p2_id with None -> [] | Some id -> [ ("p2_id", id) ])
    in
    let encode (k, v) = k ^ "=" ^ Uri.pct_encode v in
    "/compare?" ^ String.concat "&" (List.map encode params)
  in
  let selected_card ~accent_border ~season_code (p : player_aggregate) =
    let mp =
      if p.games_played <= 0 then 0.0 else p.total_minutes /. float_of_int p.games_played
    in
    let id_badge_html = Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge p.player_id) in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg border-t-4 %s">
        <div class="flex items-center gap-4">
          %s
          <div class="min-w-0">
            <div class="text-lg font-black text-slate-900 dark:text-slate-200 truncate">
              <a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s
            </div>
            <div class="mt-1 text-xs text-slate-500 dark:text-slate-400 flex flex-wrap items-center gap-2">
              <span class="truncate">%s</span>
              %s
              <span class="font-mono">GP %d</span>
              <span class="font-mono">MP %.1f</span>
              <span class="font-mono">EFF %.1f</span>
            </div>
          </div>
        </div>
        <div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400">이름을 다시 입력하면 선택이 초기화됩니다.</div>
      </div>|html}
      accent_border
      (player_img_tag ~class_name:"w-14 h-14" p.player_id p.name)
      p.player_id
      (escape_html (normalize_name p.name))
      id_badge_html
      (escape_html p.team_name)
      (season_badge season_code)
      p.games_played
      mp
      p.efficiency
  in
  let candidate_row ~accent_btn_class ~slot (c : player_aggregate) =
    let mp =
      if c.games_played <= 0 then 0.0 else c.total_minutes /. float_of_int c.games_played
    in
    let p1_for_link, p2_for_link, p1_id_for_link, p2_id_for_link =
      match slot with
      | `P1 ->
          ( normalize_name c.name,
            p2_display,
            Some c.player_id,
            p2_id )
      | `P2 ->
          ( p1_display,
            normalize_name c.name,
            p1_id,
            Some c.player_id )
    in
    let url =
      compare_url
        ~season
        ~p1_season
        ~p2_season
        ~p1:p1_for_link
        ~p2:p2_for_link
        ~p1_id:p1_id_for_link
        ~p2_id:p2_id_for_link
    in
    let id_badge_html = Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge c.player_id) in
    Printf.sprintf
      {html|<div class="flex items-center justify-between gap-3 py-2 border-b border-slate-200 dark:border-slate-800/60 last:border-0">
        <div class="flex items-center gap-3 min-w-0">
          %s
          <div class="min-w-0">
            <div class="text-sm font-medium text-slate-700 dark:text-slate-300 truncate">
              <a href="%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s
            </div>
            <div class="mt-0.5 text-[11px] text-slate-500 dark:text-slate-400 flex flex-wrap items-center gap-2">
              <span class="truncate">%s</span>
              <span class="font-mono">GP %d</span>
              <span class="font-mono">MP %.1f</span>
              <span class="font-mono">EFF %.1f</span>
            </div>
          </div>
        </div>
        <a href="%s" class="shrink-0 px-3 py-1 rounded-lg border border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800/60 text-xs font-bold %s hover:brightness-125 transition">Select</a>
      </div>|html}
      (player_img_tag ~class_name:"w-10 h-10" c.player_id c.name)
      (escape_html url)
      (escape_html (normalize_name c.name))
      id_badge_html
      (escape_html c.team_name)
      c.games_played
      mp
      c.efficiency
      (escape_html url)
      accent_btn_class
  in
  let candidates_panel ~title ~slot ~accent_btn_class (query : string) (candidates : player_aggregate list) =
    let body =
      if String.trim query = "" then
        {html|<div class="text-slate-500 dark:text-slate-400 text-sm">이름을 입력해 검색하세요.</div>|html}
      else if candidates = [] then
        {html|<div class="text-slate-500 dark:text-slate-400 text-sm">검색 결과가 없습니다.</div>|html}
      else
        candidates |> List.map (candidate_row ~accent_btn_class ~slot) |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
        <div class="flex items-center justify-between mb-3">
          <div class="text-xs font-bold uppercase tracking-wider text-slate-500 dark:text-slate-400">%s</div>
          <div class="text-[10px] text-slate-600 font-mono">ID 선택</div>
        </div>
        <div class="space-y-1">%s</div>
      </div>|html}
      (escape_html title)
      body
  in
  let error_html =
    match error with
    | None -> ""
    | Some msg ->
        Printf.sprintf
          {html|<div class="bg-rose-500/10 border border-rose-500/30 text-rose-700 dark:text-rose-400 rounded-xl p-5">%s</div>|html}
          (escape_html msg)
  in
  let compare_result_html =
    match p1_selected, p2_selected with
    | Some a, Some b ->
        let h2h_html =
          match h2h_disabled_reason with
          | Some msg ->
              Printf.sprintf
                {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-500 dark:text-slate-400">%s</div>|html}
                (escape_html msg)
          | None ->
              if h2h = [] then
                {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-500 dark:text-slate-400">No match history for this season.</div>|html}
              else
                h2h_game_table (normalize_name a.name) (normalize_name b.name) h2h
        in
        Printf.sprintf
          {html|<div class="space-y-8 animate-fade-in"><div class="grid grid-cols-1 md:grid-cols-3 gap-8 items-start"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-orange-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400"><a href="/player/%s">%s</a></div><div class="text-slate-500 dark:text-slate-400">%s</div>%s</div></div><div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6 space-y-6"><div class="text-center space-y-2"><h3 class="text-slate-500 dark:text-slate-400 text-sm font-bold uppercase">Average Stats</h3><p class="text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed"><span class="font-mono">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 +/-는 문자중계(PBP) 기반으로 일부 경기에서만 제공됩니다. (데이터가 없으면 -)</p></div>%s%s%s%s%s%s%s%s</div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-sky-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-sky-600 dark:text-sky-400"><a href="/player/%s">%s</a></div><div class="text-slate-500 dark:text-slate-400">%s</div>%s</div></div></div>%s</div>|html}
          (player_img_tag ~class_name:"w-32 h-32" a.player_id a.name)
          a.player_id
          (escape_html (normalize_name a.name))
          (escape_html a.team_name)
          (season_badge p1_season)
          (compare_stat_row "Points" a.avg_points b.avg_points)
          (compare_stat_row ~signed:true "MG" a.avg_margin b.avg_margin)
          (compare_stat_row "Rebounds" a.avg_rebounds b.avg_rebounds)
          (compare_stat_row "Assists" a.avg_assists b.avg_assists)
          (compare_stat_row "Steals" a.avg_steals b.avg_steals)
          (compare_stat_row "Blocks" a.avg_blocks b.avg_blocks)
          (compare_stat_row "Turnovers" a.avg_turnovers b.avg_turnovers)
          (compare_stat_row "Efficiency" a.efficiency b.efficiency)
          (player_img_tag ~class_name:"w-32 h-32" b.player_id b.name)
          b.player_id
          (escape_html (normalize_name b.name))
          (escape_html b.team_name)
          (season_badge p2_season)
          h2h_html
    | _ -> ""
  in
  layout ~title:"WKBL Compare"
    ~content:(Printf.sprintf
      {html|<div class="space-y-8">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
          <div>
            <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Compare Players</h2>
            <p class="text-slate-500 dark:text-slate-400 text-sm">동명이인/표기 이슈를 피하기 위해 <span class="font-mono text-slate-900 dark:text-slate-200">player_id</span>를 선택해 비교합니다.</p>
            <p class="text-slate-500 dark:text-slate-400 text-xs mt-1">Player 1/2는 시즌을 각각 선택할 수 있습니다. 시즌이 다르면 Match History는 표시하지 않습니다.</p>
          </div>
        </div>

        <form action="/compare" method="get" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
          <input type="hidden" id="p1_id" name="p1_id" value="%s">
          <input type="hidden" id="p2_id" name="p2_id" value="%s">
          <div class="grid grid-cols-1 md:grid-cols-6 gap-3">
            <select name="p1_season" aria-label="Player 1 season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-2">%s</select>
            <input name="p1" value="%s" placeholder="Player 1 (search name)" oninput="document.getElementById('p1_id').value='';" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-4">
            <select name="p2_season" aria-label="Player 2 season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-2">%s</select>
            <input name="p2" value="%s" placeholder="Player 2 (search name)" oninput="document.getElementById('p2_id').value='';" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-4">
            <div class="md:col-span-6 flex justify-end">
              <button type="submit" class="px-4 py-2 rounded-lg bg-orange-500 text-slate-900 dark:text-slate-200 font-bold hover:bg-orange-400 transition">Search</button>
            </div>
          </div>
        </form>

        %s

        <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
          %s
          %s
        </div>

        %s
      </div>|html}
      (escape_html (Option.value ~default:"" p1_id))
      (escape_html (Option.value ~default:"" p2_id))
      (season_options ~selected:p1_season)
      (escape_html p1_display)
      (season_options ~selected:p2_season)
      (escape_html p2_display)
      error_html
      (match p1_selected with
      | Some p -> selected_card ~accent_border:"border-t-orange-500" ~season_code:p1_season p
      | None -> candidates_panel ~title:"Player 1" ~slot:`P1 ~accent_btn_class:"text-orange-700" p1_display p1_candidates)
      (match p2_selected with
      | Some p -> selected_card ~accent_border:"border-t-sky-500" ~season_code:p2_season p
      | None -> candidates_panel ~title:"Player 2" ~slot:`P2 ~accent_btn_class:"text-sky-700" p2_display p2_candidates)
      compare_result_html)

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
        ("",
          {html|<div>기본 모델(Elo/Pythag(Log5)/Stats)만 사용합니다.</div>
          <div class="text-slate-500 dark:text-slate-400">컨텍스트 옵션을 켜면 “최근 5경기 폼/코어 로스터/휴식”을 Δ로 소폭 반영합니다. (부상/전술/PBP 등은 미반영)</div>|html})
    | Some ctx ->
        let delta_pp = ctx.pcb_delta *. 100.0 in
        let delta_cls =
          if delta_pp > 0.0 then "text-sky-600 dark:text-sky-400"
          else if delta_pp < 0.0 then "text-rose-400"
          else "text-slate-700 dark:text-slate-300"
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
            {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
              <div class="flex items-center justify-between">
                <div class="text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">CONTEXT</div>
                <div class="text-[10px] font-mono %s">%s</div>
              </div>
              <div class="mt-2 space-y-1 text-[10px] text-slate-500 dark:text-slate-400 font-mono">
                <div class="flex justify-between"><span>FORM (L5 win%%)</span><span class="text-slate-700 dark:text-slate-300">%.0f%% vs %.0f%%</span></div>
                <div class="flex justify-between"><span>ROSTER (core)</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
                <div class="flex justify-between"><span>REST</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
              </div>
            </div>|html}
            delta_cls
            (escape_html delta_str)
            form_home_pct
            form_away_pct
            (escape_html roster_text)
            (escape_html rest_text),
          {html|<div>컨텍스트(최근 5경기 폼/코어 로스터/휴식)를 Δ로 소폭 반영합니다. (최대 ±8%p)</div><div class="text-slate-500 dark:text-slate-400">로스터는 “마지막 경기 출전” 기준으로 추정하며, 부상/전술/PBP 컨텍스트는 여전히 미반영입니다.</div>|html}
        )
  in
  let winner_class =
    if normalize_label result.winner = normalize_label home then "text-orange-600 dark:text-orange-400"
    else "text-sky-600 dark:text-sky-400"
  in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl space-y-4">
      <div class="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-4">
        <div class="min-w-0">
          <div class="text-sm text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">Prediction</div>
          <div class="mt-1 text-2xl font-black text-slate-900 dark:text-slate-200">%s</div>
          <div class="mt-1 text-xs text-slate-500 dark:text-slate-400 truncate">%s%s vs %s%s</div>
        </div>
        <div class="text-right shrink-0">
          <div class="text-xs text-slate-500 dark:text-slate-400">Winner</div>
          <div class="text-lg font-black %s">%s</div>
        </div>
      </div>
      <div class="space-y-2">
        <div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-500 dark:text-slate-400">
          <span class="text-slate-700 dark:text-slate-300">%s</span>
          <span class="text-slate-700 dark:text-slate-300">%s</span>
        </div>
        <div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
          <div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div>
          <div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div>
        </div>
        <div class="flex justify-between font-mono text-sm">
          <span class="text-orange-600 dark:text-orange-400 font-bold">%.1f%%</span>
          <span class="text-sky-600 dark:text-sky-400 font-bold">%.1f%%</span>
        </div>
      </div>
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-3 text-xs">
        <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
          <div class="flex items-center justify-between">
            <div class="text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">ELO</div>
            <div class="text-[10px] text-slate-500 dark:text-slate-400 font-mono">%d games</div>
          </div>
          <div class="mt-2 flex items-center justify-between font-mono">
            <div class="text-orange-700">%.0f</div>
            <div class="text-sky-700">%.0f</div>
          </div>
          <div class="mt-1 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
            <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
          </div>
        </div>
        <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
          <div class="text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">PYTHAG</div>
          <div class="mt-2 flex items-center justify-between font-mono">
            <div class="text-orange-700">%.3f</div>
            <div class="text-sky-700">%.3f</div>
          </div>
          <div class="mt-1 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
            <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
          </div>
        </div>
        <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
          <div class="text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">STATS</div>
          <div class="mt-2 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
            <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
          </div>
          <div class="mt-1 text-[10px] text-slate-500 dark:text-slate-400 font-mono">Win%% + EFF blend</div>
        </div>
        %s
      </div>
      <details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-500 dark:text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">예측 안내</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Final</span> = 0.60×Elo + 0.25×Pythag(Log5) + 0.15×Stats.</div>
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Elo</span>는 경기 결과(득점차 반영)로 레이팅을 업데이트합니다.</div>
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Pythag</span>는 득실 기반 기대 승률이며 Log5로 매치업 확률을 계산합니다.</div>
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Stats</span>는 승률과 EFF를 단순 결합합니다.</div>
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

let predict_page ~season ~seasons ~teams ~home ~away ~is_neutral ~context_enabled ~include_mismatch (result: prediction_output option) (error: string option) =
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
          {html|<div class="bg-rose-500/10 border border-rose-500/30 text-rose-700 dark:text-rose-400 rounded-xl p-5">%s</div>|html}
          (escape_html msg)
    | None, None ->
        {html|<div class="text-slate-500 dark:text-slate-400 text-sm">Select teams to see a prediction.</div>|html}
  in
  layout ~title:"WKBL Predict"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
          <div>
            <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Match Prediction</h2>
            <p class="text-slate-500 dark:text-slate-400 text-sm">농구광 모드: Elo + Pythagorean + Stats 근거를 함께 보여줍니다.</p>
          </div>
        </div>
        <form action="/predict" method="get" class="grid grid-cols-1 md:grid-cols-3 gap-3 bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
          <select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <select name="home" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <select name="away" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-500 dark:text-slate-400">
            <input type="checkbox" name="context" value="1" class="accent-orange-500" %s>
            컨텍스트 반영 (폼/로스터/휴식)
          </label>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-500 dark:text-slate-400">
            <input type="checkbox" name="neutral" value="1" class="accent-orange-500" %s>
            Neutral site (no home advantage)
          </label>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-500 dark:text-slate-400">
            <input type="checkbox" name="include_mismatch" value="1" class="accent-orange-500" %s>
            Mismatch 포함 (스코어 불일치 경기 포함)
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
      (if include_mismatch then "checked" else "")
      result_html)

let leader_card ?(value_fmt=(fun v -> Printf.sprintf "%.1f" v)) title (leaders: leader_entry list) =
  if leaders = [] then ""
  else
    let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
    leaders
    |> List.iter (fun (l: leader_entry) ->
        let key = normalize_name l.le_player_name in
        let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
        Hashtbl.replace name_counts key (prev + 1));
    let show_id (l: leader_entry) =
      match Hashtbl.find_opt name_counts (normalize_name l.le_player_name) with
      | Some c when c > 1 -> true
      | _ -> false
    in
    let top = List.hd leaders in
    let top_id_badge =
      if show_id top then Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge top.le_player_id) else ""
    in
    let others = List.tl leaders in
    let others_rows =
      others
      |> List.mapi (fun i l ->
          let id_badge = if show_id l then Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge l.le_player_id) else "" in
          Printf.sprintf {html|<div class="flex items-center justify-between py-2 border-b border-slate-200 dark:border-slate-800/60 last:border-0"><div class="flex items-center gap-3"><span class="text-slate-500 dark:text-slate-400 font-mono text-sm w-4">%d</span>%s<div class="flex flex-col"><div class="text-sm font-medium text-slate-700 dark:text-slate-300"><a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s</div><div class="text-xs text-slate-500 dark:text-slate-400">%s</div></div></div><div class="font-mono font-bold text-slate-500 dark:text-slate-400">%s</div></div>|html} (i + 2) (player_img_tag ~class_name:"w-8 h-8" l.le_player_id l.le_player_name) l.le_player_id (escape_html (normalize_name l.le_player_name)) id_badge (escape_html l.le_team_name) (escape_html (value_fmt l.le_stat_value)))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">%s</h3><div class="flex items-center gap-4 mb-6 pb-6 border-b border-slate-200 dark:border-slate-800"><div class="relative">%s<div class="absolute -top-2 -right-2 bg-orange-500 text-slate-900 dark:text-slate-200 text-xs font-bold w-6 h-6 rounded-full flex items-center justify-center border-2 border-slate-900">1</div></div><div><div class="text-3xl font-black text-slate-900 dark:text-slate-200">%s</div><div class="font-bold text-orange-600 dark:text-orange-400 flex items-center flex-wrap"><a href="/player/%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition-colors">%s</a>%s</div><div class="text-xs text-slate-500 dark:text-slate-400">%s</div></div></div><div class="space-y-1">%s</div></div>|html} (escape_html title) (player_img_tag ~class_name:"w-16 h-16" top.le_player_id top.le_player_name) (escape_html (value_fmt top.le_stat_value)) top.le_player_id (escape_html (normalize_name top.le_player_name)) top_id_badge (escape_html top.le_team_name) others_rows

let leader_card_signed title (leaders: leader_entry list) =
  if leaders = [] then ""
  else
    let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
    leaders
    |> List.iter (fun (l: leader_entry) ->
        let key = normalize_name l.le_player_name in
        let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
        Hashtbl.replace name_counts key (prev + 1));
    let show_id (l: leader_entry) =
      match Hashtbl.find_opt name_counts (normalize_name l.le_player_name) with
      | Some c when c > 1 -> true
      | _ -> false
    in
    let signed v = if v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v in
    let top = List.hd leaders in
    let top_id_badge =
      if show_id top then Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge top.le_player_id) else ""
    in
    let others = List.tl leaders in
    let others_rows =
      others
      |> List.mapi (fun i l ->
          let id_badge = if show_id l then Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge l.le_player_id) else "" in
          Printf.sprintf {html|<div class="flex items-center justify-between py-2 border-b border-slate-200 dark:border-slate-800/60 last:border-0"><div class="flex items-center gap-3"><span class="text-slate-500 dark:text-slate-400 font-mono text-sm w-4">%d</span>%s<div class="flex flex-col"><div class="text-sm font-medium text-slate-700 dark:text-slate-300"><a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s</div><div class="text-xs text-slate-500 dark:text-slate-400">%s</div></div></div><div class="font-mono font-bold text-slate-500 dark:text-slate-400">%s</div></div>|html} (i + 2) (player_img_tag ~class_name:"w-8 h-8" l.le_player_id l.le_player_name) l.le_player_id (escape_html (normalize_name l.le_player_name)) id_badge (escape_html l.le_team_name) (escape_html (signed l.le_stat_value)))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">%s</h3><div class="flex items-center gap-4 mb-6 pb-6 border-b border-slate-200 dark:border-slate-800"><div class="relative">%s<div class="absolute -top-2 -right-2 bg-orange-500 text-slate-900 dark:text-slate-200 text-xs font-bold w-6 h-6 rounded-full flex items-center justify-center border-2 border-slate-900">1</div></div><div><div class="text-3xl font-black text-slate-900 dark:text-slate-200">%s</div><div class="font-bold text-orange-600 dark:text-orange-400 flex items-center flex-wrap"><a href="/player/%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition-colors">%s</a>%s</div><div class="text-xs text-slate-500 dark:text-slate-400">%s</div></div></div><div class="space-y-1">%s</div></div>|html}
      (escape_html title)
      (player_img_tag ~class_name:"w-16 h-16" top.le_player_id top.le_player_name)
      (escape_html (signed top.le_stat_value))
      top.le_player_id
      (escape_html (normalize_name top.le_player_name))
      top_id_badge
      (escape_html top.le_team_name)
      others_rows

let leaders_page ~season ~seasons ~scope (leaders_by_category: (string * leader_entry list) list) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<option value="ALL" %s>All Seasons</option>%s|html}
      (if season = "ALL" then "selected" else "")
      base
  in
  let scope_value = scope |> String.trim |> String.lowercase_ascii in
  let scope_options =
    let opt v label =
      let sel = if scope_value = v then "selected" else "" in
      Printf.sprintf {html|<option value="%s" %s>%s</option>|html} v sel label
    in
    opt "per_game" "Per Game" ^ opt "totals" "Totals" ^ opt "per_36" "Per 36"
  in
  let lookup category =
    leaders_by_category
    |> List.find_opt (fun (k, _) -> k = category)
    |> Option.map snd
    |> Option.value ~default:[]
  in
  let fmt_int v = Printf.sprintf "%.0f" v in
  let fmt_f1 v = Printf.sprintf "%.1f" v in
  let fmt_f3 v = Printf.sprintf "%.3f" v in
  let main_categories, shooting_categories =
    match scope_value with
    | "totals" ->
        ( [ ("Games", "gp", fmt_int)
          ; ("Minutes", "min", fmt_f1)
          ; ("Points", "pts", fmt_int)
          ; ("Rebounds", "reb", fmt_int)
          ; ("Assists", "ast", fmt_int)
          ; ("Steals", "stl", fmt_int)
          ; ("Blocks", "blk", fmt_int)
          ; ("Turnovers", "tov", fmt_int)
          ]
        , [ ("FG%", "fg_pct", fmt_f3)
          ; ("3P%", "fg3_pct", fmt_f3)
          ; ("FT%", "ft_pct", fmt_f3)
          ; ("TS%", "ts_pct", fmt_f3)
          ; ("eFG%", "efg_pct", fmt_f3)
          ]
        )
    | "per_36" ->
        ( [ ("PTS/36", "pts", fmt_f1)
          ; ("REB/36", "reb", fmt_f1)
          ; ("AST/36", "ast", fmt_f1)
          ; ("STL/36", "stl", fmt_f1)
          ; ("BLK/36", "blk", fmt_f1)
          ; ("TOV/36", "tov", fmt_f1)
          ; ("EFF/36", "eff", fmt_f1)
          ]
        , [ ("FG%", "fg_pct", fmt_f3)
          ; ("3P%", "fg3_pct", fmt_f3)
          ; ("FT%", "ft_pct", fmt_f3)
          ; ("TS%", "ts_pct", fmt_f3)
          ; ("eFG%", "efg_pct", fmt_f3)
          ]
        )
    | _ ->
        ( [ ("Points", "pts", fmt_f1)
          ; ("Rebounds", "reb", fmt_f1)
          ; ("Assists", "ast", fmt_f1)
          ; ("Steals", "stl", fmt_f1)
          ; ("Blocks", "blk", fmt_f1)
          ; ("Turnovers", "tov", fmt_f1)
          ; ("Minutes", "min", fmt_f1)
          ; ("Efficiency", "eff", fmt_f1)
          ]
        , [ ("FG%", "fg_pct", fmt_f3)
          ; ("3P%", "fg3_pct", fmt_f3)
          ; ("FT%", "ft_pct", fmt_f3)
          ; ("TS%", "ts_pct", fmt_f3)
          ; ("eFG%", "efg_pct", fmt_f3)
          ]
        )
  in
  let render_cards categories =
    categories
    |> List.map (fun (title, key, fmt) ->
        leader_card ~value_fmt:fmt title (lookup key))
    |> String.concat ""
  in
  let main_grid =
    Printf.sprintf
      {html|<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6">%s</div>|html}
      (render_cards main_categories)
  in
  let shooting_grid =
    Printf.sprintf
      {html|<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6">%s</div>|html}
      (render_cards shooting_categories)
  in
  let note_html =
    {html|<div class="text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">Shooting leaders require minimum attempts: <span class="font-mono text-slate-700 dark:text-slate-300">FG≥50</span>, <span class="font-mono text-slate-700 dark:text-slate-300">3P≥20</span>, <span class="font-mono text-slate-700 dark:text-slate-300">FT≥20</span>. Per-36 leaders require <span class="font-mono text-slate-700 dark:text-slate-300">MIN≥100</span>.</div>|html}
  in
  let content =
    Printf.sprintf
      {html|<div class="space-y-8">%s%s<div class="flex items-baseline justify-between"><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">Shooting</h3><div class="text-xs text-slate-500 dark:text-slate-400">FG / 3P / FT / TS / eFG</div></div>%s</div>|html}
      main_grid note_html shooting_grid
  in
  layout
    ~title:"WKBL Leaders"
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">League Leaders</h2><p class="text-slate-500 dark:text-slate-400">Basketball-reference style leaderboards.</p></div><form action="/leaders" method="get" class="grid grid-cols-1 sm:grid-cols-2 gap-3 w-full md:w-auto"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select><select name="scope" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select></form></div>%s</div>|html}
      season_options scope_options content)

let awards_page ~(season: string) ~(seasons: season_info list) ~(include_mismatch: bool) ~(prev_season_name: string option) ~(mvp: leader_entry list) ~(mip: leader_entry list) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let include_checked = if include_mismatch then "checked" else "" in
  let mvp_card =
    if mvp = [] then
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MVP (EFF)</h3><div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">표시할 데이터가 없습니다.</div></div>|html}
    else
      leader_card "MVP (EFF)" mvp
  in
  let mip_card =
    match prev_season_name with
    | None ->
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MIP (ΔEFF)</h3><div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">EFF(AVG(game_score))</span> 변화(Δ)를 계산합니다. 시즌 선택 시에만 표시됩니다.</div></div>|html}
    | Some prev_name ->
        if mip = [] then
          Printf.sprintf
            {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MIP (ΔEFF)</h3><div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">%s 대비 ΔEFF 계산에 필요한 표본이 부족합니다. (두 시즌 모두 <span class="font-mono text-slate-700 dark:text-slate-300">GP≥10</span>)</div></div>|html}
            (escape_html prev_name)
        else
          leader_card_signed (Printf.sprintf "MIP (ΔEFF vs %s)" prev_name) mip
  in
  let disclaimer_html =
    {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/60 p-5 text-slate-500 dark:text-slate-400 text-sm leading-relaxed"><div class="font-bold text-slate-700 dark:text-slate-300 mb-2">Stat Awards (unofficial)</div><ul class="list-disc list-inside space-y-1"><li>MVP: <span class="font-mono text-slate-700 dark:text-slate-300">EFF = AVG(game_score)</span> 상위</li><li>MIP: 전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">ΔEFF</span> 상위 (두 시즌 모두 <span class="font-mono text-slate-700 dark:text-slate-300">GP≥10</span>)</li><li>공식 수상 데이터가 DB에 없어서, 현재는 박스스코어 기반 지표로만 추정합니다.</li></ul></div>|html}
  in
  layout
    ~title:"WKBL Awards"
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Awards</h2><p class="text-slate-500 dark:text-slate-400">Stat-based awards for quick reference.</p></div><form action="/awards" method="get" class="flex flex-wrap items-center gap-3"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-40" onchange="this.form.submit()">%s</select><label class="flex items-center gap-2 text-xs text-slate-500 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" onchange="this.form.submit()" title="Final score != sum(points) 경기 포함"><span>Mismatch 포함</span></label></form></div><div class="grid grid-cols-1 md:grid-cols-2 gap-6">%s%s</div>%s</div>|html}
      season_options
      include_checked
      mvp_card
      mip_card
      disclaimer_html)

let player_profile_page ?(leaderboards=None) (profile: player_profile) ~scope ~(seasons_catalog: season_info list) =
  let p = profile.player in
  let pos = match p.position with Some s -> s | None -> "-" in
  let height_text =
    match p.height with
    | Some h when h > 0 -> Printf.sprintf "%dcm" h
    | _ -> "-"
  in
  let info_text = Printf.sprintf "%s | %s" pos height_text in
  let birth_text =
    match p.birth_date with
    | Some d when String.trim d <> "" -> d
    | _ -> "-"
  in
  let avg = profile.averages in
  let team_color =
    team_code_of_string avg.team_name
    |> Option.map team_code_to_color
    |> Option.value ~default:"#64748b"
  in
  let career_chips =
    if avg.games_played <= 0 then ""
    else
      Printf.sprintf
        {html|<span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">GP %d</span><span class="bg-orange-500/20 backdrop-blur-md text-orange-700 dark:text-orange-400 border border-orange-500/30 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm font-bold">PTS %.1f</span><span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">TOT %s</span><span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">REB %.1f</span><span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">AST %.1f</span>|html}
        avg.games_played
        avg.avg_points
        (format_int_commas avg.total_points)
        avg.avg_rebounds
        avg.avg_assists
  in
  let team_badge_html =
    let t = String.trim avg.team_name in
    if t = "" then "" else team_badge t
  in
  let video_links_html =
    let find_external_link link_type =
      let key = String.lowercase_ascii (String.trim link_type) in
      profile.external_links
      |> List.find_opt (fun (l: player_external_link) ->
          String.lowercase_ascii (String.trim l.pel_link_type) = key)
    in
    let link_button ~label ~url ?(title="") () =
      let title_attr = if title = "" then "" else Printf.sprintf {html| title="%s"|html} (escape_html title) in
      Printf.sprintf
        {html|<a href="%s" target="_blank" rel="noopener noreferrer" class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-lg text-sm font-mono hover:bg-white/40 dark:hover:bg-slate-700/60 transition-all shadow-sm"%s>%s</a>|html}
        (escape_html url)
        title_attr
        label
    in
    let source_chip ~source_url =
      Printf.sprintf
        {html|<a href="%s" target="_blank" rel="noopener noreferrer" class="bg-white/10 dark:bg-slate-800/20 backdrop-blur-sm text-slate-500 dark:text-slate-400 px-2 py-1 rounded text-xs font-mono hover:text-slate-900 dark:hover:text-white transition" title="Source">src</a>|html}
        (escape_html source_url)
    in
    let name = normalize_label p.name in
    let team = String.trim avg.team_name |> normalize_label in
    let base_query = if team = "" then name else Printf.sprintf "%s %s" name team in
    let wkbl_profile =
      "https://www.wkbl.or.kr/player/detail2.asp?pno=" ^ Uri.pct_encode p.id
    in
    let youtube_search =
      "https://www.youtube.com/results?search_query="
      ^ Uri.pct_encode (Printf.sprintf "%s WKBL 하이라이트" base_query)
    in
    let instagram_search =
      "https://www.google.com/search?q="
      ^ Uri.pct_encode (Printf.sprintf "site:instagram.com %s WKBL 인스타" base_query)
    in
    let youtube_html =
      match find_external_link "youtube" with
      | None ->
          link_button ~label:"▶ 검색" ~url:youtube_search ~title:"YouTube search" ()
      | Some l ->
          let title =
            match l.pel_source_url with
            | None -> "YouTube (verified link)"
            | Some src -> "YouTube (source: " ^ src ^ ")"
          in
          let src_html =
            match l.pel_source_url with
            | Some src when String.trim src <> "" && src <> l.pel_url -> source_chip ~source_url:src
            | _ -> ""
          in
          Printf.sprintf {html|%s%s|html} (link_button ~label:"▶ 영상" ~url:l.pel_url ~title ()) src_html
    in
    let instagram_html =
      match find_external_link "instagram" with
      | None ->
          link_button ~label:"인스타 검색" ~url:instagram_search ~title:"Instagram search" ()
      | Some l ->
          let title =
            match l.pel_source_url with
            | None -> "Instagram (verified link)"
            | Some src -> "Instagram (source: " ^ src ^ ")"
          in
          let src_html =
            match l.pel_source_url with
            | Some src when String.trim src <> "" && src <> l.pel_url -> source_chip ~source_url:src
            | _ -> ""
          in
          Printf.sprintf {html|%s%s|html} (link_button ~label:"인스타" ~url:l.pel_url ~title ()) src_html
    in
    Printf.sprintf
      {html|%s%s%s|html}
      (link_button ~label:"WKBL" ~url:wkbl_profile ~title:"Official profile" ())
      youtube_html
      instagram_html
  in
  let game_rows games =
    games
    |> List.map (fun (g: player_game_stat) ->
        let res_color = if g.pts >= 20 then "text-orange-600 dark:text-orange-400" else "text-slate-700 dark:text-slate-300" in
        let pm_class, pm_str =
          match g.plus_minus, g.team_score, g.opponent_score with
          | Some v, _, _ ->
              let cls =
                if v > 0 then "text-sky-600 dark:text-sky-400"
                else if v < 0 then "text-rose-600 dark:text-rose-400"
                else "text-slate-500 dark:text-slate-500"
              in
              let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
              (cls, s)
          | None, Some team_score, Some opponent_score ->
              let margin = team_score - opponent_score in
              let cls =
                if margin > 0 then "text-sky-600 dark:text-sky-400"
                else if margin < 0 then "text-rose-600 dark:text-rose-400"
                else "text-slate-500 dark:text-slate-500"
              in
              let s =
                if margin > 0 then Printf.sprintf "M +%d" margin
                else Printf.sprintf "M %d" margin
              in
              (cls, s)
          | None, _, _ -> ("text-slate-500 dark:text-slate-500", "-")
        in
        let margin_badge =
          match g.team_score, g.opponent_score with
          | Some team_score, Some opponent_score ->
              let margin = team_score - opponent_score in
              let cls =
                if margin > 0 then "bg-sky-500/10 text-sky-600 dark:text-sky-400 border-sky-500/30"
                else if margin < 0 then "bg-rose-500/10 text-rose-600 dark:text-rose-400 border-rose-500/30"
                else "bg-slate-500/10 text-slate-700 dark:text-slate-300 border-slate-500/30"
              in
              let label =
                if margin > 0 then Printf.sprintf "W +%d" margin
                else if margin < 0 then Printf.sprintf "L %d" margin
                else "T 0"
              in
              Printf.sprintf
                {html|<span class="inline-flex items-center justify-center px-2 py-0.5 rounded border text-[10px] font-mono min-w-[52px] %s">%s</span>|html}
                cls label
          | _ ->
              {html|<span class="inline-flex items-center justify-center px-2 py-0.5 rounded border border-slate-200 dark:border-slate-700/60 text-[10px] font-mono text-slate-500 dark:text-slate-500 min-w-[52px]">-</span>|html}
        in
        let quality_badge = score_quality_badge ~compact:true g.score_quality in
        let opponent_label = if g.is_home then "vs " ^ g.opponent else "@ " ^ g.opponent in
        let opponent_href = "/team/" ^ Uri.pct_encode g.opponent in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-[90px] sm:w-[110px]"><a href="/boxscore/%s" class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-slate-900 dark:text-white"><div class="flex flex-wrap items-center gap-x-3 gap-y-2"><a href="%s" class="player-name min-w-0 flex-1 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a><div class="flex items-center gap-2 shrink-0">%s%s</div></div></td><td class="px-4 py-3 text-right font-mono text-slate-500 dark:text-slate-400 w-[60px] sm:w-[72px] whitespace-nowrap">%.1f</td><td class="px-4 py-3 text-right font-bold %s w-[60px] sm:w-[72px] whitespace-nowrap">%d</td><td class="px-4 py-3 text-right font-mono w-[60px] sm:w-[72px] whitespace-nowrap %s">%s</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] whitespace-nowrap">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td></tr>|html}
          (Uri.pct_encode g.game_id)
          (escape_html g.game_date)
          (escape_html opponent_href)
          (escape_html opponent_label)
          margin_badge
          quality_badge
          g.min
          res_color
          g.pts
          pm_class
          (escape_html pm_str)
          g.reb
          g.ast
          g.stl)
    |> String.concat "\n"
  in
  let birth_chip =
    Printf.sprintf
      {html|<span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-2.5 py-1 rounded-lg text-[11px] sm:text-sm shadow-sm"><span class="text-slate-600 dark:text-slate-500">생년</span> %s</span>|html}
      (escape_html birth_text)
  in
  let recent_rows = game_rows profile.recent_games in
  let all_star_rows = game_rows profile.all_star_games in
  let season_stats_component = player_season_stats_component ~player_id:p.id ~scope profile.season_breakdown in
  let rec take n xs =
    match n, xs with
    | n, _ when n <= 0 -> []
    | _, [] -> []
    | n, x :: rest -> x :: take (n - 1) rest
  in
  let recent_wl_summary_html =
    let games = take 5 profile.recent_games in
    let n = List.length games in
    if n = 0 then
      ""
    else
      let chips_rev, wins, losses, unknown =
        games
        |> List.fold_left
          (fun (chips, w, l, u) (g: player_game_stat) ->
            match g.team_score, g.opponent_score with
            | Some team_score, Some opp_score ->
                let margin = team_score - opp_score in
                let label, cls, w', l' =
                  if margin > 0 then ("W", "bg-sky-500/10 border-sky-500/30 text-sky-600 dark:text-sky-300", w + 1, l)
                  else if margin < 0 then ("L", "bg-rose-500/10 border-rose-500/30 text-rose-600 dark:text-rose-300", w, l + 1)
                  else ("T", "bg-slate-100 dark:bg-slate-800/60 border-slate-200 dark:border-slate-700/60 text-slate-700 dark:text-slate-300", w, l)
                in
                let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin in
                let title = Printf.sprintf "%s %s (%s)" label margin_str g.game_date in
                let chip =
                  Printf.sprintf
                    {html|<span title="%s" class="inline-flex items-center justify-center w-6 h-6 rounded border %s text-[11px] font-mono shadow-sm">%s</span>|html}
                    (escape_html title)
                    cls
                    label
                in
                (chip :: chips, w', l', u)
            | _ ->
                let chip =
                  {html|<span title="Score unavailable" class="inline-flex items-center justify-center w-6 h-6 rounded border bg-slate-100 dark:bg-slate-800/40 border-slate-200 dark:border-slate-700/40 text-[11px] font-mono text-slate-400 dark:text-slate-500 shadow-sm">?</span>|html}
                in
                (chip :: chips, w, l, u + 1))
          ([], 0, 0, 0)
      in
      let record =
        let base =
          if wins + losses = 0 then "-"
          else Printf.sprintf "%d-%d" wins losses
        in
        if unknown > 0 then Printf.sprintf "%s (%d ?)" base unknown else base
      in
      let label = Printf.sprintf "최근 %d경기 %s" n record in
      let chips_html = chips_rev |> List.rev |> String.concat "" in
      Printf.sprintf
        {html|<div class="flex items-center gap-2"><span class="text-[11px] text-slate-500 dark:text-slate-500 font-mono whitespace-nowrap">%s</span><div class="flex items-center gap-1">%s</div></div>|html}
        (escape_html label)
        chips_html
  in
  let recent_games_header_html =
    Printf.sprintf
      {html|<div class="flex items-start justify-between gap-3"><h3 class="text-xl font-bold text-slate-900 dark:text-white">Recent Games</h3><div class="flex flex-wrap items-center justify-end gap-2 shrink-0">%s<a href="/player/%s/games" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">전체 경기</a></div></div><p class="text-[11px] text-slate-500 dark:text-slate-500 mt-1">개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계(PBP) 기반입니다. PBP가 없으면 <span class="font-mono text-slate-700 dark:text-slate-300">M</span>으로 팀 득실마진(경기 최종 점수)을 대신 표시합니다. <span class="font-mono text-slate-700 dark:text-slate-300">Σ</span>는 득점합 기반 보정 결과입니다.</p>|html}
      recent_wl_summary_html
      (Uri.pct_encode p.id)
  in
  let all_star_section_html =
    if profile.all_star_games = [] then
      ""
    else
      Printf.sprintf
        {html|<div class="space-y-4"><div class="flex flex-col gap-1"><h3 class="text-xl font-bold text-slate-900 dark:text-white">All-Star Games</h3><p class="text-[11px] text-slate-500 dark:text-slate-500">올스타전(<span class="font-mono text-slate-700 dark:text-slate-300">game_type=10</span>) 기록은 시즌/커리어 테이블에서 제외하고 여기서만 보여줍니다.</p></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg"><table class="min-w-[680px] sm:min-w-[920px] w-full text-sm font-mono table-auto"><thead class="bg-slate-100/80 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-xs whitespace-nowrap"><tr><th class="px-4 py-3 text-left font-sans w-[90px] sm:w-[110px]">Date</th><th class="px-4 py-3 text-left font-sans">Opponent</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">MIN</th><th class="px-4 py-3 text-right text-orange-600 dark:text-orange-400 w-[60px] sm:w-[72px]">PTS</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">+/-</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">REB</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">AST</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">STL</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">BLK</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
        all_star_rows
  in

  (* TODO: Restore team_movement_html from commit 6496a34 - lost during merge 106e40a *)
  let team_movement_html = "" in

  let display_name = normalize_name p.name in
  layout ~title:(display_name ^ " | WKBL Profile")
    ~content:(Printf.sprintf {html|<div class="space-y-6 sm:space-y-8 animate-fade-in">
      <!-- 히어로 섹션 개편 -->
      <div class="relative overflow-hidden rounded-3xl shadow-2xl border border-white/20 dark:border-slate-800/50">
        <!-- 팀 컬러 그라데이션 배경 -->
        <div class="absolute inset-0 opacity-80 dark:opacity-60" style="background: linear-gradient(135deg, %s 0%%, #000 150%%)"></div>
        <div class="absolute inset-0 bg-grid-white/[0.05] bg-[center_top_-1px]"></div>
        
        <div class="relative p-6 sm:p-10 flex flex-col md:flex-row items-center md:items-end gap-8">
          <div class="relative group">
            <div class="absolute -inset-1 bg-white/30 rounded-full blur opacity-25 group-hover:opacity-50 transition duration-1000 group-hover:duration-200"></div>
            %s
            <div class="absolute -bottom-2 -right-2 bg-white dark:bg-slate-800 border-4 border-slate-50 dark:border-[#0b0e14] text-slate-900 dark:text-white text-[11px] sm:text-xs font-black px-3 py-1 rounded-full shadow-lg transform rotate-3">#%s</div>
          </div>
          
          <div class="text-center md:text-left space-y-4 flex-1">
            <div class="space-y-1">
              <h1 class="text-4xl sm:text-6xl font-black text-white tracking-tighter drop-shadow-lg">%s</h1>
              <div class="text-white/80 text-lg sm:text-xl font-medium tracking-wide flex items-center justify-center md:justify-start gap-3">
                %s <span class="w-1.5 h-1.5 rounded-full bg-white/40"></span> %s
              </div>
            </div>
            
            <div class="flex flex-wrap gap-2.5 justify-center md:justify-start pt-2">
              %s
              %s
              %s
            </div>
            
            <div class="flex flex-wrap gap-3 justify-center md:justify-start pt-4 border-t border-white/10">
              %s
            </div>
          </div>
        </div>
      </div>

      <div class="grid grid-cols-1 lg:grid-cols-6 gap-6 sm:gap-8">
        <div class="lg:col-span-4 space-y-6 sm:space-y-8">
          %s
          <div class="space-y-4">
            %s
            <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
              <table class="min-w-[680px] sm:min-w-[920px] w-full text-xs sm:text-sm font-mono table-auto tabular-nums">
                <thead class="bg-slate-100/80 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap">
                  <tr>
                    <th class="px-4 py-3 text-left font-sans w-[90px] sm:w-[110px]">Date</th>
                    <th class="px-4 py-3 text-left font-sans">Opponent</th>
                    <th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">MIN</th>
                    <th class="px-4 py-3 text-right text-orange-600 dark:text-orange-400 w-[60px] sm:w-[72px]">PTS</th>
                    <th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">+/-</th>
                    <th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">REB</th>
                    <th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">AST</th>
                    <th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">STL</th>
                    <th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">BLK</th>
                  </tr>
                </thead>
                <tbody>%s</tbody>
              </table>
            </div>
          </div>
          %s
        </div>
        <div class="space-y-6 sm:space-y-8 lg:col-span-2">
          <div class="space-y-4">
            %s
            %s
            %s
            %s
          </div>
        </div>
      </div>
      %s
    </div>|html}
          team_color
          (player_img_tag ~class_name:"w-32 h-32 sm:w-48 sm:h-48 border-[6px] border-white/20 dark:border-slate-700/30 shadow-2xl transition-transform duration-500 group-hover:scale-105" p.id p.name)
          (escape_html p.id)
          (escape_html display_name)
          info_text
          birth_chip
          team_badge_html
          video_links_html
          career_chips
          season_stats_component
          recent_games_header_html
          recent_rows
          all_star_section_html
          team_movement_html
          leaderboards_html
          (career_highs_card profile.career_highs)
          missing_data_html
          data_notes_html)
let player_game_logs_page (profile: player_profile) ~(season: string) ~(seasons: season_info list) ~(include_mismatch: bool) (games: player_game_stat list) =
  let p = profile.player in
  let display_name = normalize_name p.name in
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let include_checked = if include_mismatch then "checked" else "" in
  let verified_cnt, derived_cnt, mismatch_cnt =
    games
    |> List.fold_left
      (fun (v, d, m) (g: player_game_stat) ->
        match g.score_quality with
        | Verified -> (v + 1, d, m)
        | Derived -> (v, d + 1, m)
        | Mismatch -> (v, d, m + 1))
      (0, 0, 0)
  in
    let quality_chips =
      Printf.sprintf
      {html|<div class="flex flex-wrap items-center gap-2 text-[11px] font-mono text-slate-500 dark:text-slate-400"><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">✓ %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">Σ %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">! %d</span></div>|html}
      verified_cnt derived_cnt mismatch_cnt
  in
  let rows =
    if games = [] then
      {html|<tr><td colspan="9" class="px-4 py-10 text-center text-slate-500 dark:text-slate-400 text-sm">No games found.</td></tr>|html}
    else
      games
      |> List.map (fun (g: player_game_stat) ->
          let res_color = if g.pts >= 20 then "text-orange-600 dark:text-orange-400" else "text-slate-700 dark:text-slate-300" in
          let pm_class, pm_str =
            match g.plus_minus, g.team_score, g.opponent_score with
            | Some v, _, _ ->
                let cls =
                  if v > 0 then "text-sky-600 dark:text-sky-400"
                  else if v < 0 then "text-rose-400"
                  else "text-slate-500 dark:text-slate-400"
                in
                let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
                (cls, s)
            | None, Some team_score, Some opponent_score ->
                let margin = team_score - opponent_score in
                let cls =
                  if margin > 0 then "text-sky-600 dark:text-sky-400"
                  else if margin < 0 then "text-rose-400"
                  else "text-slate-500 dark:text-slate-400"
                in
                let s =
                  if margin > 0 then Printf.sprintf "M +%d" margin
                  else Printf.sprintf "M %d" margin
                in
                (cls, s)
            | None, _, _ -> ("text-slate-500 dark:text-slate-400", "-")
          in
          let margin_badge =
            match g.team_score, g.opponent_score with
            | Some team_score, Some opponent_score ->
                let margin = team_score - opponent_score in
                let cls =
                  if margin > 0 then "bg-sky-500/10 text-sky-600 dark:text-sky-400 border-sky-500/30"
                  else if margin < 0 then "bg-rose-500/10 text-rose-400 border-rose-500/30"
                  else "bg-slate-50 dark:bg-[#0b0e14]0/10 text-slate-700 dark:text-slate-300 border-slate-500/30"
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
                {html|<span class="inline-flex items-center px-2 py-0.5 rounded border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-500 dark:text-slate-400">-</span>|html}
          in
          let quality_badge = score_quality_badge ~compact:true g.score_quality in
          let opponent_label = if g.is_home then "vs " ^ g.opponent else "@ " ^ g.opponent in
          Printf.sprintf
            {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-[90px] sm:w-[110px]"><a href="/boxscore/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a></td><td class="px-4 py-3 text-slate-900 dark:text-slate-200"><div class="flex items-center justify-between gap-3"><span class="player-name truncate">%s</span><div class="flex items-center gap-2 shrink-0">%s%s</div></div></td><td class="px-4 py-3 text-right font-mono text-slate-500 dark:text-slate-400 w-[60px] sm:w-[72px] hidden sm:table-cell">%.1f</td><td class="px-4 py-3 text-right font-bold %s w-[60px] sm:w-[72px]">%d</td><td class="px-4 py-3 text-right font-mono w-[60px] sm:w-[72px] %s">%s</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px]">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-4 py-3 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td></tr>|html}
            (Uri.pct_encode g.game_id)
            (escape_html g.game_date)
            (escape_html opponent_label)
            margin_badge
            quality_badge
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
  layout ~title:(display_name ^ " | Game Log")
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-4"><div class="flex items-center gap-4"><div class="shrink-0">%s</div><div class="min-w-0"><div class="text-sm text-slate-500 dark:text-slate-400"><a href="/player/%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">← Profile</a></div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200 truncate">%s <span class="text-slate-500 dark:text-slate-400 text-lg font-mono">Game Log</span></h2><div class="mt-1 text-slate-500 dark:text-slate-400 text-sm">총 %d경기</div></div></div><div class="flex flex-col items-start sm:items-end gap-3"><form action="/player/%s/games" method="get" class="flex flex-wrap items-center justify-end gap-2"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none" onchange="this.form.submit()">%s</select><label class="flex items-center gap-2 text-xs text-slate-500 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" onchange="this.form.submit()"><span>Mismatch 포함</span></label></form>%s</div></div><p class="text-[11px] text-slate-500 dark:text-slate-400">개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계(PBP) 기반입니다. PBP가 없으면 <span class="font-mono text-slate-700 dark:text-slate-300">M</span>으로 팀 득실마진(경기 최종 점수)을 대신 표시합니다. (데이터가 없거나 품질 이슈면 <span class="font-mono text-slate-700 dark:text-slate-300">-</span>)</p><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg"><table class="min-w-[680px] sm:min-w-[920px] w-full text-sm font-mono table-auto"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-xs"><tr><th class="px-4 py-3 text-left font-sans w-[90px] sm:w-[110px] whitespace-nowrap">Date</th><th class="px-4 py-3 text-left font-sans">Opponent</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">MIN</th><th class="px-4 py-3 text-right text-orange-600 dark:text-orange-400 w-[60px] sm:w-[72px]">PTS</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">+/-</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px]">REB</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">AST</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">STL</th><th class="px-4 py-3 text-right w-[60px] sm:w-[72px] hidden sm:table-cell">BLK</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (player_img_tag ~class_name:"w-14 h-14 border border-slate-300 dark:border-slate-700 shadow-lg" p.id p.name)
      (Uri.pct_encode p.id)
      (escape_html display_name)
      (List.length games)
      (Uri.pct_encode p.id)
      season_options
      include_checked
      quality_chips
      rows)

let team_profile_page (detail: team_full_detail) ~season ~seasons =
  let t = detail.tfd_team_name in
  let s = detail.tfd_standing in
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let standing_info = match s with | Some st -> Printf.sprintf {html|<div class="flex gap-6 text-sm"><div class="flex flex-col"><span>WINS</span><span class="text-2xl font-black text-slate-900 dark:text-slate-200">%d</span></div><div class="flex flex-col"><span>LOSSES</span><span class="text-2xl font-black text-slate-900 dark:text-slate-200">%d</span></div><div class="flex flex-col"><span>WIN %%</span><span class="text-2xl font-black text-orange-600 dark:text-orange-400">%.3f</span></div><div class="flex flex-col"><span>GB</span><span class="text-2xl font-black text-slate-500 dark:text-slate-400">%.1f</span></div></div>|html} st.wins st.losses st.win_pct st.gb | None -> "" in
  let roster_name_counts : (string, int) Hashtbl.t = Hashtbl.create 32 in
  detail.tfd_roster
  |> List.iter (fun (p: player_aggregate) ->
      let key = normalize_name p.name in
      let prev = Hashtbl.find_opt roster_name_counts key |> Option.value ~default:0 in
      Hashtbl.replace roster_name_counts key (prev + 1));
  let roster_rows =
    detail.tfd_roster
    |> List.mapi (fun i (p: player_aggregate) ->
        let key = normalize_name p.name in
        let show_player_id =
          match Hashtbl.find_opt roster_name_counts key with
          | Some c when c > 1 -> true
          | _ -> false
        in
        player_row ~show_player_id ~include_team:false (i + 1) p)
    |> String.concat "\n"
  in
  let roster_cards =
    let margin_chip v =
      let cls =
        if v > 0.0 then "text-sky-600 dark:text-sky-400"
        else if v < 0.0 then "text-rose-400"
        else "text-slate-700 dark:text-slate-300"
      in
      let s = if v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v in
      Printf.sprintf {html|<span class="%s font-mono font-bold tabular-nums">%s</span>|html} cls (escape_html s)
    in
    detail.tfd_roster
    |> List.mapi (fun i (p: player_aggregate) ->
        let key = normalize_name p.name in
        let show_player_id =
          match Hashtbl.find_opt roster_name_counts key with
          | Some c when c > 1 -> true
          | _ -> false
        in
        let id_badge =
          if show_player_id then Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge p.player_id) else ""
        in
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
            <div class="flex items-center justify-between gap-3">
              <div class="flex items-center gap-3 min-w-0">
                <div class="shrink-0">%s</div>
                <div class="min-w-0">
                  <div class="text-sm font-bold text-slate-900 dark:text-slate-200 truncate">
                    <a href="/player/%s" class="player-name hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s
                  </div>
                  <div class="mt-0.5 text-[11px] text-slate-500 dark:text-slate-400 font-mono tabular-nums">#%d • GP %d • EFF %.1f</div>
                </div>
              </div>
              <div class="text-right shrink-0">
                <div class="text-[10px] text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">PTS</div>
                <div class="text-lg font-black text-orange-600 dark:text-orange-400 font-mono tabular-nums">%.1f</div>
              </div>
            </div>
            <div class="mt-3 grid grid-cols-3 gap-2 text-xs font-mono tabular-nums">
              <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
                <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest">MG</div>
                <div class="mt-0.5">%s</div>
              </div>
              <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
                <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest">REB</div>
                <div class="mt-0.5 text-slate-900 dark:text-slate-200 font-bold">%.1f</div>
              </div>
              <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
                <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest">AST</div>
                <div class="mt-0.5 text-slate-900 dark:text-slate-200 font-bold">%.1f</div>
              </div>
            </div>
          </div>|html}
          (player_img_tag ~class_name:"w-10 h-10 border border-slate-300 dark:border-slate-700 shadow-sm" p.player_id p.name)
          p.player_id
          (escape_html (normalize_name p.name))
          id_badge
          (i + 1)
          p.games_played
          p.efficiency
          p.avg_points
          (margin_chip p.avg_margin)
          p.avg_rebounds
          p.avg_assists)
    |> String.concat "\n"
  in
	  let roster_table_inner =
	    Printf.sprintf
	      {html|<table class="roster-table min-w-[820px] w-full text-xs sm:text-sm font-mono table-fixed tabular-nums"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap"><tr><th class="px-3 py-3 text-left font-sans w-12 whitespace-nowrap">#</th><th class="px-3 py-3 text-left font-sans w-[260px] whitespace-nowrap">Player</th><th class="px-3 py-3 text-right w-[60px] whitespace-nowrap">GP</th><th class="px-3 py-3 text-right text-orange-600 dark:text-orange-400 w-[72px] whitespace-nowrap">PTS</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap" title="MG: 팀 득실마진(출전시간 가중 평균)">MG</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">REB</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">AST</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">STL</th><th class="px-3 py-3 text-right w-[72px] whitespace-nowrap">BLK</th><th class="px-3 py-2 text-right w-[72px] whitespace-nowrap">TO</th><th class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 w-[72px] whitespace-nowrap">EFF</th></tr></thead><tbody>%s</tbody></table>|html}
	      roster_rows
	  in
	  let season_label =
	    if season = "ALL" then
	      "All Seasons"
	    else
	      seasons
	      |> List.find_opt (fun (s: season_info) -> s.code = season)
	      |> Option.map (fun (s: season_info) -> s.name)
	      |> Option.value ~default:season
	  in
	  let game_results_chart =
	    let games = detail.tfd_game_results in
	    match games with
	    | [] ->
	        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-500 dark:text-slate-400 shadow-lg">No game results</div>|html}
	    | _ ->
	        let max_abs_margin =
	          games
	          |> List.fold_left
	               (fun acc (g: team_game_result) ->
	                 let margin = g.tgr_team_score - g.tgr_opponent_score in
	                 Stdlib.max acc (Stdlib.abs margin))
	               1
	        in
	        let wins =
	          games
	          |> List.fold_left (fun acc (g: team_game_result) -> if g.tgr_is_win then acc + 1 else acc) 0
	        in
	        let losses = List.length games - wins in
	        let avg_margin =
	          let sum =
	            games
	            |> List.fold_left
	                 (fun acc (g: team_game_result) -> acc + (g.tgr_team_score - g.tgr_opponent_score))
	                 0
	          in
	          float_of_int sum /. float_of_int (List.length games)
	        in
	        let bars =
	          games
	          |> List.map (fun (g: team_game_result) ->
	               let margin = g.tgr_team_score - g.tgr_opponent_score in
	               let abs_margin = Stdlib.abs margin in
	               let height_pct =
	                 (float_of_int abs_margin /. float_of_int max_abs_margin) *. 100.0
	               in
	               let opponent_label =
	                 if g.tgr_is_home then "vs " ^ g.tgr_opponent else "@ " ^ g.tgr_opponent
	               in
	               let margin_str =
	                 if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin
	               in
	               let title =
	                 Printf.sprintf "%s • %s • %d-%d (%s)"
	                   g.tgr_game_date
	                   opponent_label
	                   g.tgr_team_score
	                   g.tgr_opponent_score
	                   margin_str
	                 |> escape_html
	               in
	               let pos_bar =
	                 if margin > 0 then
	                   Printf.sprintf
	                     {html|<div class="w-full rounded-sm bg-sky-500/80 group-hover:bg-sky-400 transition-colors" style="height: %.0f%%"></div>|html}
	                     height_pct
	                 else
	                   ""
	               in
	               let neg_bar =
	                 if margin < 0 then
	                   Printf.sprintf
	                     {html|<div class="w-full rounded-sm bg-rose-500/80 group-hover:bg-rose-400 transition-colors" style="height: %.0f%%"></div>|html}
	                     height_pct
	                 else
	                   ""
		               in
		               Printf.sprintf
		                 {html|<a href="/boxscore/%s" class="group block w-2 sm:w-1.5 h-24" title="%s"><div class="h-1/2 flex items-end">%s</div><div class="h-1/2 flex items-start">%s</div></a>|html}
		                 (Uri.pct_encode g.tgr_game_id)
		                 title
		                 pos_bar
		                 neg_bar)
		          |> String.concat "\n"
		        in
		        Printf.sprintf
		          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg"><div class="flex items-center justify-between gap-3"><div class="text-[11px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">%s</div><div class="text-xs font-mono text-slate-500 dark:text-slate-400 tabular-nums">%d-%d</div></div><div class="mt-2 flex items-center justify-between gap-3 text-[11px] text-slate-500 dark:text-slate-400 font-mono tabular-nums"><span>AVG MG %+.1f</span><span>±%d</span></div><div class="mt-3 overflow-x-auto pb-1"><div class="relative h-24 w-max"><div class="absolute left-0 right-0 top-1/2 h-px bg-slate-100 dark:bg-slate-800/80"></div><div class="flex items-stretch gap-0.5 h-24 w-max pr-1">%s</div></div></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">막대: 득실마진(MG=팀-상대). 클릭하면 박스스코어.</div></div>|html}
		          (escape_html season_label)
		          wins
		          losses
		          avg_margin
		          max_abs_margin
		          bars
		  in
		  let game_rows =
		    detail.tfd_recent_games
		    |> List.map (fun (g: team_game_result) ->
		        let res_class = if g.tgr_is_win then "text-sky-600 dark:text-sky-400" else "text-rose-400" in
	        let res_label = if g.tgr_is_win then "W" else "L" in
	        let margin = g.tgr_team_score - g.tgr_opponent_score in
        let margin_class =
          if margin > 0 then "text-sky-600 dark:text-sky-400"
          else if margin < 0 then "text-rose-400"
          else "text-slate-500 dark:text-slate-400"
        in
        let margin_str =
          if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin
        in
	        let opponent_label =
	          if g.tgr_is_home then "vs " ^ g.tgr_opponent else "@ " ^ g.tgr_opponent
	        in
	        let opponent_href = "/team/" ^ Uri.pct_encode g.tgr_opponent in
	        let date_short =
	          if String.length g.tgr_game_date >= 10 then
	            String.sub g.tgr_game_date 5 5
	          else
	            g.tgr_game_date
	        in
	        Printf.sprintf
	          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-4 py-3 text-slate-500 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-24"><a href="/boxscore/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors" title="%s"><span class="sm:hidden">%s</span><span class="hidden sm:inline">%s</span></a></td><td class="px-4 py-3 text-slate-900 dark:text-slate-200"><a class="block truncate hover:text-orange-600 dark:text-orange-400 transition-colors" href="%s" title="%s">%s</a></td><td class="px-4 py-3 text-center font-bold %s whitespace-nowrap w-14"><span class="inline-flex items-center justify-center w-7">%s</span></td><td class="px-4 py-3 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap w-36"><div class="flex items-center justify-end gap-2 flex-nowrap tabular-nums"><span class="whitespace-nowrap">%d - %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono %s whitespace-nowrap min-w-[48px] text-center">%s</span></div></td></tr>|html}
	          (Uri.pct_encode g.tgr_game_id)
	          (escape_html g.tgr_game_date)
	          (escape_html date_short)
	          (escape_html g.tgr_game_date)
	          (escape_html opponent_href)
	          (escape_html opponent_label)
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
	    ~content:(Printf.sprintf {html|<div class="space-y-6 sm:space-y-8 animate-fade-in"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 sm:p-8 shadow-2xl flex flex-col md:flex-row items-center md:items-start gap-6 sm:gap-8"><div class="w-24 h-24 sm:w-32 sm:h-32 bg-slate-100 dark:bg-slate-800 rounded-xl flex items-center justify-center p-3 sm:p-4 border-2 border-slate-300 dark:border-slate-700 shadow-inner">%s</div><div class="text-center md:text-left space-y-4 w-full"><div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-3"><h1 class="text-3xl sm:text-4xl font-black text-slate-900 dark:text-slate-200">%s</h1><form action="/team/%s" method="get" class="flex flex-col sm:flex-row items-stretch sm:items-center justify-center sm:justify-end gap-2 w-full sm:w-auto"><span class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">Season</span><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select></form></div>%s</div></div><div class="grid grid-cols-1 lg:grid-cols-3 gap-6 sm:gap-8"><div class="space-y-4 lg:col-span-2"><h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Roster</h3><div class="sm:hidden space-y-3">%s</div><details class="sm:hidden bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4"><summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">Full table</summary><div class="mt-3 overflow-x-auto">%s</div></details><div class="hidden sm:block bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">%s</div></div><div class="space-y-6 lg:col-span-1"><h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Game Results</h3>%s<h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Recent Results</h3><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-lg"><table class="min-w-[480px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap"><tr><th class="px-4 py-3 text-left font-sans w-24">Date</th><th class="px-4 py-3 text-left font-sans">Opponent</th><th class="px-4 py-3 text-center font-sans w-14">Result</th><th class="px-4 py-3 text-right font-sans w-36">Score</th></tr></thead><tbody>%s</tbody></table></div></div></div></div>|html}
	          (team_logo_tag ~class_name:"w-16 h-16 sm:w-24 sm:h-24" t)
	          (escape_html t)
	          (Uri.pct_encode t)
	          season_options
	          standing_info
	          roster_cards
	          roster_table_inner
	          roster_table_inner
	          game_results_chart
	          game_rows)

(** DB QA dashboard page *)
let qa_dashboard_page (report: Db.qa_db_report) ?(markdown=None) () =
  let int_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%d</div>|html} v
  in
  let pct_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%.1f<span class="text-base text-slate-500 dark:text-slate-400">%%</span></div>|html} v
  in
  let kpi_card ~label ~value_html ~hint_html =
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><div class="text-slate-500 dark:text-slate-400 text-[11px] uppercase tracking-widest font-bold">%s</div><div class="mt-2">%s</div><div class="mt-2 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div></div>|html}
      (escape_html label)
      value_html
      hint_html
  in
  let score_or_dash = function | None -> "-" | Some v -> string_of_int v in
  let delta_or_dash a b =
    match (a, b) with
    | Some stored, Some summed -> Printf.sprintf "%+d" (summed - stored)
    | _ -> "-"
  in
  let mismatch_rows =
    report.qdr_score_mismatch_sample
    |> List.map (fun (row: Db.qa_score_mismatch) ->
      let stored = Printf.sprintf "%s - %s" (score_or_dash row.qsm_home_score) (score_or_dash row.qsm_away_score) in
      let summed = Printf.sprintf "%s - %s" (score_or_dash row.qsm_home_sum) (score_or_dash row.qsm_away_sum) in
      let home_delta = delta_or_dash row.qsm_home_score row.qsm_home_sum in
      let away_delta = delta_or_dash row.qsm_away_score row.qsm_away_sum in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs whitespace-nowrap">%s</td><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/boxscore/%s">%s</a></td><td class="px-3 py-2 text-slate-700 dark:text-slate-300 text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-500 dark:text-slate-400 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-500 dark:text-slate-400 tabular-nums">%s</td></tr>|html}
        (escape_html row.qsm_game_date)
        (Uri.pct_encode row.qsm_game_id)
        (escape_html row.qsm_game_id)
        (escape_html (row.qsm_home_team ^ " vs " ^ row.qsm_away_team))
        (escape_html stored)
        (escape_html summed)
        (escape_html home_delta)
        (escape_html away_delta))
    |> String.concat "\n"
  in
  let team_count_rows =
    report.qdr_team_count_anomaly_sample
    |> List.map (fun (row: Db.qa_team_count_anomaly) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/boxscore/%s">%s</a></td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td></tr>|html}
        (Uri.pct_encode row.qtca_game_id)
        (escape_html row.qtca_game_id)
        row.qtca_team_count)
    |> String.concat "\n"
  in
  let dup_row_rows =
    report.qdr_duplicate_player_row_sample
    |> List.map (fun (row: Db.qa_duplicate_player_row) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/boxscore/%s">%s</a></td><td class="px-3 py-2 text-xs">%s</td><td class="px-3 py-2 text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/player/%s">%s</a></td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td></tr>|html}
        (Uri.pct_encode row.qdpr_game_id)
        (escape_html row.qdpr_game_id)
        (team_badge row.qdpr_team_name)
        (escape_html row.qdpr_player_id)
        (escape_html row.qdpr_player_name)
        row.qdpr_row_count)
    |> String.concat "\n"
  in
  let dup_name_rows =
    report.qdr_duplicate_player_name_sample
    |> List.map (fun (row: Db.qa_duplicate_player_name) ->
      let ids =
        row.qdpn_player_ids
        |> List.map (fun id -> Printf.sprintf {html|<a href="/player/%s" class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400">%s</a>|html} (escape_html id) (escape_html id))
        |> String.concat " "
      in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td><td class="px-3 py-2">%s</td></tr>|html}
        (escape_html row.qdpn_player_name)
        row.qdpn_id_count
        ids)
    |> String.concat "\n"
  in
  let markdown_block =
    match markdown with
    | None -> ""
    | Some md ->
        Printf.sprintf
          {html|<details class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><summary class="cursor-pointer select-none text-slate-700 dark:text-slate-300 font-bold">Legacy QA (Markdown)</summary><pre class="mt-4 text-[11px] text-slate-700 dark:text-slate-300 whitespace-pre-wrap break-words">%s</pre></details>|html}
          (escape_html md)
  in
  let sources_block =
    {html|<details class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 text-xs text-slate-500 dark:text-slate-400">
      <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">출처 / 검증 기준</summary>
      <div class="mt-2 space-y-1 leading-relaxed">
        <div class="text-slate-500 dark:text-slate-400 font-bold">출처 (공식)</div>
        <div>• 경기 결과/스코어: <a href="https://www.wkbl.or.kr/game/result.asp" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:underline">wkbl.or.kr/game/result.asp</a></div>
        <div>• 박스스코어: <span class="font-mono text-slate-900 dark:text-slate-200">/game/ajax/ajax_game_result_2.asp</span> (POST) → <span class="font-mono text-slate-900 dark:text-slate-200">game_stats</span></div>
        <div>• PBP +/-: <a href="https://www.wkbl.or.kr/live11/path_live_sms.asp" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:underline">wkbl.or.kr/live11/path_live_sms.asp</a> → <span class="font-mono text-slate-900 dark:text-slate-200">player_plus_minus</span></div>
        <div class="pt-1 text-slate-500 dark:text-slate-400 font-bold">검증 기준 (스코어 교차검증)</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">VERIFIED (✓)</span>: <span class="font-mono text-slate-900 dark:text-slate-200">games.home/away_score</span>가 존재하고, 양 팀 <span class="font-mono text-slate-900 dark:text-slate-200">SUM(game_stats.pts)</span>와 모두 일치</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">DERIVED (Σ)</span>: 스코어/합계 누락으로 교차검증 불가. 표시 스코어는 <span class="font-mono text-slate-900 dark:text-slate-200">COALESCE(games score, sum pts)</span></div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">MISMATCH (!)</span>: 스코어와 합계가 모두 있는데 값이 다름</div>
        <div class="pt-1">※ 이 검증은 “최종 득점”만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 검증이 필요합니다.</div>
      </div>
    </details>|html}
  in
  layout ~title:"QA | WKBL"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in"><div class="flex flex-col gap-2"><h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Data Quality (QA)</h2><div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">기록 신뢰도를 위해 <span class="font-mono text-slate-900 dark:text-slate-200">스코어 불일치</span>, <span class="font-mono text-slate-900 dark:text-slate-200">팀 수 이상</span>, <span class="font-mono text-slate-900 dark:text-slate-200">중복 스탯 row</span>, <span class="font-mono text-slate-900 dark:text-slate-200">중복 선수 ID</span>를 점검합니다. (Generated: <span class="font-mono">%s</span>)</div></div>%s<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">%s%s%s%s</div><div class="grid grid-cols-1 lg:grid-cols-3 gap-4">%s%s%s</div><div class="grid grid-cols-1 gap-4"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Score Mismatch</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed"><span class="font-mono text-slate-900 dark:text-slate-200">games.home/away_score</span> vs <span class="font-mono text-slate-900 dark:text-slate-200">SUM(game_stats.pts)</span> 비교</div><div class="mt-4 overflow-x-auto"><table class="min-w-[860px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left w-[90px]">Date</th><th class="px-3 py-2 text-left w-[120px]">Game</th><th class="px-3 py-2 text-left">Matchup</th><th class="px-3 py-2 text-right w-[120px]">Stored</th><th class="px-3 py-2 text-right w-[120px]">Summed</th><th class="px-3 py-2 text-right w-[80px]">HΔ</th><th class="px-3 py-2 text-right w-[80px]">AΔ</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Team Count Anomaly</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">한 경기에서 <span class="font-mono text-slate-900 dark:text-slate-200">game_stats.team_code</span>가 2개가 아닌 케이스</div><div class="mt-4 overflow-x-auto"><table class="min-w-[320px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left">Game</th><th class="px-3 py-2 text-right w-[90px]">Teams</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Duplicate Player Rows</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed"><span class="font-mono text-slate-900 dark:text-slate-200">(game_id, team_code, player_id)</span> 중복</div><div class="mt-4 overflow-x-auto"><table class="min-w-[720px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left w-[120px]">Game</th><th class="px-3 py-2 text-left w-[160px]">Team</th><th class="px-3 py-2 text-left">Player</th><th class="px-3 py-2 text-right w-[80px]">Rows</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Duplicate Player Names</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">동일 이름으로 <span class="font-mono text-slate-900 dark:text-slate-200">player_id</span>가 여러 개인 케이스</div><div class="mt-4 overflow-x-auto"><table class="min-w-[720px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left">Name</th><th class="px-3 py-2 text-right w-[80px]">IDs</th><th class="px-3 py-2 text-left">player_id</th></tr></thead><tbody>%s</tbody></table></div></div></div>%s</div>|html}
      (escape_html report.qdr_generated_at)
      sources_block
      (kpi_card ~label:"Games" ~value_html:(int_chip report.qdr_games_total) ~hint_html:"전체 경기 수(정규/PO, 시범 제외)")
      (kpi_card ~label:"Games w/ Stats" ~value_html:(int_chip report.qdr_games_with_stats) ~hint_html:"game_stats가 존재하는 경기")
      (kpi_card ~label:"PBP +/- Coverage" ~value_html:(pct_chip report.qdr_plus_minus_coverage_pct) ~hint_html:(Printf.sprintf "PBP 기반 +/-가 있는 경기: %d" report.qdr_plus_minus_games))
      (kpi_card ~label:"Generated" ~value_html:(Printf.sprintf {html|<div class="text-sm font-mono text-slate-900 dark:text-slate-200 break-all">%s</div>|html} (escape_html report.qdr_generated_at)) ~hint_html:"UTC 기준")
      (kpi_card ~label:"Score Mismatch" ~value_html:(int_chip report.qdr_score_mismatch_count) ~hint_html:"최종 스코어 vs 합계 불일치")
      (kpi_card ~label:"Team Count != 2" ~value_html:(int_chip report.qdr_team_count_anomaly_count) ~hint_html:"한 경기 팀 수가 2가 아님")
      (kpi_card ~label:"Dup Player Rows" ~value_html:(int_chip report.qdr_duplicate_player_row_count) ~hint_html:"중복으로 라인이 2개 뜨는 원인")
      report.qdr_score_mismatch_count
      mismatch_rows
      report.qdr_team_count_anomaly_count
      team_count_rows
      report.qdr_duplicate_player_row_count
      dup_row_rows
      report.qdr_duplicate_player_name_count
      dup_name_rows
      markdown_block)

(** Draft / Trade (official) page *)
let transactions_page
  ~tab
  ~year
  ~q
  ~draft_years
  ~trade_years
  ~(draft_picks: draft_pick_row list)
  ~(trade_events: official_trade_event list)
  =
  let tab_value = tab |> String.trim |> String.lowercase_ascii in
  let active_tab = if tab_value = "trade" then "trade" else "draft" in
  let tab_link t label =
    let cls =
      if active_tab = t then
        "bg-slate-100 dark:bg-slate-800/80 border-slate-300 dark:border-slate-700 text-slate-900 dark:text-slate-200"
      else
        "bg-white dark:bg-slate-900/40 border-slate-200 dark:border-slate-800 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:bg-slate-100 dark:bg-slate-800/50"
    in
    Printf.sprintf
      {html|<a href="/transactions?tab=%s" class="px-3 py-2 rounded-lg border text-xs font-bold uppercase tracking-widest transition %s">%s</a>|html}
      (escape_html t)
      cls
      (escape_html label)
  in
  let year_options years =
    let all_selected = if year = 0 then "selected" else "" in
    let base =
      years
      |> List.map (fun y ->
          let selected = if y = year then "selected" else "" in
          Printf.sprintf {html|<option value="%d" %s>%d</option>|html} y selected y)
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="0" %s>All</option>%s|html} all_selected base
  in
  let active_years = if active_tab = "trade" then trade_years else draft_years in
  let filter_form =
    Printf.sprintf
      {html|<form method="get" action="/transactions" class="flex flex-col sm:flex-row sm:items-end gap-3">
  <input type="hidden" name="tab" value="%s">
  <label class="block text-xs text-slate-500 dark:text-slate-400 space-y-1">
    <div class="font-bold uppercase tracking-widest text-[10px]">Year</div>
    <select name="year" class="bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none">
      %s
    </select>
  </label>
  <label class="block text-xs text-slate-500 dark:text-slate-400 space-y-1 min-w-0 flex-1">
    <div class="font-bold uppercase tracking-widest text-[10px]">Search</div>
    <input name="q" value="%s" placeholder="player / team / keyword" class="w-full bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none">
  </label>
  <button class="shrink-0 bg-orange-600 hover:bg-orange-500 text-slate-900 dark:text-slate-200 font-bold rounded-lg px-4 py-2 text-sm transition">Apply</button>
</form>|html}
      (escape_html active_tab)
      (year_options active_years)
      (escape_html q)
  in
  let draft_table =
    let pick_label (row: draft_pick_row) =
      match row.dpr_draft_round, row.dpr_pick_in_round, row.dpr_overall_pick with
      | Some r, Some p, Some o -> Printf.sprintf "R%d #%d (O%d)" r p o
      | Some r, Some p, None -> Printf.sprintf "R%d #%d" r p
      | Some r, None, Some o -> Printf.sprintf "R%d (O%d)" r o
      | _, _, _ -> "-"
    in
    let rows =
      match draft_picks with
      | [] ->
          {html|<tr><td colspan="6" class="px-4 py-10 text-center text-slate-500 dark:text-slate-400 text-sm">No draft rows found. (Build with <span class="font-mono text-slate-700 dark:text-slate-300">WKBL_SYNC_DRAFT_TRADE=1</span> or run <span class="font-mono text-slate-700 dark:text-slate-300">scripts/wkbl_draft_trade_sync.py</span>)</td></tr>|html}
      | xs ->
          xs
          |> List.map (fun (r: draft_pick_row) ->
              let y =
                match r.dpr_draft_year with
                | Some v -> string_of_int v
                | None -> "-"
              in
              let team_html =
                match r.dpr_draft_team with
                | None -> {html|<span class="text-slate-500 dark:text-slate-400">-</span>|html}
                | Some t -> team_badge t
              in
              Printf.sprintf
                {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors">
  <td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap w-[72px]">%s</td>
  <td class="px-4 py-3 text-slate-700 dark:text-slate-300 font-mono whitespace-nowrap w-[140px]">%s</td>
  <td class="px-4 py-3 min-w-0"><a class="text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400 font-bold truncate block" href="/player/%s">%s</a><div class="mt-1 text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s</div></td>
  <td class="px-4 py-3">%s</td>
  <td class="px-4 py-3 text-[11px] text-slate-700 dark:text-slate-300 font-mono whitespace-pre-line break-words">%s</td>
  <td class="px-4 py-3 text-[11px]"><a class="text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">Source</a></td>
</tr>|html}
                (escape_html y)
                (escape_html (pick_label r))
                (Uri.pct_encode r.dpr_player_id)
                (escape_html (normalize_name r.dpr_player_name))
                (escape_html r.dpr_player_id)
                team_html
                (escape_html r.dpr_raw_text)
                (escape_html r.dpr_source_url))
          |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
  <table class="min-w-[980px] w-full text-sm table-fixed tabular-nums">
    <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] whitespace-nowrap">
      <tr>
        <th class="px-4 py-3 text-left w-[72px]">Year</th>
        <th class="px-4 py-3 text-left w-[140px]">Pick</th>
        <th class="px-4 py-3 text-left">Player</th>
        <th class="px-4 py-3 text-left w-[220px]">Team</th>
        <th class="px-4 py-3 text-left">Raw</th>
        <th class="px-4 py-3 text-left w-[90px]">Link</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
  </table>
</div>|html}
      rows
  in
  let trade_list =
    let extract_salary_krw (text : string) : int option =
      let key = "연봉" in
      match find_substring_from ~sub:key text ~from:0 with
      | None -> None
      | Some idx ->
          let start = idx + String.length key in
          let len = String.length text in
          let is_digit c = c >= '0' && c <= '9' in
          let rec scan i seen_digit acc =
            if i >= len then acc
            else
              let c = text.[i] in
              if is_digit c then scan (i + 1) true (c :: acc)
              else if c = ',' && seen_digit then scan (i + 1) seen_digit acc
              else if seen_digit then acc
              else scan (i + 1) seen_digit acc
          in
          let digits_rev = scan start false [] in
          (match digits_rev with
          | [] -> None
          | _ ->
              let digits = digits_rev |> List.rev |> List.to_seq |> String.of_seq in
              int_of_string_opt digits)
    in
    let salary_chip (text: string) =
      match extract_salary_krw text with
      | None -> ""
      | Some won when won > 0 ->
          let million = (won + 500_000) / 1_000_000 in
          Printf.sprintf
            {html|<span title="연봉 ₩%d" class="shrink-0 px-2 py-0.5 rounded bg-orange-500/10 border border-orange-500/30 text-[10px] font-mono text-orange-700 whitespace-nowrap">₩%dM</span>|html}
            won
            million
      | Some _ -> ""
    in
    let contains_team (text: string) (team: string) =
      match find_substring_from ~sub:team text ~from:0 with
      | Some _ -> true
      | None -> false
    in
    let trade_team_candidates =
      [ "BNK 썸"; "BNK"; "우리은행"; "삼성생명"; "신한은행"; "KB스타즈"; "하나원큐"; "하나은행"
      ; "KB국민은행"; "국민은행"; "신세계"; "금호생명"; "KDB생명"; "KEB하나은행"
      ]
    in
    let team_chips (text: string) =
      let found =
        trade_team_candidates
        |> List.filter (contains_team text)
        |> List.fold_left
          (fun acc team_name ->
            let key =
              match team_code_of_string team_name with
              | Some code -> "CODE:" ^ code
              | None -> "NAME:" ^ (normalize_label team_name |> String.uppercase_ascii)
            in
            if List.exists (fun (k, _) -> k = key) acc then acc else (key, team_name) :: acc)
          []
        |> List.rev
      in
      found
      |> List.map (fun (_, team_name) -> team_badge team_name)
      |> String.concat ""
    in
    let rows =
      match trade_events with
      | [] ->
          {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/50 p-5 text-slate-500 dark:text-slate-400 text-sm">No trade events found. (Build with <span class="font-mono text-slate-700 dark:text-slate-300">WKBL_SYNC_DRAFT_TRADE=1</span> or run <span class="font-mono text-slate-700 dark:text-slate-300">scripts/wkbl_draft_trade_sync.py</span>)</div>|html}
      | xs ->
          let items =
            xs
            |> List.map (fun (e: official_trade_event) ->
                let contract_chip_html =
                  match extract_contract_years e.ote_event_text with
                  | None -> ""
                  | Some years ->
                      Printf.sprintf
                        {html|<span title="계약 %d년" class="shrink-0 px-2 py-0.5 rounded bg-emerald-500/10 border border-emerald-500/30 text-[10px] font-mono text-emerald-700 dark:text-emerald-400 whitespace-nowrap">%dY</span>|html}
                        years
                        years
                in
                let salary_chip_html = salary_chip e.ote_event_text in
                let team_chips_html = team_chips e.ote_event_text in
                Printf.sprintf
                  {html|<li class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-5 shadow-lg space-y-2">
  <div class="flex flex-wrap items-center justify-between gap-2">
    <div class="font-mono text-[11px] text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</div>
    <div class="flex flex-wrap items-center gap-2">%s%s%s</div>
  </div>
  <div class="text-slate-900 dark:text-slate-200 text-sm leading-relaxed break-words">%s</div>
  <div class="text-[11px]"><a class="text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">Source</a></div>
</li>|html}
                  (escape_html e.ote_event_date)
                  team_chips_html
                  contract_chip_html
                  salary_chip_html
                  (escape_html e.ote_event_text)
                  (escape_html e.ote_source_url))
            |> String.concat "\n"
          in
          Printf.sprintf {html|<ol class="space-y-3">%s</ol>|html} items
    in
    rows
  in
  let content =
    let section =
      if active_tab = "trade" then trade_list else draft_table
    in
    Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
  <div class="flex flex-col gap-2">
    <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Draft / Trade</h2>
    <div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">WKBL 공식 페이지 원문 기반입니다. (Draft는 <span class="font-mono text-slate-900 dark:text-slate-200">player_id(pno)</span> 기반 / Trade는 <span class="font-mono text-slate-900 dark:text-slate-200">원문 저장 + 텍스트 검색</span>)</div>
  </div>
  <div class="flex flex-wrap items-center gap-2">%s%s</div>
  %s
  <details class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 text-xs text-slate-500 dark:text-slate-400">
    <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">Sync / Build</summary>
    <div class="mt-2 space-y-2 leading-relaxed">
      <div>Docker 빌드에서 공식 Draft/Trade를 포함하려면 <span class="font-mono text-slate-900 dark:text-slate-200">WKBL_SYNC_DRAFT_TRADE=1</span>을 켜세요.</div>
      <div class="text-slate-500 dark:text-slate-400">로컬 DB를 갱신하려면 아래를 실행하세요: (네트워크 필요)</div>
      <code class="block font-mono text-slate-700 dark:text-slate-300 bg-slate-950/30 border border-slate-300 dark:border-slate-700/60 px-3 py-2 rounded overflow-x-auto whitespace-nowrap">python3 scripts/wkbl_draft_trade_sync.py --only-missing --backup</code>
    </div>
  </details>
  %s
</div>|html}
      (tab_link "draft" "Draft")
      (tab_link "trade" "Trade")
      filter_form
      section
  in
  layout ~title:"Draft / Trade | WKBL" ~content

(** Error page *)
let error_page message = layout ~title:"Error" ~content:(Printf.sprintf {html|<div class="flex flex-col items-center justify-center py-20"><span class="text-6xl mb-4">😵</span><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200 mb-2">Something went wrong</h2><p class="text-slate-500 dark:text-slate-400">%s</p><a href="/" class="mt-4 text-orange-500 hover:underline">← Back to home</a></div>|html} (escape_html message))
