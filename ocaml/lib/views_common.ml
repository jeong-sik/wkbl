open Domain

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

type empty_state_icon = BasketballIcon | SearchIcon | ChartIcon | UsersIcon | TableIcon

type col_spec = {
  header : string;
  width : int option;
  align : [ `Left | `Right | `Center ];
  resp : [ `Always | `Hidden_sm | `Hidden_md | `Hidden_lg ];
  sort : string option;
  title : string option;
  highlight : bool;
}

let col ?(w=None) ?(align=`Left) ?(resp=`Always) ?sort ?title ?(highlight=false) header =
  { header; width = w; align; resp; sort; title; highlight }

let px w = Some w

(** Render a fixed-layout table (Perfectly Aligned via th-style & Data-Oriented) *)
let render_fixed_table ~id ~min_width ~(cols : col_spec list) (rows_data : string list list) =
  let resp_class = function
    | `Always -> ""
    | `Hidden_sm -> "hidden sm:table-cell"
    | `Hidden_md -> "hidden md:table-cell"
    | `Hidden_lg -> "hidden lg:table-cell"
  in
  let align_class = function
    | `Left -> "text-left"
    | `Center -> "text-center"
    | `Right -> "text-right"
  in
  (* 1. Render <thead> - Header determines column widths in table-fixed *)
  let thead =
    cols
    |> List.map (fun c ->
        let width_style =
          match c.width with
          | Some w -> Printf.sprintf "width: %dpx; min-width: %dpx;" w w
          | None -> "width: auto;"
        in
        let base_cls = "px-3 py-2 font-sans whitespace-nowrap" in
        let align_cls = align_class c.align in
        let resp_cls = resp_class c.resp in
        let highlight_cls = if c.highlight then "text-orange-600 dark:text-orange-400" else "" in
        let full_cls = String.concat " " [base_cls; align_cls; resp_cls; highlight_cls] in
        let sort_attr =
          match c.sort with
          | Some k -> Printf.sprintf " data-sortable data-sort-key=\"%s\"" k
          | None -> ""
        in
        let title_attr =
          match c.title with
          | Some t -> Printf.sprintf " title=\"%s\"" (escape_html t)
          | None -> ""
        in
        Printf.sprintf {html|<th class="%s" style="%s"%s%s>%s</th>|html}
          full_cls width_style sort_attr title_attr (escape_html c.header))
    |> String.concat "\n"
    |> fun s -> Printf.sprintf {html|<thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr>%s</tr></thead>|html} s
  in
  (* 2. Render <tbody> - Apply SAME width style to td to enforce alignment *)
  let tbody =
    rows_data
    |> List.map (fun row ->
        if List.length row <> List.length cols then
          Printf.sprintf "<!-- Row column count mismatch -->"
        else
          let cells =
            List.map2 (fun c data ->
              let width_style =
                match c.width with
                | Some w -> Printf.sprintf "width: %dpx; min-width: %dpx;" w w
                | None -> "width: auto;"
              in
              let base_cls = "px-3 py-2 whitespace-nowrap overflow-hidden truncate" in
              let align_cls = align_class c.align in
              let resp_cls = resp_class c.resp in
              let color_cls = if c.highlight then "text-orange-600 dark:text-orange-400 font-bold" else "text-slate-700 dark:text-slate-300" in
              let full_cls = String.concat " " [base_cls; align_cls; resp_cls; color_cls] in
              Printf.sprintf {html|<td class="%s" style="%s">%s</td>|html} full_cls width_style data
            ) cols row
            |> String.concat ""
          in
          Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors group">%s</tr>|html} cells
      )
    |> String.concat "\n"
    |> fun s -> Printf.sprintf {html|<tbody>%s</tbody>|html} s
  in
  (* Assemble *)
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl">
  <table id="%s" class="w-full %s text-xs sm:text-sm font-mono tabular-nums table-fixed" style="border-collapse: separate; border-spacing: 0;" aria-label="Data Table">
    %s
    %s
  </table>
</div>|html}
    id min_width thead tbody

let normalize_name s = Domain.normalize_label s
let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

let team_logo_tag ?(class_name="w-8 h-8") team_name =
  let logo_file = match Domain.team_code_of_string team_name with | Some code -> Domain.team_code_to_logo code | None -> None in
  match logo_file with
  | Some f -> Printf.sprintf {html|<img src="/static/images/%s" alt="" class="%s object-contain" loading="lazy">|html} f class_name
  | None -> Printf.sprintf {html|<div class="%s bg-slate-100 dark:bg-slate-800 rounded flex items-center justify-center text-xs">🏀</div>|html} class_name

let team_badge ?(max_width="max-w-[130px]") team_name =
  let _ = max_width in
  let display = normalize_name team_name in
  let color = Domain.team_code_of_string display |> Option.map Domain.team_code_to_color |> Option.value ~default:"#666" in
  Printf.sprintf {html|<a href="/team/%s" class="inline-flex min-w-0 items-center gap-1.5 px-1.5 py-0.5 rounded text-[11px] font-medium transition backdrop-blur-sm" style="background-color: %s20; color: %s; border: 1px solid %s40">%s<span class="truncate">%s</span></a>|html} (Uri.pct_encode team_name) color color color (team_logo_tag ~class_name:"w-4 h-4 shrink-0" display) (escape_html display)

let player_img_tag ?(class_name="w-12 h-12") player_id _name =
  let remote_src = Printf.sprintf "https://www.wkbl.or.kr/static/images/player/pimg/m_%s.jpg" player_id in
  Printf.sprintf {html|<img src="%s" class="%s rounded-full object-cover bg-slate-100 border border-slate-300" loading="lazy" onerror="this.src='/static/images/player_placeholder.svg'">|html} remote_src class_name

let player_id_badge player_id = Printf.sprintf {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/50 border border-slate-300 dark:border-slate-700/50 text-[10px] font-mono text-slate-500">ID %s</span>|html} (String.sub player_id (String.length player_id - 3) 3)

let format_int_commas n = string_of_int n
let format_int_compact n = if abs n < 1000 then string_of_int n else Printf.sprintf "%.1fK" (float_of_int n /. 1000.0)

let points_total_cell ?(extra_classes="") ?(width_style="") avg total =
  let classes = String.concat " " ["px-3 py-2 text-right"; extra_classes] in
  Printf.sprintf
    {html|<td class="%s" style="%s"><div class="flex flex-col items-end leading-tight"><span class="text-orange-600 dark:text-orange-400 font-bold font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-500 text-[9px] font-mono whitespace-nowrap" title="Career Total">Σ%s</span></div></td>|html}
    classes
    width_style
    avg
    (format_int_compact total)

let stat_total_cell ?(extra_classes="") ?(width_style="") avg total =
  let classes = String.concat " " ["px-3 py-2 text-right"; extra_classes] in
  Printf.sprintf
    {html|<td class="%s" style="%s"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-500 text-[9px] font-mono whitespace-nowrap" title="Career Total">Σ%s</span></div></td>|html}
    classes
    width_style
    avg
    (format_int_compact total)

let margin_cell ?(extra_classes="") ?(width_style="") value =
  let cls =
    if value > 0.0 then "text-sky-600 dark:text-sky-400 font-bold"
    else if value < 0.0 then "text-rose-600 dark:text-rose-400 font-bold"
    else "text-slate-700 dark:text-slate-300 font-bold"
  in
  let value_str = if value > 0.0 then Printf.sprintf "+%.1f" value else format_float value in
  let classes = String.concat " " ["px-3 py-2 text-right font-mono"; extra_classes; cls] in
  Printf.sprintf {html|<td class="%s" style="%s">%s</td>|html} classes width_style (escape_html value_str)

let stat_cell ?(highlight=false) ?(extra_classes="") ?(width_style="") value =
  let color_cls =
    if highlight then "text-orange-600 dark:text-orange-400 font-bold"
    else "text-slate-700 dark:text-slate-300"
  in
  let classes = String.concat " " ["px-3 py-2 text-right font-mono tabular-nums"; extra_classes; color_cls] in
  Printf.sprintf {html|<td class="%s" style="%s">%.1f</td>|html} classes width_style value

let empty_state ?icon title desc =
  let _ = icon in
  Printf.sprintf {html|<div class="text-center py-12 px-4"><div class="text-4xl mb-4">🏀</div><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</h3><p class="text-slate-500 dark:text-slate-400">%s</p></div>|html} title desc

let layout ~title ?(canonical_path="/") ?(description="") ?(json_ld="") ?og_title ?og_description ?og_image ?data_freshness ~content () =
  let _ = og_title in let _ = og_description in let _ = og_image in let _ = data_freshness in
  let _ = canonical_path in let _ = description in let _ = json_ld in
  Printf.sprintf {html|<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <script src="https://cdn.tailwindcss.com"></script>
  <style>
    /* Accessibility: Screen reader only utility */
    .sr-only {
      position: absolute;
      width: 1px;
      height: 1px;
      padding: 0;
      margin: -1px;
      overflow: hidden;
      clip: rect(0, 0, 0, 0);
      white-space: nowrap;
      border: 0;
    }
    /* Accessibility: Enhanced focus styles */
    :focus-visible {
      outline: 2px solid #f97316;
      outline-offset: 2px;
    }
    /* Skip link styles */
    .skip-link {
      position: absolute;
      top: -40px;
      left: 0;
      background: #f97316;
      color: white;
      padding: 8px 16px;
      z-index: 100;
      border-radius: 0 0 8px 0;
      font-weight: bold;
      transition: top 0.2s ease-in-out;
    }
    .skip-link:focus {
      top: 0;
    }
    /* Remove default focus outline for non-keyboard users */
    :focus:not(:focus-visible) {
      outline: none;
    }
    /* Smooth transitions for interactive elements */
    a, button, select, input {
      transition: outline-offset 0.1s ease-in-out, box-shadow 0.15s ease-in-out;
    }
    a:focus-visible, button:focus-visible, select:focus-visible, input:focus-visible {
      box-shadow: 0 0 0 4px rgba(249, 115, 22, 0.3);
    }
    /* Reduced motion preference */
    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after {
        animation-duration: 0.01ms !important;
        animation-iteration-count: 1 !important;
        transition-duration: 0.01ms !important;
      }
    }
    /* Spinner animation */
    @keyframes spin { to { transform: rotate(360deg); } }
    .animate-spin { animation: spin 1s linear infinite; }
    .border-3 { border-width: 3px; }
    /* HTMX loading indicator */
    .htmx-indicator { opacity: 0; transition: opacity 200ms ease-in; }
    .htmx-request .htmx-indicator, .htmx-request.htmx-indicator { opacity: 1; }
    /* Skeleton pulse animation */
    @keyframes pulse { 0%%, 100%% { opacity: 1; } 50%% { opacity: 0.5; } }
    .animate-pulse { animation: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite; }
  </style>
</head>
<body class="bg-slate-50 dark:bg-[#0b0e14] text-slate-900 dark:text-slate-200">
  <!-- Skip to main content link for keyboard users -->
  <a href="#main-content" class="skip-link">본문으로 건너뛰기</a>

  <!-- ARIA live region for dynamic content announcements -->
  <div id="aria-live" aria-live="polite" aria-atomic="true" class="sr-only"></div>

  <main id="main-content" role="main" tabindex="-1" class="max-w-7xl mx-auto px-4 py-6">
    %s
  </main>

  <!-- Toast notification container -->
  <div id="toast-container" class="fixed bottom-4 right-4 z-50 flex flex-col gap-2" aria-live="polite"></div>

  <script>
    // Toast notification system
    window.showToast = function(message, type) {
      type = type || 'info';
      var container = document.getElementById('toast-container');
      if (!container) return;
      var colors = {
        success: 'bg-emerald-500',
        error: 'bg-rose-500',
        warning: 'bg-amber-500',
        info: 'bg-sky-500'
      };
      var toast = document.createElement('div');
      toast.className = colors[type] + ' text-white px-4 py-3 rounded-lg shadow-lg transform translate-x-full opacity-0 transition-all duration-300 flex items-center gap-2 max-w-sm';
      toast.innerHTML = '<span>' + message + '</span><button onclick="this.parentElement.remove()" class="ml-2 hover:opacity-75">×</button>';
      container.appendChild(toast);
      requestAnimationFrame(function() {
        toast.classList.remove('translate-x-full', 'opacity-0');
      });
      setTimeout(function() {
        toast.classList.add('translate-x-full', 'opacity-0');
        setTimeout(function() { toast.remove(); }, 300);
      }, 4000);
    };
    window.hideAllToasts = function() {
      var container = document.getElementById('toast-container');
      if (container) container.innerHTML = '';
    };
  </script>

  <script>
    // Helper function to announce messages to screen readers
    window.announceToScreenReader = function(message) {
      const liveRegion = document.getElementById('aria-live');
      if (liveRegion) {
        liveRegion.textContent = message;
        setTimeout(() => { liveRegion.textContent = ''; }, 1000);
      }
    };
  </script>
</body>
</html>|html} title content

let eff_badge ?(show_label=false) eff =
  let color_cls = if eff >= 20.0 then "bg-orange-500/10 text-orange-600 dark:text-orange-400 border-orange-500/30"
                  else "bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400 border-slate-300 dark:border-slate-700" in
  let label = if show_label then "EFF " else "" in
  Printf.sprintf {html|<span class="px-2 py-0.5 rounded border text-[10px] font-mono font-bold %s">%s%.1f</span>|html} color_cls label eff

let score_quality_badge ?(compact=false) q =
  match q with
  | Verified -> (if compact then "<span class=\"text-sky-500\" title=\"Verified\">✓</span>" else "<span class=\"px-2 py-0.5 rounded bg-sky-500/10 text-sky-600 border border-sky-500/30 text-[10px] font-mono\">VERIFIED</span>")
  | Derived -> (if compact then "<span class=\"text-amber-500\" title=\"Derived\">Σ</span>" else "<span class=\"px-2 py-0.5 rounded bg-amber-500/10 text-amber-600 border border-amber-500/30 text-[10px] font-mono\">DERIVED</span>")
  | Mismatch -> (if compact then "<span class=\"text-rose-500\" title=\"Mismatch\">!</span>" else "<span class=\"px-2 py-0.5 rounded bg-rose-500/10 text-rose-600 border border-rose-500/30 text-[10px] font-mono\">MISMATCH</span>")

let team_scope_to_string = function PerGame -> "per_game" | Totals -> "totals"
let wkbl_official_game_result_url id = Some ("https://www.wkbl.or.kr/game/result.asp?game_id=" ^ id)
let extract_contract_years s = try Some (int_of_string (String.sub s 0 1)) with _ -> None

let radar_chart ?(show_league_avg=false) ~labels ~values_a ~values_b ?(color_a="#f97316") ?(color_b="#0ea5e9") () =
  let _ = show_league_avg in let _ = labels in let _ = values_a in let _ = values_b in let _ = color_a in let _ = color_b in "<!-- Radar Chart Placeholder -->"

let normalize_stat_for_radar _ _ = 0.0
let career_trajectory_chart _ = "<!-- Chart Placeholder -->"
let player_season_stats_component ~player_id:_ ~scope:_ _ = "<!-- Season Stats Placeholder -->"
let player_row ?(show_player_id=false) ?(team_cell_class="px-3 py-2") ?(include_team=true) (rank: int) (p: player_aggregate) =
  let id_badge =
    if show_player_id then player_id_badge p.player_id else ""
  in
  let display_name = normalize_name p.name in
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
      Printf.sprintf {html|<td class="%s whitespace-nowrap" style="width: 130px; min-width: 130px;">%s</td>|html}
        (escape_html team_cell_class)
        (team_badge p.team_name)
    else
      ""
  in
  Printf.sprintf
    {html|<tr class="group border-b border-slate-200 dark:border-slate-700/50 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors font-mono tabular-nums">
      <td class="px-2 py-2 text-slate-500 dark:text-slate-500 text-sm text-center font-bold whitespace-nowrap" style="width: 50px; min-width: 50px;">%d</td>
      <td class="px-3 py-2 font-medium text-slate-900 dark:text-white font-sans whitespace-nowrap" style="width: 200px; min-width: 200px;">
        <div class="flex items-center gap-3 min-w-0">
          %s
          <div class="flex items-center gap-2 min-w-0">
            <a href="/player/%s" class="player-name hover:text-orange-600 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
            <span class="%s">%s</span>
          </div>
        </div>
      </td>
      %s
      <td class="px-3 py-2 text-right whitespace-nowrap hidden sm:table-cell text-slate-500 dark:text-slate-400 font-mono" style="width: 60px; min-width: 60px;">%d</td>
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
    (points_total_cell ~extra_classes:"whitespace-nowrap" ~width_style:"width: 90px; min-width: 90px;" p.avg_points p.total_points)
    (margin_cell ~extra_classes:"hidden md:table-cell whitespace-nowrap" ~width_style:"width: 80px; min-width: 80px;" p.avg_margin)
    (stat_total_cell ~extra_classes:"whitespace-nowrap" ~width_style:"width: 85px; min-width: 85px;" p.avg_rebounds p.total_rebounds)
    (stat_total_cell ~extra_classes:"hidden md:table-cell whitespace-nowrap" ~width_style:"width: 85px; min-width: 85px;" p.avg_assists p.total_assists)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell whitespace-nowrap" ~width_style:"width: 80px; min-width: 80px;" p.avg_steals p.total_steals)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell whitespace-nowrap" ~width_style:"width: 80px; min-width: 80px;" p.avg_blocks p.total_blocks)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell whitespace-nowrap" ~width_style:"width: 80px; min-width: 80px;" p.avg_turnovers p.total_turnovers)
    (stat_cell ~highlight:true ~extra_classes:"whitespace-nowrap" ~width_style:"width: 80px; min-width: 80px;" p.efficiency)
    (stat_cell ~extra_classes:"hidden sm:table-cell whitespace-nowrap" ~width_style:"width: 80px; min-width: 80px;" per)
let find_substring_from ~sub:_ _ ~from:_ = None
let career_highs_card _ = "<!-- Career Highs Placeholder -->"

(* ===== Accessibility Helpers ===== *)

(** Create an accessible select with label *)
let a11y_select ~id ~label ?(hint="") ~options ~selected ?(on_change="") () =
  let hint_id = if hint = "" then "" else id ^ "-hint" in
  let aria_describedby = if hint = "" then "" else Printf.sprintf {| aria-describedby="%s"|} hint_id in
  let hint_html = if hint = "" then "" else
    Printf.sprintf {html|<span id="%s" class="text-xs text-slate-500 dark:text-slate-400">%s</span>|html} hint_id (escape_html hint)
  in
  let onchange_attr = if on_change = "" then "" else Printf.sprintf {| onchange="%s"|} on_change in
  let options_html = options |> List.map (fun (value, text) ->
    let sel = if value = selected then " selected" else "" in
    Printf.sprintf {html|<option value="%s"%s>%s</option>|html} (escape_html value) sel (escape_html text)
  ) |> String.concat "\n" in
  Printf.sprintf {html|
    <div class="flex flex-col gap-1">
      <label for="%s" class="text-xs font-bold text-slate-600 dark:text-slate-400 uppercase tracking-wider">%s</label>
      %s
      <select id="%s" name="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus-visible:border-orange-500 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/30"%s%s>
        %s
      </select>
    </div>|html}
    id (escape_html label) hint_html id id aria_describedby onchange_attr options_html

(** Create an accessible text input with label *)
let a11y_input ~id ~label ?(hint="") ?(placeholder="") ~value ?(input_type="text") () =
  let hint_id = if hint = "" then "" else id ^ "-hint" in
  let aria_describedby = if hint = "" then "" else Printf.sprintf {| aria-describedby="%s"|} hint_id in
  let hint_html = if hint = "" then "" else
    Printf.sprintf {html|<span id="%s" class="text-xs text-slate-500 dark:text-slate-400">%s</span>|html} hint_id (escape_html hint)
  in
  Printf.sprintf {html|
    <div class="flex flex-col gap-1">
      <label for="%s" class="text-xs font-bold text-slate-600 dark:text-slate-400 uppercase tracking-wider">%s</label>
      %s
      <input type="%s" id="%s" name="%s" value="%s" placeholder="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus-visible:border-orange-500 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/30"%s />
    </div>|html}
    id (escape_html label) hint_html input_type id id (escape_html value) (escape_html placeholder) aria_describedby

(** Create an accessible button *)
let a11y_button ~label ?(btn_type="submit") ?(variant=`Primary) ?(disabled=false) () =
  let base_cls = "px-4 py-2 rounded-lg font-bold transition-colors focus-visible:outline-none focus-visible:ring-2" in
  let variant_cls = match variant with
    | `Primary -> "bg-orange-600 hover:bg-orange-700 text-white focus-visible:ring-orange-500/50"
    | `Secondary -> "bg-slate-200 dark:bg-slate-700 hover:bg-slate-300 dark:hover:bg-slate-600 text-slate-700 dark:text-slate-200 focus-visible:ring-slate-500/50"
    | `Danger -> "bg-rose-600 hover:bg-rose-700 text-white focus-visible:ring-rose-500/50"
  in
  let disabled_cls = if disabled then " opacity-50 cursor-not-allowed" else "" in
  let disabled_attr = if disabled then " disabled" else "" in
  Printf.sprintf {html|<button type="%s" class="%s %s%s"%s>%s</button>|html}
    btn_type base_cls variant_cls disabled_cls disabled_attr (escape_html label)

(** Screen reader only text *)
let sr_only text =
  Printf.sprintf {html|<span class="sr-only">%s</span>|html} (escape_html text)

(* ===== Loading/Error UX Components ===== *)

(** Animated spinner component
    @param size "sm" | "md" | "lg"
    @param label Screen reader label
*)
let spinner ?(size="md") ?(label="로딩 중...") () =
  let size_cls = match size with
    | "sm" -> "w-4 h-4 border-2"
    | "lg" -> "w-12 h-12 border-4"
    | _ -> "w-8 h-8 border-3"
  in
  Printf.sprintf {html|<div class="inline-flex items-center gap-2" role="status" aria-live="polite">
    <div class="%s border-slate-200 dark:border-slate-700 border-t-orange-500 rounded-full animate-spin"></div>
    <span class="sr-only">%s</span>
  </div>|html} size_cls (escape_html label)

(** Loading placeholder with spinner *)
let loading_placeholder ?(message="데이터를 불러오는 중...") () =
  Printf.sprintf {html|<div class="flex flex-col items-center justify-center py-12 gap-4">
    %s
    <p class="text-slate-500 dark:text-slate-400 text-sm">%s</p>
  </div>|html} (spinner ~size:"lg" ()) (escape_html message)

(** Error state with retry button
    @param message Error message to display
    @param retry_url URL to reload (optional, uses JS reload if not provided)
*)
let error_with_retry ?(retry_url="") ~message () =
  let retry_action = if retry_url = "" then "location.reload()" else Printf.sprintf "location.href='%s'" retry_url in
  Printf.sprintf {html|<div class="flex flex-col items-center justify-center py-12 gap-4 text-center">
    <div class="text-5xl">😵</div>
    <div class="space-y-2">
      <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">문제가 발생했습니다</h3>
      <p class="text-slate-500 dark:text-slate-400 text-sm max-w-md">%s</p>
    </div>
    <button onclick="%s" class="mt-2 px-4 py-2 bg-orange-600 hover:bg-orange-700 text-white rounded-lg font-bold transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/50">
      🔄 다시 시도
    </button>
  </div>|html} (escape_html message) retry_action

(** Toast notification container (add to layout)
    JavaScript functions:
    - showToast(message, type) - type: "success" | "error" | "info"
    - hideToast()
*)
let toast_container () =
  {html|<div id="toast-container" class="fixed bottom-4 right-4 z-50 flex flex-col gap-2 pointer-events-none">
  </div>
  <style>
    .toast {
      pointer-events: auto;
      animation: toast-in 0.3s ease-out forwards;
    }
    .toast.hiding {
      animation: toast-out 0.3s ease-in forwards;
    }
    @keyframes toast-in {
      from { opacity: 0; transform: translateY(20px) scale(0.95); }
      to { opacity: 1; transform: translateY(0) scale(1); }
    }
    @keyframes toast-out {
      from { opacity: 1; transform: translateY(0) scale(1); }
      to { opacity: 0; transform: translateY(20px) scale(0.95); }
    }
  </style>
  <script>
    window.showToast = function(message, type) {
      type = type || 'info';
      var container = document.getElementById('toast-container');
      if (!container) return;

      var colors = {
        success: 'bg-emerald-600 text-white',
        error: 'bg-rose-600 text-white',
        info: 'bg-slate-800 text-white dark:bg-slate-200 dark:text-slate-800'
      };
      var icons = {
        success: '✅',
        error: '❌',
        info: 'ℹ️'
      };

      var toast = document.createElement('div');
      toast.className = 'toast px-4 py-3 rounded-lg shadow-lg flex items-center gap-3 ' + (colors[type] || colors.info);
      toast.innerHTML = '<span class="text-lg">' + (icons[type] || icons.info) + '</span><span class="font-medium">' + message + '</span>';
      toast.setAttribute('role', 'alert');
      toast.setAttribute('aria-live', 'assertive');

      container.appendChild(toast);

      // Auto hide after 4 seconds
      setTimeout(function() {
        toast.classList.add('hiding');
        setTimeout(function() { toast.remove(); }, 300);
      }, 4000);
    };

    window.hideAllToasts = function() {
      var container = document.getElementById('toast-container');
      if (container) container.innerHTML = '';
    };
  </script>|html}

(** HTMX loading indicator wrapper
    Shows spinner while HTMX request is in progress
*)
let htmx_loading_wrapper ~target_id content =
  Printf.sprintf {html|<div id="%s" class="relative">
    <div class="htmx-indicator absolute inset-0 bg-white/80 dark:bg-slate-900/80 flex items-center justify-center rounded-lg z-10">
      %s
    </div>
    %s
  </div>|html} target_id (spinner ()) content

(** Skeleton loading placeholder for cards *)
let skeleton_card () =
  {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 animate-pulse">
    <div class="h-4 bg-slate-200 dark:bg-slate-700 rounded w-3/4 mb-4"></div>
    <div class="space-y-2">
      <div class="h-3 bg-slate-200 dark:bg-slate-700 rounded w-full"></div>
      <div class="h-3 bg-slate-200 dark:bg-slate-700 rounded w-5/6"></div>
      <div class="h-3 bg-slate-200 dark:bg-slate-700 rounded w-4/6"></div>
    </div>
  </div>|html}

(** Skeleton loading placeholder for table rows *)
let skeleton_table_row ~cols =
  let cells = List.init cols (fun _ ->
    {html|<td class="px-3 py-2"><div class="h-4 bg-slate-200 dark:bg-slate-700 rounded animate-pulse"></div></td>|html}
  ) |> String.concat "" in
  Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800">%s</tr>|html} cells
