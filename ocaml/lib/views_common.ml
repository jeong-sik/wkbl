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
  (* 2. Render <tbody> - td classes must perfectly match th's responsive classes *)
  let tbody =
    rows_data
    |> List.map (fun row ->
        if List.length row <> List.length cols then
          Printf.sprintf "<!-- Row column count mismatch: expected %d, got %d -->" (List.length cols) (List.length row)
        else
          let cells =
            List.map2 (fun c data ->
              let base_cls = "px-3 py-2 whitespace-nowrap overflow-hidden truncate" in
              let align_cls = align_class c.align in
              let resp_cls = resp_class c.resp in
              let color_cls = if c.highlight then "text-orange-600 dark:text-orange-400 font-bold" else "text-slate-700 dark:text-slate-300" in
              Printf.sprintf {html|<td class="%s %s %s %s">%s</td>|html} base_cls align_cls resp_cls color_cls data
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

let empty_state ?icon title desc =
  let _ = icon in
  Printf.sprintf {html|<div class="text-center py-12 px-4"><div class="text-4xl mb-4">🏀</div><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</h3><p class="text-slate-500 dark:text-slate-400">%s</p></div>|html} title desc

let layout ~title ?(canonical_path="/") ?(description="") ?(json_ld="") ?og_title ?og_description ?og_image ?data_freshness ~content () =
  let _ = og_title in let _ = og_description in let _ = og_image in let _ = data_freshness in
  let _ = canonical_path in let _ = description in let _ = json_ld in
  Printf.sprintf {html|<!DOCTYPE html><html lang="ko"><head><meta charset="UTF-8"><title>%s</title><script src="https://cdn.tailwindcss.com"></script></head><body class="bg-slate-50 dark:bg-[#0b0e14] text-slate-900 dark:text-slate-200"><main id="main-content" class="max-w-7xl mx-auto px-4 py-6">%s</main></body></html>|html} title content

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
let player_row ?(show_player_id=false) ?(team_cell_class="px-3 py-2") ?(include_team=true) _ _ =
  let _ = show_player_id in let _ = team_cell_class in let _ = include_team in "<!-- Player Row Placeholder -->"
let find_substring_from ~sub:_ _ ~from:_ = None
let career_highs_card _ = "<!-- Career Highs Placeholder -->"