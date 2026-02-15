open Domain

(** Cache-busting version for static JS assets. Update on each deploy. *)
let static_version = "20260214"

(* Seed PRNG at module load for CSP nonce generation *)
let () = Random.self_init ()

(** Generate a per-request CSP nonce (32 hex chars = 128 bits). *)
let generate_csp_nonce () =
  let buf = Buffer.create 32 in
  for _ = 1 to 16 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Random.int 256))
  done;
  Buffer.contents buf

(** Replace all occurrences of [pattern] with [replacement] in [str].
    Pure OCaml, no global state (Eio-safe). *)
let replace_all_literal ~pattern ~replacement str =
  let plen = String.length pattern in
  if plen = 0 then str
  else
    let buf = Buffer.create (String.length str) in
    let i = ref 0 in
    let slen = String.length str in
    while !i <= slen - plen do
      if String.sub str !i plen = pattern then begin
        Buffer.add_string buf replacement;
        i := !i + plen
      end else begin
        Buffer.add_char buf (String.get str !i);
        i := !i + 1
      end
    done;
    if !i < slen then
      Buffer.add_string buf (String.sub str !i (slen - !i));
    Buffer.contents buf

let escape_html s =
  (* Fast path: return original string if no escaping needed (common case) *)
  let needs_escape = ref false in
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | '<' | '>' | '&' | '"' | '\'' -> needs_escape := true
    | _ -> ()
  done;
  if not !needs_escape then s
  else begin
    let buf = Buffer.create (String.length s + 8) in
    String.iter (function
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#x27;"
      | c -> Buffer.add_char buf c) s;
    Buffer.contents buf
  end

(** URL-safe href for player pages. Always pct-encodes the ID. *)
let player_href id = "/player/" ^ Uri.pct_encode id

(** URL-safe href for team pages. Always pct-encodes the team name. *)
let team_href name = "/team/" ^ Uri.pct_encode name

(** URL-safe href for boxscore pages. Always pct-encodes the game id. *)
let boxscore_href game_id = "/boxscore/" ^ Uri.pct_encode game_id

(** URL-safe href for boxscore PBP pages. Always pct-encodes the game id. *)
let boxscore_pbp_href game_id = "/boxscore/" ^ Uri.pct_encode game_id ^ "/pbp"

(** URL-safe href for season pages. Always pct-encodes the season code. *)
let season_href code = "/season/" ^ Uri.pct_encode code

let escape_js_string s =
  (* Defensive escaping for inline <script> blocks. *)
  s
  |> String.to_seq
  |> Seq.map (function
    | '\\' -> "\\\\"
    | '"' -> "\\\""
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    (* Avoid accidentally closing the script tag in case env vars contain "<" *)
    | '<' -> "\\x3C"
    | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""

(** Compute per-quarter scoring from cumulative quarter_score list.
    Returns (period, home_q_pts, away_q_pts) tuples in O(n). *)
let quarter_deltas (quarters : Domain.quarter_score list) =
  let rec go prev_h prev_a = function
    | [] -> []
    | (q : Domain.quarter_score) :: rest ->
      let h = q.qs_home_score - prev_h in
      let a = q.qs_away_score - prev_a in
      (q.qs_period, h, a) :: go q.qs_home_score q.qs_away_score rest
  in
  go 0 0 quarters

let env_nonempty name =
  match Sys.getenv_opt name with
  | None -> None
  | Some v ->
      let t = String.trim v in
      if t = "" then None else Some t

let sentry_public_key_of_dsn dsn =
  match Uri.userinfo (Uri.of_string dsn) with
  | None -> None
  | Some ui ->
      let public =
        match String.split_on_char ':' ui with
        | pub :: _ -> pub
        | [] -> ""
      in
      let t = String.trim public in
      if t = "" then None else Some t

type empty_state_icon = BasketballIcon | SearchIcon | ChartIcon | UsersIcon | TableIcon

type col_spec = {
  header : string;
  width : int option;
  align : [ `Left | `Right | `Center ];
  resp : [ `Always | `Hidden_sm | `Hidden_md | `Hidden_lg | `Hidden_xl ];
  sort : string option;
  title : string option;
  highlight : bool;
  sticky : bool;  (** Sticky left column — stays fixed on horizontal scroll *)
  numeric : bool;  (** Numeric column — applies tabular-nums font-mono *)
}

let col ?(w=None) ?(align=`Left) ?(resp=`Always) ?sort ?title ?(highlight=false) ?(sticky=false) ?numeric header =
  let numeric = match numeric with
    | Some v -> v
    | None -> align = `Right  (* Right-aligned columns default to numeric *)
  in
  { header; width = w; align; resp; sort; title; highlight; sticky; numeric }

let px w = Some w

(** Render a fixed-layout table (Perfectly Aligned via th-style & Data-Oriented)

    Optional parameters:
    - [table_attrs]: extra HTML attributes on the <table> element.
    - [aria_label]: override the default "Data Table" aria-label.
    - [striped]: alternate row backgrounds (default: false).
    - [foot_data]: rows for <tfoot> (same column layout as body rows).
*)
let render_fixed_table ?(table_attrs="") ?(aria_label="Data Table") ?(striped=true) ?(foot_data=[]) ~id ~min_width ~(cols : col_spec list) (rows_data : string list list) =
  let resp_class = function
    | `Always -> ""
    | `Hidden_sm -> "hidden sm:table-cell"
    | `Hidden_md -> "hidden md:table-cell"
    | `Hidden_lg -> "hidden lg:table-cell"
    | `Hidden_xl -> "hidden xl:table-cell"
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
          | Some w -> Printf.sprintf "width: %dpx;" w
          | None -> "width: auto;"
        in
        let base_cls = "px-3 py-2 font-sans whitespace-nowrap" in
        let align_cls = align_class c.align in
        let resp_cls = resp_class c.resp in
        let highlight_cls = if c.highlight then "text-orange-700 dark:text-orange-400" else "" in
        let sticky_cls = if c.sticky then "sticky left-0 z-20 bg-slate-100 dark:bg-slate-800/80" else "" in
        let full_cls = String.concat " " [base_cls; align_cls; resp_cls; highlight_cls; sticky_cls] in
        let sort_attr =
          match c.sort with
          | Some k -> Printf.sprintf " data-sortable data-sort-key=\"%s\" role=\"button\" tabindex=\"0\"" k
          | None -> ""
        in
        let title_attr =
          match c.title with
          | Some t -> Printf.sprintf " title=\"%s\"" (escape_html t)
          | None -> ""
        in
        Printf.sprintf {html|<th scope="col" class="%s" style="%s"%s%s>%s</th>|html}
          full_cls width_style sort_attr title_attr (escape_html c.header))
    |> String.concat "\n"
    |> fun s -> Printf.sprintf {html|<thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr>%s</tr></thead>|html} s
  in
  let render_row ~is_foot i row =
    if List.length row <> List.length cols then
      "<!-- Row column count mismatch -->"
    else
      let cells =
        List.map2 (fun c data ->
          let width_style =
            match c.width with
            | Some w -> Printf.sprintf "width: %dpx;" w
            | None -> "width: auto;"
          in
          let base_cls = "px-3 py-2 whitespace-nowrap overflow-hidden truncate" in
          let align_cls = align_class c.align in
          let resp_cls = resp_class c.resp in
          let color_cls =
            if is_foot then "text-slate-900 dark:text-slate-100 font-bold"
            else if c.highlight then "text-orange-700 dark:text-orange-400 font-bold"
            else "text-slate-700 dark:text-slate-300"
          in
          let sticky_cls =
            if c.sticky then
              "sticky left-0 z-10 bg-white dark:bg-slate-900 group-hover:bg-slate-100 dark:group-hover:bg-slate-800/50"
            else ""
          in
          let numeric_cls = if c.numeric then "tabular-nums font-mono" else "" in
          let full_cls = String.concat " " [base_cls; align_cls; resp_cls; color_cls; sticky_cls; numeric_cls] in
          Printf.sprintf {html|<td class="%s" style="%s">%s</td>|html} full_cls width_style data
        ) cols row
        |> String.concat ""
      in
      let stripe_cls =
        if is_foot then "bg-slate-50 dark:bg-slate-800/40 border-t-2 border-slate-300 dark:border-slate-700"
        else if striped && i mod 2 = 1 then "bg-slate-50/50 dark:bg-slate-800/20"
        else ""
      in
      Printf.sprintf {html|<tr class="%s border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors group">%s</tr>|html}
        stripe_cls cells
  in
  (* 2. Render <tbody> - Apply SAME width style to td to enforce alignment *)
  let tbody =
    rows_data
    |> List.mapi (fun i row -> render_row ~is_foot:false i row)
    |> String.concat "\n"
    |> fun s -> Printf.sprintf {html|<tbody>%s</tbody>|html} s
  in
  (* 3. Render optional <tfoot> *)
  let tfoot =
    match foot_data with
    | [] -> ""
    | rows ->
      rows
      |> List.mapi (fun i row -> render_row ~is_foot:true i row)
      |> String.concat "\n"
      |> fun s -> Printf.sprintf {html|<tfoot>%s</tfoot>|html} s
  in
  let has_sortable = List.exists (fun c -> c.sort <> None) cols in
  let extra_table_attrs =
    let t = String.trim table_attrs in
    let sortable_attr = if has_sortable then "data-sortable" else "" in
    let parts = List.filter (fun s -> s <> "") [t; sortable_attr] in
    match parts with
    | [] -> ""
    | _ -> " " ^ (String.concat " " parts)
  in
  (* CSV export button *)
  let csv_btn =
    Printf.sprintf
      {html|<div class="flex justify-end px-2 py-1">
  <button class="csv-export-btn text-xs text-slate-500 hover:text-orange-500 cursor-pointer transition-colors" data-table-id="%s" aria-label="Download CSV">CSV</button>
</div>|html} id
  in
  (* Assemble *)
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl scroll-shadow">
  %s
  <table id="%s"%s class="w-full %s text-xs sm:text-sm font-mono tabular-nums table-fixed" style="border-collapse: separate; border-spacing: 0;" aria-label="%s">
    %s
    %s
    %s
  </table>
</div>|html}
    csv_btn id extra_table_attrs min_width (escape_html aria_label) thead tbody tfoot

let normalize_name s = Domain.normalize_label s
let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

(** Extract first 4-digit year from a string, e.g. "1999-01-02" -> 1999 *)
let extract_year (s: string) =
  let len = String.length s in
  let is_digit c = c >= '0' && c <= '9' in
  let rec loop i =
    if i + 3 >= len then None
    else if is_digit s.[i] && is_digit s.[i+1] && is_digit s.[i+2] && is_digit s.[i+3] then
      Some (int_of_string (String.sub s i 4))
    else
      loop (i + 1)
  in
  loop 0

(** Build disambiguation line for duplicate names *)
let player_disambiguation_line ~team_name ~player_id (info_opt: player_info option) =
  let parts = ref [] in
  let team = normalize_name team_name |> String.trim in
  if team <> "" then parts := !parts @ [team];
  let has_extra =
    match info_opt with
    | None -> false
    | Some info ->
        (match info.position with Some p when String.trim p <> "" -> true | _ -> false)
        || (match info.birth_date with Some d -> extract_year d |> Option.is_some | None -> false)
        || (match info.height with Some h when h > 0 -> true | _ -> false)
  in
  (match info_opt with
  | Some info ->
      (match info.position with
      | Some p when String.trim p <> "" -> parts := !parts @ [String.uppercase_ascii p]
      | _ -> ());
      (match info.birth_date with
      | Some d -> (match extract_year d with Some y -> parts := !parts @ [string_of_int y] | None -> ())
      | None -> ());
      (match info.height with
      | Some h when h > 0 -> parts := !parts @ [Printf.sprintf "%dcm" h]
      | _ -> ())
  | None -> ());
  if not has_extra then parts := !parts @ [Printf.sprintf "고유번호 %s" player_id];
  let text = String.concat " · " !parts in
  let title_text = Printf.sprintf "%s · 고유번호 %s" text player_id in
  Printf.sprintf
    {html|<div class="text-[10px] text-slate-500 dark:text-slate-400 font-mono leading-tight break-words" title="%s">%s</div>|html}
    (escape_html title_text)
    (escape_html text)

(** Parse Tailwind "w-N h-N" classes to pixel dimensions (N * 4). *)
let tw_size_to_px class_name =
  let parts = String.split_on_char ' ' class_name in
  let find prefix =
    List.find_map (fun p ->
      let plen = String.length prefix in
      if String.length p > plen && String.sub p 0 plen = prefix then
        int_of_string_opt (String.sub p plen (String.length p - plen))
        |> Option.map (fun n -> n * 4)
      else None
    ) parts
  in
  match find "w-", find "h-" with
  | Some w, Some h -> Some (w, h)
  | _ -> None

let team_logo_tag ?(class_name="w-8 h-8") team_name =
  let logo_file = match Domain.team_code_of_string team_name with | Some code -> Domain.team_code_to_logo code | None -> None in
  let dims = match tw_size_to_px class_name with
    | Some (w, h) -> Printf.sprintf " width=\"%d\" height=\"%d\"" w h
    | None -> ""
  in
  match logo_file with
  | Some f -> Printf.sprintf {html|<img src="/static/images/%s" alt="%s 로고" class="%s object-contain" loading="lazy"%s>|html} f (escape_html team_name) class_name dims
  | None -> Printf.sprintf {html|<div class="%s bg-slate-100 dark:bg-slate-800 rounded flex items-center justify-center text-xs">🏀</div>|html} class_name

let breadcrumb (crumbs: (string * string) list) =
  if crumbs = [] then "" else
  let sep = {|<svg class="w-3 h-3 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"/></svg>|} in
  let n = List.length crumbs in
  let items = crumbs |> List.mapi (fun i (label, href) ->
    let is_last = (i = n - 1) in
    if is_last then
      Printf.sprintf {|<span class="text-slate-900 dark:text-slate-200 font-medium" aria-current="page">%s</span>|} (escape_html label)
    else
      Printf.sprintf {|<a href="%s" class="text-slate-500 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>|}
        (escape_html href) (escape_html label)
  ) in
  let inner = String.concat (Printf.sprintf {| <span class="mx-1">%s</span> |} sep) items in
  (* Schema.org BreadcrumbList JSON-LD *)
  let ld_items = crumbs |> List.mapi (fun i (label, href) ->
    let url = if href = "" then "https://wkbl.win" else "https://wkbl.win" ^ href in
    Printf.sprintf {|{"@type":"ListItem","position":%d,"name":"%s","item":"%s"}|}
      (i + 1) (escape_html label) (escape_html url)
  ) |> String.concat "," in
  let ld_script = Printf.sprintf
    {|<script type="application/ld+json">{"@context":"https://schema.org","@type":"BreadcrumbList","itemListElement":[%s]}</script>|}
    ld_items
  in
  Printf.sprintf {|<nav aria-label="Breadcrumb" class="text-sm font-sans mb-4 flex items-center flex-wrap gap-y-1">%s</nav>%s|} inner ld_script

let team_badge ?(max_width="max-w-[130px]") team_name =
  let display = normalize_name team_name in
  let color = Domain.team_code_of_string display |> Option.map Domain.team_code_to_color |> Option.value ~default:"#666" in
  Printf.sprintf {html|<a href="%s" class="%s inline-flex min-w-0 items-center gap-1.5 px-1.5 py-0.5 rounded text-[11px] font-medium transition backdrop-blur-sm" style="background-color: %s20; color: %s; border: 1px solid %s40">%s<span class="truncate">%s</span></a>|html} (team_href team_name) max_width color color color (team_logo_tag ~class_name:"w-5 h-5 shrink-0" display) (escape_html display)

let player_img_tag ?(class_name="w-12 h-12") player_id name =
  let remote_src = Printf.sprintf "https://www.wkbl.or.kr/static/images/player/pimg/m_%s.jpg" player_id in
  let dims = match tw_size_to_px class_name with
    | Some (w, h) -> Printf.sprintf " width=\"%d\" height=\"%d\"" w h
    | None -> ""
  in
  Printf.sprintf {html|<img src="%s" alt="%s" class="%s rounded-full object-cover bg-slate-100 border border-slate-300" loading="lazy" onerror="this.src='/static/images/player_placeholder.svg'"%s>|html}
    remote_src
    (escape_html name)
    class_name
    dims

let player_id_badge player_id =
  Printf.sprintf
    {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/50 border border-slate-300 dark:border-slate-700/50 text-[10px] font-mono text-slate-500">고유번호 %s</span>|html}
    (escape_html player_id)

let format_int_commas n =
  let s = string_of_int (abs n) in
  let len = String.length s in
  let buf = Buffer.create (len + len / 3) in
  if n < 0 then Buffer.add_char buf '-';
  String.iteri (fun i c ->
    if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
    Buffer.add_char buf c
  ) s;
  Buffer.contents buf
let format_int_compact n = if abs n < 1000 then string_of_int n else Printf.sprintf "%.1fK" (float_of_int n /. 1000.0)

(** Player name cell content for render_fixed_table.
    Returns inner HTML (no <td> wrapper) with image, name link, and optional ID badge. *)
let player_name_cell ?(show_player_id=false) player_id name =
  let id_badge = if show_player_id then player_id_badge player_id else "" in
  let display_name = normalize_name name in
  Printf.sprintf
    {html|<div class="flex items-center gap-3 min-w-0 font-sans">%s<div class="flex flex-col min-w-0"><div class="flex items-center gap-2 min-w-0"><a href="%s" class="player-name hover:text-orange-600 dark:hover:text-orange-400 text-slate-900 dark:text-white font-medium transition-colors truncate break-keep min-w-0">%s</a><span class="%s">%s</span></div></div></div>|html}
    (player_img_tag ~class_name:"w-8 h-8 shrink-0" player_id name)
    (player_href player_id)
    (escape_html display_name)
    (if show_player_id then "inline-flex" else "hidden")
    id_badge

let points_total_cell ?(extra_classes="") ?(width_style="") avg total =
  let classes = String.concat " " ["px-3 py-2 text-right"; extra_classes] in
  Printf.sprintf
    {html|<td class="%s" style="%s"><div class="flex flex-col items-end leading-tight"><span class="text-orange-700 dark:text-orange-400 font-bold font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-500 text-[9px] font-mono whitespace-nowrap" title="누적">누적%s</span></div></td>|html}
    classes
    width_style
    avg
    (format_int_compact total)

let stat_total_cell ?(extra_classes="") ?(width_style="") avg total =
  let classes = String.concat " " ["px-3 py-2 text-right"; extra_classes] in
  Printf.sprintf
    {html|<td class="%s" style="%s"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-500 text-[9px] font-mono whitespace-nowrap" title="누적">누적%s</span></div></td>|html}
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
  let classes = String.concat " " ["px-3 py-2 text-right font-mono tabular-nums"; extra_classes; cls] in
  Printf.sprintf {html|<td class="%s" style="%s">%s</td>|html} classes width_style (escape_html value_str)

let stat_cell ?(highlight=false) ?(extra_classes="") ?(width_style="") value =
  let color_cls =
    if highlight then "text-orange-700 dark:text-orange-400 font-bold"
    else "text-slate-700 dark:text-slate-300"
  in
  let classes = String.concat " " ["px-3 py-2 text-right font-mono tabular-nums"; extra_classes; color_cls] in
  Printf.sprintf {html|<td class="%s" style="%s">%.1f</td>|html} classes width_style value

let empty_state ?icon title desc =
  let _ = icon in
  Printf.sprintf {html|<div class="text-center py-12 px-4"><div class="text-4xl mb-4">🏀</div><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</h3><p class="text-slate-500 dark:text-slate-400">%s</p></div>|html} title desc

let layout ?(lang=I18n.Ko) ~title ?(canonical_path="/") ?(description="") ?(json_ld="") ?og_title ?og_description ?og_image ?data_freshness ~content () =
  let json_ld_html = if json_ld = "" then "" else
    Printf.sprintf {|<script type="application/ld+json">%s</script>|} json_ld
  in

  let tr = I18n.t lang in
  let html_lang = I18n.html_lang lang in

  (* SEO: canonical, description, OG tags *)
  let seo_html =
    let canonical = Printf.sprintf {|<link rel="canonical" href="https://wkbl.win%s">|} (escape_html canonical_path) in
    let desc = if description = "" then "" else
      Printf.sprintf {|<meta name="description" content="%s">|} (escape_html description) in
    let og_t = match og_title with
      | Some t -> Printf.sprintf {|<meta property="og:title" content="%s">|} (escape_html t)
      | None -> Printf.sprintf {|<meta property="og:title" content="%s">|} (escape_html title) in
    let og_d = match og_description with
      | Some d -> Printf.sprintf {|<meta property="og:description" content="%s">|} (escape_html d)
      | None -> if description = "" then "" else
        Printf.sprintf {|<meta property="og:description" content="%s">|} (escape_html description) in
    let og_url = Printf.sprintf {|<meta property="og:url" content="https://wkbl.win%s">|} (escape_html canonical_path) in
    String.concat "\n  " [canonical; desc; og_t; og_d; og_url;
      {|<meta property="og:type" content="website">|};
      {|<meta property="og:site_name" content="WKBL Analytics">|};
      {|<meta name="twitter:card" content="summary_large_image">|}]
  in

  let og_img_html = match og_image with
    | Some url -> Printf.sprintf {html|<meta property="og:image" content="%s"><meta name="twitter:image" content="%s">|html} url url
    | None -> {html|<meta property="og:image" content="https://wkbl.win/static/images/og-main.png">|html}
  in

  let freshness_html = match data_freshness with
    | Some ts -> Printf.sprintf {|<span class="hidden sm:inline-flex items-center text-xs text-slate-500 dark:text-slate-400" data-freshness="%s"></span>|} ts
    | None -> ""
  in

  let observability_html =
    let sentry_html =
      match env_nonempty "SENTRY_DSN" with
      | None -> ""
      | Some dsn ->
          (match sentry_public_key_of_dsn dsn with
          | None -> ""
          | Some public_key ->
              let env_opt = env_nonempty "SENTRY_ENVIRONMENT" in
              let release_opt =
                match env_nonempty "SENTRY_RELEASE" with
                | Some r -> Some r
                | None -> env_nonempty "RAILWAY_GIT_COMMIT_SHA"
              in
              let opts =
                [ ("dsn", dsn) ]
                @ (match env_opt with Some e -> [ ("environment", e) ] | None -> [])
                @ (match release_opt with Some r -> [ ("release", r) ] | None -> [])
              in
              let opts_js =
                opts
                |> List.map (fun (k, v) -> Printf.sprintf {|%s: "%s"|} k (escape_js_string v))
                |> String.concat ",\n          "
              in
              Printf.sprintf {html|
  <script src="https://js.sentry-cdn.com/%s.min.js" crossorigin="anonymous"></script>
  <script>
    (function() {
      if (!window.Sentry || !Sentry.onLoad) return;
      Sentry.onLoad(function() {
        Sentry.init({
          %s
        });
      });
    })();
  </script>|html}
                (escape_html public_key)
                opts_js)
    in

    let clarity_html =
      match env_nonempty "CLARITY_PROJECT_ID" with
      | None -> ""
      | Some pid ->
          Printf.sprintf {html|
  <script type="text/javascript">
    (function(c,l,a,r,i,t,y){
      c[a]=c[a]||function(){(c[a].q=c[a].q||[]).push(arguments)};
      t=l.createElement(r);t.async=1;t.src="https://www.clarity.ms/tag/"+i;
      y=l.getElementsByTagName(r)[0];y.parentNode.insertBefore(t,y);
    })(window, document, "clarity", "script", "%s");
  </script>|html} (escape_js_string pid)
    in

    sentry_html ^ clarity_html
  in

  let nav_players = tr { ko = "선수"; en = "Players" } in
  let nav_teams = tr { ko = "팀"; en = "Teams" } in
  let nav_standings = tr { ko = "순위"; en = "Standings" } in
  let nav_games = tr { ko = "경기"; en = "Games" } in
  let nav_compare = tr { ko = "비교"; en = "Compare" } in
  let nav_predict = tr { ko = "예측"; en = "Predict" } in

  let skip_to_content = tr { ko = "본문으로 건너뛰기"; en = "Skip to content" } in
  let aria_toggle_theme = tr { ko = "다크모드 전환"; en = "Toggle theme" } in
  let aria_open_menu = tr { ko = "메뉴 열기"; en = "Open menu" } in

  let sr_dark_on = tr { ko = "다크 모드 활성화"; en = "Dark mode on" } in
  let sr_dark_off = tr { ko = "라이트 모드 활성화"; en = "Light mode on" } in
  let sr_menu_open = tr { ko = "메뉴 열림"; en = "Menu opened" } in
  let sr_menu_close = tr { ko = "메뉴 닫힘"; en = "Menu closed" } in

  let nav_search = tr { ko = "검색"; en = "Search" } in
  let nav_search_placeholder = tr { ko = "검색..."; en = "Search..." } in
  let lang_label = tr { ko = "언어"; en = "Language" } in
  let lang_menu =
    let item_cls is_active =
      if is_active then
        "bg-orange-500/10 text-orange-700 dark:text-orange-300 border-orange-500/30"
      else
        "bg-white dark:bg-slate-900 text-slate-700 dark:text-slate-300 border-slate-200 dark:border-slate-700 hover:bg-slate-50 dark:hover:bg-slate-800/60"
    in
    Printf.sprintf
      {html|<details class="relative">
        <summary class="list-none cursor-pointer select-none px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 text-xs font-bold text-slate-700 dark:text-slate-300 border border-slate-200 dark:border-slate-700" aria-label="%s">%s</summary>
        <div class="absolute right-0 mt-2 w-32 rounded-lg border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 shadow-lg overflow-hidden">
          <a href="/lang/ko" class="block px-3 py-2 text-sm border-b border-slate-200 dark:border-slate-800 %s">한국어</a>
          <a href="/lang/en" class="block px-3 py-2 text-sm %s">English</a>
        </div>
      </details>|html}
      (escape_html lang_label)
      (escape_html lang_label)
      (item_cls (lang = I18n.Ko))
      (item_cls (lang = I18n.En))
  in

  let html = Printf.sprintf {html|<!DOCTYPE html>
<html lang="%s">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  %s
  %s
  <link rel="manifest" href="/manifest.json">
  <link rel="preconnect" href="https://cdn.tailwindcss.com" crossorigin>
  <link rel="preconnect" href="https://cdn.jsdelivr.net" crossorigin>
  <script src="https://cdn.tailwindcss.com"></script>
  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
  <script>
    tailwind.config = {
      darkMode: 'class'
    }
    // Global Chart.js defaults
    Chart.defaults.font.family = 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace';
    Chart.defaults.color = '#94a3b8';
    Chart.defaults.scale.grid.color = '#334155';
  </script>
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
    @keyframes pulse {
      from { opacity: 1; }
      to { opacity: 0.5; }
    }
    .animate-pulse { animation: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite; }

    /* Live Badge Animations */
    @keyframes live-pulse {
      from { transform: scale(0.95); box-shadow: 0 0 0 0 rgba(239, 68, 68, 0.7); }
      to { transform: scale(1); box-shadow: 0 0 0 6px rgba(239, 68, 68, 0); }
    }
    .live-dot {
      display: inline-block;
      width: 8px;
      height: 8px;
      background-color: #ef4444;
      border-radius: 50%%;
      margin-right: 6px;
    }
    .live-badge {
      display: inline-flex;
      items-center: center;
      padding: 2px 8px;
      background-color: rgba(239, 68, 68, 0.1);
      color: #ef4444;
      border: 1px solid rgba(239, 68, 68, 0.2);
      border-radius: 9999px;
      font-size: 10px;
      font-weight: 800;
      letter-spacing: 0.05em;
      animation: live-pulse 2s infinite;
    }
	  </style>
	  %s
</head>
<body class="bg-slate-50 dark:bg-[#0b0e14] text-slate-900 dark:text-slate-200">
  <!-- Skip to main content link for keyboard users -->
  <a href="#main-content" class="skip-link">%s</a>

  <!-- ARIA live region for dynamic content announcements -->
  <div id="aria-live" aria-live="polite" aria-atomic="true" class="sr-only"></div>

  <!-- Header with navigation and dark mode toggle -->
  <header class="bg-white dark:bg-slate-900 border-b border-slate-200 dark:border-slate-800 sticky top-0 z-40">
	    <nav class="max-w-7xl mx-auto px-4 h-14 flex items-center justify-between">
		      <div class="flex items-center gap-4">
	        <a href="/" class="flex items-center gap-2 font-bold text-lg text-orange-700 dark:text-orange-400">
	          <span>🏀</span>
	          <span>WKBL</span>
	        </a>
		        <div class="hidden md:flex items-center gap-4 text-sm">
		          <a href="/players" class="text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
		          <a href="/teams" class="text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
		          <a href="/standings" class="text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
		          <a href="/games" class="text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
		          <a href="/compare" class="text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
		          <a href="/predict" class="text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
	      </div>
      </div>
      <div class="flex items-center gap-2">
        <button type="button" onclick="window.SearchModal&&SearchModal.open()" aria-label="%s (Cmd+K)" class="hidden md:flex items-center gap-2 px-3 py-1.5 rounded-lg bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 border border-slate-200 dark:border-slate-700 transition-colors group text-sm">
          <svg class="w-4 h-4 text-slate-400 group-hover:text-orange-500" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/></svg>
          <span class="text-slate-400">%s</span>
          <kbd class="ml-1 px-1.5 py-0.5 text-[10px] font-mono text-slate-400 bg-white dark:bg-slate-700 rounded border border-slate-200 dark:border-slate-600">⌘K</kbd>
        </button>
        <button type="button" onclick="window.SearchModal&&SearchModal.open()" aria-label="%s" class="md:hidden p-2 rounded-lg bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 transition-colors group">
          <svg class="w-5 h-5 text-slate-500 dark:text-slate-400 group-hover:text-orange-500" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/></svg>
        </button>
        %s
        %s
        <button id="theme-toggle" type="button" aria-label="%s" aria-pressed="false" data-sr-on="%s" data-sr-off="%s" class="p-2 rounded-lg bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 transition-colors">
	          <svg id="theme-icon-light" class="w-5 h-5 hidden dark:block text-yellow-400" fill="currentColor" viewBox="0 0 20 20"><path d="M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z"/></svg>
	          <svg id="theme-icon-dark" class="w-5 h-5 block dark:hidden text-slate-600" fill="currentColor" viewBox="0 0 20 20"><path d="M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"/></svg>
	        </button>
	        <button id="mobile-menu-toggle" type="button" aria-label="%s" data-sr-open="%s" data-sr-close="%s" class="md:hidden p-2 rounded-lg bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 transition-colors">
          <svg class="w-5 h-5 text-slate-600 dark:text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"/></svg>
        </button>
      </div>
    </nav>
    <!-- Mobile menu -->
		    <div id="mobile-menu" class="hidden md:hidden border-t border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900">
		      <div class="px-4 py-3 space-y-2">
		        <a href="/players" class="block py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors">%s</a>
		        <a href="/teams" class="block py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors">%s</a>
		        <a href="/standings" class="block py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors">%s</a>
		        <a href="/games" class="block py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors">%s</a>
		        <a href="/compare" class="block py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors">%s</a>
		        <a href="/predict" class="block py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors">%s</a>
		        <button type="button" onclick="window.SearchModal&&SearchModal.open();document.getElementById('mobile-menu').classList.add('hidden')" class="w-full text-left py-3 px-4 rounded-lg text-base font-medium text-slate-700 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors flex items-center gap-2">
		          <svg class="w-5 h-5 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"/></svg>
		          %s
		        </button>
		      </div>
		    </div>
		  </header>

  <main id="main-content" role="main" tabindex="-1" class="max-w-7xl mx-auto px-4 py-6">
    %s
  </main>

  <footer class="mt-auto border-t border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900/50 py-8">
    <div class="max-w-7xl mx-auto px-4 flex flex-col md:flex-row items-center justify-between gap-4">
      <div class="flex items-center gap-2 font-bold text-slate-400 dark:text-slate-600">
        <span>🏀</span>
        <span>WKBL.win</span>
      </div>
      <div class="flex items-center gap-6 text-sm text-slate-500 dark:text-slate-400">
        <a href="mailto:contact@wkbl.win" class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors flex items-center gap-2">
          <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"/></svg>
          contact@wkbl.win
        </a>
      </div>
      <div class="text-[11px] text-slate-400 dark:text-slate-600 font-mono">
        &copy; 2026 WKBL.win
      </div>
    </div>
  </footer>

  <!-- Toast notification container -->
  <div id="toast-container" class="fixed bottom-4 right-4 z-50 flex flex-col gap-2" aria-live="polite"></div>

  <script src="/static/js/app-init.js?v=20260214"></script>
		  <script src="/static/js/htmx-1.9.10.min.js"></script>
			  <script src="/static/js/page-transitions.js?v=20260214"></script>
			  <script src="/static/js/scroll-shadow.js?v=20260214"></script>
			  <script src="/static/js/table-sort.js?v=20260214"></script>
			  <script src="/static/js/table-export.js?v=20260214"></script>
			  <script src="/static/js/table-row-link.js?v=20260214"></script>
			  <script src="/static/js/number-format.js?v=20260214"></script>
			  <script src="/static/js/a11y-utils.js?v=20260214"></script>
			  <script src="/static/js/skeleton-loader.js?v=20260214"></script>
			  <script src="/static/js/data-freshness.js?v=20260214"></script>
			  <script src="/static/js/search-modal.js?v=20260214"></script>
			  <script>if('serviceWorker' in navigator){navigator.serviceWorker.register('/sw.js')}</script>
				</body>
			</html>|html}
	    (escape_html html_lang)
	    title
	    seo_html
	    og_img_html
	    (observability_html ^ json_ld_html)
	    (escape_html skip_to_content)
	    (escape_html nav_players)
	    (escape_html nav_teams)
	    (escape_html nav_standings)
	    (escape_html nav_games)
	    (escape_html nav_compare)
		    (escape_html nav_predict)
		    (escape_html nav_search)
		    (escape_html nav_search_placeholder)
		    (escape_html nav_search)
		    freshness_html
		    lang_menu
		    (escape_html aria_toggle_theme)
		    (escape_html sr_dark_on)
		    (escape_html sr_dark_off)
		    (escape_html aria_open_menu)
		    (escape_html sr_menu_open)
		    (escape_html sr_menu_close)
		    (escape_html nav_players)
		    (escape_html nav_teams)
	    (escape_html nav_standings)
	    (escape_html nav_games)
	    (escape_html nav_compare)
	    (escape_html nav_predict)
	    (escape_html nav_search)
	    content
  in
  (* CSP nonce injection: generate per-request nonce, inject into all opening
     <script tags, and prepend a CSP meta tag in <head>. *)
  let nonce = generate_csp_nonce () in
  let has_sentry = env_nonempty "SENTRY_DSN" <> None in
  let has_clarity = env_nonempty "CLARITY_PROJECT_ID" <> None in
  let csp_script_extras =
    (if has_sentry then " https://js.sentry-cdn.com" else "")
    ^ (if has_clarity then " https://www.clarity.ms" else "")
  in
  let csp_connect_extras =
    (if has_sentry then " https://*.sentry.io" else "")
    ^ (if has_clarity then " https://www.clarity.ms" else "")
  in
  let csp_policy = Printf.sprintf
    "default-src 'self'; \
     script-src 'self' 'nonce-%s' https://cdn.tailwindcss.com https://cdn.jsdelivr.net%s; \
     style-src 'self' 'unsafe-inline'; \
     img-src 'self' data: https:; \
     connect-src 'self'%s; \
     font-src 'self'"
    nonce csp_script_extras csp_connect_extras
  in
  let csp_meta = Printf.sprintf
    {|<meta http-equiv="Content-Security-Policy" content="%s">|}
    (escape_html csp_policy)
  in
  (* Inject nonce into opening <script> and <script  tags. Careful not to
     touch </script> closing tags — only replace "<script>" and "<script ". *)
  let html = replace_all_literal ~pattern:"<script>" ~replacement:(Printf.sprintf "<script nonce=\"%s\">" nonce) html in
  let html = replace_all_literal ~pattern:"<script " ~replacement:(Printf.sprintf "<script nonce=\"%s\" " nonce) html in
  (* Insert CSP meta tag right after <meta charset="UTF-8"> *)
  replace_all_literal ~pattern:{|<meta charset="UTF-8">|}
    ~replacement:(Printf.sprintf {|<meta charset="UTF-8">
  %s|} csp_meta) html

let eff_badge ?(show_label=false) eff =
  let color_cls = if eff >= 20.0 then "bg-orange-500/10 text-orange-700 dark:text-orange-400 border-orange-500/30"
                  else "bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400 border-slate-300 dark:border-slate-700" in
  let label = if show_label then "EFF " else "" in
  Printf.sprintf {html|<span class="px-2 py-0.5 rounded border text-[10px] font-mono font-bold %s">%s%.1f</span>|html} color_cls label eff

let score_quality_badge ?(lang=I18n.Ko) ?(compact=false) q =
  let tr = I18n.t lang in
  let label_verified = tr { ko = "일치"; en = "Match" } in
  let label_derived = tr { ko = "추정"; en = "Estimated" } in
  let label_mismatch = tr { ko = "불일치"; en = "Mismatch" } in
  let pad_cls = if compact then "px-1.5" else "px-2" in
  let mk cls label =
    Printf.sprintf
      "<span class=\"%s py-0.5 rounded border text-[10px] font-bold whitespace-nowrap %s\">%s</span>"
      pad_cls
      cls
      (escape_html label)
  in
  match q with
  | Verified ->
      mk "bg-sky-500/10 text-sky-600 dark:text-sky-400 border-sky-500/30" label_verified
  | Derived ->
      mk "bg-amber-500/10 text-amber-600 dark:text-amber-400 border-amber-500/30" label_derived
  | Mismatch ->
      mk "bg-rose-500/10 text-rose-600 dark:text-rose-400 border-rose-500/30" label_mismatch

let team_scope_to_string = function PerGame -> "per_game" | Totals -> "totals"

(** Link to the official WKBL game result page.

    The legacy `?game_id=046-01-2` format is rejected by WKBL ("허용되지 않는 입력값").
    The working format requires (season_gu, game_type, game_no, ym).
*)
let wkbl_official_game_result_url ~(game_id : string) ~(game_date : string) =
  let parse_game_id id =
    match String.split_on_char '-' (String.trim id) with
    | [season_gu; game_type; game_no_s] -> (
        match int_of_string_opt (String.trim game_no_s) with
        | Some game_no -> Some (String.trim season_gu, String.trim game_type, game_no)
        | None -> None)
    | _ -> None
  in
  let ym_of_date d =
    (* Expecting YYYY-MM-DD (from DB date::text). *)
    let d = String.trim d in
    if String.length d >= 7 && d.[4] = '-' then
      let yyyy = String.sub d 0 4 in
      let mm = String.sub d 5 2 in
      Some (yyyy ^ mm)
    else None
  in
  match (parse_game_id game_id, ym_of_date game_date) with
  | Some (season_gu, game_type, game_no), Some ym ->
      Some
        (Printf.sprintf
           "https://www.wkbl.or.kr/game/result.asp?season_gu=%s&game_type=%s&game_no=%d&ym=%s&viewType=1"
           (Uri.pct_encode season_gu) (Uri.pct_encode game_type) game_no (Uri.pct_encode ym))
  | _ -> None

(** Responsive table wrapper - enables horizontal scroll on mobile *)
let responsive_table_wrapper ?(class_extra="") content =
  Printf.sprintf {html|<div class="overflow-x-auto -mx-4 px-4 sm:mx-0 sm:px-0 %s">%s</div>|html} class_extra content
let extract_contract_years s = try Some (int_of_string (String.sub s 0 1)) with Failure _ | Invalid_argument _ -> None

let radar_chart ?(show_league_avg=false) ~labels ~values_a ~values_b ?(color_a="#f97316") ?(color_b="#0ea5e9") () =
  let _ = show_league_avg in
  let labels_json = "[" ^ (labels |> List.map (fun s -> "'" ^ s ^ "'") |> String.concat ",") ^ "]" in
  let data_a_json = "[" ^ (values_a |> List.map string_of_float |> String.concat ",") ^ "]" in
  let data_b_json = "[" ^ (values_b |> List.map string_of_float |> String.concat ",") ^ "]" in
  let chart_id = "radar-" ^ string_of_int (Random.int 10000) in
  
  Printf.sprintf {html|
    <div class="relative w-full max-w-md mx-auto aspect-square p-4">
      <canvas id="%s"></canvas>
    </div>
    <script>
      (function() {
        const ctx = document.getElementById('%s');
        new Chart(ctx, {
          type: 'radar',
          data: {
            labels: %s,
            datasets: [
              {
                label: 'A',
                data: %s,
                fill: true,
                backgroundColor: '%s33',
                borderColor: '%s',
                pointBackgroundColor: '%s',
                pointBorderColor: '#fff',
                pointHoverBackgroundColor: '#fff',
                pointHoverBorderColor: '%s'
              },
              {
                label: 'B',
                data: %s,
                fill: true,
                backgroundColor: '%s33',
                borderColor: '%s',
                pointBackgroundColor: '%s',
                pointBorderColor: '#fff',
                pointHoverBackgroundColor: '#fff',
                pointHoverBorderColor: '%s'
              }
            ]
          },
          options: {
            responsive: true,
            maintainAspectRatio: false,
            elements: {
              line: { borderWidth: 3 }
            },
            plugins: {
              legend: { display: false }
            },
            scales: {
              r: {
                angleLines: { color: 'rgba(148, 163, 184, 0.2)' },
                grid: { color: 'rgba(148, 163, 184, 0.2)' },
                pointLabels: {
                  color: 'rgba(148, 163, 184, 0.8)',
                  font: { size: 11, weight: 'bold' }
                },
                ticks: { display: false, backdropColor: 'transparent' },
                suggestedMin: 0,
                suggestedMax: 5
              }
            }
          }
        });
      })();
    </script>
  |html} chart_id chart_id labels_json data_a_json color_a color_a color_a color_a data_b_json color_b color_b color_b color_b

let normalize_stat_for_radar category current_val =
  let max_val = match category with
    | `Points -> 25.0
    | `Rebounds -> 15.0
    | `Assists -> 8.0
    | `Steals -> 3.0
    | `Blocks -> 2.0
    | `Efficiency -> 30.0
    | _ -> 10.0
  in
  if max_val <= 0.0 then 0.0 else (current_val /. max_val) *. 5.0

let player_summary_comparison (seasons: season_stats list) =
  match seasons with
  | [] -> ""
  | _ ->
    let seasons_desc = List.sort (fun a b -> compare b.ss_season_code a.ss_season_code) seasons in
    let current = List.hd seasons_desc in
    let n = List.length seasons_desc in
    let total_gp = List.fold_left (fun acc s -> acc + s.ss_games_played) 0 seasons_desc in
    let gp_f = float_of_int (max total_gp 1) in
    let wavg field = (List.fold_left (fun acc s -> acc +. field s *. float_of_int s.ss_games_played) 0.0 seasons_desc) /. gp_f in
    let total_min = List.fold_left (fun acc s -> acc +. s.ss_total_minutes) 0.0 seasons_desc in
    let career_mpg = total_min /. gp_f in
    let fmt v = Printf.sprintf "%.1f" v in
    let fmt_pct v = if v > 0.0 then Printf.sprintf "%.1f" (v *. 100.0) else "-" in
    let stat_cell cls v = Printf.sprintf {html|<td class="px-2 py-1.5 text-right font-mono tabular-nums %s">%s</td>|html} cls v in
    let hdr cls lbl title = Printf.sprintf
      {html|<th scope="col" class="px-2 py-1.5 text-right text-[10px] uppercase tracking-wider %s" title="%s">%s</th>|html} cls title lbl in
    (* Render a single season row *)
    let season_row ~label ~border (s: season_stats) =
      let mpg = if s.ss_games_played > 0
        then s.ss_total_minutes /. float_of_int s.ss_games_played else 0.0 in
      let border_cls = if border then " border-b border-slate-200 dark:border-slate-700" else "" in
      Printf.sprintf
        {html|<tr class="%s">
          <td class="px-2 py-1.5 text-left font-sans font-semibold text-slate-600 dark:text-slate-400 whitespace-nowrap">%s</td>
          %s %s %s %s %s %s %s %s %s %s
        </tr>|html}
        border_cls
        label
        (stat_cell "" (string_of_int s.ss_games_played))
        (stat_cell "" (fmt mpg))
        (stat_cell "text-orange-700 dark:text-orange-400 font-bold" (fmt s.ss_avg_points))
        (stat_cell "" (fmt s.ss_avg_rebounds))
        (stat_cell "" (fmt s.ss_avg_assists))
        (stat_cell "hidden sm:table-cell" (fmt s.ss_avg_steals))
        (stat_cell "hidden sm:table-cell" (fmt s.ss_avg_blocks))
        (stat_cell "hidden md:table-cell" (fmt_pct s.ss_ts_pct))
        (stat_cell "hidden md:table-cell" (fmt_pct s.ss_efg_pct))
        (stat_cell "" (fmt s.ss_efficiency))
    in
    let current_label = Printf.sprintf {|<a href="%s" class="hover:underline">%s</a>|} (season_href current.ss_season_code) (escape_html current.ss_season_name) in
    let current_row = season_row ~label:current_label ~border:true current in
    (* Peak season: highest PPG among seasons with 10+ GP, different from current *)
    let peak_row =
      let eligible = seasons_desc |> List.filter (fun s -> s.ss_games_played >= 10) in
      let peak_opt = eligible |> List.fold_left (fun best s ->
        match best with
        | None -> Some s
        | Some b -> if s.ss_avg_points > b.ss_avg_points then Some s else best
      ) None in
      match peak_opt with
      | Some peak when peak.ss_season_code <> current.ss_season_code ->
          let label = Printf.sprintf {|<span class="text-amber-600 dark:text-amber-400">★</span> <a href="%s" class="hover:underline">%s</a>|} (season_href peak.ss_season_code) (escape_html peak.ss_season_name) in
          season_row ~label ~border:true peak
      | _ -> ""
    in
    (* Career row *)
    let career_row =
      Printf.sprintf
        {html|<tr>
          <td class="px-2 py-1.5 text-left font-sans font-semibold text-slate-600 dark:text-slate-400">Career</td>
          %s %s %s %s %s %s %s %s %s %s
        </tr>|html}
        (stat_cell "" (Printf.sprintf "%d <span class=\"text-[10px] text-slate-500\">(%d시즌)</span>" total_gp n))
        (stat_cell "" (fmt career_mpg))
        (stat_cell "text-orange-700 dark:text-orange-400 font-bold" (fmt (wavg (fun s -> s.ss_avg_points))))
        (stat_cell "" (fmt (wavg (fun s -> s.ss_avg_rebounds))))
        (stat_cell "" (fmt (wavg (fun s -> s.ss_avg_assists))))
        (stat_cell "hidden sm:table-cell" (fmt (wavg (fun s -> s.ss_avg_steals))))
        (stat_cell "hidden sm:table-cell" (fmt (wavg (fun s -> s.ss_avg_blocks))))
        (stat_cell "hidden md:table-cell" (fmt_pct (wavg (fun s -> s.ss_ts_pct))))
        (stat_cell "hidden md:table-cell" (fmt_pct (wavg (fun s -> s.ss_efg_pct))))
        (stat_cell "" (fmt (wavg (fun s -> s.ss_efficiency))))
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
        <h2 class="font-bold text-slate-900 dark:text-slate-200 text-sm mb-3">시즌 요약</h2>
        <div class="overflow-x-auto">
          <table class="w-full text-xs tabular-nums">
            <thead class="text-slate-500 dark:text-slate-500">
              <tr>
                <th scope="col" class="px-2 py-1.5 text-left text-[10px] uppercase tracking-wider font-sans w-32"></th>
                %s %s %s %s %s %s %s %s %s %s
              </tr>
            </thead>
            <tbody class="text-slate-800 dark:text-slate-200">
              %s%s%s
            </tbody>
          </table>
        </div>
      </div>|html}
      (* headers *)
      (hdr "" "GP" "경기수")
      (hdr "" "MPG" "평균 출전시간")
      (hdr "text-orange-700 dark:text-orange-400" "PPG" "평균 득점")
      (hdr "" "RPG" "평균 리바운드")
      (hdr "" "APG" "평균 어시스트")
      (hdr "hidden sm:table-cell" "SPG" "평균 스틸")
      (hdr "hidden sm:table-cell" "BPG" "평균 블록")
      (hdr "hidden md:table-cell" "TS%" "True Shooting %")
      (hdr "hidden md:table-cell" "eFG%" "Effective FG %")
      (hdr "" "EFF" "효율")
      (* rows *)
      current_row
      peak_row
      career_row

let player_season_stats_component ~(player_id: string) ~scope (seasons: season_stats list) =
  match seasons with
  | [] -> ""
  | _ ->
      let scopes = [("per_game", "Per Game"); ("totals", "Totals"); ("per_36", "Per 36"); ("advanced", "Advanced")] in
      let tab_html =
        scopes |> List.map (fun (s, label) ->
          let active = (s = scope) in
          let cls = if active
            then "px-3 py-1.5 text-sm font-semibold text-orange-700 dark:text-orange-400 border-b-2 border-orange-500"
            else "px-3 py-1.5 text-sm text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-slate-200 border-b-2 border-transparent cursor-pointer"
          in
          Printf.sprintf
            {html|<button class="%s" hx-get="/player/%s/season-stats?scope=%s" hx-target="#season-stats-panel" hx-swap="outerHTML">%s</button>|html}
            cls (Uri.pct_encode player_id) s label
        ) |> String.concat "\n"
      in
      let scope_label = match scope with "totals" -> "시즌별 합계" | "per_36" -> "36분당 환산" | "advanced" -> "고급 지표" | _ -> "시즌별 요약" in
      let is_advanced = (scope = "advanced") in
      let season_code_int s = try int_of_string s with Failure _ -> 0 in
      let seasons_desc =
        seasons
        |> List.sort (fun a b -> compare (season_code_int b.ss_season_code) (season_code_int a.ss_season_code))
      in
      let row (s: season_stats) =
        let season_href =
          Printf.sprintf "/standings?season=%s"
            (Uri.pct_encode s.ss_season_code)
        in
        let team_href =
          Printf.sprintf "/team/%s?season=%s"
            (Uri.pct_encode s.ss_team_name)
            (Uri.pct_encode s.ss_season_code)
        in
        let gamelog_href =
          Printf.sprintf "/player/%s/games?season=%s"
            (Uri.pct_encode player_id)
            (Uri.pct_encode s.ss_season_code)
        in
        let mpg =
          if s.ss_games_played <= 0 then 0.0 else s.ss_total_minutes /. float_of_int s.ss_games_played
        in
        let fmt_pct v = if v > 0.0 then Printf.sprintf "%.1f" (v *. 100.0) else "-" in
        let margin_cls =
          if s.ss_margin > 0.0 then "text-sky-600 dark:text-sky-400"
          else if s.ss_margin < 0.0 then "text-rose-600 dark:text-rose-400"
          else "text-slate-600 dark:text-slate-400"
        in
        let tr_cls = "border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/40 transition-colors" in
        let season_td = Printf.sprintf
          {html|<td class="px-3 py-2 text-left font-sans whitespace-nowrap">
            <a class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors" href="%s" title="시즌 순위표">%s</a>
            <a class="ml-1 text-[10px] text-slate-400 hover:text-orange-500 transition-colors" href="%s" title="경기 기록">GL</a>
          </td>|html}
          (escape_html season_href) (escape_html s.ss_season_name) (escape_html gamelog_href)
        in
        let team_td = Printf.sprintf
          {html|<td class="px-3 py-2 text-left font-sans whitespace-nowrap hidden sm:table-cell">
            <a class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors" href="%s">%s</a>
          </td>|html}
          (escape_html team_href) (escape_html s.ss_team_name)
        in
        if is_advanced then
          let ast_tov =
            if s.ss_avg_turnovers > 0.0 then Printf.sprintf "%.2f" (s.ss_avg_assists /. s.ss_avg_turnovers)
            else "-"
          in
          Printf.sprintf
            {html|<tr class="%s">%s%s
              <td class="px-3 py-2 text-right font-mono tabular-nums">%d</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums text-emerald-600 dark:text-emerald-400 font-semibold">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums text-emerald-600 dark:text-emerald-400 font-semibold">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden md:table-cell">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums %s font-semibold">%+.1f</td>
            </tr>|html}
            tr_cls season_td team_td
            s.ss_games_played mpg
            s.ss_avg_points s.ss_avg_turnovers ast_tov
            (fmt_pct s.ss_ts_pct) (fmt_pct s.ss_efg_pct)
            s.ss_efficiency margin_cls s.ss_margin
        else
          Printf.sprintf
            {html|<tr class="%s">%s%s
              <td class="px-3 py-2 text-right font-mono tabular-nums">%d</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums text-orange-700 dark:text-orange-400 font-semibold">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden md:table-cell">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden md:table-cell">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden lg:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden lg:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden lg:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden xl:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden xl:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums %s">%.1f</td>
            </tr>|html}
            tr_cls season_td team_td
            s.ss_games_played mpg
            s.ss_avg_points s.ss_avg_rebounds s.ss_avg_assists
            s.ss_avg_steals s.ss_avg_blocks
            (fmt_pct s.ss_fg_pct) (fmt_pct s.ss_fg3_pct) (fmt_pct s.ss_ft_pct)
            (fmt_pct s.ss_ts_pct) (fmt_pct s.ss_efg_pct)
            s.ss_efficiency margin_cls s.ss_margin
      in
      let rows = seasons_desc |> List.map row |> String.concat "\n" in
      (* Career totals/averages footer *)
      let n = List.length seasons_desc in
      let total_gp = List.fold_left (fun acc s -> acc + s.ss_games_played) 0 seasons_desc in
      let total_min = List.fold_left (fun acc s -> acc +. s.ss_total_minutes) 0.0 seasons_desc in
      let gp_f = float_of_int (max total_gp 1) in
      let wavg field = (List.fold_left (fun acc s -> acc +. field s *. float_of_int s.ss_games_played) 0.0 seasons_desc) /. gp_f in
      let career_mpg = total_min /. gp_f in
      let career_ppg = wavg (fun s -> s.ss_avg_points) in
      let career_rpg = wavg (fun s -> s.ss_avg_rebounds) in
      let career_apg = wavg (fun s -> s.ss_avg_assists) in
      let career_spg = wavg (fun s -> s.ss_avg_steals) in
      let career_bpg = wavg (fun s -> s.ss_avg_blocks) in
      let career_tov = wavg (fun s -> s.ss_avg_turnovers) in
      let career_eff = wavg (fun s -> s.ss_efficiency) in
      let career_margin = wavg (fun s -> s.ss_margin) in
      let fmt_pct_f v = if v > 0.0 then Printf.sprintf "%.1f" (v *. 100.0) else "-" in
      let career_fg = wavg (fun s -> s.ss_fg_pct) in
      let career_fg3 = wavg (fun s -> s.ss_fg3_pct) in
      let career_ft = wavg (fun s -> s.ss_ft_pct) in
      let career_ts = wavg (fun s -> s.ss_ts_pct) in
      let career_efg = wavg (fun s -> s.ss_efg_pct) in
      let tfoot_cls = "bg-slate-100 dark:bg-slate-800/70 font-semibold text-slate-900 dark:text-slate-200 border-t-2 border-slate-300 dark:border-slate-600" in
      let career_margin_cls =
        if career_margin > 0.0 then "text-sky-600 dark:text-sky-400"
        else if career_margin < 0.0 then "text-rose-600 dark:text-rose-400"
        else "text-slate-600 dark:text-slate-400"
      in
      let tfoot_html =
        if is_advanced then
          let career_ast_tov =
            if career_tov > 0.0 then Printf.sprintf "%.2f" (career_apg /. career_tov) else "-"
          in
          Printf.sprintf
            {html|<tfoot class="%s"><tr>
              <td class="px-3 py-2 text-left font-sans">Career (%d시즌)</td>
              <td class="px-3 py-2 hidden sm:table-cell"></td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%d</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums text-emerald-600 dark:text-emerald-400">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums text-emerald-600 dark:text-emerald-400">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden md:table-cell">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums %s">%+.1f</td>
            </tr></tfoot>|html}
            tfoot_cls n total_gp career_mpg career_ppg career_tov career_ast_tov
            (fmt_pct_f career_ts) (fmt_pct_f career_efg)
            career_eff career_margin_cls career_margin
        else
          Printf.sprintf
            {html|<tfoot class="%s"><tr>
              <td class="px-3 py-2 text-left font-sans">Career (%d시즌)</td>
              <td class="px-3 py-2 hidden sm:table-cell"></td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%d</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums text-orange-700 dark:text-orange-400">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden md:table-cell">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden md:table-cell">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden lg:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden lg:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden lg:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden xl:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums hidden xl:table-cell">%s</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums">%.1f</td>
              <td class="px-3 py-2 text-right font-mono tabular-nums %s">%.1f</td>
            </tr></tfoot>|html}
            tfoot_cls n total_gp career_mpg career_ppg career_rpg career_apg
            career_spg career_bpg
            (fmt_pct_f career_fg) (fmt_pct_f career_fg3) (fmt_pct_f career_ft)
            (fmt_pct_f career_ts) (fmt_pct_f career_efg)
            career_eff career_margin_cls career_margin
      in
      let colgroup_html =
        if is_advanced then
          {html|<colgroup>
            <col style="width: 140px;">
            <col class="hidden sm:table-column" style="width: 100px;">
            <col style="width: 50px;">
            <col style="width: 55px;">
            <col style="width: 55px;">
            <col style="width: 55px;">
            <col style="width: 65px;">
            <col style="width: 65px;">
            <col style="width: 65px;">
            <col class="hidden md:table-column" style="width: 60px;">
            <col style="width: 60px;">
          </colgroup>|html}
        else
          {html|<colgroup>
            <col style="width: 140px;">
            <col class="hidden sm:table-column" style="width: 100px;">
            <col style="width: 55px;">
            <col style="width: 60px;">
            <col style="width: 60px;">
            <col style="width: 55px;">
            <col style="width: 55px;">
            <col class="hidden md:table-column" style="width: 55px;">
            <col class="hidden md:table-column" style="width: 55px;">
            <col class="hidden lg:table-column" style="width: 55px;">
            <col class="hidden lg:table-column" style="width: 55px;">
            <col class="hidden lg:table-column" style="width: 55px;">
            <col class="hidden xl:table-column" style="width: 55px;">
            <col class="hidden xl:table-column" style="width: 55px;">
            <col style="width: 60px;">
            <col style="width: 60px;">
          </colgroup>|html}
      in
      let thead_html =
        if is_advanced then
          {html|<thead class="bg-slate-100 dark:bg-slate-800/70 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider"><tr>
            <th scope="col" class="px-3 py-2 text-left font-sans">시즌</th>
            <th scope="col" class="px-3 py-2 text-left font-sans hidden sm:table-cell">팀</th>
            <th scope="col" class="px-3 py-2 text-right">GP</th>
            <th scope="col" class="px-3 py-2 text-right" title="출전시간">MIN</th>
            <th scope="col" class="px-3 py-2 text-right" title="득점">PTS</th>
            <th scope="col" class="px-3 py-2 text-right" title="턴오버">TOV</th>
            <th scope="col" class="px-3 py-2 text-right" title="어시스트/턴오버 비율">AST/TOV</th>
            <th scope="col" class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400" title="True Shooting %">TS%</th>
            <th scope="col" class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400" title="Effective FG%">eFG%</th>
            <th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="효율">EFF</th>
            <th scope="col" class="px-3 py-2 text-right" title="팀 득실마진(출전시간 가중)">+/-</th>
          </tr></thead>|html}
        else
          Printf.sprintf
            {html|<thead class="bg-slate-100 dark:bg-slate-800/70 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider"><tr>
              <th scope="col" class="px-3 py-2 text-left font-sans">시즌</th>
              <th scope="col" class="px-3 py-2 text-left font-sans hidden sm:table-cell">팀</th>
              <th scope="col" class="px-3 py-2 text-right">GP</th>
              <th scope="col" class="px-3 py-2 text-right" title="출전시간">%s</th>
              <th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" title="득점">%s</th>
              <th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th>
              <th scope="col" class="px-3 py-2 text-right" title="어시스트">AST</th>
              <th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="스틸">STL</th>
              <th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="블록">BLK</th>
              <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell" title="야투 성공률">FG%%</th>
              <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell" title="3점슛 성공률">3P%%</th>
              <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell" title="자유투 성공률">FT%%</th>
              <th scope="col" class="px-3 py-2 text-right hidden xl:table-cell" title="True Shooting %%">TS%%</th>
              <th scope="col" class="px-3 py-2 text-right hidden xl:table-cell" title="Effective FG%%">eFG%%</th>
              <th scope="col" class="px-3 py-2 text-right" title="효율">EFF</th>
              <th scope="col" class="px-3 py-2 text-right" title="팀 득실마진(출전시간 가중)">+/-</th>
            </tr></thead>|html}
            "MIN" "PTS"
      in
      let min_w = if is_advanced then "min-w-[700px]" else "min-w-[800px]" in
      Printf.sprintf
        {html|<div id="season-stats-panel" class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
          <div class="flex items-center justify-between mb-4">
            <h3 class="font-bold text-slate-900 dark:text-slate-200">%s</h3>
          </div>
          <div class="flex gap-1 border-b border-slate-200 dark:border-slate-700 mb-4">
            %s
          </div>
          <div class="text-[11px] text-slate-500 dark:text-slate-500 leading-relaxed mb-3">시즌명 → 순위표 | GL → 경기 기록 | 팀명 → 팀 페이지</div>
          <div class="overflow-x-auto">
            <table class="%s w-full text-sm table-fixed tabular-nums" aria-label="%s">
              %s
              %s
              <tbody>%s</tbody>
              %s
            </table>
          </div>
        </div>|html}
        scope_label tab_html min_w scope_label colgroup_html thead_html rows tfoot_html

let career_trajectory_chart (seasons: season_stats list) =
  let season_code_int s = try int_of_string s with Failure _ -> 0 in
  let seasons_asc =
    seasons
    |> List.sort (fun a b -> compare (season_code_int a.ss_season_code) (season_code_int b.ss_season_code))
  in
  match seasons_asc with
  | [] -> ""
  | [_] -> ""
  | _ ->
      let n = List.length seasons_asc in
      let width = 420 in
      let height = 160 in
      let padding = 22 in
      let inner_w = width - (2 * padding) in
      let inner_h = height - (2 * padding) in
      (* Find global max across all 3 metrics for a shared y-axis *)
      let global_max =
        seasons_asc
        |> List.fold_left (fun acc s ->
            max acc (max s.ss_avg_points (max s.ss_avg_rebounds s.ss_avg_assists))) 0.0
        |> fun m -> if m <= 0.0 then 1.0 else m
      in
      let x_of_i i =
        if n <= 1 then padding
        else padding + (i * inner_w) / (n - 1)
      in
      let y_of v =
        let ratio = max 0.0 (min 1.0 (v /. global_max)) in
        padding + int_of_float (float_of_int inner_h *. (1.0 -. ratio))
      in
      (* Generate path + dots for a metric *)
      let make_line ~extract ~color ~fill ~label =
        let xy =
          seasons_asc
          |> List.mapi (fun i s -> (x_of_i i, y_of (extract s), s))
        in
        let path_d =
          match xy with
          | [] -> ""
          | (x0, y0, _) :: tl ->
              let rest = tl |> List.map (fun (x, y, _) -> Printf.sprintf "L %d %d" x y) |> String.concat " " in
              Printf.sprintf "M %d %d %s" x0 y0 rest
        in
        let dots =
          xy
          |> List.map (fun (x, y, s) ->
              let tip = Printf.sprintf "%s: %s %.1f" s.ss_season_name label (extract s) in
              Printf.sprintf {svg|<g><title>%s</title><circle cx="%d" cy="%d" r="3" class="%s"/></g>|svg}
                (escape_html tip) x y fill)
          |> String.concat "\n"
        in
        Printf.sprintf {svg|<path d="%s" fill="none" stroke="currentColor" class="%s" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" opacity="0.85"/>%s|svg}
          (escape_html path_d) color dots
      in
      let pts_line = make_line ~extract:(fun s -> s.ss_avg_points) ~color:"text-orange-500" ~fill:"fill-orange-500" ~label:"PPG" in
      let reb_line = make_line ~extract:(fun s -> s.ss_avg_rebounds) ~color:"text-sky-500" ~fill:"fill-sky-500" ~label:"RPG" in
      let ast_line = make_line ~extract:(fun s -> s.ss_avg_assists) ~color:"text-emerald-500" ~fill:"fill-emerald-500" ~label:"APG" in
      let first_season = (List.hd seasons_asc).ss_season_name in
      let last_season = (List.hd (List.rev seasons_asc)).ss_season_name in
      Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
          <div class="flex items-end justify-between gap-3">
            <div>
              <h3 class="text-slate-900 dark:text-slate-200 font-bold">커리어 스탯 추이</h3>
              <div class="text-[11px] text-slate-500 dark:text-slate-500">%s → %s</div>
            </div>
            <div class="flex items-center gap-3 text-[11px]">
              <span class="flex items-center gap-1"><span class="w-3 h-0.5 bg-orange-500 rounded"></span><span class="text-slate-500 dark:text-slate-400">PPG</span></span>
              <span class="flex items-center gap-1"><span class="w-3 h-0.5 bg-sky-500 rounded"></span><span class="text-slate-500 dark:text-slate-400">RPG</span></span>
              <span class="flex items-center gap-1"><span class="w-3 h-0.5 bg-emerald-500 rounded"></span><span class="text-slate-500 dark:text-slate-400">APG</span></span>
            </div>
          </div>
          <div class="mt-4">
            <svg viewBox="0 0 %d %d" class="w-full max-w-xl">
              %s
              %s
              %s
            </svg>
          </div>
        </div>|html}
        (escape_html first_season)
        (escape_html last_season)
        width height
        pts_line reb_line ast_line
let player_row ?(show_player_id=false) ?(team_cell_class="px-3 py-2") ?(include_team=true) ?(player_info=None) (rank: int) (p: player_aggregate) =
  let id_badge = if show_player_id then player_id_badge p.player_id else "" in
  let display_name = normalize_name p.name in
  let disambiguation =
    if show_player_id then player_disambiguation_line ~team_name:p.team_name ~player_id:p.player_id player_info
    else ""
  in
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
          <div class="flex flex-col min-w-0">
            <div class="flex items-center gap-2 min-w-0">
              <a href="%s" class="player-name hover:text-orange-700 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
              <span class="%s">%s</span>
            </div>
            %s
          </div>
        </div>
      </td>
      %s
      <td class="px-3 py-2 text-right whitespace-nowrap hidden sm:table-cell text-slate-500 dark:text-slate-400 font-mono" style="width: 60px; min-width: 60px;">%d</td>
      %s%s%s%s%s%s%s%s%s
    </tr>|html}
    rank
    (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
    (player_href p.player_id)
    (escape_html display_name)
    (if show_player_id then "inline-flex" else "hidden")
    id_badge
    disambiguation
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
	let find_substring_from ~(sub: string) (s: string) ~(from: int) =
	  let len_s = String.length s in
	  let len_sub = String.length sub in
	  if len_sub = 0 then Some (max 0 from)
	  else if from < 0 || from > len_s - len_sub then None
	  else
	    let rec loop i =
	      if i > len_s - len_sub then None
	      else if String.sub s i len_sub = sub then Some i
	      else loop (i + 1)
	    in
	    loop from

	let career_highs_card (items_opt: career_high_item list option) =
	  match items_opt with
	  | None | Some [] -> ""
	  | Some items ->
	      let item_html (it: career_high_item) =
	        let venue = if it.chi_is_home then "홈" else "원정" in
	        let href = Printf.sprintf "/boxscore/%s" (Uri.pct_encode it.chi_game_id) in
	        let opp_code = team_code_of_string it.chi_opponent |> Option.value ~default:"" in
	        let opp_logo = team_logo_tag ~class_name:"w-4 h-4 inline-block" opp_code in
	        Printf.sprintf
	          {html|<a href="%s" class="block bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-4 hover:border-orange-400 dark:hover:border-orange-600 transition-colors">
	            <div class="flex items-start justify-between gap-3">
	              <div class="min-w-0">
	                <div class="text-[11px] text-slate-500 dark:text-slate-500 font-mono uppercase tracking-wider">%s</div>
	                <div class="mt-1 flex items-center gap-1.5 text-sm text-slate-700 dark:text-slate-300">
	                  <span class="font-mono text-[11px] text-slate-500 dark:text-slate-500">%s</span>
	                  <span class="text-slate-400 dark:text-slate-600">·</span>
	                  <span class="text-[11px] text-slate-500 dark:text-slate-500">%s</span>
	                  <span class="text-slate-400 dark:text-slate-600">·</span>
	                  %s
	                  <span class="truncate">%s</span>
	                </div>
	              </div>
	              <div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%d</div>
	            </div>
	          </a>|html}
	          (escape_html href)
	          (escape_html it.chi_label)
	          (escape_html it.chi_game_date)
	          (escape_html venue)
	          opp_logo
	          (escape_html it.chi_opponent)
	          it.chi_value
	      in
	      let items_html = items |> List.map item_html |> String.concat "\n" in
	      Printf.sprintf
	        {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
	          <div class="flex items-center justify-between gap-3">
	            <h3 class="text-slate-900 dark:text-slate-200 font-bold">커리어 하이</h3>
	            <div class="text-[11px] text-slate-500 dark:text-slate-500 font-mono">%d개</div>
	          </div>
	          <div class="mt-4 grid grid-cols-1 sm:grid-cols-2 gap-3">%s</div>
	        </div>|html}
	        (List.length items)
	        items_html

 (* Helper: Recent Games List *)
 let render_recent_games_for_predict team_name (games : game_summary list) limit =
   let target = normalize_name team_name in
   let relevant =
     games
     |> List.filter (fun g ->
         let (_, _, home_team, away_team, home_score, away_score) = Db.extract_game_info g in
         match home_score, away_score with
         | Some _, Some _ ->
             normalize_name home_team = target || normalize_name away_team = target
         | _ -> false)
     |> List.sort (fun a b ->
         let (_, date_a, _, _, _, _) = Db.extract_game_info a in
         let (_, date_b, _, _, _, _) = Db.extract_game_info b in
         String.compare date_b date_a) (* Descending date *)
     |> (fun l -> try List.filteri (fun i _ -> i < limit) l with Invalid_argument _ -> l)
   in
	   if relevant = [] then
	     {html|<div class="text-slate-400 text-xs italic">최근 경기 없음</div>|html}
	   else
     let rows =
       relevant
       |> List.map (fun g ->
           let (_, game_date, home_team, away_team, home_score, away_score) = Db.extract_game_info g in
           let is_home = normalize_name home_team = target in
           let opponent = if is_home then away_team else home_team in
           let my_score = if is_home then Option.get home_score else Option.get away_score in
           let opp_score = if is_home then Option.get away_score else Option.get home_score in
           let is_win = my_score > opp_score in
           let result_badge =
             if is_win then {html|<span class="inline-flex items-center justify-center w-5 h-5 rounded-full bg-emerald-100 text-emerald-700 text-[10px] font-bold">W</span>|html}
             else {html|<span class="inline-flex items-center justify-center w-5 h-5 rounded-full bg-rose-100 text-rose-700 text-[10px] font-bold">L</span>|html}
           in
           let date_short = String.sub game_date 5 5 in (* MM-DD *)
           Printf.sprintf
             {html|
               <div class="flex items-center justify-between py-1.5 border-b border-slate-100 dark:border-slate-800 last:border-0 text-xs">
                 <div class="flex items-center gap-2 min-w-0 flex-1">
                   <span class="text-slate-400 font-mono shrink-0">%s</span>
                   %s
                   %s
                 </div>
                 <div class="font-mono font-bold shrink-0 ml-2 %s">
                   %d-%d
                 </div>
               </div>
             |html}
             date_short
             result_badge
             (team_badge ~max_width:"max-w-[100px]" opponent)
             (if is_win then "text-slate-900 dark:text-white" else "text-slate-500")
             my_score opp_score
         )
       |> String.concat "\n"
     in
     Printf.sprintf {html|<div class="space-y-1">%s</div>|html} rows

 (* Helper: Head-to-Head Summary *)
 let render_h2h_summary home away (games : game_summary list) season =
   let h = normalize_name home in
   let a = normalize_name away in
   let relevant =
     games
     |> List.filter (fun g ->
         let (_, _, home_team, away_team, home_score, away_score) = Db.extract_game_info g in
         match home_score, away_score with
         | Some _, Some _ ->
             let gh = normalize_name home_team in
             let ga = normalize_name away_team in
             (gh = h && ga = a) || (gh = a && ga = h)
         | _ -> false)
     |> List.sort (fun a b ->
         let (_, date_a, _, _, _, _) = Db.extract_game_info a in
         let (_, date_b, _, _, _, _) = Db.extract_game_info b in
         String.compare date_b date_a)
   in
   if relevant = [] then
     ""
   else
     let total = List.length relevant in
     let home_wins =
       relevant
       |> List.filter (fun g ->
           let (_, _, home_team, _, home_score, away_score) = Db.extract_game_info g in
           let gh = normalize_name home_team in
           let hs = Option.get home_score in
           let as_ = Option.get away_score in
           if gh = h then hs > as_ else as_ > hs) (* if home is away team, check away score *)
       |> List.length
     in
     let away_wins = total - home_wins in
     let recent_games_html = render_recent_games_for_predict h relevant 5 in
     Printf.sprintf
       {html|
         <div class="mt-6 bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
	           <h3 class="text-sm font-bold text-slate-600 dark:text-slate-400 uppercase tracking-wider mb-3">상대 전적 (시즌 %s)</h3>
	           <div class="flex items-center justify-between mb-4 px-4">
	             <div class="text-center">
	               <div class="text-2xl font-bold text-slate-900 dark:text-white">%d</div>
	               <div class="text-xs text-slate-500">%s 승</div>
	             </div>
	             <div class="text-xs font-mono text-slate-400">총 %d경기</div>
	             <div class="text-center">
	               <div class="text-2xl font-bold text-slate-900 dark:text-white">%d</div>
	               <div class="text-xs text-slate-500">%s 승</div>
	             </div>
	           </div>
           %s
         </div>
       |html}
       season
       home_wins home total away_wins away
       recent_games_html
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
