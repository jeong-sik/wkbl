(** UI Components Library - Declarative HTML generation *)

(** HTML rendering helpers *)
module Html = struct
  let class_ attr = if attr = "" then "" else Printf.sprintf " class=\"%s\"" attr
  let style_ attr = if attr = "" then "" else Printf.sprintf " style=\"%s\"" attr
  let title_ attr = if attr = "" then "" else Printf.sprintf " title=\"%s\"" attr
  let escape = Views_common.escape_html
end

(** Declarative Table Component *)
module Table = struct
  type align = [ `Left | `Center | `Right ]
  type responsive = [ `Always | `Hidden_sm | `Hidden_md | `Hidden_lg ]

  type 'a column = {
    header : string;
    width : int option; (* pixel width, None for auto *)
    render : 'a -> string; (* cell content renderer *)
    align : align;
    responsive : responsive;
    sort_key : string option;
    title : string option; (* tooltip *)
    highlight : bool; (* orange text *)
  }

  (** Column builder helper *)
  let col ?(w=None) ?(align=`Left) ?(resp=`Always) ?sort ?title ?(highlight=false) header render =
    { header; width = w; render; align; responsive = resp; sort_key = sort; title; highlight }

  (** Helper to create int width option *)
  let px w = Some w

  (** Get Tailwind classes for responsiveness *)
  let resp_class = function
    | `Always -> ""
    | `Hidden_sm -> "hidden sm:table-cell"
    | `Hidden_md -> "hidden md:table-cell"
    | `Hidden_lg -> "hidden lg:table-cell"

  (** Get Tailwind classes for colgroup (display: table-column) *)
  let resp_col_class = function
    | `Always -> ""
    | `Hidden_sm -> "hidden sm:table-column"
    | `Hidden_md -> "hidden md:table-column"
    | `Hidden_lg -> "hidden lg:table-column"

  (** Get Tailwind classes for alignment *)
  let align_class = function
    | `Left -> "text-left"
    | `Center -> "text-center"
    | `Right -> "text-right"

  (** Render <colgroup> *)
  let render_colgroup columns =
    let cols =
      columns
      |> List.map (fun c ->
          let width_style =
            match c.width with
            | Some w -> Printf.sprintf "width: %dpx;" w
            | None -> "width: auto;"
          in
          let cls = resp_col_class c.responsive in
          Printf.sprintf {html|<col%s%s>|html} (Html.class_ cls) (Html.style_ width_style))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<colgroup>%s</colgroup>|html} cols

  (** Render <thead> *)
  let render_thead columns =
    let ths =
      columns
      |> List.map (fun c ->
          let base_cls = "px-3 py-2 font-sans whitespace-nowrap" in
          let align_cls = align_class c.align in
          let resp_cls = resp_class c.responsive in
          let highlight_cls = if c.highlight then "text-orange-600 dark:text-orange-400" else "" in
          let sort_attr =
            match c.sort_key with
            | Some k -> Printf.sprintf " data-sortable data-sort-key=\"%s\"" k
            | None -> ""
          in
          Printf.sprintf {html|<th%s%s%s>%s</th>|html}
            (Html.class_ (String.concat " " [base_cls; align_cls; resp_cls; highlight_cls]))
            sort_attr
            (Html.title_ (Option.value ~default:"" c.title))
            (Html.escape c.header))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr>%s</tr></thead>|html}
      ths

  (** Render <tbody> *)
  let render_tbody columns data =
    let rows =
      data
      |> List.map (fun item ->
          let tds =
            columns
            |> List.map (fun c ->
                let base_cls = "px-3 py-2" in
                let align_cls = align_class c.align in
                let resp_cls = resp_class c.responsive in
                let highlight_cls =
                  if c.highlight then "text-orange-600 dark:text-orange-400 font-bold"
                  else if c.align = `Left then "font-medium text-slate-900 dark:text-slate-200"
                  else "text-slate-700 dark:text-slate-300"
                in
                Printf.sprintf {html|<td%s>%s</td>|html}
                  (Html.class_ (String.concat " " [base_cls; align_cls; resp_cls; highlight_cls]))
                  (c.render item))
            |> String.concat ""
          in
          Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors group">%s</tr>|html} tds)
      |> String.concat "\n"
    in
    Printf.sprintf {html|<tbody id="table-body">%s</tbody>|html} rows

  (** Main render function *)
  let render ?(id="table") ?(min_width="min-w-[720px]") columns data =
    let colgroup = render_colgroup columns in
    let thead = render_thead columns in
    let tbody = render_tbody columns data in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl">
  <table id="%s" class="w-full %s text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="Data Table">
    %s
    %s
    %s
  </table>
</div>|html}
      id min_width colgroup thead tbody
end
