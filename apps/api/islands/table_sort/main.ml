(** Table-sort Wasm island.
    Replaces static/js/table-sort.js with wasm_of_ocaml for
    type-safe sorting shared with the server via island_core.

    Behaviour:
    - Discovers table[data-sortable] inside [data-island="table_sort"]
    - Attaches click handlers to th[data-sortable] headers
    - Sorts rows using Wkbl_island_core.Table_sort_logic
    - Updates sort indicators (SVG arrows)
    - Syncs sort state to URL query params (?sort=&order=)
    - Re-stripes rows after sorting
    - Marks container data-island-hydrated="true" *)

open Js_of_ocaml

(* ── SVG icons ─────────────────────────────── *)

let svg_neutral =
  {|<svg class="w-3 h-3 opacity-30" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4"></path></svg>|}

let svg_asc =
  {|<svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7"></path></svg>|}

let svg_desc =
  {|<svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>|}

(* ── JS interop helpers ────────────────────── *)

(** querySelectorAll returning OCaml list. *)
let query_all (root : #Dom.node Js.t) (sel : string) : Dom.node Js.t list =
  let nl = (Js.Unsafe.coerce root)##querySelectorAll (Js.string sel) in
  let n : int = nl##.length in
  List.init n (fun i -> Js.Opt.get (nl##item i) (fun () -> assert false))

(** Coerce Dom.node to Dom_html.element. *)
let to_element (n : Dom.node Js.t) : Dom_html.element Js.t =
  Js.Opt.get (Dom_html.CoerceTo.element n) (fun () -> assert false)

(** Read textContent of an element, empty string if null. *)
let text_content (el : Dom_html.element Js.t) : string =
  Js.Opt.case el##.textContent (fun () -> "") Js.to_string

(** Get child td/th cells of a tr as an array. *)
let row_cells (tr : Dom_html.element Js.t) : Dom_html.element Js.t array =
  let nodes = query_all (tr :> Dom.node Js.t) "td, th" in
  Array.of_list (List.map to_element nodes)

(** Get attribute value, None if missing. *)
let get_attr (el : Dom_html.element Js.t) (name : string) : string option =
  let v = el##getAttribute (Js.string name) in
  Js.Opt.to_option (Js.Opt.map v Js.to_string)

(** Set innerHTML. *)
let set_inner_html (el : Dom_html.element Js.t) (html : string) : unit =
  el##.innerHTML := Js.string html

(* ── Per-table sort state ──────────────────── *)

(* Simple mutable map from table id to (col_index, direction) *)
let sort_states : (string, int * string) Hashtbl.t = Hashtbl.create 8

(* ── Core sort logic ───────────────────────── *)

let sort_table (table : Dom_html.element Js.t) (col_index : int) (dir : string) =
  let tbody_nodes = query_all (table :> Dom.node Js.t) "tbody" in
  match tbody_nodes with
  | [] -> ()
  | tbody_node :: _ ->
    let tbody = to_element tbody_node in
    let rows = query_all (tbody :> Dom.node Js.t) "tr" in
    let row_els = Array.of_list (List.map to_element rows) in
    let n = Array.length row_els in
    (* Extract sort values *)
    let values = Array.init n (fun i ->
      let cells = row_cells row_els.(i) in
      if col_index < Array.length cells then
        Wkbl_island_core.Table_sort_logic.parse_sort_value
          (text_content cells.(col_index))
      else Wkbl_island_core.Table_sort_logic.Empty
    ) in
    (* Get sorted indices *)
    let indices = Wkbl_island_core.Table_sort_logic.sort_indices values dir in
    (* Re-append rows in sorted order, update stripe *)
    Array.iteri (fun visual_i orig_i ->
      let row = row_els.(orig_i) in
      Dom.appendChild tbody row;
      (* Stripe: odd visual rows get subtle background *)
      ignore (row##.classList##remove (Js.string "bg-slate-50/50"));
      ignore (row##.classList##remove (Js.string "dark:bg-slate-800/20"));
      if visual_i mod 2 = 1 then begin
        ignore (row##.classList##add (Js.string "bg-slate-50/50"));
        ignore (row##.classList##add (Js.string "dark:bg-slate-800/20"))
      end
    ) indices

(* ── Sort indicators ───────────────────────── *)

let update_indicators (table : Dom_html.element Js.t) (active_col : int) (dir : string) =
  let headers = query_all (table :> Dom.node Js.t) "thead th" in
  List.iteri (fun i th_node ->
    let th = to_element th_node in
    let indicators = query_all (th :> Dom.node Js.t) ".sort-indicator" in
    match indicators with
    | [] -> ()
    | ind_node :: _ ->
      let ind = to_element ind_node in
      if i = active_col then begin
        ind##setAttribute (Js.string "data-sort") (Js.string dir);
        set_inner_html ind (if dir = "asc" then svg_asc else svg_desc)
      end else begin
        ind##removeAttribute (Js.string "data-sort");
        set_inner_html ind svg_neutral
      end
  ) headers

(* ── URL state sync ────────────────────────── *)

let update_url (col_key : string) (dir : string) =
  let url = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "URL")
    [| Js.Unsafe.inject Dom_html.window##.location##.href |] in
  let sp = Js.Unsafe.get url "searchParams" in
  ignore (Js.Unsafe.meth_call sp "set"
    [| Js.Unsafe.inject (Js.string "sort");
       Js.Unsafe.inject (Js.string col_key) |]);
  ignore (Js.Unsafe.meth_call sp "set"
    [| Js.Unsafe.inject (Js.string "order");
       Js.Unsafe.inject (Js.string dir) |]);
  let new_url = Js.Unsafe.meth_call url "toString" [||] in
  Dom_html.window##.history##replaceState
    Js.null (Js.string "") (Js.some new_url)

(* ── Initialize a single sortable table ────── *)

let init_sortable_table (table : Dom_html.element Js.t) =
  let headers = query_all (table :> Dom.node Js.t) "thead th[data-sortable]" in
  if headers = [] then ()
  else begin
    List.iteri (fun col_index th_node ->
      let th = to_element th_node in
      (* Add sort indicator if not present *)
      let existing = query_all (th :> Dom.node Js.t) ".sort-indicator" in
      if existing = [] then begin
        let indicator = Dom_html.document##createElement (Js.string "span") in
        ignore (indicator##.classList##add (Js.string "sort-indicator"));
        ignore (indicator##.classList##add (Js.string "ml-1"));
        ignore (indicator##.classList##add (Js.string "inline-block"));
        set_inner_html indicator svg_neutral;
        Dom.appendChild th indicator
      end;
      (* Clickable styling *)
      th##.style##.cssText :=
        Js.string ((Js.to_string th##.style##.cssText) ^ ";cursor:pointer");
      th##setAttribute (Js.string "role") (Js.string "button");
      (* Click handler *)
      ignore (Dom_html.addEventListener th Dom_html.Event.click
        (Dom_html.handler (fun _ev ->
          let table_id = match get_attr table "id" with
            | Some id -> id | None -> "default" in
          let col_key = match get_attr th "data-sort-key" with
            | Some k -> k | None -> string_of_int col_index in
          let current = Hashtbl.find_opt sort_states table_id in
          let dir = match current with
            | Some (c, d) when c = col_index ->
              if d = "desc" then "asc" else "desc"
            | _ -> "desc"
          in
          Hashtbl.replace sort_states table_id (col_index, dir);
          sort_table table col_index dir;
          update_indicators table col_index dir;
          update_url col_key dir;
          Js._true))
        Js._false)
    ) headers;
    (* Apply initial sort from URL params *)
    let loc = Dom_html.window##.location##.href in
    let url = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "URL")
      [| Js.Unsafe.inject loc |] in
    let sp = Js.Unsafe.get url "searchParams" in
    let sort_key =
      Js.Opt.to_option
        (Js.Opt.map
           (Js.Unsafe.meth_call sp "get"
              [| Js.Unsafe.inject (Js.string "sort") |])
           Js.to_string) in
    let sort_order =
      Js.Opt.to_option
        (Js.Opt.map
           (Js.Unsafe.meth_call sp "get"
              [| Js.Unsafe.inject (Js.string "order") |])
           Js.to_string) in
    match sort_key, sort_order with
    | Some key, Some order ->
      let sel = Printf.sprintf "thead th[data-sort-key=\"%s\"]" key in
      let ths = query_all (table :> Dom.node Js.t) sel in
      (match ths with
       | th_node :: _ ->
         let th = to_element th_node in
         let parent = Js.Opt.get th##.parentNode (fun () -> assert false) in
         let siblings = (Js.Unsafe.coerce parent)##.children in
         let ci = ref 0 in
         for i = 0 to siblings##.length - 1 do
           let sib = Js.Opt.get (siblings##item i) (fun () -> assert false) in
           if sib == (th :> Dom.node Js.t) then ci := i
         done;
         let table_id = match get_attr table "id" with
           | Some id -> id | None -> "default" in
         Hashtbl.replace sort_states table_id (!ci, order);
         sort_table table !ci order;
         update_indicators table !ci order
       | [] -> ())
    | _ -> ()
  end

(* ── Hydration entry point ─────────────────── *)

let hydrate_subtree (root : Dom.node Js.t) =
  (* Find island containers *)
  let containers = query_all root "[data-island='table_sort']:not([data-island-hydrated])" in
  List.iter (fun container_node ->
    let container = to_element container_node in
    (* Init all sortable tables inside this island container *)
    let tables = query_all (container :> Dom.node Js.t) "table[data-sortable]" in
    List.iter (fun t -> init_sortable_table (to_element t)) tables;
    (* Mark hydrated *)
    container##setAttribute (Js.string "data-island-hydrated") (Js.string "true")
  ) containers

let () =
  Console.console##log
    (Js.string "[wasm-island:table_sort] loaded");
  (* Initial hydration *)
  hydrate_subtree (Dom_html.document :> Dom.node Js.t);
  (* Re-hydrate after HTMX swaps *)
  ignore (Dom_html.addEventListener Dom_html.document##.body
    (Dom_html.Event.make "htmx:afterSwap")
    (Dom_html.handler (fun ev ->
      let target =
        Js.Opt.get (Js.Unsafe.get (Js.Unsafe.get ev "detail") "target")
          (fun () -> (Dom_html.document :> Dom.node Js.t)) in
      hydrate_subtree target;
      Js._true))
    Js._false)
