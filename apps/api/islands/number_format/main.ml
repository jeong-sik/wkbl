(** Number-format Wasm island.
    Replaces static/js/number-format.js with wasm_of_ocaml.
    Formats large numbers into human-readable form (1.2K, 3.5만, etc.)
    using Wkbl_island_core.Number_format.

    Behaviour:
    - Discovers elements with [data-format-number] attribute
    - Reads optional [data-format-threshold] and [data-format-decimals]
    - Applies formatting, stores original value in title attribute
    - Re-processes after HTMX swaps
    - Marks container data-island-hydrated="true" *)

open Js_of_ocaml

(* ── JS interop helpers ────────────────────── *)

(** querySelectorAll returning OCaml list. *)
let query_all (root : #Dom.node Js.t) (sel : string) : Dom.node Js.t list =
  let nl = (Js.Unsafe.coerce root)##querySelectorAll (Js.string sel) in
  let n : int = nl##.length in
  List.init n (fun i -> Js.Opt.get (nl##item i) (fun () -> assert false))

(** Coerce Dom.node to Dom_html.element. *)
let to_element (n : Dom.node Js.t) : Dom_html.element Js.t =
  Js.Opt.get (Dom_html.CoerceTo.element n) (fun () -> assert false)

(** Get attribute value, None if missing. *)
let get_attr (el : Dom_html.element Js.t) (name : string) : string option =
  let v = el##getAttribute (Js.string name) in
  Js.Opt.to_option (Js.Opt.map v Js.to_string)

(* ── Format a single element ─────────────── *)

let process_element (el : Dom_html.element Js.t) : unit =
  match get_attr el "data-format-number" with
  | None -> ()
  | Some raw_value ->
    (* Strip commas for parsing *)
    let cleaned =
      String.to_seq raw_value
      |> Seq.filter (fun c -> c <> ',')
      |> String.of_seq
    in
    match float_of_string_opt cleaned with
    | None -> ()
    | Some num ->
      (* Read options from data attributes *)
      let threshold =
        match get_attr el "data-format-threshold" with
        | Some s -> (match float_of_string_opt s with Some f -> f | None -> 10000.0)
        | None -> 10000.0
      in
      let decimals =
        match get_attr el "data-format-decimals" with
        | Some s -> (match int_of_string_opt s with Some d -> d | None -> 1)
        | None -> 1
      in
      let formatted =
        Wkbl_island_core.Number_format.format_number
          ~decimals ~min_threshold:threshold num
      in
      (* Store original value for tooltip if not already set *)
      (match get_attr el "data-original-value" with
       | Some _ -> ()
       | None ->
         (* Format with commas for tooltip *)
         let original =
           if Float.is_integer num then
             let n = int_of_float num in
             let s = string_of_int (abs n) in
             let len = String.length s in
             let buf = Buffer.create (len + len / 3) in
             if n < 0 then Buffer.add_char buf '-';
             String.iteri (fun i c ->
               if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
               Buffer.add_char buf c
             ) s;
             Buffer.contents buf
           else
             Printf.sprintf "%.1f" num
         in
         el##setAttribute (Js.string "data-original-value") (Js.string original);
         el##setAttribute (Js.string "title") (Js.string original));
      (* Update displayed text *)
      el##.textContent := Js.some (Js.string formatted)

(* ── Process all matching elements ───────── *)

let process_all (root : Dom.node Js.t) : unit =
  let elements = query_all root "[data-format-number]" in
  List.iter (fun n -> process_element (to_element n)) elements

(* ── Hydration entry point ───────────────── *)

let hydrate_subtree (root : Dom.node Js.t) =
  (* Process all data-format-number elements in subtree *)
  process_all root;
  (* Find island containers and mark hydrated *)
  let containers = query_all root "[data-island='number_format']:not([data-island-hydrated])" in
  List.iter (fun container_node ->
    let container = to_element container_node in
    process_all (container :> Dom.node Js.t);
    container##setAttribute (Js.string "data-island-hydrated") (Js.string "true")
  ) containers

let () =
  Console.console##log
    (Js.string "[wasm-island:number_format] loaded");
  (* Initial processing *)
  hydrate_subtree (Dom_html.document :> Dom.node Js.t);
  (* Re-process after HTMX swaps *)
  ignore (Dom_html.addEventListener Dom_html.document##.body
    (Dom_html.Event.make "htmx:afterSwap")
    (Dom_html.handler (fun ev ->
      let target =
        Js.Opt.get (Js.Unsafe.get (Js.Unsafe.get ev "detail") "target")
          (fun () -> (Dom_html.document :> Dom.node Js.t)) in
      hydrate_subtree target;
      Js._true))
    Js._false)
