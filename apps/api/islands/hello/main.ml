(** Minimal Wasm island for validating the build pipeline.
    Logs a message and demonstrates island_core dependency. *)

open Js_of_ocaml

let () =
  (* Verify wasm_of_ocaml runtime *)
  Console.console##log
    (Js.string "[wasm-island:hello] island hydrated");

  (* Demonstrate island_core usage *)
  let v = Wkbl_island_core.Table_sort_logic.parse_sort_value "42.5%" in
  let label =
    match v with
    | Wkbl_island_core.Table_sort_logic.Numeric f ->
        Printf.sprintf "parsed: Numeric(%g)" f
    | Text s -> Printf.sprintf "parsed: Text(%s)" s
    | Empty -> "parsed: Empty"
  in
  Console.console##log (Js.string ("[wasm-island:hello] " ^ label));

  (* Find and mark any hello islands as hydrated *)
  let doc = Dom_html.document in
  let islands =
    doc##querySelectorAll (Js.string "[data-island='hello']")
  in
  for i = 0 to islands##.length - 1 do
    Js.Opt.iter (islands##item i) (fun node ->
        Js.Opt.iter (Dom_html.CoerceTo.element node) (fun el ->
            el##setAttribute
              (Js.string "data-island-hydrated")
              (Js.string "true");
            el##.textContent := Js.some (Js.string "Hello from Wasm.")))
  done
