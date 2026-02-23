(** Pure table-sort value parsing, comparison, and sorting logic.
    Shared between server (wkbl lib) and client (Wasm islands).

    All functions are pure — no DOM or I/O dependency. *)

type sort_value =
  | Numeric of float
  | Text of string
  | Empty

(** Column data type, detected from cell content. *)
type col_type = Number | Percent | TextCol

let parse_sort_value (text : string) : sort_value =
  let trimmed = String.trim text in
  if trimmed = "" || trimmed = "-" || trimmed = "N/A" then Empty
  else
    (* Percentage: strip trailing '%' *)
    let maybe_pct =
      if String.length trimmed > 0 && trimmed.[String.length trimmed - 1] = '%'
      then String.sub trimmed 0 (String.length trimmed - 1)
      else trimmed
    in
    (* Strip commas for number parsing *)
    let cleaned =
      String.to_seq maybe_pct
      |> Seq.filter (fun c -> c <> ',')
      |> String.of_seq
    in
    match float_of_string_opt cleaned with
    | Some f -> Numeric f
    | None -> Text (String.lowercase_ascii trimmed)

let compare_sort_values (a : sort_value) (b : sort_value) : int =
  match a, b with
  | Empty, Empty -> 0
  | Empty, _ -> -1
  | _, Empty -> 1
  | Numeric x, Numeric y -> Float.compare x y
  | Text x, Text y -> String.compare x y
  | Numeric _, Text _ -> -1
  | Text _, Numeric _ -> 1

(** Detect column type from cell text values. Returns the type of
    the first non-empty cell. *)
let detect_col_type (cell_texts : string list) : col_type =
  let rec aux = function
    | [] -> TextCol
    | t :: rest ->
      let trimmed = String.trim t in
      if trimmed = "" || trimmed = "-" then aux rest
      else if String.length trimmed > 0
              && trimmed.[String.length trimmed - 1] = '%' then Percent
      else
        let cleaned =
          String.to_seq trimmed
          |> Seq.filter (fun c -> c <> ',')
          |> String.of_seq
        in
        match float_of_string_opt cleaned with
        | Some _ -> Number
        | None -> TextCol
  in
  aux cell_texts

(** Stable sort: returns indices in sorted order.
    [values] is an array of sort_value, [dir] is "asc" or "desc".
    Returns array of original indices. *)
let sort_indices (values : sort_value array) (dir : string) : int array =
  let n = Array.length values in
  let indexed = Array.init n (fun i -> (i, values.(i))) in
  Array.sort (fun (ai, av) (bi, bv) ->
    let cmp = compare_sort_values av bv in
    let cmp = if dir = "desc" then -cmp else cmp in
    if cmp = 0 then Int.compare ai bi else cmp
  ) indexed;
  Array.map fst indexed
