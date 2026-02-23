(** Pure table-sort value parsing and comparison logic.
    Shared between server (wkbl lib) and client (Wasm islands). *)

type sort_value =
  | Numeric of float
  | Text of string
  | Empty

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
