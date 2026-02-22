(** Minimal parser for application/x-www-form-urlencoded request bodies.

    We keep this tiny (no dependency on a web framework) so it's easy to test
    and reuse in small admin/ops endpoints. *)

let decode_component (s : string) : string =
  (* In x-www-form-urlencoded, spaces are encoded as '+'. *)
  let s = String.map (fun c -> if c = '+' then ' ' else c) s in
  Uri.pct_decode s

let parse (body : string) : (string * string) list =
  if String.trim body = "" then
    []
  else
    body
    |> String.split_on_char '&'
    |> List.filter_map (fun part ->
      if part = "" then None
      else
        match String.index_opt part '=' with
        | None ->
            Some (decode_component part, "")
        | Some i ->
            let k = String.sub part 0 i in
            let v = String.sub part (i + 1) (String.length part - i - 1) in
            Some (decode_component k, decode_component v)
    )

let find (fields : (string * string) list) (key : string) : string option =
  let key = String.trim key in
  if key = "" then None
  else List.assoc_opt key fields

