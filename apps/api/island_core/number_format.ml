(** Pure number formatting logic.
    Shared between server (wkbl lib) and client (Wasm islands). *)

type format_unit = { threshold : float; symbol : string }

let default_units =
  [
    { threshold = 1e9; symbol = "B" };
    { threshold = 1e6; symbol = "M" };
    { threshold = 1e4; symbol = "\xEB\xA7\x8C" };  (* 만 in UTF-8 *)
    { threshold = 1e3; symbol = "K" };
  ]

let format_number ?(units = default_units) ?(decimals = 1) ?(min_threshold = 1000.0)
    (num : float) : string =
  if num < min_threshold then
    (* Below threshold: simple integer or decimal display *)
    if Float.is_integer num then string_of_int (int_of_float num)
    else Printf.sprintf "%.*f" decimals num
  else
    match
      List.find_opt (fun u -> num >= u.threshold) units
    with
    | Some u ->
        let scaled = num /. u.threshold in
        let s = Printf.sprintf "%.*f" decimals scaled in
        (* Strip trailing zeros after decimal point *)
        let s =
          if String.contains s '.' then
            let len = String.length s in
            let rec strip i =
              if i <= 0 then s
              else if s.[i] = '0' then strip (i - 1)
              else if s.[i] = '.' then String.sub s 0 i
              else String.sub s 0 (i + 1)
            in
            strip (len - 1)
          else s
        in
        s ^ u.symbol
    | None ->
        if Float.is_integer num then string_of_int (int_of_float num)
        else Printf.sprintf "%.*f" decimals num
