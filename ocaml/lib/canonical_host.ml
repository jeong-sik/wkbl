let strip_port (host : string) : string =
  let h = String.trim host |> String.lowercase_ascii in
  match String.index_opt h ':' with
  | None -> h
  | Some i -> String.sub h 0 i

let should_redirect_to_wkbl (host : string) : bool =
  match strip_port host with
  | "woman.win" | "www.woman.win" -> true
  | _ -> false

let wkbl_location ~(request_uri : string) : string =
  let path =
    let t = String.trim request_uri in
    if t = "" then "/" else t
  in
  "https://wkbl.win" ^ path

