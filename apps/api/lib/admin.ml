(** Admin guard for QA override tools (exclude/restore).
    Cookie-based authentication with environment-variable token. *)

let cookie_name = "wkbl_admin"

let token_env : string option =
  match Sys.getenv_opt "WKBL_ADMIN_TOKEN" with
  | None -> None
  | Some s ->
      let s = String.trim s in
      if s = "" then None else Some s

let cookie_header ?(secure=false) (token : string) : string =
  (* Short-lived, path-wide; Strict reduces CSRF risk for these admin-only POSTs. *)
  let value = Uri.pct_encode token in
  let secure_attr = if secure then "; Secure" else "" in
  Printf.sprintf
    "%s=%s; Path=/; Max-Age=604800; SameSite=Strict; HttpOnly%s"
    cookie_name
    value
    secure_attr

let is_admin request : bool =
  match (token_env, Kirin.header "Cookie" request) with
  | Some expected, Some cookie_header ->
      (match I18n.cookie_value cookie_header cookie_name with
      | Some got -> Uri.pct_decode (String.trim got) = expected
      | None -> false)
  | _ -> false

let referer_to_path (referer : string) : string option =
  let r = String.trim referer in
  if r = "" then None
  else if String.starts_with ~prefix:"/" r then Some r
  else
    (* Best-effort: extract "/path?query" from an absolute URL. *)
    let len = String.length r in
    let rec find_scheme_slash i =
      if i + 2 >= len then None
      else if r.[i] = ':' && r.[i + 1] = '/' && r.[i + 2] = '/' then Some (i + 3)
      else find_scheme_slash (i + 1)
    in
    match find_scheme_slash 0 with
    | None -> None
    | Some start ->
        let rec find_path i =
          if i >= len then None
          else if r.[i] = '/' then Some i
          else find_path (i + 1)
        in
        (match find_path start with
        | None -> None
        | Some i -> Some (String.sub r i (len - i)))

let redirect_back_or_home request =
  let next_q = Kirin.query_opt "next" request |> Option.value ~default:"" |> String.trim in
  if next_q <> "" && Route_helpers.is_safe_redirect_path next_q then
    next_q
  else
    match Option.bind (Kirin.header "Referer" request) referer_to_path with
    | Some p when Route_helpers.is_safe_redirect_path p -> p
    | _ -> "/"
