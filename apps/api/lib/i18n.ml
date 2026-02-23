(** Minimal i18n helpers for WKBL.win.

    Policy:
    - Default language is Korean (ko).
    - Language selection is stored in a cookie and can be changed via /lang/:code.
    - When a translation is missing, the Korean text should be used as the source of truth. *)

type lang =
  | Ko
  | En

let cookie_name = "wkbl_lang"

let lang_to_code = function
  | Ko -> "ko"
  | En -> "en"

let lang_of_code code =
  match String.lowercase_ascii (String.trim code) with
  | "ko" | "kr" -> Some Ko
  | "en" -> Some En
  | _ -> None

let html_lang = lang_to_code

type text = { ko : string; en : string }

let t (lang : lang) (x : text) =
  match lang with
  | Ko -> x.ko
  | En -> x.en

let set_cookie_header (lang : lang) =
  (* One year, path-wide, safe defaults. *)
  Printf.sprintf
    "%s=%s; Path=/; Max-Age=31536000; SameSite=Lax; HttpOnly"
    cookie_name
    (lang_to_code lang)

let cookie_value (cookie_header : string) (name : string) : string option =
  let parts = String.split_on_char ';' cookie_header in
  let name = String.trim name in
  let rec find = function
    | [] -> None
    | p :: ps ->
        let p = String.trim p in
        (match String.index_opt p '=' with
        | None -> find ps
        | Some i ->
            let k = String.sub p 0 i |> String.trim in
            if k <> name then
              find ps
            else
              let v = String.sub p (i + 1) (String.length p - i - 1) |> String.trim in
              Some v)
  in
  if cookie_header = "" || name = "" then None else find parts

let lang_of_cookie_header (cookie_header : string) : lang option =
  Option.bind (cookie_value cookie_header cookie_name) lang_of_code
