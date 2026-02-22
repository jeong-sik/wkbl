(** Shared route helper functions used across all route modules. *)

let query_bool request name =
  Kirin.query_opt name request
  |> Option.fold ~none:false ~some:(fun v ->
      List.mem (String.lowercase_ascii (String.trim v)) ["1"; "true"; "yes"; "on"])

let query_nonempty request name =
  match Kirin.query_opt name request with
  | None -> None
  | Some v ->
      let trimmed = String.trim v in
      if trimmed = "" then None else Some trimmed

let query_season_or_latest request (seasons : Domain.season_info list) =
  let requested = query_nonempty request "season" |> Option.value ~default:"" in
  Request_params.normalize_season ~seasons ~requested

let is_safe_redirect_path (s : string) =
  let t = String.trim s in
  String.length t > 0
  && String.get t 0 = '/'
  && (String.length t = 1 || String.get t 1 <> '/')
  && not (String.contains t '\n')
  && not (String.contains t '\r')

let request_lang request =
  Option.bind (Kirin.header "Cookie" request) I18n.lang_of_cookie_header
  |> Option.value ~default:I18n.Ko
