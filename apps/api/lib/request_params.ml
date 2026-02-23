open Domain

let latest_season_code (seasons : season_info list) : string =
  match List.rev seasons with
  | s :: _ -> s.code
  | [] -> "ALL"

let normalize_season ~(seasons : season_info list) ~(requested : string) : string =
  let requested = String.trim requested in
  if requested = "" then
    latest_season_code seasons
  else
    let code = String.uppercase_ascii requested in
    if code = "ALL" then
      "ALL"
    else if List.exists (fun (s : season_info) -> s.code = code) seasons then
      code
    else
      latest_season_code seasons

