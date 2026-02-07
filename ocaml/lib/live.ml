(** Live game scores for SSE streaming *)

open Domain

(** Get today's date string in YYYY-MM-DD format *)
let today_str () =
  let today = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (today.Unix.tm_year + 1900)
    (today.Unix.tm_mon + 1)
    today.Unix.tm_mday

(** Minimal JSON string escaping (for safe API responses). *)
let escape_json_string (s : string) : string =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(** In-memory state for live games (Thread-safe) *)
let current_games : live_game list Atomic.t = Atomic.make []

(** Update live games state *)
let update_games new_games =
  Atomic.set current_games new_games

(** Get current live games from memory *)
let get_current_games () =
  Atomic.get current_games

(** Convert live_game to JSON *)
let live_game_to_json (lg: live_game) =
  let q = String.trim lg.lg_quarter in
  let is_pre_game =
    (not lg.lg_is_live) && (q = "경기전" || q = "경기 전" || q = "예정")
  in
  let home_score_json = if is_pre_game then "null" else string_of_int lg.lg_home_score in
  let away_score_json = if is_pre_game then "null" else string_of_int lg.lg_away_score in
  Printf.sprintf
    {|{"game_id":"%s","home_team":"%s","away_team":"%s","home_score":%s,"away_score":%s,"quarter":"%s","time":"%s","is_live":%b}|}
    (escape_json_string lg.lg_game_id)
    (escape_json_string lg.lg_home_team)
    (escape_json_string lg.lg_away_team)
    home_score_json
    away_score_json
    (escape_json_string lg.lg_quarter)
    (escape_json_string lg.lg_time_remaining)
    lg.lg_is_live

(** Convert list of live games to JSON array *)
let live_games_to_json (games: live_game list) =
  let items = List.map live_game_to_json games in
  "[" ^ String.concat "," items ^ "]"

(** Get all live games as SSE event (from Memory) *)
let get_live_sse_event () =
  let games = get_current_games () in
  let json = live_games_to_json games in
  let event_id = Printf.sprintf "%d" (int_of_float (Unix.time ())) in
  Kirin.Sse.event "scores" json
  |> Kirin.Sse.with_id event_id
  |> Kirin.Sse.with_retry 30000  (* 30 second retry *)

(** Global broadcaster for live scores *)
let broadcaster = Kirin.Sse.Broadcaster.create ()

(** Broadcast current scores to all connected clients *)
let broadcast_scores () =
  let event = get_live_sse_event () in
  Kirin.Sse.Broadcaster.broadcast broadcaster event

(** SSE handler for live scores *)
let sse_handler req =
  (* Broadcast current state to update everyone (including new client) *)
  broadcast_scores ();
  Kirin.Sse.handler broadcaster req

(** Build a live status JSON payload for an explicit list of games. *)
let status_json_of_games (games : live_game list) : string =
  let json = live_games_to_json games in
  Printf.sprintf {|{"status":"ok","games":%s,"updated_at":%d,"today":"%s"}|}
    json (int_of_float (Unix.time ())) (escape_json_string (today_str ()))

(** Get live status as JSON for API endpoint *)
let get_status_json () =
  status_json_of_games (get_current_games ())
