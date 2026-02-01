(** Live game scores for SSE streaming *)

open Domain

(** Get today's date string in YYYY-MM-DD format *)
let today_str () =
  let today = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (today.Unix.tm_year + 1900)
    (today.Unix.tm_mon + 1)
    today.Unix.tm_mday

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
  Printf.sprintf
    {|{"game_id":"%s","home_team":"%s","away_team":"%s","home_score":%d,"away_score":%d,"quarter":"%s","time":"%s","is_live":%b}|}
    lg.lg_game_id lg.lg_home_team lg.lg_away_team
    lg.lg_home_score lg.lg_away_score lg.lg_quarter lg.lg_time_remaining lg.lg_is_live

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

(** Get live status as JSON for API endpoint *)
let get_status_json () =
  let games = get_current_games () in
  let json = live_games_to_json games in
  Printf.sprintf {|{"status":"ok","games":%s,"updated_at":%d,"today":"%s"}|}
    json (int_of_float (Unix.time ())) (today_str ())
