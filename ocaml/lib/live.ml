(** Live game scores for SSE streaming *)

open Domain

(** Live game state *)
type live_game = {
  game_id : string;
  home_team : string;
  away_team : string;
  home_score : int;
  away_score : int;
  quarter : string;  (* "Q1", "Q2", "Q3", "Q4", "OT", "FINAL" *)
  time_remaining : string;  (* "10:00", "FINAL" *)
  is_live : bool;
}

(** Get today's date string in YYYY-MM-DD format *)
let today_str () =
  let today = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (today.Unix.tm_year + 1900)
    (today.Unix.tm_mon + 1)
    today.Unix.tm_mday

(** Get today's games from DB using existing Db.get_games *)
let get_todays_games () =
  let today = today_str () in
  match Db.get_games ~season:"ALL" () with
  | Error _ -> []
  | Ok games ->
      (* Filter for today's games *)
      List.filter (fun (g: game_summary) -> g.game_date = today) games

(** Convert game_summary to live_game *)
let game_to_live (g: game_summary) : live_game =
  {
    game_id = g.game_id;
    home_team = g.home_team;
    away_team = g.away_team;
    home_score = Option.value ~default:0 g.home_score;
    away_score = Option.value ~default:0 g.away_score;
    quarter = "FINAL";
    time_remaining = "FINAL";
    is_live = false;
  }

(** Convert live_game to JSON *)
let live_game_to_json (lg: live_game) =
  Printf.sprintf
    {|{"game_id":"%s","home_team":"%s","away_team":"%s","home_score":%d,"away_score":%d,"quarter":"%s","time":"%s","is_live":%b}|}
    lg.game_id lg.home_team lg.away_team
    lg.home_score lg.away_score lg.quarter lg.time_remaining lg.is_live

(** Convert list of live games to JSON array *)
let live_games_to_json (games: live_game list) =
  let items = List.map live_game_to_json games in
  "[" ^ String.concat "," items ^ "]"

(** Get all live games as SSE event *)
let get_live_sse_event () =
  let games = get_todays_games () in
  let live_games = List.map game_to_live games in
  let json = live_games_to_json live_games in
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
  (* Broadcast current state to new client *)
  broadcast_scores ();
  Kirin.Sse.handler broadcaster req

(** Get live status as JSON for API endpoint *)
let get_status_json () =
  let games = get_todays_games () in
  let live_games = List.map game_to_live games in
  let json = live_games_to_json live_games in
  Printf.sprintf {|{"status":"ok","games":%s,"updated_at":%d,"today":"%s"}|}
    json (int_of_float (Unix.time ())) (today_str ())
