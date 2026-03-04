(** Streak and clutch types — hot streaks, clutch time statistics, PBP scoring.
    Depends on Domain_core for pbp_event and player_game_stat. *)

open Domain_core

(** Hot Streaks Types *)
type streak_type =
  | WinStreak           (** Team consecutive wins *)
  | Points20Plus        (** 20+ points in consecutive games *)
  | DoubleDouble        (** Double-double in consecutive games *)
  | TripleDouble        (** Triple-double in consecutive games *)
  | Rebounds10Plus      (** 10+ rebounds in consecutive games *)
  | Assists7Plus        (** 7+ assists in consecutive games *)
  | Blocks3Plus         (** 3+ blocks in consecutive games *)
  | Steals3Plus         (** 3+ steals in consecutive games *)

let streak_type_to_string = function
  | WinStreak -> "win_streak"
  | Points20Plus -> "pts_20plus"
  | DoubleDouble -> "double_double"
  | TripleDouble -> "triple_double"
  | Rebounds10Plus -> "reb_10plus"
  | Assists7Plus -> "ast_7plus"
  | Blocks3Plus -> "blk_3plus"
  | Steals3Plus -> "stl_3plus"

let streak_type_to_label = function
  | WinStreak -> "연승"
  | Points20Plus -> "20+ 득점"
  | DoubleDouble -> "더블더블"
  | TripleDouble -> "트리플더블"
  | Rebounds10Plus -> "10+ 리바운드"
  | Assists7Plus -> "7+ 어시스트"
  | Blocks3Plus -> "3+ 블록"
  | Steals3Plus -> "3+ 스틸"

let streak_type_to_emoji = function
  | WinStreak -> "W"
  | Points20Plus -> "P"
  | DoubleDouble -> "DD"
  | TripleDouble -> "TD"
  | Rebounds10Plus -> "R"
  | Assists7Plus -> "A"
  | Blocks3Plus -> "B"
  | Steals3Plus -> "S"

type player_streak = {
  ps_player_id: string;
  ps_player_name: string;
  ps_team_name: string;
  ps_streak_type: streak_type;
  ps_current_count: int;
  ps_is_active: bool;  (** True if streak is ongoing *)
  ps_start_date: string;
  ps_end_date: string option;  (** None if still active *)
  ps_games: player_game_stat list;  (** Games in the streak *)
}

type team_streak = {
  ts_team_name: string;
  ts_streak_type: streak_type;
  ts_current_count: int;
  ts_is_active: bool;
  ts_start_date: string;
  ts_end_date: string option;
  ts_game_ids: string list;
}

type streak_record = {
  sr_holder_name: string;      (** Player or team name *)
  sr_holder_id: string option; (** Player ID if player streak *)
  sr_team_name: string option; (** Team name for player streaks *)
  sr_streak_type: streak_type;
  sr_count: int;
  sr_start_date: string;
  sr_end_date: string;
  sr_season: string;
}

(** Clutch Time Statistics
    Clutch time = Q4 remaining 5 minutes + score diff <= 5 points *)
type clutch_stats = {
  cs_player_id: string;
  cs_player_name: string;
  cs_team_name: string;
  cs_clutch_games: int;
  cs_clutch_points: int;
  cs_clutch_fg_made: int;
  cs_clutch_fg_att: int;
  cs_clutch_fg_pct: float;
  cs_clutch_ft_made: int;
  cs_clutch_ft_att: int;
  cs_clutch_3p_made: int;
}

(** Safe string prefix extraction *)
let safe_prefix s len =
  if String.length s >= len then Some (String.sub s 0 len) else None

(** Safe string suffix extraction *)
let safe_suffix s len =
  let slen = String.length s in
  if slen >= len then Some (String.sub s (slen - len) len) else None

(** Check if string starts with prefix *)
let starts_with s prefix =
  safe_prefix s (String.length prefix) = Some prefix

(** Check if string ends with suffix *)
let ends_with s suffix =
  safe_suffix s (String.length suffix) = Some suffix

(** Parse clock string "MM:SS" to seconds remaining *)
let parse_clock (clock: string) : int =
  try
    match String.split_on_char ':' clock with
    | [min; sec] ->
      let m = int_of_string (String.trim min) in
      let s = int_of_string (String.trim sec) in
      m * 60 + s
    | _ -> 0
  with Failure _ -> 0

(** Check if event is in clutch time
    Clutch time = Q4 + remaining <= 5 min + score diff <= 5 points *)
let is_clutch_time ~period_code ~clock ~score_diff : bool =
  period_code = "Q4" &&
  parse_clock clock <= 300 &&  (* 5 minutes = 300 seconds *)
  abs score_diff <= 5

(** Extract clutch time events from PBP data *)
let extract_clutch_events (events: pbp_event list) : pbp_event list =
  List.filter (fun e ->
    let score_diff = match e.pe_team1_score, e.pe_team2_score with
      | Some s1, Some s2 -> s1 - s2
      | _ -> 999  (* No score info = not clutch *)
    in
    is_clutch_time ~period_code:e.pe_period_code ~clock:e.pe_clock ~score_diff
  ) events

(** Parse points from PBP description
    Returns (points, is_made, is_3pt, is_ft) *)
let parse_scoring_from_description (desc: string) : (int * bool * bool * bool) option =
  let desc_lower = String.lowercase_ascii desc in
  (* Helper to check if shot was made *)
  let is_made d = ends_with d "성공" || ends_with d "made" in
  (* Korean basketball PBP patterns *)
  if String.length desc_lower = 0 then None
  else if starts_with desc_lower "자유투" || starts_with desc_lower "ft" then
    if is_made desc_lower then Some (1, true, false, true)
    else Some (0, false, false, true)
  else if starts_with desc_lower "3점" || starts_with desc_lower "3p" then
    if is_made desc_lower then Some (3, true, true, false)
    else Some (0, false, true, false)
  else if starts_with desc_lower "2점" || starts_with desc_lower "2p" ||
          starts_with desc_lower "슛" || starts_with desc_lower "layup" ||
          starts_with desc_lower "dunk" then
    if is_made desc_lower then Some (2, true, false, false)
    else Some (0, false, false, false)
  else
    None
