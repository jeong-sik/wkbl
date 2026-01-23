(** Clutch Time Stats Calculation **)

open Domain

type clutch_criteria = {
  quarter: string list; (** e.g. ["4Q"; "OT1"; "OT2"] *)
  time_remaining: int; (** seconds *)
  score_diff: int; (** absolute difference <= this *)
}

(** Default NBA clutch definition: Last 5 mins, <= 5 pts diff *)
let default_criteria = {
  quarter = ["4Q"; "EQ"; "OT1"; "OT2"; "OT3"]; (* EQ might be End of Quarter? *)
  time_remaining = 300; (* 5 minutes *)
  score_diff = 5;
}

(** Parse clock string "MM:SS" to seconds *)
let parse_clock clock_str =
  try
    match String.split_on_char ':' clock_str with
    | [m; s] -> (int_of_string m * 60) + int_of_string s
    | _ -> 0
  with _ -> 0

let is_clutch_moment (event: pbp_event) (criteria: clutch_criteria) =
  let in_quarter = List.mem event.pe_period_code criteria.quarter in
  let time_left = parse_clock event.pe_clock in
  let in_time = time_left <= criteria.time_remaining in
  
  let s1 = Option.value ~default:0 event.pe_team1_score in
  let s2 = Option.value ~default:0 event.pe_team2_score in
  let diff = abs (s1 - s2) in
  let close_score = diff <= criteria.score_diff in
  
  in_quarter && in_time && close_score

(** Accumulated stats for a player *)
type player_clutch_stats = {
  player_name: string;
  team_side: int; (** 1 or 2, 0 if unknown *)
  mutable pts: int;
  mutable reb: int;
  mutable ast: int;
  mutable stl: int;
  mutable blk: int;
  mutable tov: int;
  mutable fg3_m: int;
  mutable fg2_m: int;
  mutable ft_m: int;
}

let create_empty_stats name side = {
  player_name = name;
  team_side = side;
  pts = 0;
  reb = 0;
  ast = 0;
  stl = 0;
  blk = 0;
  tov = 0;
  fg3_m = 0;
  fg2_m = 0;
  ft_m = 0;
}

(** Heuristic parser for Korean play-by-play descriptions.
    Returns (player_name option, stat_update_function) *)
let contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub > len_s then false
  else
    let rec loop i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else loop (i + 1)
    in
    loop 0

let parse_description desc =
  (* Simplistic splitting by space. 
     Assumption: "[Team] [Player] [Action]" or "[Player] [Action]" 
     Actually, looking at standard logs:
     "박지수 2점슛 성공"
     "김단비 리바운드 (공격)"
     
     We need to handle the case where player name might have spaces? 
     Korean names usually don't. Foreign names might.
  *)
  let parts = String.split_on_char ' ' desc in
  match parts with
  | [] -> None, fun _ -> ()
  | name :: action_parts ->
      let action = String.concat " " action_parts in
      
      (* Helper to update stats *)
      let update stats =
        if contains action "3점" && contains action "성공" then (
          stats.pts <- stats.pts + 3;
          stats.fg3_m <- stats.fg3_m + 1
        ) else if contains action "2점" && contains action "성공" then (
          stats.pts <- stats.pts + 2;
          stats.fg2_m <- stats.fg2_m + 1
        ) else if contains action "자유투" && contains action "성공" then (
          stats.pts <- stats.pts + 1;
          stats.ft_m <- stats.ft_m + 1
        ) else if contains action "리바운드" then (
          stats.reb <- stats.reb + 1
        ) else if contains action "어시스트" then (
          stats.ast <- stats.ast + 1
        ) else if contains action "스틸" then (
          stats.stl <- stats.stl + 1
        ) else if contains action "블록" then (
          stats.blk <- stats.blk + 1
        ) else if contains action "턴오버" then (
          stats.tov <- stats.tov + 1
        ) else (
          ()
        )
      in
      Some name, update

(** Aggregate stats from a list of events *)
let calculate_clutch_stats (events: pbp_event list) =
  let stats_map = Hashtbl.create 16 in
  
  (* We need to filter for clutch moments first *)
  let clutch_events = List.filter (fun e -> is_clutch_moment e default_criteria) events in
  
  List.iter (fun event ->
    let name_opt, update_fn = parse_description event.pe_description in
    match name_opt with
    | Some name ->
        let stats = 
          match Hashtbl.find_opt stats_map name with
          | Some s -> s
          | None -> 
              let s = create_empty_stats name event.pe_team_side in
              Hashtbl.add stats_map name s;
              s
        in
        update_fn stats
    | None -> ()
  ) clutch_events;
  
  (* Convert to list *)
  Hashtbl.fold (fun _ stats acc -> stats :: acc) stats_map []
