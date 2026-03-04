(** Game flow types — score flow extraction, PBP analysis, quarter scores.
    Depends on Domain_core for pbp_event and game_info. *)

open Domain_core

(** Score flow point for game flow chart visualization *)
type score_flow_point = {
  sfp_clock: string;       (** Time remaining in period, e.g. "09:30" *)
  sfp_period: string;      (** Period code, e.g. "Q1", "Q2", "X1" *)
  sfp_home_score: int;     (** Home team cumulative score *)
  sfp_away_score: int;     (** Away team cumulative score *)
  sfp_diff: int;           (** Score difference: home - away *)
  sfp_elapsed_seconds: int; (** Total elapsed seconds from game start *)
}

(** Convert period code to period number (1-4 for Q1-Q4, 5+ for OT) *)
let period_to_number = function
  | "Q1" -> 1 | "Q2" -> 2 | "Q3" -> 3 | "Q4" -> 4
  | "X1" -> 5 | "X2" -> 6 | "X3" -> 7 | "X4" -> 8
  | _ -> 0

(** Parse clock string "MM:SS" to seconds remaining in period *)
let parse_clock_to_seconds clock =
  try
    let parts = String.split_on_char ':' clock in
    match parts with
    | [min_str; sec_str] ->
        let mins = int_of_string (String.trim min_str) in
        let secs = int_of_string (String.trim sec_str) in
        mins * 60 + secs
    | _ -> 600 (* Default to 10 minutes if parse fails *)
  with Failure _ -> 600

(** Calculate total elapsed seconds from game start *)
let calculate_elapsed_seconds ~period_code ~clock =
  let period_num = period_to_number period_code in
  let period_length = if period_num <= 4 then 600 else 300 in (* 10 min quarters, 5 min OT *)
  let seconds_remaining = parse_clock_to_seconds clock in
  let completed_periods =
    if period_num <= 4 then (period_num - 1) * 600
    else (4 * 600) + ((period_num - 5) * 300)
  in
  completed_periods + (period_length - seconds_remaining)

(** Extract score flow points from PBP events for chart visualization.
    Returns a list of points where scores changed, sorted by elapsed time. *)
let extract_score_flow (events: pbp_event list) : score_flow_point list =
  let score_changes =
    events
    |> List.filter_map (fun e ->
        match (e.pe_team1_score, e.pe_team2_score) with
        (* NOTE: In our PBP model, team1_score is AWAY and team2_score is HOME. *)
        | (Some away, Some home) ->
            let elapsed = calculate_elapsed_seconds ~period_code:e.pe_period_code ~clock:e.pe_clock in
            Some
              ( elapsed,
                e.pe_event_index,
                {
                  sfp_clock = e.pe_clock;
                  sfp_period = e.pe_period_code;
                  sfp_home_score = home;
                  sfp_away_score = away;
                  sfp_diff = home - away;
                  sfp_elapsed_seconds = elapsed;
                }
              )
        | _ -> None)
  in
  (* Sort by elapsed time; when multiple score changes share the same timestamp,
     keep their original event order stable via event_index. *)
  let sorted =
    score_changes
    |> List.sort (fun (t1, i1, _) (t2, i2, _) ->
         let c = compare t1 t2 in
         if c <> 0 then c else compare i1 i2)
    |> List.map (fun (_t, _i, p) -> p)
  in
  (* Deduplicate consecutive same scores using fold_left *)
  let dedup lst =
    List.fold_left (fun acc x ->
      match acc with
      | prev :: _ when prev.sfp_home_score = x.sfp_home_score && prev.sfp_away_score = x.sfp_away_score -> acc
      | _ -> x :: acc) [] lst |> List.rev
  in
  (* Add starting point at 0-0 if not present *)
  let with_start =
    match sorted with
    | [] -> []
    | first :: _ as pts ->
        if first.sfp_elapsed_seconds > 0 then
          { sfp_clock = "10:00"; sfp_period = "Q1"; sfp_home_score = 0; sfp_away_score = 0;
            sfp_diff = 0; sfp_elapsed_seconds = 0 } :: pts
        else pts
  in
  dedup with_start

(** PBP periods should include regulation quarters for a finished game. *)
let pbp_periods_complete_for_regulation (periods : string list) : bool =
  List.for_all (fun p -> List.mem p periods) [ "Q1"; "Q2"; "Q3"; "Q4" ]

(** Decide whether to refresh PBP data on-demand.

    We treat missing quarters as a problem for finished games (KST) with non-zero scores.
    (We include today's games here because our boxscore pages are only served once stats exist.) *)
let pbp_should_backfill ~today_kst (g : game_info) (periods : string list) : bool =
  let scores_known = g.gi_home_score > 0 && g.gi_away_score > 0 in
  match periods with
  | [] ->
      (* Avoid fetching PBP for scheduled games (0-0), but allow live/finished games. *)
      scores_known
  | _ ->
      let game_date = String.trim g.gi_game_date in
      let is_not_future_game =
        game_date <> "" && String.compare game_date (String.trim today_kst) <= 0
      in
      scores_known && is_not_future_game && not (pbp_periods_complete_for_regulation periods)

let score_flow_last_scores (flow_points : score_flow_point list) : (int * int) option =
  match List.rev flow_points with
  | [] -> None
  | p :: _ -> Some (p.sfp_home_score, p.sfp_away_score)

let score_flow_matches_final ~final_home ~final_away (flow_points : score_flow_point list) : bool =
  match score_flow_last_scores flow_points with
  | None -> false
  | Some (h, a) -> h = final_home && a = final_away

(** Quarter score for game flow analysis *)
type quarter_score = {
  qs_period: string;      (* Q1, Q2, Q3, Q4, OT *)
  qs_home_score: int;     (* Cumulative home score at end of quarter *)
  qs_away_score: int;     (* Cumulative away score at end of quarter *)
}

(** Game flow summary from PBP data *)
type game_flow = {
  gf_quarters: quarter_score list;
  gf_lead_changes: int;
  gf_largest_lead_home: int;
  gf_largest_lead_away: int;
}
