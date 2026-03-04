(** Play analysis types — On/Off impact, lineup chemistry, shot chart.
    No inter-domain dependencies. Requires [str] library for shot zone parsing. *)

(* ============================================= *)
(* On/Off Impact Analysis Types                  *)
(* ============================================= *)

(** On-Court stats: team performance when player is on court *)
type on_court_stats = {
  ocs_games: int;                (** Number of games with on-court time *)
  ocs_minutes: float;            (** Total minutes on court *)
  ocs_team_pts: int;             (** Team points scored while on court *)
  ocs_opp_pts: int;              (** Opponent points while on court *)
  ocs_possessions: float;        (** Estimated possessions *)
}

(** Off-Court stats: team performance when player is off court *)
type off_court_stats = {
  ofcs_games: int;               (** Number of games with off-court time *)
  ofcs_minutes: float;           (** Total minutes off court *)
  ofcs_team_pts: int;            (** Team points scored while off court *)
  ofcs_opp_pts: int;             (** Opponent points while off court *)
  ofcs_possessions: float;       (** Estimated possessions *)
}

(** Complete On/Off Impact analysis for a player *)
type on_off_impact = {
  ooi_player_id: string;
  ooi_player_name: string;
  ooi_team_name: string;
  ooi_games_played: int;
  ooi_total_minutes: float;
  ooi_on_court: on_court_stats;
  ooi_off_court: off_court_stats;
  ooi_net_rating_on: float;      (** Net Rating when on court (per 100 possessions) *)
  ooi_net_rating_off: float;     (** Net Rating when off court (per 100 possessions) *)
  ooi_net_rating_diff: float;    (** Difference: on - off (positive = good impact) *)
  ooi_plus_minus_total: int;     (** Total Plus/Minus across all games *)
  ooi_plus_minus_avg: float;     (** Average Plus/Minus per game *)
}

(** Calculate Net Rating: (Team Pts - Opp Pts) per 100 possessions *)
let calculate_net_rating ~team_pts ~opp_pts ~possessions : float =
  if possessions <= 0.0 then 0.0
  else
    let diff = float_of_int (team_pts - opp_pts) in
    (diff /. possessions) *. 100.0

(** Calculate offensive rating: Team points per 100 possessions *)
let calculate_off_rating ~team_pts ~possessions : float =
  if possessions <= 0.0 then 0.0
  else (float_of_int team_pts /. possessions) *. 100.0

(** Calculate defensive rating: Opponent points per 100 possessions *)
let calculate_def_rating ~opp_pts ~possessions : float =
  if possessions <= 0.0 then 0.0
  else (float_of_int opp_pts /. possessions) *. 100.0

(** Estimate possessions from basic stats
    Simple formula: FGA + 0.44*FTA + TOV - OREB *)
let estimate_possessions ~fga ~fta ~tov ~oreb : float =
  float_of_int fga +. (0.44 *. float_of_int fta) +. float_of_int tov -. float_of_int oreb

(** Create on_off_impact from aggregated data *)
let create_on_off_impact
    ~player_id ~player_name ~team_name
    ~games_played ~total_minutes
    ~on_minutes ~on_team_pts ~on_opp_pts ~on_possessions
    ~off_minutes ~off_team_pts ~off_opp_pts ~off_possessions
    ~plus_minus_total
    : on_off_impact =
  let on_games = if on_minutes > 0.0 then games_played else 0 in
  let off_games = if off_minutes > 0.0 then games_played else 0 in
  let on_court = {
    ocs_games = on_games;
    ocs_minutes = on_minutes;
    ocs_team_pts = on_team_pts;
    ocs_opp_pts = on_opp_pts;
    ocs_possessions = on_possessions;
  } in
  let off_court = {
    ofcs_games = off_games;
    ofcs_minutes = off_minutes;
    ofcs_team_pts = off_team_pts;
    ofcs_opp_pts = off_opp_pts;
    ofcs_possessions = off_possessions;
  } in
  let net_on = calculate_net_rating ~team_pts:on_team_pts ~opp_pts:on_opp_pts ~possessions:on_possessions in
  let net_off = calculate_net_rating ~team_pts:off_team_pts ~opp_pts:off_opp_pts ~possessions:off_possessions in
  {
    ooi_player_id = player_id;
    ooi_player_name = player_name;
    ooi_team_name = team_name;
    ooi_games_played = games_played;
    ooi_total_minutes = total_minutes;
    ooi_on_court = on_court;
    ooi_off_court = off_court;
    ooi_net_rating_on = net_on;
    ooi_net_rating_off = net_off;
    ooi_net_rating_diff = net_on -. net_off;
    ooi_plus_minus_total = plus_minus_total;
    ooi_plus_minus_avg = if games_played > 0 then float_of_int plus_minus_total /. float_of_int games_played else 0.0;
  }

(** Simple on/off impact calculation from plus_minus only *)
let simple_on_off_impact_from_plus_minus
    ~player_id ~player_name ~team_name
    ~games_played ~total_minutes ~plus_minus_total
    : on_off_impact =
  create_on_off_impact
    ~player_id ~player_name ~team_name
    ~games_played ~total_minutes
    ~on_minutes:total_minutes ~on_team_pts:0 ~on_opp_pts:0 ~on_possessions:0.0
    ~off_minutes:0.0 ~off_team_pts:0 ~off_opp_pts:0 ~off_possessions:0.0
    ~plus_minus_total

(** =================================================================
    Lineup Chemistry - 5인 라인업 조합별 성적 분석
    ================================================================= *)

(** Individual player in a lineup *)
type lineup_player = {
  lp_player_id: string;
  lp_player_name: string;
  lp_position: string option;
}

(** Statistics for a specific lineup combination *)
type lineup_stats = {
  ls_players: lineup_player list;      (** 5 players in lineup *)
  ls_team_name: string;                (** Team name *)
  ls_games_together: int;              (** Number of games played together *)
  ls_total_minutes: float;             (** Total minutes on court together *)
  ls_total_pts: int;                   (** Total points scored *)
  ls_total_opp_pts: int;               (** Total opponent points allowed *)
  ls_plus_minus: int;                  (** Net points (pts - opp_pts) *)
  ls_avg_pts_per_min: float;           (** Points per minute *)
  ls_avg_margin_per_min: float;        (** +/- per minute *)
}

(** Synergy score between players *)
type lineup_synergy = {
  syn_player1_id: string;
  syn_player1_name: string;
  syn_player2_id: string;
  syn_player2_name: string;
  syn_games_together: int;
  syn_total_minutes: float;
  syn_avg_plus_minus: float;
  syn_synergy_score: float;            (** Calculated synergy metric *)
}

(** Overall lineup chemistry analysis result *)
type lineup_chemistry = {
  lc_team_name: string;
  lc_season: string;
  lc_top_lineups: lineup_stats list;   (** Best performing lineups *)
  lc_frequent_lineups: lineup_stats list;  (** Most used lineups *)
  lc_synergies: lineup_synergy list;   (** Player pair synergies *)
}

(** Sort options for lineup display *)
type lineup_sort =
  | LineupByMinutes
  | LineupByPlusMinus
  | LineupByEfficiency
  | LineupByFrequency

let lineup_sort_of_string = function
  | "min" | "minutes" -> LineupByMinutes
  | "pm" | "plus_minus" -> LineupByPlusMinus
  | "eff" | "efficiency" -> LineupByEfficiency
  | _ -> LineupByFrequency

(** Create lineup key from player IDs (sorted for consistency) *)
let make_lineup_key (player_ids: string list) : string =
  player_ids
  |> List.sort String.compare
  |> String.concat ","

(** Calculate synergy score based on shared performance *)
let calculate_synergy_score ~games_together ~total_minutes ~avg_plus_minus : float =
  if games_together < 2 || total_minutes < 10.0 then 0.0
  else
    let minutes_weight = min 1.0 (total_minutes /. 100.0) in
    let games_weight = min 1.0 (float_of_int games_together /. 10.0) in
    avg_plus_minus *. minutes_weight *. games_weight

(** Compare two lineup_stats by a given sort criterion *)
let compare_lineup_stats (sort: lineup_sort) (a: lineup_stats) (b: lineup_stats) : int =
  match sort with
  | LineupByMinutes -> compare b.ls_total_minutes a.ls_total_minutes
  | LineupByPlusMinus -> compare b.ls_plus_minus a.ls_plus_minus
  | LineupByEfficiency -> compare b.ls_avg_margin_per_min a.ls_avg_margin_per_min
  | LineupByFrequency -> compare b.ls_games_together a.ls_games_together

(** Empty lineup stats for initialization *)
let empty_lineup_stats ~team_name : lineup_stats = {
  ls_players = [];
  ls_team_name = team_name;
  ls_games_together = 0;
  ls_total_minutes = 0.0;
  ls_total_pts = 0;
  ls_total_opp_pts = 0;
  ls_plus_minus = 0;
  ls_avg_pts_per_min = 0.0;
  ls_avg_margin_per_min = 0.0;
}

(* ========== Shot Chart Types ========== *)

(** Shot zone types based on PBP description patterns *)
type shot_zone =
  | Paint      (* 페인트존 - near basket *)
  | MidRange   (* 2점슛 without 페인트존 *)
  | ThreePoint (* 3점슛 *)

(** Shot result *)
type shot_result = Made | Missed

(** Single shot event *)
type shot_event = {
  se_player_name: string;
  se_zone: shot_zone;
  se_result: shot_result;
  se_game_id: string;
  se_period: string;
  se_clock: string;
}

(** Zone stats for shot chart *)
type zone_stats = {
  zs_zone: shot_zone;
  zs_made: int;
  zs_attempts: int;
  zs_pct: float;
}

(** Player shot chart data
    Note: WKBL PBP data only tags paint zone on successful shots.
    Missed paint shots appear as generic "2점슛시도", indistinguishable
    from mid-range misses. Therefore we combine all 2PT shots into one zone
    and report paint_made as supplementary info. *)
type player_shot_chart = {
  psc_player_id: string;
  psc_player_name: string;
  psc_team_name: string;
  psc_two_pt: zone_stats;    (** All 2-point shots combined (paint + mid-range) *)
  psc_three: zone_stats;     (** 3-point shots *)
  psc_paint_made: int;       (** Paint zone successes only — misses not tracked *)
  psc_total_made: int;
  psc_total_attempts: int;
  psc_total_pct: float;
}

(** Parse shot zone from description *)
let parse_shot_zone desc =
  if String.length desc >= 3 then
    if String.sub desc 0 3 = "페인" ||
       (try let _ = Str.search_forward (Str.regexp "페인트존") desc 0 in true with Not_found -> false)
    then Some Paint
    else if (try let _ = Str.search_forward (Str.regexp "3점슛") desc 0 in true with Not_found -> false)
    then Some ThreePoint
    else if (try let _ = Str.search_forward (Str.regexp "2점슛") desc 0 in true with Not_found -> false)
    then Some MidRange
    else None
  else None

(** Parse shot result from description *)
let parse_shot_result desc =
  if (try let _ = Str.search_forward (Str.regexp "성공") desc 0 in true with Not_found -> false)
  then Some Made
  else if (try let _ = Str.search_forward (Str.regexp "시도") desc 0 in true with Not_found -> false)
  then Some Missed
  else None

(** Calculate zone stats *)
let calc_zone_stats made attempts : zone_stats =
  let zone = Paint in (* placeholder, will be set by caller *)
  {
    zs_zone = zone;
    zs_made = made;
    zs_attempts = attempts;
    zs_pct = if attempts > 0 then (float_of_int made /. float_of_int attempts) *. 100.0 else 0.0;
  }

(** String representation of shot zone *)
let string_of_zone = function
  | Paint -> "paint"
  | MidRange -> "mid"
  | ThreePoint -> "three"

let zone_label = function
  | Paint -> "페인트존"
  | MidRange -> "미드레인지"
  | ThreePoint -> "3점"
