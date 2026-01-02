(** WKBL Domain Types

    Following "Make Impossible States Impossible" principle:
    - All types are fully specified
    - No stringly-typed data
    - Option for nullable, never null strings
*)

(** Team codes - exhaustive enumeration, no string matching at runtime *)
type team_code =
  | Woori       (* 우리은행 *)
  | Samsung     (* 삼성생명 *)
  | Shinhan     (* 신한은행 *)
  | KB          (* KB스타즈 *)
  | Hana        (* 하나은행 *)
  | BNK         (* BNK썸 *)
  | Unknown of string  (* 역사적 팀 또는 미지 *)
[@@deriving show, eq]

(** Team metadata - immutable record *)
type team = {
  code: team_code;
  name_kr: string;
  nickname: string;
  color: string;  (* hex color *)
}
[@@deriving show]

(** Player position - typed, not string *)
type position =
  | Guard
  | Forward
  | Center
  | GuardForward
  | ForwardCenter
[@@deriving show, eq]

(** Season identifier - year-based, validated *)
module Season : sig
  type t [@@deriving show, eq]
  val make : int -> t option
  val year : t -> int
end = struct
  type t = int [@@deriving show, eq]
  let make year =
    if year >= 1998 && year <= 2030 then Some year else None
  let year t = t
end

(** Game statistics - all numeric, no string parsing at use site *)
type game_stats = {
  player_id: int;
  player_name: string;
  team: team_code;
  minutes: float;
  points: int;
  rebounds: int;
  assists: int;
  steals: int;
  blocks: int;
  turnovers: int;
  field_goal_pct: float option;  (* None if 0 attempts *)
  three_point_pct: float option;
  free_throw_pct: float option;
}
[@@deriving show]

(** Aggregated player stats *)
type player_aggregate = {
  id: string;
  name: string;
  team_name: string;
  games_played: int;
  total_minutes: float;
  avg_points: float;
  avg_rebounds: float;
  avg_assists: float;
  avg_steals: float;
  avg_blocks: float;
  avg_turnovers: float;
  efficiency: float;      (* Game Score - single-game performance metric *)
  ts_pct: float;          (* True Shooting % - shooting efficiency including FTs *)
  efg_pct: float;         (* Effective FG % - adjusts for 3-point value *)
}
[@@deriving show]

(** Team standing stats *)
type team_standing = {
  team_name: string;
  games_played: int;
  wins: int;
  losses: int;
  win_pct: float;
  gb: float;
  avg_pts: float;
  avg_opp_pts: float;
  diff: float;
}
[@@deriving show, yojson]

(** Basic player info *)
type player = {
  id: string;
  name: string;
  position: string option;
  birth_date: string option;
  height: int option;
  weight: int option;
}
[@@deriving show]

(** Player stats for a specific game *)
type player_game_stat = {
  game_id: string;
  game_date: string;
  opponent: string;
  is_home: bool;
  min: float;
  pts: int;
  reb: int;
  ast: int;
  stl: int;
  blk: int;
  tov: int;
}
[@@deriving show]

(** Season-specific stats for a player *)
type season_stats = {
  ss_season_code: string;
  ss_season_name: string;
  ss_games_played: int;
  ss_total_minutes: float;
  ss_avg_points: float;
  ss_avg_rebounds: float;
  ss_avg_assists: float;
  ss_avg_steals: float;
  ss_avg_blocks: float;
  ss_avg_turnovers: float;
  ss_efficiency: float;
  ss_ts_pct: float;
  ss_efg_pct: float;
}
[@@deriving show]

(** Career high stats - single game maximums *)
type career_highs = {
  ch_max_pts: int;
  ch_max_pts_date: string;
  ch_max_pts_opponent: string;
  ch_max_reb: int;
  ch_max_reb_date: string;
  ch_max_reb_opponent: string;
  ch_max_ast: int;
  ch_max_ast_date: string;
  ch_max_ast_opponent: string;
  ch_max_stl: int;
  ch_max_stl_date: string;
  ch_max_blk: int;
  ch_max_blk_date: string;
}
[@@deriving show]

(** Full player profile with season breakdown *)
type player_profile = {
  player: player;
  averages: player_aggregate;  (* Career averages *)
  recent_games: player_game_stat list;
  season_breakdown: season_stats list;  (* Per-season stats *)
  career_highs: career_highs option;  (* Career high records *)
}
[@@deriving show]

(** Team aggregates - totals from game_stats *)
type team_totals = {
  season: string;
  team: string;
  gp: int;
  min_total: float;
  fg2_m: int;
  fg2_a: int;
  fg3_m: int;
  fg3_a: int;
  ft_m: int;
  ft_a: int;
  reb_off: int;
  reb_def: int;
  reb: int;
  ast: int;
  stl: int;
  blk: int;
  turnovers: int;
  pts: int;
}
[@@deriving show]

(** Team points from games table *)
type team_margin = {
  season: string;
  team: string;
  gp: int;
  pts_for: int;
  pts_against: int;
}
[@@deriving show]

(** Team stats for display (per-game or totals) *)
type team_stats = {
  season: string;
  team: string;
  gp: int;
  min_total: float;
  pts: float;
  margin: float;
  pts_against: float;
  reb: float;
  reb_off: float;
  reb_def: float;
  ast: float;
  stl: float;
  blk: float;
  turnovers: float;
  fg_pct: float;
  fg3_pct: float;
  ft_pct: float;
  efg_pct: float;
  ts_pct: float;
  eff: float;
}
[@@deriving show]

(** Team scopes *)
type team_scope = Totals | PerGame
[@@deriving show, eq]

type team_sort_field =
  | TeamByPoints
  | TeamByRebounds
  | TeamByAssists
  | TeamBySteals
  | TeamByBlocks
  | TeamByEfficiency
  | TeamByTsPct
  | TeamByFg3Pct
  | TeamByMinutes
[@@deriving show, eq]

(** Season list entry *)
type season_info = {
  code: string;
  name: string;
}
[@@deriving show]

(** Sort options - typed enum, not string *)
type sort_field =
  | ByPoints
  | ByRebounds
  | ByAssists
  | ByEfficiency
  | ByMinutes
[@@deriving show, eq]

type sort_order = Asc | Desc
[@@deriving show, eq]

(** Statistical category for league leaders - Parse Don't Validate *)
type stat_category =
  | Points
  | Rebounds
  | Assists
  | Steals
  | Blocks
[@@deriving show, eq]

(** Sort parsing - total functions returning option *)
let sort_field_of_string value =
  match String.lowercase_ascii value with
  | "pts" | "points" -> Some ByPoints
  | "reb" | "rebs" | "rebounds" -> Some ByRebounds
  | "ast" | "assists" -> Some ByAssists
  | "eff" | "efficiency" -> Some ByEfficiency
  | "min" | "minutes" -> Some ByMinutes
  | _ -> None

let sort_order_of_string value =
  match String.lowercase_ascii value with
  | "asc" -> Some Asc
  | "desc" -> Some Desc
  | _ -> None

(** Stat category parsing - Total function using Option *)
let stat_category_of_string value =
  match String.lowercase_ascii value with
  | "pts" | "points" -> Some Points
  | "reb" | "rebs" | "rebounds" -> Some Rebounds
  | "ast" | "assists" -> Some Assists
  | "stl" | "steals" -> Some Steals
  | "blk" | "blocks" -> Some Blocks
  | _ -> None

(** Stat category to display string *)
let stat_category_to_string = function
  | Points -> "Points"
  | Rebounds -> "Rebounds"
  | Assists -> "Assists"
  | Steals -> "Steals"
  | Blocks -> "Blocks"

(** Stat category to short code *)
let stat_category_to_code = function
  | Points -> "pts"
  | Rebounds -> "reb"
  | Assists -> "ast"
  | Steals -> "stl"
  | Blocks -> "blk"

(** All stat categories for iteration *)
let all_stat_categories = [Points; Rebounds; Assists; Steals; Blocks]

(** WKBL Season - Parse Don't Validate pattern (Codex design) *)
type season =
  | S2025_26  (* Current season: 046 *)
  | S2024_25  (* 045 *)
  | S2023_24  (* 044 *)
[@@deriving show, eq]

let default_season = S2025_26
let all_season_variants = [S2025_26; S2024_25; S2023_24]

let season_to_code = function
  | S2025_26 -> "046"
  | S2024_25 -> "045"
  | S2023_24 -> "044"

let season_of_code = function
  | "046" -> Some S2025_26
  | "045" -> Some S2024_25
  | "044" -> Some S2023_24
  | _ -> None

let season_to_name = function
  | S2025_26 -> "2025-2026"
  | S2024_25 -> "2024-2025"
  | S2023_24 -> "2023-2024"

let season_filter_label = function
  | None -> "All Seasons"
  | Some s -> season_to_name s

(** Legacy: Season info from database (for backwards compatibility) *)
type season_info = {
  season_code: string;
  season_name: string;
}
[@@deriving show]

(** Legacy: All available seasons for UI dropdown *)
let all_seasons = [
  { season_code = "046"; season_name = "2025-2026" };
  { season_code = "045"; season_name = "2024-2025" };
  { season_code = "044"; season_name = "2023-2024" };
]

(** Legacy: Get season display name from code *)
let season_name_of_code code =
  match List.find_opt (fun s -> s.season_code = code) all_seasons with
  | Some s -> s.season_name
  | None -> code

(** Filter options *)
type player_filter = {
  team: team_code option;
  search: string option;
  sort_by: sort_field;
  sort_order: sort_order;
  limit: int;
  season_code: string option;  (* None = all seasons *)
}

let default_filter = {
  team = None;
  search = None;
  sort_by = ByEfficiency;
  sort_order = Desc;
  limit = 50;
  season_code = Some "046";  (* Default to current season *)
}

let team_scope_of_string = function
  | "totals" -> Totals
  | "per_game" -> PerGame
  | _ -> PerGame

let team_scope_to_string = function
  | Totals -> "totals"
  | PerGame -> "per_game"

let team_sort_field_of_string value =
  match String.lowercase_ascii value with
  | "pts" | "points" -> TeamByPoints
  | "reb" | "rebounds" -> TeamByRebounds
  | "ast" | "assists" -> TeamByAssists
  | "stl" | "steals" -> TeamBySteals
  | "blk" | "blocks" -> TeamByBlocks
  | "eff" | "efficiency" -> TeamByEfficiency
  | "ts_pct" | "ts" -> TeamByTsPct
  | "fg3_pct" | "3p" -> TeamByFg3Pct
  | "min" | "minutes" | "min_total" -> TeamByMinutes
  | _ -> TeamByPoints

(** Team code parsing - Total function using Option *)
let team_code_of_string = function
  | "01" | "KB스타즈" | "KB" -> Some KB
  | "03" | "삼성생명" | "삼성" -> Some Samsung
  | "05" | "우리은행" | "우리" -> Some Woori
  | "07" | "신한은행" | "신한" -> Some Shinhan
  | "09" | "하나은행" | "하나" -> Some Hana
  | "11" | "BNK썸" | "BNK" -> Some BNK
  | s -> Some (Unknown s)  (* Historical teams *)

let team_code_to_kr = function
  | Woori -> "우리은행"
  | Samsung -> "삼성생명"
  | Shinhan -> "신한은행"
  | KB -> "KB스타즈"
  | Hana -> "하나은행"
  | BNK -> "BNK썸"
  | Unknown s -> s

let team_code_to_color = function
  | Woori -> "#005BAA"
  | Samsung -> "#007AFF"
  | Shinhan -> "#2B3990"
  | KB -> "#FFCC00"
  | Hana -> "#009490"
  | BNK -> "#D6001C"
  | Unknown _ -> "#666666"

(** Team code to logo filename - maps to static/images/team_XX.png *)
let team_code_to_logo = function
  | KB -> Some "team_01.png"
  | Samsung -> Some "team_03.png"
  | Woori -> Some "team_05.png"
  | Shinhan -> Some "team_07.png"
  | Hana -> Some "team_09.png"
  | BNK -> Some "team_11.png"
  | Unknown _ -> None

(** Get team logo URL from team name string *)
let team_logo_url team_name =
  match team_code_of_string team_name with
  | Some code ->
      (match team_code_to_logo code with
       | Some filename -> Some ("/static/images/" ^ filename)
       | None -> None)
  | None -> None

(** Get player photo URL - falls back to placeholder if not found *)
let player_photo_url player_id =
  "/static/images/player_" ^ player_id ^ ".png"

(** Position parsing - Total function *)
let position_of_string = function
  | "G" | "가드" -> Some Guard
  | "F" | "포워드" -> Some Forward
  | "C" | "센터" -> Some Center
  | "GF" | "G/F" -> Some GuardForward
  | "FC" | "F/C" -> Some ForwardCenter
  | _ -> None

let position_to_string = function
  | Guard -> "G"
  | Forward -> "F"
  | Center -> "C"
  | GuardForward -> "G/F"
  | ForwardCenter -> "F/C"

(** Head-to-Head game stats *)
type h2h_game = {
  game_id: string;
  game_date: string;
  player1_team: string;
  player2_team: string;
  player1_pts: int;
  player1_reb: int;
  player1_ast: int;
  player2_pts: int;
  player2_reb: int;
  player2_ast: int;
  winner_team: string;
  score_diff: int;
}
[@@deriving show]

(** Head-to-Head comparison result *)
type head_to_head = {
  player1: player_aggregate;
  player2: player_aggregate;
  games: h2h_game list;
  total_games: int;
  player1_wins: int;  (* games where player1's team won *)
  player2_wins: int;
  player1_avg_pts: float;
  player1_avg_reb: float;
  player1_avg_ast: float;
  player2_avg_pts: float;
  player2_avg_reb: float;
  player2_avg_ast: float;
}
[@@deriving show]

(** Prediction result *)
type prediction = {
  team_a: string;
  team_b: string;
  prob_a: float;
  prob_b: float;
  reason: string;
}
[@@deriving show, yojson]

(** ELO Rating for a team *)
type elo_rating = {
  elo_team_name: string;
  elo_rating: float;        (* Current ELO rating, default 1500 *)
  elo_games_played: int;
  elo_last_updated: string; (* ISO date string *)
}
[@@deriving show, yojson]

(** Team statistics for prediction model *)
type team_stats = {
  ts_team_name: string;
  ts_wins: int;
  ts_losses: int;
  ts_home_wins: int;
  ts_home_losses: int;
  ts_away_wins: int;
  ts_away_losses: int;
  ts_avg_pts_scored: float;
  ts_avg_pts_allowed: float;
  ts_recent_form: float;    (* Win rate in last 5 games *)
  ts_streak: int;           (* Positive = wins, negative = losses *)
}
[@@deriving show, yojson]

(** Head-to-head record between two teams *)
type team_h2h = {
  th_team_a: string;
  th_team_b: string;
  th_team_a_wins: int;
  th_team_b_wins: int;
  th_last_meeting: string option;
  th_last_winner: string option;
}
[@@deriving show, yojson]

(** Extended prediction result with multiple models *)
type prediction_result = {
  pr_team_a: string;
  pr_team_b: string;
  pr_elo_prob_a: float;         (* ELO-based probability *)
  pr_stats_prob_a: float;       (* Stats-based probability *)
  pr_combined_prob_a: float;    (* Weighted average *)
  pr_team_a_elo: float;
  pr_team_b_elo: float;
  pr_team_a_stats: team_stats;
  pr_team_b_stats: team_stats;
  pr_h2h: team_h2h option;
  pr_home_advantage: bool;      (* Is team_a playing at home? *)
  pr_reasons: string list;
}
[@@deriving show, yojson]

(** Game summary for schedule page *)
type game_summary = {
  game_id: string;
  game_date: string;        (* "Unknown" if NULL in DB *)
  home_team: string;
  away_team: string;
  home_score: int option;   (* None if game not played yet *)
  away_score: int option;
  game_type: string;        (* "01" = regular season *)
}
[@@deriving show]

(** Box score player stat - individual player's stats in a specific game *)
type boxscore_player_stat = {
  bs_player_id: string;
  bs_player_name: string;
  bs_team_code: string;
  bs_team_name: string;
  bs_minutes: float;
  bs_pts: int;
  bs_reb: int;
  bs_ast: int;
  bs_stl: int;
  bs_blk: int;
  bs_tov: int;
  bs_fg_made: int;
  bs_fg_att: int;
  bs_fg_pct: float;
  bs_fg3_made: int;
  bs_fg3_att: int;
  bs_fg3_pct: float;
  bs_ft_made: int;
  bs_ft_att: int;
  bs_ft_pct: float;
}
[@@deriving show]

(** Game info - metadata about a specific game *)
type game_info = {
  gi_game_id: string;
  gi_game_date: string;
  gi_home_team_code: string;
  gi_home_team_name: string;
  gi_away_team_code: string;
  gi_away_team_name: string;
  gi_home_score: int;
  gi_away_score: int;
}
[@@deriving show]

(** Full box score - game info plus both teams' player stats *)
type game_boxscore = {
  boxscore_game: game_info;
  boxscore_home_players: boxscore_player_stat list;
  boxscore_away_players: boxscore_player_stat list;
}
[@@deriving show]

(** Team recent game result *)
type team_game_result = {
  tgr_game_id: string;
  tgr_game_date: string;
  tgr_opponent: string;
  tgr_is_home: bool;
  tgr_team_score: int;
  tgr_opponent_score: int;
  tgr_is_win: bool;
}
[@@deriving show]

(** Team full detail - roster, stats, recent games *)
type team_full_detail = {
  tfd_team_name: string;
  tfd_standing: team_standing option;
  tfd_roster: player_aggregate list;
  tfd_recent_games: team_game_result list;
}
[@@deriving show]

(** League leader entry for home page summary *)
type leader_entry = {
  le_player_id: string;
  le_player_name: string;
  le_team_name: string;
  le_stat_value: float;
}
[@@deriving show]

(** League summary for home page *)
type league_summary = {
  ls_scoring_leaders: leader_entry list;
  ls_rebound_leaders: leader_entry list;
  ls_assist_leaders: leader_entry list;
  ls_recent_games: game_summary list;
}
[@@deriving show]

(* ============================================ *)
(* All-Time Records System                      *)
(* ============================================ *)

(** Record type - Career total, single season, or single game *)
type record_type =
  | CareerTotal     (* 통산 기록 *)
  | SingleSeason    (* 시즌 최고 기록 *)
  | SingleGame      (* 단일 경기 최고 기록 *)
[@@deriving show, eq]

(** Career record - Total stats across all seasons *)
type career_record = {
  cr_player_id: string;
  cr_player_name: string;
  cr_team_name: string;       (* Most recent team *)
  cr_seasons_played: int;     (* Number of seasons *)
  cr_games_played: int;
  cr_total_points: int;
  cr_total_rebounds: int;
  cr_total_assists: int;
  cr_total_steals: int;
  cr_total_blocks: int;
}
[@@deriving show]

(** Single season record - Best season performance *)
type single_season_record = {
  ssr_player_id: string;
  ssr_player_name: string;
  ssr_team_name: string;
  ssr_season_code: string;
  ssr_season_name: string;
  ssr_games_played: int;
  ssr_total_points: int;      (* Total points in season *)
  ssr_avg_points: float;      (* PPG *)
  ssr_total_rebounds: int;
  ssr_avg_rebounds: float;
  ssr_total_assists: int;
  ssr_avg_assists: float;
}
[@@deriving show]

(** Single game record - Best single game performance *)
type single_game_record = {
  sgr_player_id: string;
  sgr_player_name: string;
  sgr_team_name: string;
  sgr_game_id: string;
  sgr_game_date: string;
  sgr_opponent: string;
  sgr_stat_value: int;        (* The record value (pts, reb, ast, etc.) *)
  sgr_pts: int;
  sgr_reb: int;
  sgr_ast: int;
}
[@@deriving show]

(** Season leader - Winner of a stat category for a season *)
type season_leader = {
  sl_season_code: string;
  sl_season_name: string;
  sl_player_id: string;
  sl_player_name: string;
  sl_team_name: string;
  sl_stat_value: float;       (* Average per game *)
  sl_total_value: int;        (* Total for season *)
  sl_games_played: int;
}
[@@deriving show]

(** Team season record - Best/worst team season *)
type team_season_record = {
  tsr_team_name: string;
  tsr_season_code: string;
  tsr_season_name: string;
  tsr_wins: int;
  tsr_losses: int;
  tsr_win_pct: float;
  tsr_avg_points: float;
  tsr_avg_opp_points: float;
}
[@@deriving show]

(** Win/Loss streak record *)
type streak_record = {
  str_team_name: string;
  str_streak_length: int;
  str_start_date: string;
  str_end_date: string;
  str_season_code: string;
}
[@@deriving show]

(** Complete records collection for the records page *)
type all_time_records = {
  atr_career_points: career_record list;
  atr_career_rebounds: career_record list;
  atr_career_assists: career_record list;
  atr_season_points: single_season_record list;
  atr_season_rebounds: single_season_record list;
  atr_season_assists: single_season_record list;
  atr_game_points: single_game_record list;
  atr_game_rebounds: single_game_record list;
  atr_game_assists: single_game_record list;
  atr_scoring_leaders: season_leader list;
  atr_rebound_leaders: season_leader list;
  atr_assist_leaders: season_leader list;
  atr_team_best_seasons: team_season_record list;
}
[@@deriving show]