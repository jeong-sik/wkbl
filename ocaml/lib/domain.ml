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
  efficiency: float;
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

(** Filter options *)
type player_filter = {
  team: team_code option;
  search: string option;
  sort_by: sort_field;
  sort_order: sort_order;
  limit: int;
}

let default_filter = {
  team = None;
  search = None;
  sort_by = ByEfficiency;
  sort_order = Desc;
  limit = 50;
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
