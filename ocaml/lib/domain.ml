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
