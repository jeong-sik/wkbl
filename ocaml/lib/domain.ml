(** Domain Models - shared types across the application *)

type player_aggregate = {
  player_id: string;
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

type player_sort =
  | ByPoints
  | ByRebounds
  | ByAssists
  | ByEfficiency
  | ByMinutes

let player_sort_of_string = function
  | "pts" | "points" -> ByPoints
  | "reb" | "rebounds" -> ByRebounds
  | "ast" | "assists" -> ByAssists
  | "min" | "minutes" -> ByMinutes
  | _ -> ByEfficiency

type season_info = {
  code: string;
  name: string;
}

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

type team_margin = {
  season: string;
  team: string;
  gp: int;
  pts_for: int;
  pts_against: int;
}

type team_stats = {
  team: string;
  gp: int;
  min_total: float;
  pts: float;
  margin: float;
  pts_against: float;
  reb: float;
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

type team_scope =
  | Totals
  | PerGame

let team_scope_of_string = function
  | "totals" -> Totals
  | _ -> PerGame

let team_scope_to_string = function
  | Totals -> "totals"
  | PerGame -> "per_game"

type team_sort =
  | TeamByPoints
  | TeamByRebounds
  | TeamByAssists
  | TeamBySteals
  | TeamByBlocks
  | TeamByEfficiency
  | TeamByTsPct
  | TeamByFg3Pct
  | TeamByMinutes

let team_sort_of_string = function
  | "pts" -> TeamByPoints
  | "reb" -> TeamByRebounds
  | "ast" -> TeamByAssists
  | "stl" -> TeamBySteals
  | "blk" -> TeamByBlocks
  | "eff" -> TeamByEfficiency
  | "ts_pct" -> TeamByTsPct
  | "fg3_pct" -> TeamByFg3Pct
  | "min_total" -> TeamByMinutes
  | _ -> TeamByPoints

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

type game_summary = {
  game_id: string;
  game_date: string;
  home_team: string;
  away_team: string;
  home_score: int option;
  away_score: int option;
  game_type: string;
}

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

type game_boxscore = {
  boxscore_game: game_info;
  boxscore_home_players: boxscore_player_stat list;
  boxscore_away_players: boxscore_player_stat list;
}

type leader_entry = {
  le_player_id: string;
  le_player_name: string;
  le_team_name: string;
  le_stat_value: float;
}

type player_info = {
  id: string;
  name: string;
  position: string option;
  birth_date: string option;
  height: int option;
  weight: int option;
}

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
  ss_margin: float;
  ss_ts_pct: float;
  ss_efg_pct: float;
}

type player_game_stat = {
  game_id: string;
  game_date: string;
  opponent: string;
  is_home: bool;
  team_score: int option;
  opponent_score: int option;
  min: float;
  pts: int;
  reb: int;
  ast: int;
  stl: int;
  blk: int;
  tov: int;
}

type career_high_item = {
  chi_label: string;
  chi_value: int;
  chi_game_id: string;
  chi_game_date: string;
  chi_opponent: string;
  chi_is_home: bool;
}

type player_profile = {
  player: player_info;
  averages: player_aggregate;
  recent_games: player_game_stat list;
  season_breakdown: season_stats list;
  career_highs: career_high_item list option;
}

type team_game_result = {
  tgr_game_id: string;
  tgr_game_date: string;
  tgr_opponent: string;
  tgr_is_home: bool;
  tgr_team_score: int;
  tgr_opponent_score: int;
  tgr_is_win: bool;
}

type team_full_detail = {
  tfd_team_name: string;
  tfd_standing: team_standing option;
  tfd_roster: player_aggregate list;
  tfd_recent_games: team_game_result list;
}

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

(** Prediction types *)
type team_prediction_stats = {
  ps_pts: float;
  ps_reb: float;
  ps_ast: float;
  ps_stl: float;
  ps_blk: float;
  ps_eff: float;
  ps_win_pct: float;
}

type prediction_result = {
  prob_a: float;
  prob_b: float;
  winner: string;
}

(** Helper to map team names to codes *)
let team_code_of_string = function
  | "아산 우리은행 우리WON" | "우리은행" | "우리WON" | "WO" -> Some "WO"
  | "용인 삼성생명 블루밍스" | "삼성생명" | "SS" -> Some "SS"
  | "인천 신한은행 에스버드" | "신한은행" | "SH" -> Some "SH"
  | "청주 KB스타즈" | "KB스타즈" | "KB" -> Some "KB"
  | "부천 하나은행" | "하나은행" | "HN" -> Some "HN"
  | "부산 BNK 썸" | "BNK썸" | "BNK" | "BN" -> Some "BN"
  | _ -> None

let team_code_to_color = function
  | "WO" -> "#005BAA"
  | "SS" -> "#007AFF"
  | "SH" -> "#2B3990"
  | "KB" -> "#FFCC00"
  | "HN" -> "#009490"
  | "BN" -> "#D6001C"
  | _ -> "#666666"

let team_code_to_logo = function
  | "WO" -> Some "team_05.png"
  | "SS" -> Some "team_03.png"
  | "SH" -> Some "team_07.png"
  | "KB" -> Some "team_01.png"
  | "HN" -> Some "team_09.png"
  | "BN" -> Some "team_11.png"
  | _ -> None
