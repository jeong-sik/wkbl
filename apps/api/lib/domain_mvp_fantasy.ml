(** MVP race and fantasy scoring types.
    Depends on Domain_core for player_aggregate. *)

open Domain_core

(** MVP Race types *)
type mvp_candidate = {
  mvp_rank: int;
  mvp_player_id: string;
  mvp_player_name: string;
  mvp_team_name: string;
  mvp_team_code: string option;
  mvp_games_played: int;
  mvp_ppg: float;
  mvp_rpg: float;
  mvp_apg: float;
  mvp_spg: float;
  mvp_bpg: float;
  mvp_efficiency: float;
  mvp_team_wins: int;
  mvp_team_losses: int;
  mvp_team_win_pct: float;
  mvp_base_score: float;
  mvp_win_bonus: float;
  mvp_final_score: float;
}

(** Calculate MVP score from stats
    Formula:
    - Base Score = (PPG * 2) + (RPG * 1.2) + (APG * 1.5) + (SPG * 2) + (BPG * 2) + (EFF * 0.5)
    - Win Bonus = Team Win% * 20
    - Final Score = Base Score + Win Bonus
*)
let calculate_mvp_score ~ppg ~rpg ~apg ~spg ~bpg ~efficiency ~win_pct =
  (* Input validation: check for NaN values and invalid win_pct range *)
  let is_valid x = not (Float.is_nan x) in
  let all_valid = [ppg; rpg; apg; spg; bpg; efficiency; win_pct] |> List.for_all is_valid in
  if not all_valid || win_pct < 0.0 || win_pct > 1.0 then
    (0.0, 0.0, 0.0)
  else
    let base_score = (ppg *. 2.0) +. (rpg *. 1.2) +. (apg *. 1.5) +. (spg *. 2.0) +. (bpg *. 2.0) +. (efficiency *. 0.5) in
    let win_bonus = win_pct *. 20.0 in
    (base_score, win_bonus, base_score +. win_bonus)

(** Fantasy scoring types and functions *)
type fantasy_scoring_rule = {
  fsr_points: float;      (** Points per point scored *)
  fsr_rebounds: float;    (** Points per rebound *)
  fsr_assists: float;     (** Points per assist *)
  fsr_steals: float;      (** Points per steal *)
  fsr_blocks: float;      (** Points per block *)
  fsr_turnovers: float;   (** Points per turnover (usually negative) *)
}

(** Default fantasy scoring rules based on standard fantasy basketball *)
let default_fantasy_rules = {
  fsr_points = 1.0;
  fsr_rebounds = 1.2;
  fsr_assists = 1.5;
  fsr_steals = 2.0;
  fsr_blocks = 2.0;
  fsr_turnovers = -1.0;
}

(** Fantasy player score result *)
type fantasy_player_score = {
  fps_player_id: string;
  fps_player_name: string;
  fps_team_name: string;
  fps_games_played: int;
  fps_total_score: float;
  fps_avg_score: float;
  fps_pts_contrib: float;
  fps_reb_contrib: float;
  fps_ast_contrib: float;
  fps_stl_contrib: float;
  fps_blk_contrib: float;
  fps_tov_contrib: float;
}

(** Calculate fantasy score for a single game or totals *)
let calculate_fantasy_score
    ~(rules: fantasy_scoring_rule)
    ~pts ~reb ~ast ~stl ~blk ~tov =
  let pts_contrib = (Float.of_int pts) *. rules.fsr_points in
  let reb_contrib = (Float.of_int reb) *. rules.fsr_rebounds in
  let ast_contrib = (Float.of_int ast) *. rules.fsr_assists in
  let stl_contrib = (Float.of_int stl) *. rules.fsr_steals in
  let blk_contrib = (Float.of_int blk) *. rules.fsr_blocks in
  let tov_contrib = (Float.of_int tov) *. rules.fsr_turnovers in
  let total = pts_contrib +. reb_contrib +. ast_contrib +. stl_contrib +. blk_contrib +. tov_contrib in
  (total, pts_contrib, reb_contrib, ast_contrib, stl_contrib, blk_contrib, tov_contrib)

(** Calculate fantasy score from player_aggregate *)
let fantasy_score_of_aggregate
    ~(rules: fantasy_scoring_rule)
    (p: player_aggregate) : fantasy_player_score =
  let (total, pts_c, reb_c, ast_c, stl_c, blk_c, tov_c) =
    calculate_fantasy_score
      ~rules
      ~pts:p.total_points
      ~reb:p.total_rebounds
      ~ast:p.total_assists
      ~stl:p.total_steals
      ~blk:p.total_blocks
      ~tov:p.total_turnovers
  in
  let avg_score =
    if p.games_played > 0 then total /. (Float.of_int p.games_played)
    else 0.0
  in
  {
    fps_player_id = p.player_id;
    fps_player_name = p.name;
    fps_team_name = p.team_name;
    fps_games_played = p.games_played;
    fps_total_score = total;
    fps_avg_score = avg_score;
    fps_pts_contrib = pts_c;
    fps_reb_contrib = reb_c;
    fps_ast_contrib = ast_c;
    fps_stl_contrib = stl_c;
    fps_blk_contrib = blk_c;
    fps_tov_contrib = tov_c;
  }
