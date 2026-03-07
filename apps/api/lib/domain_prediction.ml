(** Prediction types — game outcome prediction and live game state.
    Standalone module: no dependencies on other domain modules. *)

(** Live game state *)
type live_game = {
  lg_game_id : string;
  lg_home_team : string;
  lg_away_team : string;
  lg_home_score : int;
  lg_away_score : int;
  lg_quarter : string;  (* "Q1", "Q2", "Q3", "Q4", "OT", "FINAL" *)
  lg_time_remaining : string;  (* "10:00", "FINAL" *)
  lg_is_live : bool;
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
  predicted_margin: float; (** Positive means Team A wins, negative means Team B wins *)
  predicted_total_score: float; (** Estimated total points (Over/Under) *)
}

(** Prediction context inputs/breakdown (optional, best-effort). *)
type roster_core_status = {
  rcs_present: int;
  rcs_total: int;
}

type what_if_missing = {
  wim_player_id: string;
  wim_player_name: string;
  wim_team: string;
  wim_pts_impact: float;
  wim_reb_impact: float;
  wim_ast_impact: float;
  wim_eff_impact: float;
}

type prediction_context_input = {
  pci_home_roster: roster_core_status option;
  pci_away_roster: roster_core_status option;
  pci_what_if_missing: what_if_missing list;
}

type prediction_context_breakdown = {
  pcb_delta: float;
  pcb_form_home: float;
  pcb_form_away: float;
  pcb_form_delta: float;
  pcb_roster_home: roster_core_status option;
  pcb_roster_away: roster_core_status option;
  pcb_roster_delta: float;
  pcb_rest_home_days: int option;
  pcb_rest_away_days: int option;
  pcb_rest_delta: float;
}

(** Prediction breakdown (nerd mode) *)
type prediction_breakdown = {
  pb_season: string;
  pb_is_neutral: bool;
  pb_games_used: int;
  pb_elo_home: float;
  pb_elo_away: float;
  pb_elo_prob: float;
  pb_pyth_home: float;
  pb_pyth_away: float;
  pb_pyth_prob: float;
  pb_stats_prob: float;
  pb_base_prob: float;
  pb_context: prediction_context_breakdown option;
  pb_final_prob: float;
}

type prediction_output = {
  result: prediction_result;
  breakdown: prediction_breakdown;
}
