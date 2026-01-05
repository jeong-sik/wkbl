(** Match Winner Prediction Engine

    Logic:
    - Pythagorean Expectation (Points for/against)
    - Statistical Probability (ELO-like relative strength)
    - Home Court Advantage
*)

open Domain

(** Model parameters *)
let home_court_boost = 0.05
let pythagorean_exponent = 13.91  (* Optimized for professional basketball *)

let clamp01 value =
  if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value

(** 1. Pythagorean Winning Percentage 
    formula: (pts_for^exp) / (pts_for^exp + pts_against^exp)
*)
let pythagorean_expectancy ~pts_for ~pts_against =
  let pf = float pts_for in
  let pa = float pts_against in
  if pf = 0.0 && pa = 0.0 then 0.5
  else
    let pf_exp = pf ** pythagorean_exponent in
    let pa_exp = pa ** pythagorean_exponent in
    pf_exp /. (pf_exp +. pa_exp)

(** 2. Statistical Probability based on per-game metrics *)
let stats_probability ~(stats_a: team_prediction_stats) ~(stats_b: team_prediction_stats) ~is_home_a =
  let wp_a = stats_a.ps_win_pct in
  let wp_b = stats_b.ps_win_pct in
  
  (* Efficiency comparison *)
  let eff_ratio =
    if stats_a.ps_eff = 0.0 && stats_b.ps_eff = 0.0 then 0.5
    else stats_a.ps_eff /. (stats_a.ps_eff +. stats_b.ps_eff)
  in
  
  (* Combine Win% and Efficiency *)
  let base_prob = (wp_a +. eff_ratio) /. 2.0 in
  let opponent_prob = (wp_b +. (1.0 -. eff_ratio)) /. 2.0 in
  
  let relative_strength = base_prob /. (base_prob +. opponent_prob) in
  
  (* Apply Home Boost *)
  let adjusted =
    if is_home_a then relative_strength +. home_court_boost
    else relative_strength -. home_court_boost
  in
  clamp01 adjusted

(** Main prediction function *)
let predict_match ~(team_a: team_prediction_stats) ~(team_b: team_prediction_stats) ~name_a ~name_b =
  let prob_a = stats_probability ~stats_a:team_a ~stats_b:team_b ~is_home_a:true in
  let prob_b = 1.0 -. prob_a in
  
  {
    prob_a;
    prob_b;
    winner = if prob_a >= prob_b then name_a else name_b;
  }
