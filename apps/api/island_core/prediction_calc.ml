(** Pure prediction calculations shared between server and Wasm island.
    No Domain.ml dependency — all inputs are primitive floats/ints.
    Mirrors the formulas in prediction.ml without IO or domain type coupling. *)

(** Model constants *)
let home_court_boost = 0.05
let pythagorean_exponent = 13.91

(** Blend weights: Elo 55%, Pythagorean 20%, Stats 15%, H2H 10% *)
let w_elo = 0.55
let w_pyth = 0.20
let w_stats = 0.15
let w_h2h = 0.10

let clamp01 v =
  if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v

(** Log5 formula (Bill James): head-to-head probability from win percentages. *)
let log5 ~a ~b =
  let denom = (a *. (1.0 -. b)) +. ((1.0 -. a) *. b) in
  if denom = 0.0 then 0.5 else (a *. (1.0 -. b)) /. denom |> clamp01

(** Pythagorean Winning Percentage.
    pts_for / pts_against are season totals (int).
    Returns expected win fraction [0, 1]. *)
let pythagorean_expectancy ~pts_for ~pts_against =
  let pf = float_of_int pts_for in
  let pa = float_of_int pts_against in
  if pf = 0.0 && pa = 0.0 then 0.5
  else
    let pf_exp = pf ** pythagorean_exponent in
    let pa_exp = pa ** pythagorean_exponent in
    pf_exp /. (pf_exp +. pa_exp)

(** Elo expected score.
    Returns probability of home winning [0, 1]. *)
let elo_expected ~home_rating ~away_rating ~home_adv =
  let diff = away_rating -. (home_rating +. home_adv) in
  1.0 /. (1.0 +. (10.0 ** (diff /. 400.0)))

(** Stats-based probability.
    Combines win percentage and efficiency ratio with home advantage. *)
let stats_probability ~win_pct_a ~eff_a ~win_pct_b ~eff_b ~home_advantage =
  let eff_ratio =
    if eff_a = 0.0 && eff_b = 0.0 then 0.5
    else eff_a /. (eff_a +. eff_b)
  in
  let base_prob = (win_pct_a +. eff_ratio) /. 2.0 in
  let opponent_prob = (win_pct_b +. (1.0 -. eff_ratio)) /. 2.0 in
  let relative_strength = base_prob /. (base_prob +. opponent_prob) in
  clamp01 (relative_strength +. home_advantage)

(** Predicted score margin via inverse logistic.
    Scale factor 11.0 calibrated for WKBL. *)
let predict_margin ~prob_a =
  if prob_a <= 0.0 then -20.0
  else if prob_a >= 1.0 then 20.0
  else
    let logit = log (prob_a /. (1.0 -. prob_a)) in
    logit *. 11.0

(** Blend four model probabilities into a final prediction.
    ~elo_prob, ~pyth_prob, ~stats_prob, ~h2h_prob each in [0, 1]. *)
let blend_probabilities ~elo_prob ~pyth_prob ~stats_prob ~h2h_prob =
  clamp01 ((w_elo *. elo_prob) +. (w_pyth *. pyth_prob) +. (w_stats *. stats_prob) +. (w_h2h *. h2h_prob))

(** Margin badge label based on absolute margin. *)
let margin_badge margin =
  let m = abs_float margin in
  if m < 1.5 then "\xEC\xB4\x88\xEC\xA0\x91\xEC\xA0\x84"      (* 초접전 *)
  else if m < 5.5 then "\xEC\xA0\x91\xEC\xA0\x84"               (* 접전 *)
  else if m < 10.5 then "\xEC\x9A\xB0\xEC\x84\xB8"              (* 우세 *)
  else "\xEC\x95\x95\xEC\x8A\xB9"                                (* 압승 *)
