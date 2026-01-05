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

(** Log5 formula (Bill James) to convert win% strengths into head-to-head probability. *)
let log5 ~a ~b =
  let denom = (a *. (1.0 -. b)) +. ((1.0 -. a) *. b) in
  if denom = 0.0 then 0.5 else (a *. (1.0 -. b)) /. denom |> clamp01

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
let stats_probability ~(stats_a: team_prediction_stats) ~(stats_b: team_prediction_stats) ~home_advantage =
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
  clamp01 (relative_strength +. home_advantage)

(** Main prediction function *)
let predict_match ~(team_a: team_prediction_stats) ~(team_b: team_prediction_stats) ~name_a ~name_b =
  let prob_a = stats_probability ~stats_a:team_a ~stats_b:team_b ~home_advantage:home_court_boost in
  let prob_b = 1.0 -. prob_a in
  
  {
    prob_a;
    prob_b;
    winner = if prob_a >= prob_b then name_a else name_b;
  }

(** Elo rating model (basketball-nerd friendly default) *)
module Elo = struct
  module TeamMap = Map.Make (String)

  type ratings = float TeamMap.t

  let default_rating = 1500.0
  let home_advantage = 65.0
  let k_base = 20.0

  let rating_of_team (ratings : ratings) (team : string) =
    TeamMap.find_opt team ratings |> Option.value ~default:default_rating

  let expected ~home_rating ~away_rating ~home_adv =
    let diff = away_rating -. (home_rating +. home_adv) in
    1.0 /. (1.0 +. (10.0 ** (diff /. 400.0)))

  let k_factor ~margin ~elo_diff =
    let margin_f = abs_float (float_of_int margin) in
    let mov = log (margin_f +. 1.0) in
    let scale = 2.2 /. ((0.001 *. abs_float elo_diff) +. 2.2) in
    k_base *. mov *. scale

  let update_ratings (ratings : ratings) ~(home : string) ~(away : string) ~(home_score : int) ~(away_score : int) =
    let home_rating = rating_of_team ratings home in
    let away_rating = rating_of_team ratings away in
    let margin = home_score - away_score in
    let outcome_home = if margin > 0 then 1.0 else if margin < 0 then 0.0 else 0.5 in
    let expected_home = expected ~home_rating ~away_rating ~home_adv:home_advantage in
    let diff = outcome_home -. expected_home in
    let k = k_factor ~margin ~elo_diff:(home_rating -. away_rating) in
    let home_new = home_rating +. (k *. diff) in
    let away_new = away_rating -. (k *. diff) in
    ratings |> TeamMap.add home home_new |> TeamMap.add away away_new

  let ratings_of_games (games : game_summary list) =
    let step (ratings, used) (g : game_summary) =
      match g.home_score, g.away_score with
      | Some hs, Some ascore ->
          (update_ratings ratings ~home:g.home_team ~away:g.away_team ~home_score:hs ~away_score:ascore, used + 1)
      | _ -> (ratings, used)
    in
    List.fold_left step (TeamMap.empty, 0) games

  let win_probability (ratings : ratings) ~(home : string) ~(away : string) ~(is_neutral : bool) =
    let home_rating = rating_of_team ratings home in
    let away_rating = rating_of_team ratings away in
    let home_adv = if is_neutral then 0.0 else home_advantage in
    expected ~home_rating ~away_rating ~home_adv |> clamp01
end

let team_prediction_stats_of_totals ~(win_pct: float) (totals: team_stats) =
  let gp_f = float_of_int totals.gp in
  let gp_div = if gp_f = 0.0 then 1.0 else gp_f in
  let pg v = v /. gp_div in
  {
    ps_pts = pg totals.pts;
    ps_reb = pg totals.reb;
    ps_ast = pg totals.ast;
    ps_stl = pg totals.stl;
    ps_blk = pg totals.blk;
    ps_eff = pg totals.eff;
    ps_win_pct = win_pct;
  }

let predict_match_nerd ~(season: string) ~(is_neutral: bool) ~(games: game_summary list) ~(home: team_stats) ~(away: team_stats) ~(home_win_pct: float) ~(away_win_pct: float) ~name_home ~name_away =
  let ratings, games_used = Elo.ratings_of_games games in
  let elo_home = Elo.rating_of_team ratings name_home in
  let elo_away = Elo.rating_of_team ratings name_away in
  let elo_prob =
    if games_used = 0 then 0.5
    else Elo.win_probability ratings ~home:name_home ~away:name_away ~is_neutral
  in

  let pyth_home = pythagorean_expectancy ~pts_for:(int_of_float home.pts) ~pts_against:(int_of_float home.pts_against) in
  let pyth_away = pythagorean_expectancy ~pts_for:(int_of_float away.pts) ~pts_against:(int_of_float away.pts_against) in
  let pyth_prob = log5 ~a:pyth_home ~b:pyth_away in

  let home_stats = team_prediction_stats_of_totals ~win_pct:home_win_pct home in
  let away_stats = team_prediction_stats_of_totals ~win_pct:away_win_pct away in
  let stats_prob =
    stats_probability
      ~stats_a:home_stats
      ~stats_b:away_stats
      ~home_advantage:(if is_neutral then 0.0 else home_court_boost)
  in

  (* Blend probabilities (Elo-heavy) *)
  let final_prob = clamp01 ((0.6 *. elo_prob) +. (0.25 *. pyth_prob) +. (0.15 *. stats_prob)) in
  let result =
    {
      prob_a = final_prob;
      prob_b = 1.0 -. final_prob;
      winner = if final_prob >= 0.5 then name_home else name_away;
    }
  in
  let breakdown =
    {
      pb_season = season;
      pb_is_neutral = is_neutral;
      pb_games_used = games_used;
      pb_elo_home = elo_home;
      pb_elo_away = elo_away;
      pb_elo_prob = elo_prob;
      pb_pyth_home = pyth_home;
      pb_pyth_away = pyth_away;
      pb_pyth_prob = pyth_prob;
      pb_stats_prob = stats_prob;
      pb_final_prob = final_prob;
    }
  in
  { result; breakdown }
