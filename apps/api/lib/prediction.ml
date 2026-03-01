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

(** Calculate expected score margin based on probabilities
    Using inverse Logistic function approx: margin = -ln(1/p - 1) * scale
    Scale factor ~10.0-12.0 for basketball fits reasonably well for win% -> margin
*)
let predict_margin ~prob_a =
  if prob_a <= 0.0 then -20.0
  else if prob_a >= 1.0 then 20.0
  else
    let logit = log (prob_a /. (1.0 -. prob_a)) in
    logit *. 11.0 (* Scaling factor for WKBL *)

(** Calculate expected total score based on team stats *)
let predict_total_score ~(team_a: team_prediction_stats) ~(team_b: team_prediction_stats) =
  (* Simple model: Average of both teams' scoring + opponent scoring *)
  (* WKBL average is around 130-135 points *)
  let a_off = team_a.ps_pts in
  let b_off = team_b.ps_pts in
  (* No defensive stats in team_prediction_stats, so we rely on offensive averages *)
  (* Adjust for pace interaction: fast + fast = very fast, slow + slow = very slow *)
  (a_off +. b_off)

(** Main prediction function *)
let predict_match ~(team_a: team_prediction_stats) ~(team_b: team_prediction_stats) ~name_a ~name_b =
  let prob_a = stats_probability ~stats_a:team_a ~stats_b:team_b ~home_advantage:home_court_boost in
  let prob_b = 1.0 -. prob_a in
  let margin = predict_margin ~prob_a in
  let total_score = predict_total_score ~team_a ~team_b in
  
  {
    prob_a;
    prob_b;
    winner = if prob_a >= prob_b then name_a else name_b;
    predicted_margin = margin;
    predicted_total_score = total_score;
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

type team_form = {
  tf_games: int;
  tf_win_pct: float;
  tf_avg_margin: float;
  tf_last_date: string option;
}

let compare_game_desc (a : game_summary) (b : game_summary) =
  let by_date = String.compare b.game_date a.game_date in
  if by_date <> 0 then by_date else String.compare b.game_id a.game_id

let rec take n xs =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: tl -> x :: take (n - 1) tl

let parse_ymd (s : string) =
  match String.split_on_char '-' s with
  | [y; m; d] -> (
      match int_of_string_opt y, int_of_string_opt m, int_of_string_opt d with
      | Some yy, Some mm, Some dd -> Some (yy, mm, dd)
      | _ -> None)
  | _ -> None

let time_of_ymd (y, m, d) =
  let tm =
    { Unix.tm_sec = 0;
      tm_min = 0;
      tm_hour = 12;
      tm_mday = d;
      tm_mon = m - 1;
      tm_year = y - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false
    }
  in
  fst (Unix.mktime tm)

let days_between (a : string) (b : string) =
  match parse_ymd a, parse_ymd b with
  | Some da, Some db ->
      let ta = time_of_ymd da in
      let tb = time_of_ymd db in
      Some (int_of_float ((tb -. ta) /. 86400.0))
  | _ -> None

let latest_game_date (games : game_summary list) =
  games
  |> List.fold_left
       (fun acc (g : game_summary) ->
         match acc with
         | None -> Some g.game_date
         | Some d -> if String.compare g.game_date d > 0 then Some g.game_date else Some d)
       None

let team_form ~team ~(games : game_summary list) ~n =
  let key = normalize_label team in
  let relevant =
    games
    |> List.filter (fun (g : game_summary) ->
        normalize_label g.home_team = key || normalize_label g.away_team = key)
    |> List.sort compare_game_desc
    |> take n
  in
  let last_date = match relevant with | g :: _ -> Some g.game_date | [] -> None in
  let wins, ties, margin_sum, count =
    relevant
    |> List.fold_left
         (fun (wins, ties, margin_sum, count) (g : game_summary) ->
           match g.home_score, g.away_score with
           | Some hs, Some ascore ->
               let margin =
                 if normalize_label g.home_team = key then hs - ascore else ascore - hs
               in
               if margin > 0 then (wins + 1, ties, margin_sum + margin, count + 1)
               else if margin < 0 then (wins, ties, margin_sum + margin, count + 1)
               else (wins, ties + 1, margin_sum + margin, count + 1)
           | _ -> (wins, ties, margin_sum, count))
         (0, 0, 0, 0)
  in
  let win_pct =
    if count <= 0 then 0.5
    else ((float_of_int wins) +. (0.5 *. float_of_int ties)) /. float_of_int count
  in
  let avg_margin = if count <= 0 then 0.0 else (float_of_int margin_sum) /. float_of_int count in
  { tf_games = count; tf_win_pct = win_pct; tf_avg_margin = avg_margin; tf_last_date = last_date }

let roster_ratio (r : roster_core_status option) =
  match r with
  | None -> None
  | Some r ->
      if r.rcs_total <= 0 then None
      else Some ((float_of_int r.rcs_present) /. float_of_int r.rcs_total)

let predict_match_nerd ~(context : prediction_context_input option) ~(season: string) ~(is_neutral: bool) ~(games: game_summary list) ~(home: team_stats) ~(away: team_stats) ~(home_win_pct: float) ~(away_win_pct: float) ~name_home ~name_away =
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

  (* H2H Probability *)
  let h2h_prob =
    let h_key = normalize_label name_home in
    let a_key = normalize_label name_away in
    let relevant =
      games
      |> List.filter (fun (g : game_summary) ->
          match g.home_score, g.away_score with
          | Some _, Some _ ->
              let gh = normalize_label g.home_team in
              let ga = normalize_label g.away_team in
              (gh = h_key && ga = a_key) || (gh = a_key && ga = h_key)
          | _ -> false)
    in
    if relevant = [] then 0.5
    else
      let h_wins =
        relevant
        |> List.filter (fun g ->
            let gh = normalize_label g.home_team in
            match g.home_score, g.away_score with
            | Some hs, Some as_ -> if gh = h_key then hs > as_ else as_ > hs
            | _ -> false)
        |> List.length
      in
      float_of_int h_wins /. float_of_int (List.length relevant)
  in

  (* Blend probabilities (Elo-heavy but H2H included) *)
  (* 0.55 Elo + 0.20 Pythag + 0.15 Stats + 0.10 H2H *)
  let base_prob = clamp01 ((0.55 *. elo_prob) +. (0.20 *. pyth_prob) +. (0.15 *. stats_prob) +. (0.10 *. h2h_prob)) in

  let ctx_breakdown, final_prob =
    match context with
    | None -> (None, base_prob)
    | Some (ctx : prediction_context_input) ->
        let form_n = 5 in
        let form_home = team_form ~team:name_home ~games ~n:form_n in
        let form_away = team_form ~team:name_away ~games ~n:form_n in

        let win_factor = 0.04 in
        let margin_factor = 0.02 in
        let margin_norm = 20.0 in
        let form_delta =
          ((form_home.tf_win_pct -. form_away.tf_win_pct) *. win_factor)
          +. (((form_home.tf_avg_margin -. form_away.tf_avg_margin) /. margin_norm) *. margin_factor)
        in

        let roster_factor = 0.06 in
        let roster_delta =
          match roster_ratio ctx.pci_home_roster, roster_ratio ctx.pci_away_roster with
          | Some rh, Some ra -> (rh -. ra) *. roster_factor
          | _ -> 0.0
        in

        let rest_factor = 0.004 in
        let rest_cap = 0.02 in
        let ref_date = latest_game_date games in
        let rest_home_days =
          match form_home.tf_last_date, ref_date with
          | Some d, Some r -> days_between d r
          | _ -> None
        in
        let rest_away_days =
          match form_away.tf_last_date, ref_date with
          | Some d, Some r -> days_between d r
          | _ -> None
        in
        let rest_delta =
          match rest_home_days, rest_away_days with
          | Some dh, Some da ->
              let diff = dh - da in
              let scaled = (float_of_int diff) *. rest_factor in
              if scaled < -.rest_cap then -.rest_cap else if scaled > rest_cap then rest_cap else scaled
          | _ -> 0.0
        in

        let ctx_cap = 0.08 in
        let ctx_delta =
          let raw = form_delta +. roster_delta +. rest_delta in
          if raw < -.ctx_cap then -.ctx_cap else if raw > ctx_cap then ctx_cap else raw
        in
        let final_prob = clamp01 (base_prob +. ctx_delta) in
        ( Some
            { pcb_delta = ctx_delta;
              pcb_form_home = form_home.tf_win_pct;
              pcb_form_away = form_away.tf_win_pct;
              pcb_form_delta = form_delta;
              pcb_roster_home = ctx.pci_home_roster;
              pcb_roster_away = ctx.pci_away_roster;
              pcb_roster_delta = roster_delta;
              pcb_rest_home_days = rest_home_days;
              pcb_rest_away_days = rest_away_days;
              pcb_rest_delta = rest_delta
            },
          final_prob )
  in
  
  (* Calculate margin based on Elo difference and final probability *)
  let elo_margin = (elo_home -. elo_away +. (if is_neutral then 0.0 else Elo.home_advantage)) /. 28.0 in
  let prob_margin = predict_margin ~prob_a:final_prob in
  (* Blend margins: Elo is good for spread, Prob is good for win/loss *)
  let final_margin = (elo_margin +. prob_margin) /. 2.0 in

  (* Calculate total score *)
  (* Average both teams' pace estimates, normalize to league average ~70 possessions *)
  let pace_factor =
    let avg_pace = (home.pace +. away.pace) /. 2.0 in
    if avg_pace > 0.0 then avg_pace /. 70.0 else 1.0
  in
  let total_score = (home.pts +. away.pts) *. pace_factor in

  let result =
    {
      prob_a = final_prob;
      prob_b = 1.0 -. final_prob;
      winner = if final_prob >= 0.5 then name_home else name_away;
      predicted_margin = final_margin;
      predicted_total_score = total_score;
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
      pb_base_prob = base_prob;
      pb_context = ctx_breakdown;
      pb_final_prob = final_prob;
    }
  in
  { result; breakdown }
