(** Prediction Module - ELO Rating System and Statistical Models

    Implements:
    1. ELO Rating System with K-factor adjustment
    2. Simple Stats-Based Model (recent form, home/away, H2H)
    3. Combined weighted prediction

    ELO Formula: P(A) = 1 / (1 + 10^((RB-RA)/400))
*)

open Domain

(** ELO System Constants *)
module Elo_config = struct
  let initial_rating = 1500.0
  let k_factor = 32.0           (* Standard chess K-factor *)
  let home_advantage = 50.0     (* ELO points for home court *)
end

(** In-memory ELO ratings table *)
module Elo_table = struct
  let ratings : (string, float) Hashtbl.t = Hashtbl.create 10

  let get team_name =
    try Hashtbl.find ratings team_name
    with Not_found -> Elo_config.initial_rating

  let set team_name rating =
    Hashtbl.replace ratings team_name rating

  let reset () =
    Hashtbl.clear ratings

  let all () =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) ratings []
    |> List.sort (fun (_, a) (_, b) -> compare b a)
end

(** Calculate expected score using ELO formula *)
let expected_score rating_a rating_b =
  1.0 /. (1.0 +. (10.0 ** ((rating_b -. rating_a) /. 400.0)))

(** Update ELO ratings after a game *)
let update_elo ~winner_rating ~loser_rating ~k =
  let expected_winner = expected_score winner_rating loser_rating in
  let expected_loser = 1.0 -. expected_winner in
  let new_winner = winner_rating +. k *. (1.0 -. expected_winner) in
  let new_loser = loser_rating +. k *. (0.0 -. expected_loser) in
  (new_winner, new_loser)

(** Process a single game and update ELO table *)
let process_game ~home_team ~away_team ~home_score ~away_score =
  let home_elo = Elo_table.get home_team in
  let away_elo = Elo_table.get away_team in
  (* Apply home advantage *)
  let adj_home_elo = home_elo +. Elo_config.home_advantage in
  if home_score > away_score then begin
    (* Home team won *)
    let new_home, new_away = update_elo
      ~winner_rating:adj_home_elo
      ~loser_rating:away_elo
      ~k:Elo_config.k_factor in
    Elo_table.set home_team (new_home -. Elo_config.home_advantage);
    Elo_table.set away_team new_away
  end else begin
    (* Away team won *)
    let new_away, new_home = update_elo
      ~winner_rating:away_elo
      ~loser_rating:adj_home_elo
      ~k:Elo_config.k_factor in
    Elo_table.set home_team (new_home -. Elo_config.home_advantage);
    Elo_table.set away_team new_away
  end

(** Initialize ELO from historical games *)
let initialize_elo games =
  Elo_table.reset ();
  List.iter (fun (home, (away, (_date, (home_score, away_score)))) ->
    process_game ~home_team:home ~away_team:away ~home_score ~away_score
  ) games

(** Get ELO-based win probability *)
let elo_probability ~team_a ~team_b ~is_home_a =
  let rating_a = Elo_table.get team_a in
  let rating_b = Elo_table.get team_b in
  let adj_rating_a = if is_home_a then rating_a +. Elo_config.home_advantage else rating_a in
  let adj_rating_b = if not is_home_a then rating_b +. Elo_config.home_advantage else rating_b in
  expected_score adj_rating_a adj_rating_b

(** Stats-based win probability *)
let stats_probability ~(stats_a: team_stats) ~(stats_b: team_stats) ~is_home_a =
  let factors = [
    (* Win percentage *)
    (let wp_a = Float.of_int stats_a.ts_wins /. Float.of_int (stats_a.ts_wins + stats_a.ts_losses) in
     let wp_b = Float.of_int stats_b.ts_wins /. Float.of_int (stats_b.ts_wins + stats_b.ts_losses) in
     (wp_a -. wp_b +. 1.0) /. 2.0, 0.25);

    (* Point differential *)
    (let diff_a = stats_a.ts_avg_pts_scored -. stats_a.ts_avg_pts_allowed in
     let diff_b = stats_b.ts_avg_pts_scored -. stats_b.ts_avg_pts_allowed in
     let norm = ((diff_a -. diff_b) /. 20.0 +. 1.0) /. 2.0 in
     Float.max 0.1 (Float.min 0.9 norm), 0.25);

    (* Recent form *)
    ((stats_a.ts_recent_form -. stats_b.ts_recent_form +. 1.0) /. 2.0, 0.20);

    (* Home/Away record *)
    ((if is_home_a then
       let home_wp_a = Float.of_int stats_a.ts_home_wins /.
         Float.of_int (stats_a.ts_home_wins + stats_a.ts_home_losses) in
       let away_wp_b = Float.of_int stats_b.ts_away_wins /.
         Float.of_int (stats_b.ts_away_wins + stats_b.ts_away_losses) in
       (home_wp_a -. away_wp_b +. 1.0) /. 2.0
     else
       let away_wp_a = Float.of_int stats_a.ts_away_wins /.
         Float.of_int (stats_a.ts_away_wins + stats_a.ts_away_losses) in
       let home_wp_b = Float.of_int stats_b.ts_home_wins /.
         Float.of_int (stats_b.ts_home_wins + stats_b.ts_home_losses) in
       (away_wp_a -. home_wp_b +. 1.0) /. 2.0), 0.15);

    (* Current streak *)
    (let streak_factor =
       (Float.of_int stats_a.ts_streak -. Float.of_int stats_b.ts_streak +. 10.0) /. 20.0 in
     Float.max 0.2 (Float.min 0.8 streak_factor), 0.15);
  ] in
  (* Weighted average *)
  let weighted_sum = List.fold_left (fun acc (prob, weight) -> acc +. prob *. weight) 0.0 factors in
  Float.max 0.05 (Float.min 0.95 weighted_sum)

(** Generate prediction reasons based on stats *)
let generate_reasons ~(stats_a: team_stats) ~(stats_b: team_stats) ~(h2h: team_h2h option) ~is_home_a =
  let reasons = ref [] in

  (* Win record comparison *)
  let wp_a = Float.of_int stats_a.ts_wins /. Float.of_int (stats_a.ts_wins + stats_a.ts_losses) in
  let wp_b = Float.of_int stats_b.ts_wins /. Float.of_int (stats_b.ts_wins + stats_b.ts_losses) in
  reasons := (Printf.sprintf "Season record: %s (%.0f%%) vs %s (%.0f%%)"
    stats_a.ts_team_name (wp_a *. 100.0)
    stats_b.ts_team_name (wp_b *. 100.0)) :: !reasons;

  (* Point differential *)
  let diff_a = stats_a.ts_avg_pts_scored -. stats_a.ts_avg_pts_allowed in
  let diff_b = stats_b.ts_avg_pts_scored -. stats_b.ts_avg_pts_allowed in
  reasons := (Printf.sprintf "Point diff: %s (%+.1f) vs %s (%+.1f)"
    stats_a.ts_team_name diff_a
    stats_b.ts_team_name diff_b) :: !reasons;

  (* Recent form *)
  reasons := (Printf.sprintf "Recent form (last 5): %s (%.0f%%) vs %s (%.0f%%)"
    stats_a.ts_team_name (stats_a.ts_recent_form *. 100.0)
    stats_b.ts_team_name (stats_b.ts_recent_form *. 100.0)) :: !reasons;

  (* Current streak *)
  let streak_str s = if s.ts_streak > 0 then Printf.sprintf "W%d" s.ts_streak
                     else if s.ts_streak < 0 then Printf.sprintf "L%d" (abs s.ts_streak)
                     else "Even" in
  reasons := (Printf.sprintf "Streak: %s (%s), %s (%s)"
    stats_a.ts_team_name (streak_str stats_a)
    stats_b.ts_team_name (streak_str stats_b)) :: !reasons;

  (* Home advantage *)
  if is_home_a then
    reasons := (Printf.sprintf "%s playing at home (home record: %d-%d)"
      stats_a.ts_team_name stats_a.ts_home_wins stats_a.ts_home_losses) :: !reasons
  else
    reasons := (Printf.sprintf "%s playing at home (home record: %d-%d)"
      stats_b.ts_team_name stats_b.ts_home_wins stats_b.ts_home_losses) :: !reasons;

  (* Head-to-head *)
  (match h2h with
   | Some h when h.th_team_a_wins + h.th_team_b_wins > 0 ->
       reasons := (Printf.sprintf "H2H record: %s %d - %d %s"
         h.th_team_a h.th_team_a_wins h.th_team_b_wins h.th_team_b) :: !reasons
   | _ -> ());

  List.rev !reasons

(** Create full prediction result *)
let make_prediction ~team_a ~team_b ~stats_a ~stats_b ~h2h ~is_home_a =
  let elo_prob = elo_probability ~team_a ~team_b ~is_home_a in
  let stats_prob = stats_probability ~stats_a ~stats_b ~is_home_a in

  (* H2H adjustment *)
  let h2h_adj = match h2h with
    | Some h when h.th_team_a_wins + h.th_team_b_wins >= 3 ->
        let total = Float.of_int (h.th_team_a_wins + h.th_team_b_wins) in
        let h2h_rate = Float.of_int h.th_team_a_wins /. total in
        (h2h_rate -. 0.5) *. 0.1  (* Max +/- 5% adjustment *)
    | _ -> 0.0
  in

  (* Combined probability: 40% ELO, 50% Stats, 10% H2H *)
  let combined = (elo_prob *. 0.4) +. (stats_prob *. 0.5) +. ((stats_prob +. h2h_adj) *. 0.1) in
  let combined = Float.max 0.05 (Float.min 0.95 combined) in

  let reasons = generate_reasons ~stats_a ~stats_b ~h2h ~is_home_a in

  {
    pr_team_a = team_a;
    pr_team_b = team_b;
    pr_elo_prob_a = elo_prob;
    pr_stats_prob_a = stats_prob;
    pr_combined_prob_a = combined;
    pr_team_a_elo = Elo_table.get team_a;
    pr_team_b_elo = Elo_table.get team_b;
    pr_team_a_stats = stats_a;
    pr_team_b_stats = stats_b;
    pr_h2h = h2h;
    pr_home_advantage = is_home_a;
    pr_reasons = reasons;
  }

(** Get all current ELO ratings *)
let get_all_elo_ratings () =
  Elo_table.all ()
  |> List.map (fun (team, rating) ->
    { elo_team_name = team;
      elo_rating = rating;
      elo_games_played = 0;  (* Would need to track separately *)
      elo_last_updated = "";
    })
