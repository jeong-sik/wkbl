(** Split aggregate types — Home/Away, per-opponent, per-month breakdowns.
    Depends on Domain_core for player_game_stat. *)

open Domain_core

(** Aggregated split — Home/Away, per-opponent, per-month *)
type split_aggregate = {
  sa_label: string;
  sa_games: int;
  sa_wins: int;
  sa_losses: int;
  sa_min: float;
  sa_pts: int;
  sa_reb: int;
  sa_ast: int;
  sa_stl: int;
  sa_blk: int;
  sa_tov: int;
  sa_fg_made: int;
  sa_fg_att: int;
  sa_fg3_made: int;
  sa_fg3_att: int;
  sa_ft_made: int;
  sa_ft_att: int;
}

let empty_split label = {
  sa_label = label; sa_games = 0; sa_wins = 0; sa_losses = 0;
  sa_min = 0.0; sa_pts = 0; sa_reb = 0; sa_ast = 0;
  sa_stl = 0; sa_blk = 0; sa_tov = 0;
  sa_fg_made = 0; sa_fg_att = 0; sa_fg3_made = 0; sa_fg3_att = 0;
  sa_ft_made = 0; sa_ft_att = 0;
}

let add_game_to_split (s: split_aggregate) (g: player_game_stat) =
  let is_win = match g.team_score, g.opponent_score with
    | Some ts, Some os -> ts > os
    | _ -> false
  in
  let is_loss = match g.team_score, g.opponent_score with
    | Some ts, Some os -> ts < os
    | _ -> false
  in
  { s with
    sa_games = s.sa_games + 1;
    sa_wins = s.sa_wins + (if is_win then 1 else 0);
    sa_losses = s.sa_losses + (if is_loss then 1 else 0);
    sa_min = s.sa_min +. g.min;
    sa_pts = s.sa_pts + g.pts;
    sa_reb = s.sa_reb + g.reb;
    sa_ast = s.sa_ast + g.ast;
    sa_stl = s.sa_stl + g.stl;
    sa_blk = s.sa_blk + g.blk;
    sa_tov = s.sa_tov + g.tov;
    sa_fg_made = s.sa_fg_made + g.fg_made;
    sa_fg_att = s.sa_fg_att + g.fg_att;
    sa_fg3_made = s.sa_fg3_made + g.fg3_made;
    sa_fg3_att = s.sa_fg3_att + g.fg3_att;
    sa_ft_made = s.sa_ft_made + g.ft_made;
    sa_ft_att = s.sa_ft_att + g.ft_att;
  }

(** Compute splits from game logs.
    Returns (home_away, per_opponent, per_month) triplet. *)
let compute_splits (games: player_game_stat list) =
  (* Home/Away *)
  let home = List.fold_left add_game_to_split (empty_split "홈") (List.filter (fun g -> g.is_home) games) in
  let away = List.fold_left add_game_to_split (empty_split "원정") (List.filter (fun g -> not g.is_home) games) in
  let total = List.fold_left add_game_to_split (empty_split "전체") games in
  let home_away = [home; away; total] in
  (* Per opponent *)
  let opp_tbl = Hashtbl.create 8 in
  List.iter (fun (g: player_game_stat) ->
    let key = g.opponent in
    let cur = match Hashtbl.find_opt opp_tbl key with
      | Some s -> s
      | None -> empty_split key
    in
    Hashtbl.replace opp_tbl key (add_game_to_split cur g)
  ) games;
  let per_opponent =
    Hashtbl.fold (fun _ v acc -> v :: acc) opp_tbl []
    |> List.sort (fun a b -> compare b.sa_games a.sa_games)
  in
  (* Per month *)
  let month_tbl = Hashtbl.create 12 in
  List.iter (fun (g: player_game_stat) ->
    let key = if String.length g.game_date >= 7 then String.sub g.game_date 0 7 else g.game_date in
    let cur = match Hashtbl.find_opt month_tbl key with
      | Some s -> s
      | None -> empty_split key
    in
    Hashtbl.replace month_tbl key (add_game_to_split cur g)
  ) games;
  let per_month =
    Hashtbl.fold (fun _ v acc -> v :: acc) month_tbl []
    |> List.sort (fun a b -> String.compare a.sa_label b.sa_label)
  in
  (home_away, per_opponent, per_month)
