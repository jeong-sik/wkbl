(** Hot Streaks Analysis Module
    Calculates and tracks consecutive performance records for players and teams *)

open Domain

(** Minimum number of games required to count as a streak *)
let minimum_streak_length = 2

(** Check if a game qualifies for a specific streak type *)
let game_qualifies_for_streak (game: player_game_stat) streak_type =
  match streak_type with
  | WinStreak ->
      (* For player games, we check if player's team won *)
      (match game.team_score, game.opponent_score with
       | Some ts, Some os -> ts > os
       | _ -> false)
  | Points20Plus -> game.pts >= 20
  | DoubleDouble ->
      (* At least 2 categories with 10+ *)
      let categories = [game.pts; game.reb; game.ast; game.stl; game.blk] in
      let count_10plus = List.filter (fun x -> x >= 10) categories |> List.length in
      count_10plus >= 2
  | TripleDouble ->
      (* At least 3 categories with 10+ *)
      let categories = [game.pts; game.reb; game.ast; game.stl; game.blk] in
      let count_10plus = List.filter (fun x -> x >= 10) categories |> List.length in
      count_10plus >= 3
  | Rebounds10Plus -> game.reb >= 10
  | Assists7Plus -> game.ast >= 7
  | Blocks3Plus -> game.blk >= 3
  | Steals3Plus -> game.stl >= 3

(** Calculate streaks from a list of games sorted by date (newest first) *)
let calculate_streaks_for_player ~player_id ~player_name ~team_name ~streak_type (games: player_game_stat list) : player_streak list =
  (* Games should be sorted by date descending (newest first) *)
  let sorted_games = List.sort (fun (a: player_game_stat) (b: player_game_stat) -> String.compare b.game_date a.game_date) games in

  (* Build streaks by iterating through games *)
  let rec find_streaks acc (current_streak: player_game_stat list) = function
    | [] ->
        (* End of games - finalize any ongoing streak *)
        if List.length current_streak >= minimum_streak_length then
          let games_rev = List.rev current_streak in
          let first_game = List.hd games_rev in
          let last_game = List.hd current_streak in
          let streak = {
            ps_player_id = player_id;
            ps_player_name = player_name;
            ps_team_name = team_name;
            ps_streak_type = streak_type;
            ps_current_count = List.length current_streak;
            ps_is_active = false; (* Will be updated later *)
            ps_start_date = first_game.game_date;
            ps_end_date = Some last_game.game_date;
            ps_games = games_rev;
          } in
          streak :: acc
        else
          acc
    | game :: rest ->
        if game_qualifies_for_streak game streak_type then
          (* Game qualifies, extend current streak *)
          find_streaks acc (game :: current_streak) rest
        else
          (* Streak broken *)
          if List.length current_streak >= minimum_streak_length then
            let games_rev = List.rev current_streak in
            let first_game = List.hd games_rev in
            let last_game = List.hd current_streak in
            let streak = {
              ps_player_id = player_id;
              ps_player_name = player_name;
              ps_team_name = team_name;
              ps_streak_type = streak_type;
              ps_current_count = List.length current_streak;
              ps_is_active = false;
              ps_start_date = first_game.game_date;
              ps_end_date = Some last_game.game_date;
              ps_games = games_rev;
            } in
            find_streaks (streak :: acc) [] rest
          else
            find_streaks acc [] rest
  in

  let streaks = find_streaks [] [] sorted_games in

  (* Mark the most recent streak as active if it includes the most recent game *)
  match sorted_games with
  | latest_game :: _ when game_qualifies_for_streak latest_game streak_type ->
      (* Find the streak that contains the latest game and mark it as active *)
      let contains_latest (s: player_streak) =
        List.exists (fun (g: player_game_stat) -> g.game_id = latest_game.game_id) s.ps_games
      in
      List.map (fun s ->
        if contains_latest s then { s with ps_is_active = true; ps_end_date = None }
        else s
      ) streaks
  | _ -> streaks

(** Get all streak types for analysis *)
let all_player_streak_types = [
  Points20Plus;
  DoubleDouble;
  TripleDouble;
  Rebounds10Plus;
  Assists7Plus;
  Blocks3Plus;
  Steals3Plus;
]

(** Analyze all streaks for a player *)
let analyze_player_streaks ~player_id ~player_name ~team_name (games: player_game_stat list) : player_streak list =
  all_player_streak_types
  |> List.map (fun streak_type ->
      calculate_streaks_for_player ~player_id ~player_name ~team_name ~streak_type games)
  |> List.concat
  |> List.sort (fun a b ->
      (* Sort by count descending, then by date descending *)
      let count_cmp = compare b.ps_current_count a.ps_current_count in
      if count_cmp <> 0 then count_cmp
      else String.compare b.ps_start_date a.ps_start_date)

(** Get the best (longest) streak of each type *)
let get_best_streaks (streaks: player_streak list) : player_streak list =
  let rec add_if_better acc streak =
    match acc with
    | [] -> [streak]
    | existing :: rest ->
        if existing.ps_streak_type = streak.ps_streak_type then
          if streak.ps_current_count > existing.ps_current_count then
            streak :: rest
          else
            acc
        else
          existing :: add_if_better rest streak
  in
  List.fold_left add_if_better [] streaks

(** Get currently active streaks *)
let get_active_streaks (streaks: player_streak list) : player_streak list =
  streaks
  |> List.filter (fun s -> s.ps_is_active)
  |> List.sort (fun a b -> compare b.ps_current_count a.ps_current_count)

(** Calculate team win streaks from game results *)
let calculate_team_win_streaks ~team_name (results: team_game_result list) : team_streak list =
  (* Sort by date descending *)
  let sorted = List.sort (fun a b -> String.compare b.tgr_game_date a.tgr_game_date) results in

  let rec find_streaks acc current_streak = function
    | [] ->
        if List.length current_streak >= minimum_streak_length then
          let games_rev = List.rev current_streak in
          let first = List.hd games_rev in
          let last = List.hd current_streak in
          let streak = {
            ts_team_name = team_name;
            ts_streak_type = WinStreak;
            ts_current_count = List.length current_streak;
            ts_is_active = false;
            ts_start_date = first.tgr_game_date;
            ts_end_date = Some last.tgr_game_date;
            ts_game_ids = List.map (fun g -> g.tgr_game_id) games_rev;
          } in
          streak :: acc
        else
          acc
    | game :: rest ->
        if game.tgr_is_win then
          find_streaks acc (game :: current_streak) rest
        else
          if List.length current_streak >= minimum_streak_length then
            let games_rev = List.rev current_streak in
            let first = List.hd games_rev in
            let last = List.hd current_streak in
            let streak = {
              ts_team_name = team_name;
              ts_streak_type = WinStreak;
              ts_current_count = List.length current_streak;
              ts_is_active = false;
              ts_start_date = first.tgr_game_date;
              ts_end_date = Some last.tgr_game_date;
              ts_game_ids = List.map (fun g -> g.tgr_game_id) games_rev;
            } in
            find_streaks (streak :: acc) [] rest
          else
            find_streaks acc [] rest
  in

  let streaks = find_streaks [] [] sorted in

  (* Mark the most recent streak as active if applicable *)
  match sorted, streaks with
  | latest :: _, streak :: rest when latest.tgr_is_win ->
      let is_active = List.mem latest.tgr_game_id streak.ts_game_ids in
      if is_active then
        { streak with ts_is_active = true; ts_end_date = None } :: rest
      else
        streaks
  | _ -> streaks

(** Convert player streak to streak record *)
let player_streak_to_record ~season (streak: player_streak) : streak_record =
  {
    sr_holder_name = streak.ps_player_name;
    sr_holder_id = Some streak.ps_player_id;
    sr_team_name = Some streak.ps_team_name;
    sr_streak_type = streak.ps_streak_type;
    sr_count = streak.ps_current_count;
    sr_start_date = streak.ps_start_date;
    sr_end_date = Option.value ~default:streak.ps_start_date streak.ps_end_date;
    sr_season = season;
  }

(** Convert team streak to streak record *)
let team_streak_to_record ~season (streak: team_streak) : streak_record =
  {
    sr_holder_name = streak.ts_team_name;
    sr_holder_id = None;
    sr_team_name = Some streak.ts_team_name;
    sr_streak_type = streak.ts_streak_type;
    sr_count = streak.ts_current_count;
    sr_start_date = streak.ts_start_date;
    sr_end_date = Option.value ~default:streak.ts_start_date streak.ts_end_date;
    sr_season = season;
  }
