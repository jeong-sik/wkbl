(** Domain Models - shared types across the application *)

type player_aggregate = {
  player_id: string;
  name: string;
  team_name: string;
  games_played: int;
  total_minutes: float;
  total_points: int;
  total_rebounds: int;
  total_assists: int;
  total_steals: int;
  total_blocks: int;
  total_turnovers: int;
  avg_points: float;
  avg_margin: float;
  avg_rebounds: float;
  avg_assists: float;
  avg_steals: float;
  avg_blocks: float;
  avg_turnovers: float;
  efficiency: float;
}

type player_sort =
  | ByPoints
  | ByMargin
  | ByRebounds
  | ByAssists
  | ByEfficiency
  | ByMinutes

let player_sort_of_string = function
  | "pts" | "points" -> ByPoints
  | "mg" | "margin" -> ByMargin
  | "reb" | "rebounds" -> ByRebounds
  | "ast" | "assists" -> ByAssists
  | "min" | "minutes" -> ByMinutes
  | _ -> ByEfficiency

type season_info = {
  code: string;
  name: string;
}

type team_info = {
  team_code: string;
  team_name: string;
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

(** Schedule entry for upcoming/scheduled games *)
type schedule_entry = {
  sch_id: int;
  sch_game_date: string;
  sch_game_time: string option;
  sch_season_code: string;
  sch_home_team_code: string;
  sch_away_team_code: string;
  sch_home_team_name: string option;
  sch_away_team_name: string option;
  sch_venue: string option;
  sch_status: string;
}

type boxscore_player_stat = {
  bs_player_id: string;
  bs_player_name: string;
  bs_position: string option;
  bs_team_code: string;
  bs_team_name: string;
  bs_minutes: float;
  bs_pts: int;
  bs_plus_minus: int option;
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

type game_score_quality =
  | Verified
  | Derived
  | Mismatch

let game_score_quality_of_int = function
  | 2 -> Verified
  | 0 -> Mismatch
  | _ -> Derived

type game_info = {
  gi_game_id: string;
  gi_game_date: string;
  gi_home_team_code: string;
  gi_home_team_name: string;
  gi_away_team_code: string;
  gi_away_team_name: string;
  gi_home_score: int;
  gi_away_score: int;
  gi_score_quality: game_score_quality;
}

type game_boxscore = {
  boxscore_game: game_info;
  boxscore_home_players: boxscore_player_stat list;
  boxscore_away_players: boxscore_player_stat list;
}

type pbp_event = {
  pe_period_code: string;
  pe_event_index: int;
  pe_team_side: int;
  pe_description: string;
  pe_team1_score: int option;
  pe_team2_score: int option;
  pe_clock: string;
}

(** Score flow point for game flow chart visualization *)
type score_flow_point = {
  sfp_clock: string;       (** Time remaining in period, e.g. "09:30" *)
  sfp_period: string;      (** Period code, e.g. "Q1", "Q2", "X1" *)
  sfp_home_score: int;     (** Home team cumulative score *)
  sfp_away_score: int;     (** Away team cumulative score *)
  sfp_diff: int;           (** Score difference: home - away *)
  sfp_elapsed_seconds: int; (** Total elapsed seconds from game start *)
}

(** Convert period code to period number (1-4 for Q1-Q4, 5+ for OT) *)
let period_to_number = function
  | "Q1" -> 1 | "Q2" -> 2 | "Q3" -> 3 | "Q4" -> 4
  | "X1" -> 5 | "X2" -> 6 | "X3" -> 7 | "X4" -> 8
  | _ -> 0

(** Parse clock string "MM:SS" to seconds remaining in period *)
let parse_clock_to_seconds clock =
  try
    let parts = String.split_on_char ':' clock in
    match parts with
    | [min_str; sec_str] ->
        let mins = int_of_string (String.trim min_str) in
        let secs = int_of_string (String.trim sec_str) in
        mins * 60 + secs
    | _ -> 600 (* Default to 10 minutes if parse fails *)
  with _ -> 600

(** Calculate total elapsed seconds from game start *)
let calculate_elapsed_seconds ~period_code ~clock =
  let period_num = period_to_number period_code in
  let period_length = if period_num <= 4 then 600 else 300 in (* 10 min quarters, 5 min OT *)
  let seconds_remaining = parse_clock_to_seconds clock in
  let completed_periods =
    if period_num <= 4 then (period_num - 1) * 600
    else (4 * 600) + ((period_num - 5) * 300)
  in
  completed_periods + (period_length - seconds_remaining)

(** Extract score flow points from PBP events for chart visualization.
    Returns a list of points where scores changed, sorted by elapsed time. *)
let extract_score_flow (events: pbp_event list) : score_flow_point list =
  let score_changes =
    events
    |> List.filter_map (fun e ->
        match (e.pe_team1_score, e.pe_team2_score) with
        | (Some home, Some away) ->
            let elapsed = calculate_elapsed_seconds ~period_code:e.pe_period_code ~clock:e.pe_clock in
            Some {
              sfp_clock = e.pe_clock;
              sfp_period = e.pe_period_code;
              sfp_home_score = home;
              sfp_away_score = away;
              sfp_diff = home - away;
              sfp_elapsed_seconds = elapsed;
            }
        | _ -> None)
  in
  (* Sort by elapsed time and deduplicate consecutive same scores *)
  let sorted = List.sort (fun a b -> compare a.sfp_elapsed_seconds b.sfp_elapsed_seconds) score_changes in
  let rec dedup acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: (y :: _ as rest) ->
        if x.sfp_home_score = y.sfp_home_score && x.sfp_away_score = y.sfp_away_score
        then dedup acc rest
        else dedup (x :: acc) rest
  in
  (* Add starting point at 0-0 if not present *)
  let with_start =
    match sorted with
    | [] -> []
    | first :: _ as pts ->
        if first.sfp_elapsed_seconds > 0 then
          { sfp_clock = "10:00"; sfp_period = "Q1"; sfp_home_score = 0; sfp_away_score = 0;
            sfp_diff = 0; sfp_elapsed_seconds = 0 } :: pts
        else pts
  in
  dedup [] with_start

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
  score_quality: game_score_quality;
  min: float;
  pts: int;
  reb: int;
  ast: int;
  stl: int;
  blk: int;
  tov: int;
  plus_minus: int option;
}

type career_high_item = {
  chi_label: string;
  chi_value: int;
  chi_game_id: string;
  chi_game_date: string;
  chi_opponent: string;
  chi_is_home: bool;
}

type player_team_stint = {
  pts_team_name: string;
  pts_start_date: string;
  pts_end_date: string;
  pts_games_played: int;
}

type player_draft = {
  pd_player_id: string;
  pd_draft_year: int option;
  pd_draft_round: int option;
  pd_pick_in_round: int option;
  pd_overall_pick: int option;
  pd_draft_team: string option;
  pd_raw_text: string;
  pd_source_url: string;
  pd_scraped_at: string;
}

type draft_pick_row = {
  dpr_player_id: string;
  dpr_player_name: string;
  dpr_draft_year: int option;
  dpr_draft_round: int option;
  dpr_pick_in_round: int option;
  dpr_overall_pick: int option;
  dpr_draft_team: string option;
  dpr_raw_text: string;
  dpr_source_url: string;
  dpr_scraped_at: string;
}

type official_trade_event = {
  ote_event_date: string;
  ote_event_year: int;
  ote_event_text: string;
  ote_source_url: string;
  ote_scraped_at: string;
}

type player_external_link = {
  pel_player_id: string;
  pel_link_type: string;
  pel_url: string;
  pel_source_url: string option;
  pel_scraped_at: string;
}

type player_profile = {
  player: player_info;
  averages: player_aggregate;
  recent_games: player_game_stat list;
  all_star_games: player_game_stat list;
  draft: player_draft option;
  official_trade_events: official_trade_event list;
  external_links: player_external_link list;
  team_stints: player_team_stint list;
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
  tfd_game_results: team_game_result list;
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

(** H2H Advanced Summary - computed from h2h_game list *)
type h2h_summary = {
  h2h_total_games: int;
  h2h_p1_wins: int;
  h2h_p2_wins: int;
  h2h_p1_avg_pts: float;
  h2h_p1_avg_reb: float;
  h2h_p1_avg_ast: float;
  h2h_p2_avg_pts: float;
  h2h_p2_avg_reb: float;
  h2h_p2_avg_ast: float;
}

(** Calculate H2H summary from a list of h2h_games *)
let calculate_h2h_summary ~p1_team (games: h2h_game list) : h2h_summary =
  let total = List.length games in
  if total = 0 then
    { h2h_total_games = 0;
      h2h_p1_wins = 0; h2h_p2_wins = 0;
      h2h_p1_avg_pts = 0.0; h2h_p1_avg_reb = 0.0; h2h_p1_avg_ast = 0.0;
      h2h_p2_avg_pts = 0.0; h2h_p2_avg_reb = 0.0; h2h_p2_avg_ast = 0.0 }
  else
    let p1_wins = List.length (List.filter (fun g -> g.player1_team = g.winner_team) games) in
    let p2_wins = total - p1_wins in
    let sum_p1_pts = List.fold_left (fun acc g -> acc + g.player1_pts) 0 games in
    let sum_p1_reb = List.fold_left (fun acc g -> acc + g.player1_reb) 0 games in
    let sum_p1_ast = List.fold_left (fun acc g -> acc + g.player1_ast) 0 games in
    let sum_p2_pts = List.fold_left (fun acc g -> acc + g.player2_pts) 0 games in
    let sum_p2_reb = List.fold_left (fun acc g -> acc + g.player2_reb) 0 games in
    let sum_p2_ast = List.fold_left (fun acc g -> acc + g.player2_ast) 0 games in
    let n = float_of_int total in
    let _ = p1_team in (* suppress unused warning *)
    { h2h_total_games = total;
      h2h_p1_wins = p1_wins;
      h2h_p2_wins = p2_wins;
      h2h_p1_avg_pts = float_of_int sum_p1_pts /. n;
      h2h_p1_avg_reb = float_of_int sum_p1_reb /. n;
      h2h_p1_avg_ast = float_of_int sum_p1_ast /. n;
      h2h_p2_avg_pts = float_of_int sum_p2_pts /. n;
      h2h_p2_avg_reb = float_of_int sum_p2_reb /. n;
      h2h_p2_avg_ast = float_of_int sum_p2_ast /. n }

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

(** Prediction context inputs/breakdown (optional, best-effort). *)
type roster_core_status = {
  rcs_present: int;
  rcs_total: int;
}

type prediction_context_input = {
  pci_home_roster: roster_core_status option;
  pci_away_roster: roster_core_status option;
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

(** Historical season data - champions and MVPs *)
type historical_season = {
  hs_season_id: string;
  hs_season_name: string;
  hs_champion_team: string option;
  hs_runner_up: string option;
  hs_regular_mvp: string option;
  hs_finals_mvp: string option;
  hs_rookie_of_year: string option;
  hs_scoring_leader: string option;
  hs_notes: string option;
}

(** Legend player data *)
type legend_player = {
  lp_player_name: string;
  lp_career_years: string option;
  lp_teams: string option;
  lp_championships: int;
  lp_mvp_count: int;
  lp_all_star_count: int;
  lp_career_points: int;
  lp_career_rebounds: int;
  lp_career_assists: int;
  lp_notable_achievements: string option;
  lp_is_hall_of_fame: bool;
}

(** Coach data *)
type coach = {
  c_coach_name: string;
  c_team: string option;
  c_tenure_start: int option;
  c_tenure_end: int option;
  c_championships: int;
  c_regular_season_wins: int;
  c_playoff_wins: int;
  c_former_player: bool;
  c_player_career_years: string option;
  c_notable_achievements: string option;
}

(** Player career history entry - year by year *)
type player_career_entry = {
  pce_player_name: string;
  pce_season_id: string;
  pce_team: string;
  pce_jersey_number: int option;
  pce_games_played: int option;
  pce_points_per_game: float option;
  pce_rebounds_per_game: float option;
  pce_assists_per_game: float option;
  pce_is_allstar: bool;
  pce_awards: string option;
}

(** Helper to map team names to codes *)
let normalize_label (s: string) =
  let len = String.length s in
  let is_ascii_space = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  in
  let buf = Buffer.create len in
  let add_space prev_space =
    if prev_space then () else Buffer.add_char buf ' '
  in
  let rec loop i prev_space =
    if i >= len then ()
    else
      let c0 = s.[i] in
      if c0 = '"' || c0 = '\\' then (
        add_space prev_space;
        loop (i + 1) true
      ) else if is_ascii_space c0 then (
        add_space prev_space;
        loop (i + 1) true
      ) else if c0 = '\xC2' && i + 1 < len && s.[i + 1] = '\xA0' then (
        (* NBSP in UTF-8 is 0xC2 0xA0. Avoid corrupting multi-byte UTF-8 chars. *)
        add_space prev_space;
        loop (i + 2) true
      ) else if c0 = '\xEF' && i + 2 < len && s.[i + 1] = '\xBB' && s.[i + 2] = '\xBF' then (
        (* UTF-8 BOM (zero width no-break space): 0xEF 0xBB 0xBF *)
        add_space prev_space;
        loop (i + 3) true
      ) else if c0 = '\xE2' && i + 2 < len && s.[i + 1] = '\x80' &&
                (s.[i + 2] = '\x8A' (* HAIR SPACE *)
                 || s.[i + 2] = '\x8B' (* ZWSP *)
                 || s.[i + 2] = '\x8C' (* ZWNJ *)
                 || s.[i + 2] = '\x8D' (* ZWJ *)
                 || s.[i + 2] = '\x8E' (* LRM *)
                 || s.[i + 2] = '\x8F' (* RLM *)
                 || s.[i + 2] = '\x89' (* THIN SPACE *)
                 || s.[i + 2] = '\xAF' (* NARROW NBSP *)) then (
        add_space prev_space;
        loop (i + 3) true
      ) else if c0 = '\xE2' && i + 2 < len && s.[i + 1] = '\x81' && s.[i + 2] = '\xA0' then (
        (* WORD JOINER (U+2060): 0xE2 0x81 0xA0 *)
        add_space prev_space;
        loop (i + 3) true
      ) else if c0 = '\xE3' && i + 2 < len && s.[i + 1] = '\x80' && s.[i + 2] = '\x80' then (
        (* IDEOGRAPHIC SPACE (U+3000): 0xE3 0x80 0x80 *)
        add_space prev_space;
        loop (i + 3) true
      ) else (
        Buffer.add_char buf c0;
        loop (i + 1) false
      )
  in
  loop 0 true;
  Buffer.contents buf |> String.trim

let team_code_of_string team_name =
  let key = team_name |> normalize_label |> String.uppercase_ascii in
  let contains (needle : string) =
    let nlen = String.length needle in
    let hlen = String.length key in
    if nlen = 0 then false
    else
      let rec loop i =
        if i + nlen > hlen then false
        else if String.sub key i nlen = needle then true
        else loop (i + 1)
      in
      loop 0
  in
  match key with
  | "아산 우리은행 우리WON" | "우리은행" | "우리WON" | "WO" -> Some "WO"
  | "용인 삼성생명 블루밍스" | "삼성생명" | "SS" -> Some "SS"
  | "인천 신한은행 에스버드" | "신한은행" | "SH" -> Some "SH"
  | "청주 KB스타즈" | "KB스타즈" | "KB" -> Some "KB"
  | "부천 하나은행" | "하나은행" | "HN" -> Some "HN"
  | "부산 BNK 썸" | "BNK 썸" | "BNK썸" | "BNK" | "BN" -> Some "BN"
  | _ when contains "우리은행" -> Some "WO"
  | _ when contains "삼성생명" -> Some "SS"
  | _ when contains "신한은행" -> Some "SH"
  | _ when contains "하나은행" -> Some "HN"
  | _ when contains "BNK" -> Some "BN"
  | _ when contains "KB" -> Some "KB"
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

let team_code_to_name_en = function
  | "WO" -> "Woori Bank Wibee"
  | "SS" -> "Samsung Life Blueminx"
  | "SH" -> "Shinhan Bank S-Birds"
  | "KB" -> "KB Stars"
  | "HN" -> "Hana Bank"
  | "BN" -> "BNK Sum"
  | _ -> "Unknown"

let team_code_to_city_en = function
  | "WO" -> "Asan"
  | "SS" -> "Yongin"
  | "SH" -> "Incheon"
  | "KB" -> "Cheongju"
  | "HN" -> "Bucheon"
  | "BN" -> "Busan"
  | _ -> ""

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

(** Hot Streaks Types *)
type streak_type =
  | WinStreak           (** Team consecutive wins *)
  | Points20Plus        (** 20+ points in consecutive games *)
  | DoubleDouble        (** Double-double in consecutive games *)
  | TripleDouble        (** Triple-double in consecutive games *)
  | Rebounds10Plus      (** 10+ rebounds in consecutive games *)
  | Assists7Plus        (** 7+ assists in consecutive games *)
  | Blocks3Plus         (** 3+ blocks in consecutive games *)
  | Steals3Plus         (** 3+ steals in consecutive games *)

let streak_type_to_string = function
  | WinStreak -> "win_streak"
  | Points20Plus -> "pts_20plus"
  | DoubleDouble -> "double_double"
  | TripleDouble -> "triple_double"
  | Rebounds10Plus -> "reb_10plus"
  | Assists7Plus -> "ast_7plus"
  | Blocks3Plus -> "blk_3plus"
  | Steals3Plus -> "stl_3plus"

let streak_type_to_label = function
  | WinStreak -> "연승"
  | Points20Plus -> "20+ 득점"
  | DoubleDouble -> "더블더블"
  | TripleDouble -> "트리플더블"
  | Rebounds10Plus -> "10+ 리바운드"
  | Assists7Plus -> "7+ 어시스트"
  | Blocks3Plus -> "3+ 블록"
  | Steals3Plus -> "3+ 스틸"

let streak_type_to_emoji = function
  | WinStreak -> "W"
  | Points20Plus -> "P"
  | DoubleDouble -> "DD"
  | TripleDouble -> "TD"
  | Rebounds10Plus -> "R"
  | Assists7Plus -> "A"
  | Blocks3Plus -> "B"
  | Steals3Plus -> "S"

type player_streak = {
  ps_player_id: string;
  ps_player_name: string;
  ps_team_name: string;
  ps_streak_type: streak_type;
  ps_current_count: int;
  ps_is_active: bool;  (** True if streak is ongoing *)
  ps_start_date: string;
  ps_end_date: string option;  (** None if still active *)
  ps_games: player_game_stat list;  (** Games in the streak *)
}

type team_streak = {
  ts_team_name: string;
  ts_streak_type: streak_type;
  ts_current_count: int;
  ts_is_active: bool;
  ts_start_date: string;
  ts_end_date: string option;
  ts_game_ids: string list;
}

type streak_record = {
  sr_holder_name: string;      (** Player or team name *)
  sr_holder_id: string option; (** Player ID if player streak *)
  sr_team_name: string option; (** Team name for player streaks *)
  sr_streak_type: streak_type;
  sr_count: int;
  sr_start_date: string;
  sr_end_date: string;
  sr_season: string;
}

(** Clutch Time Statistics
    Clutch time = Q4 remaining 5 minutes + score diff <= 5 points *)
type clutch_stats = {
  cs_player_id: string;
  cs_player_name: string;
  cs_team_name: string;
  cs_clutch_games: int;
  cs_clutch_points: int;
  cs_clutch_fg_made: int;
  cs_clutch_fg_att: int;
  cs_clutch_fg_pct: float;
  cs_clutch_ft_made: int;
  cs_clutch_ft_att: int;
  cs_clutch_3p_made: int;
}

(** Parse clock string "MM:SS" to seconds remaining *)
let parse_clock (clock: string) : int =
  try
    match String.split_on_char ':' clock with
    | [min; sec] ->
      let m = int_of_string (String.trim min) in
      let s = int_of_string (String.trim sec) in
      m * 60 + s
    | _ -> 0
  with _ -> 0

(** Check if event is in clutch time
    Clutch time = Q4 + remaining <= 5 min + score diff <= 5 points *)
let is_clutch_time ~period_code ~clock ~score_diff : bool =
  period_code = "Q4" &&
  parse_clock clock <= 300 &&  (* 5 minutes = 300 seconds *)
  abs score_diff <= 5

(** Extract clutch time events from PBP data *)
let extract_clutch_events (events: pbp_event list) : pbp_event list =
  List.filter (fun e ->
    let score_diff = match e.pe_team1_score, e.pe_team2_score with
      | Some s1, Some s2 -> s1 - s2
      | _ -> 999  (* No score info = not clutch *)
    in
    is_clutch_time ~period_code:e.pe_period_code ~clock:e.pe_clock ~score_diff
  ) events

(** Parse points from PBP description
    Returns (points, is_made, is_3pt, is_ft) *)
let parse_scoring_from_description (desc: string) : (int * bool * bool * bool) option =
  let desc_lower = String.lowercase_ascii desc in
  (* Korean basketball PBP patterns *)
  if String.length desc_lower = 0 then None
  else if String.sub desc_lower 0 (min 4 (String.length desc_lower)) = "자유투" ||
          (try String.sub desc_lower 0 2 = "ft" with _ -> false) then
    if String.sub desc_lower (String.length desc_lower - 2) 2 = "성공" ||
       (try String.sub desc_lower (String.length desc_lower - 4) 4 = "made" with _ -> false) then
      Some (1, true, false, true)
    else
      Some (0, false, false, true)
  else if (try String.sub desc_lower 0 3 = "3점" with _ -> false) ||
          (try String.sub desc_lower 0 2 = "3p" with _ -> false) then
    if String.sub desc_lower (String.length desc_lower - 2) 2 = "성공" ||
       (try String.sub desc_lower (String.length desc_lower - 4) 4 = "made" with _ -> false) then
      Some (3, true, true, false)
    else
      Some (0, false, true, false)
  else if (try String.sub desc_lower 0 2 = "2점" with _ -> false) ||
          (try String.sub desc_lower 0 2 = "2p" with _ -> false) ||
          (try String.sub desc_lower 0 3 = "슛" with _ -> false) ||
          (try String.sub desc_lower 0 6 = "layup" with _ -> false) ||
          (try String.sub desc_lower 0 4 = "dunk" with _ -> false) then
    if String.sub desc_lower (String.length desc_lower - 2) 2 = "성공" ||
       (try String.sub desc_lower (String.length desc_lower - 4) 4 = "made" with _ -> false) then
      Some (2, true, false, false)
    else
      Some (0, false, false, false)
  else
    None
