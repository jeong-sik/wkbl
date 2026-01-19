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
