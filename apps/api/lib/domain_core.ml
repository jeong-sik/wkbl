(** Domain Core — foundational types shared across domain sub-modules.
    Sub-modules open this module; the Domain facade includes everything. *)

(* ============================================= *)
(* Player Core Types                              *)
(* ============================================= *)

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
  total_fg_made: int;
  total_fg_att: int;
  total_fg3_made: int;
  total_fg3_att: int;
  total_ft_made: int;
  total_ft_att: int;
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

(* ============================================= *)
(* Season & Team Types                            *)
(* ============================================= *)

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
  tov_pct: float;
  orb_pct: float;
  ftr: float;
  pace: float;
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
  | TeamByEfgPct
  | TeamByTsPct
  | TeamByTovPct
  | TeamByOrbPct
  | TeamByFtr
  | TeamByFg3Pct
  | TeamByMinutes

let team_sort_of_string = function
  | "pts" -> TeamByPoints
  | "reb" -> TeamByRebounds
  | "ast" -> TeamByAssists
  | "stl" -> TeamBySteals
  | "blk" -> TeamByBlocks
  | "eff" -> TeamByEfficiency
  | "efg" | "efg_pct" -> TeamByEfgPct
  | "ts_pct" -> TeamByTsPct
  | "tov" | "tov_pct" -> TeamByTovPct
  | "orb" | "orb_pct" -> TeamByOrbPct
  | "ftr" -> TeamByFtr
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

(* ============================================= *)
(* Game Types                                     *)
(* ============================================= *)

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

(* ============================================= *)
(* PBP Event Type                                 *)
(* ============================================= *)

type pbp_event = {
  pe_period_code: string;
  pe_event_index: int;
  pe_team_side: int;
  pe_description: string;
  pe_team1_score: int option;
  pe_team2_score: int option;
  pe_clock: string;
}

(* ============================================= *)
(* Player Stats & Info Types                      *)
(* ============================================= *)

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
  ss_team_name: string;
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
  ss_fg_pct: float;
  ss_fg3_pct: float;
  ss_ft_pct: float;
  ss_ts_pct: float;
  ss_efg_pct: float;
  ss_usg_pct: float;
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
  fg_made: int;
  fg_att: int;
  fg3_made: int;
  fg3_att: int;
  ft_made: int;
  ft_att: int;
  pts: int;
  reb: int;
  ast: int;
  stl: int;
  blk: int;
  tov: int;
  plus_minus: int option;
}

(* ============================================= *)
(* Player Career & Profile Types                  *)
(* ============================================= *)

(** Player game stat with player_id for batch queries *)
type player_game_stat_with_id = {
  pgs_player_id: string;
  pgs_stat: player_game_stat;
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

(** Normalize team stints derived from per-game boxscores.

    The official dataset occasionally contains an obvious one-off row that would
    create a "A -> B -> A" timeline with B=1 game (often very low minutes).
    For user-facing "team movement" UI, we collapse that sandwiched 1-game stint. *)
let normalize_player_team_stints (stints : player_team_stint list) : player_team_stint list =
  let rec pass ~changed acc = function
    | a :: b :: c :: rest
      when a.pts_team_name = c.pts_team_name
        && b.pts_team_name <> a.pts_team_name
        && b.pts_games_played = 1 ->
        let merged =
          { a with
            pts_end_date = c.pts_end_date;
            pts_games_played = a.pts_games_played + c.pts_games_played;
          }
        in
        pass ~changed:true acc (merged :: rest)
    | x :: xs -> pass ~changed (x :: acc) xs
    | [] -> (List.rev acc, changed)
  in
  let rec fix xs =
    let ys, changed = pass ~changed:false [] xs in
    if changed then fix ys else ys
  in
  fix stints

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

type dataset_status = {
  ds_count: int;
  ds_last_scraped_at: string option;
  ds_reason: string option;
}

type player_external_link = {
  pel_player_id: string;
  pel_link_type: string;
  pel_url: string;
  pel_source_url: string option;
  pel_scraped_at: string;
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
  career_entries: player_career_entry list;
}

(* ============================================= *)
(* Player Advanced & Shooting Stats               *)
(* ============================================= *)

(** Aggregated shooting stats for shot distribution visualization *)
type player_shooting_stats = {
  pss_player_id: string;
  pss_name: string;
  pss_games: int;
  pss_fg_made: int;
  pss_fg_attempted: int;
  pss_fg_pct: float;
  pss_fg3_made: int;
  pss_fg3_attempted: int;
  pss_fg3_pct: float;
  pss_ft_made: int;
  pss_ft_attempted: int;
  pss_ft_pct: float;
}

(** Advanced player statistics *)
type player_advanced_stats = {
  pas_player_id: string;
  pas_name: string;
  pas_team: string;
  pas_games: int;
  pas_ts_pct: float;      (** True Shooting % *)
  pas_efg_pct: float;     (** Effective FG % *)
  pas_usage_pct: float;   (** Usage Rate % *)
  pas_per: float;         (** Player Efficiency Rating *)
  pas_ortg: float;        (** Offensive Rating (points per 100 possessions) *)
  pas_drtg: float;        (** Defensive Rating (points allowed per 100 possessions) *)
  pas_net_rtg: float;     (** Net Rating = ORtg - DRtg *)
}

(* ============================================= *)
(* Team Detail & H2H Types                        *)
(* ============================================= *)

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
  tfd_standings: team_standing list;  (** All teams for league rank context *)
  tfd_roster: player_aggregate list;
  tfd_game_results: team_game_result list;
  tfd_recent_games: team_game_result list;
  tfd_team_totals: team_totals option;  (** For Four Factors calculation *)
  tfd_all_totals: team_totals list;  (** All teams' totals for league ranking *)
}

type h2h_game = {
  hg_game_id: string;
  hg_game_date: string;
  player1_team: string;
  player2_team: string;
  player1_pts: int;
  player1_reb: int;
  player1_ast: int;
  player1_stl: int;
  player1_blk: int;
  player2_pts: int;
  player2_reb: int;
  player2_ast: int;
  player2_stl: int;
  player2_blk: int;
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
  h2h_p1_avg_stl: float;
  h2h_p1_avg_blk: float;
  h2h_p2_avg_pts: float;
  h2h_p2_avg_reb: float;
  h2h_p2_avg_ast: float;
  h2h_p2_avg_stl: float;
  h2h_p2_avg_blk: float;
}

(** Calculate H2H summary from a list of h2h_games *)
let calculate_h2h_summary ~p1_team (games: h2h_game list) : h2h_summary =
  let total = List.length games in
  if total = 0 then
    { h2h_total_games = 0;
      h2h_p1_wins = 0; h2h_p2_wins = 0;
      h2h_p1_avg_pts = 0.0; h2h_p1_avg_reb = 0.0; h2h_p1_avg_ast = 0.0;
      h2h_p1_avg_stl = 0.0; h2h_p1_avg_blk = 0.0;
      h2h_p2_avg_pts = 0.0; h2h_p2_avg_reb = 0.0; h2h_p2_avg_ast = 0.0;
      h2h_p2_avg_stl = 0.0; h2h_p2_avg_blk = 0.0 }
  else
    (* Single-pass fold for all stats including steals and blocks *)
    let (p1_wins, sum_p1_pts, sum_p1_reb, sum_p1_ast, sum_p1_stl, sum_p1_blk,
         sum_p2_pts, sum_p2_reb, sum_p2_ast, sum_p2_stl, sum_p2_blk) =
      List.fold_left (fun (w1, p1pts, p1reb, p1ast, p1stl, p1blk, p2pts, p2reb, p2ast, p2stl, p2blk) g ->
        let w1' = if g.player1_team = g.winner_team then w1 + 1 else w1 in
        (w1', p1pts + g.player1_pts, p1reb + g.player1_reb, p1ast + g.player1_ast,
         p1stl + g.player1_stl, p1blk + g.player1_blk,
         p2pts + g.player2_pts, p2reb + g.player2_reb, p2ast + g.player2_ast,
         p2stl + g.player2_stl, p2blk + g.player2_blk))
      (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) games
    in
    let p2_wins = total - p1_wins in
    let n = float_of_int total in
    let _ = p1_team in (* suppress unused warning *)
    { h2h_total_games = total;
      h2h_p1_wins = p1_wins;
      h2h_p2_wins = p2_wins;
      h2h_p1_avg_pts = float_of_int sum_p1_pts /. n;
      h2h_p1_avg_reb = float_of_int sum_p1_reb /. n;
      h2h_p1_avg_ast = float_of_int sum_p1_ast /. n;
      h2h_p1_avg_stl = float_of_int sum_p1_stl /. n;
      h2h_p1_avg_blk = float_of_int sum_p1_blk /. n;
      h2h_p2_avg_pts = float_of_int sum_p2_pts /. n;
      h2h_p2_avg_reb = float_of_int sum_p2_reb /. n;
      h2h_p2_avg_ast = float_of_int sum_p2_ast /. n;
      h2h_p2_avg_stl = float_of_int sum_p2_stl /. n;
      h2h_p2_avg_blk = float_of_int sum_p2_blk /. n }

(* ============================================= *)
(* Historical Types                               *)
(* ============================================= *)

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

(* ============================================= *)
(* Team Code Utilities                            *)
(* ============================================= *)

(** Detect UTF-8 whitespace/invisible chars. Returns Some skip_bytes or None. *)
let is_utf8_whitespace s i len =
  let c0 = s.[i] in
  if c0 = ' ' || c0 = '\t' || c0 = '\n' || c0 = '\r' || c0 = '"' || c0 = '\\' then Some 1
  else if c0 = '\xC2' && i + 1 < len && s.[i + 1] = '\xA0' then Some 2  (* NBSP *)
  else if i + 2 < len then
    let c1, c2 = s.[i + 1], s.[i + 2] in
    match c0, c1 with
    | '\xEF', '\xBB' when c2 = '\xBF' -> Some 3  (* UTF-8 BOM *)
    | '\xE2', '\x80' when c2 >= '\x89' && c2 <= '\x8F' || c2 = '\xAF' -> Some 3
    | '\xE2', '\x81' when c2 = '\xA0' -> Some 3  (* WORD JOINER *)
    | '\xE3', '\x80' when c2 = '\x80' -> Some 3  (* IDEOGRAPHIC SPACE *)
    | _ -> None
  else None

(** Helper to map team names to codes *)
let normalize_label (s: string) =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i prev_space =
    if i >= len then ()
    else match is_utf8_whitespace s i len with
      | Some skip -> if not prev_space then Buffer.add_char buf ' '; loop (i + skip) true
      | None -> Buffer.add_char buf s.[i]; loop (i + 1) false
  in
  loop 0 true;
  Buffer.contents buf |> String.trim

let team_code_of_string team_name =
  let key = team_name |> normalize_label |> String.uppercase_ascii in
  let key_len = String.length key in
  (* Reject strings longer than 30 characters to prevent loose matching on arbitrary text *)
  let max_length_for_contains = 30 in
  let contains (needle : string) =
    if key_len > max_length_for_contains then false
    else
      let nlen = String.length needle in
      let hlen = key_len in
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
  (* Modern teams (active) *)
  | "아산 우리은행 우리WON" | "우리은행" | "우리WON" | "WO" -> Some "WO"
  | "용인 삼성생명 블루밍스" | "삼성생명" | "SS" -> Some "SS"
  | "인천 신한은행 에스버드" | "신한은행" | "SH" -> Some "SH"
  | "청주 KB스타즈" | "KB스타즈" | "KB" -> Some "KB"
  | "부천 하나은행" | "하나은행" | "HN" -> Some "HN"
  | "부산 BNK 썸" | "BNK 썸" | "BNK썸" | "BNK" | "BN" -> Some "BN"
  (* Historical teams (defunct/renamed) *)
  | "금호생명" | "GH" -> Some "GH"    (* 2003-2011 *)
  | "신세계" | "SG" -> Some "SG"       (* 1998-2011 *)
  | "현대" | "HD" -> Some "HD"         (* 1998-2011 *)
  | "국민은행" -> Some "KB"            (* Pre-KB era *)
  | "농협" | "NH" -> Some "NH"         (* 2003-2004 *)
  | "LG" -> Some "LG"                  (* 1998-2001 *)
  | "한화" | "HW" -> Some "HW"         (* 1998-2000 *)
  | "하나외환" -> Some "HN"            (* 2011-2015, merged to 하나은행 *)
  | "KEB하나" -> Some "HN"             (* 2015-2018 era name *)
  | "KDB생명" | "KDB" -> Some "KD"     (* 2012-2016 *)
  | "OK저축은행" | "OK" -> Some "OK"   (* 2018-2019 special events *)
  | "흥국생명" -> Some "HK"            (* 2016-2020 *)
  (* Special events / All-Star - map to special codes *)
  | "팀 포니블" | "팀 유니블" -> Some "AS"  (* All-Star *)
  | "블루스타" | "핑크스타" -> Some "AS"
  | "남부선발" | "중부선발" -> Some "AS"
  | "희망팀" | "사랑팀" -> Some "AS"
  | "질풍가도" | "여유만만" -> Some "AS"
  (* Contains matching *)
  | _ when contains "우리은행" -> Some "WO"
  | _ when contains "삼성생명" -> Some "SS"
  | _ when contains "신한은행" -> Some "SH"
  | _ when contains "하나은행" -> Some "HN"
  | _ when contains "하나외환" -> Some "HN"
  | _ when contains "KEB하나" -> Some "HN"
  | _ when contains "BNK" -> Some "BN"
  | _ when contains "KB" -> Some "KB"
  | _ when contains "금호생명" -> Some "GH"
  | _ when contains "신세계" -> Some "SG"
  | _ when contains "현대" -> Some "HD"
  | _ when contains "KDB" -> Some "KD"
  | _ when contains "흥국" -> Some "HK"
  (* International / exhibition games - skip these *)
  | _ when contains "CHANSON" -> None
  | _ when contains "JOMO" -> None
  | _ when contains "중국" -> None
  | _ when contains "일본" -> None
  | _ when contains "북경" -> None
  | _ when contains "한국 올스타" -> Some "AS"
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
