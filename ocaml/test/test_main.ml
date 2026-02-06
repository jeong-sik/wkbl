(** WKBL Test Suite
    Tests for domain logic, MVP scoring, and utility functions *)

open Wkbl.Domain

(** Helper for float comparison with tolerance *)
let float_eq ?(eps=0.001) a b = Float.abs (a -. b) < eps

let float_testable =
  Alcotest.testable (Fmt.float) (fun a b -> float_eq a b)

(** Simple substring check for HTML assertions in tests. *)
let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  if len_sub = 0 then true else loop 0

(* ============================================= *)
(* MVP Score Calculation Tests                   *)
(* ============================================= *)

let test_mvp_score_basic () =
  (* Test with typical player stats *)
  let (base, bonus, final) = calculate_mvp_score
    ~ppg:20.0 ~rpg:8.0 ~apg:5.0 ~spg:1.5 ~bpg:1.0
    ~efficiency:25.0 ~win_pct:0.75 in
  (* Base = (20*2) + (8*1.2) + (5*1.5) + (1.5*2) + (1*2) + (25*0.5)
         = 40 + 9.6 + 7.5 + 3 + 2 + 12.5 = 74.6 *)
  (* Win bonus = 0.75 * 20 = 15 *)
  Alcotest.(check float_testable) "base score" 74.6 base;
  Alcotest.(check float_testable) "win bonus" 15.0 bonus;
  Alcotest.(check float_testable) "final score" 89.6 final

let test_mvp_score_zero_stats () =
  (* Test with zero stats *)
  let (base, bonus, final) = calculate_mvp_score
    ~ppg:0.0 ~rpg:0.0 ~apg:0.0 ~spg:0.0 ~bpg:0.0
    ~efficiency:0.0 ~win_pct:0.0 in
  Alcotest.(check float_testable) "zero base" 0.0 base;
  Alcotest.(check float_testable) "zero bonus" 0.0 bonus;
  Alcotest.(check float_testable) "zero final" 0.0 final

let test_mvp_score_elite_player () =
  (* Test with elite player stats (e.g., MVP caliber) *)
  let (base, bonus, final) = calculate_mvp_score
    ~ppg:25.0 ~rpg:10.0 ~apg:7.0 ~spg:2.0 ~bpg:2.0
    ~efficiency:35.0 ~win_pct:0.9 in
  (* Base = (25*2) + (10*1.2) + (7*1.5) + (2*2) + (2*2) + (35*0.5)
         = 50 + 12 + 10.5 + 4 + 4 + 17.5 = 98.0 *)
  (* Win bonus = 0.9 * 20 = 18 *)
  Alcotest.(check float_testable) "elite base" 98.0 base;
  Alcotest.(check float_testable) "elite bonus" 18.0 bonus;
  Alcotest.(check float_testable) "elite final" 116.0 final

let test_mvp_score_losing_team () =
  (* Good individual stats but losing team *)
  let (base, bonus, final) = calculate_mvp_score
    ~ppg:22.0 ~rpg:9.0 ~apg:6.0 ~spg:1.5 ~bpg:1.5
    ~efficiency:28.0 ~win_pct:0.25 in
  (* Base = (22*2) + (9*1.2) + (6*1.5) + (1.5*2) + (1.5*2) + (28*0.5)
         = 44 + 10.8 + 9 + 3 + 3 + 14 = 83.8 *)
  (* Win bonus = 0.25 * 20 = 5 *)
  Alcotest.(check float_testable) "losing team base" 83.8 base;
  Alcotest.(check float_testable) "losing team bonus" 5.0 bonus;
  Alcotest.(check float_testable) "losing team final" 88.8 final

let test_mvp_score_perfect_record () =
  (* Perfect win percentage *)
  let (_base, bonus, _final) = calculate_mvp_score
    ~ppg:15.0 ~rpg:5.0 ~apg:3.0 ~spg:1.0 ~bpg:0.5
    ~efficiency:18.0 ~win_pct:1.0 in
  Alcotest.(check float_testable) "perfect record bonus" 20.0 bonus

let test_mvp_score_nan_input () =
  (* Test with NaN input - should return zeros *)
  let (base, bonus, final) = calculate_mvp_score
    ~ppg:Float.nan ~rpg:8.0 ~apg:5.0 ~spg:1.5 ~bpg:1.0
    ~efficiency:25.0 ~win_pct:0.75 in
  Alcotest.(check float_testable) "nan input base" 0.0 base;
  Alcotest.(check float_testable) "nan input bonus" 0.0 bonus;
  Alcotest.(check float_testable) "nan input final" 0.0 final

let test_mvp_score_win_pct_out_of_range () =
  (* Test with win_pct > 1.0 - should return zeros *)
  let (base, bonus, final) = calculate_mvp_score
    ~ppg:20.0 ~rpg:8.0 ~apg:5.0 ~spg:1.5 ~bpg:1.0
    ~efficiency:25.0 ~win_pct:1.5 in
  Alcotest.(check float_testable) "win_pct > 1.0 base" 0.0 base;
  Alcotest.(check float_testable) "win_pct > 1.0 bonus" 0.0 bonus;
  Alcotest.(check float_testable) "win_pct > 1.0 final" 0.0 final;
  (* Test with win_pct < 0.0 - should also return zeros *)
  let (base2, bonus2, final2) = calculate_mvp_score
    ~ppg:20.0 ~rpg:8.0 ~apg:5.0 ~spg:1.5 ~bpg:1.0
    ~efficiency:25.0 ~win_pct:(-0.5) in
  Alcotest.(check float_testable) "win_pct < 0.0 base" 0.0 base2;
  Alcotest.(check float_testable) "win_pct < 0.0 bonus" 0.0 bonus2;
  Alcotest.(check float_testable) "win_pct < 0.0 final" 0.0 final2

let mvp_score_tests = [
  Alcotest.test_case "Basic MVP score calculation" `Quick test_mvp_score_basic;
  Alcotest.test_case "Zero stats" `Quick test_mvp_score_zero_stats;
  Alcotest.test_case "Elite player stats" `Quick test_mvp_score_elite_player;
  Alcotest.test_case "Losing team scenario" `Quick test_mvp_score_losing_team;
  Alcotest.test_case "Perfect record bonus" `Quick test_mvp_score_perfect_record;
  Alcotest.test_case "NaN input handling" `Quick test_mvp_score_nan_input;
  Alcotest.test_case "win_pct out of range" `Quick test_mvp_score_win_pct_out_of_range;
]

(* ============================================= *)
(* Player Sort Parsing Tests                     *)
(* ============================================= *)

let player_sort_testable =
  Alcotest.testable
    (fun fmt s -> Fmt.string fmt (match s with
      | ByPoints -> "ByPoints"
      | ByMargin -> "ByMargin"
      | ByRebounds -> "ByRebounds"
      | ByAssists -> "ByAssists"
      | ByEfficiency -> "ByEfficiency"
      | ByMinutes -> "ByMinutes"))
    (=)

let test_player_sort_points () =
  Alcotest.(check player_sort_testable) "pts" ByPoints (player_sort_of_string "pts");
  Alcotest.(check player_sort_testable) "points" ByPoints (player_sort_of_string "points")

let test_player_sort_margin () =
  Alcotest.(check player_sort_testable) "mg" ByMargin (player_sort_of_string "mg");
  Alcotest.(check player_sort_testable) "margin" ByMargin (player_sort_of_string "margin")

let test_player_sort_rebounds () =
  Alcotest.(check player_sort_testable) "reb" ByRebounds (player_sort_of_string "reb");
  Alcotest.(check player_sort_testable) "rebounds" ByRebounds (player_sort_of_string "rebounds")

let test_player_sort_assists () =
  Alcotest.(check player_sort_testable) "ast" ByAssists (player_sort_of_string "ast");
  Alcotest.(check player_sort_testable) "assists" ByAssists (player_sort_of_string "assists")

let test_player_sort_minutes () =
  Alcotest.(check player_sort_testable) "min" ByMinutes (player_sort_of_string "min");
  Alcotest.(check player_sort_testable) "minutes" ByMinutes (player_sort_of_string "minutes")

let test_player_sort_default () =
  (* Unknown strings should default to ByEfficiency *)
  Alcotest.(check player_sort_testable) "unknown" ByEfficiency (player_sort_of_string "unknown");
  Alcotest.(check player_sort_testable) "empty" ByEfficiency (player_sort_of_string "");
  Alcotest.(check player_sort_testable) "eff" ByEfficiency (player_sort_of_string "eff")

let player_sort_tests = [
  Alcotest.test_case "Points sort" `Quick test_player_sort_points;
  Alcotest.test_case "Margin sort" `Quick test_player_sort_margin;
  Alcotest.test_case "Rebounds sort" `Quick test_player_sort_rebounds;
  Alcotest.test_case "Assists sort" `Quick test_player_sort_assists;
  Alcotest.test_case "Minutes sort" `Quick test_player_sort_minutes;
  Alcotest.test_case "Default to efficiency" `Quick test_player_sort_default;
]

(* ============================================= *)
(* Team Code Translation Tests                   *)
(* ============================================= *)

let test_team_code_known () =
  (* team_code_to_city_en returns city names *)
  Alcotest.(check string) "SH -> Incheon" "Incheon" (team_code_to_city_en "SH");
  Alcotest.(check string) "KB -> Cheongju" "Cheongju" (team_code_to_city_en "KB");
  Alcotest.(check string) "WO -> Asan" "Asan" (team_code_to_city_en "WO");
  Alcotest.(check string) "SS -> Yongin" "Yongin" (team_code_to_city_en "SS");
  Alcotest.(check string) "HN -> Bucheon" "Bucheon" (team_code_to_city_en "HN");
  Alcotest.(check string) "BN -> Busan" "Busan" (team_code_to_city_en "BN")

let test_team_code_unknown () =
  Alcotest.(check string) "empty for unknown" "" (team_code_to_city_en "XX");
  Alcotest.(check string) "empty for blank" "" (team_code_to_city_en "")

(* team_code_of_string tests *)
let team_code_opt_testable =
  Alcotest.testable
    (fun fmt opt -> match opt with
      | None -> Fmt.string fmt "None"
      | Some s -> Fmt.pf fmt "Some %s" s)
    (=)

let test_team_code_of_string_exact_match () =
  (* Exact matches should work *)
  Alcotest.(check team_code_opt_testable) "우리은행" (Some "WO") (team_code_of_string "우리은행");
  Alcotest.(check team_code_opt_testable) "WO" (Some "WO") (team_code_of_string "WO");
  Alcotest.(check team_code_opt_testable) "삼성생명" (Some "SS") (team_code_of_string "삼성생명");
  Alcotest.(check team_code_opt_testable) "SS" (Some "SS") (team_code_of_string "SS");
  Alcotest.(check team_code_opt_testable) "신한은행" (Some "SH") (team_code_of_string "신한은행");
  Alcotest.(check team_code_opt_testable) "KB스타즈" (Some "KB") (team_code_of_string "KB스타즈");
  Alcotest.(check team_code_opt_testable) "하나은행" (Some "HN") (team_code_of_string "하나은행");
  Alcotest.(check team_code_opt_testable) "BNK" (Some "BN") (team_code_of_string "BNK")

let test_team_code_of_string_contains () =
  (* Short strings with team name should match via contains *)
  Alcotest.(check team_code_opt_testable) "우리은행 경기" (Some "WO") (team_code_of_string "우리은행 경기");
  Alcotest.(check team_code_opt_testable) "오늘의 KB 경기" (Some "KB") (team_code_of_string "오늘의 KB 경기")

let test_team_code_of_string_long_string_rejected () =
  (* Long strings (>30 chars) should be rejected even if they contain team name *)
  let long_text = "이것은 매우 긴 텍스트입니다. 우리은행이 포함되어 있지만 너무 길어서 매칭되면 안됩니다." in
  Alcotest.(check team_code_opt_testable) "Long string with 우리은행" None (team_code_of_string long_text);
  let another_long = "악의적인 우리은행 텍스트가 포함된 아주 긴 문자열입니다" in
  Alcotest.(check team_code_opt_testable) "Another long string" None (team_code_of_string another_long)

let test_team_code_of_string_edge_cases () =
  (* Empty and unknown strings *)
  Alcotest.(check team_code_opt_testable) "empty" None (team_code_of_string "");
  Alcotest.(check team_code_opt_testable) "random" None (team_code_of_string "random text");
  (* Exactly 30 chars with team name should still work *)
  let exactly_30 = "AB우리은행CD" in  (* This is short, should work *)
  Alcotest.(check team_code_opt_testable) "Short with 우리은행" (Some "WO") (team_code_of_string exactly_30)

let team_code_tests = [
  Alcotest.test_case "Known team codes" `Quick test_team_code_known;
  Alcotest.test_case "Unknown team codes" `Quick test_team_code_unknown;
  Alcotest.test_case "team_code_of_string exact match" `Quick test_team_code_of_string_exact_match;
  Alcotest.test_case "team_code_of_string contains" `Quick test_team_code_of_string_contains;
  Alcotest.test_case "team_code_of_string long string rejected" `Quick test_team_code_of_string_long_string_rejected;
  Alcotest.test_case "team_code_of_string edge cases" `Quick test_team_code_of_string_edge_cases;
]

(* ============================================= *)
(* Game Score Quality Tests                      *)
(* ============================================= *)

let game_quality_testable =
  Alcotest.testable
    (fun fmt q -> Fmt.string fmt (match q with
      | Verified -> "Verified"
      | Derived -> "Derived"
      | Mismatch -> "Mismatch"))
    (=)

let test_game_score_quality () =
  Alcotest.(check game_quality_testable) "verified" Verified (game_score_quality_of_int 2);
  Alcotest.(check game_quality_testable) "mismatch" Mismatch (game_score_quality_of_int 0);
  Alcotest.(check game_quality_testable) "derived 1" Derived (game_score_quality_of_int 1);
  Alcotest.(check game_quality_testable) "derived -1" Derived (game_score_quality_of_int (-1));
  Alcotest.(check game_quality_testable) "derived 99" Derived (game_score_quality_of_int 99)

let game_quality_tests = [
  Alcotest.test_case "Score quality mapping" `Quick test_game_score_quality;
]

(* ============================================= *)
(* Milestone Calculation Tests (Logic)           *)
(* ============================================= *)

let test_milestone_logic () =
  (* Test milestone calculation logic *)
  let milestones = [1000; 2000; 3000; 5000; 7000; 10000] in
  let current = 2500 in
  let achieved = List.filter (fun m -> current >= m) milestones in
  let next = List.find_opt (fun m -> current < m) milestones in
  Alcotest.(check (list int)) "achieved milestones" [1000; 2000] achieved;
  Alcotest.(check (option int)) "next milestone" (Some 3000) next

let test_milestone_all_achieved () =
  let milestones = [1000; 2000; 3000] in
  let current = 5000 in
  let achieved = List.filter (fun m -> current >= m) milestones in
  let next = List.find_opt (fun m -> current < m) milestones in
  Alcotest.(check (list int)) "all achieved" [1000; 2000; 3000] achieved;
  Alcotest.(check (option int)) "no next" None next

let test_milestone_none_achieved () =
  let milestones = [1000; 2000; 3000] in
  let current = 500 in
  let achieved = List.filter (fun m -> current >= m) milestones in
  let next = List.find_opt (fun m -> current < m) milestones in
  Alcotest.(check (list int)) "none achieved" [] achieved;
  Alcotest.(check (option int)) "first is next" (Some 1000) next

let test_milestone_progress () =
  (* Progress calculation: current / next *)
  let current = 1500 in
  let next = 2000 in
  let progress = (Float.of_int current) /. (Float.of_int next) *. 100.0 in
  Alcotest.(check float_testable) "75% progress" 75.0 progress

let milestone_tests = [
  Alcotest.test_case "Partial milestone achievement" `Quick test_milestone_logic;
  Alcotest.test_case "All milestones achieved" `Quick test_milestone_all_achieved;
  Alcotest.test_case "No milestones achieved" `Quick test_milestone_none_achieved;
  Alcotest.test_case "Progress calculation" `Quick test_milestone_progress;
]

(* ============================================= *)
(* Career Trajectory Logic Tests                 *)
(* ============================================= *)

let test_find_peak_season () =
  (* Simulate finding peak efficiency season *)
  let seasons = [
    ("2021-22", 18.5);
    ("2022-23", 22.3);  (* Peak *)
    ("2023-24", 20.1);
  ] in
  let peak = List.fold_left (fun acc (s, e) ->
    match acc with
    | None -> Some (s, e)
    | Some (_, prev_e) -> if e > prev_e then Some (s, e) else acc
  ) None seasons in
  Alcotest.(check (option (pair string float_testable)))
    "peak season"
    (Some ("2022-23", 22.3))
    peak

let test_trend_calculation () =
  (* Test trend: compare first and last season *)
  let ppg_first = 15.0 in
  let ppg_last = 18.5 in
  let trend = ppg_last -. ppg_first in
  let trend_pct = (trend /. ppg_first) *. 100.0 in
  Alcotest.(check float_testable) "PPG improved by 3.5" 3.5 trend;
  Alcotest.(check float_testable) "23.33% improvement" 23.333 trend_pct

let test_single_season_no_chart () =
  (* Minimum 2 seasons required for chart *)
  let seasons = [("2023-24", 20.0)] in
  let can_show_chart = List.length seasons >= 2 in
  Alcotest.(check bool) "single season cannot show chart" false can_show_chart

let test_multi_season_chart () =
  let seasons = [("2022-23", 18.0); ("2023-24", 20.0)] in
  let can_show_chart = List.length seasons >= 2 in
  Alcotest.(check bool) "multi season can show chart" true can_show_chart

let career_trajectory_tests = [
  Alcotest.test_case "Find peak season" `Quick test_find_peak_season;
  Alcotest.test_case "Trend calculation" `Quick test_trend_calculation;
  Alcotest.test_case "Single season no chart" `Quick test_single_season_no_chart;
  Alcotest.test_case "Multi season enables chart" `Quick test_multi_season_chart;
]

(* ============================================= *)
(* Fantasy Score Calculation Tests               *)
(* ============================================= *)

let test_fantasy_score_default_rules () =
  (* Test with default rules:
     pts=1.0, reb=1.2, ast=1.5, stl=2.0, blk=2.0, tov=-1.0 *)
  let rules = default_fantasy_rules in
  let (total, pts_c, reb_c, ast_c, stl_c, blk_c, tov_c) =
    calculate_fantasy_score ~rules ~pts:20 ~reb:10 ~ast:5 ~stl:2 ~blk:1 ~tov:3
  in
  (* pts: 20*1.0=20, reb: 10*1.2=12, ast: 5*1.5=7.5, stl: 2*2=4, blk: 1*2=2, tov: 3*(-1)=-3 *)
  Alcotest.(check float_testable) "pts contribution" 20.0 pts_c;
  Alcotest.(check float_testable) "reb contribution" 12.0 reb_c;
  Alcotest.(check float_testable) "ast contribution" 7.5 ast_c;
  Alcotest.(check float_testable) "stl contribution" 4.0 stl_c;
  Alcotest.(check float_testable) "blk contribution" 2.0 blk_c;
  Alcotest.(check float_testable) "tov contribution" (-3.0) tov_c;
  Alcotest.(check float_testable) "total score" 42.5 total

let test_fantasy_score_custom_rules () =
  (* Test with custom rules: double points value, zero turnover penalty *)
  let rules = {
    fsr_points = 2.0;
    fsr_rebounds = 1.0;
    fsr_assists = 1.0;
    fsr_steals = 1.0;
    fsr_blocks = 1.0;
    fsr_turnovers = 0.0;
  } in
  let (total, pts_c, reb_c, ast_c, stl_c, blk_c, tov_c) =
    calculate_fantasy_score ~rules ~pts:15 ~reb:8 ~ast:4 ~stl:1 ~blk:1 ~tov:5
  in
  (* pts: 15*2=30, reb: 8*1=8, ast: 4*1=4, stl: 1*1=1, blk: 1*1=1, tov: 5*0=0 *)
  Alcotest.(check float_testable) "pts custom" 30.0 pts_c;
  Alcotest.(check float_testable) "reb custom" 8.0 reb_c;
  Alcotest.(check float_testable) "ast custom" 4.0 ast_c;
  Alcotest.(check float_testable) "stl custom" 1.0 stl_c;
  Alcotest.(check float_testable) "blk custom" 1.0 blk_c;
  Alcotest.(check float_testable) "tov custom" 0.0 tov_c;
  Alcotest.(check float_testable) "total custom" 44.0 total

let test_fantasy_score_zero_stats () =
  let rules = default_fantasy_rules in
  let (total, _, _, _, _, _, _) =
    calculate_fantasy_score ~rules ~pts:0 ~reb:0 ~ast:0 ~stl:0 ~blk:0 ~tov:0
  in
  Alcotest.(check float_testable) "zero total" 0.0 total

let test_fantasy_score_high_turnovers () =
  (* Test that high turnovers significantly reduce score *)
  let rules = default_fantasy_rules in
  let (total, _, _, _, _, _, _) =
    calculate_fantasy_score ~rules ~pts:10 ~reb:5 ~ast:2 ~stl:0 ~blk:0 ~tov:10
  in
  (* pts: 10*1=10, reb: 5*1.2=6, ast: 2*1.5=3, stl: 0, blk: 0, tov: 10*(-1)=-10 *)
  (* Total = 10 + 6 + 3 + 0 + 0 - 10 = 9 *)
  Alcotest.(check float_testable) "high turnover score" 9.0 total

let test_fantasy_aggregate () =
  (* Test fantasy_score_of_aggregate function *)
  let rules = default_fantasy_rules in
  let player : player_aggregate = {
    player_id = "TEST001";
    name = "Test Player";
    team_name = "Test Team";
    games_played = 10;
    total_minutes = 300.0;
    total_points = 200;
    total_rebounds = 100;
    total_assists = 50;
    total_steals = 20;
    total_blocks = 10;
    total_turnovers = 30;
    avg_points = 20.0;
    avg_margin = 0.0;
    avg_rebounds = 10.0;
    avg_assists = 5.0;
    avg_steals = 2.0;
    avg_blocks = 1.0;
    avg_turnovers = 3.0;
    efficiency = 25.0;
  } in
  let score = fantasy_score_of_aggregate ~rules player in
  (* Total: 200*1 + 100*1.2 + 50*1.5 + 20*2 + 10*2 + 30*(-1) = 200+120+75+40+20-30 = 425 *)
  (* Avg: 425 / 10 = 42.5 *)
  Alcotest.(check float_testable) "aggregate total" 425.0 score.fps_total_score;
  Alcotest.(check float_testable) "aggregate avg" 42.5 score.fps_avg_score;
  Alcotest.(check string) "aggregate player_id" "TEST001" score.fps_player_id;
  Alcotest.(check int) "aggregate games" 10 score.fps_games_played

let fantasy_score_tests = [
  Alcotest.test_case "Default rules calculation" `Quick test_fantasy_score_default_rules;
  Alcotest.test_case "Custom rules calculation" `Quick test_fantasy_score_custom_rules;
  Alcotest.test_case "Zero stats" `Quick test_fantasy_score_zero_stats;
  Alcotest.test_case "High turnovers penalty" `Quick test_fantasy_score_high_turnovers;
  Alcotest.test_case "Aggregate calculation" `Quick test_fantasy_aggregate;
]

(* ============================================= *)
(* H2H Summary Calculation Tests                 *)
(* ============================================= *)

let make_h2h_game ~game_id ~game_date ~p1_team ~p2_team ~p1_pts ~p1_reb ~p1_ast
    ?(p1_stl=0) ?(p1_blk=0) ~p2_pts ~p2_reb ~p2_ast ?(p2_stl=0) ?(p2_blk=0)
    ~winner_team ~score_diff () : h2h_game =
  { hg_game_id = game_id; hg_game_date = game_date; player1_team = p1_team; player2_team = p2_team;
    player1_pts = p1_pts; player1_reb = p1_reb; player1_ast = p1_ast;
    player1_stl = p1_stl; player1_blk = p1_blk;
    player2_pts = p2_pts; player2_reb = p2_reb; player2_ast = p2_ast;
    player2_stl = p2_stl; player2_blk = p2_blk;
    winner_team; score_diff }

let test_h2h_summary_empty () =
  (* Empty games list should return zero summary *)
  let summary = calculate_h2h_summary ~p1_team:"TeamA" [] in
  Alcotest.(check int) "total games" 0 summary.h2h_total_games;
  Alcotest.(check int) "p1 wins" 0 summary.h2h_p1_wins;
  Alcotest.(check int) "p2 wins" 0 summary.h2h_p2_wins;
  Alcotest.(check float_testable) "p1 avg pts" 0.0 summary.h2h_p1_avg_pts;
  Alcotest.(check float_testable) "p2 avg pts" 0.0 summary.h2h_p2_avg_pts

let test_h2h_summary_single_game () =
  (* Single game where P1's team wins *)
  let games = [
    make_h2h_game ~game_id:"G1" ~game_date:"2024-01-15"
      ~p1_team:"KB" ~p2_team:"SH"
      ~p1_pts:25 ~p1_reb:10 ~p1_ast:5
      ~p2_pts:18 ~p2_reb:8 ~p2_ast:3
      ~winner_team:"KB" ~score_diff:12 ()
  ] in
  let summary = calculate_h2h_summary ~p1_team:"KB" games in
  Alcotest.(check int) "total games" 1 summary.h2h_total_games;
  Alcotest.(check int) "p1 wins" 1 summary.h2h_p1_wins;
  Alcotest.(check int) "p2 wins" 0 summary.h2h_p2_wins;
  Alcotest.(check float_testable) "p1 avg pts" 25.0 summary.h2h_p1_avg_pts;
  Alcotest.(check float_testable) "p1 avg reb" 10.0 summary.h2h_p1_avg_reb;
  Alcotest.(check float_testable) "p1 avg ast" 5.0 summary.h2h_p1_avg_ast;
  Alcotest.(check float_testable) "p2 avg pts" 18.0 summary.h2h_p2_avg_pts;
  Alcotest.(check float_testable) "p2 avg reb" 8.0 summary.h2h_p2_avg_reb;
  Alcotest.(check float_testable) "p2 avg ast" 3.0 summary.h2h_p2_avg_ast

let test_h2h_summary_multiple_games () =
  (* Three games: P1 team wins 2, P2 team wins 1 *)
  let games = [
    make_h2h_game ~game_id:"G1" ~game_date:"2024-01-10"
      ~p1_team:"KB" ~p2_team:"SH"
      ~p1_pts:20 ~p1_reb:8 ~p1_ast:6
      ~p2_pts:22 ~p2_reb:10 ~p2_ast:4
      ~winner_team:"KB" ~score_diff:5 ();
    make_h2h_game ~game_id:"G2" ~game_date:"2024-01-20"
      ~p1_team:"KB" ~p2_team:"SH"
      ~p1_pts:15 ~p1_reb:6 ~p1_ast:3
      ~p2_pts:28 ~p2_reb:12 ~p2_ast:8
      ~winner_team:"SH" ~score_diff:(-10) ();
    make_h2h_game ~game_id:"G3" ~game_date:"2024-01-30"
      ~p1_team:"KB" ~p2_team:"SH"
      ~p1_pts:30 ~p1_reb:7 ~p1_ast:9
      ~p2_pts:20 ~p2_reb:5 ~p2_ast:3
      ~winner_team:"KB" ~score_diff:15 ();
  ] in
  let summary = calculate_h2h_summary ~p1_team:"KB" games in
  Alcotest.(check int) "total games" 3 summary.h2h_total_games;
  Alcotest.(check int) "p1 wins" 2 summary.h2h_p1_wins;
  Alcotest.(check int) "p2 wins" 1 summary.h2h_p2_wins;
  (* P1 avg: (20+15+30)/3 = 65/3 = 21.667, reb: (8+6+7)/3 = 7, ast: (6+3+9)/3 = 6 *)
  Alcotest.(check float_testable) "p1 avg pts" 21.667 summary.h2h_p1_avg_pts;
  Alcotest.(check float_testable) "p1 avg reb" 7.0 summary.h2h_p1_avg_reb;
  Alcotest.(check float_testable) "p1 avg ast" 6.0 summary.h2h_p1_avg_ast;
  (* P2 avg: (22+28+20)/3 = 70/3 = 23.333, reb: (10+12+5)/3 = 9, ast: (4+8+3)/3 = 5 *)
  Alcotest.(check float_testable) "p2 avg pts" 23.333 summary.h2h_p2_avg_pts;
  Alcotest.(check float_testable) "p2 avg reb" 9.0 summary.h2h_p2_avg_reb;
  Alcotest.(check float_testable) "p2 avg ast" 5.0 summary.h2h_p2_avg_ast

let test_h2h_summary_all_p2_wins () =
  (* Two games where P2's team wins both *)
  let games = [
    make_h2h_game ~game_id:"G1" ~game_date:"2024-02-01"
      ~p1_team:"WO" ~p2_team:"BN"
      ~p1_pts:12 ~p1_reb:5 ~p1_ast:2
      ~p2_pts:25 ~p2_reb:11 ~p2_ast:7
      ~winner_team:"BN" ~score_diff:(-20) ();
    make_h2h_game ~game_id:"G2" ~game_date:"2024-02-15"
      ~p1_team:"WO" ~p2_team:"BN"
      ~p1_pts:18 ~p1_reb:7 ~p1_ast:4
      ~p2_pts:21 ~p2_reb:9 ~p2_ast:5
      ~winner_team:"BN" ~score_diff:(-8) ();
  ] in
  let summary = calculate_h2h_summary ~p1_team:"WO" games in
  Alcotest.(check int) "total games" 2 summary.h2h_total_games;
  Alcotest.(check int) "p1 wins" 0 summary.h2h_p1_wins;
  Alcotest.(check int) "p2 wins" 2 summary.h2h_p2_wins;
  (* P1 avg: (12+18)/2 = 15, reb: (5+7)/2 = 6, ast: (2+4)/2 = 3 *)
  Alcotest.(check float_testable) "p1 avg pts" 15.0 summary.h2h_p1_avg_pts;
  Alcotest.(check float_testable) "p1 avg reb" 6.0 summary.h2h_p1_avg_reb;
  Alcotest.(check float_testable) "p1 avg ast" 3.0 summary.h2h_p1_avg_ast

let h2h_summary_tests = [
  Alcotest.test_case "Empty games list" `Quick test_h2h_summary_empty;
  Alcotest.test_case "Single game" `Quick test_h2h_summary_single_game;
  Alcotest.test_case "Multiple games" `Quick test_h2h_summary_multiple_games;
  Alcotest.test_case "All P2 wins" `Quick test_h2h_summary_all_p2_wins;
]

(* ============================================= *)
(* Hot Streaks Analysis Tests                    *)
(* ============================================= *)

(** Helper to create a mock player_game_stat for streaks testing *)
let make_streak_game ~game_id ~game_date ~pts ~reb ~ast ~stl ~blk ?team_score ?opponent_score () : player_game_stat =
  {
    game_id;
    game_date;
    opponent = "Opponent";
    is_home = true;
    team_score;
    opponent_score;
    score_quality = Verified;
    min = 30.0;
    pts; reb; ast; stl; blk;
    tov = 2;
    plus_minus = Some 5;
  }

let test_streak_20plus_points () =
  (* Create 5 consecutive games with 20+ points *)
  let games = [
    make_streak_game ~game_id:"G5" ~game_date:"2024-01-15" ~pts:22 ~reb:5 ~ast:3 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G4" ~game_date:"2024-01-12" ~pts:25 ~reb:6 ~ast:4 ~stl:2 ~blk:1 ();
    make_streak_game ~game_id:"G3" ~game_date:"2024-01-09" ~pts:21 ~reb:4 ~ast:5 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:28 ~reb:7 ~ast:3 ~stl:0 ~blk:1 ();
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:20 ~reb:5 ~ast:2 ~stl:1 ~blk:0 ();
  ] in
  let streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  (* Should find a 5-game 20+ points streak *)
  let pts_streaks = List.filter (fun (s: player_streak) -> s.ps_streak_type = Points20Plus) streaks in
  Alcotest.(check bool) "has pts streak" true (List.length pts_streaks > 0);
  let best = List.hd pts_streaks in
  Alcotest.(check int) "5-game streak" 5 best.ps_current_count;
  Alcotest.(check bool) "is active" true best.ps_is_active

let test_streak_broken () =
  (* Games with a broken streak *)
  let games = [
    make_streak_game ~game_id:"G5" ~game_date:"2024-01-15" ~pts:22 ~reb:5 ~ast:3 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G4" ~game_date:"2024-01-12" ~pts:25 ~reb:6 ~ast:4 ~stl:2 ~blk:1 ();
    make_streak_game ~game_id:"G3" ~game_date:"2024-01-09" ~pts:15 ~reb:4 ~ast:5 ~stl:1 ~blk:0 ();  (* Break: 15 pts *)
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:28 ~reb:7 ~ast:3 ~stl:0 ~blk:1 ();
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:20 ~reb:5 ~ast:2 ~stl:1 ~blk:0 ();
  ] in
  let streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  let pts_streaks = List.filter (fun (s: player_streak) -> s.ps_streak_type = Points20Plus) streaks in
  (* The most recent streak should be 2 games (G5, G4) *)
  let active = List.filter (fun s -> s.ps_is_active) pts_streaks in
  Alcotest.(check bool) "has active streak" true (List.length active > 0);
  let best_active = List.hd active in
  Alcotest.(check int) "2-game active streak" 2 best_active.ps_current_count

let test_streak_double_double () =
  (* Create games with double-doubles (10+ in 2 categories) *)
  let games = [
    make_streak_game ~game_id:"G4" ~game_date:"2024-01-12" ~pts:15 ~reb:12 ~ast:5 ~stl:1 ~blk:0 ();  (* DD: pts+reb *)
    make_streak_game ~game_id:"G3" ~game_date:"2024-01-09" ~pts:18 ~reb:10 ~ast:3 ~stl:2 ~blk:1 ();  (* DD: pts+reb *)
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:12 ~reb:11 ~ast:4 ~stl:0 ~blk:0 ();  (* DD: pts+reb *)
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:8 ~reb:5 ~ast:2 ~stl:1 ~blk:0 ();    (* No DD *)
  ] in
  let streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  let dd_streaks = List.filter (fun (s: player_streak) -> s.ps_streak_type = DoubleDouble) streaks in
  Alcotest.(check bool) "has DD streak" true (List.length dd_streaks > 0);
  let best = List.hd dd_streaks in
  Alcotest.(check int) "3-game DD streak" 3 best.ps_current_count

let test_streak_10plus_rebounds () =
  (* Create games with 10+ rebounds *)
  let games = [
    make_streak_game ~game_id:"G3" ~game_date:"2024-01-09" ~pts:10 ~reb:12 ~ast:3 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:8 ~reb:15 ~ast:2 ~stl:0 ~blk:1 ();
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:12 ~reb:11 ~ast:4 ~stl:1 ~blk:0 ();
  ] in
  let streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  let reb_streaks = List.filter (fun (s: player_streak) -> s.ps_streak_type = Rebounds10Plus) streaks in
  Alcotest.(check bool) "has reb streak" true (List.length reb_streaks > 0);
  let best = List.hd reb_streaks in
  Alcotest.(check int) "3-game reb streak" 3 best.ps_current_count

let test_streak_get_best () =
  (* Test get_best_streaks function *)
  let games = [
    make_streak_game ~game_id:"G5" ~game_date:"2024-01-15" ~pts:22 ~reb:5 ~ast:3 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G4" ~game_date:"2024-01-12" ~pts:25 ~reb:12 ~ast:4 ~stl:2 ~blk:1 ();
    make_streak_game ~game_id:"G3" ~game_date:"2024-01-09" ~pts:21 ~reb:10 ~ast:5 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:28 ~reb:11 ~ast:3 ~stl:0 ~blk:1 ();
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:20 ~reb:13 ~ast:2 ~stl:1 ~blk:0 ();
  ] in
  let all_streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  let best = Wkbl.Streaks.get_best_streaks all_streaks in
  (* Should return one best streak per type *)
  let pts_best = List.filter (fun (s: player_streak) -> s.ps_streak_type = Points20Plus) best in
  let reb_best = List.filter (fun (s: player_streak) -> s.ps_streak_type = Rebounds10Plus) best in
  Alcotest.(check int) "one best pts streak" 1 (List.length pts_best);
  Alcotest.(check int) "one best reb streak" 1 (List.length reb_best)

let test_streak_get_active () =
  (* Test get_active_streaks function *)
  let games = [
    make_streak_game ~game_id:"G4" ~game_date:"2024-01-12" ~pts:22 ~reb:5 ~ast:3 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G3" ~game_date:"2024-01-09" ~pts:25 ~reb:4 ~ast:4 ~stl:2 ~blk:1 ();
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:10 ~reb:3 ~ast:5 ~stl:1 ~blk:0 (); (* Break *)
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:21 ~reb:5 ~ast:2 ~stl:1 ~blk:0 ();
  ] in
  let all_streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  let active = Wkbl.Streaks.get_active_streaks all_streaks in
  (* Only the most recent 2-game streak should be active *)
  let active_pts = List.filter (fun (s: player_streak) -> s.ps_streak_type = Points20Plus) active in
  Alcotest.(check bool) "has active pts streak" true (List.length active_pts > 0);
  let active_one = List.hd active_pts in
  Alcotest.(check int) "active 2-game" 2 active_one.ps_current_count;
  Alcotest.(check bool) "is active" true active_one.ps_is_active

let test_streak_minimum_length () =
  (* Streaks require minimum 2 games *)
  let games = [
    make_streak_game ~game_id:"G2" ~game_date:"2024-01-06" ~pts:22 ~reb:5 ~ast:3 ~stl:1 ~blk:0 ();
    make_streak_game ~game_id:"G1" ~game_date:"2024-01-03" ~pts:10 ~reb:4 ~ast:2 ~stl:1 ~blk:0 ();  (* Not 20+ *)
  ] in
  let streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  (* Single game with 20+ pts should not create a streak (min 2 required) *)
  let pts_streaks = List.filter (fun (s: player_streak) -> s.ps_streak_type = Points20Plus) streaks in
  Alcotest.(check int) "no streak for single game" 0 (List.length pts_streaks)

let test_streak_empty_games () =
  (* Empty games list should return no streaks *)
  let games = [] in
  let streaks = Wkbl.Streaks.analyze_player_streaks
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    games
  in
  Alcotest.(check int) "no streaks for empty games" 0 (List.length streaks)

let test_make_player_streak_empty_defensive () =
  let streak =
    Wkbl.Streaks.make_player_streak
      ~player_id:"P001"
      ~player_name:"Test Player"
      ~team_name:"Test Team"
      ~streak_type:Points20Plus
      []
  in
  Alcotest.(check int) "empty streak count" 0 streak.ps_current_count;
  Alcotest.(check string) "empty streak start_date" "" streak.ps_start_date;
  Alcotest.(check (option string)) "empty streak end_date" None streak.ps_end_date;
  Alcotest.(check int) "empty streak games" 0 (List.length streak.ps_games)

let test_streak_type_labels () =
  Alcotest.(check string) "WinStreak" "win_streak" (streak_type_to_string WinStreak);
  Alcotest.(check string) "Points20Plus" "pts_20plus" (streak_type_to_string Points20Plus);
  Alcotest.(check string) "DoubleDouble" "double_double" (streak_type_to_string DoubleDouble);
  Alcotest.(check string) "TripleDouble" "triple_double" (streak_type_to_string TripleDouble);
  Alcotest.(check string) "Rebounds10Plus" "reb_10plus" (streak_type_to_string Rebounds10Plus);
  Alcotest.(check string) "Assists7Plus" "ast_7plus" (streak_type_to_string Assists7Plus);
  Alcotest.(check string) "Blocks3Plus" "blk_3plus" (streak_type_to_string Blocks3Plus);
  Alcotest.(check string) "Steals3Plus" "stl_3plus" (streak_type_to_string Steals3Plus)

let streaks_tests = [
  Alcotest.test_case "20+ points streak" `Quick test_streak_20plus_points;
  Alcotest.test_case "Broken streak" `Quick test_streak_broken;
  Alcotest.test_case "Double-double streak" `Quick test_streak_double_double;
  Alcotest.test_case "10+ rebounds streak" `Quick test_streak_10plus_rebounds;
  Alcotest.test_case "Get best streaks" `Quick test_streak_get_best;
  Alcotest.test_case "Get active streaks" `Quick test_streak_get_active;
  Alcotest.test_case "Minimum streak length" `Quick test_streak_minimum_length;
  Alcotest.test_case "Empty games list" `Quick test_streak_empty_games;
  Alcotest.test_case "Empty streak defensive" `Quick test_make_player_streak_empty_defensive;
  Alcotest.test_case "Streak type labels" `Quick test_streak_type_labels;
]

(* ============================================= *)
(* Clutch Time Tests                             *)
(* ============================================= *)

let test_parse_clock_normal () =
  Alcotest.(check int) "5:00 = 300 seconds" 300 (parse_clock "5:00");
  Alcotest.(check int) "4:30 = 270 seconds" 270 (parse_clock "4:30");
  Alcotest.(check int) "0:30 = 30 seconds" 30 (parse_clock "0:30");
  Alcotest.(check int) "10:00 = 600 seconds" 600 (parse_clock "10:00")

let test_parse_clock_edge_cases () =
  Alcotest.(check int) "0:00 = 0 seconds" 0 (parse_clock "0:00");
  Alcotest.(check int) "invalid = 0" 0 (parse_clock "invalid");
  Alcotest.(check int) "empty = 0" 0 (parse_clock "")

let test_is_clutch_time_true () =
  (* Q4, 4:30 remaining, score diff = 3 -> clutch *)
  Alcotest.(check bool) "Q4 4:30 diff=3" true
    (is_clutch_time ~period_code:"Q4" ~clock:"4:30" ~score_diff:3);
  (* Q4, 0:30 remaining, score diff = 5 -> clutch *)
  Alcotest.(check bool) "Q4 0:30 diff=5" true
    (is_clutch_time ~period_code:"Q4" ~clock:"0:30" ~score_diff:5);
  (* Q4, 5:00 remaining, score diff = 0 -> clutch *)
  Alcotest.(check bool) "Q4 5:00 diff=0" true
    (is_clutch_time ~period_code:"Q4" ~clock:"5:00" ~score_diff:0);
  (* Q4, negative score diff within range -> clutch *)
  Alcotest.(check bool) "Q4 3:00 diff=-4" true
    (is_clutch_time ~period_code:"Q4" ~clock:"3:00" ~score_diff:(-4))

let test_is_clutch_time_false () =
  (* Q3 is not clutch time *)
  Alcotest.(check bool) "Q3 not clutch" false
    (is_clutch_time ~period_code:"Q3" ~clock:"4:00" ~score_diff:2);
  (* Q4 but more than 5 minutes remaining *)
  Alcotest.(check bool) "Q4 6:00 not clutch" false
    (is_clutch_time ~period_code:"Q4" ~clock:"6:00" ~score_diff:3);
  (* Q4 but score diff > 5 *)
  Alcotest.(check bool) "Q4 diff=6 not clutch" false
    (is_clutch_time ~period_code:"Q4" ~clock:"4:00" ~score_diff:6);
  (* Q4 but score diff < -5 *)
  Alcotest.(check bool) "Q4 diff=-6 not clutch" false
    (is_clutch_time ~period_code:"Q4" ~clock:"4:00" ~score_diff:(-6))

let test_extract_clutch_events () =
  let events = [
    { pe_period_code = "Q4"; pe_event_index = 1; pe_team_side = 1;
      pe_description = "2점 성공"; pe_team1_score = Some 70; pe_team2_score = Some 68;
      pe_clock = "4:00" };
    { pe_period_code = "Q4"; pe_event_index = 2; pe_team_side = 2;
      pe_description = "3점 성공"; pe_team1_score = Some 70; pe_team2_score = Some 71;
      pe_clock = "3:30" };
    { pe_period_code = "Q4"; pe_event_index = 3; pe_team_side = 1;
      pe_description = "2점 실패"; pe_team1_score = Some 70; pe_team2_score = Some 71;
      pe_clock = "6:00" };  (* Not clutch - too early *)
    { pe_period_code = "Q3"; pe_event_index = 4; pe_team_side = 1;
      pe_description = "2점 성공"; pe_team1_score = Some 50; pe_team2_score = Some 52;
      pe_clock = "4:00" };  (* Not clutch - wrong period *)
    { pe_period_code = "Q4"; pe_event_index = 5; pe_team_side = 2;
      pe_description = "자유투 성공"; pe_team1_score = Some 70; pe_team2_score = Some 80;
      pe_clock = "2:00" };  (* Not clutch - score diff > 5 *)
  ] in
  let clutch = extract_clutch_events events in
  Alcotest.(check int) "2 clutch events" 2 (List.length clutch);
  Alcotest.(check int) "first clutch event index" 1 (List.hd clutch).pe_event_index;
  Alcotest.(check int) "second clutch event index" 2 (List.nth clutch 1).pe_event_index

let test_extract_clutch_events_empty () =
  let events = [
    { pe_period_code = "Q1"; pe_event_index = 1; pe_team_side = 1;
      pe_description = "2점 성공"; pe_team1_score = Some 10; pe_team2_score = Some 8;
      pe_clock = "8:00" };
  ] in
  let clutch = extract_clutch_events events in
  Alcotest.(check int) "no clutch events" 0 (List.length clutch)

let test_extract_clutch_events_no_score () =
  let events = [
    { pe_period_code = "Q4"; pe_event_index = 1; pe_team_side = 1;
      pe_description = "2점 성공"; pe_team1_score = None; pe_team2_score = None;
      pe_clock = "4:00" };
  ] in
  let clutch = extract_clutch_events events in
  Alcotest.(check int) "no clutch (no score)" 0 (List.length clutch)

let clutch_tests = [
  Alcotest.test_case "Parse clock normal" `Quick test_parse_clock_normal;
  Alcotest.test_case "Parse clock edge cases" `Quick test_parse_clock_edge_cases;
  Alcotest.test_case "Is clutch time (true cases)" `Quick test_is_clutch_time_true;
  Alcotest.test_case "Is clutch time (false cases)" `Quick test_is_clutch_time_false;
  Alcotest.test_case "Extract clutch events" `Quick test_extract_clutch_events;
  Alcotest.test_case "Extract clutch events (empty)" `Quick test_extract_clutch_events_empty;
  Alcotest.test_case "Extract clutch events (no score)" `Quick test_extract_clutch_events_no_score;
]

(* ============================================= *)
(* Score Flow Tests                              *)
(* ============================================= *)

let make_pbp_event ~period_code ~clock ~home_score ~away_score =
  { pe_period_code = period_code;
    pe_event_index = 0;
    pe_team_side = 1;
    pe_description = "Test event";
    pe_team1_score = home_score;
    pe_team2_score = away_score;
    pe_clock = clock }

let test_period_to_number () =
  Alcotest.(check int) "Q1 -> 1" 1 (period_to_number "Q1");
  Alcotest.(check int) "Q2 -> 2" 2 (period_to_number "Q2");
  Alcotest.(check int) "Q3 -> 3" 3 (period_to_number "Q3");
  Alcotest.(check int) "Q4 -> 4" 4 (period_to_number "Q4");
  Alcotest.(check int) "X1 -> 5" 5 (period_to_number "X1");
  Alcotest.(check int) "X2 -> 6" 6 (period_to_number "X2");
  Alcotest.(check int) "unknown -> 0" 0 (period_to_number "unknown")

let test_parse_clock_to_seconds () =
  Alcotest.(check int) "10:00 = 600" 600 (parse_clock_to_seconds "10:00");
  Alcotest.(check int) "05:30 = 330" 330 (parse_clock_to_seconds "05:30");
  Alcotest.(check int) "00:15 = 15" 15 (parse_clock_to_seconds "00:15");
  Alcotest.(check int) "invalid = 600" 600 (parse_clock_to_seconds "invalid")

let test_calculate_elapsed_seconds () =
  (* Q1 at 10:00 -> 0 seconds elapsed *)
  Alcotest.(check int) "Q1 10:00 -> 0" 0
    (calculate_elapsed_seconds ~period_code:"Q1" ~clock:"10:00");
  (* Q1 at 5:00 -> 300 seconds elapsed *)
  Alcotest.(check int) "Q1 05:00 -> 300" 300
    (calculate_elapsed_seconds ~period_code:"Q1" ~clock:"05:00");
  (* Q2 at 10:00 -> 600 seconds elapsed (Q1 complete) *)
  Alcotest.(check int) "Q2 10:00 -> 600" 600
    (calculate_elapsed_seconds ~period_code:"Q2" ~clock:"10:00");
  (* Q4 at 0:00 -> 2400 seconds elapsed (game over) *)
  Alcotest.(check int) "Q4 00:00 -> 2400" 2400
    (calculate_elapsed_seconds ~period_code:"Q4" ~clock:"00:00");
  (* OT1 at 5:00 -> 2400 seconds (4 quarters complete) *)
  Alcotest.(check int) "X1 05:00 -> 2400" 2400
    (calculate_elapsed_seconds ~period_code:"X1" ~clock:"05:00")

let test_extract_score_flow_basic () =
  let events = [
    make_pbp_event ~period_code:"Q1" ~clock:"09:30" ~home_score:(Some 2) ~away_score:(Some 0);
    make_pbp_event ~period_code:"Q1" ~clock:"08:00" ~home_score:(Some 2) ~away_score:(Some 3);
    make_pbp_event ~period_code:"Q1" ~clock:"06:00" ~home_score:(Some 5) ~away_score:(Some 3);
  ] in
  let flow = extract_score_flow events in
  (* Should have starting point + 3 score changes *)
  Alcotest.(check int) "4 flow points (start + 3 changes)" 4 (List.length flow);
  (* First point should be 0-0 at start *)
  let first = List.hd flow in
  Alcotest.(check int) "First home score = 0" 0 first.sfp_home_score;
  Alcotest.(check int) "First away score = 0" 0 first.sfp_away_score;
  Alcotest.(check int) "First diff = 0" 0 first.sfp_diff

let test_extract_score_flow_empty () =
  let flow = extract_score_flow [] in
  Alcotest.(check int) "Empty events -> empty flow" 0 (List.length flow)

let test_extract_score_flow_no_scores () =
  let events = [
    make_pbp_event ~period_code:"Q1" ~clock:"09:30" ~home_score:None ~away_score:None;
    make_pbp_event ~period_code:"Q1" ~clock:"08:00" ~home_score:(Some 2) ~away_score:None;
  ] in
  let flow = extract_score_flow events in
  Alcotest.(check int) "No complete scores -> empty flow" 0 (List.length flow)

let test_extract_score_flow_dedup () =
  let events = [
    make_pbp_event ~period_code:"Q1" ~clock:"09:30" ~home_score:(Some 2) ~away_score:(Some 0);
    make_pbp_event ~period_code:"Q1" ~clock:"09:00" ~home_score:(Some 2) ~away_score:(Some 0);  (* Same score, should be deduped *)
    make_pbp_event ~period_code:"Q1" ~clock:"08:00" ~home_score:(Some 4) ~away_score:(Some 0);
  ] in
  let flow = extract_score_flow events in
  (* Should have: start (0-0), first 2-0, and 4-0 (deduped the duplicate 2-0) *)
  Alcotest.(check int) "Deduped flow = 3 points" 3 (List.length flow)

let test_game_flow_chart_empty () =
  let html = Wkbl.Views_tools.game_flow_chart ~home_team:"A" ~away_team:"B" [] in
  Alcotest.(check bool) "empty placeholder" true (contains_substring html "No score flow data available")

let test_game_flow_chart_single_point () =
  let pt : Wkbl.Domain.score_flow_point = {
    sfp_clock = "10:00";
    sfp_period = "Q1";
    sfp_home_score = 0;
    sfp_away_score = 0;
    sfp_diff = 0;
    sfp_elapsed_seconds = 0;
  } in
  let html = Wkbl.Views_tools.game_flow_chart ~home_team:"A" ~away_team:"B" [pt] in
  Alcotest.(check bool) "renders svg" true (contains_substring html "<svg");
  Alcotest.(check bool) "has header" true (contains_substring html "Game Flow")

let score_flow_tests = [
  Alcotest.test_case "Period to number conversion" `Quick test_period_to_number;
  Alcotest.test_case "Parse clock to seconds" `Quick test_parse_clock_to_seconds;
  Alcotest.test_case "Calculate elapsed seconds" `Quick test_calculate_elapsed_seconds;
  Alcotest.test_case "Extract score flow basic" `Quick test_extract_score_flow_basic;
  Alcotest.test_case "Extract score flow empty" `Quick test_extract_score_flow_empty;
  Alcotest.test_case "Extract score flow no scores" `Quick test_extract_score_flow_no_scores;
  Alcotest.test_case "Extract score flow dedup" `Quick test_extract_score_flow_dedup;
]

let views_tools_tests = [
  Alcotest.test_case "game_flow_chart empty" `Quick test_game_flow_chart_empty;
  Alcotest.test_case "game_flow_chart single point" `Quick test_game_flow_chart_single_point;
]

(* ============================================= *)
(* Lineup Chemistry Tests                        *)
(* ============================================= *)

let test_make_lineup_key () =
  (* Test that lineup key is created correctly with sorted IDs *)
  let key1 = make_lineup_key ["p3"; "p1"; "p5"; "p2"; "p4"] in
  let key2 = make_lineup_key ["p1"; "p2"; "p3"; "p4"; "p5"] in
  Alcotest.(check string) "Keys should be sorted and equal" key1 key2;
  Alcotest.(check string) "Key format" "p1,p2,p3,p4,p5" key1

let test_make_lineup_key_single () =
  let key = make_lineup_key ["player1"] in
  Alcotest.(check string) "Single player key" "player1" key

let test_calculate_synergy_score_basic () =
  (* Positive avg_plus_minus should give positive score *)
  let score = calculate_synergy_score
    ~games_together:10 ~total_minutes:100.0 ~avg_plus_minus:5.0 in
  Alcotest.(check bool) "Positive synergy" true (score > 0.0);
  Alcotest.(check bool) "Score with max weights" true (score >= 4.9 && score <= 5.1)

let test_calculate_synergy_score_insufficient_data () =
  (* Less than 2 games should return 0 *)
  let score1 = calculate_synergy_score
    ~games_together:1 ~total_minutes:100.0 ~avg_plus_minus:5.0 in
  Alcotest.(check (float 0.01)) "1 game = 0 synergy" 0.0 score1;
  (* Less than 10 minutes should return 0 *)
  let score2 = calculate_synergy_score
    ~games_together:10 ~total_minutes:5.0 ~avg_plus_minus:5.0 in
  Alcotest.(check (float 0.01)) "5 min = 0 synergy" 0.0 score2

let test_calculate_synergy_score_negative () =
  (* Negative avg_plus_minus should give negative score *)
  let score = calculate_synergy_score
    ~games_together:10 ~total_minutes:100.0 ~avg_plus_minus:(-3.0) in
  Alcotest.(check bool) "Negative synergy" true (score < 0.0)

let test_compare_lineup_stats_by_minutes () =
  let l1 : lineup_stats = {
    ls_players = []; ls_team_name = "A"; ls_games_together = 1;
    ls_total_minutes = 100.0; ls_total_pts = 50; ls_total_opp_pts = 40;
    ls_plus_minus = 10; ls_avg_pts_per_min = 0.5; ls_avg_margin_per_min = 0.1;
  } in
  let l2 : lineup_stats = { l1 with ls_total_minutes = 200.0 } in
  (* l2 should come first (more minutes) *)
  let cmp = compare_lineup_stats LineupByMinutes l1 l2 in
  Alcotest.(check bool) "l2 has more minutes" true (cmp > 0)

let test_compare_lineup_stats_by_plus_minus () =
  let l1 : lineup_stats = {
    ls_players = []; ls_team_name = "A"; ls_games_together = 1;
    ls_total_minutes = 100.0; ls_total_pts = 50; ls_total_opp_pts = 40;
    ls_plus_minus = 10; ls_avg_pts_per_min = 0.5; ls_avg_margin_per_min = 0.1;
  } in
  let l2 : lineup_stats = { l1 with ls_plus_minus = 20 } in
  (* l2 should come first (higher +/-) *)
  let cmp = compare_lineup_stats LineupByPlusMinus l1 l2 in
  Alcotest.(check bool) "l2 has higher +/-" true (cmp > 0)

let test_lineup_sort_of_string () =
  Alcotest.(check bool) "min -> LineupByMinutes"
    true (lineup_sort_of_string "min" = LineupByMinutes);
  Alcotest.(check bool) "minutes -> LineupByMinutes"
    true (lineup_sort_of_string "minutes" = LineupByMinutes);
  Alcotest.(check bool) "pm -> LineupByPlusMinus"
    true (lineup_sort_of_string "pm" = LineupByPlusMinus);
  Alcotest.(check bool) "eff -> LineupByEfficiency"
    true (lineup_sort_of_string "eff" = LineupByEfficiency);
  Alcotest.(check bool) "unknown -> LineupByFrequency"
    true (lineup_sort_of_string "unknown" = LineupByFrequency)

let test_empty_lineup_stats () =
  let empty = empty_lineup_stats ~team_name:"Test Team" in
  Alcotest.(check string) "Team name set" "Test Team" empty.ls_team_name;
  Alcotest.(check int) "Games = 0" 0 empty.ls_games_together;
  Alcotest.(check (float 0.01)) "Minutes = 0" 0.0 empty.ls_total_minutes;
  Alcotest.(check int) "Pts = 0" 0 empty.ls_total_pts;
  Alcotest.(check int) "+/- = 0" 0 empty.ls_plus_minus;
  Alcotest.(check (list pass)) "Players empty" [] empty.ls_players

let lineup_chemistry_tests = [
  Alcotest.test_case "Make lineup key (sorted)" `Quick test_make_lineup_key;
  Alcotest.test_case "Make lineup key (single)" `Quick test_make_lineup_key_single;
  Alcotest.test_case "Calculate synergy score (basic)" `Quick test_calculate_synergy_score_basic;
  Alcotest.test_case "Calculate synergy score (insufficient)" `Quick test_calculate_synergy_score_insufficient_data;
  Alcotest.test_case "Calculate synergy score (negative)" `Quick test_calculate_synergy_score_negative;
  Alcotest.test_case "Compare lineup stats (by minutes)" `Quick test_compare_lineup_stats_by_minutes;
  Alcotest.test_case "Compare lineup stats (by +/-)" `Quick test_compare_lineup_stats_by_plus_minus;
  Alcotest.test_case "Lineup sort of string" `Quick test_lineup_sort_of_string;
  Alcotest.test_case "Empty lineup stats" `Quick test_empty_lineup_stats;
]

(* ============================================= *)
(* On/Off Impact Tests                           *)
(* ============================================= *)

let test_calculate_net_rating_basic () =
  (* Team scores 110, opponent scores 100, 100 possessions -> +10.0 net rating *)
  let net = calculate_net_rating ~team_pts:110 ~opp_pts:100 ~possessions:100.0 in
  Alcotest.(check float_testable) "Net rating +10" 10.0 net

let test_calculate_net_rating_negative () =
  (* Team scores 90, opponent scores 100, 100 possessions -> -10.0 net rating *)
  let net = calculate_net_rating ~team_pts:90 ~opp_pts:100 ~possessions:100.0 in
  Alcotest.(check float_testable) "Net rating -10" (-10.0) net

let test_calculate_net_rating_zero_possessions () =
  (* Zero possessions should return 0 *)
  let net = calculate_net_rating ~team_pts:110 ~opp_pts:100 ~possessions:0.0 in
  Alcotest.(check float_testable) "Net rating with 0 poss" 0.0 net

let test_calculate_off_rating () =
  (* 110 points on 100 possessions -> 110.0 offensive rating *)
  let off = calculate_off_rating ~team_pts:110 ~possessions:100.0 in
  Alcotest.(check float_testable) "Offensive rating 110" 110.0 off

let test_calculate_def_rating () =
  (* 95 opponent points on 100 possessions -> 95.0 defensive rating *)
  let def = calculate_def_rating ~opp_pts:95 ~possessions:100.0 in
  Alcotest.(check float_testable) "Defensive rating 95" 95.0 def

let test_estimate_possessions () =
  (* FGA=80, FTA=20, TOV=15, OREB=10 -> 80 + 0.44*20 + 15 - 10 = 93.8 *)
  let poss = estimate_possessions ~fga:80 ~fta:20 ~tov:15 ~oreb:10 in
  Alcotest.(check float_testable) "Possessions estimate" 93.8 poss

let test_create_on_off_impact () =
  let impact = create_on_off_impact
    ~player_id:"P001"
    ~player_name:"Test Player"
    ~team_name:"Test Team"
    ~games_played:20
    ~total_minutes:500.0
    ~on_minutes:500.0
    ~on_team_pts:1100
    ~on_opp_pts:1000
    ~on_possessions:1000.0
    ~off_minutes:0.0
    ~off_team_pts:0
    ~off_opp_pts:0
    ~off_possessions:0.0
    ~plus_minus_total:100
  in
  Alcotest.(check string) "Player ID" "P001" impact.ooi_player_id;
  Alcotest.(check string) "Player name" "Test Player" impact.ooi_player_name;
  Alcotest.(check int) "Games played" 20 impact.ooi_games_played;
  Alcotest.(check float_testable) "Total minutes" 500.0 impact.ooi_total_minutes;
  Alcotest.(check int) "Plus/minus total" 100 impact.ooi_plus_minus_total;
  Alcotest.(check float_testable) "Plus/minus avg" 5.0 impact.ooi_plus_minus_avg;
  (* Net rating on: (1100-1000)/1000 * 100 = 10.0 *)
  Alcotest.(check float_testable) "Net rating on" 10.0 impact.ooi_net_rating_on

let test_simple_on_off_impact () =
  let impact = simple_on_off_impact_from_plus_minus
    ~player_id:"P002"
    ~player_name:"Simple Player"
    ~team_name:"Simple Team"
    ~games_played:10
    ~total_minutes:200.0
    ~plus_minus_total:50
  in
  Alcotest.(check string) "Player ID" "P002" impact.ooi_player_id;
  Alcotest.(check int) "Games played" 10 impact.ooi_games_played;
  Alcotest.(check float_testable) "Plus/minus avg" 5.0 impact.ooi_plus_minus_avg;
  (* Simple function sets net ratings to 0 *)
  Alcotest.(check float_testable) "Net rating on" 0.0 impact.ooi_net_rating_on

let test_on_off_impact_zero_games () =
  let impact = create_on_off_impact
    ~player_id:"P003"
    ~player_name:"Zero Player"
    ~team_name:"Zero Team"
    ~games_played:0
    ~total_minutes:0.0
    ~on_minutes:0.0
    ~on_team_pts:0
    ~on_opp_pts:0
    ~on_possessions:0.0
    ~off_minutes:0.0
    ~off_team_pts:0
    ~off_opp_pts:0
    ~off_possessions:0.0
    ~plus_minus_total:0
  in
  Alcotest.(check float_testable) "Zero games -> 0 avg" 0.0 impact.ooi_plus_minus_avg;
  Alcotest.(check int) "On court games" 0 impact.ooi_on_court.ocs_games

let test_on_off_impact_negative_pm () =
  let impact = simple_on_off_impact_from_plus_minus
    ~player_id:"P004"
    ~player_name:"Negative Player"
    ~team_name:"Negative Team"
    ~games_played:10
    ~total_minutes:300.0
    ~plus_minus_total:(-50)
  in
  Alcotest.(check int) "Negative plus/minus" (-50) impact.ooi_plus_minus_total;
  Alcotest.(check float_testable) "Negative avg" (-5.0) impact.ooi_plus_minus_avg

let on_off_impact_tests = [
  Alcotest.test_case "Calculate net rating (positive)" `Quick test_calculate_net_rating_basic;
  Alcotest.test_case "Calculate net rating (negative)" `Quick test_calculate_net_rating_negative;
  Alcotest.test_case "Calculate net rating (zero poss)" `Quick test_calculate_net_rating_zero_possessions;
  Alcotest.test_case "Calculate offensive rating" `Quick test_calculate_off_rating;
  Alcotest.test_case "Calculate defensive rating" `Quick test_calculate_def_rating;
  Alcotest.test_case "Estimate possessions" `Quick test_estimate_possessions;
  Alcotest.test_case "Create on/off impact" `Quick test_create_on_off_impact;
  Alcotest.test_case "Simple on/off from +/-" `Quick test_simple_on_off_impact;
  Alcotest.test_case "On/off impact zero games" `Quick test_on_off_impact_zero_games;
  Alcotest.test_case "On/off impact negative +/-" `Quick test_on_off_impact_negative_pm;
]

(* ============================================= *)
(* Advanced Stats Tests                          *)
(* ============================================= *)

let test_true_shooting_basic () =
  (* Example: 24 pts, 25 FGA, 6 FTA
     TS% = 24 / (2 * (25 + 0.44 * 6)) * 100 = 24 / 55.28 * 100 = 43.415 *)
  let ts = Wkbl.Stats.true_shooting_pct ~pts:24 ~fga:25 ~fta:6 in
  Alcotest.(check float_testable) "TS% basic calc" 43.415 ts

let test_true_shooting_zero () =
  (* Zero FGA and FTA should return 0 *)
  let ts = Wkbl.Stats.true_shooting_pct ~pts:0 ~fga:0 ~fta:0 in
  Alcotest.(check float_testable) "TS% zero" 0.0 ts

let test_true_shooting_elite () =
  (* Elite scorer: 30 pts, 20 FGA, 10 FTA
     TS% = 30 / (2 * (20 + 4.4)) * 100 = 30 / 48.8 * 100 = 61.475 *)
  let ts = Wkbl.Stats.true_shooting_pct ~pts:30 ~fga:20 ~fta:10 in
  Alcotest.(check float_testable) "TS% elite" 61.475 ts

let test_effective_fg_pct () =
  (* 8 FGM, 3 3PM, 15 FGA
     eFG% = (8 + 0.5*3) / 15 * 100 = 9.5 / 15 * 100 = 63.333 *)
  let efg = Wkbl.Stats.effective_fg_pct ~fg_made:8 ~fg3_made:3 ~fga:15 in
  Alcotest.(check float_testable) "eFG% calc" 63.333 efg

let test_usage_rate () =
  (* 15 FGA, 6 FTA, 3 TOV, team_poss=80
     USG% = (15 + 0.44*6 + 3) / 80 * 100 = 20.64 / 80 * 100 = 25.8 *)
  let usg = Wkbl.Stats.usage_rate ~fga:15 ~fta:6 ~tov:3 ~team_poss:80.0 in
  Alcotest.(check float_testable) "Usage rate" 25.8 usg

let test_estimate_possessions () =
  (* FGA=75, FTA=20, TOV=12, OREB=10
     Poss = 75 + 0.44*20 + 12 - 10 = 75 + 8.8 + 12 - 10 = 85.8 *)
  let poss = Wkbl.Stats.estimate_team_possessions ~team_fga:75 ~team_fta:20 ~team_tov:12 ~team_oreb:10 in
  Alcotest.(check float_testable) "Team possessions" 85.8 poss

let advanced_stats_tests = [
  Alcotest.test_case "True Shooting % basic" `Quick test_true_shooting_basic;
  Alcotest.test_case "True Shooting % zero" `Quick test_true_shooting_zero;
  Alcotest.test_case "True Shooting % elite" `Quick test_true_shooting_elite;
  Alcotest.test_case "Effective FG %" `Quick test_effective_fg_pct;
  Alcotest.test_case "Usage Rate" `Quick test_usage_rate;
  Alcotest.test_case "Estimate Possessions" `Quick test_estimate_possessions;
]

(* ============================================= *)
(* Scraper Function Tests                        *)
(* ============================================= *)

let test_code_from_team_name_known () =
  (* Known teams should map correctly *)
  Alcotest.(check string) "KB스타즈 -> 01" "01" (Wkbl.Scraper.code_from_team_name "KB스타즈");
  Alcotest.(check string) "삼성생명 -> 03" "03" (Wkbl.Scraper.code_from_team_name "삼성생명");
  Alcotest.(check string) "우리은행 -> 05" "05" (Wkbl.Scraper.code_from_team_name "우리은행");
  Alcotest.(check string) "신한은행 -> 07" "07" (Wkbl.Scraper.code_from_team_name "신한은행");
  Alcotest.(check string) "하나은행 -> 09" "09" (Wkbl.Scraper.code_from_team_name "하나은행");
  Alcotest.(check string) "BNK썸 -> 11" "11" (Wkbl.Scraper.code_from_team_name "BNK썸")

let test_code_from_team_name_unknown () =
  (* Unknown teams should get fallback XX_ prefix *)
  let result = Wkbl.Scraper.code_from_team_name "알수없는팀" in
  Alcotest.(check bool) "Unknown team starts with XX_" true (String.length result >= 3 && String.sub result 0 3 = "XX_")

let test_code_from_team_name_alternate () =
  (* Alternate names should work *)
  Alcotest.(check string) "KB 스타즈 (space)" "01" (Wkbl.Scraper.code_from_team_name "KB 스타즈");
  Alcotest.(check string) "하나원큐 -> 09" "09" (Wkbl.Scraper.code_from_team_name "하나원큐");
  Alcotest.(check string) "BNK 썸 (space)" "11" (Wkbl.Scraper.code_from_team_name "BNK 썸")

let test_code_from_team_name_extended () =
  (* Modern team variants *)
  Alcotest.(check string) "KDB생명 -> 08" "08" (Wkbl.Scraper.code_from_team_name "KDB생명");
  Alcotest.(check string) "OK저축은행 -> 10" "10" (Wkbl.Scraper.code_from_team_name "OK저축은행");
  (* Domain fallback + alpha->numeric mapping *)
  Alcotest.(check string) "KEB하나은행 -> 09" "09" (Wkbl.Scraper.code_from_team_name "KEB하나은행");
  (* All-star teams should be mapped explicitly (no AS/XX_ placeholder) *)
  Alcotest.(check string) "핑크스타 -> 87" "87" (Wkbl.Scraper.code_from_team_name "핑크스타");
  Alcotest.(check string) "블루스타 -> 88" "88" (Wkbl.Scraper.code_from_team_name "블루스타");
  Alcotest.(check string) "남부선발 -> 91" "91" (Wkbl.Scraper.code_from_team_name "남부선발");
  Alcotest.(check string) "중부선발 -> 92" "92" (Wkbl.Scraper.code_from_team_name "중부선발");
  Alcotest.(check string) "한국 올스타 -> 83" "83" (Wkbl.Scraper.code_from_team_name "한국 올스타");
  Alcotest.(check string) "일본 올스타 -> 84" "84" (Wkbl.Scraper.code_from_team_name "일본 올스타")

let test_normalize_schedule_date_formats () =
  (* ISO passthrough *)
  Alcotest.(check string) "ISO passthrough" "2016-10-29"
    (Wkbl.Scraper.normalize_schedule_date ~season_code:"037" "2016-10-29");
  (* M/D(day) parsing *)
  Alcotest.(check string) "10/29(토)" "2016-10-29"
    (Wkbl.Scraper.normalize_schedule_date ~season_code:"037" "10/29(토)");
  Alcotest.(check string) "3/6(월) -> next year" "2017-03-06"
    (Wkbl.Scraper.normalize_schedule_date ~season_code:"037" "3/6(월)")

let test_get_last_sync_time_str_initial () =
  (* Initial state should show no sync record *)
  let result = Wkbl.Scraper.get_last_sync_time_str () in
  (* Either "동기화 기록 없음" or a time string is valid *)
  Alcotest.(check bool) "Sync time string not empty" true (String.length result > 0)

let test_seasons_catalog_name_of_code () =
  Alcotest.(check string) "046 -> 2025-2026" "2025-2026" (Wkbl.Seasons_catalog.name_of_code "046");
  Alcotest.(check string) "025 -> 2007-2008" "2007-2008" (Wkbl.Seasons_catalog.name_of_code "025");
  Alcotest.(check string) "047 inferred -> 2026-2027" "2026-2027" (Wkbl.Seasons_catalog.name_of_code "047")

let test_seasons_catalog_unique_codes () =
  let module S = Set.Make (String) in
  let codes = Wkbl.Seasons_catalog.all |> List.map fst in
  let uniq = codes |> List.fold_left (fun acc c -> S.add c acc) S.empty |> S.cardinal in
  Alcotest.(check int) "unique season_code" (List.length codes) uniq

let test_game_params_of_href () =
  let href = "/game/result.asp?season_gu=046&gun=1&game_type=01&game_no=40&ym=202601&viewType=2" in
  let (game_type, game_no) = Wkbl.Scraper.game_params_of_href href in
  Alcotest.(check (option string)) "game_type" (Some "01") game_type;
  Alcotest.(check (option int)) "game_no" (Some 40) game_no

let test_game_id_of_params () =
  let gid = Wkbl.Scraper.game_id_of_params ~season_code:"046" ~game_type_opt:(Some "01") ~game_no_opt:(Some 40) in
  Alcotest.(check (option string)) "game_id" (Some "046-01-40") gid

let test_parse_schedule_html_extracts_game_meta () =
  let html = {|
<div class="info_table01 type_list">
  <table>
    <tbody>
      <tr id="20260110">
        <td>1/10(<span class="language" data-kr="토" data-en="Sat">토</span>)</td>
        <td>
          <div class="team_versus">
            <div class="info_team away">
              <strong class="team_name language" data-kr="신한은행" data-en="SHINHAN BANK">신한은행</strong>
              <em class="txt_score">61</em>
            </div>
            <span class="txt_vs">vs</span>
            <div class="info_team home">
              <strong class="team_name language" data-kr="삼성생명" data-en="SAMSUNG LIFE">삼성생명</strong>
              <em class="txt_score">74</em>
            </div>
          </div>
        </td>
        <td class="language" data-kr="용인실내체육관" data-en="Yongin Indoor Gymnasium">용인실내체육관</td>
        <td>14:00</td>
        <td>
          <a href="/game/result.asp?season_gu=046&gun=1&game_type=01&game_no=40&ym=202601&viewType=" class="btn_type1 language">경기기록</a>
        </td>
      </tr>
    </tbody>
  </table>
</div>
|} in
  let entries = Wkbl.Scraper.parse_schedule_html ~season:"046" ~ym:"202601" html in
  match entries with
  | [e] ->
      Alcotest.(check string) "date normalized from row id" "2026-01-10" e.sch_date;
      Alcotest.(check (option string)) "game_id" (Some "046-01-40") e.sch_game_id;
      Alcotest.(check (option string)) "game_type" (Some "01") e.sch_game_type;
      Alcotest.(check (option int)) "game_no" (Some 40) e.sch_game_no
  | _ -> Alcotest.fail "expected exactly one schedule entry"

let test_parse_schedule_api_html_extracts_game_meta () =
  let html = {|
<table><tbody>
  <tr id="20260110">
    <td>1/10(<span class="language">토</span>)</td>
    <td>
      <div class="team_versus">
        <div class="info_team away"><strong class="team_name">신한은행</strong><em class="txt_score">61</em></div>
        <div class="info_team home"><strong class="team_name">삼성생명</strong><em class="txt_score">74</em></div>
      </div>
    </td>
    <td>용인실내체육관</td>
    <td>14:00</td>
    <td><a href="/game/result.asp?season_gu=046&gun=1&game_type=01&game_no=40&ym=202601&viewType=">경기기록</a></td>
  </tr>
</tbody></table>
|} in
  let entries = Wkbl.Scraper.parse_schedule_api_html ~season_code:"046" ~season_name:"2025-2026" html in
  match entries with
  | [e] ->
      Alcotest.(check string) "date normalized from row id" "2026-01-10" e.sch_date;
      Alcotest.(check string) "day extracted" "토" e.sch_day;
      Alcotest.(check (option string)) "game_id" (Some "046-01-40") e.sch_game_id;
      Alcotest.(check (option string)) "game_type" (Some "01") e.sch_game_type;
      Alcotest.(check (option int)) "game_no" (Some 40) e.sch_game_no
  | _ -> Alcotest.fail "expected exactly one schedule entry"

let test_normalize_game_date_formats () =
  let open Wkbl.Scraper in
  let string_opt = Alcotest.option Alcotest.string in
  Alcotest.(check string_opt) "YYYYMMDD" (Some "2026-01-23") (normalize_game_date "20260123");
  Alcotest.(check string_opt) "Dot format" (Some "2026-01-23") (normalize_game_date "2026.01.23");
  Alcotest.(check string_opt) "Dash format" (Some "2026-01-23") (normalize_game_date "2026-01-23");
  Alcotest.(check string_opt) "Slash format" (Some "2026-01-23") (normalize_game_date "2026/01/23");
  Alcotest.(check string_opt) "With text" (Some "2026-01-23") (normalize_game_date "2026.01.23 (금)");
  Alcotest.(check string_opt) "Invalid" None (normalize_game_date "no-date")

let test_parse_boxscore_html_two_teams () =
  let html = {|
<div class="info_table01 type_record">
  <table><tbody>
    <tr>
      <td><a href="/player/detail.asp?pno=095633">신이슬</a></td>
      <td>G</td>
      <td>27:10</td>
      <td>0-3</td>
      <td>0-2</td>
      <td>1-2</td>
      <td>2</td>
      <td>1</td>
      <td>3</td>
      <td>1</td>
      <td>3</td>
      <td>1</td>
      <td>3</td>
      <td>1</td>
      <td>1</td>
    </tr>
  </tbody></table>
</div>
<div class="info_table01 type_record">
  <table><tbody>
    <tr>
      <td><a href="/player/detail.asp?pno=095499">진안</a></td>
      <td>C</td>
      <td>32:00</td>
      <td>5-10</td>
      <td>0-0</td>
      <td>2-4</td>
      <td>1</td>
      <td>5</td>
      <td>6</td>
      <td>2</td>
      <td>1</td>
      <td>0</td>
      <td>1</td>
      <td>0</td>
      <td>12</td>
    </tr>
  </tbody></table>
</div>
|} in
  match Wkbl.Scraper.parse_boxscore_html html with
  | [away; home] ->
      (match away with
      | [p] ->
          Alcotest.(check string) "away player_id" "095633" p.bs_player_id;
          Alcotest.(check string) "away player_name" "신이슬" p.bs_player_name;
          Alcotest.(check int) "away min_seconds" ((27 * 60) + 10) p.bs_min_seconds;
          Alcotest.(check int) "away fg2 made" 0 p.bs_fg2_m;
          Alcotest.(check int) "away fg2 att" 3 p.bs_fg2_a;
          Alcotest.(check int) "away fg3 made" 0 p.bs_fg3_m;
          Alcotest.(check int) "away fg3 att" 2 p.bs_fg3_a;
          Alcotest.(check int) "away ft made" 1 p.bs_ft_m;
          Alcotest.(check int) "away ft att" 2 p.bs_ft_a;
          Alcotest.(check int) "away off" 2 p.bs_off_reb;
          Alcotest.(check int) "away def" 1 p.bs_def_reb;
          Alcotest.(check int) "away tot" 3 p.bs_tot_reb;
          Alcotest.(check int) "away ast" 1 p.bs_ast;
          Alcotest.(check int) "away pf" 3 p.bs_pf;
          Alcotest.(check int) "away stl" 1 p.bs_stl;
          Alcotest.(check int) "away tov" 3 p.bs_tov;
          Alcotest.(check int) "away blk" 1 p.bs_blk;
          Alcotest.(check int) "away pts" 1 p.bs_pts
      | _ -> Alcotest.fail "expected exactly one away player row");
      (match home with
      | [p] ->
          Alcotest.(check string) "home player_id" "095499" p.bs_player_id;
          Alcotest.(check string) "home player_name" "진안" p.bs_player_name;
          Alcotest.(check int) "home min_seconds" (32 * 60) p.bs_min_seconds;
          Alcotest.(check int) "home fg2 made" 5 p.bs_fg2_m;
          Alcotest.(check int) "home fg2 att" 10 p.bs_fg2_a;
          Alcotest.(check int) "home ft att" 4 p.bs_ft_a;
          Alcotest.(check int) "home tot" 6 p.bs_tot_reb;
          Alcotest.(check int) "home pts" 12 p.bs_pts
      | _ -> Alcotest.fail "expected exactly one home player row")
  | _ -> Alcotest.fail "expected exactly two team tables"

let test_schedule_sync_success_policy () =
  let open Wkbl.Scraper in
  Alcotest.(check bool)
    "0 schedule, 0 games, 0 errors is not success"
    false
    (schedule_sync_success ~schedule_synced:0 ~games_upserted:0 ~errors:0);
  Alcotest.(check bool)
    "schedule > 0 and no errors is success"
    true
    (schedule_sync_success ~schedule_synced:1 ~games_upserted:0 ~errors:0);
  Alcotest.(check bool)
    "games > 0 and no errors is success"
    true
    (schedule_sync_success ~schedule_synced:0 ~games_upserted:1 ~errors:0);
  Alcotest.(check bool)
    "any errors makes it failure"
    false
    (schedule_sync_success ~schedule_synced:1 ~games_upserted:1 ~errors:1)

let test_schedule_sync_suspicion_reason_policy () =
  let open Wkbl.Scraper in
  let string_opt = Alcotest.option Alcotest.string in
  Alcotest.(check string_opt)
    "not enforced -> none"
    None
    (schedule_sync_suspicion_reason_v
       ~enforce:false
       ~current_ym:"2026-02"
       ~threshold_old:"2026-01-01"
       ~dates:[]
       ~completed_dates:[]);
  Alcotest.(check bool)
    "missing current month -> some"
    true
    (schedule_sync_suspicion_reason_v
       ~enforce:true
       ~current_ym:"2026-02"
       ~threshold_old:"2026-01-01"
       ~dates:["2026-01-31"; "2026-03-01"]
       ~completed_dates:["2026-01-31"]
     <> None);
  Alcotest.(check bool)
    "stale completed -> some"
    true
    (schedule_sync_suspicion_reason_v
       ~enforce:true
       ~current_ym:"2026-02"
       ~threshold_old:"2026-01-20"
       ~dates:["2026-02-01"; "2026-02-02"]
       ~completed_dates:["2026-01-10"]
     <> None);
  Alcotest.(check string_opt)
    "healthy -> none"
    None
    (schedule_sync_suspicion_reason_v
       ~enforce:true
       ~current_ym:"2026-02"
       ~threshold_old:"2026-01-20"
       ~dates:["2026-02-01"; "2026-02-02"]
       ~completed_dates:["2026-02-01"])

let scraper_tests = [
  Alcotest.test_case "code_from_team_name known" `Quick test_code_from_team_name_known;
  Alcotest.test_case "code_from_team_name unknown" `Quick test_code_from_team_name_unknown;
  Alcotest.test_case "code_from_team_name alternate" `Quick test_code_from_team_name_alternate;
  Alcotest.test_case "code_from_team_name extended" `Quick test_code_from_team_name_extended;
  Alcotest.test_case "normalize_schedule_date formats" `Quick test_normalize_schedule_date_formats;
  Alcotest.test_case "get_last_sync_time_str" `Quick test_get_last_sync_time_str_initial;
  Alcotest.test_case "seasons_catalog name_of_code" `Quick test_seasons_catalog_name_of_code;
  Alcotest.test_case "seasons_catalog unique codes" `Quick test_seasons_catalog_unique_codes;
  Alcotest.test_case "schedule sync success policy" `Quick test_schedule_sync_success_policy;
  Alcotest.test_case "schedule sync suspicion policy" `Quick test_schedule_sync_suspicion_reason_policy;
  Alcotest.test_case "game_params_of_href" `Quick test_game_params_of_href;
  Alcotest.test_case "game_id_of_params" `Quick test_game_id_of_params;
  Alcotest.test_case "parse_schedule_html extracts game meta" `Quick test_parse_schedule_html_extracts_game_meta;
  Alcotest.test_case "parse_schedule_api_html extracts game meta" `Quick test_parse_schedule_api_html_extracts_game_meta;
  Alcotest.test_case "normalize_game_date formats" `Quick test_normalize_game_date_formats;
  Alcotest.test_case "parse_boxscore_html extracts two teams" `Quick test_parse_boxscore_html_two_teams;
]

(* ============================================= *)
(* Disambiguation Line Tests                     *)
(* ============================================= *)

let test_player_disambiguation_line () =
  let info : Wkbl.Domain.player_info = {
    id = "P0001";
    name = "Test";
    position = Some "G";
    birth_date = Some "1999-01-02";
    height = Some 175;
    weight = None;
  } in
  let html = Wkbl.Views_common.player_disambiguation_line ~team_name:"우리은행" ~player_id:"P0001" (Some info) in
  Alcotest.(check bool) "contains team" true (contains_substring html "우리은행");
  Alcotest.(check bool) "contains position" true (contains_substring html "G");
  Alcotest.(check bool) "contains year" true (contains_substring html "1999");
  Alcotest.(check bool) "contains height" true (contains_substring html "175cm");
  Alcotest.(check bool) "contains id" true (contains_substring html "고유번호 P0001")

let test_leader_card_disambiguation () =
  let leaders : Wkbl.Domain.leader_entry list = [
    { le_player_id = "P1"; le_player_name = "Kim"; le_team_name = "우리은행"; le_stat_value = 10.0 };
    { le_player_id = "P2"; le_player_name = "Kim"; le_team_name = "KB스타즈"; le_stat_value = 9.0 };
  ] in
  let info1 : Wkbl.Domain.player_info = {
    id = "P1";
    name = "Kim";
    position = Some "G";
    birth_date = Some "1990-01-01";
    height = Some 180;
    weight = None;
  } in
  let map = Hashtbl.create 4 in
  Hashtbl.replace map info1.id info1;
  let html = Wkbl.Views.leader_card ~player_info_map:(Some map) "Test" leaders in
  Alcotest.(check bool) "contains leader id P1" true (contains_substring html "고유번호 P1");
  Alcotest.(check bool) "contains leader id P2" true (contains_substring html "고유번호 P2");
  Alcotest.(check bool) "contains leader year" true (contains_substring html "1990");
  Alcotest.(check bool) "contains leader height" true (contains_substring html "180cm")

let disambiguation_tests = [
  Alcotest.test_case "player disambiguation line" `Quick test_player_disambiguation_line;
  Alcotest.test_case "leader card disambiguation" `Quick test_leader_card_disambiguation;
]

(* ============================================= *)
(* QA Utility Tests                              *)
(* ============================================= *)

let test_coverage_pct () =
  Alcotest.(check float_testable) "zero total" 0.0 (Wkbl.Db_common.coverage_pct ~total:0 ~covered:10);
  Alcotest.(check float_testable) "half" 50.0 (Wkbl.Db_common.coverage_pct ~total:10 ~covered:5);
  Alcotest.(check float_testable) "rounding" 33.3 (Wkbl.Db_common.coverage_pct ~total:3 ~covered:1)

let test_split_csv_ids () =
  let list_string = Alcotest.(list string) in
  Alcotest.(check list_string) "basic" ["a"; "b"; "c"] (Wkbl.Db.split_csv_ids "a, b, c");
  Alcotest.(check list_string) "empty" [] (Wkbl.Db.split_csv_ids "");
  Alcotest.(check list_string) "trim + skip empty" ["a"; "b"] (Wkbl.Db.split_csv_ids "a,, ,b,")

let test_qa_dashboard_schedule_coverage () =
  let report : Wkbl.Db.qa_db_report = {
    qdr_generated_at = "2026-02-06T00:00:00Z";
    qdr_games_total = 0;
    qdr_games_with_stats = 0;
    qdr_plus_minus_games = 0;
    qdr_plus_minus_coverage_pct = 0.0;
    qdr_schedule_total = 0;
    qdr_schedule_completed = 0;
    qdr_schedule_missing_game_count = 0;
    qdr_schedule_missing_game_pct = 0.0;
    qdr_schedule_missing_game_sample = [];
    qdr_schedule_missing_stats_count = 0;
    qdr_schedule_missing_stats_pct = 0.0;
    qdr_schedule_missing_stats_sample = [];
    qdr_schedule_coverage = [
      { qsc_season_code = "001";
        qsc_schedule_completed = 20;
        qsc_games_total = 0;
        qsc_matched = 0;
        qsc_missing = 20;
        qsc_coverage_pct = 0.0;
        qsc_season_uningested = true;
        qsc_games_missing_team = false;
      }
    ];
    qdr_score_mismatch_count = 0;
    qdr_score_mismatch_sample = [];
    qdr_team_count_anomaly_count = 0;
    qdr_team_count_anomaly_sample = [];
    qdr_duplicate_player_row_count = 0;
    qdr_duplicate_player_row_sample = [];
    qdr_duplicate_player_name_count = 0;
    qdr_duplicate_player_name_sample = [];
    qdr_duplicate_player_identity_count = 0;
    qdr_duplicate_player_identity_sample = [];
  } in
  let html = Wkbl.Views_tools.qa_dashboard_page report () in
  Alcotest.(check bool) "contains schedule coverage header" true (contains_substring html "시즌별 일정-경기 매칭");
  Alcotest.(check bool) "contains no games flag" true (contains_substring html "경기 없음")

let qa_util_tests = [
  Alcotest.test_case "coverage pct" `Quick test_coverage_pct;
  Alcotest.test_case "split csv ids" `Quick test_split_csv_ids;
  Alcotest.test_case "qa schedule coverage block" `Quick test_qa_dashboard_schedule_coverage;
]

(* ============================================= *)
(* Observability Scripts Tests                   *)
(* ============================================= *)

let with_env name value f =
  let prev = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f

let test_layout_observability_off () =
  let html =
    with_env "SENTRY_DSN" "" (fun () ->
      with_env "SENTRY_ENVIRONMENT" "" (fun () ->
        with_env "CLARITY_PROJECT_ID" "" (fun () ->
          Wkbl.Views_common.layout
            ~title:"WKBL"
            ~content:{html|<main id="main-content">hi</main>|html}
            ())))
  in
  Alcotest.(check bool) "no sentry script" false (contains_substring html "js.sentry-cdn.com");
  Alcotest.(check bool) "no clarity script" false (contains_substring html "clarity.ms/tag/")

let test_layout_observability_on () =
  let dsn = "https://PUBLICKEY@o0.ingest.sentry.io/0" in
  let html =
    with_env "SENTRY_DSN" dsn (fun () ->
      with_env "SENTRY_ENVIRONMENT" "production" (fun () ->
        with_env "CLARITY_PROJECT_ID" "clarity123" (fun () ->
          Wkbl.Views_common.layout
            ~title:"WKBL"
            ~content:{html|<main id="main-content">hi</main>|html}
            ())))
  in
  Alcotest.(check bool) "sentry loader uses public key" true (contains_substring html "js.sentry-cdn.com/PUBLICKEY.min.js");
  Alcotest.(check bool) "sentry init includes DSN" true (contains_substring html dsn);
  Alcotest.(check bool) "sentry init includes env" true (contains_substring html {|environment: "production"|});
  Alcotest.(check bool) "clarity snippet base url" true (contains_substring html "clarity.ms/tag/");
  Alcotest.(check bool) "clarity snippet includes id" true (contains_substring html "clarity123")

let observability_tests = [
  Alcotest.test_case "layout without scripts" `Quick test_layout_observability_off;
  Alcotest.test_case "layout with scripts" `Quick test_layout_observability_on;
]

(* ============================================= *)
(* UI Copy Tests                                 *)
(* ============================================= *)

let test_find_substring_from () =
  let open Wkbl.Views_common in
  Alcotest.(check (option int)) "find at 0" (Some 0) (find_substring_from ~sub:"foo" "foo" ~from:0);
  Alcotest.(check (option int)) "find in middle" (Some 4) (find_substring_from ~sub:"bar" "foo bar baz" ~from:0);
  Alcotest.(check (option int)) "find after offset" (Some 8) (find_substring_from ~sub:"baz" "foo bar baz" ~from:5);
  Alcotest.(check (option int)) "not found" None (find_substring_from ~sub:"zzz" "foo bar baz" ~from:0)

let test_ui_copy_no_dev_terms () =
  let banned = [ "PBP"; "FLOW"; "VERIFIED"; "DERIVED"; "MISMATCH"; "COALESCE"; "player_plus_minus"; "game_stats"; "SUM("; "team1/team2"; "Play-by-Play" ] in
  let check_clean ~ctx html =
    banned
    |> List.iter (fun sub ->
        Alcotest.(check bool) (ctx ^ " no " ^ sub) false (contains_substring html sub))
  in

  (* Score quality badges should be Korean *)
  let badge_full = Wkbl.Views_common.score_quality_badge Wkbl.Domain.Verified in
  Alcotest.(check bool) "badge label is Korean" true (contains_substring badge_full "일치");
  check_clean ~ctx:"score_quality_badge" badge_full;

  (* Boxscore action links and notes should not leak internal terms *)
  let pbp_link = Wkbl.Views.boxscore_pbp_link_html "046-01-62" in
  Alcotest.(check bool) "pbp link label" true (contains_substring pbp_link "문자중계");
  check_clean ~ctx:"boxscore_pbp_link_html" pbp_link;

  let flow_link = Wkbl.Views.boxscore_flow_link_html "046-01-62" in
  Alcotest.(check bool) "flow link label" true (contains_substring flow_link "득점흐름");
  check_clean ~ctx:"boxscore_flow_link_html" flow_link;

  let notes = Wkbl.Views.boxscore_data_notes_html ~official_link:"" in
  Alcotest.(check bool) "notes mention source" true (contains_substring notes "\xec\xb6\x9c\xec\xb2\x98");
  check_clean ~ctx:"boxscore_data_notes_html" notes;

  (* PBP page copy should be user-facing Korean labels *)
  let game : Wkbl.Domain.game_info =
    { gi_game_id = "046-01-62";
      gi_game_date = "2026-02-04";
      gi_home_team_code = "09";
      gi_home_team_name = "하나은행";
      gi_away_team_code = "03";
      gi_away_team_name = "삼성생명";
      gi_home_score = 54;
      gi_away_score = 74;
      gi_score_quality = Wkbl.Domain.Derived;
    }
  in
  let pbp_page_html = Wkbl.Views.pbp_page ~game ~periods:[] ~selected_period:"ALL" ~events:[] in
  Alcotest.(check bool) "pbp page title mentions Korean label" true (contains_substring pbp_page_html "문자중계");
  check_clean ~ctx:"pbp_page" pbp_page_html

let test_ops_copy_hidden_by_default () =
  let banned = [ "dune exec"; "scraper_tool"; "WKBL_SYNC_DRAFT_TRADE"; "scripts/wkbl_draft_trade_sync.py" ] in
  let check_clean ~ctx html =
    banned
    |> List.iter (fun sub ->
        Alcotest.(check bool) (ctx ^ " no " ^ sub) false (contains_substring html sub))
  in

  let seasons_catalog : Wkbl.Domain.season_info list =
    [ { code = "046"; name = "2025-2026" }
    ; { code = "045"; name = "2024-2025" }
    ]
  in
  let player : Wkbl.Domain.player_info =
    { id = "TEST001"
    ; name = "테스트"
    ; position = Some "G"
    ; birth_date = Some "2000-01-01"
    ; height = Some 170
    ; weight = None
    }
  in
  let averages : Wkbl.Domain.player_aggregate =
    { player_id = "TEST001"
    ; name = "테스트"
    ; team_name = "테스트팀"
    ; games_played = 0
    ; total_minutes = 0.0
    ; total_points = 0
    ; total_rebounds = 0
    ; total_assists = 0
    ; total_steals = 0
    ; total_blocks = 0
    ; total_turnovers = 0
    ; avg_points = 0.0
    ; avg_margin = 0.0
    ; avg_rebounds = 0.0
    ; avg_assists = 0.0
    ; avg_steals = 0.0
    ; avg_blocks = 0.0
    ; avg_turnovers = 0.0
    ; efficiency = 0.0
    }
  in
  let profile : Wkbl.Domain.player_profile =
    { player
    ; averages
    ; recent_games = []
    ; all_star_games = []
    ; draft = None
    ; official_trade_events = []
    ; external_links = []
    ; team_stints = []
    ; season_breakdown = []
    ; career_highs = None
    }
  in

  let player_html =
    Wkbl.Views_player.player_profile_page profile ~scope:"per_game" ~seasons_catalog
  in
  check_clean ~ctx:"player_profile_page" player_html;

  let tx_html =
    Wkbl.Views_tools.transactions_page
      ~show_ops:false
      ~tab:"draft"
      ~year:0
      ~q:""
      ~draft_years:[]
      ~trade_years:[]
      ~draft_picks:[]
      ~trade_events:[]
  in
  check_clean ~ctx:"transactions_page" tx_html

let test_nav_labels_are_korean () =
  let html =
    Wkbl.Views_common.layout
      ~title:"WKBL"
      ~canonical_path:"/"
      ~content:{html|<main id="main-content">hi</main>|html}
      ()
  in
  Alcotest.(check bool) "nav 선수" true (contains_substring html ">선수<");
  Alcotest.(check bool) "nav 팀" true (contains_substring html ">팀<");
  Alcotest.(check bool) "nav 순위" true (contains_substring html ">순위<");
  Alcotest.(check bool) "nav 경기" true (contains_substring html ">경기<");
  Alcotest.(check bool) "nav 비교" true (contains_substring html ">비교<");
  Alcotest.(check bool) "nav 예측" true (contains_substring html ">예측<");
  Alcotest.(check bool) "nav no Players" false (contains_substring html ">Players<");
  Alcotest.(check bool) "nav no Teams" false (contains_substring html ">Teams<")

let test_totals_tooltip_is_season () =
  let p : Wkbl.Domain.player_aggregate =
    { player_id = "TEST001"
    ; name = "테스트"
    ; team_name = "테스트팀"
    ; games_played = 10
    ; total_minutes = 250.0
    ; total_points = 123
    ; total_rebounds = 45
    ; total_assists = 67
    ; total_steals = 8
    ; total_blocks = 9
    ; total_turnovers = 10
    ; avg_points = 12.3
    ; avg_margin = 1.2
    ; avg_rebounds = 4.5
    ; avg_assists = 6.7
    ; avg_steals = 0.8
    ; avg_blocks = 0.9
    ; avg_turnovers = 1.0
    ; efficiency = 15.0
    }
  in
  let html = Wkbl.Views.players_table [ p ] in
  Alcotest.(check bool) "totals tooltip is cumulative" true (contains_substring html {|title="누적"|});
  Alcotest.(check bool) "no career total tooltip" false (contains_substring html "Career Total")

let ui_copy_tests = [
  Alcotest.test_case "find_substring_from" `Quick test_find_substring_from;
  Alcotest.test_case "ui copy avoids dev terms" `Quick test_ui_copy_no_dev_terms;
  Alcotest.test_case "ops copy hidden by default" `Quick test_ops_copy_hidden_by_default;
  Alcotest.test_case "nav labels are Korean" `Quick test_nav_labels_are_korean;
  Alcotest.test_case "totals tooltip is season" `Quick test_totals_tooltip_is_season;
]

(* ============================================= *)
(* Main Test Runner                              *)
(* ============================================= *)

let () =
  let open Alcotest in
  run "Wkbl Tests" [
    "MVP Score", mvp_score_tests;
    "Player Sort", player_sort_tests;
    "Team Codes", team_code_tests;
    "Game Quality", game_quality_tests;
    "Milestones", milestone_tests;
    "Career Trajectory", career_trajectory_tests;
    "Fantasy Score", fantasy_score_tests;
    "H2H Summary", h2h_summary_tests;
    "Hot Streaks", streaks_tests;
    "Clutch Time", clutch_tests;
    "Score Flow", score_flow_tests;
    "Views Tools", views_tools_tests;
    "Lineup Chemistry", lineup_chemistry_tests;
    "On/Off Impact", on_off_impact_tests;
    "Advanced Stats", advanced_stats_tests;
    "Scraper Functions", scraper_tests;
    "Disambiguation", disambiguation_tests;
    "QA Utils", qa_util_tests;
    "Observability", observability_tests;
    "UI Copy", ui_copy_tests;
  ]
