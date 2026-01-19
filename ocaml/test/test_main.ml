(** WKBL Test Suite
    Tests for domain logic, MVP scoring, and utility functions *)

open Wkbl.Domain

(** Helper for float comparison with tolerance *)
let float_eq ?(eps=0.001) a b = Float.abs (a -. b) < eps

let float_testable =
  Alcotest.testable (Fmt.float) (fun a b -> float_eq a b)

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

let mvp_score_tests = [
  Alcotest.test_case "Basic MVP score calculation" `Quick test_mvp_score_basic;
  Alcotest.test_case "Zero stats" `Quick test_mvp_score_zero_stats;
  Alcotest.test_case "Elite player stats" `Quick test_mvp_score_elite_player;
  Alcotest.test_case "Losing team scenario" `Quick test_mvp_score_losing_team;
  Alcotest.test_case "Perfect record bonus" `Quick test_mvp_score_perfect_record;
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

let team_code_tests = [
  Alcotest.test_case "Known team codes" `Quick test_team_code_known;
  Alcotest.test_case "Unknown team codes" `Quick test_team_code_unknown;
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
    ~p2_pts ~p2_reb ~p2_ast ~winner_team ~score_diff : h2h_game =
  { game_id; game_date; player1_team = p1_team; player2_team = p2_team;
    player1_pts = p1_pts; player1_reb = p1_reb; player1_ast = p1_ast;
    player2_pts = p2_pts; player2_reb = p2_reb; player2_ast = p2_ast;
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
      ~winner_team:"KB" ~score_diff:12
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
      ~winner_team:"KB" ~score_diff:5;
    make_h2h_game ~game_id:"G2" ~game_date:"2024-01-20"
      ~p1_team:"KB" ~p2_team:"SH"
      ~p1_pts:15 ~p1_reb:6 ~p1_ast:3
      ~p2_pts:28 ~p2_reb:12 ~p2_ast:8
      ~winner_team:"SH" ~score_diff:(-10);
    make_h2h_game ~game_id:"G3" ~game_date:"2024-01-30"
      ~p1_team:"KB" ~p2_team:"SH"
      ~p1_pts:30 ~p1_reb:7 ~p1_ast:9
      ~p2_pts:20 ~p2_reb:5 ~p2_ast:3
      ~winner_team:"KB" ~score_diff:15;
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
      ~winner_team:"BN" ~score_diff:(-20);
    make_h2h_game ~game_id:"G2" ~game_date:"2024-02-15"
      ~p1_team:"WO" ~p2_team:"BN"
      ~p1_pts:18 ~p1_reb:7 ~p1_ast:4
      ~p2_pts:21 ~p2_reb:9 ~p2_ast:5
      ~winner_team:"BN" ~score_diff:(-8);
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

let score_flow_tests = [
  Alcotest.test_case "Period to number conversion" `Quick test_period_to_number;
  Alcotest.test_case "Parse clock to seconds" `Quick test_parse_clock_to_seconds;
  Alcotest.test_case "Calculate elapsed seconds" `Quick test_calculate_elapsed_seconds;
  Alcotest.test_case "Extract score flow basic" `Quick test_extract_score_flow_basic;
  Alcotest.test_case "Extract score flow empty" `Quick test_extract_score_flow_empty;
  Alcotest.test_case "Extract score flow no scores" `Quick test_extract_score_flow_no_scores;
  Alcotest.test_case "Extract score flow dedup" `Quick test_extract_score_flow_dedup;
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
    "Lineup Chemistry", lineup_chemistry_tests;
    "On/Off Impact", on_off_impact_tests;
  ]
