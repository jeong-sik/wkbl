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
  ]
