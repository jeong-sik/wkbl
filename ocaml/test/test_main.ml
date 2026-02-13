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
    fg_made = 0; fg_att = 0;
    fg3_made = 0; fg3_att = 0;
    ft_made = 0; ft_att = 0;
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

let make_pbp_event ?(event_index=0) ~period_code ~clock ~home_score ~away_score () =
  { pe_period_code = period_code;
    pe_event_index = event_index;
    pe_team_side = 1;
    pe_description = "Test event";
    (* team1_score = away, team2_score = home *)
    pe_team1_score = away_score;
    pe_team2_score = home_score;
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
    make_pbp_event ~period_code:"Q1" ~clock:"09:30" ~home_score:(Some 2) ~away_score:(Some 0) ();
    make_pbp_event ~period_code:"Q1" ~clock:"08:00" ~home_score:(Some 2) ~away_score:(Some 3) ();
    make_pbp_event ~period_code:"Q1" ~clock:"06:00" ~home_score:(Some 5) ~away_score:(Some 3) ();
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
    make_pbp_event ~period_code:"Q1" ~clock:"09:30" ~home_score:None ~away_score:None ();
    make_pbp_event ~period_code:"Q1" ~clock:"08:00" ~home_score:(Some 2) ~away_score:None ();
  ] in
  let flow = extract_score_flow events in
  Alcotest.(check int) "No complete scores -> empty flow" 0 (List.length flow)

let test_extract_score_flow_dedup () =
  let events = [
    make_pbp_event ~period_code:"Q1" ~clock:"09:30" ~home_score:(Some 2) ~away_score:(Some 0) ();
    make_pbp_event ~period_code:"Q1" ~clock:"09:00" ~home_score:(Some 2) ~away_score:(Some 0) ();  (* Same score, should be deduped *)
    make_pbp_event ~period_code:"Q1" ~clock:"08:00" ~home_score:(Some 4) ~away_score:(Some 0) ();
  ] in
  let flow = extract_score_flow events in
  (* Should have: start (0-0), first 2-0, and 4-0 (deduped the duplicate 2-0) *)
  Alcotest.(check int) "Deduped flow = 3 points" 3 (List.length flow)

let test_extract_score_flow_stable_order_same_time () =
  let events = [
    make_pbp_event ~event_index:1 ~period_code:"Q1" ~clock:"09:30" ~home_score:(Some 2) ~away_score:(Some 0) ();
    make_pbp_event ~event_index:2 ~period_code:"Q1" ~clock:"09:30" ~home_score:(Some 4) ~away_score:(Some 0) ();
  ] in
  let flow = extract_score_flow events in
  Alcotest.(check int) "start + 2 score changes" 3 (List.length flow);
  let p1 = List.nth flow 1 in
  let p2 = List.nth flow 2 in
  Alcotest.(check int) "first change home" 2 p1.sfp_home_score;
  Alcotest.(check int) "second change home" 4 p2.sfp_home_score

let test_game_flow_chart_empty () =
  let html = Wkbl.Views_tools.game_flow_chart ~home_team:"A" ~away_team:"B" [] in
  Alcotest.(check bool) "empty placeholder" true (contains_substring html "득점흐름을 만들 기록이 없어요")

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
  Alcotest.(check bool) "has header" true (contains_substring html "경기 흐름");
  Alcotest.(check bool) "uses neutral line stroke" true (contains_substring html "stroke=\"currentColor\"")

let score_flow_tests = [
  Alcotest.test_case "Period to number conversion" `Quick test_period_to_number;
  Alcotest.test_case "Parse clock to seconds" `Quick test_parse_clock_to_seconds;
  Alcotest.test_case "Calculate elapsed seconds" `Quick test_calculate_elapsed_seconds;
  Alcotest.test_case "Extract score flow basic" `Quick test_extract_score_flow_basic;
  Alcotest.test_case "Extract score flow empty" `Quick test_extract_score_flow_empty;
  Alcotest.test_case "Extract score flow no scores" `Quick test_extract_score_flow_no_scores;
  Alcotest.test_case "Extract score flow dedup" `Quick test_extract_score_flow_dedup;
  Alcotest.test_case "Extract score flow stable order on same timestamp" `Quick test_extract_score_flow_stable_order_same_time;
]

(* ============================================= *)
(* PBP Refresh Heuristics Tests                   *)
(* ============================================= *)

let make_game_info_for_pbp_refresh ~game_id ~date ~home_score ~away_score =
  {
    gi_game_id = game_id;
    gi_game_date = date;
    gi_home_team_code = "SS";
    gi_home_team_name = "삼성생명";
    gi_away_team_code = "SH";
    gi_away_team_name = "신한은행";
    gi_home_score = home_score;
    gi_away_score = away_score;
    gi_score_quality = Derived;
  }

let test_pbp_should_backfill_empty_periods () =
  let g =
    make_game_info_for_pbp_refresh
      ~game_id:"046-01-01"
      ~date:"2026-01-01"
      ~home_score:70 ~away_score:60
  in
  Alcotest.(check bool) "empty periods -> backfill" true
    (Wkbl.Domain.pbp_should_backfill ~today_kst:"2026-02-08" g [])

let test_pbp_should_backfill_scheduled_empty_periods_is_false () =
  let g =
    make_game_info_for_pbp_refresh
      ~game_id:"046-01-00"
      ~date:"2026-02-08"
      ~home_score:0 ~away_score:0
  in
  Alcotest.(check bool) "scheduled empty periods -> no backfill" false
    (Wkbl.Domain.pbp_should_backfill ~today_kst:"2026-02-08" g [])

let test_pbp_should_backfill_incomplete_past_game () =
  let g =
    make_game_info_for_pbp_refresh
      ~game_id:"046-01-65"
      ~date:"2026-01-21"
      ~home_score:69 ~away_score:55
  in
  Alcotest.(check bool) "missing quarters in past game -> backfill" true
    (Wkbl.Domain.pbp_should_backfill ~today_kst:"2026-02-08" g ["Q1"])

let test_pbp_should_backfill_incomplete_today_game () =
  let g =
    make_game_info_for_pbp_refresh
      ~game_id:"046-01-99"
      ~date:"2026-02-08"
      ~home_score:10 ~away_score:8
  in
  Alcotest.(check bool) "missing quarters for today game -> backfill" true
    (Wkbl.Domain.pbp_should_backfill ~today_kst:"2026-02-08" g ["Q1"])

let test_score_flow_matches_final_true () =
  let pt : Wkbl.Domain.score_flow_point = {
    sfp_clock = "00:05";
    sfp_period = "Q4";
    sfp_home_score = 70;
    sfp_away_score = 60;
    sfp_diff = 10;
    sfp_elapsed_seconds = 2395;
  } in
  Alcotest.(check bool) "matches final" true
    (Wkbl.Domain.score_flow_matches_final ~final_home:70 ~final_away:60 [pt])

let test_score_flow_matches_final_false () =
  let pt : Wkbl.Domain.score_flow_point = {
    sfp_clock = "00:05";
    sfp_period = "Q4";
    sfp_home_score = 68;
    sfp_away_score = 60;
    sfp_diff = 8;
    sfp_elapsed_seconds = 2395;
  } in
  Alcotest.(check bool) "does not match final" false
    (Wkbl.Domain.score_flow_matches_final ~final_home:70 ~final_away:60 [pt])

let pbp_refresh_tests = [
  Alcotest.test_case "pbp_should_backfill: empty periods" `Quick test_pbp_should_backfill_empty_periods;
  Alcotest.test_case "pbp_should_backfill: scheduled empty periods is false" `Quick test_pbp_should_backfill_scheduled_empty_periods_is_false;
  Alcotest.test_case "pbp_should_backfill: incomplete past game" `Quick test_pbp_should_backfill_incomplete_past_game;
  Alcotest.test_case "pbp_should_backfill: today game" `Quick test_pbp_should_backfill_incomplete_today_game;
  Alcotest.test_case "score_flow_matches_final: true" `Quick test_score_flow_matches_final_true;
  Alcotest.test_case "score_flow_matches_final: false" `Quick test_score_flow_matches_final_false;
]

(* ============================================= *)
(* DB Transaction Wrapper Tests                  *)
(* ============================================= *)

let test_with_transaction_success () =
  let calls = ref [] in
  let push x = calls := !calls @ [ x ] in
  let begin_tx () = push "begin"; Ok () in
  let commit_tx () = push "commit"; Ok () in
  let rollback_tx () = push "rollback"; Ok () in
  let body () = push "body"; Ok 42 in
  let res =
    Wkbl.Db_sync.PbpSync.with_transaction ~begin_tx ~commit_tx ~rollback_tx body
  in
  Alcotest.(check (result int string)) "result" (Ok 42) res;
  Alcotest.(check (list string)) "calls"
    [ "begin"; "body"; "commit" ] !calls

let test_with_transaction_body_error_rolls_back () =
  let calls = ref [] in
  let push x = calls := !calls @ [ x ] in
  let begin_tx () = push "begin"; Ok () in
  let commit_tx () = push "commit"; Ok () in
  let rollback_tx () = push "rollback"; Ok () in
  let body () = push "body"; Error "boom" in
  let res =
    Wkbl.Db_sync.PbpSync.with_transaction ~begin_tx ~commit_tx ~rollback_tx body
  in
  Alcotest.(check (result int string)) "result" (Error "boom") res;
  Alcotest.(check (list string)) "calls"
    [ "begin"; "body"; "rollback" ] !calls

let test_with_transaction_commit_error_rolls_back () =
  let calls = ref [] in
  let push x = calls := !calls @ [ x ] in
  let begin_tx () = push "begin"; Ok () in
  let commit_tx () = push "commit"; Error "commit-failed" in
  let rollback_tx () = push "rollback"; Ok () in
  let body () = push "body"; Ok 1 in
  let res =
    Wkbl.Db_sync.PbpSync.with_transaction ~begin_tx ~commit_tx ~rollback_tx body
  in
  Alcotest.(check (result int string)) "result" (Error "commit-failed") res;
  Alcotest.(check (list string)) "calls"
    [ "begin"; "body"; "commit"; "rollback" ] !calls

let test_with_transaction_begin_error_skips_body () =
  let calls = ref [] in
  let push x = calls := !calls @ [ x ] in
  let begin_tx () = push "begin"; Error "begin-failed" in
  let commit_tx () = push "commit"; Ok () in
  let rollback_tx () = push "rollback"; Ok () in
  let body () = push "body"; Ok 1 in
  let res =
    Wkbl.Db_sync.PbpSync.with_transaction ~begin_tx ~commit_tx ~rollback_tx body
  in
  Alcotest.(check (result int string)) "result" (Error "begin-failed") res;
  Alcotest.(check (list string)) "calls"
    [ "begin" ] !calls

let test_with_transaction_exception_rolls_back () =
  let calls = ref [] in
  let push x = calls := !calls @ [ x ] in
  let begin_tx () = push "begin"; Ok () in
  let commit_tx () = push "commit"; Ok () in
  let rollback_tx () = push "rollback"; Ok () in
  let body () = push "body"; failwith "boom" in
  let raised =
    try
      let _ =
        Wkbl.Db_sync.PbpSync.with_transaction ~begin_tx ~commit_tx ~rollback_tx body
      in
      false
    with
    | Failure msg when msg = "boom" -> true
    | _ -> false
  in
  Alcotest.(check bool) "raises" true raised;
  Alcotest.(check (list string)) "calls"
    [ "begin"; "body"; "rollback" ] !calls

let db_transaction_tests = [
  Alcotest.test_case "with_transaction: success" `Quick test_with_transaction_success;
  Alcotest.test_case "with_transaction: body error rolls back" `Quick test_with_transaction_body_error_rolls_back;
  Alcotest.test_case "with_transaction: commit error rolls back" `Quick test_with_transaction_commit_error_rolls_back;
  Alcotest.test_case "with_transaction: begin error skips body" `Quick test_with_transaction_begin_error_skips_body;
  Alcotest.test_case "with_transaction: exception rolls back" `Quick test_with_transaction_exception_rolls_back;
]

let views_tools_tests = [
  Alcotest.test_case "game_flow_chart empty" `Quick test_game_flow_chart_empty;
  Alcotest.test_case "game_flow_chart single point" `Quick test_game_flow_chart_single_point;
]

(* ============================================= *)
(* Boxscore Link Chips Tests                     *)
(* ============================================= *)

let test_boxscore_pbp_chip_disabled_when_no_pbp () =
  let html = Wkbl.Views.boxscore_pbp_chip_html ~has_pbp:false "046-01-2" in
  Alcotest.(check bool) "shows unavailable label" true (contains_substring html "문자중계 없음");
  Alcotest.(check bool) "not a link" false (contains_substring html "<a href=\"/boxscore/046-01-2/pbp\"")

let test_boxscore_flow_chip_disabled_when_no_pbp () =
  let html = Wkbl.Views.boxscore_flow_chip_html ~has_pbp:false "046-01-2" in
  Alcotest.(check bool) "shows unavailable label" true (contains_substring html "득점흐름 없음");
  Alcotest.(check bool) "not a link" false (contains_substring html "<a href=\"/boxscore/046-01-2/flow\"")

let boxscore_link_chip_tests = [
  Alcotest.test_case "pbp chip disabled when no pbp" `Quick test_boxscore_pbp_chip_disabled_when_no_pbp;
  Alcotest.test_case "flow chip disabled when no pbp" `Quick test_boxscore_flow_chip_disabled_when_no_pbp;
]

(* ============================================= *)
(* Table Row Link Tests                          *)
(* ============================================= *)

let test_standings_table_has_row_link_attr () =
  let s : team_standing =
    {
      team_name = "KB스타즈";
      games_played = 1;
      wins = 1;
      losses = 0;
      win_pct = 1.0;
      gb = 0.0;
      avg_pts = 80.0;
      avg_opp_pts = 70.0;
      diff = 10.0;
    }
  in
  let html = Wkbl.Views.standings_table ~season:"046" [ s ] in
  Alcotest.(check bool) "data-row-link present" true (contains_substring html "data-row-link=\"team\"")

let test_teams_table_has_row_link_attr () =
  let s : team_stats =
    {
      team = "KB스타즈";
      gp = 1;
      min_total = 40.0;
      pts = 80.0;
      margin = 10.0;
      pts_against = 70.0;
      reb = 35.0;
      ast = 20.0;
      stl = 8.0;
      blk = 2.0;
      turnovers = 12.0;
      fg_pct = 45.0;
      fg3_pct = 33.3;
      ft_pct = 75.0;
      efg_pct = 50.0;
      ts_pct = 55.0;
      pace = 78.0;
      eff = 90.0;
    }
  in
  let html = Wkbl.Views.teams_table ~season:"046" ~scope:PerGame [ s ] in
  Alcotest.(check bool) "data-row-link present" true (contains_substring html "data-row-link=\"team\"")

let table_row_link_tests = [
  Alcotest.test_case "standings table has row-click attr" `Quick test_standings_table_has_row_link_attr;
  Alcotest.test_case "teams table has row-click attr" `Quick test_teams_table_has_row_link_attr;
]

(* ============================================= *)
(* AI Game Summary Tests                         *)
(* ============================================= *)

let make_game_info ~game_id ~date ~home_code ~home_name ~away_code ~away_name ~home_score ~away_score =
  {
    gi_game_id = game_id;
    gi_game_date = date;
    gi_home_team_code = home_code;
    gi_home_team_name = home_name;
    gi_away_team_code = away_code;
    gi_away_team_name = away_name;
    gi_home_score = home_score;
    gi_away_score = away_score;
    gi_score_quality = Derived;
  }

let dummy_player_stat ~player_id ~player_name ~team_code ~team_name ~pts ~reb ~ast : boxscore_player_stat =
  {
    bs_player_id = player_id;
    bs_player_name = player_name;
    bs_position = None;
    bs_team_code = team_code;
    bs_team_name = team_name;
    bs_minutes = 30.0;
    bs_pts = pts;
    bs_plus_minus = None;
    bs_reb = reb;
    bs_ast = ast;
    bs_stl = 0;
    bs_blk = 0;
    bs_tov = 0;
    bs_fg_made = 0;
    bs_fg_att = 0;
    bs_fg_pct = 0.0;
    bs_fg3_made = 0;
    bs_fg3_att = 0;
    bs_fg3_pct = 0.0;
    bs_ft_made = 0;
    bs_ft_att = 0;
    bs_ft_pct = 0.0;
  }

let test_ai_game_summary_scheduled_ko () =
  let gi =
    make_game_info
      ~game_id:"046-01-65"
      ~date:"2026-02-07"
      ~home_code:"SS" ~home_name:"삼성생명"
      ~away_code:"SH" ~away_name:"신한은행"
      ~home_score:0 ~away_score:0
  in
  let bs : game_boxscore = { boxscore_game = gi; boxscore_home_players = []; boxscore_away_players = [] } in
  let s = Wkbl.Ai.generate_game_summary ~lang:Wkbl.I18n.Ko bs in
  Alcotest.(check bool) "scheduled ko mentions not started" true (contains_substring s "경기 전");
  Alcotest.(check bool) "scheduled ko must not claim win" false (contains_substring s "신승");
  Alcotest.(check bool) "scheduled ko must not claim win (generic)" false (contains_substring s "이겼")

let test_ai_game_summary_scheduled_en () =
  let gi =
    make_game_info
      ~game_id:"046-01-65"
      ~date:"2026-02-07"
      ~home_code:"SS" ~home_name:"Samsung Life"
      ~away_code:"SH" ~away_name:"Shinhan Bank"
      ~home_score:0 ~away_score:0
  in
  let bs : game_boxscore = { boxscore_game = gi; boxscore_home_players = []; boxscore_away_players = [] } in
  let s = Wkbl.Ai.generate_game_summary ~lang:Wkbl.I18n.En bs in
  Alcotest.(check bool) "scheduled en mentions not started" true (contains_substring s "hasn't started")

let test_ai_game_summary_completed_ko () =
  let gi =
    make_game_info
      ~game_id:"046-01-01"
      ~date:"2026-01-01"
      ~home_code:"SS" ~home_name:"삼성생명"
      ~away_code:"SH" ~away_name:"신한은행"
      ~home_score:70 ~away_score:60
  in
  let p = dummy_player_stat ~player_id:"p1" ~player_name:"홍길동" ~team_code:"SS" ~team_name:"삼성생명" ~pts:22 ~reb:10 ~ast:5 in
  let bs : game_boxscore = { boxscore_game = gi; boxscore_home_players = [p]; boxscore_away_players = [] } in
  let s = Wkbl.Ai.generate_game_summary ~lang:Wkbl.I18n.Ko bs in
  Alcotest.(check bool) "completed ko contains winner" true (contains_substring s "삼성생명");
  Alcotest.(check bool) "completed ko must not be scheduled copy" false (contains_substring s "경기 전")

let ai_game_summary_tests = [
  Alcotest.test_case "scheduled summary (ko) is safe" `Quick test_ai_game_summary_scheduled_ko;
  Alcotest.test_case "scheduled summary (en) is safe" `Quick test_ai_game_summary_scheduled_en;
  Alcotest.test_case "completed summary (ko) mentions winner" `Quick test_ai_game_summary_completed_ko;
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

let test_seasons_catalog_regular_season_code_of_year_month () =
  Alcotest.(check string) "Feb 2026 -> 046" "046"
    (Wkbl.Seasons_catalog.regular_season_code_of_year_month ~year:2026 ~month:2);
  Alcotest.(check string) "Sep 2026 -> 046" "046"
    (Wkbl.Seasons_catalog.regular_season_code_of_year_month ~year:2026 ~month:9);
  Alcotest.(check string) "Oct 2026 -> 047" "047"
    (Wkbl.Seasons_catalog.regular_season_code_of_year_month ~year:2026 ~month:10)

let test_season_start_year_of_datalab_code () =
  Alcotest.(check (option int))
    "046 -> 2025"
    (Some 2025)
    (Wkbl.Scraper.season_start_year_of_datalab_code "046");
  Alcotest.(check (option int))
    "001 -> 1980"
    (Some 1980)
    (Wkbl.Scraper.season_start_year_of_datalab_code "001");
  Alcotest.(check (option int))
    "invalid -> None"
    None
    (Wkbl.Scraper.season_start_year_of_datalab_code "bad")

let test_is_league_team_variants () =
  Alcotest.(check bool) "BNK썸 -> true" true (Wkbl.Scraper.is_league_team "BNK썸");
  Alcotest.(check bool) "부산 BNK 썸 -> true" true (Wkbl.Scraper.is_league_team "부산 BNK 썸");
  (* All-Star teams should not be included in league schedule filters *)
  Alcotest.(check bool) "팀 포니블 -> false" false (Wkbl.Scraper.is_league_team "팀 포니블")

let test_schedule_status_from_scores () =
  let open Wkbl.Scraper in
  Alcotest.(check string)
    "0-0 placeholder -> scheduled"
    "scheduled"
    (schedule_status_from_scores (Some 0) (Some 0));
  Alcotest.(check string)
    "both None -> scheduled"
    "scheduled"
    (schedule_status_from_scores None None);
  Alcotest.(check string)
    "both positive -> completed"
    "completed"
    (schedule_status_from_scores (Some 71) (Some 78));
  Alcotest.(check string)
    "partial -> in_progress"
    "in_progress"
    (schedule_status_from_scores (Some 71) None)

let test_normalize_placeholder_scores () =
  let open Wkbl.Scraper in
  Alcotest.(check (Alcotest.pair (Alcotest.option Alcotest.int) (Alcotest.option Alcotest.int)))
    "0-0 placeholder -> (None, None)"
    (None, None)
    (normalize_placeholder_scores (Some 0) (Some 0));
  Alcotest.(check (Alcotest.pair (Alcotest.option Alcotest.int) (Alcotest.option Alcotest.int)))
    "real scores preserved"
    (Some 63, Some 65)
    (normalize_placeholder_scores (Some 63) (Some 65));
  Alcotest.(check (Alcotest.pair (Alcotest.option Alcotest.int) (Alcotest.option Alcotest.int)))
    "non-placeholder partial preserved"
    (Some 0, None)
    (normalize_placeholder_scores (Some 0) None)

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

let test_parse_schedule_html_extracts_game_meta_from_datalab_link () =
  let html = {|
<table><tbody>
  <tr id="20260204">
    <td>2/4(<span class="language">수</span>)</td>
    <td>
      <div class="team_versus">
        <div class="info_team away"><strong class="team_name">삼성생명</strong><em class="txt_score">74</em></div>
        <span class="txt_vs">vs</span>
        <div class="info_team home"><strong class="team_name">하나은행</strong><em class="txt_score">54</em></div>
      </div>
    </td>
    <td>용인실내체육관</td>
    <td>19:00</td>
    <td><a href="http://datalab.wkbl.or.kr/?id=04601062" target="_blank">Data Lab</a></td>
  </tr>
</tbody></table>
|} in
  let entries = Wkbl.Scraper.parse_schedule_html ~season:"046" ~ym:"202602" html in
  match entries with
  | [e] ->
      Alcotest.(check (option string)) "game_id" (Some "046-01-62") e.sch_game_id;
      Alcotest.(check (option string)) "game_type" (Some "01") e.sch_game_type;
      Alcotest.(check (option int)) "game_no" (Some 62) e.sch_game_no
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

let test_parse_boxscore_html_low_minutes_player () =
  (* Regression: short-stint players still need stable ids + points parsing. *)
  let html = {|
<div class="info_table01 type_record">
  <table><tbody>
    <tr>
      <td><a href="/player/detail.asp?pno=096140">양혜은</a></td>
      <td>C</td>
      <td>01:53</td>
      <td>1-1</td>
      <td>0-0</td>
      <td>0-0</td>
      <td>1</td>
      <td>0</td>
      <td>1</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>1</td>
      <td>0</td>
      <td>2</td>
    </tr>
  </tbody></table>
</div>
<div class="info_table01 type_record">
  <table><tbody>
    <tr>
      <td><a href="/player/detail.asp?pno=095041">김정은</a></td>
      <td>F</td>
      <td>10:00</td>
      <td>1-1</td>
      <td>0-0</td>
      <td>0-0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>0</td>
      <td>2</td>
    </tr>
  </tbody></table>
</div>
|} in
  match Wkbl.Scraper.parse_boxscore_html html with
  | [away; home] ->
      (match away with
      | [p] ->
          Alcotest.(check string) "away player_id" "096140" p.bs_player_id;
          Alcotest.(check string) "away player_name" "양혜은" p.bs_player_name;
          Alcotest.(check int) "away min_seconds" ((1 * 60) + 53) p.bs_min_seconds;
          Alcotest.(check int) "away pts" 2 p.bs_pts
      | _ -> Alcotest.fail "expected exactly one away player row");
      (match home with
      | [p] ->
          Alcotest.(check string) "home player_id" "095041" p.bs_player_id;
          Alcotest.(check string) "home player_name" "김정은" p.bs_player_name;
          Alcotest.(check int) "home min_seconds" (10 * 60) p.bs_min_seconds;
          Alcotest.(check int) "home pts" 2 p.bs_pts
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

let test_parse_live_pbp_xml_basic () =
  let xml =
    {|<?xml version="1.0" encoding="utf-8" ?><playHistory>|}
    ^ {|<playhistory>Q1|2|게임시작|||10:00</playhistory>|}
    ^ {|<playhistory>Q1|2|세키 나나미 2점슛성공|0|2|07:45</playhistory>|}
    ^ {|</playHistory>|}
  in
  let events = Wkbl.Scraper.parse_live_pbp_xml ~seq0:0 xml in
  Alcotest.(check int) "event count" 2 (List.length events);
  let e0 = List.nth events 0 in
  Alcotest.(check string) "e0 period" "Q1" e0.pe_period_code;
  Alcotest.(check int) "e0 index" 0 e0.pe_event_index;
  Alcotest.(check int) "e0 team_side" 2 e0.pe_team_side;
  Alcotest.(check string) "e0 desc" "게임시작" e0.pe_description;
  Alcotest.(check (option int)) "e0 t1 score none" None e0.pe_team1_score;
  Alcotest.(check (option int)) "e0 t2 score none" None e0.pe_team2_score;
  Alcotest.(check string) "e0 clock" "10:00" e0.pe_clock;

  let e1 = List.nth events 1 in
  Alcotest.(check int) "e1 index" 1 e1.pe_event_index;
  Alcotest.(check string) "e1 desc keeps spaced name" "세키 나나미 2점슛성공" e1.pe_description;
  Alcotest.(check (option int)) "e1 away score (team1)" (Some 0) e1.pe_team1_score;
  Alcotest.(check (option int)) "e1 home score (team2)" (Some 2) e1.pe_team2_score;

  let events2 = Wkbl.Scraper.parse_live_pbp_xml ~seq0:50 xml in
  let e1b = List.nth events2 1 in
  Alcotest.(check int) "seq0 offset applied" 51 e1b.pe_event_index

let test_parse_player_detail_html_basic () =
  let html =
    {|
    <div class="profile_view">
      <h3 class="tit_name">
        No.13 <span class="language" data-kr="박지수" data-en="PARK JI SU">박지수</span>
      </h3>
      <div class="scr_area default-skin">
        <ul class="list_text">
          <li><span class="language" data-kr="포지션" data-en="Position">포지션</span> - F</li>
          <li><span class="language" data-kr="신장" data-en="Height">신장</span> - 177 cm</li>
          <li><span class="language" data-kr="생년월일" data-en="Birth">생년월일</span> - 2003.04.12</li>
        </ul>
      </div>
    </div>
    |}
  in
  match Wkbl.Scraper.parse_player_detail_html ~player_id:"096136" html with
  | None -> Alcotest.fail "expected Some player_info"
  | Some info ->
      Alcotest.(check string) "name" "박지수" info.name;
      Alcotest.(check (option string)) "position" (Some "F") info.position;
      Alcotest.(check (option int)) "height" (Some 177) info.height;
      Alcotest.(check (option string)) "birth_date normalized" (Some "2003-04-12") info.birth_date

let scraper_tests = [
  Alcotest.test_case "code_from_team_name known" `Quick test_code_from_team_name_known;
  Alcotest.test_case "code_from_team_name unknown" `Quick test_code_from_team_name_unknown;
  Alcotest.test_case "code_from_team_name alternate" `Quick test_code_from_team_name_alternate;
  Alcotest.test_case "code_from_team_name extended" `Quick test_code_from_team_name_extended;
  Alcotest.test_case "normalize_schedule_date formats" `Quick test_normalize_schedule_date_formats;
  Alcotest.test_case "get_last_sync_time_str" `Quick test_get_last_sync_time_str_initial;
  Alcotest.test_case "seasons_catalog name_of_code" `Quick test_seasons_catalog_name_of_code;
  Alcotest.test_case "seasons_catalog unique codes" `Quick test_seasons_catalog_unique_codes;
  Alcotest.test_case "seasons_catalog regular season code (YM)" `Quick test_seasons_catalog_regular_season_code_of_year_month;
  Alcotest.test_case "season_start_year_of_datalab_code" `Quick test_season_start_year_of_datalab_code;
  Alcotest.test_case "is_league_team variants" `Quick test_is_league_team_variants;
  Alcotest.test_case "schedule_status_from_scores" `Quick test_schedule_status_from_scores;
  Alcotest.test_case "normalize_placeholder_scores" `Quick test_normalize_placeholder_scores;
  Alcotest.test_case "schedule sync success policy" `Quick test_schedule_sync_success_policy;
  Alcotest.test_case "schedule sync suspicion policy" `Quick test_schedule_sync_suspicion_reason_policy;
  Alcotest.test_case "game_params_of_href" `Quick test_game_params_of_href;
  Alcotest.test_case "game_id_of_params" `Quick test_game_id_of_params;
  Alcotest.test_case "parse_schedule_html extracts game meta" `Quick test_parse_schedule_html_extracts_game_meta;
  Alcotest.test_case "parse_schedule_html extracts meta from datalab link" `Quick test_parse_schedule_html_extracts_game_meta_from_datalab_link;
  Alcotest.test_case "parse_schedule_api_html extracts game meta" `Quick test_parse_schedule_api_html_extracts_game_meta;
  Alcotest.test_case "normalize_game_date formats" `Quick test_normalize_game_date_formats;
  Alcotest.test_case "parse_boxscore_html extracts two teams" `Quick test_parse_boxscore_html_two_teams;
  Alcotest.test_case "parse_boxscore_html keeps low minutes player" `Quick test_parse_boxscore_html_low_minutes_player;
  Alcotest.test_case "parse_live_pbp_xml basic" `Quick test_parse_live_pbp_xml_basic;
  Alcotest.test_case "parse_player_detail_html basic" `Quick test_parse_player_detail_html_basic;
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
(* I18n Tests                                    *)
(* ============================================= *)

let i18n_lang_testable =
  Alcotest.testable
    (fun fmt l ->
      Fmt.string fmt (match l with
        | Wkbl.I18n.Ko -> "Ko"
        | Wkbl.I18n.En -> "En"))
    (=)

let test_i18n_lang_of_code () =
  let open Wkbl.I18n in
  Alcotest.(check (option i18n_lang_testable)) "ko" (Some Ko) (lang_of_code "ko");
  Alcotest.(check (option i18n_lang_testable)) "KR" (Some Ko) (lang_of_code "KR");
  Alcotest.(check (option i18n_lang_testable)) "en" (Some En) (lang_of_code "en");
  Alcotest.(check (option i18n_lang_testable)) "unknown" None (lang_of_code "fr")

let test_i18n_lang_of_cookie_header () =
  let open Wkbl.I18n in
  Alcotest.(check (option i18n_lang_testable))
    "cookie en"
    (Some En)
    (lang_of_cookie_header "a=b; wkbl_lang=en; c=d");
  Alcotest.(check (option i18n_lang_testable))
    "cookie ko"
    (Some Ko)
    (lang_of_cookie_header "wkbl_lang=ko");
  Alcotest.(check (option i18n_lang_testable))
    "cookie invalid"
    None
    (lang_of_cookie_header "wkbl_lang=fr");
  Alcotest.(check (option i18n_lang_testable))
    "cookie missing"
    None
    (lang_of_cookie_header "a=b")

let test_i18n_set_cookie_header () =
  let open Wkbl.I18n in
  let h = set_cookie_header En in
  Alcotest.(check bool) "has wkbl_lang=en" true (contains_substring h "wkbl_lang=en");
  Alcotest.(check bool) "has Path=/" true (contains_substring h "Path=/");
  Alcotest.(check bool) "has Max-Age" true (contains_substring h "Max-Age=");
  Alcotest.(check bool) "has SameSite" true (contains_substring h "SameSite=");
  Alcotest.(check bool) "has HttpOnly" true (contains_substring h "HttpOnly")

let i18n_tests = [
  Alcotest.test_case "lang_of_code" `Quick test_i18n_lang_of_code;
  Alcotest.test_case "lang_of_cookie_header" `Quick test_i18n_lang_of_cookie_header;
  Alcotest.test_case "set_cookie_header" `Quick test_i18n_set_cookie_header;
]

(* ============================================= *)
(* Request Param Tests                           *)
(* ============================================= *)

let test_request_params_normalize_season () =
  let seasons : Wkbl.Domain.season_info list =
    [
      { code = "045"; name = "2024-2025" };
      { code = "046"; name = "2025-2026" };
    ]
  in
  let open Wkbl.Request_params in
  Alcotest.(check string) "empty -> latest" "046"
    (normalize_season ~seasons ~requested:"");
  Alcotest.(check string) "trim -> keep" "046"
    (normalize_season ~seasons ~requested:" 046 ");
  Alcotest.(check string) "ALL" "ALL"
    (normalize_season ~seasons ~requested:"all");
  Alcotest.(check string) "invalid -> latest" "046"
    (normalize_season ~seasons ~requested:"024");
  Alcotest.(check string) "empty seasons -> ALL" "ALL"
    (normalize_season ~seasons:[] ~requested:"024")

let request_params_tests = [
  Alcotest.test_case "normalize_season" `Quick test_request_params_normalize_season;
]

(* ============================================= *)
(* Canonical Host Tests                          *)
(* ============================================= *)

let test_canonical_host_strip_port () =
  let open Wkbl.Canonical_host in
  Alcotest.(check string) "no port" "woman.win" (strip_port "woman.win");
  Alcotest.(check string) "with port" "woman.win" (strip_port "woman.win:443");
  Alcotest.(check string) "trim + case" "www.woman.win" (strip_port "  WWW.WOMAN.WIN:80  ")

let test_canonical_host_should_redirect_to_wkbl () =
  let open Wkbl.Canonical_host in
  Alcotest.(check bool) "woman.win" true (should_redirect_to_wkbl "woman.win");
  Alcotest.(check bool) "www.woman.win" true (should_redirect_to_wkbl "www.woman.win:443");
  Alcotest.(check bool) "wkbl.win" false (should_redirect_to_wkbl "wkbl.win");
  Alcotest.(check bool) "random" false (should_redirect_to_wkbl "example.com")

let test_canonical_host_wkbl_location () =
  let open Wkbl.Canonical_host in
  Alcotest.(check string) "root fallback" "https://wkbl.win/" (wkbl_location ~request_uri:"");
  Alcotest.(check string) "path+query" "https://wkbl.win/games?season=046"
    (wkbl_location ~request_uri:"/games?season=046")

let canonical_host_tests = [
  Alcotest.test_case "strip_port" `Quick test_canonical_host_strip_port;
  Alcotest.test_case "should_redirect_to_wkbl" `Quick test_canonical_host_should_redirect_to_wkbl;
  Alcotest.test_case "wkbl_location" `Quick test_canonical_host_wkbl_location;
]

(* ============================================= *)
(* Player Identity Tests                         *)
(* ============================================= *)

let test_player_identity_redirect_location_none () =
  let open Wkbl.Player_identity in
  let uri = Uri.of_string "/player/095533?scope=totals&ops=1" in
  Alcotest.(check (option string)) "same id -> no redirect" None
    (redirect_location ~requested_id:"095533" ~canonical_id:"095533" uri)

let test_player_identity_redirect_location_preserves_query () =
  let open Wkbl.Player_identity in
  let uri = Uri.of_string "/player/096136?scope=totals&ops=1" in
  Alcotest.(check (option string)) "redirect keeps query"
    (Some "/player/095533?scope=totals&ops=1")
    (redirect_location ~requested_id:"096136" ~canonical_id:"095533" uri)

let test_player_identity_redirect_location_suffix () =
  let open Wkbl.Player_identity in
  let uri = Uri.of_string "/player/096136/games?season=046&include_mismatch=1" in
  Alcotest.(check (option string)) "suffix + query"
    (Some "/player/095533/games?season=046&include_mismatch=1")
    (redirect_location ~requested_id:"096136" ~canonical_id:"095533" ~suffix:"games" uri)

let player_identity_tests =
  [
    Alcotest.test_case "redirect_location none" `Quick test_player_identity_redirect_location_none;
    Alcotest.test_case "redirect_location preserves query" `Quick
      test_player_identity_redirect_location_preserves_query;
    Alcotest.test_case "redirect_location suffix" `Quick test_player_identity_redirect_location_suffix;
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
    qdr_finished_games_total = 0;
    qdr_games_with_stats = 0;
    qdr_pbp_games = 0;
    qdr_pbp_missing_games = 0;
    qdr_pbp_coverage_pct = 0.0;
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
  Alcotest.(check bool) "contains no games flag" true (contains_substring html "경기 없음");
  Alcotest.(check bool) "shows live text kpi" true (contains_substring html "문자중계 수집");
  Alcotest.(check bool) "links to pbp missing page" true (contains_substring html "/qa/pbp-missing")

let test_qa_dashboard_english_mode () =
  let report : Wkbl.Db.qa_db_report = {
    qdr_generated_at = "2026-02-06T00:00:00Z";
    qdr_games_total = 0;
    qdr_finished_games_total = 0;
    qdr_games_with_stats = 0;
    qdr_pbp_games = 0;
    qdr_pbp_missing_games = 0;
    qdr_pbp_coverage_pct = 0.0;
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
  let html = Wkbl.Views_tools.qa_dashboard_page ~lang:Wkbl.I18n.En report () in
  Alcotest.(check bool) "en heading" true (contains_substring html "Data Check");
  Alcotest.(check bool) "en last checked" true (contains_substring html "Last checked");
  Alcotest.(check bool) "en schedule coverage heading" true (contains_substring html "Schedule coverage");
  Alcotest.(check bool) "en sources summary" true (contains_substring html "Sources / How it is checked");
  Alcotest.(check bool) "en score mismatch block" true (contains_substring html "Score mismatch");
  Alcotest.(check bool) "en live text kpi" true (contains_substring html "Live text coverage")

let test_qa_schedule_missing_english_mode () =
  let report : Wkbl.Db.qa_schedule_missing_report =
    { qsmr_generated_at = "2026-02-06T00:00:00Z";
      qsmr_summary =
        { qsms_missing_ingested = 1;
          qsms_missing_uningested = 2;
          qsms_missing_total = 3;
        };
      qsmr_reason_counts = [ ("no_game_on_date", 1) ];
      qsmr_samples = [];
    }
  in
  let html = Wkbl.Views_tools.qa_schedule_missing_page ~lang:Wkbl.I18n.En report () in
  Alcotest.(check bool) "en heading" true (contains_substring html "Schedule gaps");
  Alcotest.(check bool) "en last checked" true (contains_substring html "Last checked");
  Alcotest.(check bool) "en table headers" true (contains_substring html ">Reason<")

let test_qa_pbp_missing_page_korean () =
  let seasons : Wkbl.Domain.season_info list = [
    { code = "046"; name = "2025-2026" };
  ] in
  let report : Wkbl.Db.qa_pbp_missing_report =
    { qpmr_generated_at = "2026-02-06T00:00:00Z";
      qpmr_season_code = "046";
      qpmr_finished_games_total = 10;
      qpmr_pbp_games = 7;
      qpmr_missing_games = 3;
      qpmr_coverage_pct = 70.0;
      qpmr_missing_sample = [
        { qpmg_game_id = "GAME-1";
          qpmg_game_date = "2026-02-01";
          qpmg_home_team = "우리은행";
          qpmg_away_team = "KB스타즈";
          qpmg_home_score = 70;
          qpmg_away_score = 65;
        }
      ];
    }
  in
  let html = Wkbl.Views_tools.qa_pbp_missing_page ~season:"046" ~seasons report () in
  Alcotest.(check bool) "korean heading" true (contains_substring html "문자중계 누락");
  Alcotest.(check bool) "has boxscore link" true (contains_substring html "/boxscore/GAME-1");
  Alcotest.(check bool) "has live text link" true (contains_substring html "/boxscore/GAME-1/pbp")

let qa_util_tests = [
  Alcotest.test_case "coverage pct" `Quick test_coverage_pct;
  Alcotest.test_case "split csv ids" `Quick test_split_csv_ids;
  Alcotest.test_case "qa schedule coverage block" `Quick test_qa_dashboard_schedule_coverage;
  Alcotest.test_case "qa dashboard english mode" `Quick test_qa_dashboard_english_mode;
  Alcotest.test_case "qa schedule missing english mode" `Quick test_qa_schedule_missing_english_mode;
  Alcotest.test_case "qa live text missing page (ko)" `Quick test_qa_pbp_missing_page_korean;
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
  Alcotest.(check bool) "no clarity script" false (contains_substring html "clarity.ms/tag/");
  Alcotest.(check bool) "includes htmx" true (contains_substring html "/static/js/htmx-1.9.10.min.js")

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
  let pbp_page_html = Wkbl.Views.pbp_page ~game ~periods:[] ~selected_period:"ALL" ~events:[] () in
  Alcotest.(check bool) "pbp page title mentions Korean label" true (contains_substring pbp_page_html "문자중계");
  check_clean ~ctx:"pbp_page" pbp_page_html;

  (* If a period tab exists but the selected period has no rows, avoid showing an empty list. *)
  let pbp_empty_period_html =
    Wkbl.Views.pbp_page ~game ~periods:["Q1"] ~selected_period:"Q1" ~events:[] ()
  in
  Alcotest.(check bool) "pbp empty period message" true
    (contains_substring pbp_empty_period_html "이 쿼터에는 기록이 없어요");
  check_clean ~ctx:"pbp_page_empty_period" pbp_empty_period_html

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
    ; career_entries = []
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
      ()
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

let test_boxscore_shooting_hides_0_0_pct () =
  let p : Wkbl.Domain.boxscore_player_stat =
    { bs_player_id = "P0001"
    ; bs_player_name = "테스트"
    ; bs_position = Some "G"
    ; bs_team_code = "09"
    ; bs_team_name = "우리은행"
    ; bs_minutes = 10.0
    ; bs_pts = 0
    ; bs_plus_minus = Some 0
    ; bs_reb = 0
    ; bs_ast = 0
    ; bs_stl = 0
    ; bs_blk = 0
    ; bs_tov = 0
    ; bs_fg_made = 0
    ; bs_fg_att = 1
    ; bs_fg_pct = 0.0
    ; bs_fg3_made = 0
    ; bs_fg3_att = 0
    ; bs_fg3_pct = 0.0
    ; bs_ft_made = 0
    ; bs_ft_att = 0
    ; bs_ft_pct = 0.0
    }
  in
  let html = Wkbl.Views.boxscore_player_table "Test" [ p ] in
  Alcotest.(check bool) "avoid 0-0 (0.0%)" false (contains_substring html "0-0 (0.0%)");
  Alcotest.(check bool) "dash for zero attempts" true
    (contains_substring html {|<span class="text-slate-400 dark:text-slate-600">-</span>|})

let test_pbp_page_passes_lang_to_layout () =
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
  let html =
    Wkbl.Views.pbp_page
      ~lang:Wkbl.I18n.En
      ~game
      ~periods:[]
      ~selected_period:"ALL"
      ~events:[]
      ()
  in
  Alcotest.(check bool) "html lang is en" true (contains_substring html {|<html lang="en">|});
  Alcotest.(check bool) "has English heading" true (contains_substring html "Play by play")

let test_game_flow_page_no_scoring_hides_chart () =
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
  let pt : Wkbl.Domain.score_flow_point =
    { sfp_clock = "10:00";
      sfp_period = "Q1";
      sfp_home_score = 0;
      sfp_away_score = 0;
      sfp_diff = 0;
      sfp_elapsed_seconds = 0;
    }
  in
  let html = Wkbl.Views_tools.game_flow_page ~game [ pt ] in
  Alcotest.(check bool) "empty state text" true (contains_substring html "득점흐름을 만들 기록이 없어요");
  Alcotest.(check bool) "no flow chart html" false (contains_substring html "Area fill")

let ui_copy_tests = [
  Alcotest.test_case "find_substring_from" `Quick test_find_substring_from;
  Alcotest.test_case "ui copy avoids dev terms" `Quick test_ui_copy_no_dev_terms;
  Alcotest.test_case "ops copy hidden by default" `Quick test_ops_copy_hidden_by_default;
  Alcotest.test_case "nav labels are Korean" `Quick test_nav_labels_are_korean;
  Alcotest.test_case "totals tooltip is season" `Quick test_totals_tooltip_is_season;
  Alcotest.test_case "boxscore hides 0-0%" `Quick test_boxscore_shooting_hides_0_0_pct;
  Alcotest.test_case "pbp page passes lang to layout" `Quick test_pbp_page_passes_lang_to_layout;
  Alcotest.test_case "flow page hides chart when no scoring" `Quick test_game_flow_page_no_scoring_hides_chart;
]

(* ============================================= *)
(* External Link / Live UX Tests                 *)
(* ============================================= *)

let test_wkbl_official_game_result_url_builds_valid_url () =
  let url =
    Wkbl.Views_common.wkbl_official_game_result_url
      ~game_id:"046-01-2"
      ~game_date:"2025-11-17"
  in
  Alcotest.(check (option string))
    "official URL"
    (Some
       "https://www.wkbl.or.kr/game/result.asp?season_gu=046&game_type=01&game_no=2&ym=202511&viewType=1")
    url

let test_live_page_treats_0_0_as_scheduled () =
  let html = Wkbl.Views.live_page () in
  Alcotest.(check bool)
    "0-0 scheduled guard present"
    true
    (contains_substring html "game.home_score === 0 && game.away_score === 0")

let external_link_tests = [
  Alcotest.test_case "wkbl official url builds valid url" `Quick test_wkbl_official_game_result_url_builds_valid_url;
  Alcotest.test_case "live page treats 0-0 as scheduled" `Quick test_live_page_treats_0_0_as_scheduled;
]

(* ============================================= *)
(* Live API Tests                                *)
(* ============================================= *)

let test_live_status_json_hides_pregame_scores () =
  let g : Wkbl.Domain.live_game =
    { lg_game_id = "046-01-65";
      lg_home_team = "A\"B";
      lg_away_team = "C";
      lg_home_score = 0;
      lg_away_score = 0;
      lg_quarter = "경기전";
      lg_time_remaining = "";
      lg_is_live = false;
    }
  in
  let json = Wkbl.Live.status_json_of_games [ g ] in
  Alcotest.(check bool) "home_score is null" true (contains_substring json {|"home_score":null|});
  Alcotest.(check bool) "away_score is null" true (contains_substring json {|"away_score":null|});
  Alcotest.(check bool) "home team quotes escaped" true (contains_substring json {|"home_team":"A\"B"|});
  Alcotest.(check bool) "no pregame 0 score" false (contains_substring json {|"home_score":0|})

let test_live_status_json_hides_pregame_scores_when_quarter_missing () =
  let g : Wkbl.Domain.live_game =
    { lg_game_id = "046-01-66";
      lg_home_team = "A";
      lg_away_team = "B";
      lg_home_score = 0;
      lg_away_score = 0;
      lg_quarter = "";
      lg_time_remaining = "";
      lg_is_live = false;
    }
  in
  let json = Wkbl.Live.status_json_of_games [ g ] in
  Alcotest.(check bool) "home_score is null" true (contains_substring json {|"home_score":null|});
  Alcotest.(check bool) "away_score is null" true (contains_substring json {|"away_score":null|});
  Alcotest.(check bool) "no pregame 0 score" false (contains_substring json {|"home_score":0|})

let test_live_widget_shows_vs_when_quarter_missing () =
  let g : Wkbl.Domain.live_game =
    { lg_game_id = "046-01-67";
      lg_home_team = "A";
      lg_away_team = "B";
      lg_home_score = 0;
      lg_away_score = 0;
      lg_quarter = "";
      lg_time_remaining = "";
      lg_is_live = false;
    }
  in
  let html = Wkbl.Views.live_scores_widget [ g ] in
  Alcotest.(check bool) "shows VS" true (contains_substring html ">VS<");
  Alcotest.(check bool) "badge shows scheduled" true (contains_substring html ">예정<");
  Alcotest.(check bool) "no 0 : 0" false (contains_substring html "0 : 0")

let live_api_tests = [
  Alcotest.test_case "pregame scores hidden + JSON escaped" `Quick test_live_status_json_hides_pregame_scores;
  Alcotest.test_case "pregame scores hidden when quarter missing" `Quick test_live_status_json_hides_pregame_scores_when_quarter_missing;
  Alcotest.test_case "live widget shows VS when quarter missing" `Quick test_live_widget_shows_vs_when_quarter_missing;
]

(* ============================================= *)
(* Player Team Stint Normalization Tests         *)
(* ============================================= *)

let test_normalize_player_team_stints_sandwich_one_game () =
  let stints : player_team_stint list =
    [ { pts_team_name = "KB스타즈";
        pts_start_date = "2016-12-17";
        pts_end_date = "2025-11-22";
        pts_games_played = 100;
      };
      { pts_team_name = "BNK 썸";
        pts_start_date = "2025-11-28";
        pts_end_date = "2025-11-28";
        pts_games_played = 1;
      };
      { pts_team_name = "KB스타즈";
        pts_start_date = "2025-12-15";
        pts_end_date = "2026-02-07";
        pts_games_played = 10;
      };
    ]
  in
  let normalized = normalize_player_team_stints stints in
  Alcotest.(check int) "collapsed to 1 stint" 1 (List.length normalized);
  let s = List.hd normalized in
  Alcotest.(check string) "team" "KB스타즈" s.pts_team_name;
  Alcotest.(check string) "start" "2016-12-17" s.pts_start_date;
  Alcotest.(check string) "end" "2026-02-07" s.pts_end_date;
  Alcotest.(check int) "gp excludes one-off" 110 s.pts_games_played

let test_normalize_player_team_stints_no_merge_three_teams () =
  let stints : player_team_stint list =
    [ { pts_team_name = "A";
        pts_start_date = "2025-01-01";
        pts_end_date = "2025-01-10";
        pts_games_played = 3;
      };
      { pts_team_name = "B";
        pts_start_date = "2025-01-12";
        pts_end_date = "2025-01-12";
        pts_games_played = 1;
      };
      { pts_team_name = "C";
        pts_start_date = "2025-01-14";
        pts_end_date = "2025-01-20";
        pts_games_played = 4;
      };
    ]
  in
  let normalized = normalize_player_team_stints stints in
  Alcotest.(check int) "keeps 3 stints" 3 (List.length normalized)

let test_normalize_player_team_stints_no_merge_when_middle_gt1 () =
  let stints : player_team_stint list =
    [ { pts_team_name = "A";
        pts_start_date = "2025-01-01";
        pts_end_date = "2025-01-10";
        pts_games_played = 3;
      };
      { pts_team_name = "B";
        pts_start_date = "2025-01-12";
        pts_end_date = "2025-01-15";
        pts_games_played = 2;
      };
      { pts_team_name = "A";
        pts_start_date = "2025-01-17";
        pts_end_date = "2025-01-20";
        pts_games_played = 2;
      };
    ]
  in
  let normalized = normalize_player_team_stints stints in
  Alcotest.(check int) "keeps 3 stints" 3 (List.length normalized)

let team_stint_tests =
  [ Alcotest.test_case "A-B(1)-A collapses" `Quick test_normalize_player_team_stints_sandwich_one_game;
    Alcotest.test_case "A-B-C stays" `Quick test_normalize_player_team_stints_no_merge_three_teams;
    Alcotest.test_case "A-B(2)-A stays" `Quick test_normalize_player_team_stints_no_merge_when_middle_gt1;
  ]

(* ============================================= *)
(* Player List Collapse Tests                     *)
(* ============================================= *)

let test_collapse_player_bases_ignores_zero_minute_team () =
  let open Wkbl.Db in
  let pb_kb : player_base =
    { pb_player_id = "095533";
      pb_player_name = "박지수";
      pb_team_name = "KB스타즈";
      pb_gp = 16;
      pb_min_seconds = 20000;
      pb_pts = 254;
      pb_reb = 157;
      pb_ast = 37;
      pb_stl = 9;
      pb_blk = 26;
      pb_tov = 31;
      pb_avg_pts = 15.875;
      pb_margin = 8.2;
      pb_avg_reb = 9.8125;
      pb_avg_ast = 2.3125;
      pb_avg_stl = 0.5625;
      pb_avg_blk = 1.625;
      pb_avg_tov = 1.9375;
      pb_eff = 15.4;
      pb_margin_seconds = 20000;
    }
  in
  let pb_ghost : player_base =
    { pb_player_id = "095533";
      pb_player_name = "박지수";
      pb_team_name = "BNK 썸";
      pb_gp = 1;
      pb_min_seconds = 0;
      pb_pts = 0;
      pb_reb = 0;
      pb_ast = 0;
      pb_stl = 0;
      pb_blk = 0;
      pb_tov = 1;
      pb_avg_pts = 0.0;
      pb_margin = 9.0;
      pb_avg_reb = 0.0;
      pb_avg_ast = 0.0;
      pb_avg_stl = 0.0;
      pb_avg_blk = 0.0;
      pb_avg_tov = 1.0;
      pb_eff = -1.0;
      pb_margin_seconds = 0;
    }
  in
  let collapsed = collapse_player_bases [ pb_kb; pb_ghost ] in
  Alcotest.(check int) "count" 1 (List.length collapsed);
  let c = List.hd collapsed in
  Alcotest.(check string) "team" "KB스타즈" c.pb_team_name;
  Alcotest.(check int) "gp" 16 c.pb_gp;
  Alcotest.(check int) "pts" 254 c.pb_pts;
  Alcotest.(check (Alcotest.float 0.0001)) "avg_pts" 15.875 c.pb_avg_pts

let player_collapse_tests = [
  Alcotest.test_case "collapse ignores 0-minute team rows" `Quick test_collapse_player_bases_ignores_zero_minute_team;
]

(* ============================================= *)
(* Games View Tests                              *)
(* ============================================= *)

let test_games_table_shows_missing_score_for_past_games () =
  let g : game_summary =
    { game_id = "999-01-01";
      game_date = "2000-01-01";
      home_team = "A";
      away_team = "B";
      home_score = Some 0;
      away_score = Some 0;
      game_type = "01";
    }
  in
  let html = Wkbl.Views.games_table [ g ] in
  Alcotest.(check bool) "shows missing score label" true (contains_substring html "점수 없음");
  Alcotest.(check bool) "does not show scheduled label" false (contains_substring html "예정")

let test_games_table_shows_scheduled_for_future_games () =
  let g : game_summary =
    { game_id = "999-01-02";
      game_date = "2100-01-01";
      home_team = "A";
      away_team = "B";
      home_score = None;
      away_score = None;
      game_type = "01";
    }
  in
  let html = Wkbl.Views.games_table [ g ] in
  Alcotest.(check bool) "shows scheduled label" true (contains_substring html "예정");
  Alcotest.(check bool) "does not show missing score label" false (contains_substring html "점수 없음")

let games_view_tests = [
  Alcotest.test_case "games table shows missing score for past games" `Quick test_games_table_shows_missing_score_for_past_games;
  Alcotest.test_case "games table shows scheduled for future games" `Quick test_games_table_shows_scheduled_for_future_games;
]

(* ============================================= *)
(* Team Page Tests                               *)
(* ============================================= *)

let test_team_profile_page_hides_0_0_and_has_roster_toggle () =
  let seasons : Wkbl.Domain.season_info list =
    [ { code = "046"; name = "2025-2026" } ]
  in
  let g_future : Wkbl.Domain.team_game_result =
    {
      tgr_game_id = "046-01-999";
      tgr_game_date = "2100-01-01";
      tgr_opponent = "BNK 썸";
      tgr_is_home = true;
      tgr_team_score = 0;
      tgr_opponent_score = 0;
      tgr_is_win = false;
    }
  in
  let g_past : Wkbl.Domain.team_game_result =
    {
      tgr_game_id = "046-01-998";
      tgr_game_date = "2000-01-01";
      tgr_opponent = "우리은행";
      tgr_is_home = false;
      tgr_team_score = 0;
      tgr_opponent_score = 0;
      tgr_is_win = false;
    }
  in
  let detail : Wkbl.Domain.team_full_detail =
    {
      tfd_team_name = "KB스타즈";
      tfd_standing = None;
      tfd_standings = [];
      tfd_roster = [];
      tfd_game_results = [ g_future; g_past ];
      tfd_recent_games = [ g_future; g_past ];
      tfd_team_totals = None;
      tfd_all_totals = [];
    }
  in
  let html = Wkbl.Views_team.team_profile_page detail ~season:"046" ~seasons in
  Alcotest.(check bool) "roster cards id exists" true (contains_substring html "id=\"roster-cards\"");
  Alcotest.(check bool) "details ontoggle exists" true (contains_substring html "ontoggle=");
  Alcotest.(check bool) "shows scheduled label" true (contains_substring html "예정");
  Alcotest.(check bool) "shows missing-score label" true (contains_substring html "점수 없음");
  Alcotest.(check bool) "does not show 0-0" false (contains_substring html "0 - 0")

let team_page_tests = [
  Alcotest.test_case "team profile hides 0-0 and has roster toggle" `Quick test_team_profile_page_hides_0_0_and_has_roster_toggle;
]

(* ============================================= *)
(* Boxscores View Tests                          *)
(* ============================================= *)

let test_boxscores_table_hides_unscored_games () =
  let open Wkbl.Domain in
  let g_scored : game_summary =
    {
      game_id = "046-01-50";
      game_date = "2026-01-21";
      home_team = "BNK썸";
      away_team = "우리은행";
      home_score = Some 63;
      away_score = Some 65;
      game_type = "01";
    }
  in
  let g_unscored : game_summary =
    {
      game_id = "046-02-99";
      game_date = "2026-02-01";
      home_team = "BNK썸";
      away_team = "우리은행";
      home_score = None;
      away_score = None;
      game_type = "01";
    }
  in
  let html = Wkbl.Views.boxscores_table [ g_unscored; g_scored ] in
  Alcotest.(check bool) "scored game present" true (contains_substring html "046-01-50");
  Alcotest.(check bool) "unscored game hidden" false (contains_substring html "046-02-99")

let boxscores_view_tests = [
  Alcotest.test_case "boxscores table hides unscored games" `Quick test_boxscores_table_hides_unscored_games;
]

(* ============================================= *)
(* Form URL-Encoded Tests                        *)
(* ============================================= *)

let test_form_urlencoded_parse_basic () =
  let open Wkbl.Form_urlencoded in
  let fields = parse "game_id=046-01-50&player_id=095533&reason=%ED%85%8C%EC%8A%A4%ED%8A%B8+%EA%B0%80%EC%B9%98" in
  Alcotest.(check (option string)) "game_id" (Some "046-01-50") (find fields "game_id");
  Alcotest.(check (option string)) "player_id" (Some "095533") (find fields "player_id");
  Alcotest.(check (option string)) "reason" (Some "테스트 가치") (find fields "reason")

let form_urlencoded_tests = [
  Alcotest.test_case "parse basic urlencoded body" `Quick test_form_urlencoded_parse_basic;
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
    "PBP Refresh", pbp_refresh_tests;
    "DB Transaction", db_transaction_tests;
    "Views Tools", views_tools_tests;
    "Tables UX", table_row_link_tests;
    "Boxscore Link Chips", boxscore_link_chip_tests;
    "AI Summary", ai_game_summary_tests;
    "Lineup Chemistry", lineup_chemistry_tests;
    "On/Off Impact", on_off_impact_tests;
    "Advanced Stats", advanced_stats_tests;
    "Scraper Functions", scraper_tests;
    "Disambiguation", disambiguation_tests;
    "I18n", i18n_tests;
    "Request Params", request_params_tests;
    "Canonical Host", canonical_host_tests;
    "Player Identity", player_identity_tests;
    "QA Utils", qa_util_tests;
    "Observability", observability_tests;
    "UI Copy", ui_copy_tests;
    "External Links", external_link_tests;
    "Live API", live_api_tests;
    "Team Stints", team_stint_tests;
    "Player Collapse", player_collapse_tests;
    "Games View", games_view_tests;
    "Team Page", team_page_tests;
    "Boxscores View", boxscores_view_tests;
    "Form URL-Encoded", form_urlencoded_tests;
  ]
