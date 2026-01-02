open Wkbl.Domain
open Wkbl.Qa
open Wkbl.Stats

let test_season_make () =
  let check input expected =
    let msg = Printf.sprintf "Season.make %d" input in
    Alcotest.(check (option int)) msg expected (Season.make input |> Option.map Season.year)
  in
  check 2025 (Some 2025);
  check 1900 None;
  check 2050 None;
  check 1998 (Some 1998);
  check 2030 (Some 2030)

let test_team_code_parsing () =
  let check input expected =
    let msg = Printf.sprintf "team_code_of_string %s" input in
    Alcotest.(check (option (testable pp_team_code equal_team_code))) msg expected (team_code_of_string input)
  in
  check "KB스타즈" (Some KB);
  check "01" (Some KB);
  check "우리은행" (Some Woori);
  check "OldTeam" (Some (Unknown "OldTeam"))

let test_position_parsing () =
  let check input expected =
    let msg = Printf.sprintf "position_of_string %s" input in
    Alcotest.(check (option (testable pp_position equal_position))) msg expected (position_of_string input)
  in
  check "G" (Some Guard);
  check "가드" (Some Guard);
  check "C" (Some Center);
  check "GF" (Some GuardForward);
  check "Invalid" None

let test_csv_parse () =
  let parsed = parse_csv_line {|alpha,"bravo,charlie",delta|} in
  Alcotest.(check (list string)) "csv parse with quotes"
    ["alpha"; "bravo,charlie"; "delta"] parsed

let test_normalize_key () =
  let ok = normalize_key "046" "01" "202401" "33" in
  let bad = normalize_key "xx" "01" "202401" "33" in
  Alcotest.(check (option string)) "normalize_key ok" (Some "46-1-202401-33") ok;
  Alcotest.(check (option string)) "normalize_key bad" None bad

let test_team_stats_per_game () =
  let totals = {
    season = "046";
    team = "우리은행";
    gp = 10;
    min_total = 200.0;
    fg2_m = 20;
    fg2_a = 40;
    fg3_m = 10;
    fg3_a = 20;
    ft_m = 15;
    ft_a = 20;
    reb_off = 30;
    reb_def = 50;
    reb = 80;
    ast = 40;
    stl = 10;
    blk = 5;
    turnovers = 12;
    pts = 100;
  } in
  let margin = Some { season = "046"; team = "우리은행"; gp = 10; pts_for = 100; pts_against = 80 } in
  let stats = team_stats_of_totals ~scope:PerGame ~margin totals in
  Alcotest.(check (float 0.01)) "pts per game" 10.0 stats.pts;
  Alcotest.(check (float 0.01)) "margin per game" 2.0 stats.margin;
  Alcotest.(check (float 0.01)) "fg%" 50.0 stats.fg_pct;
  Alcotest.(check (float 0.01)) "efg%" 58.3 stats.efg_pct;
  Alcotest.(check (float 0.01)) "ts%" 72.7 stats.ts_pct

let test_team_stats_totals () =
  let totals = {
    season = "046";
    team = "우리은행";
    gp = 10;
    min_total = 200.0;
    fg2_m = 20;
    fg2_a = 40;
    fg3_m = 10;
    fg3_a = 20;
    ft_m = 15;
    ft_a = 20;
    reb_off = 30;
    reb_def = 50;
    reb = 80;
    ast = 40;
    stl = 10;
    blk = 5;
    turnovers = 12;
    pts = 100;
  } in
  let margin = Some { season = "046"; team = "우리은행"; gp = 10; pts_for = 100; pts_against = 80 } in
  let stats = team_stats_of_totals ~scope:Totals ~margin totals in
  Alcotest.(check (float 0.01)) "pts totals" 100.0 stats.pts;
  Alcotest.(check (float 0.01)) "margin totals" 20.0 stats.margin;
  Alcotest.(check (float 0.01)) "pts against totals" 80.0 stats.pts_against

let () =
  let open Alcotest in
  run "WKBL Domain Tests" [
    "Season", [
      test_case "make" `Quick test_season_make;
    ];
    "TeamCode", [
      test_case "parsing" `Quick test_team_code_parsing;
    ];
    "Position", [
      test_case "parsing" `Quick test_position_parsing;
    ];
    "QA", [
      test_case "csv parse" `Quick test_csv_parse;
      test_case "normalize key" `Quick test_normalize_key;
    ];
    "TeamStats", [
      test_case "per game" `Quick test_team_stats_per_game;
      test_case "totals" `Quick test_team_stats_totals;
    ];
  ]
