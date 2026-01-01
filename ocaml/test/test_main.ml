open Wkbl.Domain

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
  ]