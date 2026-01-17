(** Basic tests for the application *)

open Wkbl.Clutch

let test_parse_description () =
  let name, update = parse_description "박지수 2점슛 성공" in
  Alcotest.(check (option string)) "name" (Some "박지수") name;
  let stats = create_empty_stats "박지수" 1 in
  update stats;
  Alcotest.(check int) "pts" 2 stats.pts;
  
  let name, update = parse_description "김단비 3점슛 성공" in
  Alcotest.(check (option string)) "name" (Some "김단비") name;
  let stats = create_empty_stats "김단비" 1 in
  update stats;
  Alcotest.(check int) "pts" 3 stats.pts;
  
  let name, update = parse_description "신지현 리바운드 (공격)" in
  Alcotest.(check (option string)) "name" (Some "신지현") name;
  let stats = create_empty_stats "신지현" 1 in
  update stats;
  Alcotest.(check int) "reb" 1 stats.reb

let () =
  let open Alcotest in
  run "Wkbl Tests" [
    "Clutch", [
      test_case "Parse Description" `Quick test_parse_description;
    ];
  ]