(** Basic tests for the application *)

open Wkbl.Domain
open Wkbl.Clutch
open Wkbl.Similarity

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

let test_similarity () =
  let p1 = {
    player_id = "P1"; name = "Player 1"; team_name = "Team A";
    games_played = 10; total_minutes = 300.0; total_points = 200;
    total_rebounds = 100; total_assists = 50; total_steals = 10;
    total_blocks = 5; total_turnovers = 20;
    avg_points = 20.0; avg_margin = 5.0; avg_rebounds = 10.0;
    avg_assists = 5.0; avg_steals = 1.0; avg_blocks = 0.5;
    avg_turnovers = 2.0; efficiency = 25.0;
  } in
  let p2 = {
    p1 with player_id = "P2"; name = "Player 2"; avg_points = 21.0; efficiency = 26.0;
  } in
  let p3 = {
    p1 with player_id = "P3"; name = "Player 3"; avg_points = 5.0; efficiency = 5.0;
  } in
  
  let similar = calculate_similar_players p1 [p1; p2; p3] ~limit:2 in
  Alcotest.(check int) "count" 2 (List.length similar);
  Alcotest.(check string) "first is p2" "P2" (List.hd similar).player_id;
  Alcotest.(check bool) "p2 more similar than p3" true 
    ((List.hd similar).score > (List.nth similar 1).score)

let () =
  let open Alcotest in
  run "Wkbl Tests" [
    "Clutch", [
      test_case "Parse Description" `Quick test_parse_description;
    ];
    "Similarity", [
      test_case "Player Similarity" `Quick test_similarity;
    ];
  ]