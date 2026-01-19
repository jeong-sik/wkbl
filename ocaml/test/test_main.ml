(** Basic tests for the application *)

open Alcotest

let test_escape_html () =
  let input = "<div>&\"</div>" in
  let expected = "&lt;div&gt;&amp;&quot;&lt;/div&gt;" in
  check string "escape_html" expected (Wkbl.Views.escape_html input)

let test_short_player_id () =
  check string "short_player_id" "345" (Wkbl.Views.short_player_id "12345")

let () =
  run "Wkbl Tests" [
    "views", [
      test_case "escape_html" `Quick test_escape_html;
      test_case "short_player_id" `Quick test_short_player_id;
    ];
  ]
