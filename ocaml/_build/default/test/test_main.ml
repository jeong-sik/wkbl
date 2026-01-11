(** Basic tests for the application *)

let () =
  let open Alcotest in
  run "Wkbl Tests" [
    "Dummy", [
      test_case "Simple" `Quick (fun () -> (check int) "equals" 1 1);
    ];
  ]