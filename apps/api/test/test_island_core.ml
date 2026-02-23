(** Tests for wkbl_island_core: table_sort_logic, number_format, prediction_calc.
    Pure computation modules shared between server and Wasm islands. *)

(* ── Float helpers ────────────────────────── *)

let float_eq ?(eps = 0.001) a b = Float.abs (a -. b) < eps

let float_testable =
  Alcotest.testable Fmt.float (fun a b -> float_eq a b)

(* ════════════════════════════════════════════ *)
(* Table Sort Logic                            *)
(* ════════════════════════════════════════════ *)

module Ts = Wkbl_island_core.Table_sort_logic

let sort_value_testable =
  Alcotest.testable
    (fun ppf v ->
      match v with
      | Ts.Numeric f -> Fmt.pf ppf "Numeric(%g)" f
      | Ts.Text s -> Fmt.pf ppf "Text(%s)" s
      | Ts.Empty -> Fmt.string ppf "Empty")
    ( = )

let test_parse_numeric () =
  Alcotest.(check sort_value_testable) "integer" (Ts.Numeric 42.0)
    (Ts.parse_sort_value "42");
  Alcotest.(check sort_value_testable) "float" (Ts.Numeric 3.14)
    (Ts.parse_sort_value "3.14");
  Alcotest.(check sort_value_testable) "negative" (Ts.Numeric (-7.0))
    (Ts.parse_sort_value "-7")

let test_parse_with_commas () =
  Alcotest.(check sort_value_testable) "1,234" (Ts.Numeric 1234.0)
    (Ts.parse_sort_value "1,234");
  Alcotest.(check sort_value_testable) "1,234,567" (Ts.Numeric 1234567.0)
    (Ts.parse_sort_value "1,234,567")

let test_parse_percentage () =
  Alcotest.(check sort_value_testable) "45.2%" (Ts.Numeric 45.2)
    (Ts.parse_sort_value "45.2%");
  Alcotest.(check sort_value_testable) "100%" (Ts.Numeric 100.0)
    (Ts.parse_sort_value "100%")

let test_parse_empty () =
  Alcotest.(check sort_value_testable) "empty string" Ts.Empty
    (Ts.parse_sort_value "");
  Alcotest.(check sort_value_testable) "dash" Ts.Empty
    (Ts.parse_sort_value "-");
  Alcotest.(check sort_value_testable) "N/A" Ts.Empty
    (Ts.parse_sort_value "N/A");
  Alcotest.(check sort_value_testable) "whitespace" Ts.Empty
    (Ts.parse_sort_value "   ")

let test_parse_text () =
  Alcotest.(check sort_value_testable) "team name" (Ts.Text "samsung")
    (Ts.parse_sort_value "Samsung")

let test_compare_sort_values () =
  (* Numeric ordering *)
  Alcotest.(check bool) "3 < 5" true
    (Ts.compare_sort_values (Numeric 3.0) (Numeric 5.0) < 0);
  Alcotest.(check bool) "5 > 3" true
    (Ts.compare_sort_values (Numeric 5.0) (Numeric 3.0) > 0);
  Alcotest.(check bool) "equal" true
    (Ts.compare_sort_values (Numeric 7.0) (Numeric 7.0) = 0);
  (* Empty sorts before everything *)
  Alcotest.(check bool) "empty < numeric" true
    (Ts.compare_sort_values Empty (Numeric 1.0) < 0);
  Alcotest.(check bool) "empty < text" true
    (Ts.compare_sort_values Empty (Text "a") < 0);
  (* Numeric before text *)
  Alcotest.(check bool) "numeric < text" true
    (Ts.compare_sort_values (Numeric 99.0) (Text "z") < 0)

let test_detect_col_type () =
  Alcotest.(check string) "number" "Number"
    (match Ts.detect_col_type [ "123"; "456" ] with
     | Ts.Number -> "Number" | Ts.Percent -> "Percent" | Ts.TextCol -> "TextCol");
  Alcotest.(check string) "percent" "Percent"
    (match Ts.detect_col_type [ "-"; "45.2%" ] with
     | Ts.Number -> "Number" | Ts.Percent -> "Percent" | Ts.TextCol -> "TextCol");
  Alcotest.(check string) "text" "TextCol"
    (match Ts.detect_col_type [ "Samsung"; "LG" ] with
     | Ts.Number -> "Number" | Ts.Percent -> "Percent" | Ts.TextCol -> "TextCol");
  Alcotest.(check string) "empty list" "TextCol"
    (match Ts.detect_col_type [] with
     | Ts.Number -> "Number" | Ts.Percent -> "Percent" | Ts.TextCol -> "TextCol")

let test_sort_indices_asc () =
  let values = [| Ts.Numeric 3.0; Ts.Numeric 1.0; Ts.Numeric 2.0 |] in
  let result = Ts.sort_indices values "asc" in
  Alcotest.(check (array int)) "asc sort" [| 1; 2; 0 |] result

let test_sort_indices_desc () =
  let values = [| Ts.Numeric 3.0; Ts.Numeric 1.0; Ts.Numeric 2.0 |] in
  let result = Ts.sort_indices values "desc" in
  Alcotest.(check (array int)) "desc sort" [| 0; 2; 1 |] result

let test_sort_indices_stable () =
  (* Equal values should preserve original order *)
  let values = [| Ts.Numeric 5.0; Ts.Numeric 5.0; Ts.Numeric 5.0 |] in
  let result = Ts.sort_indices values "asc" in
  Alcotest.(check (array int)) "stable" [| 0; 1; 2 |] result

let test_sort_indices_mixed () =
  let values = [| Ts.Empty; Ts.Numeric 2.0; Ts.Text "z"; Ts.Numeric 1.0 |] in
  let result = Ts.sort_indices values "asc" in
  (* Empty < Numeric 1 < Numeric 2 < Text "z" *)
  Alcotest.(check (array int)) "mixed types" [| 0; 3; 1; 2 |] result

(* ════════════════════════════════════════════ *)
(* Number Format                               *)
(* ════════════════════════════════════════════ *)

module Nf = Wkbl_island_core.Number_format

let test_format_below_threshold () =
  Alcotest.(check string) "small int" "500" (Nf.format_number 500.0);
  Alcotest.(check string) "zero" "0" (Nf.format_number 0.0);
  Alcotest.(check string) "small float" "3.5" (Nf.format_number 3.5)

let test_format_thousands () =
  Alcotest.(check string) "1K" "1K" (Nf.format_number 1000.0);
  Alcotest.(check string) "1.5K" "1.5K" (Nf.format_number 1500.0);
  Alcotest.(check string) "9.9K" "9.9K" (Nf.format_number 9900.0)

let test_format_man () =
  (* 만 = 10,000 *)
  Alcotest.(check string) "1만" "1\xEB\xA7\x8C"
    (Nf.format_number 10000.0);
  Alcotest.(check string) "5.3만" "5.3\xEB\xA7\x8C"
    (Nf.format_number 53000.0)

let test_format_millions () =
  Alcotest.(check string) "1M" "1M" (Nf.format_number 1000000.0);
  Alcotest.(check string) "2.5M" "2.5M" (Nf.format_number 2500000.0)

let test_format_billions () =
  Alcotest.(check string) "1B" "1B" (Nf.format_number 1000000000.0)

let test_format_trailing_zero_strip () =
  (* 2.0K should become "2K" not "2.0K" *)
  Alcotest.(check string) "2K no trailing zero" "2K"
    (Nf.format_number 2000.0)

let test_format_custom_decimals () =
  (* trailing zero strip removes .50 → .5 even with decimals:2 *)
  Alcotest.(check string) "2 decimals" "1.5K"
    (Nf.format_number ~decimals:2 1500.0);
  (* 1234 / 1000 = 1.234 → sprintf "%.2f" → "1.23" → strip → "1.23" *)
  Alcotest.(check string) "2 decimals non-zero" "1.23K"
    (Nf.format_number ~decimals:2 1234.0);
  Alcotest.(check string) "0 decimals" "2K"
    (Nf.format_number ~decimals:0 1500.0)

let test_format_custom_threshold () =
  (* With threshold 100, even small numbers get formatted *)
  Alcotest.(check string) "below custom" "50"
    (Nf.format_number ~min_threshold:100.0 50.0);
  Alcotest.(check string) "above custom" "500"
    (Nf.format_number ~min_threshold:100.0 500.0)

(* ════════════════════════════════════════════ *)
(* Prediction Calc                             *)
(* ════════════════════════════════════════════ *)

module Pc = Wkbl_island_core.Prediction_calc

let test_clamp01 () =
  Alcotest.(check float_testable) "below" 0.0 (Pc.clamp01 (-0.5));
  Alcotest.(check float_testable) "above" 1.0 (Pc.clamp01 1.5);
  Alcotest.(check float_testable) "in range" 0.7 (Pc.clamp01 0.7)

let test_log5 () =
  (* Equal teams: 50/50 *)
  Alcotest.(check float_testable) "equal" 0.5 (Pc.log5 ~a:0.5 ~b:0.5);
  (* Dominant team: 80% vs 20% *)
  let result = Pc.log5 ~a:0.8 ~b:0.2 in
  Alcotest.(check bool) "dominant > 0.9" true (result > 0.9);
  (* Zero denominator *)
  Alcotest.(check float_testable) "zero denom" 0.5 (Pc.log5 ~a:0.0 ~b:0.0)

let test_pythagorean_expectancy () =
  (* Equal points: 50% *)
  Alcotest.(check float_testable) "equal pts" 0.5
    (Pc.pythagorean_expectancy ~pts_for:1000 ~pts_against:1000);
  (* Better offense *)
  let result = Pc.pythagorean_expectancy ~pts_for:1200 ~pts_against:1000 in
  Alcotest.(check bool) "better offense > 0.5" true (result > 0.5);
  (* Zero/zero *)
  Alcotest.(check float_testable) "zero/zero" 0.5
    (Pc.pythagorean_expectancy ~pts_for:0 ~pts_against:0)

let test_elo_expected () =
  (* Equal ratings with no home advantage *)
  Alcotest.(check float_testable) "equal" 0.5
    (Pc.elo_expected ~home_rating:1500.0 ~away_rating:1500.0 ~home_adv:0.0);
  (* Higher-rated home team *)
  let result =
    Pc.elo_expected ~home_rating:1600.0 ~away_rating:1400.0 ~home_adv:0.0
  in
  Alcotest.(check bool) "higher rated > 0.5" true (result > 0.5);
  (* With home advantage *)
  let no_adv =
    Pc.elo_expected ~home_rating:1500.0 ~away_rating:1500.0 ~home_adv:0.0
  in
  let with_adv =
    Pc.elo_expected ~home_rating:1500.0 ~away_rating:1500.0 ~home_adv:100.0
  in
  Alcotest.(check bool) "home adv helps" true (with_adv > no_adv)

let test_stats_probability () =
  (* Equal teams, no home advantage *)
  let result =
    Pc.stats_probability ~win_pct_a:0.5 ~eff_a:100.0
      ~win_pct_b:0.5 ~eff_b:100.0 ~home_advantage:0.0
  in
  Alcotest.(check float_testable) "equal teams" 0.5 result;
  (* With home advantage *)
  let result_home =
    Pc.stats_probability ~win_pct_a:0.5 ~eff_a:100.0
      ~win_pct_b:0.5 ~eff_b:100.0 ~home_advantage:Pc.home_court_boost
  in
  Alcotest.(check bool) "home boost" true (result_home > 0.5)

let test_predict_margin () =
  (* 50% probability = 0 margin *)
  Alcotest.(check float_testable) "even" 0.0 (Pc.predict_margin ~prob_a:0.5);
  (* > 50% = positive margin *)
  Alcotest.(check bool) "favored" true
    (Pc.predict_margin ~prob_a:0.7 > 0.0);
  (* < 50% = negative margin *)
  Alcotest.(check bool) "underdog" true
    (Pc.predict_margin ~prob_a:0.3 < 0.0);
  (* Edge: 0.0 and 1.0 *)
  Alcotest.(check float_testable) "prob 0" (-20.0)
    (Pc.predict_margin ~prob_a:0.0);
  Alcotest.(check float_testable) "prob 1" 20.0
    (Pc.predict_margin ~prob_a:1.0)

let test_blend_probabilities () =
  (* All 50% = 50% *)
  Alcotest.(check float_testable) "all even" 0.5
    (Pc.blend_probabilities ~elo_prob:0.5 ~pyth_prob:0.5
       ~stats_prob:0.5 ~h2h_prob:0.5);
  (* Check weights sum to 1.0 *)
  let w_sum = Pc.w_elo +. Pc.w_pyth +. Pc.w_stats +. Pc.w_h2h in
  Alcotest.(check float_testable) "weights sum" 1.0 w_sum;
  (* All 100% = 100% *)
  Alcotest.(check float_testable) "all max" 1.0
    (Pc.blend_probabilities ~elo_prob:1.0 ~pyth_prob:1.0
       ~stats_prob:1.0 ~h2h_prob:1.0)

let test_margin_badge () =
  Alcotest.(check string) "close" "\xEC\xB4\x88\xEC\xA0\x91\xEC\xA0\x84"
    (Pc.margin_badge 1.0);
  Alcotest.(check string) "tight" "\xEC\xA0\x91\xEC\xA0\x84"
    (Pc.margin_badge 3.0);
  Alcotest.(check string) "advantage" "\xEC\x9A\xB0\xEC\x84\xB8"
    (Pc.margin_badge 8.0);
  Alcotest.(check string) "blowout" "\xEC\x95\x95\xEC\x8A\xB9"
    (Pc.margin_badge 15.0);
  (* Negative margins should use absolute value *)
  Alcotest.(check string) "negative close"
    "\xEC\xB4\x88\xEC\xA0\x91\xEC\xA0\x84"
    (Pc.margin_badge (-0.5))

(* ── Test runner ────────────────────────────── *)

let () =
  Alcotest.run "island_core"
    [
      ( "table_sort_logic",
        [
          Alcotest.test_case "parse numeric" `Quick test_parse_numeric;
          Alcotest.test_case "parse with commas" `Quick test_parse_with_commas;
          Alcotest.test_case "parse percentage" `Quick test_parse_percentage;
          Alcotest.test_case "parse empty" `Quick test_parse_empty;
          Alcotest.test_case "parse text" `Quick test_parse_text;
          Alcotest.test_case "compare values" `Quick test_compare_sort_values;
          Alcotest.test_case "detect col type" `Quick test_detect_col_type;
          Alcotest.test_case "sort asc" `Quick test_sort_indices_asc;
          Alcotest.test_case "sort desc" `Quick test_sort_indices_desc;
          Alcotest.test_case "sort stable" `Quick test_sort_indices_stable;
          Alcotest.test_case "sort mixed types" `Quick test_sort_indices_mixed;
        ] );
      ( "number_format",
        [
          Alcotest.test_case "below threshold" `Quick test_format_below_threshold;
          Alcotest.test_case "thousands (K)" `Quick test_format_thousands;
          Alcotest.test_case "man (만)" `Quick test_format_man;
          Alcotest.test_case "millions (M)" `Quick test_format_millions;
          Alcotest.test_case "billions (B)" `Quick test_format_billions;
          Alcotest.test_case "trailing zero strip" `Quick
            test_format_trailing_zero_strip;
          Alcotest.test_case "custom decimals" `Quick test_format_custom_decimals;
          Alcotest.test_case "custom threshold" `Quick test_format_custom_threshold;
        ] );
      ( "prediction_calc",
        [
          Alcotest.test_case "clamp01" `Quick test_clamp01;
          Alcotest.test_case "log5" `Quick test_log5;
          Alcotest.test_case "pythagorean" `Quick test_pythagorean_expectancy;
          Alcotest.test_case "elo expected" `Quick test_elo_expected;
          Alcotest.test_case "stats probability" `Quick test_stats_probability;
          Alcotest.test_case "predict margin" `Quick test_predict_margin;
          Alcotest.test_case "blend probabilities" `Quick
            test_blend_probabilities;
          Alcotest.test_case "margin badge" `Quick test_margin_badge;
        ] );
    ]
