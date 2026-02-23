(** Smoke tests for Html_dsl — verifies core rendering, escaping, and HTMX attrs. *)

open Wkbl.Html_dsl

let s = to_string

let test_basic_element () =
  let html = div [cls "container"] [
    h1 [cls "title"] [text "Hello"];
    p [] [text "World"];
  ] in
  Alcotest.(check string) "basic div"
    {|<div class="container"><h1 class="title">Hello</h1><p>World</p></div>|}
    (s html)

let test_xss_escaping () =
  let html = span [] [text "<script>alert('xss')</script>"] in
  Alcotest.(check string) "escapes HTML entities"
    {|<span>&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;</span>|}
    (s html)

let test_raw_trusted () =
  let html = span [] [raw "<b>bold</b>"] in
  Alcotest.(check string) "raw passes through"
    {|<span><b>bold</b></span>|}
    (s html)

let test_htmx_attrs () =
  let html = button [
    cls "btn";
    hx_get "/api/data";
    hx_trigger "click";
    hx_swap "innerHTML";
  ] [text "Load"] in
  Alcotest.(check string) "htmx attributes"
    {|<button class="btn" hx-get="/api/data" hx-trigger="click" hx-swap="innerHTML">Load</button>|}
    (s html)

let test_island_marker () =
  let html = div [island "player-stats"; cls "w"] [text "x"] in
  Alcotest.(check string) "island = data-island"
    {|<div data-island="player-stats" class="w">x</div>|}
    (s html)

let test_void_element () =
  let html = img [src "/logo.png"; alt "Logo"] in
  Alcotest.(check string) "void element (no closing tag)"
    {|<img src="/logo.png" alt="Logo">|}
    (s html)

let test_combinators () =
  let html = concat [
    when_ true (text "yes");
    when_ false (text "no");
    maybe (fun v -> text v) (Some "found");
    maybe (fun v -> text v) None;
  ] in
  Alcotest.(check string) "when_ and maybe"
    "yesfound"
    (s html)

let test_empty () =
  Alcotest.(check string) "empty produces nothing"
    ""
    (s empty)

let test_float_formatting () =
  let html = float_ ~decimals:2 3.14159 in
  Alcotest.(check string) "float with 2 decimals"
    "3.14"
    (s html)

let test_int_rendering () =
  let html = int 42 in
  Alcotest.(check string) "int rendering"
    "42"
    (s html)

let test_boolean_attrs () =
  let html = input [type_ "checkbox"; checked; disabled] in
  Alcotest.(check string) "boolean attrs (no value)"
    {|<input type="checkbox" checked disabled>|}
    (s html)

let test_nested_structure () =
  let html = table [cls "t"] [
    thead [] [
      tr [] [th [] [text "Name"]; th [] [text "Score"]];
    ];
    tbody [] [
      tr [] [td [] [text "A"]; td [] [int 10]];
    ];
  ] in
  Alcotest.(check string) "nested table"
    {|<table class="t"><thead><tr><th>Name</th><th>Score</th></tr></thead><tbody><tr><td>A</td><td>10</td></tr></tbody></table>|}
    (s html)

let test_attr_escaping () =
  let html = a [href "/search?q=a&b=c"; title "say \"hello\""] [text "link"] in
  Alcotest.(check string) "attribute value escaping"
    {|<a href="/search?q=a&amp;b=c" title="say &quot;hello&quot;">link</a>|}
    (s html)

let () =
  Alcotest.run "html_dsl" [
    "text", [
      Alcotest.test_case "basic element"    `Quick test_basic_element;
      Alcotest.test_case "xss escaping"     `Quick test_xss_escaping;
      Alcotest.test_case "raw trusted"      `Quick test_raw_trusted;
      Alcotest.test_case "int"              `Quick test_int_rendering;
      Alcotest.test_case "float"            `Quick test_float_formatting;
      Alcotest.test_case "empty"            `Quick test_empty;
    ];
    "attrs", [
      Alcotest.test_case "htmx"            `Quick test_htmx_attrs;
      Alcotest.test_case "island"           `Quick test_island_marker;
      Alcotest.test_case "boolean"          `Quick test_boolean_attrs;
      Alcotest.test_case "escaping"         `Quick test_attr_escaping;
    ];
    "elements", [
      Alcotest.test_case "void element"     `Quick test_void_element;
      Alcotest.test_case "nested structure" `Quick test_nested_structure;
    ];
    "combinators", [
      Alcotest.test_case "when_ and maybe"  `Quick test_combinators;
    ];
  ]
