let contains hay needle =
  let hay_len = String.length hay in
  let needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop i =
      i + needle_len <= hay_len
      && (String.sub hay i needle_len = needle || loop (i + 1))
    in
    loop 0

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: no_shell_cards_test <cards.ml>";
    exit 2
  );
  let path = Sys.argv.(1) in
  let content = In_channel.with_open_text path In_channel.input_all in
  if contains content "Sys.command" then (
    prerr_endline "forbidden API found: Sys.command";
    exit 1
  );
  ()

