(** Data QA checks for WKBL datasets (CSV-based). *)

type coverage_row = {
  season : string;
  expected : int;
  actual : int;
  missing : int;
  coverage_pct : float;
} [@@deriving yojson]

type coverage = {
  meta_available : bool;
  expected_games : int;
  actual_games : int;
  missing_games : int;
  extra_games : int;
  coverage_pct : float;
  by_season : coverage_row list;
  missing_sample : string list;
  extra_sample : string list;
} [@@deriving yojson]

type duplicate_team = {
  game_key : string;
  team_count : int;
} [@@deriving yojson]

type duplicate_player = {
  game_key : string;
  team : string;
  name : string;
  row_count : int;
} [@@deriving yojson]

type outlier_iqr = {
  season : string;
  game_key : string;
  team : string;
  pts_for : float;
  pts_against : float;
  margin : float;
  low_threshold : float;
  high_threshold : float;
} [@@deriving yojson]

type outlier_abs = {
  game_key : string;
  team : string;
  pts_for : float;
  pts_against : float;
  margin : float;
} [@@deriving yojson]

type duplicates = {
  team_count_anomalies : duplicate_team list;
  player_duplicates : duplicate_player list;
} [@@deriving yojson]

type outliers = {
  iqr : outlier_iqr list;
  absolute_pts : outlier_abs list;
  absolute_margin : outlier_abs list;
} [@@deriving yojson]

type sources = {
  meta_file : string;
  players_games : string;
  team_games : string;
} [@@deriving yojson]

type report = {
  generated_at : string;
  sources : sources;
  coverage : coverage;
  duplicates : duplicates;
  outliers : outliers;
} [@@deriving yojson]

type report_error = {
  message : string;
  path : string option;
}

type paths = {
  data_dir : string;
  derived_dir : string;
  qa_dir : string;
  meta_file : string;
  players_games : string;
  team_games : string;
  qa_json : string;
  qa_md : string;
}

let abs_pts_high = 100.0
let abs_pts_low = 40.0
let abs_margin_high = 40.0

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let rec take n items =
  if n <= 0 then []
  else
    match items with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

let string_set_of_list items =
  List.fold_left (fun acc item -> StringSet.add item acc) StringSet.empty items

let rec find_data_dir dir depth =
  if depth < 0 then None
  else
    let candidate = Filename.concat dir "data/wkbl" in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_data_dir parent (depth - 1)

let resolve_data_dir () =
  match Sys.getenv_opt "WKBL_DATA_DIR" with
  | Some path when Sys.file_exists path -> path
  | Some _ -> "data/wkbl"
  | None ->
      match find_data_dir (Sys.getcwd ()) 4 with
      | Some path -> path
      | None -> "data/wkbl"

let resolve_paths () =
  let data_dir = resolve_data_dir () in
  let derived_dir = Filename.concat data_dir "derived" in
  let qa_dir = Filename.concat data_dir "qa" in
  {
    data_dir;
    derived_dir;
    qa_dir;
    meta_file = Filename.concat data_dir "meta/all_games_list.json";
    players_games = Filename.concat derived_dir "players_games.csv";
    team_games = Filename.concat derived_dir "team_games.csv";
    qa_json = Filename.concat qa_dir "qa_report.json";
    qa_md = Filename.concat qa_dir "qa_report.md";
  }

let rec ensure_dir path =
  if Sys.file_exists path then ()
  else
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755

let iso8601_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let parse_csv_line line =
  let line =
    let len = String.length line in
    if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1) else line
  in
  let buf = Buffer.create (String.length line) in
  let fields = ref [] in
  let i = ref 0 in
  let in_quotes = ref false in
  let push_field () =
    fields := Buffer.contents buf :: !fields;
    Buffer.clear buf
  in
  while !i < String.length line do
    let c = line.[!i] in
    if !in_quotes then (
      if c = '"' then (
        if !i + 1 < String.length line && line.[!i + 1] = '"' then (
          Buffer.add_char buf '"';
          i := !i + 2
        ) else (
          in_quotes := false;
          i := !i + 1
        )
      ) else (
        Buffer.add_char buf c;
        i := !i + 1
      )
    ) else (
      match c with
      | '"' ->
          in_quotes := true;
          i := !i + 1
      | ',' ->
          push_field ();
          i := !i + 1
      | _ ->
          Buffer.add_char buf c;
          i := !i + 1
    )
  done;
  push_field ();
  List.rev !fields

let load_csv path =
  if not (Sys.file_exists path) then
    Error { message = "missing required file"; path = Some path }
  else
    let ic = open_in path in
    let rec loop acc =
      match input_line ic with
      | line -> loop (parse_csv_line line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    match loop [] with
    | [] -> Error { message = "empty csv"; path = Some path }
    | header :: rows ->
        Ok (Array.of_list header, List.map Array.of_list rows)

let header_map header =
  let map = Hashtbl.create (Array.length header) in
  Array.iteri (fun idx name -> Hashtbl.replace map name idx) header;
  map

let get_field map row name =
  match Hashtbl.find_opt map name with
  | None -> ""
  | Some idx ->
      if idx < Array.length row then row.(idx) else ""

let int_opt value =
  try Some (int_of_string (String.trim value)) with
  | _ -> None

let float_opt value =
  try Some (float_of_string (String.trim value)) with
  | _ -> None

let normalize_key season game_type ym game_no =
  match int_opt season, int_opt game_type, int_opt ym, int_opt game_no with
  | Some season, Some game_type, Some ym, Some game_no ->
      Some (Printf.sprintf "%d-%d-%d-%d" season game_type ym game_no)
  | _ -> None

let build_expected_games meta_file =
  if not (Sys.file_exists meta_file) then
    (StringSet.empty, StringMap.empty, false)
  else
    let json = Yojson.Safe.from_file meta_file in
    let items =
      match json with
      | `List items -> items
      | _ -> []
    in
    let expected = ref StringSet.empty in
    let by_season = ref StringMap.empty in
    let add_season season =
      let count = match StringMap.find_opt season !by_season with
        | None -> 0
        | Some count -> count
      in
      by_season := StringMap.add season (count + 1) !by_season
    in
    let parse_int_field key item =
      match Yojson.Safe.Util.member key item with
      | `Int v -> Some v
      | `Intlit v -> int_opt v
      | `String v -> int_opt v
      | _ -> None
    in
    List.iter
      (fun item ->
        match
          parse_int_field "season_code" item,
          parse_int_field "game_type" item,
          parse_int_field "ym" item,
          parse_int_field "game_no" item
        with
        | Some season, Some game_type, Some ym, Some game_no ->
            let key = Printf.sprintf "%d-%d-%d-%d" season game_type ym game_no in
            expected := StringSet.add key !expected;
            add_season (string_of_int season)
        | _ -> ())
      items;
    (!expected, !by_season, true)

let build_actual_games rows map =
  let actual = ref StringSet.empty in
  let by_season = ref StringMap.empty in
  List.iter
    (fun row ->
      let season = get_field map row "season_gu" in
      let game_type = get_field map row "game_type" in
      let ym = get_field map row "ym" in
      let game_no = get_field map row "game_no" in
      match normalize_key season game_type ym game_no with
      | None -> ()
      | Some key ->
          actual := StringSet.add key !actual;
          let season_key =
            match int_opt season with
            | Some v -> string_of_int v
            | None -> season
          in
          let set =
            match StringMap.find_opt season_key !by_season with
            | None -> StringSet.empty
            | Some set -> set
          in
          by_season := StringMap.add season_key (StringSet.add key set) !by_season)
    rows;
  let counts =
    StringMap.fold
      (fun season keys acc -> StringMap.add season (StringSet.cardinal keys) acc)
      !by_season
      StringMap.empty
  in
  (!actual, counts)

let find_team_count_anomalies rows map =
  let counts = Hashtbl.create 1024 in
  List.iter
    (fun row ->
      let game_key = get_field map row "game_key" in
      let team = get_field map row "team" in
      if game_key <> "" && team <> "" then
        let set =
          match Hashtbl.find_opt counts game_key with
          | None -> StringSet.empty
          | Some set -> set
        in
        Hashtbl.replace counts game_key (StringSet.add team set))
    rows;
  let anomalies =
    Hashtbl.fold
      (fun game_key teams acc ->
        let team_count = StringSet.cardinal teams in
        if team_count <> 2 then { game_key; team_count } :: acc else acc)
      counts
      []
  in
  anomalies
  |> List.sort (fun a b -> compare b.team_count a.team_count)
  |> take 50

let find_duplicate_players rows map =
  let counts = Hashtbl.create 4096 in
  List.iter
    (fun row ->
      let game_key = get_field map row "game_key" in
      let team = get_field map row "team" in
      let name = get_field map row "name" in
      if game_key <> "" && team <> "" && name <> "" then
        let key = String.concat "|" [game_key; team; name] in
        let count = match Hashtbl.find_opt counts key with
          | None -> 0
          | Some count -> count
        in
        Hashtbl.replace counts key (count + 1))
    rows;
  let dupes =
    Hashtbl.fold
      (fun key count acc ->
        if count > 1 then
          match String.split_on_char '|' key with
          | [game_key; team; name] ->
              { game_key; team; name; row_count = count } :: acc
          | _ -> acc
        else acc)
      counts
      []
  in
  dupes
  |> List.sort (fun a b -> compare b.row_count a.row_count)
  |> take 50

type team_game = {
  season : string;
  game_key : string;
  team : string;
  pts_for : float option;
  pts_against : float option;
  margin : float option;
}

let build_team_games rows map =
  List.filter_map
    (fun row ->
      let season = get_field map row "season_gu" in
      let game_key = get_field map row "game_key" in
      let team = get_field map row "team" in
      if season = "" || game_key = "" || team = "" then
        None
      else
        let pts_for = float_opt (get_field map row "pts_for") in
        let pts_against = float_opt (get_field map row "pts_against") in
        let margin = float_opt (get_field map row "margin") in
        Some { season; game_key; team; pts_for; pts_against; margin })
    rows

let quantile q values =
  let sorted = List.sort compare values in
  let n = List.length sorted in
  if n = 0 then None
  else
    let pos = (float_of_int (n - 1)) *. q in
    let lower = int_of_float (floor pos) in
    let upper = int_of_float (ceil pos) in
    match List.nth_opt sorted lower, List.nth_opt sorted upper with
    | Some lower_v, Some upper_v ->
        if lower = upper then Some lower_v
        else
          let frac = pos -. float_of_int lower in
          Some (lower_v +. (upper_v -. lower_v) *. frac)
    | _ -> None

let build_pts_outliers (team_games: team_game list) =
  let by_season =
    List.fold_left
      (fun acc game ->
        let season_key =
          match int_opt game.season with
          | Some v -> string_of_int v
          | None -> game.season
        in
        let list = match StringMap.find_opt season_key acc with
          | None -> []
          | Some list -> list
        in
        StringMap.add season_key (game :: list) acc)
      StringMap.empty
      team_games
  in
  let iqr_outliers =
    StringMap.fold
      (fun season games acc ->
        let pts =
          games
          |> List.filter_map (fun g -> g.pts_for)
        in
        match quantile 0.25 pts, quantile 0.75 pts with
        | Some q1, Some q3 ->
            let iqr = q3 -. q1 in
            if iqr = 0.0 then acc
            else
              let low = max 0.0 (q1 -. (1.5 *. iqr)) in
              let high = q3 +. (1.5 *. iqr) in
              let flagged =
                games
                |> List.filter_map (fun g ->
                    match g.pts_for, g.pts_against, g.margin with
                    | Some pts_for, Some pts_against, Some margin ->
                        if pts_for > high || pts_for < low then
                          Some {
                            season;
                            game_key = g.game_key;
                            team = g.team;
                            pts_for;
                            pts_against;
                            margin;
                            low_threshold = Float.round (low *. 100.) /. 100.;
                            high_threshold = Float.round (high *. 100.) /. 100.;
                          }
                        else None
                    | _ -> None)
              in
              flagged @ acc
        | _ -> acc)
      by_season
      []
  in
  let iqr_outliers = List.rev iqr_outliers in
  let abs_pts : outlier_abs list =
    team_games
    |> List.filter_map (fun ({ season = _; game_key; team; pts_for; pts_against; margin } : team_game) ->
        match pts_for, pts_against, margin with
        | Some pts_for, Some pts_against, Some margin ->
            if pts_for >= abs_pts_high || pts_for <= abs_pts_low then
              Some { game_key; team; pts_for; pts_against; margin }
            else None
        | _ -> None)
    |> List.sort (fun (a: outlier_abs) (b: outlier_abs) ->
        compare b.pts_for a.pts_for)
  in
  let abs_margin : outlier_abs list =
    team_games
    |> List.filter_map (fun ({ season = _; game_key; team; pts_for; pts_against; margin } : team_game) ->
        match pts_for, pts_against, margin with
        | Some pts_for, Some pts_against, Some margin ->
            if Float.abs margin >= abs_margin_high then
              Some { game_key; team; pts_for; pts_against; margin }
            else None
        | _ -> None)
    |> List.sort (fun (a: outlier_abs) (b: outlier_abs) ->
        compare b.margin a.margin)
  in
  {
    iqr = take 50 iqr_outliers;
    absolute_pts = take 50 abs_pts;
    absolute_margin = take 50 abs_margin;
  }

let write_markdown report path =
  let lines = ref [] in
  let add line = lines := line :: !lines in
  let add_empty () = add "" in
  let coverage = report.coverage in
  add "# WKBL Data QA Report";
  add_empty ();
  add ("Generated: " ^ report.generated_at);
  add_empty ();
  add "## Summary";
  add (Printf.sprintf "- Meta file available: %b" coverage.meta_available);
  add (Printf.sprintf "- Expected games: %d" coverage.expected_games);
  add (Printf.sprintf "- Actual games: %d" coverage.actual_games);
  add (Printf.sprintf "- Missing games: %d" coverage.missing_games);
  add (Printf.sprintf "- Coverage: %.1f%%" coverage.coverage_pct);
  add_empty ();
  add "## Coverage by Season";
  add_empty ();
  add "| Season | Expected | Actual | Missing | Coverage |";
  add "| --- | ---: | ---: | ---: | ---: |";
  List.iter
    (fun (row: coverage_row) ->
      add (Printf.sprintf "| %s | %d | %d | %d | %.1f%% |"
        row.season row.expected row.actual row.missing row.coverage_pct))
    coverage.by_season;
  add_empty ();
  add "## Missing Games (sample)";
  add_empty ();
  add "```";
  List.iter add coverage.missing_sample;
  add "```";
  add_empty ();
  add "## Team Count Anomalies (game_key where team count != 2)";
  add_empty ();
  add "```";
  List.iter
    (fun (row: duplicate_team) ->
      add (Printf.sprintf "%s teams=%d" row.game_key row.team_count))
    report.duplicates.team_count_anomalies;
  add "```";
  add_empty ();
  add "## Duplicate Player Rows (sample)";
  add_empty ();
  add "```";
  List.iter
    (fun (row: duplicate_player) ->
      add (Printf.sprintf "%s %s %s x%d" row.game_key row.team row.name row.row_count))
    report.duplicates.player_duplicates;
  add "```";
  add_empty ();
  add "## Point Outliers (IQR)";
  add_empty ();
  add "```";
  List.iter
    (fun (row: outlier_iqr) ->
      add (Printf.sprintf "%s %s %s pts=%.1f"
        row.season row.game_key row.team row.pts_for))
    report.outliers.iqr;
  add "```";
  add_empty ();
  add "## Point Outliers (Absolute Thresholds)";
  add_empty ();
  add (Printf.sprintf "Thresholds: pts_for <= %.0f or >= %.0f" abs_pts_low abs_pts_high);
  add_empty ();
  add "```";
  List.iter
    (fun (row: outlier_abs) ->
      add (Printf.sprintf "%s %s pts=%.1f" row.game_key row.team row.pts_for))
    report.outliers.absolute_pts;
  add "```";
  add_empty ();
  add "## Margin Outliers (Absolute Thresholds)";
  add_empty ();
  add (Printf.sprintf "Threshold: |margin| >= %.0f" abs_margin_high);
  add_empty ();
  add "```";
  List.iter
    (fun (row: outlier_abs) ->
      add (Printf.sprintf "%s %s margin=%.1f" row.game_key row.team row.margin))
    report.outliers.absolute_margin;
  add "```";
  let content = String.concat "\n" (List.rev !lines) in
  ensure_dir (Filename.dirname path);
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  content

let generate_report paths =
  match load_csv paths.players_games with
  | Error err -> Error err
  | Ok (players_header, players_rows) ->
      (match load_csv paths.team_games with
      | Error err -> Error err
      | Ok (team_header, team_rows) ->
          let players_map = header_map players_header in
          let team_map = header_map team_header in
          let expected_games, expected_by_season, meta_available =
            build_expected_games paths.meta_file
          in
          let actual_games, actual_by_season = build_actual_games players_rows players_map in
          let missing, extra =
            if meta_available then
              ( StringSet.diff expected_games actual_games,
                StringSet.diff actual_games expected_games )
            else
              (StringSet.empty, StringSet.empty)
          in
          let seasons =
            StringSet.union
              (string_set_of_list (List.map fst (StringMap.bindings expected_by_season)))
              (string_set_of_list (List.map fst (StringMap.bindings actual_by_season)))
            |> StringSet.elements
            |> List.sort (fun a b ->
                match int_opt a, int_opt b with
                | Some a, Some b -> compare a b
                | Some _, None -> -1
                | None, Some _ -> 1
                | None, None -> String.compare a b)
          in
          let by_season =
            List.map
              (fun season ->
                let expected = match StringMap.find_opt season expected_by_season with
                  | None -> 0
                  | Some count -> count
                in
                let actual = match StringMap.find_opt season actual_by_season with
                  | None -> 0
                  | Some count -> count
                in
                let missing_count = if expected = 0 then 0 else max (expected - actual) 0 in
                let coverage_pct = if expected = 0 then 0.0 else (float_of_int actual /. float_of_int expected *. 100.0) in
                {
                  season;
                  expected;
                  actual;
                  missing = missing_count;
                  coverage_pct = Float.round (coverage_pct *. 10.) /. 10.;
                })
              seasons
          in
          let coverage =
            {
              meta_available;
              expected_games = StringSet.cardinal expected_games;
              actual_games = StringSet.cardinal actual_games;
              missing_games = StringSet.cardinal missing;
              extra_games = StringSet.cardinal extra;
              coverage_pct =
                if StringSet.cardinal expected_games = 0 then 0.0
                else
                  Float.round
                    ((float_of_int (StringSet.cardinal actual_games)
                      /. float_of_int (StringSet.cardinal expected_games) *. 100.0) *. 10.)
                  /. 10.;
              by_season;
              missing_sample = take 50 (StringSet.elements missing);
              extra_sample = take 50 (StringSet.elements extra);
            }
          in
          let duplicates =
            {
              team_count_anomalies = find_team_count_anomalies team_rows team_map;
              player_duplicates = find_duplicate_players players_rows players_map;
            }
          in
          let team_games = build_team_games team_rows team_map in
          let outliers = build_pts_outliers team_games in
          let report =
            {
              generated_at = iso8601_utc ();
              sources = {
                meta_file = paths.meta_file;
                players_games = paths.players_games;
                team_games = paths.team_games;
              };
              coverage;
              duplicates;
              outliers;
            }
          in
          Ok report)

let write_report paths report =
  ensure_dir paths.qa_dir;
  let json = report_to_yojson report |> Yojson.Safe.to_string in
  let oc = open_out paths.qa_json in
  output_string oc json;
  close_out oc;
  ignore (write_markdown report paths.qa_md)

let load_report ?(refresh=false) () =
  let paths = resolve_paths () in
  if (not refresh) && Sys.file_exists paths.qa_json then
    (try
       match Yojson.Safe.from_file paths.qa_json |> report_of_yojson with
       | Ok report -> Ok report
       | Error _ ->
           Error { message = "Failed to parse QA report"; path = Some paths.qa_json }
     with
     | _ -> Error { message = "Failed to parse QA report"; path = Some paths.qa_json })
  else
    match generate_report paths with
    | Ok report ->
        write_report paths report;
        Ok report
    | Error err -> Error err

let load_markdown ?(refresh=false) () =
  let paths = resolve_paths () in
  if (not refresh) && Sys.file_exists paths.qa_md then
    Ok (In_channel.with_open_text paths.qa_md In_channel.input_all)
  else
    match load_report ~refresh () with
    | Ok report ->
        Ok (write_markdown report paths.qa_md)
    | Error err -> Error err

let read_markdown_if_exists () =
  let paths = resolve_paths () in
  if Sys.file_exists paths.qa_md then
    Some (In_channel.with_open_text paths.qa_md In_channel.input_all)
  else
    None
