(** WKBL Official Site Scraper (Eio-based)

    Scrapes data from https://www.wkbl.or.kr:
    - Draft history: /history/draft.asp
    - Awards: /history/awards.asp (TODO)
    - FA results: /history/fa_result.asp (TODO)
*)

let base_url = "https://www.wkbl.or.kr"

(** ==========================================================================
    Unified Season Code Mapping

    WKBL uses TWO different season code schemes:
    - Main Site (Draft API): 044 = 2025-2026
    - DataLab/Schedule API:  046 = 2025-2026

    Pattern: DataLab code = Main code + 2 (for modern seasons)

    This unified mapping provides:
    1. Canonical season names
    2. Bidirectional code conversion
    3. Single source of truth for all season data
    ========================================================================== *)

type season_mapping = {
  name: string;           (** e.g., "2025-2026" *)
  main_code: string;      (** Main site code (Draft API) *)
  datalab_code: string;   (** DataLab/Schedule API code *)
}

(** Master season list with both code schemes *)
let unified_seasons = [
  (* Modern era: Regular seasons (Oct-Mar) - offset +2 *)
  { name = "2025-2026"; main_code = "044"; datalab_code = "046" };
  { name = "2024-2025"; main_code = "043"; datalab_code = "045" };
  { name = "2023-2024"; main_code = "042"; datalab_code = "044" };
  { name = "2022-2023"; main_code = "041"; datalab_code = "043" };
  { name = "2021-2022"; main_code = "040"; datalab_code = "042" };
  { name = "2020-2021"; main_code = "039"; datalab_code = "041" };
  { name = "2019-2020"; main_code = "038"; datalab_code = "040" };
  { name = "2018-2019"; main_code = "037"; datalab_code = "039" };
  { name = "2017-2018"; main_code = "036"; datalab_code = "038" };
  { name = "2016-2017"; main_code = "035"; datalab_code = "037" };
  { name = "2015-2016"; main_code = "034"; datalab_code = "036" };
  { name = "2014-2015"; main_code = "033"; datalab_code = "035" };
  { name = "2013-2014"; main_code = "032"; datalab_code = "034" };
  { name = "2012-2013"; main_code = "031"; datalab_code = "033" };
  { name = "2011-2012"; main_code = "030"; datalab_code = "032" };
  { name = "2010-2011"; main_code = "029"; datalab_code = "031" };
]

(** Convert main site code to DataLab code *)
let main_to_datalab code =
  unified_seasons
  |> List.find_opt (fun s -> s.main_code = code)
  |> Option.map (fun s -> s.datalab_code)
  |> Option.value ~default:code

(** Convert DataLab code to main site code *)
let datalab_to_main code =
  unified_seasons
  |> List.find_opt (fun s -> s.datalab_code = code)
  |> Option.map (fun s -> s.main_code)
  |> Option.value ~default:code

(** Get season name from any code (main or datalab) *)
let season_name_of_code code =
  unified_seasons
  |> List.find_opt (fun s -> s.main_code = code || s.datalab_code = code)
  |> Option.map (fun s -> s.name)
  |> Option.value ~default:(Printf.sprintf "Unknown-%s" code)

(** Get both codes for a season name *)
let codes_of_season_name name =
  unified_seasons
  |> List.find_opt (fun s -> s.name = name)
  |> Option.map (fun s -> (s.main_code, s.datalab_code))

(** Get main site season code list (for Draft API) *)
let main_season_codes () =
  List.map (fun s -> (s.main_code, s.name)) unified_seasons

(** Get DataLab season code list (for DataLab/Schedule API) *)
let datalab_season_codes_list () =
  List.map (fun s -> (s.datalab_code, s.name)) unified_seasons

(** TLS authenticator - lazily initialized once *)
let tls_authenticator = lazy (Ca_certs.authenticator () |> Result.get_ok)

(** HTTPS wrapper for cohttp-eio *)
let https_wrapper uri socket =
  let host = Uri.host uri |> Option.map (fun h ->
    Domain_name.(of_string_exn h |> host_exn)
  ) in
  let authenticator = Lazy.force tls_authenticator in
  let tls_config = Tls.Config.client ~authenticator () |> Result.get_ok in
  Tls_eio.client_of_flow tls_config ?host socket

(** HTTP/HTTPS fetch with User-Agent - Eio-based with TLS support *)
let fetch_url ~sw ~env url =
  let net = Eio.Stdenv.net env in
  let headers = Cohttp.Header.of_list [
    ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) WKBL-Stats-Bot/1.0");
    ("Accept", "text/html,application/xhtml+xml");
    ("Accept-Language", "ko-KR,ko;q=0.9");
  ] in
  let uri = Uri.of_string url in
  let client = Cohttp_eio.Client.make ~https:(Some https_wrapper) net in
  match Cohttp_eio.Client.get ~sw client ~headers uri with
  | resp, body ->
      let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
      if code >= 200 && code < 300 then
        Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
      else begin
        Printf.eprintf "HTTP %d for %s\n" code url;
        ""
      end
  | exception exn ->
      Printf.eprintf "HTTP error for %s: %s\n" url (Printexc.to_string exn);
      ""

(** POST request with form data.
    Some WKBL endpoints require an AJAX-style request (Accept + X-Requested-With)
    and a game-specific Referer. *)
let post_url ~sw ~env ~referer url body_params =
  let net = Eio.Stdenv.net env in
  let headers = Cohttp.Header.of_list [
    ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) WKBL-Stats-Bot/1.0");
    ("Accept", "text/plain, */*; q=0.01");
    ("Accept-Language", "ko-KR,ko;q=0.9");
    ("X-Requested-With", "XMLHttpRequest");
    ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
    ("Referer", referer);
    ("Origin", "https://www.wkbl.or.kr");
  ] in
  let uri = Uri.of_string url in
  let body_str = Uri.encoded_of_query body_params in
  let client = Cohttp_eio.Client.make ~https:(Some https_wrapper) net in
  match Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri with
  | resp, body ->
      let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
      if code >= 200 && code < 300 then
        Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
      else begin
        Printf.eprintf "HTTP %d for POST %s\n" code url;
        ""
      end
  | exception exn ->
      Printf.eprintf "HTTP error for POST %s: %s\n" url (Printexc.to_string exn);
      ""

(* ======== BOXSCORE SCRAPER ======== *)

(** Player stat from game results *)
type boxscore_entry = {
  bs_player_id: string;
  bs_player_name: string;
  bs_min_seconds: int;
  bs_fg2_m: int;
  bs_fg2_a: int;
  bs_fg3_m: int;
  bs_fg3_a: int;
  bs_ft_m: int;
  bs_ft_a: int;
  bs_off_reb: int;
  bs_def_reb: int;
  bs_tot_reb: int;
  bs_ast: int;
  bs_stl: int;
  bs_blk: int;
  bs_tov: int;
  bs_pf: int;
  bs_pts: int;
}

(** Parse one team boxscore table from WKBL "ajax_game_result_2.asp" HTML. *)
let parse_boxscore_table table =
  let open Soup in
  let cell_text td = td |> texts |> String.concat "" |> String.trim in
  let parse_int s = s |> String.trim |> int_of_string_opt |> Option.value ~default:0 in
  let parse_min min_str =
    let parts = String.split_on_char ':' (String.trim min_str) in
    match parts with
    | [m; s] -> (parse_int m * 60) + parse_int s
    | [m] -> parse_int m * 60
    | _ -> 0
  in
  let parse_made_attempt s =
    match String.split_on_char '-' (String.trim s) with
    | [m; a] -> (parse_int m, parse_int a)
    | _ -> (0, 0)
  in
  table |> select "tbody tr" |> to_list |> List.filter_map (fun row ->
    let tds = row $$ "td" |> to_list in
    match tds with
    | player_td :: _pos_td :: min_td :: fg2_td :: fg3_td :: ft_td
      :: off_td :: def_td :: tot_td :: ast_td :: pf_td :: stl_td :: tov_td :: blk_td :: pts_td :: _ ->
        let a_opt = player_td $? "a" in
        let player_id =
          match a_opt with
          | None -> ""
          | Some a ->
              let href = attribute "href" a |> Option.value ~default:"" in
              let uri = Uri.of_string href in
              Uri.get_query_param uri "pno" |> Option.value ~default:""
        in
        let player_name =
          match a_opt with
          | Some a -> leaf_text a |> Option.value ~default:"" |> String.trim
          | None -> cell_text player_td
        in
        if player_id = "" || player_name = "" then None
        else
          let fg2_m, fg2_a = parse_made_attempt (cell_text fg2_td) in
          let fg3_m, fg3_a = parse_made_attempt (cell_text fg3_td) in
          let ft_m, ft_a = parse_made_attempt (cell_text ft_td) in
          Some {
            bs_player_id = player_id;
            bs_player_name = player_name;
            bs_min_seconds = parse_min (cell_text min_td);
            bs_fg2_m = fg2_m;
            bs_fg2_a = fg2_a;
            bs_fg3_m = fg3_m;
            bs_fg3_a = fg3_a;
            bs_ft_m = ft_m;
            bs_ft_a = ft_a;
            bs_off_reb = parse_int (cell_text off_td);
            bs_def_reb = parse_int (cell_text def_td);
            bs_tot_reb = parse_int (cell_text tot_td);
            bs_ast = parse_int (cell_text ast_td);
            bs_pf = parse_int (cell_text pf_td);
            bs_stl = parse_int (cell_text stl_td);
            bs_tov = parse_int (cell_text tov_td);
            bs_blk = parse_int (cell_text blk_td);
            bs_pts = parse_int (cell_text pts_td);
          }
    | _ -> None
  )

(** Parse per-team boxscore tables from WKBL "ajax_game_result_2.asp" HTML.
    Returns one list per team in the order shown on the WKBL site: [away; home]. *)
let parse_boxscore_html html =
  let open Soup in
  let soup = parse html in
  soup |> select ".info_table01.type_record table" |> to_list |> List.map parse_boxscore_table

(** Fetch per-team boxscores for a game.
    Note: WKBL expects (season_gu, game_type, game_no, ym) rather than (s_pcode, s_team). *)
let fetch_game_boxscore ~sw ~env ~season_gu ~game_type ~game_no ~ym =
  let url = "https://www.wkbl.or.kr/game/ajax/ajax_game_result_2.asp" in
  let referer =
    Printf.sprintf "https://www.wkbl.or.kr/game/result.asp?season_gu=%s&gun=1&game_type=%s&game_no=%d&ym=%s&viewType=1"
      season_gu game_type game_no ym
  in
  let params = [
    ("season_gu", [season_gu]);
    ("game_type", [game_type]);
    ("game_no", [string_of_int game_no]);
    ("ym", [ym]);
    ("h_player", [""]);
    ("a_player", [""]);
  ] in
  let html = post_url ~sw ~env ~referer url params in
  if String.length html > 0 then parse_boxscore_html html else []

(** Draft entry type *)
type draft_entry = {
  season_code: string;      (* e.g., "044" for 2025-2026 *)
  season_name: string;      (* e.g., "2025-2026" *)
  pick_order: int;          (* 1, 2, 3, ... *)
  team_name: string;        (* 신한은행, BNK 썸, etc. *)
  player_name: string;
  birth_date: string option;
  school: string option;
}

(** Parse draft table from HTML *)
let parse_draft_html ~season_code ~season_name html =
  let soup = Soup.parse html in
  (* Find table rows in .info_table01 tbody *)
  let rows = soup |> Soup.select ".info_table01 tbody tr" |> Soup.to_list in
  rows |> List.filter_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    | pick :: team :: name :: birth :: school :: _ ->
        let get_text node =
          Soup.leaf_text node
          |> Option.map String.trim
          |> Option.value ~default:""
        in
        let pick_order = get_text pick |> int_of_string_opt |> Option.value ~default:0 in
        let player_name = get_text name in
        if pick_order > 0 && String.length player_name > 0 then
          Some {
            season_code;
            season_name;
            pick_order;
            team_name = get_text team;
            player_name;
            birth_date = (let b = get_text birth in if b = "" then None else Some b);
            school = (let s = get_text school in if s = "" then None else Some s);
          }
        else None
    | _ -> None
  )

(** Fetch draft data for a specific season *)
let fetch_draft_season ~sw ~env ~season_code ~season_name =
  let url = Printf.sprintf "%s/history/draft.asp?season_gu=%s" base_url season_code in
  Printf.printf "Fetching draft season %s (%s)...\n%!" season_name season_code;
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_draft_html ~season_code ~season_name html

(** Season codes mapping (recent seasons)
    @deprecated Use [main_season_codes ()] from unified_seasons instead.
    This uses MAIN SITE codes (Draft API). *)
let season_codes = [
  ("044", "2025-2026");
  ("043", "2024-2025");
  ("042", "2023-2024");
  ("041", "2022-2023");
  ("040", "2021-2022");
  ("039", "2020-2021");
  ("038", "2019-2020");
  ("037", "2018-2019");
  ("036", "2017-2018");
  ("035", "2016-2017");
  ("034", "2015-2016");
  ("033", "2014-2015");
  ("032", "2013-2014");
  ("031", "2012-2013");
  ("030", "2011-2012");
]

(** Fetch all draft history *)
let fetch_all_drafts ~sw ~env =
  let clock = Eio.Stdenv.clock env in
  let rec fetch_all acc = function
    | [] -> List.rev acc
    | (code, name) :: rest ->
        let entries = fetch_draft_season ~sw ~env ~season_code:code ~season_name:name in
        (* Rate limiting: 500ms delay between requests *)
        Eio.Time.sleep clock 0.5;
        fetch_all (entries @ acc) rest
  in
  fetch_all [] season_codes

(** Print draft entries as CSV *)
let print_draft_csv entries =
  Printf.printf "season_code,season_name,pick_order,team_name,player_name,birth_date,school\n";
  entries |> List.iter (fun e ->
    Printf.printf "%s,%s,%d,%s,%s,%s,%s\n"
      e.season_code
      e.season_name
      e.pick_order
      e.team_name
      e.player_name
      (Option.value ~default:"" e.birth_date)
      (Option.value ~default:"" e.school)
  )

(** Insert draft entries to database *)
let insert_drafts_to_db entries =
  let open Caqti_request.Infix in
  let insert_query =
    (Caqti_type.(t6 string string int string string (option string)) ->. Caqti_type.unit)
    {|INSERT INTO player_drafts
      (season_code, season_name, pick_order, team_name, player_name, birth_date)
      VALUES ($1, $2, $3, $4, $5, $6)
      ON CONFLICT (season_code, pick_order) DO UPDATE SET
        team_name = EXCLUDED.team_name,
        player_name = EXCLUDED.player_name,
        birth_date = EXCLUDED.birth_date|}
  in
  entries |> List.iter (fun e ->
    match Db.with_db (fun (module Db : Caqti_eio.CONNECTION) ->
      Db.exec insert_query (e.season_code, e.season_name, e.pick_order, e.team_name, e.player_name, e.birth_date)
    ) with
    | Ok () -> ()
    | Error _ -> ()
  )

(* ======== AWARDS SCRAPER ======== *)

(** Award category type *)
type award_category =
  | Scoring          (* 득점상 *)
  | ThreePointScoring (* 3점 득점상 *)
  | ThreePointPct    (* 3점 야투상 *)
  | TwoPointPct      (* 2점 야투상 *)
  | FreeThrowPct     (* 자유투상 *)
  | Rebounding       (* 리바운드상 *)
  | Assists          (* 어시스트상 *)
  | Steals           (* 스틸상 *)
  | Blocks           (* 블록상 *)
  | MVP              (* MVP/윤덕주상 *)
  | Best5            (* BEST5 *)
  | Rookie           (* 신인상 *)

(** Statistical award entry *)
type stat_award = {
  season_name: string;
  category: award_category;
  player_name: string;
  stat_value: string option;  (* e.g., "21.10점", "64개", "37.50%" *)
}

(** BEST5 award entry *)
type best5_award = {
  b5_season_name: string;
  players: string list;  (* 5 players *)
}

let award_category_to_string = function
  | Scoring -> "scoring"
  | ThreePointScoring -> "three_point_scoring"
  | ThreePointPct -> "three_point_pct"
  | TwoPointPct -> "two_point_pct"
  | FreeThrowPct -> "free_throw_pct"
  | Rebounding -> "rebounding"
  | Assists -> "assists"
  | Steals -> "steals"
  | Blocks -> "blocks"
  | MVP -> "mvp"
  | Best5 -> "best5"
  | Rookie -> "rookie"

(** Get all text content from a node (handles <br> tags) *)
let get_all_text node =
  node
  |> Soup.texts
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> String.concat " "

(** Parse player name and stat from cell text like "김단비 (21.10점)" *)
let parse_stat_cell text =
  let text = String.trim text in
  (* Try to split name and stat by parenthesis *)
  match String.index_opt text '(' with
  | Some idx when idx > 0 ->
      let name = String.trim (String.sub text 0 idx) in
      let rest = String.sub text idx (String.length text - idx) in
      let stat =
        if String.length rest > 2 && rest.[0] = '(' then
          let end_idx = String.rindex_opt rest ')' |> Option.value ~default:(String.length rest) in
          Some (String.sub rest 1 (end_idx - 1))
        else
          Some rest
      in
      (name, stat)
  | _ -> (text, None)

(** Parse statistics awards table *)
let parse_stat_awards_html html =
  let soup = Soup.parse html in
  let rows = soup |> Soup.select ".info_table01 tbody tr" |> Soup.to_list in
  let categories = [|
    Scoring; ThreePointScoring; ThreePointPct; TwoPointPct;
    FreeThrowPct; Rebounding; Assists; Steals; Blocks; MVP
  |] in
  rows |> List.concat_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    | season_cell :: award_cells when List.length award_cells >= 10 ->
        let season_name = get_all_text season_cell in
        if String.length season_name > 0 then
          award_cells |> List.mapi (fun i cell ->
            if i < 10 then
              let text = get_all_text cell in
              let (player_name, stat_value) = parse_stat_cell text in
              Some {
                season_name;
                category = categories.(i);
                player_name;
                stat_value;
              }
            else None
          ) |> List.filter_map Fun.id
        else []
    | _ -> []
  )

(** Parse BEST5 awards table *)
let parse_best5_html html =
  let soup = Soup.parse html in
  let rows = soup |> Soup.select ".info_table01 tbody tr" |> Soup.to_list in
  rows |> List.filter_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    | season_cell :: players_cell :: _ ->
        let season_name = get_all_text season_cell in
        let players =
          players_cell |> Soup.select "p" |> Soup.to_list
          |> List.map get_all_text
          |> List.filter (fun s -> String.length s > 0)
        in
        if String.length season_name > 0 && List.length players > 0 then
          Some { b5_season_name = season_name; players }
        else None
    | _ -> None
  )

(** Fetch statistical awards *)
let fetch_stat_awards ~sw ~env =
  let url = Printf.sprintf "%s/history/awards_statistics.asp" base_url in
  Printf.printf "Fetching statistical awards...\n%!";
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_stat_awards_html html

(** Fetch BEST5 awards *)
let fetch_best5_awards ~sw ~env =
  let url = Printf.sprintf "%s/history/awards.asp" base_url in
  Printf.printf "Fetching BEST5 awards...\n%!";
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_best5_html html

(** Print stat awards as CSV *)
let print_stat_awards_csv entries =
  Printf.printf "season_name,category,player_name,stat_value\n";
  entries |> List.iter (fun e ->
    Printf.printf "%s,%s,%s,%s\n"
      e.season_name
      (award_category_to_string e.category)
      e.player_name
      (Option.value ~default:"" e.stat_value)
  )

(** Print BEST5 awards as CSV *)
let print_best5_csv entries =
  Printf.printf "season_name,rank,player_name\n";
  entries |> List.iter (fun e ->
    e.players |> List.iteri (fun i player ->
      Printf.printf "%s,%d,%s\n" e.b5_season_name (i + 1) player
    )
  )

(* ======== FA RESULTS SCRAPER ======== *)

(** FA result entry *)
type fa_entry = {
  year: string;              (* 2025, 2024, ... *)
  round: string;             (* 1차, 2차 *)
  original_team: string;     (* 소속구단 *)
  acquiring_team: string;    (* 영입구단 *)
  fa_player_name: string;
  contract_period: string;   (* 계약기간: 4년, 3년, ... *)
  salary: string;            (* 연봉 *)
  bonus: string option;      (* 수당 *)
  total_salary: string;      (* 연봉 총액 *)
}

(** Parse FA results table for a specific year tab *)
let parse_fa_year_table year_tab =
  let year =
    Soup.attribute "id" year_tab
    |> Option.map (fun s -> String.sub s 3 4)  (* tab2025 -> 2025 *)
    |> Option.value ~default:"unknown"
  in
  let rows = year_tab |> Soup.select "tbody tr" |> Soup.to_list in
  let current_round = ref "" in
  rows |> List.filter_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    (* Header row for round: "1차 협상결과 (2025.04.04)" *)
    | [cell] when Soup.attribute "colspan" cell |> Option.is_some ->
        let text = get_all_text cell in
        if String.length text > 0 && (String.sub text 0 2 = "1차" || String.sub text 0 2 = "2차") then
          current_round := String.sub text 0 2;
        None
    (* Data row: round, original_team, acquiring_team, name, period, salary, bonus, total *)
    | round_cell :: orig :: acq :: name :: period :: salary :: bonus :: total :: _ ->
        let round_text = get_all_text round_cell in
        let round = if String.length round_text > 0 then round_text else !current_round in
        let player_name = get_all_text name in
        if String.length player_name > 0 then
          Some {
            year;
            round;
            original_team = get_all_text orig;
            acquiring_team = get_all_text acq;
            fa_player_name = player_name;
            contract_period = get_all_text period;
            salary = get_all_text salary;
            bonus = (let b = get_all_text bonus in if b = "" then None else Some b);
            total_salary = get_all_text total;
          }
        else None
    | _ -> None
  )

(** Parse all FA results from the page *)
let parse_fa_html html =
  let soup = Soup.parse html in
  (* Find all year tabs *)
  let tabs = soup |> Soup.select ".info_table01[id^='tab']" |> Soup.to_list in
  tabs |> List.concat_map parse_fa_year_table

(** Fetch FA results *)
let fetch_fa_results ~sw ~env =
  let url = Printf.sprintf "%s/history/fa_result.asp" base_url in
  Printf.printf "Fetching FA results...\n%!";
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_fa_html html

(** Print FA results as CSV *)
let print_fa_csv entries =
  Printf.printf "year,round,original_team,acquiring_team,player_name,contract_period,salary,bonus,total_salary\n";
  entries |> List.iter (fun e ->
    Printf.printf "%s,%s,%s,%s,%s,%s,%s,%s,%s\n"
      e.year
      e.round
      e.original_team
      e.acquiring_team
      e.fa_player_name
      e.contract_period
      e.salary
      (Option.value ~default:"" e.bonus)
      e.total_salary
  )

(* ======== SALARY SCRAPER ======== *)

(** Salary entry type *)
type salary_entry = {
  season: string;           (* 2025-2026 *)
  rank: int;
  sal_player_name: string;
  sal_team_name: string;
  base_salary: string;      (* 연봉 *)
  bonus: string option;     (* 수당 *)
  total: string;            (* 총액 *)
  note: string option;      (* 비고 *)
}

(** Parse salary table for a specific season tab *)
let parse_salary_year_table year_tab =
  let season =
    Soup.attribute "id" year_tab
    |> Option.map (fun s ->
        (* tab2025 -> 2025-2026 *)
        let year = String.sub s 3 4 in
        let year_int = int_of_string year in
        Printf.sprintf "%d-%d" year_int (year_int + 1)
      )
    |> Option.value ~default:"unknown"
  in
  let rows = year_tab |> Soup.select "tbody tr" |> Soup.to_list in
  let current_rank = ref 0 in
  rows |> List.filter_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    (* 6 or 7 cells depending on rowspan *)
    | rank_cell :: rest when List.length cells >= 6 ->
        let rank_text = get_all_text rank_cell in
        (* Update rank if present, otherwise use previous *)
        (match int_of_string_opt rank_text with
         | Some r -> current_rank := r
         | None -> ());
        (* Handle different cell counts due to rowspan *)
        let (name_cell, team_cell, base_cell, bonus_cell, total_cell, note_cell) =
          if String.length rank_text > 0 && int_of_string_opt rank_text |> Option.is_some then
            (* First row of rank group: 6 data cells *)
            match rest with
            | n :: t :: b :: bn :: tot :: note :: _ -> (n, t, b, bn, tot, Some note)
            | n :: t :: b :: bn :: tot :: [] -> (n, t, b, bn, tot, None)
            | _ -> (rank_cell, rank_cell, rank_cell, rank_cell, rank_cell, None)
          else
            (* Continuation row: rank is rowspanned, so cells shift *)
            match cells with
            | n :: t :: b :: bn :: tot :: note :: _ -> (n, t, b, bn, tot, Some note)
            | n :: t :: b :: bn :: tot :: [] -> (n, t, b, bn, tot, None)
            | _ -> (rank_cell, rank_cell, rank_cell, rank_cell, rank_cell, None)
        in
        let player_name = get_all_text name_cell in
        if String.length player_name > 0 && !current_rank > 0 then
          Some {
            season;
            rank = !current_rank;
            sal_player_name = player_name;
            sal_team_name = get_all_text team_cell;
            base_salary = get_all_text base_cell;
            bonus = (let b = get_all_text bonus_cell in if b = "" then None else Some b);
            total = get_all_text total_cell;
            note = Option.bind note_cell (fun c ->
              let n = get_all_text c in if n = "" then None else Some n);
          }
        else None
    | _ -> None
  )

(** Parse all salary data from the page *)
let parse_salary_html html =
  let soup = Soup.parse html in
  (* Find all year tabs - they have id like "tab2025" *)
  let tabs = soup |> Soup.select ".info_table01[id^='tab']" |> Soup.to_list in
  tabs |> List.concat_map parse_salary_year_table

(** Fetch salary data *)
let fetch_salary ~sw ~env =
  let url = Printf.sprintf "%s/history/salary.asp" base_url in
  Printf.printf "Fetching salary data...\n%!";
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_salary_html html

(** Print salary as CSV *)
let print_salary_csv entries =
  Printf.printf "season,rank,player_name,team_name,base_salary,bonus,total,note\n";
  entries |> List.iter (fun e ->
    Printf.printf "%s,%d,%s,%s,\"%s\",\"%s\",\"%s\",%s\n"
      e.season
      e.rank
      e.sal_player_name
      e.sal_team_name
      e.base_salary
      (Option.value ~default:"" e.bonus)
      e.total
      (Option.value ~default:"" e.note)
  )

(* ======== CROWD SCRAPER ======== *)

(** Crowd (attendance) entry type *)
type crowd_entry = {
  crowd_season: string;
  samsung_life: string;
  shinhan_bank: string;
  woori_bank: string;
  hana_bank: string;
  bnk_sum: string;
  kb_stars: string;
  neutral: string;
  total: string;
}

(** Parse crowd tables from HTML *)
let parse_crowd_html html =
  let soup = Soup.parse html in
  (* Find all table rows with class table_body *)
  let rows = soup |> Soup.select ".table_body" |> Soup.to_list in
  rows |> List.filter_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    | season :: sam :: shin :: woori :: hana :: bnk :: kb :: neutral :: total :: _ ->
        let season_name = get_all_text season in
        if String.length season_name > 0 then
          Some {
            crowd_season = season_name;
            samsung_life = get_all_text sam;
            shinhan_bank = get_all_text shin;
            woori_bank = get_all_text woori;
            hana_bank = get_all_text hana;
            bnk_sum = get_all_text bnk;
            kb_stars = get_all_text kb;
            neutral = get_all_text neutral;
            total = get_all_text total;
          }
        else None
    | _ -> None
  )

(** Fetch crowd data *)
let fetch_crowd ~sw ~env =
  let url = Printf.sprintf "%s/history/crowd.asp" base_url in
  Printf.printf "Fetching crowd (attendance) data...\n%!";
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_crowd_html html

(** Print crowd as CSV *)
let print_crowd_csv entries =
  Printf.printf "season,samsung_life,shinhan_bank,woori_bank,hana_bank,bnk_sum,kb_stars,neutral,total\n";
  entries |> List.iter (fun e ->
    Printf.printf "%s,\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
      e.crowd_season
      e.samsung_life
      e.shinhan_bank
      e.woori_bank
      e.hana_bank
      e.bnk_sum
      e.kb_stars
      e.neutral
      e.total
  )

(* ======== MAJOR RECORDS SCRAPER ======== *)

(** Major team record entry *)
type major_record = {
  category: string;         (* 정규리그 한 경기 팀 최다 득점 *)
  league_name: string;      (* 대회명 *)
  date: string;             (* 날짜 *)
  record_value: string;     (* 기록 *)
  team1: string;            (* 팀1 *)
  team2: string;            (* 팀2 *)
}

(** Check if element has a specific class *)
let has_class cls node =
  Soup.attribute "class" node
  |> Option.map (fun s -> String.split_on_char ' ' s |> List.mem cls)
  |> Option.value ~default:false

(** Parse major records table *)
let parse_major_records_html html =
  let soup = Soup.parse html in
  let rows = soup |> Soup.select "tbody tr" |> Soup.to_list in
  let current_category = ref "" in
  rows |> List.filter_map (fun row ->
    let cells = row |> Soup.select "td" |> Soup.to_list in
    match cells with
    (* Row with category (has rowspan or bg_gray class) *)
    | cat :: league :: date :: record :: team1 :: team2 :: _
      when Soup.has_attribute "rowspan" cat || has_class "bg_gray" cat ->
        let cat_text = get_all_text cat in
        current_category := cat_text;
        Some {
          category = cat_text;
          league_name = get_all_text league;
          date = get_all_text date;
          record_value = get_all_text record;
          team1 = get_all_text team1;
          team2 = get_all_text team2;
        }
    (* Continuation row (no category cell due to rowspan) *)
    | league :: date :: record :: team1 :: team2 :: _
      when String.length !current_category > 0 ->
        Some {
          category = !current_category;
          league_name = get_all_text league;
          date = get_all_text date;
          record_value = get_all_text record;
          team1 = get_all_text team1;
          team2 = get_all_text team2;
        }
    | _ -> None
  )

(** Fetch major records *)
let fetch_major_records ~sw ~env =
  let url = Printf.sprintf "%s/history/major_team.asp" base_url in
  Printf.printf "Fetching major records...\n%!";
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    []
  else
    parse_major_records_html html

(** Print major records as CSV *)
let print_major_records_csv entries =
  Printf.printf "category,league_name,date,record_value,team1,team2\n";
  entries |> List.iter (fun e ->
    Printf.printf "\"%s\",\"%s\",%s,\"%s\",\"%s\",\"%s\"\n"
      e.category
      e.league_name
      e.date
      e.record_value
      e.team1
      e.team2
  )

(* ======== DATALAB SCRAPER ======== *)

(** DataLab base URL *)
let datalab_url = "https://datalab.wkbl.or.kr"

(** Matchup indices to try when scraping DataLab.
    The DataLab uses URL format: {season}01{index}1
    - "01" is fixed (analysis type)
    - index ranges from 01-15 (tested: 01-08 return data, 09+ return HTTP 500)
    - Actual team codes come from the JSON response, not URL
    - 15 indices covers 6 teams × 5 opponents / 2 = 15 unique matchups
*)
let max_matchup_index = 15

(** Team codes in DataLab JSON response
    These are DIFFERENT from URL codes!
    Discovered through winnerTeamCode/winnerTeamName matching.
*)
let team_codes_json = [
  ("01", "KB스타즈");
  ("03", "삼성생명");
  ("05", "우리은행");
  ("07", "신한은행");
  ("09", "하나은행");
  ("10", "하나원큐");  (* Alias for 하나은행 *)
  ("11", "BNK썸");
]

(** Get team name from JSON code *)
let team_name_from_code code =
  List.assoc_opt code team_codes_json |> Option.value ~default:code

(** Comprehensive team name → code mapping (includes historical teams)
    Used for syncing scraped schedule data to database *)
let team_name_to_code = [
  (* Modern teams (active) *)
  ("KB스타즈", "01"); ("KB 스타즈", "01");
  ("삼성생명", "03");
  ("우리은행", "05");
  ("신한은행", "07");
  ("KDB생명", "08"); ("KDB 생명", "08"); ("KDB", "08");
  ("하나은행", "09"); ("하나원큐", "09");
  ("OK저축은행", "10"); ("OK 저축은행", "10"); ("OK", "10");
  ("BNK썸", "11"); ("BNK 썸", "11"); ("BNK", "11");
  (* Historical teams (defunct/renamed) *)
  ("금호생명", "02");  (* 2003-2011, became 하나외환 *)
  ("하나외환", "02");  (* franchise name used in some seasons *)
  ("신세계", "04");    (* 1998-2011 *)
  ("현대", "06");      (* 1998-2011, became 하나외환 *)
  ("국민은행", "01");  (* Pre-KB era name *)
  ("삼성화재", "03");  (* Alternate name *)
  ("농협", "08");      (* 2003-2004 briefly *)
  ("LG", "12");        (* 1998-2001 *)
  ("한화", "13");      (* 1998-2000 *)
  (* All-star / special events (teams table has 83/84/87/88/91/92) *)
  ("한국 올스타", "83"); ("한국올스타", "83");
  ("일본 올스타", "84"); ("일본올스타", "84");
  ("핑크스타", "87");
  ("블루스타", "88");
  ("남부선발", "91"); ("남부 선발", "91");
  ("중부선발", "92"); ("중부 선발", "92");
]

(** Normalize schedule team name strings for code lookup.
    Keeps this simple (no regex deps) since inputs are short. *)
let normalize_team_name s =
  let s = String.trim s in
  let b = Buffer.create (String.length s) in
  let rec loop i in_space =
    if i >= String.length s then
      ()
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' ->
          if in_space then
            loop (i + 1) true
          else (
            Buffer.add_char b ' ';
            loop (i + 1) true
          )
      | c ->
          Buffer.add_char b c;
          loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents b

(** Domain.team_code_of_string returns alpha codes (WO/KB/SS/...)
    while the DB uses numeric codes (01/03/05/...). *)
let numeric_code_of_domain_team_code = function
  | "KB" -> Some "01"
  | "SS" -> Some "03"
  | "WO" -> Some "05"
  | "SH" -> Some "07"
  | "KD" -> Some "08"
  | "HN" -> Some "09"
  | "OK" -> Some "10"
  | "BN" -> Some "11"
  | "GH" -> Some "02"
  | "SG" -> Some "04"
  | "HD" -> Some "06"
  | _ -> None

(** Convert team name to code for database sync *)
let code_from_team_name name =
  let name = normalize_team_name name in
  match List.assoc_opt name team_name_to_code with
  | Some code -> code
  | None ->
      (* Fallback: Domain matching (more tolerant), then map alpha→numeric *)
      (match Domain.team_code_of_string name with
      | Some alpha_code ->
          (match numeric_code_of_domain_team_code alpha_code with
          | Some code -> code
          | None -> Printf.sprintf "XX_%s" (String.sub name 0 (min 3 (String.length name))))
      | None -> Printf.sprintf "XX_%s" (String.sub name 0 (min 3 (String.length name))))

(** DataLab uses different season codes (offset by +2 from main site)
    Main site: 044 = 2025-2026
    DataLab:   046 = 2025-2026

    @deprecated Use [datalab_season_codes_list ()] from unified_seasons instead.
    Or use [main_to_datalab] / [datalab_to_main] for code conversion.
*)
let datalab_season_codes = [
  ("046", "2025-2026");
  ("045", "2024-2025");
  ("044", "2023-2024");
  ("043", "2022-2023");
  ("042", "2021-2022");
  ("041", "2020-2021");
  ("040", "2019-2020");
  ("039", "2018-2019");
  ("038", "2017-2018");
  ("037", "2016-2017");
  ("036", "2015-2016");
  ("035", "2014-2015");
  ("034", "2013-2014");
  ("033", "2012-2013");
  ("032", "2011-2012");
]

(** Game record from matchRecordList *)
type game_record = {
  game_id: string;
  game_season: string;
  game_date: string;
  court_name: string;
  home_team_name: string;
  away_team_name: string;
  home_team_score: int;
  away_team_score: int;
  home_q1: int;
  home_q2: int;
  home_q3: int;
  home_q4: int;
  home_ext: int;
  away_q1: int;
  away_q2: int;
  away_q3: int;
  away_q4: int;
  away_ext: int;
  winner_team_name: string;
}

(** Team statistics from homeTeamStatistics/awayTeamStatistics *)
type team_stat = {
  ts_season: string;
  ts_team_name: string;
  opponent_team_name: string;
  score_avg: float;
  reb_avg: float;
  ast_avg: float;
  stl_avg: float;
  blk_avg: float;
  fg_pct: float;
  three_pct: float;
  ft_pct: float;
}

(** Head-to-head versus record from versusList *)
type versus_record = {
  vs_season: string;
  vs_season_name: string;
  vs_home_team: string;
  vs_away_team: string;
  home_win: int;
  home_lose: int;
}

(** Safe JSON field extraction helpers *)
let json_string_opt json key =
  try
    match Yojson.Safe.Util.member key json with
    | `String s -> Some s
    | `Null -> None
    | _ -> None
  with _ -> None

let json_string json key =
  json_string_opt json key |> Option.value ~default:""

let json_int json key =
  try
    match Yojson.Safe.Util.member key json with
    | `Int i -> i
    | `String s -> int_of_string_opt s |> Option.value ~default:0
    | _ -> 0
  with _ -> 0

let json_float json key =
  try
    match Yojson.Safe.Util.member key json with
    | `Float f -> f
    | `Int i -> float_of_int i
    | `String s -> float_of_string_opt s |> Option.value ~default:0.0
    | _ -> 0.0
  with _ -> 0.0

(** Extract JSON data from DataLab page HTML *)
let extract_datalab_json html =
  (* Look for pattern: var dataString = JSON.parse('...'); *)
  let pattern = Str.regexp {|JSON\.parse('\([^']*\)')|} in
  try
    let _ = Str.search_forward pattern html 0 in
    let json_str = Str.matched_group 1 html in
    (* Unescape the JSON string *)
    let unescaped =
      json_str
      |> Str.global_replace (Str.regexp {|\\"|}) "\""
      |> Str.global_replace (Str.regexp {|\\'|}) "'"
      |> Str.global_replace (Str.regexp {|\\\\|}) "\\"
    in
    Some (Yojson.Safe.from_string unescaped)
  with
  | Not_found -> None
  | Yojson.Json_error msg ->
      Printf.eprintf "JSON parse error: %s\n" msg;
      None

(** Parse game records from matchRecordList JSON array *)
let parse_game_records json =
  try
    let records = Yojson.Safe.Util.member "matchRecordList" json in
    match records with
    | `List items ->
        items |> List.filter_map (fun item ->
          let game_id = json_string item "gameID" in
          if String.length game_id > 0 then
            let home_code = json_string item "homeTeamCode" in
            let away_code = json_string item "awayTeamCode" in
            Some {
              game_id;
              game_season = json_string item "season";
              game_date = json_string item "gameDate";
              court_name = json_string item "courtName";
              home_team_name = team_name_from_code home_code;
              away_team_name = team_name_from_code away_code;
              home_team_score = json_int item "homeTeamScore";
              away_team_score = json_int item "awayTeamScore";
              home_q1 = json_int item "homeTeamScoreQ1";
              home_q2 = json_int item "homeTeamScoreQ2";
              home_q3 = json_int item "homeTeamScoreQ3";
              home_q4 = json_int item "homeTeamScoreQ4";
              home_ext = json_int item "homeTeamScoreEQ";  (* EQ not Ext in JSON *)
              away_q1 = json_int item "awayTeamScoreQ1";
              away_q2 = json_int item "awayTeamScoreQ2";
              away_q3 = json_int item "awayTeamScoreQ3";
              away_q4 = json_int item "awayTeamScoreQ4";
              away_ext = json_int item "awayTeamScoreEQ";  (* EQ not Ext in JSON *)
              winner_team_name = json_string item "winnerTeamName";
            }
          else None
        )
    | _ -> []
  with _ -> []

(** Extract home and away team names from DataLab JSON
    Returns (home_team_name, away_team_name) option
*)
let extract_team_names json =
  try
    let home_stats = Yojson.Safe.Util.member "homeTeamWholeStatistics" json in
    let away_stats = Yojson.Safe.Util.member "awayTeamWholeStatistics" json in
    let home_code = json_string home_stats "teamCode" in
    let away_code = json_string away_stats "teamCode" in
    if String.length home_code > 0 && String.length away_code > 0 then
      let home_name = List.assoc_opt home_code team_codes_json
        |> Option.value ~default:home_code in
      let away_name = List.assoc_opt away_code team_codes_json
        |> Option.value ~default:away_code in
      Some (home_name, away_name)
    else
      None
  with _ -> None

(** Parse team statistics from JSON *)
let parse_team_stats ~season ~home_team ~away_team json =
  let parse_stat_obj key team opponent =
    try
      let obj = Yojson.Safe.Util.member key json in
      match obj with
      | `Assoc _ ->
          Some {
            ts_season = season;
            ts_team_name = team;
            opponent_team_name = opponent;
            score_avg = json_float obj "scoreAvg";
            reb_avg = json_float obj "rebAvg";
            ast_avg = json_float obj "astAvg";
            stl_avg = json_float obj "stlAvg";
            blk_avg = json_float obj "blkAvg";
            fg_pct = json_float obj "fgPct";
            three_pct = json_float obj "threePct";
            ft_pct = json_float obj "ftPct";
          }
      | _ -> None
    with _ -> None
  in
  let home_stats = parse_stat_obj "homeTeamStatistics" home_team away_team in
  let away_stats = parse_stat_obj "awayTeamStatistics" away_team home_team in
  List.filter_map Fun.id [home_stats; away_stats]

(** Parse versus records from versusList *)
let parse_versus_records ~home_team ~away_team json =
  try
    let records = Yojson.Safe.Util.member "versusList" json in
    match records with
    | `List items ->
        items |> List.filter_map (fun item ->
          let season = json_string item "season" in
          if String.length season > 0 then
            Some {
              vs_season = season;
              vs_season_name = json_string item "seasonName";
              vs_home_team = home_team;
              vs_away_team = away_team;
              home_win = json_int item "homeWin";
              home_lose = json_int item "homeLose";
            }
          else None
        )
    | _ -> []
  with _ -> []

(** Fetch DataLab team analysis page by matchup index
    URL format: teamAnalysis?id={season}01{index}1
    - season = 3-digit season code (e.g., 046)
    - "01" = fixed analysis type
    - index = 2-digit matchup index (01-30)
    - "1" = trailing digit (always 1)
    Example: 04601031 = season 046, matchup index 03
*)
let fetch_datalab_by_index ~sw ~env ~season_code ~matchup_index =
  let id = Printf.sprintf "%s01%02d1" season_code matchup_index in
  let url = Printf.sprintf "%s/teamAnalysis?id=%s" datalab_url id in
  Printf.printf "  Fetching DataLab: season=%s index=%02d (id=%s)...\n%!"
    season_code matchup_index id;
  let html = fetch_url ~sw ~env url in
  if String.length html = 0 then
    None
  else
    extract_datalab_json html

(** Generate matchup indices to try (1 to max_matchup_index) *)
let all_matchup_indices () =
  List.init max_matchup_index (fun i -> i + 1)

(** Fetch all games for a season from DataLab *)
let fetch_season_games ~sw ~env season_code =
  let clock = Eio.Stdenv.clock env in
  let indices = all_matchup_indices () in
  let seen_games = Hashtbl.create 256 in  (* Deduplicate games *)
  let rec fetch_all acc = function
    | [] -> acc
    | idx :: rest ->
        let json_opt = fetch_datalab_by_index ~sw ~env ~season_code ~matchup_index:idx in
        let games = match json_opt with
          | Some json ->
              parse_game_records json
              |> List.filter (fun g ->
                  (* Filter by season and deduplicate *)
                  g.game_season = season_code &&
                  not (Hashtbl.mem seen_games g.game_id) &&
                  begin
                    Hashtbl.add seen_games g.game_id true;
                    true
                  end
                )
          | None -> []
        in
        (* Rate limiting: 200ms delay *)
        Eio.Time.sleep clock 0.2;
        fetch_all (games @ acc) rest
  in
  fetch_all [] indices

(** Fetch all games for multiple seasons *)
let fetch_all_games ~sw ~env ?(seasons=datalab_season_codes) () =
  Printf.printf "Fetching games from DataLab (%d seasons)...\n%!" (List.length seasons);
  let rec fetch_all acc = function
    | [] -> List.rev acc
    | (code, name) :: rest ->
        Printf.printf "\n=== Season %s (%s) ===\n%!" name code;
        let games = fetch_season_games ~sw ~env code in
        Printf.printf "  Found %d games\n%!" (List.length games);
        fetch_all (games @ acc) rest
  in
  fetch_all [] seasons

(** Fetch team stats for all matchups in a season *)
let fetch_season_team_stats ~sw ~env season_code =
  let clock = Eio.Stdenv.clock env in
  let indices = all_matchup_indices () in
  let seen_matchups = Hashtbl.create 64 in  (* Deduplicate matchups *)
  let rec fetch_all acc = function
    | [] -> acc
    | idx :: rest ->
        let json_opt = fetch_datalab_by_index ~sw ~env ~season_code ~matchup_index:idx in
        let stats = match json_opt with
          | Some json ->
              (match extract_team_names json with
               | Some (home_name, away_name) ->
                   let key = home_name ^ "vs" ^ away_name in
                   if Hashtbl.mem seen_matchups key then []
                   else begin
                     Hashtbl.add seen_matchups key true;
                     parse_team_stats ~season:season_code ~home_team:home_name ~away_team:away_name json
                   end
               | None -> [])
          | None -> []
        in
        Eio.Time.sleep clock 0.2;
        fetch_all (stats @ acc) rest
  in
  fetch_all [] indices

(** Fetch all team stats for multiple seasons *)
let fetch_all_team_stats ~sw ~env ?(seasons=datalab_season_codes) () =
  Printf.printf "Fetching team stats from DataLab (%d seasons)...\n%!" (List.length seasons);
  let rec fetch_all acc = function
    | [] -> List.rev acc
    | (code, name) :: rest ->
        Printf.printf "\n=== Season %s (%s) ===\n%!" name code;
        let stats = fetch_season_team_stats ~sw ~env code in
        Printf.printf "  Found %d stat entries\n%!" (List.length stats);
        fetch_all (stats @ acc) rest
  in
  fetch_all [] seasons

(** Fetch versus records for all matchups in a season *)
let fetch_season_versus_records ~sw ~env season_code =
  let clock = Eio.Stdenv.clock env in
  let indices = all_matchup_indices () in
  let seen_matchups = Hashtbl.create 64 in  (* Deduplicate matchups *)
  let rec fetch_all acc = function
    | [] -> acc
    | idx :: rest ->
        let json_opt = fetch_datalab_by_index ~sw ~env ~season_code ~matchup_index:idx in
        let records = match json_opt with
          | Some json ->
              (match extract_team_names json with
               | Some (home_name, away_name) ->
                   let key = home_name ^ "vs" ^ away_name in
                   if Hashtbl.mem seen_matchups key then []
                   else begin
                     Hashtbl.add seen_matchups key true;
                     parse_versus_records ~home_team:home_name ~away_team:away_name json
                   end
               | None -> [])
          | None -> []
        in
        Eio.Time.sleep clock 0.2;
        fetch_all (records @ acc) rest
  in
  fetch_all [] indices

(** Print game records as CSV *)
let print_games_csv games =
  Printf.printf "game_id,season,date,court,home_team,away_team,home_score,away_score,home_q1,home_q2,home_q3,home_q4,home_ext,away_q1,away_q2,away_q3,away_q4,away_ext,winner\n";
  games |> List.iter (fun g ->
    Printf.printf "%s,%s,%s,\"%s\",%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s\n"
      g.game_id
      g.game_season
      g.game_date
      g.court_name
      g.home_team_name
      g.away_team_name
      g.home_team_score
      g.away_team_score
      g.home_q1 g.home_q2 g.home_q3 g.home_q4 g.home_ext
      g.away_q1 g.away_q2 g.away_q3 g.away_q4 g.away_ext
      g.winner_team_name
  )

(** Print team stats as CSV *)
let print_team_stats_csv stats =
  Printf.printf "season,team,opponent,score_avg,reb_avg,ast_avg,stl_avg,blk_avg,fg_pct,three_pct,ft_pct\n";
  stats |> List.iter (fun s ->
    Printf.printf "%s,%s,%s,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f\n"
      s.ts_season
      s.ts_team_name
      s.opponent_team_name
      s.score_avg
      s.reb_avg
      s.ast_avg
      s.stl_avg
      s.blk_avg
      s.fg_pct
      s.three_pct
      s.ft_pct
  )

(** Print versus records as CSV *)
let print_versus_csv records =
  Printf.printf "season,season_name,home_team,away_team,home_win,home_lose\n";
  records |> List.iter (fun v ->
    Printf.printf "%s,%s,%s,%s,%d,%d\n"
      v.vs_season
      v.vs_season_name
      v.vs_home_team
      v.vs_away_team
      v.home_win
      v.home_lose
  )

(* ============================================================
   CHAMPIONSHIP & ALL-STAR HISTORY SCRAPERS
   ============================================================ *)

(** Championship record type *)
type championship_record = {
  champ_season: string;        (* e.g., "2024-2025" *)
  champ_edition: int;          (* e.g., 28 *)
  champion_team: string;       (* e.g., "BNK 썸" *)
  runner_up_team: string;      (* e.g., "우리은행" *)
  finals_result: string;       (* e.g., "3승 0패" *)
  regular_champion: string;    (* Regular season winner *)
}
[@@deriving show, eq]

(** All-star record type *)
type allstar_record = {
  as_edition: int;            (* e.g., 24 *)
  as_season: string;          (* e.g., "2025~2026" *)
  as_date: string;            (* e.g., "2020. 1. 12" *)
  as_venue: string;           (* e.g., "부산사직실내체육관" *)
  as_mvp: string;             (* MVP player name *)
}
[@@deriving show, eq]

(** Extract text from td, preferring data-kr attribute *)
let extract_td_text td =
  let data_kr = Soup.attribute "data-kr" td in
  match data_kr with
  | Some kr when String.length kr > 0 -> String.trim kr
  | _ ->
      (* Try to get text from nested span with data-kr *)
      let span = Soup.select_one "span.language" td in
      match span with
      | Some s ->
          (match Soup.attribute "data-kr" s with
           | Some kr -> String.trim kr
           | None -> Soup.leaf_text td |> Option.value ~default:"" |> String.trim)
      | None -> Soup.leaf_text td |> Option.value ~default:"" |> String.trim

(** Fetch championship history from WKBL website *)
let fetch_championship_history ~sw ~env =
  Printf.printf "Fetching championship history...\n%!";
  let url = "https://www.wkbl.or.kr/history/league_champion.asp" in
  let html = fetch_url ~sw ~env url in
  let soup = Soup.parse html in

  let records = ref [] in
  let edition = ref 0 in

  (* Find tbody rows - each championship starts with a row that has 6 TDs *)
  let rows = Soup.select "tbody tr" soup in
  rows |> Soup.iter (fun row ->
    let tds = Soup.select "td" row |> Soup.to_list in
    (* First row of each championship has 6 cells (with rowspans) *)
    match
      List.nth_opt tds 0,
      List.nth_opt tds 1,
      List.nth_opt tds 2,
      List.nth_opt tds 4,
      List.nth_opt tds 5
    with
    | Some season_td, Some matchup_td, Some finals_td, Some champion_td, Some regular_td ->
        incr edition;
        let season = extract_td_text season_td in
        let finals_result = extract_td_text finals_td in
        let champion = extract_td_text champion_td in
        let regular_champ = extract_td_text regular_td in

        (* Extract runner-up from matchup (td 1) - format: "TeamA : TeamB" *)
        let home_span = Soup.select_one "span.home_team" matchup_td in
        let away_span = Soup.select_one "span.away_team" matchup_td in
        let home_team = match home_span with
          | Some s -> Soup.attribute "data-kr" s |> Option.value ~default:""
          | None -> "" in
        let away_team = match away_span with
          | Some s -> Soup.attribute "data-kr" s |> Option.value ~default:""
          | None -> "" in

        (* Determine runner-up: the team that's not the champion *)
        let runner_up =
          if String.equal champion home_team then away_team
          else if String.equal champion away_team then home_team
          else if String.length home_team > 0 then home_team
          else away_team
        in

        (* Skip if no champion (e.g., COVID cancelled season) *)
        if String.length champion > 0 && not (String.equal champion "-") then
          records := {
            champ_season = season;
            champ_edition = !edition;
            champion_team = champion;
            runner_up_team = runner_up;
            finals_result;
            regular_champion = regular_champ;
          } :: !records
    | _ -> ()
  );

  Printf.printf "  Found %d championship records\n%!" (List.length !records);
  List.rev !records

(** Fetch all-star history from WKBL website *)
let fetch_allstar_history ~sw ~env =
  Printf.printf "Fetching all-star history...\n%!";
  let url = "https://www.wkbl.or.kr/history/league_allstar.asp" in
  let html = fetch_url ~sw ~env url in
  let soup = Soup.parse html in

  (* First, collect edition/season pairs from the summary table (2 columns) *)
  let edition_seasons = ref [] in
  let tables = Soup.select "table" soup |> Soup.to_list in

  (* First table has edition + season *)
  (match tables with
   | first_table :: _ ->
       let rows = Soup.select "tbody tr" first_table in
       rows |> Soup.iter (fun row ->
         let tds = Soup.select "td" row |> Soup.to_list in
         if List.length tds = 2 then begin
           match List.nth_opt tds 0, List.nth_opt tds 1 with
           | Some edition_td, Some season_td ->
               let edition_text = Soup.leaf_text edition_td |> Option.value ~default:"" |> String.trim in
               let season_text = Soup.leaf_text season_td |> Option.value ~default:"" |> String.trim in
               (match int_of_string_opt edition_text with
               | Some edition -> edition_seasons := (edition, season_text) :: !edition_seasons
               | None -> ())
           | _ -> ()
         end
       )
   | [] -> ()
  );

  (* Reverse to get ascending order *)
  edition_seasons := List.rev !edition_seasons;

  (* Second table (or later) has detailed data: venue, home, score, away, mvp, etc. *)
  let records = ref [] in
  let detail_idx = ref 0 in

  tables |> List.iter (fun table ->
    let rows = Soup.select "tbody tr" table in
    rows |> Soup.iter (fun row ->
      let tds = Soup.select "td" row |> Soup.to_list in
      (* Detail table has 7 columns: venue, home, score, away, mvp, scorer, best_perf *)
      if List.length tds >= 5 then begin
        let venue = List.nth_opt tds 0 |> Option.map extract_td_text |> Option.value ~default:"" in
        let mvp = List.nth_opt tds 4 |> Option.map extract_td_text |> Option.value ~default:"" in

        (* Get edition/season from the collected list *)
        let (edition, season) =
          match List.nth_opt !edition_seasons !detail_idx with
          | Some x -> x
          | None -> (0, "")
        in
        incr detail_idx;

        if edition > 0 then
          records := {
            as_edition = edition;
            as_season = season;
            as_date = "";  (* Date not available in current HTML *)
            as_venue = venue;
            as_mvp = mvp;
          } :: !records
      end
    )
  );

  Printf.printf "  Found %d all-star records\n%!" (List.length !records);
  List.rev !records

(** Print championship history as CSV *)
let print_championship_csv records =
  Printf.printf "edition,season,champion,runner_up,finals_result,regular_champion\n";
  records |> List.iter (fun r ->
    Printf.printf "%d,\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
      r.champ_edition
      r.champ_season
      r.champion_team
      r.runner_up_team
      r.finals_result
      r.regular_champion
  )

(** Print all-star history as CSV *)
let print_allstar_csv records =
  Printf.printf "edition,season,date,venue,mvp\n";
  records |> List.iter (fun r ->
    Printf.printf "%d,\"%s\",\"%s\",\"%s\",\"%s\"\n"
      r.as_edition
      r.as_season
      r.as_date
      r.as_venue
      r.as_mvp
  )

(* ============================================================
   DATA ANALYSIS FUNCTIONS
   ============================================================ *)

(** Count occurrences in a list and return sorted by count desc *)
let count_occurrences items =
  let tbl = Hashtbl.create 16 in
  items |> List.iter (fun item ->
    let count = try Hashtbl.find tbl item with Not_found -> 0 in
    Hashtbl.replace tbl item (count + 1)
  );
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (_, a) (_, b) -> compare b a)

(** Analyze championship records - team statistics *)
let analyze_championships records =
  (* Count championships won *)
  let champions = records |> List.map (fun r -> r.champion_team)
    |> List.filter (fun t -> String.length t > 0) in
  let champion_counts = count_occurrences champions in

  (* Count runner-up finishes *)
  let runners_up = records |> List.map (fun r -> r.runner_up_team)
    |> List.filter (fun t -> String.length t > 0) in
  let runner_up_counts = count_occurrences runners_up in

  (* Count regular season wins *)
  let regular_champs = records |> List.map (fun r -> r.regular_champion)
    |> List.filter (fun t -> String.length t > 0) in
  let regular_counts = count_occurrences regular_champs in

  (champion_counts, runner_up_counts, regular_counts)

(** Print championship analysis *)
let print_championship_analysis records =
  let (champs, runners, regulars) = analyze_championships records in

  Printf.printf "\n📊 WKBL 챔피언십 통계 (총 %d시즌)\n" (List.length records);
  Printf.printf "════════════════════════════════════════\n\n";

  Printf.printf "🏆 역대 우승 횟수:\n";
  Printf.printf "────────────────────────────────────────\n";
  champs |> List.iter (fun (team, count) ->
    let bar = String.make (count * 2) '#' in
    Printf.printf "  %-12s %2d회 %s\n" team count bar
  );

  Printf.printf "\n🥈 역대 준우승 횟수:\n";
  Printf.printf "────────────────────────────────────────\n";
  runners |> List.iter (fun (team, count) ->
    Printf.printf "  %-12s %2d회\n" team count
  );

  Printf.printf "\n🎯 정규시즌 1위 횟수:\n";
  Printf.printf "────────────────────────────────────────\n";
  regulars |> List.iter (fun (team, count) ->
    Printf.printf "  %-12s %2d회\n" team count
  );

  Printf.printf "\n"

(** Extract player name from MVP string like "박지수(KB스타즈)" *)
let extract_player_name mvp_str =
  let str = String.trim mvp_str in
  if String.length str = 0 || String.equal str "-" then None
  else
    (* Handle multiple MVPs separated by <br> or <br/> *)
    let first_mvp =
      try
        let br_pos = Str.search_forward (Str.regexp_string "<br") str 0 in
        String.sub str 0 br_pos
      with Not_found -> str
    in
    (* Extract name before parenthesis *)
    try
      let paren_pos = String.index first_mvp '(' in
      Some (String.sub first_mvp 0 paren_pos |> String.trim)
    with Not_found -> Some (String.trim first_mvp)

(** Analyze all-star MVP records *)
let analyze_allstar_mvps records =
  let mvps = records
    |> List.filter_map (fun r -> extract_player_name r.as_mvp) in
  count_occurrences mvps

(** Print all-star MVP analysis *)
let print_allstar_analysis records =
  let mvp_counts = analyze_allstar_mvps records in

  Printf.printf "\n⭐ WKBL 올스타 MVP 통계 (총 %d회)\n" (List.length records);
  Printf.printf "════════════════════════════════════════\n\n";

  Printf.printf "🌟 다회 MVP 수상자:\n";
  Printf.printf "────────────────────────────────────────\n";
  mvp_counts |> List.iter (fun (player, count) ->
    if count >= 2 then
      Printf.printf "  %-15s %2d회\n" player count
  );

  Printf.printf "\n📍 역대 개최지:\n";
  Printf.printf "────────────────────────────────────────\n";
  let venues = records |> List.map (fun r -> r.as_venue)
    |> List.filter (fun v -> String.length v > 0) in
  let venue_counts = count_occurrences venues in
  venue_counts |> List.iter (fun (venue, count) ->
    Printf.printf "  %-20s %2d회\n" venue count
  );

  Printf.printf "\n"

(** =========================== SCHEDULE SCRAPER ========================== *)

(** Schedule entry type *)
type schedule_entry = {
  sch_game_id: string option;    (* e.g., "046-01-40" *)
  sch_game_type: string option;  (* "01" regular, "02" playoff, "10" special *)
  sch_game_no: int option;       (* game_no from WKBL result link *)
  sch_date: string;              (* e.g., "2026-01-25" *)
  sch_day: string;               (* e.g., "일" *)
  sch_time: string;              (* e.g., "16:00" *)
  sch_home_team: string;         (* Home team name *)
  sch_away_team: string;         (* Away team name *)
  sch_home_team_code: string option; (* Numeric team code when available (e.g., "09") *)
  sch_away_team_code: string option; (* Numeric team code when available (e.g., "03") *)
  sch_home_score: int option;    (* Home score (None if not played) *)
  sch_away_score: int option;    (* Away score (None if not played) *)
  sch_venue: string;             (* e.g., "부천체육관" *)
  sch_season: string;            (* e.g., "046" *)
}

(** Extract game_type and game_no from WKBL result link *)
let game_params_of_href href =
  let uri = Uri.of_string href in
  let game_type = Uri.get_query_param uri "game_type" in
  let game_no =
    let opt = Uri.get_query_param uri "game_no" in
    Option.bind opt int_of_string_opt
  in
  (game_type, game_no)

(** Extract (game_type, game_no) from schedule row action links. *)
let extract_game_meta_from_schedule_row row =
  let open Soup in
  let parse_js_call s =
    let try_re re =
      try
        let _ = Str.search_forward re s 0 in
        let gt = Str.matched_group 1 s in
        let gn = Str.matched_group 2 s |> int_of_string_opt in
        (Some gt, gn)
      with Not_found -> (None, None)
    in
    (* Examples:
       - goTodayGame('01', 62);
       - goLive('01', 62); *)
    let r1 = Str.regexp "goTodayGame('\\([0-9][0-9]\\)'[ ]*,[ ]*\\([0-9]+\\))" in
    let r2 = Str.regexp "goLive('\\([0-9][0-9]\\)'[ ]*,[ ]*\\([0-9]+\\))" in
    match try_re r1 with
    | (Some _ as gt, gn) -> (gt, gn)
    | (None, None) -> try_re r2
    | other -> other
  in
  let parse_datalab_id id =
    (* id format: {season(3)}{game_type(2)}{game_no(rest)} *)
    if String.length id >= 8 then
      let game_type = String.sub id 3 2 in
      let game_no = String.sub id 5 (String.length id - 5) |> int_of_string_opt in
      (Some game_type, game_no)
    else
      (None, None)
  in
  (* 1) result.asp link *)
  match row $? "a[href*='result.asp']" with
  | Some a ->
      let href = attribute "href" a |> Option.value ~default:"" |> String.trim in
      if href = "" then (None, None) else game_params_of_href href
  | None -> (
      (* 2) DataLab link: http://datalab.wkbl.or.kr/?id=04601062 *)
      match row $? "a[href*='datalab.wkbl.or.kr']" with
      | Some a ->
          let href = attribute "href" a |> Option.value ~default:"" |> String.trim in
          if href = "" then (None, None)
          else
            let uri = Uri.of_string href in
            let id_opt = Uri.get_query_param uri "id" in
            (match id_opt with
            | Some id -> parse_datalab_id id
            | None -> (None, None))
      | None -> (
          (* 3) JS onclick fallback *)
          match row $? "a[onclick*='goTodayGame']" with
          | Some a ->
              let s = attribute "onclick" a |> Option.value ~default:"" |> String.trim in
              if s = "" then (None, None) else parse_js_call s
          | None ->
              (match row $? "a[onclick*='goLive']" with
              | Some a ->
                  let s = attribute "onclick" a |> Option.value ~default:"" |> String.trim in
                  if s = "" then (None, None) else parse_js_call s
              | None -> (None, None))
        )
    )

(** Extract numeric team_code from a team logo image src.
    Example: "/static/images/team/teamlogo_03.png" -> Some "03" *)
let team_code_of_teamlogo_src src =
  let re = Str.regexp "teamlogo_\\([0-9][0-9]\\)\\.png" in
  try
    let _ = Str.search_forward re src 0 in
    Some (Str.matched_group 1 src)
  with Not_found -> None

let team_code_from_logo_node node =
  let open Soup in
  match node $? "img[src*='teamlogo_']" with
  | None -> None
  | Some img ->
      let src = attribute "src" img |> Option.value ~default:"" |> String.trim in
      if src = "" then None else team_code_of_teamlogo_src src

(** Build game_id from season_code + game_type + game_no *)
let game_id_of_params ~season_code ~game_type_opt ~game_no_opt =
  match game_type_opt, game_no_opt with
  | Some gt, Some gn -> Some (Printf.sprintf "%s-%s-%d" season_code gt gn)
  | _ -> None

(** Parse schedule HTML from inc_list_1_new.asp
    Returns list of schedule entries
*)
let parse_schedule_html ~season ~ym html =
  let open Soup in
  let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e in
  let soup = parse html in
  let games = ref [] in
  let errors = ref [] in

  let texts_trim node =
    node |> texts |> String.concat "" |> String.trim
  in

  let require_opt ~ctx = function
    | Some x -> Ok x
    | None -> Error ctx
  in

  let parse_score_opt ~date_id row sel =
    match select_one sel row with
    | None -> Ok None
    | Some node ->
        let s = texts_trim node in
        if s = "" then Ok None
        else (
          match int_of_string_opt s with
          | Some v -> Ok (Some v)
          | None -> Error (Printf.sprintf "invalid score (%s id=%s): %S" sel date_id s)
        )
  in

  let parse_row row : (schedule_entry option, string) result =
    let date_id = attribute "id" row |> Option.value ~default:"" in
    if String.length date_id < 8 then Ok None
    else
	      let date =
	        Printf.sprintf "%s-%s-%s"
	          (String.sub date_id 0 4)
	          (String.sub date_id 4 2)
	          (String.sub date_id 6 2)
	      in
	      let tds = row |> select "td" |> to_list in
	      match List.nth_opt tds 0, List.nth_opt tds 2, List.nth_opt tds 3 with
	      | Some day_cell, Some venue_cell, Some time_cell ->
	          let* day_span =
	            select_one "span.language" day_cell
	            |> require_opt ~ctx:(Printf.sprintf "missing day span (id=%s)" date_id)
	          in
	          let* away_team_node =
	            select_one ".info_team.away .team_name" row
	            |> require_opt ~ctx:(Printf.sprintf "missing away team (id=%s)" date_id)
	          in
	          let* home_team_node =
	            select_one ".info_team.home .team_name" row
	            |> require_opt ~ctx:(Printf.sprintf "missing home team (id=%s)" date_id)
	          in
	          let away_team_code =
	            match select_one ".info_team.away" row with
	            | None -> None
	            | Some node -> team_code_from_logo_node node
	          in
	          let home_team_code =
	            match select_one ".info_team.home" row with
	            | None -> None
	            | Some node -> team_code_from_logo_node node
	          in
	          let day = texts_trim day_span in
	          let away_team = texts_trim away_team_node in
	          let home_team = texts_trim home_team_node in
	          let venue = texts_trim venue_cell in
	          let time = texts_trim time_cell in
	          let* away_score = parse_score_opt ~date_id row ".info_team.away .txt_score" in
	          let* home_score = parse_score_opt ~date_id row ".info_team.home .txt_score" in

	          let game_type_opt, game_no_opt = extract_game_meta_from_schedule_row row in
	          let game_id_opt = game_id_of_params ~season_code:season ~game_type_opt ~game_no_opt in
	          Ok (Some {
	            sch_game_id = game_id_opt;
            sch_game_type = game_type_opt;
            sch_game_no = game_no_opt;
            sch_date = date;
            sch_day = day;
            sch_time = time;
            sch_home_team = home_team;
            sch_away_team = away_team;
            sch_home_team_code = home_team_code;
            sch_away_team_code = away_team_code;
            sch_home_score = home_score;
            sch_away_score = away_score;
            sch_venue = venue;
            sch_season = season;
          })
      | _ ->
          Error (Printf.sprintf "missing schedule tds (id=%s)" date_id)
  in

  (* Each game is in a <tr id="YYYYMMDD"> *)
  let rows = soup |> select "tbody tr" |> to_list in
  rows |> List.iter (fun row ->
    match parse_row row with
    | Ok None -> ()
    | Ok (Some entry) -> games := entry :: !games
    | Error e -> errors := e :: !errors
  );
  if !errors <> [] then begin
    let errs = List.rev !errors in
    Printf.eprintf "[schedule parse] season=%s ym=%s errors=%d\n%!" season ym (List.length errs);
    let rec print_first n = function
      | [] -> ()
      | _ when n <= 0 -> ()
      | e :: rest ->
          Printf.eprintf "  - %s\n%!" e;
          print_first (n - 1) rest
    in
    print_first 3 errs
  end;
  List.rev !games

(** Fetch schedule for a specific month
    @param ym Year-month in YYYYMM format (e.g., "202601")
    @param season Season code (e.g., "046")
*)
let fetch_schedule_month ~sw ~env ~ym ~season =
  let url = Printf.sprintf
    "https://www.wkbl.or.kr/game/sch/inc_list_1_new.asp?season_gu=%s&ym=%s&viewType=&gun=1"
    season ym in
  Printf.eprintf "  Fetching schedule: %s...\n%!" ym;
  let html = fetch_url ~sw ~env url in
  if String.length html > 0 then
    parse_schedule_html ~season ~ym html
  else
    []

(** Fetch full season schedule (October to March)
    @param season_code e.g., "046" for 2025-2026 season
*)
(** Check if a team name belongs to a recognized WKBL league team (current or historical).

    This uses [Domain.team_code_of_string], which normalizes whitespace quirks
    (NBSP, double spaces) and handles known aliases like "BNK썸".

    We exclude All-Star teams ("AS") because schedule pages can include special
    events that are not part of the regular season dataset. *)
let is_league_team name =
  match Domain.team_code_of_string name with
  | Some "AS" -> false
  | Some _ -> true
  | None -> false

(** Convert DataLab season code to the season start year.

    DataLab season codes follow: 1980 = 001, so start_year = code + 1979. *)
let season_start_year_of_datalab_code (season_code : string) : int option =
  match int_of_string_opt (String.trim season_code) with
  | Some n when n > 0 -> Some (n + 1979)
  | _ -> None

let fetch_season_schedule ~sw ~env ~season_code =
  let start_year =
    season_start_year_of_datalab_code season_code
    |> Option.value ~default:((Unix.localtime (Unix.time ())).Unix.tm_year + 1900)
  in
  let months = [
    (start_year, 10);     (* October *)
    (start_year, 11);     (* November *)
    (start_year, 12);     (* December *)
    (start_year + 1, 1);  (* January *)
    (start_year + 1, 2);  (* February *)
    (start_year + 1, 3);  (* March *)
    (start_year + 1, 4);  (* April - regular season ends early April *)
  ] in
  Printf.printf "Fetching season schedule for %s (%d months)...\n\n" season_code (List.length months);

  months |> List.concat_map (fun (year, month) ->
    let ym = Printf.sprintf "%04d%02d" year month in
    fetch_schedule_month ~sw ~env ~ym ~season:season_code
  )
  |> List.filter (fun e -> is_league_team e.sch_home_team && is_league_team e.sch_away_team)

(** Print schedule as CSV *)
let print_schedule_csv entries =
  Printf.printf "date,day,time,home_team,away_team,home_score,away_score,venue,season\n";
  entries |> List.iter (fun e ->
    Printf.printf "%s,%s,%s,%s,%s,%s,%s,%s,%s\n"
      e.sch_date
      e.sch_day
      e.sch_time
      e.sch_home_team
      e.sch_away_team
      (match e.sch_home_score with Some s -> string_of_int s | None -> "")
      (match e.sch_away_score with Some s -> string_of_int s | None -> "")
      e.sch_venue
      e.sch_season
  )

(* ============================================================
   FULL HISTORY SCRAPER (1998-2026)
   Uses the main site schedule API: /game/sch/inc_list_1_new.asp
   ============================================================ *)

(** Complete season code mapping from 1998 to today.

    Single source of truth lives in [Seasons_catalog]. *)
let all_season_codes = Seasons_catalog.all

(** Get approximate date range for a season
    Returns list of (year, month) tuples to query

    NOTE: Early-era (pre-2007) winter leagues had different schedules:
    - 겨울 1999-2006: Dec (Y-1), Jan-Feb Y, May Y (playoffs)
    - 겨울 2007+: Nov Y → Mar Y+1 (spanning two years) *)
let season_date_range season_name =
  (* Parse season name to determine date range *)
  if String.length season_name >= 9 && season_name.[4] = '-' then
    (* Format: "YYYY-YYYY" - Regular season (Oct to Mar) *)
    let start_year = int_of_string (String.sub season_name 0 4) in
    [(start_year, 10); (start_year, 11); (start_year, 12);
     (start_year + 1, 1); (start_year + 1, 2); (start_year + 1, 3)]
  else if String.length season_name >= 6 then
    let year_str = String.sub season_name 0 4 in
    let year = try int_of_string year_str with _ -> 2000 in
    if String.length season_name > 4 then
      let suffix = String.sub season_name 4 (String.length season_name - 4) in
      if suffix = "여름" then
        (* Summer league: Jun-Sep *)
        [(year, 6); (year, 7); (year, 8); (year, 9)]
      else if suffix = "퓨처스" then
        (* Futures league: Jun-Oct (some like 2006 only have Oct games) *)
        [(year, 6); (year, 7); (year, 8); (year, 9); (year, 10)]
      else if suffix = "겨울" then
        (* Winter league: different pattern based on era *)
        if year <= 2007 then
          (* Early era: Dec(Y-1), Jan-Mar Y, May Y (playoffs) *)
          [(year - 1, 12); (year, 1); (year, 2); (year, 3); (year, 5)]
        else
          (* Modern era (2008+): Nov Y → Mar Y+1 *)
          [(year, 11); (year, 12); (year + 1, 1); (year + 1, 2); (year + 1, 3)]
      else
        (* Default: full year *)
        [(year, 1); (year, 2); (year, 3); (year, 6); (year, 7); (year, 8);
         (year, 10); (year, 11); (year, 12)]
    else
      [(year, 1); (year, 2); (year, 3); (year, 10); (year, 11); (year, 12)]
  else
    []

(** Fetch schedule from new API endpoint
    URL: /game/sch/inc_list_1_new.asp?season_gu={code}&ym={YYYYMM}&viewType=1&gun=1
    Requires Referer header for authentication *)
let fetch_schedule_api ~sw ~env ~season_code ~ym =
  let url = Printf.sprintf "%s/game/sch/inc_list_1_new.asp?season_gu=%s&ym=%s&viewType=1&gun=1"
    base_url season_code ym in
  let net = Eio.Stdenv.net env in
  let headers = Cohttp.Header.of_list [
    ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) WKBL-Stats-Bot/1.0");
    ("Accept", "text/html,application/xhtml+xml");
    ("Accept-Language", "ko-KR,ko;q=0.9");
    ("Referer", "https://www.wkbl.or.kr/game/sch/schedule1.asp");
    ("X-Requested-With", "XMLHttpRequest");
  ] in
  let uri = Uri.of_string url in
  let client = Cohttp_eio.Client.make ~https:(Some https_wrapper) net in
  match Cohttp_eio.Client.get ~sw client ~headers uri with
  | resp, body ->
      let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
      if code >= 200 && code < 300 then
        Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
      else begin
        Printf.eprintf "HTTP %d for schedule API (season=%s, ym=%s)\n" code season_code ym;
        ""
      end
  | exception exn ->
      Printf.eprintf "HTTP error for schedule API: %s\n" (Printexc.to_string exn);
      ""

(** Parse schedule HTML from new API
    Extracts: date, teams, scores, venue from team_versus divs *)
let parse_schedule_api_html ~season_code ~season_name:_ html =
  let open Soup in
  let soup = parse html in
  let rows = soup $$ "tr[id]" |> to_list in
  rows |> List.filter_map (fun row ->
    try
      (* Prefer row id (YYYYMMDD) for a stable ISO date. *)
      let date_id = attribute "id" row |> Option.value ~default:"" in
      let date_iso =
        if String.length date_id >= 8 then
          Printf.sprintf "%s-%s-%s"
            (String.sub date_id 0 4)
            (String.sub date_id 4 2)
            (String.sub date_id 6 2)
        else
          ""
      in

      (* Extract day of week from first td (e.g., "토") *)
      let date_td = row $ "td" in
      let day =
        match date_td $? "span.language" with
        | None -> ""
        | Some sp ->
            leaf_text sp |> Option.value ~default:"" |> String.trim
      in

      (* Extract teams and scores from team_versus div *)
      let team_div = row $ ".team_versus" in
      let away_block = team_div $ ".away" in
      let home_block = team_div $ ".home" in
      let away_team = away_block $ ".team_name" |> R.leaf_text in
      let home_team = home_block $ ".team_name" |> R.leaf_text in
      let away_team_code = team_code_from_logo_node away_block in
      let home_team_code = team_code_from_logo_node home_block in

      (* Extract scores - may not exist for scheduled games *)
      let away_score = try
        let s = team_div $ ".away .txt_score" |> R.leaf_text in
        if s = "" then None else Some (int_of_string s)
      with _ -> None in
      let home_score = try
        let s = team_div $ ".home .txt_score" |> R.leaf_text in
        if s = "" then None else Some (int_of_string s)
      with _ -> None in

      (* Extract venue - third td *)
      let tds = row $$ "td" |> to_list in
      let venue =
        match List.nth_opt tds 2 with
        | Some td -> td |> texts |> String.concat "" |> String.trim
        | None -> ""
      in

      (* Extract time - fourth td *)
      let time =
        match List.nth_opt tds 3 with
        | Some td -> td |> texts |> String.concat "" |> String.trim
        | None -> ""
      in

      let game_type_opt, game_no_opt = extract_game_meta_from_schedule_row row in
      let game_id_opt = game_id_of_params ~season_code:season_code ~game_type_opt ~game_no_opt in
      Some {
        sch_game_id = game_id_opt;
        sch_game_type = game_type_opt;
        sch_game_no = game_no_opt;
        sch_season = season_code;
        sch_date = if date_iso <> "" then date_iso else (texts date_td |> String.concat "" |> String.trim);
        sch_day = day;
        sch_time = time;
        sch_home_team = home_team;
        sch_away_team = away_team;
        sch_home_team_code = home_team_code;
        sch_away_team_code = away_team_code;
        sch_home_score = home_score;
        sch_away_score = away_score;
        sch_venue = venue;
      }
    with _ -> None
  )

(** Deduplicate schedule entries by game_id (preferred) or date+teams *)
let dedupe_schedule_entries entries =
  let seen = Hashtbl.create (List.length entries * 2 + 1) in
  let key_of (e: schedule_entry) =
    match e.sch_game_id with
    | Some gid -> "id:" ^ gid
    | None -> Printf.sprintf "date:%s|%s|%s" e.sch_date e.sch_home_team e.sch_away_team
  in
  entries |> List.filter (fun e ->
    let key = key_of e in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key true; true)
  )

(** Fetch full schedule for a season using new API *)
let fetch_full_season_schedule ~sw ~env ~season_code ~season_name =
  let clock = Eio.Stdenv.clock env in
  let date_range = season_date_range season_name in
  Printf.printf "Fetching %s (%s) - %d months...\n%!" season_name season_code (List.length date_range);

  date_range |> List.concat_map (fun (year, month) ->
    let ym = Printf.sprintf "%04d%02d" year month in
    let html = fetch_schedule_api ~sw ~env ~season_code ~ym in
    (* Rate limiting *)
    Eio.Time.sleep clock 0.3;
    if String.length html > 100 then
      parse_schedule_api_html ~season_code ~season_name html
    else
      []
  )
  |> dedupe_schedule_entries

(** Fetch all historical schedules (1998-2026)
    @param seasons Optional subset of seasons to fetch *)
let fetch_all_historical_schedules ~sw ~env ?(seasons=all_season_codes) () =
  Printf.printf "=== WKBL Full History Scraper ===\n";
  Printf.printf "Fetching %d seasons (1998-2026)...\n\n%!" (List.length seasons);

  let clock = Eio.Stdenv.clock env in
  let all_entries = ref [] in

  seasons |> List.iter (fun (code, name) ->
    let entries = fetch_full_season_schedule ~sw ~env ~season_code:code ~season_name:name in
    Printf.printf "  %s: %d games\n%!" name (List.length entries);
    all_entries := entries @ !all_entries;
    (* Rate limiting between seasons *)
    Eio.Time.sleep clock 0.5
  );

  Printf.printf "\n=== Total: %d games across %d seasons ===\n%!"
    (List.length !all_entries) (List.length seasons);
  !all_entries

(** Convert schedule_entry date format for database
    Input: "1/10(월)" or "12/25(일)" → Output: "2000-01-10" *)
let normalize_schedule_date ~season_code date_str =
  let clean = String.trim date_str in
  (* Fast path: already in YYYY-MM-DD or YYYY.MM.DD / YYYY/MM/DD *)
  let date_re =
    Str.regexp "\\([0-9][0-9][0-9][0-9]\\)[./-]\\([0-9][0-9]?\\)[./-]\\([0-9][0-9]?\\)"
  in
  if Str.string_match date_re clean 0 then
    let y = int_of_string (Str.matched_group 1 clean) in
    let m = int_of_string (Str.matched_group 2 clean) in
    let d = int_of_string (Str.matched_group 3 clean) in
    Printf.sprintf "%04d-%02d-%02d" y m d
  else
  (* Extract season year from season_code *)
  let season_name = Seasons_catalog.name_of_code season_code in
  let current_year =
    let tm = Unix.localtime (Unix.time ()) in
    tm.Unix.tm_year + 1900
  in
  let base_year =
    if String.length season_name >= 4 then
      try int_of_string (String.sub season_name 0 4) with _ -> current_year
    else current_year
  in
  (* Parse "M/D(day)" format *)
  try
    let paren_pos = String.index clean '(' in
    let date_part = String.sub clean 0 paren_pos in
    let slash_pos = String.index date_part '/' in
    let month = int_of_string (String.sub date_part 0 slash_pos) in
    let day = int_of_string (String.sub date_part (slash_pos + 1) (String.length date_part - slash_pos - 1)) in
    (* Determine actual year based on month and season type *)
    let year =
      if String.length season_name >= 9 && season_name.[4] = '-' then
        (* Regular season: Oct-Mar spans two years *)
        if month >= 10 then base_year else base_year + 1
      else if String.sub season_name 4 (String.length season_name - 4) = "겨울" then
        (* Winter: Dec spans into next year, Jan-Mar is next year *)
        if month = 12 then base_year - 1 else base_year
      else
        base_year
    in
    Printf.sprintf "%04d-%02d-%02d" year month day
  with _ -> Printf.sprintf "%04d-01-01" base_year  (* fallback *)

(** Normalize DataLab game_date into YYYY-MM-DD
    Accepts formats like "20260123", "2026.01.23", "2026-01-23", or strings with extra text.
    Returns [None] if no 8-digit date can be extracted. *)
let normalize_game_date date_str =
  let digits =
    date_str
    |> String.to_seq
    |> Seq.filter (fun c -> c >= '0' && c <= '9')
    |> String.of_seq
  in
  if String.length digits = 8 then
    let y = String.sub digits 0 4 in
    let m = String.sub digits 4 2 in
    let d = String.sub digits 6 2 in
    Some (Printf.sprintf "%s-%s-%s" y m d)
  else
    None

(** Get schedule status from scores *)
let schedule_status_from_scores home_score away_score =
  match home_score, away_score with
  | Some _, Some _ -> "completed"
  | None, None -> "scheduled"
  | _ -> "in_progress"

(** Last successful sync timestamp (Unix time) *)
let last_sync_time : float option ref = ref None

(** Get last sync time as formatted string *)
let get_last_sync_time_str () =
  match !last_sync_time with
  | None -> "동기화 기록 없음"
  | Some t ->
    let tm = Unix.localtime t in
    let now = Unix.time () in
    let diff_min = int_of_float ((now -. t) /. 60.0) in
    if diff_min < 1 then "방금 전"
    else if diff_min < 60 then Printf.sprintf "%d분 전" diff_min
    else if diff_min < 1440 then Printf.sprintf "%d시간 전" (diff_min / 60)
    else Printf.sprintf "%04d-%02d-%02d %02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min

(** Calculate current season code based on date
    WKBL season runs Oct-Mar, so:
    - Oct 2025 ~ Mar 2026 = "2025-2026" season = code "046"
    - Oct 2024 ~ Mar 2025 = "2024-2025" season = code "045"
    Base: 1980-1981 = "001" (season_start_year - 1979) *)
let current_season_code_auto () =
  let tm = Unix.localtime (Unix.time ()) in
  let year = tm.Unix.tm_year + 1900 in
  let month = tm.Unix.tm_mon + 1 in
  (* Oct-Dec: current year's season, Jan-Sep: previous year's season *)
  let season_start_year = if month >= 10 then year else year - 1 in
  let code = season_start_year - 1979 in  (* 1980 = 001 *)
  Printf.sprintf "%03d" code

let current_season_name_auto () =
  let tm = Unix.localtime (Unix.time ()) in
  let year = tm.Unix.tm_year + 1900 in
  let month = tm.Unix.tm_mon + 1 in
  let season_start_year = if month >= 10 then year else year - 1 in
  Printf.sprintf "%d-%d" season_start_year (season_start_year + 1)

let iso_date_of_time (t: float) =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

let year_month_prefix_of_time (t: float) =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)

(** Decide whether fetched schedule entries look suspiciously incomplete.

    Rationale: a non-empty scrape can still miss the current month and make the
    site appear "stuck" while logs say "synced".

    Policy:
    - Only enforce during core season months (Nov-Mar).
    - If the current month has no entries, flag as suspicious.
    - If the latest completed game is too old (30d) during the season, flag.

    Returns [Some reason] when suspicious, else [None]. *)
let schedule_sync_suspicion_reason_v
    ~enforce
    ~current_ym
    ~threshold_old
    ~(dates: string list)
    ~(completed_dates: string list)
  =
  if not enforce then
    None
  else
    let has_current_month =
      dates
      |> List.exists (fun d ->
          String.length d >= 7 && String.starts_with ~prefix:current_ym d)
    in
    if not has_current_month then
      Some (Printf.sprintf "no entries for current month (%s)" current_ym)
    else
      let max_completed =
        completed_dates
        |> List.fold_left
             (fun acc d -> if String.compare d acc > 0 then d else acc)
             "0000-00-00"
      in
      if max_completed = "0000-00-00" then
        None
      else if String.compare max_completed threshold_old < 0 then
        Some (Printf.sprintf "latest completed game is stale (%s < %s)" max_completed threshold_old)
      else
        None

let schedule_sync_suspicion_reason
    ~now
    ~(dates: string list)
    ~(completed_dates: string list)
  =
  let tm = Unix.localtime now in
  let month = tm.Unix.tm_mon + 1 in
  let enforce = (month >= 11 || month <= 3) in
  let current_ym = year_month_prefix_of_time now in
  let threshold_old = iso_date_of_time (now -. (30.0 *. 86400.0)) in
  schedule_sync_suspicion_reason_v
    ~enforce
    ~current_ym
    ~threshold_old
    ~dates
    ~completed_dates

(** Ensure the seasons catalog exists and is correct in DB.

    This fixes UI facts such as season dropdown labels when the DB was seeded
    with an incorrect season_code -> season_name mapping. *)
let ensure_seasons_catalog_in_db () =
  let current_code = current_season_code_auto () |> main_to_datalab in
  let current_name = current_season_name_auto () in
  Db.with_db (fun db ->
    let seeded =
      Seasons_catalog.all
      |> List.fold_left (fun acc (code, name) ->
          match acc with
          | Error _ as e -> e
          | Ok () ->
              Db.Repo.upsert_season ~season_code:code ~season_name:name db)
          (Ok ())
    in
    match seeded with
    | Error _ as e -> e
    | Ok () -> Db.Repo.upsert_season ~season_code:current_code ~season_name:current_name db)

(** Decide whether a schedule sync attempt should be treated as "successful".

    Important: (0 synced, 0 errors) is treated as failure, because it almost
    always means the upstream HTML was blocked/changed and nothing was ingested. *)
let schedule_sync_success ~schedule_synced ~games_upserted ~errors =
  errors = 0 && (schedule_synced > 0 || games_upserted > 0)

(** Sync current season schedule to database
    Returns (schedule_synced, games_upserted, error_count) *)
let sync_current_season_schedule ~sw ~env () =
  let current_season_code = current_season_code_auto () |> main_to_datalab in
  let current_season_name = current_season_name_auto () in
  Printf.printf "[Sync] Starting schedule sync for season %s (%s)...\n%!" current_season_code current_season_name;
  try
    let entries = fetch_full_season_schedule ~sw ~env ~season_code:current_season_code ~season_name:current_season_name in
    Printf.printf "[Sync] Fetched %d schedule entries\n%!" (List.length entries);
    if entries = [] then begin
      Printf.eprintf
        "[Sync] No schedule entries fetched (season=%s, name=%s). Treating as failure.\n%!"
        current_season_code
        current_season_name;
      (0, 0, 1)
    end else
    let synced = ref 0 in
    let synced_games = ref 0 in
    let errors = ref 0 in
    let normalized_dates =
      entries
      |> List.map (fun (e: schedule_entry) ->
          if String.contains e.sch_date '-' then e.sch_date
          else normalize_schedule_date ~season_code:current_season_code e.sch_date)
    in
    let normalized_completed_dates =
      entries
      |> List.filter (fun (e: schedule_entry) ->
          match e.sch_home_score, e.sch_away_score with
          | Some _, Some _ -> true
          | _ -> false)
      |> List.map (fun (e: schedule_entry) ->
          if String.contains e.sch_date '-' then e.sch_date
          else normalize_schedule_date ~season_code:current_season_code e.sch_date)
    in
    (match schedule_sync_suspicion_reason
             ~now:(Unix.time ())
             ~dates:normalized_dates
             ~completed_dates:normalized_completed_dates
     with
    | None -> ()
    | Some reason ->
        Printf.eprintf "[Sync] Suspicious schedule data: %s\n%!" reason;
        incr errors);
    entries |> List.iter (fun (entry : schedule_entry) ->
      let status = schedule_status_from_scores entry.sch_home_score entry.sch_away_score in
      let game_date =
        (* Prefer the ISO date from row id; fallback to normalizer for legacy formats. *)
        if String.contains entry.sch_date '-' then entry.sch_date
        else normalize_schedule_date ~season_code:current_season_code entry.sch_date
      in
      let game_time = if entry.sch_time = "" then None else Some entry.sch_time in
      let venue = if entry.sch_venue = "" then None else Some entry.sch_venue in

      let home_team_code =
        match entry.sch_home_team_code with
        | Some c -> c
        | None -> code_from_team_name entry.sch_home_team
      in
      let away_team_code =
        match entry.sch_away_team_code with
        | Some c -> c
        | None -> code_from_team_name entry.sch_away_team
      in

      let upsert_res =
        Db.with_db (fun db ->
          Db.Repo.upsert_schedule_entry
            ~game_date
            ~game_time
            ~season_code:current_season_code
            ~home_team_code
            ~away_team_code
            ~venue
            ~status
            db)
      in
      (match upsert_res with
      | Ok () -> incr synced
      | Error e ->
          Printf.eprintf "[Sync] Error upserting schedule %s: %s\n%!" game_date (Db.show_db_error e);
          incr errors);

      (* Also upsert into games table when we can derive stable identifiers. *)
      (match (entry.sch_game_type, entry.sch_game_no) with
      | Some game_type, Some game_no
        when not (String.starts_with ~prefix:"XX_" home_team_code)
             && not (String.starts_with ~prefix:"XX_" away_team_code) ->
          let game_id = Printf.sprintf "%s-%s-%d" current_season_code game_type game_no in
          let game_res =
            Db.with_db (fun db ->
	              Db.Repo.upsert_game_entry
	                ~game_id
	                ~season_code:current_season_code
	                ~game_type
	                ~game_no
	                ~game_date:(Some game_date)
	                ~home_team_code
	                ~away_team_code
	                ~home_score:entry.sch_home_score
	                ~away_score:entry.sch_away_score
                ~stadium:venue
                ~attendance:None
                db)
          in
          (match game_res with
          | Ok () -> incr synced_games
          | Error e ->
              Printf.eprintf "[Sync] Error upserting game %s: %s\n%!" game_id (Db.show_db_error e);
              incr errors)
      | _ -> ())
    );
    (* Refresh materialized views so /games reflects new rows quickly. *)
    (match Db.refresh_matviews () with
    | Ok () -> ()
    | Error e -> Printf.eprintf "[Sync] Failed to refresh materialized views: %s\n%!" (Db.show_db_error e));

    Printf.printf "[Sync] Complete: %d schedule synced, %d games upserted, %d errors\n%!"
      !synced !synced_games !errors;
    (* Update last sync time on success *)
    if schedule_sync_success ~schedule_synced:!synced ~games_upserted:!synced_games ~errors:!errors then
      last_sync_time := Some (Unix.time ());
    (!synced, !synced_games, !errors)
  with exn ->
    Printf.eprintf "[Sync] Fatal error: %s\n%!" (Printexc.to_string exn);
    (0, 0, 1)

(** Fetch live games from WKBL main page *)
let fetch_live_games ~sw ~env () =
  let url = "https://www.wkbl.or.kr/main/main.asp" in
  try
    let html = fetch_url ~sw ~env url in
    let soup = Soup.parse html in
    (* WKBL Main Page Structure (Heuristic) *)
    let games =
      soup
      |> Soup.select ".main_game_list li"
      |> Soup.to_list
      |> List.filter_map (fun node ->
          let open Soup in
          let home_node = node $? ".home" in
          let away_node = node $? ".away" in
          let score_node = node $? ".score" in
          let status_node = node $? ".state" in
          
          match home_node, away_node, score_node with
          | Some h, Some a, Some s ->
              let home_team = leaf_text h |> Option.value ~default:"" |> String.trim in
              let away_team = leaf_text a |> Option.value ~default:"" |> String.trim in
              let score_text = leaf_text s |> Option.value ~default:"0:0" in
              let status = 
                (match status_node with Some n -> leaf_text n | None -> None) 
                |> Option.value ~default:"" |> String.trim 
              in
              
              let home_score, away_score =
                match String.split_on_char ':' score_text with
                | [h; a] -> 
                    (try (int_of_string (String.trim h), int_of_string (String.trim a)) 
                     with _ -> (0, 0))
                | _ -> (0, 0)
              in
              
              if home_team <> "" && away_team <> "" then
                Some {
                  Domain.lg_game_id = "live_" ^ home_team;
                  lg_home_team = home_team;
                  lg_away_team = away_team;
                  lg_home_score = home_score;
                  lg_away_score = away_score;
                  lg_quarter = status;
                  lg_time_remaining = "";
                  lg_is_live = (status <> "경기전" && status <> "경기종료" && status <> "");
                }
              else None
          | _ -> None
      )
    in
    if games = [] then
      (* Fallback: Use DB scheduled games *)
      let today = 
        let tm = Unix.localtime (Unix.time ()) in
        Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      in
      match Db.get_games ~season:"ALL" () with
      | Ok all_games ->
          all_games 
          |> List.filter (fun (g: Domain.game_summary) -> 
              String.length g.game_date >= 10 && String.sub g.game_date 0 10 = today)
          |> List.map (fun (g: Domain.game_summary) -> 
              {
                Domain.lg_game_id = g.game_id;
                lg_home_team = g.home_team;
                lg_away_team = g.away_team;
                lg_home_score = Option.value ~default:0 g.home_score;
                lg_away_score = Option.value ~default:0 g.away_score;
                lg_quarter = if g.home_score = None then "경기전" else "경기종료";
                lg_time_remaining = "";
                lg_is_live = false; 
              }
          )
      | Error _ -> []
    else games
  with e ->
    Printf.eprintf "[Scraper] Live fetch failed: %s\n%!" (Printexc.to_string e);
    []
