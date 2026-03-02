(** WKBL Scraper CLI Tool

    Usage:
      scraper_tool draft [--csv]           Fetch all draft history
      scraper_tool draft --season=044      Fetch specific season
      scraper_tool awards stats [--csv]    Fetch statistical awards
      scraper_tool awards best5 [--csv]    Fetch BEST5 awards
      scraper_tool sync schedule           Sync schedule to database
      scraper_tool sync draft [--season=X] Sync draft history to database
      scraper_tool sync trade              Sync official trade events to database
      scraper_tool sync coaches            Sync team coaches to database
*)

open Wkbl

(** Get database URL from environment *)
let get_db_url () =
  match Sys.getenv_opt "WKBL_DATABASE_URL" with
  | Some url -> Some url
  | None -> Sys.getenv_opt "DATABASE_URL"

let usage = {|
WKBL Scraper Tool

Usage:
  scraper_tool draft [--csv]           Fetch all draft history (15 seasons)
  scraper_tool draft --season=CODE     Fetch specific season (e.g., 044)
  scraper_tool awards stats [--csv]    Fetch statistical awards (득점상, 리바운드상, etc.)
  scraper_tool awards best5 [--csv]    Fetch BEST5 awards
  scraper_tool fa [--csv]              Fetch FA (Free Agent) results
  scraper_tool salary [--csv]          Fetch player salary data (13 seasons)
  scraper_tool crowd [--csv]           Fetch team attendance by season
  scraper_tool records [--csv]         Fetch major team records (역대주요기록)
  scraper_tool games [--csv]           Fetch game results from DataLab (15 seasons)
  scraper_tool games --season=CODE     Fetch games for specific season
  scraper_tool teamstats [--csv]       Fetch team vs team statistics from DataLab
  scraper_tool versus [--csv]          Fetch head-to-head records from DataLab
  scraper_tool championship [--csv]    Fetch championship history (챔피언 역사)
  scraper_tool championship --stats    Show championship statistics (통계 분석)
  scraper_tool allstar [--csv]         Fetch all-star game history (올스타 역사)
  scraper_tool allstar --stats         Show all-star MVP statistics (통계 분석)
  scraper_tool schedule [--csv]        Fetch season schedule (경기 일정)
  scraper_tool schedule --month=YYYYMM Fetch specific month schedule
  scraper_tool sync schedule [--purge] Sync current season schedule to database
  scraper_tool sync draft [--season=X] Sync draft history to database
  scraper_tool sync trade              Sync official trade events to database
  scraper_tool sync coaches            Sync team coaches to database
  scraper_tool sync games              Sync DataLab game results to database
  scraper_tool sync games --season=X   Sync specific season game results
  scraper_tool sync boxscore           Sync missing boxscores to database
  scraper_tool sync pbp [--season=X]   Sync play-by-play (live text) to database
  scraper_tool sync history [--purge]            Sync ALL historical games to database (1998-2026)
  scraper_tool sync history --season=X [--purge] Sync specific season to database
  scraper_tool history [--csv]         Fetch ALL historical games (1998-2026, 43 seasons!)
  scraper_tool history --season=CODE   Fetch specific season from full history
  scraper_tool --help                  Show this help

Examples:
  scraper_tool draft --csv > drafts.csv
  scraper_tool draft --season=044
  scraper_tool awards stats --csv > stat_awards.csv
  scraper_tool awards best5
  scraper_tool fa --csv > fa_results.csv
  scraper_tool salary --csv > salaries.csv
  scraper_tool crowd --csv > crowd.csv
  scraper_tool records --csv > records.2csv
  scraper_tool games --csv > games.csv
  scraper_tool games --season=044 --csv
  scraper_tool teamstats --csv > team_stats.csv
  scraper_tool versus --csv > versus_records.csv
  scraper_tool championship --csv > championships.csv
  scraper_tool allstar --csv > allstars.csv
  scraper_tool schedule --csv > schedule.csv
  scraper_tool schedule --month=202601 --csv
|}

(** Run with Eio + RNG initialized for TLS/HTTPS *)
let run_with_rng f =
  (* Initialize RNG for TLS - must be done before any TLS operations *)
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  f ~sw ~env

let init_db_or_exit ~sw ~env =
  let db_url =
    match get_db_url () with
    | Some url -> url
    | None ->
        Printf.eprintf "Error: DATABASE_URL or WKBL_DATABASE_URL not set\n";
        exit 1
  in
  match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "Database connection failed: %s\n" (Db.show_db_error e);
      exit 1

let run_sync_draft ~sw ~env ~season_filter =
  init_db_or_exit ~sw ~env;
  let total, inserted, unmatched =
    Scraper.sync_drafts_to_db ~sw ~env ?season_filter ()
  in
  Db.clear_all_caches ();
  Printf.printf "\n=== Draft Sync Complete ===\n";
  Printf.printf "Fetched: %d, Inserted/Updated: %d, Unmatched players: %d\n%!"
    total inserted unmatched

let run_sync_trade ~sw ~env =
  init_db_or_exit ~sw ~env;
  let total, inserted = Scraper.sync_trade_events_to_db ~sw ~env () in
  Db.clear_all_caches ();
  Printf.printf "\n=== Trade Sync Complete ===\n";
  Printf.printf "Fetched(unique): %d, Inserted/Updated: %d\n%!"
    total inserted

let run_sync_coaches ~sw ~env =
  init_db_or_exit ~sw ~env;
  let teams, teams_with_data, inserted = Scraper.sync_coaches_to_db ~sw ~env () in
  Db.clear_all_caches ();
  Printf.printf "\n=== Coaches Sync Complete ===\n";
  Printf.printf "Teams checked: %d, Teams with coach rows: %d, Inserted: %d\n%!"
    teams teams_with_data inserted

(** Sync boxscore data for games that don't have stats yet *)
let run_sync_boxscore ~sw ~env ~season_filter =
  let db_url = match get_db_url () with
    | Some url -> url
    | None -> Printf.eprintf "Error: DATABASE_URL not set\n"; exit 1
  in

  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> ()
  | Error e -> Printf.eprintf "DB Error: %s\n" (Db.show_db_error e); exit 1);

  let season_missing =
    Option.value ~default:(Scraper.current_season_code_auto ()) season_filter
  in
  let season_mismatch = Option.value ~default:"ALL" season_filter in

  let missing_games =
    match Db_sync.get_games_missing_boxscore ~season:season_missing () with
    | Ok xs -> xs
    | Error e ->
        Printf.eprintf "Failed to fetch missing games: %s\n" (Db.show_db_error e);
        []
  in

  let mismatch_games =
    let take n xs =
      let rec loop acc n = function
        | [] -> List.rev acc
        | _ when n <= 0 -> List.rev acc
        | x :: rest -> loop (x :: acc) (n - 1) rest
      in
      loop [] n xs
    in
    match Db_sync.get_games_score_mismatch ~season:season_mismatch () with
    | Ok xs ->
        (* When running daily for current season, repairing ALL historical mismatches can
           be unexpectedly heavy. Keep it bounded by default and let manual runs go deeper. *)
        (match season_filter with
        | None -> take 200 xs
        | Some _ -> xs)
    | Error e ->
        Printf.eprintf "Failed to fetch mismatch games: %s\n" (Db.show_db_error e);
        []
  in

  let uniq_by_game_id (games : Db_sync.missing_boxscore_game list) =
    let seen = Hashtbl.create 256 in
    games
    |> List.filter (fun g ->
      if Hashtbl.mem seen g.Db_sync.game_id then false
      else (Hashtbl.add seen g.Db_sync.game_id (); true)
    )
  in

  let games = uniq_by_game_id (missing_games @ mismatch_games) in
  let total = List.length games in

  Printf.printf
    "Found %d games missing boxscores + %d score mismatches. Starting sync (%d total)...\n%!"
    (List.length missing_games)
    (List.length mismatch_games)
    total;

  if total = 0 then (
    Printf.printf "Nothing to sync.\n%!";
  ) else (
      
      let clock = Eio.Stdenv.clock env in
      games |> List.iteri (fun i (g : Db_sync.missing_boxscore_game) ->
        Printf.printf "[%d/%d] Syncing boxscore for game: %s\n%!" (i + 1) total g.Db_sync.game_id;

        let tables =
          Scraper.fetch_game_boxscore
            ~sw ~env
            ~season_gu:g.Db_sync.season_code
            ~game_type:g.Db_sync.game_type
            ~game_no:g.Db_sync.game_no
            ~ym:g.Db_sync.ym
        in
        match tables with
        | [away_stats; home_stats] ->
            let upsert_team team_code stats =
              stats |> List.iter (fun s ->
                match Db_sync.upsert_game_stat s ~game_id:g.Db_sync.game_id ~team_code with
                | Ok () -> ()
                | Error e ->
                    Printf.eprintf "  ✗ Failed to upsert stat for %s: %s\n"
                      s.bs_player_name (Db.show_db_error e)
              )
            in
            upsert_team g.Db_sync.away_team_code away_stats;
            upsert_team g.Db_sync.home_team_code home_stats;
            let sum_pts stats =
              stats |> List.fold_left (fun acc s -> acc + s.Scraper.bs_pts) 0
            in
            let away_pts = sum_pts away_stats in
            let home_pts = sum_pts home_stats in
            (match Db_sync.update_game_scores_if_missing ~game_id:g.Db_sync.game_id ~away_score:away_pts ~home_score:home_pts with
            | Ok () -> ()
            | Error e ->
                Printf.eprintf "  ✗ Failed to backfill scores for %s: %s\n"
                  g.Db_sync.game_id (Db.show_db_error e));
            Eio.Time.sleep clock 0.2 (* Rate limit between games *)
        | _ ->
            Printf.eprintf "  ✗ Unexpected boxscore HTML for game %s (tables=%d)\n"
              g.Db_sync.game_id (List.length tables)
      );
      (match Db.refresh_matviews () with
      | Ok () -> ()
      | Error e ->
          Printf.eprintf "  ✗ Failed to refresh materialized views: %s\n%!"
            (Db.show_db_error e));

      Printf.printf "\n=== Boxscore Sync Complete ===\n"
  )

let run_sync_pbp ~sw ~env ~season_filter =
  let db_url =
    match get_db_url () with
    | Some url -> url
    | None ->
        Printf.eprintf "Error: DATABASE_URL not set\n";
        exit 1
  in

  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "DB Error: %s\n" (Db.show_db_error e);
      exit 1);

  let season =
    Option.value ~default:(Scraper.current_season_code_auto ()) season_filter
  in

  let take n xs =
    let rec loop acc n = function
      | [] -> List.rev acc
      | _ when n <= 0 -> List.rev acc
      | x :: rest -> loop (x :: acc) (n - 1) rest
    in
    loop [] n xs
  in

  let missing_games =
    match Db_sync.get_games_missing_pbp ~season () with
    | Ok xs -> xs
    | Error e ->
        Printf.eprintf "Failed to fetch missing PBP games: %s\n" (Db.show_db_error e);
        []
  in

  let incomplete_game_ids =
    match Db.get_incomplete_pbp_games () with
    | Ok xs ->
        xs
        |> List.filter (fun g -> season = "ALL" || g.Db.ig_season = season)
        |> List.map (fun g -> g.Db.ig_game_id)
    | Error _ -> []
  in

  let missing_game_ids = missing_games |> List.map (fun g -> g.Db_sync.PbpSync.game_id) in

  let uniq_game_ids (ids : string list) =
    let seen = Hashtbl.create 512 in
    ids
    |> List.filter (fun id ->
        if Hashtbl.mem seen id then false
        else (
          Hashtbl.add seen id ();
          true))
  in

  let max_games =
    match season_filter with
    | None -> 80
    | Some _ -> 200
  in

  let game_ids =
    uniq_game_ids (missing_game_ids @ incomplete_game_ids) |> take max_games
  in

  let total = List.length game_ids in
  Printf.printf
    "Found %d games missing PBP + %d incomplete PBP. Starting sync (%d total, season=%s)...\n%!"
    (List.length missing_game_ids)
    (List.length incomplete_game_ids)
    total
    season;

  let parse_game_id game_id =
    match String.split_on_char '-' (String.trim game_id) with
    | [season_gu; game_type; game_no_s] -> (
        match int_of_string_opt (String.trim game_no_s) with
        | Some game_no -> Some (season_gu, game_type, game_no)
        | None -> None)
    | _ -> None
  in

  let pick_player_id ~(roster : (string * string) list) (desc : string) =
    let desc = String.trim desc in
    if desc = "" then None
    else
      let starts_with_name name =
        let name = String.trim name in
        if name = "" then false
        else
          let ln = String.length name in
          let ld = String.length desc in
          if ld < ln then false
          else
            String.sub desc 0 ln = name
            && (ld = ln || desc.[ln] = ' ')
      in
      let candidates =
        roster
        |> List.filter (fun (name, _id) -> starts_with_name name)
        |> List.sort (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
      in
      match candidates with
      | [] -> None
      | (name1, id1) :: rest ->
          let len1 = String.length (String.trim name1) in
          let same_len =
            rest
            |> List.filter (fun (n, _) -> String.length (String.trim n) = len1)
          in
          (* If two different IDs match with the same longest name length, do not guess. *)
          if List.exists (fun (_n, id) -> id <> id1) same_len then None else Some id1
  in

  let get_rosters ~game_id =
    match Db.get_boxscore ~game_id () with
    | Ok bs ->
        let away =
          bs.boxscore_away_players
          |> List.map (fun (p : Domain.boxscore_player_stat) ->
              (String.trim p.bs_player_name, p.bs_player_id))
        in
        let home =
          bs.boxscore_home_players
          |> List.map (fun (p : Domain.boxscore_player_stat) ->
              (String.trim p.bs_player_name, p.bs_player_id))
        in
        Some (away, home)
    | Error _ -> None
  in

  let clock = Eio.Stdenv.clock env in
  let ok_count = ref 0 in
  let skip_count = ref 0 in
  let err_count = ref 0 in
  game_ids
  |> List.iteri (fun i game_id ->
      Printf.printf "[%d/%d] Syncing PBP for game: %s\n%!" (i + 1) total game_id;
      match parse_game_id game_id with
      | None ->
          incr err_count;
          Printf.eprintf "  ✗ Cannot parse game_id: %s\n%!" game_id
      | Some (season_gu, game_type, game_no) ->
          let events =
            Scraper.fetch_game_pbp_events ~sw ~env ~season_gu ~game_type ~game_no ()
          in
          if events = [] then (
            incr skip_count;
            Printf.printf "  - No PBP available, skipping.\n%!"
          ) else (
            let rosters_opt = get_rosters ~game_id in
            let rows =
              events
              |> List.map (fun (e : Domain.pbp_event) ->
                  let player_id =
                    match (e.pe_team_side, rosters_opt) with
                    | 1, Some (away, _home) ->
                        pick_player_id ~roster:away e.pe_description
                    | 2, Some (_away, home) ->
                        pick_player_id ~roster:home e.pe_description
                    | _ -> None
                  in
                  (e, player_id))
            in
            match Db_sync.replace_pbp_events ~game_id rows with
            | Ok n ->
                incr ok_count;
                Printf.printf "  ✓ Upserted %d events\n%!" n
            | Error e ->
                incr err_count;
                Printf.eprintf "  ✗ DB error: %s\n%!" (Db.show_db_error e)
          );
          Eio.Time.sleep clock 0.2);
  Printf.printf
    "\n=== PBP Sync Complete ===\nOK: %d, Skipped(no PBP): %d, Errors: %d\n%!"
    !ok_count !skip_count !err_count

let run_draft ~sw ~env ~season_filter ~csv_output =
  let entries = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.season_codes
          |> Option.value ~default:"Unknown" in
        Scraper.fetch_draft_season ~sw ~env ~season_code:code ~season_name:name
    | None ->
        Scraper.fetch_all_drafts ~sw ~env
  in
  Printf.printf "\n=== Fetched %d draft entries ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_draft_csv entries
  else
    entries |> List.iter (fun (e : Scraper.draft_entry) ->
      Printf.printf "[%s] #%d %s - %s (%s)\n"
        e.season_name
        e.pick_order
        e.player_name
        e.team_name
        (Option.value ~default:"?" e.school)
    )

let run_stat_awards ~sw ~env ~csv_output =
  let entries = Scraper.fetch_stat_awards ~sw ~env in
  Printf.printf "\n=== Fetched %d statistical award entries ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_stat_awards_csv entries
  else
    entries |> List.iter (fun e ->
      Printf.printf "[%s] %s: %s (%s)\n"
        e.Scraper.season_name
        (Scraper.award_category_to_string e.category)
        e.player_name
        (Option.value ~default:"-" e.stat_value)
    )

let run_best5_awards ~sw ~env ~csv_output =
  let entries = Scraper.fetch_best5_awards ~sw ~env in
  Printf.printf "\n=== Fetched %d BEST5 seasons ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_best5_csv entries
  else
    entries |> List.iter (fun e ->
      Printf.printf "[%s] BEST5:\n" e.Scraper.b5_season_name;
      e.players |> List.iteri (fun i player ->
        Printf.printf "  %d. %s\n" (i + 1) player
      )
    )

let run_fa ~sw ~env ~csv_output =
  let entries = Scraper.fetch_fa_results ~sw ~env in
  Printf.printf "\n=== Fetched %d FA entries ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_fa_csv entries
  else
    entries |> List.iter (fun (e : Scraper.fa_entry) ->
      Printf.printf "[%s %s] %s: %s -> %s (%s, %s)\n"
        e.year
        e.round
        e.fa_player_name
        e.original_team
        e.acquiring_team
        e.contract_period
        e.total_salary
    )

let run_salary ~sw ~env ~csv_output =
  let entries = Scraper.fetch_salary ~sw ~env in
  Printf.printf "\n=== Fetched %d salary entries ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_salary_csv entries
  else
    entries |> List.iter (fun (e : Scraper.salary_entry) ->
      Printf.printf "[%s] #%d %s (%s): %s\n"
        e.season
        e.rank
        e.sal_player_name
        e.sal_team_name
        e.total
    )

let run_crowd ~sw ~env ~csv_output =
  let entries = Scraper.fetch_crowd ~sw ~env in
  Printf.printf "\n=== Fetched %d crowd (attendance) seasons ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_crowd_csv entries
  else
    entries |> List.iter (fun (e : Scraper.crowd_entry) ->
      Printf.printf "[%s] Total: %s (KB: %s, BNK: %s, Woori: %s)\n"
        e.crowd_season
        e.total
        e.kb_stars
        e.bnk_sum
        e.woori_bank
    )

let run_records ~sw ~env ~csv_output =
  let entries = Scraper.fetch_major_records ~sw ~env in
  Printf.printf "\n=== Fetched %d major records ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_major_records_csv entries
  else
    entries |> List.iter (fun (e : Scraper.major_record) ->
      Printf.printf "[%s] %s - %s vs %s (%s)\n"
        e.category
        e.record_value
        e.team1
        e.team2
        e.date
    )

let run_games ~sw ~env ~season_filter ~csv_output =
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.datalab_season_codes
  in
  let games = Scraper.fetch_all_games ~sw ~env ~seasons () in
  Printf.printf "\n=== Fetched %d game records ===\n\n" (List.length games);
  if csv_output then
    Scraper.print_games_csv games
  else
    games |> List.iter (fun (g : Scraper.game_record) ->
      Printf.printf "[%s] %s: %s %d - %d %s @ %s (W: %s)\n"
        g.game_season
        g.game_date
        g.home_team_name
        g.home_team_score
        g.away_team_score
        g.away_team_name
        g.court_name
        g.winner_team_name
    )

let run_teamstats ~sw ~env ~season_filter ~csv_output =
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.datalab_season_codes
  in
  let stats = Scraper.fetch_all_team_stats ~sw ~env ~seasons () in
  Printf.printf "\n=== Fetched %d team stat entries ===\n\n" (List.length stats);
  if csv_output then
    Scraper.print_team_stats_csv stats
  else
    stats |> List.iter (fun (s : Scraper.team_stat) ->
      Printf.printf "[%s] %s vs %s: %.1f PPG, %.1f RPG, %.1f APG\n"
        s.ts_season
        s.ts_team_name
        s.opponent_team_name
        s.score_avg
        s.reb_avg
        s.ast_avg
    )

let run_versus ~sw ~env ~season_filter ~csv_output =
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.datalab_season_codes
  in
  (* Fetch from each season by iterating *)
  let records =
    let rec fetch_all acc = function
      | [] -> List.rev acc
      | (code, _) :: rest ->
          let season_records = Scraper.fetch_season_versus_records ~sw ~env code in
          fetch_all (season_records @ acc) rest
    in
    fetch_all [] seasons
  in
  Printf.printf "\n=== Fetched %d versus records ===\n\n" (List.length records);
  if csv_output then
    Scraper.print_versus_csv records
  else
    records |> List.iter (fun (v : Scraper.versus_record) ->
      Printf.printf "[%s] %s vs %s: %d-%d\n"
        v.vs_season_name
        v.vs_home_team
        v.vs_away_team
        v.home_win
        v.home_lose
    )

let run_championship ~sw ~env ~csv_output ~stats_output =
  let records = Scraper.fetch_championship_history ~sw ~env in
  Printf.printf "\n=== Fetched %d championship records ===\n\n" (List.length records);
  if stats_output then
    Scraper.print_championship_analysis records
  else if csv_output then
    Scraper.print_championship_csv records
  else
    records |> List.iter (fun (c : Scraper.championship_record) ->
      Printf.printf "[%s] 제%d대회: %s vs %s (%s) | 정규시즌: %s\n"
        c.champ_season
        c.champ_edition
        c.champion_team
        c.runner_up_team
        c.finals_result
        c.regular_champion
    )

let run_allstar ~sw ~env ~csv_output ~stats_output =
  let records = Scraper.fetch_allstar_history ~sw ~env in
  Printf.printf "\n=== Fetched %d all-star records ===\n\n" (List.length records);
  if stats_output then
    Scraper.print_allstar_analysis records
  else if csv_output then
    Scraper.print_allstar_csv records
  else
    records |> List.iter (fun (a : Scraper.allstar_record) ->
      Printf.printf "[%s] 제%d회 @ %s (%s) | MVP: %s\n"
        a.as_season
        a.as_edition
        a.as_venue
        a.as_date
        a.as_mvp
    )

let run_schedule ~sw ~env ~month_filter ~csv_output =
  let season_code = Scraper.current_season_code_auto () in
  let entries = match month_filter with
    | Some ym ->
        Printf.printf "Fetching schedule for %s...\n\n" ym;
        Scraper.fetch_schedule_month ~sw ~env ~ym ~season:season_code
    | None ->
        Scraper.fetch_season_schedule ~sw ~env ~season_code
  in
  Printf.printf "\n=== Fetched %d schedule entries ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_schedule_csv entries
  else
    entries |> List.iter (fun (e : Scraper.schedule_entry) ->
      let score_str = match (e.sch_away_score, e.sch_home_score) with
        | Some a, Some h -> Printf.sprintf "[%d - %d]" a h
        | _ -> "[예정]"
      in
      Printf.printf "%s(%s) %s %s vs %s %s @ %s\n"
        e.sch_date
        e.sch_day
        e.sch_time
        e.sch_away_team
        e.sch_home_team
        score_str
        e.sch_venue
    )

(** Sync schedule data to Supabase database *)
let run_sync_schedule ~sw ~env ~purge =
  let db_url =
    match get_db_url () with
    | Some url -> url
    | None ->
        Printf.eprintf "Error: DATABASE_URL or WKBL_DATABASE_URL not set\n";
        exit 1
  in

  Printf.printf "Connecting to database...\n";
  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> Printf.printf "Database connected.\n"
  | Error e ->
      Printf.eprintf "Database connection failed: %s\n" (Db.show_db_error e);
      exit 1);

  let season_code = Scraper.current_season_code_auto () in
  let season_name =
    let from_catalog = Scraper.season_name_of_code season_code in
    if String.starts_with ~prefix:"Unknown-" from_catalog then
      Scraper.current_season_name_auto ()
    else
      from_catalog
  in
  let _ = Db.with_db (fun db -> Db.Repo.upsert_season ~season_code ~season_name db) in

  Printf.printf "Fetching schedule from WKBL...\n";
  let entries = Scraper.fetch_season_schedule ~sw ~env ~season_code in
  Printf.printf "Fetched %d schedule entries.\n\n" (List.length entries);

  if purge && entries = [] then (
    Printf.eprintf "Error: fetched 0 schedule entries; refusing to purge season %s.\n%!" season_code;
    exit 1
  );

  if purge then (
    Printf.printf "Purging existing schedule rows for season %s...\n%!" season_code;
    match Db.with_db (fun db -> Db.Repo.delete_schedule_by_season ~season_code db) with
    | Ok () -> Printf.printf "  ✓ Purged.\n%!"
    | Error e ->
        Printf.eprintf "  ✗ Purge failed: %s\n%!" (Db.show_db_error e);
        exit 1
  );

  let success_count = ref 0 in
  let error_count = ref 0 in
  let game_success = ref 0 in
  let seen_games = Hashtbl.create (List.length entries * 2 + 1) in

  entries
  |> List.iter (fun (e : Scraper.schedule_entry) ->
    let hc =
      match e.sch_home_team_code with
      | Some c -> c
      | None -> Scraper.code_from_team_name e.sch_home_team
    in
    let ac =
      match e.sch_away_team_code with
      | Some c -> c
      | None -> Scraper.code_from_team_name e.sch_away_team
    in
    if String.starts_with ~prefix:"XX_" hc || String.starts_with ~prefix:"XX_" ac then (
      incr error_count;
      Printf.eprintf "  ✗ %s: Unknown team - %s or %s\n" e.sch_date e.sch_home_team e.sch_away_team
    ) else (
      let status = Scraper.schedule_status_from_scores e.sch_home_score e.sch_away_score in
      let game_date = Scraper.normalize_schedule_date ~season_code:e.sch_season e.sch_date in
      let venue = if String.trim e.sch_venue = "" then None else Some e.sch_venue in
      (* Some schedule pages use 0-0 placeholders. Avoid writing fake scores to [games]. *)
      let home_score, away_score =
        match (e.sch_home_score, e.sch_away_score) with
        | Some 0, Some 0 -> (None, None)
        | hs, as_ -> (hs, as_)
      in
      let result_schedule =
        Db.with_db (fun db ->
          Db.Repo.upsert_schedule_entry
            ~game_date
            ~game_time:(Some e.sch_time)
            ~season_code:e.sch_season
            ~home_team_code:hc
            ~away_team_code:ac
            ~venue
            ~status
            db)
      in

      (match (e.sch_game_id, e.sch_game_type, e.sch_game_no) with
      | Some game_id, Some game_type, Some game_no ->
          if not (Hashtbl.mem seen_games game_id) then (
            Hashtbl.add seen_games game_id true;
            match Db.with_db (fun db ->
              Db.Repo.upsert_game_entry
                ~game_id
                ~season_code:e.sch_season
                ~game_type
                ~game_no
                ~game_date:(Some game_date)
                ~home_team_code:hc
                ~away_team_code:ac
                ~home_score
                ~away_score
                ~stadium:venue
                ~attendance:None
                db
            ) with
            | Ok () -> incr game_success
            | Error _ -> ()
          )
      | _ -> ());

      match result_schedule with
      | Ok () ->
          incr success_count;
          Printf.printf "  ✓ %s: %s vs %s (%s)\n" e.sch_date e.sch_away_team e.sch_home_team status
      | Error db_err ->
          incr error_count;
          Printf.eprintf "  ✗ %s: %s\n" e.sch_date (Db.show_db_error db_err)
    )
  );

  let _ = Db.refresh_matviews () in

  Printf.printf "\n=== Sync Complete ===\n";
  Printf.printf "Schedule: %d ok, %d errors | Games: %d upserted\n" !success_count !error_count !game_success;

  let completed_result =
    Db.with_db (fun db -> Db.Repo.count_schedule_by_status ~season_code ~status:"completed" db)
  in
  let scheduled_result =
    Db.with_db (fun db -> Db.Repo.count_schedule_by_status ~season_code ~status:"scheduled" db)
  in
  match (completed_result, scheduled_result) with
  | Ok (Some c), Ok (Some s) ->
      Printf.printf "Season %s: %d completed, %d scheduled (Total: %d)\n" season_code c s (c + s)
  | _ -> ()

(* Sync DataLab game results to database *)
let run_sync_games ~sw ~env ~season_filter =
  let db_url = match get_db_url () with
    | Some url -> url
    | None ->
        Printf.eprintf "Error: DATABASE_URL or WKBL_DATABASE_URL not set\n";
        exit 1
  in

  Printf.printf "Connecting to database...\n";
  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> Printf.printf "Database connected.\n"
  | Error e ->
      Printf.eprintf "Database connection failed: %s\n" (Db.show_db_error e);
      exit 1);

  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None ->
        (* Default: current season only (DataLab code) *)
        let current_code = Scraper.current_season_code_auto () in
        let name = Scraper.season_name_of_code current_code in
        [(current_code, name)]
  in

  let games = Scraper.fetch_all_games ~sw ~env ~seasons () in
  Printf.printf "\nFetched %d game records. Syncing...\n%!" (List.length games);

  let parse_game_id game_id =
    match String.split_on_char '-' game_id with
    | [season; game_type; game_no_str] ->
        int_of_string_opt game_no_str |> Option.map (fun game_no ->
          (season, game_type, game_no))
    | _ -> None
  in

  let synced = ref 0 in
  let skipped = ref 0 in
  let errors = ref 0 in

  games |> List.iter (fun (g : Scraper.game_record) ->
    match parse_game_id g.Scraper.game_id with
    | None ->
        incr skipped;
        if !skipped <= 5 then
          Printf.eprintf "  ✗ Skip (bad game_id): %s\n" g.Scraper.game_id
    | Some (season_code, game_type, game_no) ->
        let hc = Scraper.code_from_team_name g.Scraper.home_team_name in
        let ac = Scraper.code_from_team_name g.Scraper.away_team_name in
        if String.starts_with ~prefix:"XX_" hc || String.starts_with ~prefix:"XX_" ac then (
          incr skipped;
          if !skipped <= 5 then
            Printf.eprintf "  ✗ Unknown team codes: %s vs %s\n" g.Scraper.home_team_name g.Scraper.away_team_name
        ) else (
            let game_date = Scraper.normalize_game_date g.Scraper.game_date in
            let home_score = if g.Scraper.home_team_score > 0 then Some g.Scraper.home_team_score else None in
            let away_score = if g.Scraper.away_team_score > 0 then Some g.Scraper.away_team_score else None in
            let stadium = if String.trim g.Scraper.court_name = "" then None else Some g.Scraper.court_name in
            let season_code =
              if String.length g.Scraper.game_season = 3 then g.Scraper.game_season else season_code
            in
            (match Db.with_db (fun db ->
              Db.Repo.upsert_game
                ~game_id:g.Scraper.game_id
                ~season_code
                ~game_type
                ~game_no
                ~game_date
                ~home_team_code:hc
                ~away_team_code:ac
                ~home_score
                ~away_score
                ~stadium
                db
            ) with
            | Ok () -> incr synced
            | Error e ->
                incr errors;
                if !errors <= 5 then
                  Printf.eprintf "  ✗ DB error (%s): %s\n" g.Scraper.game_id (Db.show_db_error e))
        )
  );

  (match Db.with_db (fun db -> Db.Repo.refresh_matviews db) with
  | Ok () -> ()
  | Error e -> Printf.eprintf "  ✗ Failed to refresh matviews: %s\n" (Db.show_db_error e));

  Printf.printf "\n=== Sync Complete ===\n";
  Printf.printf "Synced: %d, Skipped: %d, Errors: %d\n%!" !synced !skipped !errors

(** Sync ALL historical schedules (1998-2026) to database *)
let run_sync_history ~sw ~env ~season_filter ~purge =
  (* Get DB URL *)
  let db_url = match get_db_url () with
    | Some url -> url
    | None ->
        Printf.eprintf "Error: DATABASE_URL or WKBL_DATABASE_URL not set\n";
        exit 1
  in

  (* Initialize DB pool *)
  Printf.printf "Connecting to database...\n";
  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> Printf.printf "Database connected.\n"
  | Error e ->
      Printf.eprintf "Database connection failed: %s\n" (Db.show_db_error e);
      exit 1);

  (* Determine which seasons to sync *)
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.all_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.all_season_codes
  in

  Printf.printf "Syncing %d seasons to database...\n\n" (List.length seasons);

  let total_success = ref 0 in
  let total_error = ref 0 in

	  seasons |> List.iter (fun (season_code, season_name) ->
	    Printf.printf "=== %s (%s) ===\n" season_name season_code;

    let _ = Db.with_db (fun db -> Db.Repo.upsert_season ~season_code ~season_name db) in

    (* Fetch schedule for this season *)
    let entries = Scraper.fetch_full_season_schedule ~sw ~env ~season_code ~season_name in
    Printf.printf "Fetched %d games. Syncing...\n" (List.length entries);

    let success = ref 0 in
    let errors = ref 0 in
    let game_success = ref 0 in
    let seen_games = Hashtbl.create (List.length entries * 2 + 1) in

    if purge && entries = [] then (
      Printf.eprintf "Error: fetched 0 schedule entries; refusing to purge season %s.\n%!" season_code;
      exit 1
    );

    if purge then (
      Printf.printf "  Purging existing schedule rows for season %s...\n%!" season_code;
      match Db.with_db (fun db -> Db.Repo.delete_schedule_by_season ~season_code db) with
      | Ok () -> Printf.printf "  ✓ Purged.\n%!"
      | Error e ->
          Printf.eprintf "  ✗ Purge failed: %s\n%!" (Db.show_db_error e);
          exit 1
    );

    entries |> List.iter (fun (e : Scraper.schedule_entry) ->
      let hc =
        match e.sch_home_team_code with
        | Some c -> c
        | None -> Scraper.code_from_team_name e.sch_home_team
      in
      let ac =
        match e.sch_away_team_code with
        | Some c -> c
        | None -> Scraper.code_from_team_name e.sch_away_team
      in
      if String.starts_with ~prefix:"XX_" hc || String.starts_with ~prefix:"XX_" ac then (
        incr errors;
        if !errors <= 3 then
          Printf.eprintf "  ✗ Unknown team: %s or %s\n" e.sch_home_team e.sch_away_team
      ) else (
        let status = Scraper.schedule_status_from_scores e.sch_home_score e.sch_away_score in
        let game_date = Scraper.normalize_schedule_date ~season_code e.sch_date in
        let venue = if String.trim e.sch_venue = "" then None else Some e.sch_venue in
        (* Some schedule pages use 0-0 placeholders. Avoid writing fake scores to [games]. *)
        let home_score, away_score =
          match (e.sch_home_score, e.sch_away_score) with
          | Some 0, Some 0 -> (None, None)
          | hs, as_ -> (hs, as_)
        in
        let result_schedule =
          Db.with_db (fun db ->
            Db.Repo.upsert_schedule_entry
              ~game_date
              ~game_time:(Some e.sch_time)
              ~season_code
              ~home_team_code:hc
              ~away_team_code:ac
              ~venue
              ~status
              db)
        in

        (match (e.sch_game_id, e.sch_game_type, e.sch_game_no) with
        | Some game_id, Some game_type, Some game_no ->
            if not (Hashtbl.mem seen_games game_id) then (
              Hashtbl.add seen_games game_id true;
              match Db.with_db (fun db ->
                Db.Repo.upsert_game_entry
                  ~game_id
                  ~season_code
                  ~game_type
                  ~game_no
                  ~game_date:(Some game_date)
                  ~home_team_code:hc
                  ~away_team_code:ac
                  ~home_score
                  ~away_score
                  ~stadium:venue
                  ~attendance:None
                  db
              ) with
              | Ok () -> incr game_success
              | Error _ -> ()
            )
        | _ -> ());

        match result_schedule with
        | Ok () -> incr success
        | Error _ -> incr errors
      )
    );

    Printf.printf "  ✓ schedule: %d synced, %d errors | games: %d upserted\n\n" !success !errors !game_success;
    total_success := !total_success + !success;
    total_error := !total_error + !errors
  );

  let _ = Db.refresh_matviews () in

  Printf.printf "\n=== History Sync Complete ===\n";
  Printf.printf "Total: %d synced, %d errors\n" !total_success !total_error

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  
  (* Global RNG initialization for HTTPS/TLS connectivity *)
  Mirage_crypto_rng_unix.use_default ();

  let csv_output = List.mem "--csv" args in
  let stats_output = List.mem "--stats" args in
  let season_filter =
    args |> List.find_map (fun arg ->
      if String.length arg > 9 && String.sub arg 0 9 = "--season=" then
        Some (String.sub arg 9 (String.length arg - 9))
      else None
    )
  in
  let month_filter =
    args |> List.find_map (fun arg ->
      if String.length arg > 8 && String.sub arg 0 8 = "--month=" then
        Some (String.sub arg 8 (String.length arg - 8))
      else None
    )
  in
  let purge = List.mem "--purge" args in

  match args with
  | [] | "--help" :: _ | "-h" :: _ ->
      print_string usage
  | "draft" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_draft ~sw ~env ~season_filter ~csv_output
  | "awards" :: "stats" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_stat_awards ~sw ~env ~csv_output
  | "awards" :: "best5" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_best5_awards ~sw ~env ~csv_output
  | "awards" :: _ ->
      Printf.eprintf "Unknown awards subcommand. Use 'stats' or 'best5'\n%s" usage;
      exit 1
  | "fa" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_fa ~sw ~env ~csv_output
  | "salary" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_salary ~sw ~env ~csv_output
  | "crowd" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_crowd ~sw ~env ~csv_output
  | "records" :: _ ->
      Eio_main.run @@ fun env ->
      Mirage_crypto_rng_unix.use_default ();
      Eio.Switch.run @@ fun sw ->
      run_records ~sw ~env ~csv_output
  | "games" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_games ~sw ~env ~season_filter ~csv_output
  | "teamstats" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_teamstats ~sw ~env ~season_filter ~csv_output
  | "versus" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_versus ~sw ~env ~season_filter ~csv_output
  | "championship" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_championship ~sw ~env ~csv_output ~stats_output
  | "allstar" :: _ ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      run_allstar ~sw ~env ~csv_output ~stats_output
  | "schedule" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_schedule ~sw ~env ~month_filter ~csv_output
  | "sync" :: "schedule" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_schedule ~sw ~env ~purge
  | "sync" :: "draft" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_draft ~sw ~env ~season_filter
  | "sync" :: "trade" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_trade ~sw ~env
  | "sync" :: "coaches" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_coaches ~sw ~env
  | "sync" :: "games" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_games ~sw ~env ~season_filter
  | "sync" :: "boxscore" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_boxscore ~sw ~env ~season_filter
  | "sync" :: "pbp" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_pbp ~sw ~env ~season_filter
  | "sync" :: "history" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_history ~sw ~env ~season_filter ~purge
  | "sync" :: _ ->
      Printf.eprintf "Unknown sync subcommand. Use 'schedule', 'draft', 'trade', 'coaches', 'games', 'boxscore', 'pbp', or 'history'\n%s" usage;
      exit 1
  | "history" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      let seasons = match season_filter with
        | Some code ->
            let name = List.assoc_opt code Scraper.all_season_codes
              |> Option.value ~default:"Unknown" in
            [(code, name)]
        | None -> Scraper.all_season_codes
      in
      let entries = Scraper.fetch_all_historical_schedules ~sw ~env ~seasons () in
      Printf.printf "\n=== Fetched %d historical games ===\n\n" (List.length entries);
      if csv_output then
        Scraper.print_schedule_csv entries
      else
        entries |> List.iter (fun (e : Scraper.schedule_entry) ->
          let score_str = match (e.sch_away_score, e.sch_home_score) with
            | Some a, Some h -> Printf.sprintf "[%d - %d]" a h
            | _ -> "[예정]"
          in
          Printf.printf "%s %s vs %s %s @ %s\n"
            e.sch_date
            e.sch_away_team
            e.sch_home_team
            score_str
            e.sch_venue
        )
  | cmd :: _ ->
      Printf.eprintf "Unknown command: %s\n%s" cmd usage;
      exit 1
