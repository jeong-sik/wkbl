(** WKBL Scraper CLI Tool

    Usage:
      scraper_tool draft [--csv]           Fetch all draft history
      scraper_tool draft --season=044      Fetch specific season
      scraper_tool awards stats [--csv]    Fetch statistical awards
      scraper_tool awards best5 [--csv]    Fetch BEST5 awards
      scraper_tool sync schedule           Sync schedule to database
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
  scraper_tool sync schedule           Sync current season schedule to database
  scraper_tool sync boxscore           Sync missing boxscores to database
  scraper_tool sync history            Sync ALL historical games to database (1998-2026)
  scraper_tool sync history --season=X Sync specific season to database
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

(** Sync boxscore data for games that don't have stats yet *)
let run_sync_boxscore ~sw ~env ~season_filter =
  let db_url = match get_db_url () with
    | Some url -> url
    | None -> Printf.eprintf "Error: DATABASE_URL not set\n"; exit 1
  in

  (match Db.init_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv) db_url with
  | Ok () -> ()
  | Error e -> Printf.eprintf "DB Error: %s\n" (Db.show_db_error e); exit 1);

  let season = Option.value ~default:"ALL" season_filter in
  match Db_sync.get_games_without_stats ~season () with
  | Error e -> Printf.eprintf "Failed to fetch missing games: %s\n" (Db.show_db_error e)
  | Ok game_ids ->
      let total = List.length game_ids in
      Printf.printf "Found %d games missing boxscores. Starting sync...\n%!" total;
      
      let clock = Eio.Stdenv.clock env in
      game_ids |> List.iteri (fun i game_id ->
        Printf.printf "[%d/%d] Syncing boxscore for game: %s\n%!" (i + 1) total game_id;
        
        match Db_sync.get_game_teams ~game_id with
        | Ok (Some (home_code, away_code)) ->
            (* Fetch and sync for both teams *)
            let teams = [home_code; away_code] in
            teams |> List.iter (fun team_code ->
              let stats = Scraper.fetch_game_boxscore ~sw ~env ~game_id ~team_code in
              stats |> List.iter (fun s ->
                match Db_sync.upsert_game_stat s ~game_id ~team_code with
                | Ok () -> ()
                | Error e -> Printf.eprintf "  ✗ Failed to upsert stat for %s: %s\n" s.bs_player_name (Db.show_db_error e)
              );
              Eio.Time.sleep clock 0.2 (* Rate limit between team requests *)
            )
        | _ -> Printf.eprintf "  ✗ Could not find teams for game %s\n" game_id
      );
      Printf.printf "\n=== Boxscore Sync Complete ===\n"

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
  let entries = match month_filter with
    | Some ym ->
        Printf.printf "Fetching schedule for %s...\n\n" ym;
        Scraper.fetch_schedule_month ~sw ~env ~ym ~season:"046"
    | None ->
        Scraper.fetch_season_schedule ~sw ~env ~season_code:"046"
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
let run_sync_schedule ~sw ~env =
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

  (* Fetch schedule from WKBL website *)
  Printf.printf "Fetching schedule from WKBL...\n";
  let entries = Scraper.fetch_season_schedule ~sw ~env ~season_code:"046" in
  Printf.printf "Fetched %d schedule entries.\n\n" (List.length entries);

  (* Sync each entry *)
  let success_count = ref 0 in
  let error_count = ref 0 in
  entries |> List.iter (fun (e : Scraper.schedule_entry) ->
    (* Convert team names to team codes *)
    let home_code = Domain.team_code_of_string e.sch_home_team in
    let away_code = Domain.team_code_of_string e.sch_away_team in
    match (home_code, away_code) with
    | Some hc, Some ac ->
        let status = match (e.sch_home_score, e.sch_away_score) with
          | Some _, Some _ -> "completed"
          | _ -> "scheduled"
        in
        let result = Db.with_db (fun db ->
          Db.Repo.upsert_schedule_entry
            ~game_date:e.sch_date
            ~game_time:(Some e.sch_time)
            ~season_code:e.sch_season
            ~home_team_code:hc
            ~away_team_code:ac
            ~venue:(Some e.sch_venue)
            ~status
            db
        ) in
        (match result with
        | Ok () ->
            incr success_count;
            Printf.printf "  ✓ %s: %s vs %s (%s)\n" e.sch_date e.sch_away_team e.sch_home_team status
        | Error db_err ->
            incr error_count;
            Printf.eprintf "  ✗ %s: %s\n" e.sch_date (Db.show_db_error db_err))
    | _ ->
        incr error_count;
        Printf.eprintf "  ✗ %s: Unknown team - %s or %s\n" e.sch_date e.sch_home_team e.sch_away_team
  );

  Printf.printf "\n=== Sync Complete ===\n";
  Printf.printf "Success: %d, Errors: %d\n" !success_count !error_count;

  (* Show summary - count from schedule table *)
  let completed_result = Db.with_db (fun db ->
    Db.Repo.count_schedule_by_status ~season_code:"046" ~status:"completed" db
  ) in
  let scheduled_result = Db.with_db (fun db ->
    Db.Repo.count_schedule_by_status ~season_code:"046" ~status:"scheduled" db
  ) in
  (match (completed_result, scheduled_result) with
  | Ok (Some c), Ok (Some s) ->
      Printf.printf "Season 046: %d completed, %d scheduled (Total: %d)\n" c s (c + s)
  | _ -> ())

(** Sync ALL historical schedules (1998-2026) to database *)
let run_sync_history ~sw ~env ~season_filter =
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

    (* Fetch schedule for this season *)
    let entries = Scraper.fetch_full_season_schedule ~sw ~env ~season_code ~season_name in
    Printf.printf "Fetched %d games. Syncing...\n" (List.length entries);

    let success = ref 0 in
    let errors = ref 0 in

    entries |> List.iter (fun (e : Scraper.schedule_entry) ->
      (* Convert team names to codes *)
      let home_code = Domain.team_code_of_string e.sch_home_team in
      let away_code = Domain.team_code_of_string e.sch_away_team in
      match (home_code, away_code) with
      | Some hc, Some ac ->
          let status = match (e.sch_home_score, e.sch_away_score) with
            | Some _, Some _ -> "completed"
            | _ -> "scheduled"
          in
          (* Normalize date format: "1/10(월)" → "2000-01-10" *)
          let game_date = Scraper.normalize_schedule_date ~season_code e.sch_date in
          let result = Db.with_db (fun db ->
            Db.Repo.upsert_schedule_entry
              ~game_date
              ~game_time:(Some e.sch_time)
              ~season_code
              ~home_team_code:hc
              ~away_team_code:ac
              ~venue:(Some e.sch_venue)
              ~status
              db
          ) in
          (match result with
          | Ok () -> incr success
          | Error _ -> incr errors)
      | _ ->
          incr errors;
          if !errors <= 3 then  (* Only show first few errors per season *)
            Printf.eprintf "  ✗ Unknown team: %s or %s\n" e.sch_home_team e.sch_away_team
    );

    Printf.printf "  ✓ %d synced, %d errors\n\n" !success !errors;
    total_success := !total_success + !success;
    total_error := !total_error + !errors
  );

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
      run_sync_schedule ~sw ~env
  | "sync" :: "boxscore" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_boxscore ~sw ~env ~season_filter
  | "sync" :: "history" :: _ ->
      run_with_rng @@ fun ~sw ~env ->
      run_sync_history ~sw ~env ~season_filter
  | "sync" :: _ ->
      Printf.eprintf "Unknown sync subcommand. Use 'schedule' or 'history'\n%s" usage;
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
