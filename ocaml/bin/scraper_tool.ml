(** WKBL Scraper CLI Tool

    Usage:
      scraper_tool draft [--csv]           Fetch all draft history
      scraper_tool draft --season=044      Fetch specific season
      scraper_tool awards stats [--csv]    Fetch statistical awards
      scraper_tool awards best5 [--csv]    Fetch BEST5 awards
*)

open Lwt.Syntax
open Wkbl

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
  scraper_tool allstar [--csv]         Fetch all-star game history (올스타 역사)
  scraper_tool --help                  Show this help

Examples:
  scraper_tool draft --csv > drafts.csv
  scraper_tool draft --season=044
  scraper_tool awards stats --csv > stat_awards.csv
  scraper_tool awards best5
  scraper_tool fa --csv > fa_results.csv
  scraper_tool salary --csv > salaries.csv
  scraper_tool crowd --csv > crowd.csv
  scraper_tool records --csv > records.csv
  scraper_tool games --csv > games.csv
  scraper_tool games --season=044 --csv
  scraper_tool teamstats --csv > team_stats.csv
  scraper_tool versus --csv > versus_records.csv
  scraper_tool championship --csv > championships.csv
  scraper_tool allstar --csv > allstars.csv
|}

let run_draft ~season_filter ~csv_output =
  let* entries = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.season_codes
          |> Option.value ~default:"Unknown" in
        Scraper.fetch_draft_season ~season_code:code ~season_name:name
    | None ->
        Scraper.fetch_all_drafts ()
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
    );
  Lwt.return ()

let run_stat_awards ~csv_output =
  let* entries = Scraper.fetch_stat_awards () in
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
    );
  Lwt.return ()

let run_best5_awards ~csv_output =
  let* entries = Scraper.fetch_best5_awards () in
  Printf.printf "\n=== Fetched %d BEST5 seasons ===\n\n" (List.length entries);
  if csv_output then
    Scraper.print_best5_csv entries
  else
    entries |> List.iter (fun e ->
      Printf.printf "[%s] BEST5:\n" e.Scraper.b5_season_name;
      e.players |> List.iteri (fun i player ->
        Printf.printf "  %d. %s\n" (i + 1) player
      )
    );
  Lwt.return ()

let run_fa ~csv_output =
  let* entries = Scraper.fetch_fa_results () in
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
    );
  Lwt.return ()

let run_salary ~csv_output =
  let* entries = Scraper.fetch_salary () in
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
    );
  Lwt.return ()

let run_crowd ~csv_output =
  let* entries = Scraper.fetch_crowd () in
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
    );
  Lwt.return ()

let run_records ~csv_output =
  let* entries = Scraper.fetch_major_records () in
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
    );
  Lwt.return ()

let run_games ~season_filter ~csv_output =
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.datalab_season_codes
  in
  let* games = Scraper.fetch_all_games ~seasons () in
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
    );
  Lwt.return ()

let run_teamstats ~season_filter ~csv_output =
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.datalab_season_codes
  in
  let* stats = Scraper.fetch_all_team_stats ~seasons () in
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
    );
  Lwt.return ()

let run_versus ~season_filter ~csv_output =
  let seasons = match season_filter with
    | Some code ->
        let name = List.assoc_opt code Scraper.datalab_season_codes
          |> Option.value ~default:"Unknown" in
        [(code, name)]
    | None -> Scraper.datalab_season_codes
  in
  (* Fetch from each season by iterating *)
  let* records =
    let rec fetch_all acc = function
      | [] -> Lwt.return (List.rev acc)
      | (code, _) :: rest ->
          let* season_records = Scraper.fetch_season_versus_records code in
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
    );
  Lwt.return ()

let run_championship ~csv_output =
  let* records = Scraper.fetch_championship_history () in
  Printf.printf "\n=== Fetched %d championship records ===\n\n" (List.length records);
  if csv_output then
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
    );
  Lwt.return ()

let run_allstar ~csv_output =
  let* records = Scraper.fetch_allstar_history () in
  Printf.printf "\n=== Fetched %d all-star records ===\n\n" (List.length records);
  if csv_output then
    Scraper.print_allstar_csv records
  else
    records |> List.iter (fun (a : Scraper.allstar_record) ->
      Printf.printf "[%s] 제%d회 @ %s (%s) | MVP: %s\n"
        a.as_season
        a.as_edition
        a.as_venue
        a.as_date
        a.as_mvp
    );
  Lwt.return ()

let () =
  let args = Array.to_list Sys.argv |> List.tl in

  let csv_output = List.mem "--csv" args in
  let season_filter =
    args |> List.find_map (fun arg ->
      if String.length arg > 9 && String.sub arg 0 9 = "--season=" then
        Some (String.sub arg 9 (String.length arg - 9))
      else None
    )
  in

  match args with
  | [] | "--help" :: _ | "-h" :: _ ->
      print_string usage
  | "draft" :: _ ->
      Lwt_main.run (run_draft ~season_filter ~csv_output)
  | "awards" :: "stats" :: _ ->
      Lwt_main.run (run_stat_awards ~csv_output)
  | "awards" :: "best5" :: _ ->
      Lwt_main.run (run_best5_awards ~csv_output)
  | "awards" :: _ ->
      Printf.eprintf "Unknown awards subcommand. Use 'stats' or 'best5'\n%s" usage;
      exit 1
  | "fa" :: _ ->
      Lwt_main.run (run_fa ~csv_output)
  | "salary" :: _ ->
      Lwt_main.run (run_salary ~csv_output)
  | "crowd" :: _ ->
      Lwt_main.run (run_crowd ~csv_output)
  | "records" :: _ ->
      Lwt_main.run (run_records ~csv_output)
  | "games" :: _ ->
      Lwt_main.run (run_games ~season_filter ~csv_output)
  | "teamstats" :: _ ->
      Lwt_main.run (run_teamstats ~season_filter ~csv_output)
  | "versus" :: _ ->
      Lwt_main.run (run_versus ~season_filter ~csv_output)
  | "championship" :: _ ->
      Lwt_main.run (run_championship ~csv_output)
  | "allstar" :: _ ->
      Lwt_main.run (run_allstar ~csv_output)
  | cmd :: _ ->
      Printf.eprintf "Unknown command: %s\n%s" cmd usage;
      exit 1
