(** Caqti type definitions for our domain.

    Extracted from db.ml module Types.
    Each definition maps OCaml types to Caqti row types for SQL query results.
*)

open Domain
open Db_common
open Caqti_type

(** Custom type for player_aggregate *)
let player_aggregate =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (name, rest) = rest in
    let (team_name, rest) = rest in
    let (games_played, rest) = rest in
    let (total_minutes, rest) = rest in
    let (total_points, rest) = rest in
    let (total_rebounds, rest) = rest in
    let (total_assists, rest) = rest in
    let (total_steals, rest) = rest in
    let (total_blocks, rest) = rest in
    let (total_turnovers, rest) = rest in
    let (avg_points, rest) = rest in
    let (avg_margin, rest) = rest in
    let (avg_rebounds, rest) = rest in
    let (avg_assists, rest) = rest in
    let (avg_steals, rest) = rest in
    let (avg_blocks, rest) = rest in
    let (avg_turnovers, rest) = rest in
    let (efficiency, rest) = rest in
    let (total_fg_made, rest) = rest in
    let (total_fg_att, rest) = rest in
    let (total_fg3_made, rest) = rest in
    let (total_fg3_att, rest) = rest in
    let (total_ft_made, total_ft_att) = rest in
    Ok {
      player_id;
      name;
      team_name;
      games_played;
      total_minutes;
      total_points;
      total_rebounds;
      total_assists;
      total_steals;
      total_blocks;
      total_turnovers;
      avg_points;
      avg_margin;
      avg_rebounds;
      avg_assists;
      avg_steals;
      avg_blocks;
      avg_turnovers;
      efficiency;
      total_fg_made;
      total_fg_att;
      total_fg3_made;
      total_fg3_att;
      total_ft_made;
      total_ft_att;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
      (t string
        (t string
          (t int
            (t float
              (t int
                (t int
                  (t int
                    (t int
                      (t int
                        (t int
                          (t float
                            (t float
                              (t float
                                (t float
                                  (t float
                                    (t float
                                      (t float
                                        (t float
                                          (t int
                                            (t int
                                              (t int
                                                (t int
                                                  (t int int))))))))))))))))))))))))

let player_base =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (name, rest) = rest in
    let (team_name, rest) = rest in
    let (games_played, rest) = rest in
    let (min_seconds, rest) = rest in
    let (total_points, rest) = rest in
    let (total_rebounds, rest) = rest in
    let (total_assists, rest) = rest in
    let (total_steals, rest) = rest in
    let (total_blocks, rest) = rest in
    let (total_turnovers, rest) = rest in
    let (avg_points, rest) = rest in
    let (avg_margin, rest) = rest in
    let (avg_rebounds, rest) = rest in
    let (avg_assists, rest) = rest in
    let (avg_steals, rest) = rest in
    let (avg_blocks, rest) = rest in
    let (avg_turnovers, rest) = rest in
    let (efficiency, margin_seconds) = rest in
    Ok {
      pb_player_id = player_id;
      pb_player_name = name;
      pb_team_name = team_name;
      pb_gp = games_played;
      pb_min_seconds = min_seconds;
      pb_pts = total_points;
      pb_reb = total_rebounds;
      pb_ast = total_assists;
      pb_stl = total_steals;
      pb_blk = total_blocks;
      pb_tov = total_turnovers;
      pb_avg_pts = avg_points;
      pb_margin = avg_margin;
      pb_avg_reb = avg_rebounds;
      pb_avg_ast = avg_assists;
      pb_avg_stl = avg_steals;
      pb_avg_blk = avg_blocks;
      pb_avg_tov = avg_turnovers;
      pb_eff = efficiency;
      pb_margin_seconds = margin_seconds;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
      (t string
        (t string
          (t int
            (t int
              (t int
                (t int
                  (t int
                    (t int
                      (t int
                        (t int
                          (t float
                            (t float
                              (t float
                                (t float
                                  (t float
                                    (t float
                                      (t float
                                        (t float int)))))))))))))))))))

let season_info =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (code, name) = Ok { code; name } in
  custom ~encode ~decode (t2 string string)

let team_info =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (team_code, team_name) = Ok { team_code; team_name } in
  custom ~encode ~decode (t2 string string)

let team_totals =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode
      ( season,
        ( team,
          ( gp,
            ( min_total,
              ( fg2_m,
                ( fg2_a,
                  ( fg3_m,
                    ( fg3_a,
                      ( ft_m,
                        ( ft_a,
                          ( reb_off,
                            ( reb_def,
                              ( reb,
                                ( ast,
                                  ( stl,
                                    ( blk,
                                      ( turnovers,
                                        pts )) )) )) )) )) )) )) )) )
    =
    Ok {
      season; team; gp; min_total;
      fg2_m; fg2_a; fg3_m; fg3_a; ft_m; ft_a;
      reb_off; reb_def; reb; ast; stl; blk; turnovers; pts;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t int (t float (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int int)))))))))))))))))

let team_margin =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (season, (team, (gp, (pts_for, pts_against)))) =
    Ok { season; team; gp; pts_for; pts_against }
  in
  custom ~encode ~decode (t2 string (t2 string (t2 int (t2 int int))))

let team_standing =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (team_name, (games_played, (wins, (losses, (avg_pts, avg_opp_pts))))) =
    let win_pct = if games_played = 0 then 0.0 else Stdlib.float wins /. Stdlib.float games_played in
    let diff = avg_pts -. avg_opp_pts in
    Ok { team_name; games_played; wins; losses; win_pct; gb = 0.0; avg_pts; avg_opp_pts; diff }
  in
  custom ~encode ~decode (t2 string (t2 int (t2 int (t2 int (t2 float float)))))

let game_summary =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (home_team, (away_team, (home_score, (away_score, game_type)))))) =
    Ok { game_id; game_date; home_team; away_team; home_score; away_score; game_type }
  in
  custom ~encode ~decode
    (t2 string (t2 string (t2 string (t2 string (t2 (option int) (t2 (option int) string))))))

let qa_score_mismatch =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (home_team, (away_team, (home_score, (away_score, (home_sum, away_sum))))))) =
    Ok { qsm_game_id = game_id;
         qsm_game_date = game_date;
         qsm_home_team = home_team;
         qsm_away_team = away_team;
         qsm_home_score = home_score;
         qsm_away_score = away_score;
         qsm_home_sum = home_sum;
         qsm_away_sum = away_sum;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t string
                (t (option int)
                   (t (option int)
                      (t (option int) (option int))))))))

let qa_team_count_anomaly =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, team_count) = Ok { qtca_game_id = game_id; qtca_team_count = team_count } in
  custom ~encode ~decode (t2 string int)

let qa_duplicate_player_row =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (team_code, (team_name, (player_id, (player_name, row_count))))) =
    Ok { qdpr_game_id = game_id;
         qdpr_team_code = team_code;
         qdpr_team_name = team_name;
         qdpr_player_id = player_id;
         qdpr_player_name = player_name;
         qdpr_row_count = row_count;
    }
  in
  let t = t2 in
  custom ~encode ~decode (t string (t string (t string (t string (t string int)))))

let qa_duplicate_player_name_row =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_name, (id_count, player_ids_csv)) =
    Ok (player_name, id_count, player_ids_csv)
  in
  custom ~encode ~decode (t2 string (t2 int string))

let qa_duplicate_player_identity_row =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_name, (birth_date, (id_count, player_ids_csv))) =
    Ok (player_name, birth_date, id_count, player_ids_csv)
  in
  custom ~encode ~decode (t2 string (t2 string (t2 int string)))

let qa_schedule_missing_game =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_date, (season_code, (home_team, away_team))) =
    Ok {
      qsmg_game_date = game_date;
      qsmg_season_code = season_code;
      qsmg_home_team = home_team;
      qsmg_away_team = away_team;
    }
  in
  custom ~encode ~decode (t2 string (t2 string (t2 string string)))

let qa_schedule_missing_stats =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (home_team, away_team))) =
    Ok {
      qsms_game_id = game_id;
      qsms_game_date = game_date;
      qsms_home_team = home_team;
      qsms_away_team = away_team;
    }
  in
  custom ~encode ~decode (t2 string (t2 string (t2 string string)))

let qa_pbp_missing_game =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (home_team, (away_team, (home_score, away_score))))) =
    Ok {
      qpmg_game_id = game_id;
      qpmg_game_date = game_date;
      qpmg_home_team = home_team;
      qpmg_away_team = away_team;
      qpmg_home_score = home_score;
      qpmg_away_score = away_score;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t string
                (t int int)))))

let qa_schedule_coverage =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (season_code, (schedule_completed, (games_total, (matched, (missing, (coverage_pct, (season_uningested, games_missing_team))))))) =
    Ok {
      qsc_season_code = season_code;
      qsc_schedule_completed = schedule_completed;
      qsc_games_total = games_total;
      qsc_matched = matched;
      qsc_missing = missing;
      qsc_coverage_pct = coverage_pct;
      qsc_season_uningested = season_uningested;
      qsc_games_missing_team = games_missing_team;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t int
          (t int
             (t int
                (t int
                   (t float
                      (t bool bool)))))))

let qa_schedule_missing_summary =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (missing_ingested, (missing_uningested, missing_total)) =
    Ok {
      qsms_missing_ingested = missing_ingested;
      qsms_missing_uningested = missing_uningested;
      qsms_missing_total = missing_total;
    }
  in
  custom ~encode ~decode (t2 int (t2 int int))

let qa_schedule_missing_sample =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (season_code, (game_date, (home_team_code, (away_team_code, (reason, (games_on_date, games_info)))))) =
    Ok {
      qsmp_season_code = season_code;
      qsmp_game_date = game_date;
      qsmp_home_team_code = home_team_code;
      qsmp_away_team_code = away_team_code;
      qsmp_reason = reason;
      qsmp_games_on_date = games_on_date;
      qsmp_games_info = games_info;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
      (t string
        (t string
          (t string
            (t string
              (t int (option string)))))))

let qa_stat_anomaly =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (team_name, (player_id, (player_name, (min_seconds, (pts, (primary_team_name, (primary_gp, primary_min_seconds))))))))) =
    Ok {
      qsa_game_id = game_id;
      qsa_game_date = game_date;
      qsa_team_name = team_name;
      qsa_player_id = player_id;
      qsa_player_name = player_name;
      qsa_min_seconds = min_seconds;
      qsa_pts = pts;
      qsa_primary_team_name = primary_team_name;
      qsa_primary_gp = primary_gp;
      qsa_primary_min_seconds = primary_min_seconds;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t string
                (t string
                   (t int
                      (t int
                         (t string
                            (t int int)))))))))

let qa_stat_exclusion =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (team_name, (player_id, (player_name, (min_seconds, (pts, (reason, created_at)))))))) =
    Ok {
      qse_game_id = game_id;
      qse_game_date = game_date;
      qse_team_name = team_name;
      qse_player_id = player_id;
      qse_player_name = player_name;
      qse_min_seconds = min_seconds;
      qse_pts = pts;
      qse_reason = reason;
      qse_created_at = created_at;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t string
                (t string
                   (t int
                      (t int
                         (t string string))))))))
let boxscore_player_stat =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, (player_name, (position, (team_code, (team_name, (min_seconds, (pts, (plus_minus, (reb, (ast, (stl, (blk, (tov, (fg2_m, (fg2_a, (fg3_m, (fg3_a, (ft_m, ft_a)))))))))))))))))) =
    let min = Stdlib.float min_seconds /. 60.0 in
    let fg_m = fg2_m + fg3_m in
    let fg_a = fg2_a + fg3_a in
    let pct m a = if a = 0 then 0.0 else Stdlib.float m /. Stdlib.float a *. 100.0 in
    Ok {
      bs_player_id = player_id;
      bs_player_name = player_name;
      bs_position = position;
      bs_team_code = team_code;
      bs_team_name = team_name;
      bs_minutes = min; bs_pts = pts; bs_plus_minus = plus_minus; bs_reb = reb; bs_ast = ast; bs_stl = stl; bs_blk = blk; bs_tov = tov;
      bs_fg_made = fg_m; bs_fg_att = fg_a; bs_fg_pct = pct fg_m fg_a;
      bs_fg3_made = fg3_m; bs_fg3_att = fg3_a; bs_fg3_pct = pct fg3_m fg3_a;
      bs_ft_made = ft_m; bs_ft_att = ft_a; bs_ft_pct = pct ft_m ft_a;
    }
  in
  let t = t2 in
  let stats_tail =
    t int
      (t int
         (t int
            (t int
               (t int
                  (t int
                     (t int
                        (t int
                           (t int
                              (t int int)))))))))
  in
  custom ~encode ~decode
    (t string
       (t string
          (t (option string)
             (t string
                (t string
                   (t int
                      (t int
                         (t (option int) stats_tail))))))))

let game_info =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (home_code, (home_name, (away_code, (away_name, (home_score, (away_score, score_quality_int)))))))) =
    Ok {
      gi_game_id = game_id; gi_game_date = game_date;
      gi_home_team_code = home_code; gi_home_team_name = home_name;
      gi_away_team_code = away_code; gi_away_team_name = away_name;
      gi_home_score = home_score;
      gi_away_score = away_score;
      gi_score_quality = game_score_quality_of_int score_quality_int;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t string (t string (t string (t string (t int (t int int))))))))

let leader_entry =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, (player_name, (team_name, stat_value))) =
    Ok { le_player_id = player_id; le_player_name = player_name; le_team_name = team_name; le_stat_value = stat_value }
  in
  custom ~encode ~decode (t2 string (t2 string (t2 string float)))

let leader_base =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (player_name, rest) = rest in
    let (team_name, rest) = rest in
    let (gp, rest) = rest in
    let (min_seconds, rest) = rest in
    let (pts, rest) = rest in
    let (reb, rest) = rest in
    let (ast, rest) = rest in
    let (stl, rest) = rest in
    let (blk, rest) = rest in
    let (tov, rest) = rest in
    let (eff, rest) = rest in
    let (fg_m, rest) = rest in
    let (fg_a, rest) = rest in
    let (fg3_m, rest) = rest in
    let (fg3_a, rest) = rest in
    let (ft_m, ft_a) = rest in
    Ok {
      lb_player_id = player_id;
      lb_player_name = player_name;
      lb_team_name = team_name;
      lb_gp = gp;
      lb_min_seconds = min_seconds;
      lb_pts = pts;
      lb_reb = reb;
      lb_ast = ast;
      lb_stl = stl;
      lb_blk = blk;
      lb_tov = tov;
      lb_eff = eff;
      lb_fg_m = fg_m;
      lb_fg_a = fg_a;
      lb_fg3_m = fg3_m;
      lb_fg3_a = fg3_a;
      lb_ft_m = ft_m;
      lb_ft_a = ft_a;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t int
                (t int
                   (t int
                      (t int
                         (t int
                            (t int
                               (t int
                                  (t int
                                     (t float
       (t int
          (t int
             (t int
                (t int
                   (t int int)))))))))))))))))

let player_info =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (id, (name, (position, (birth_date, (height, weight))))) =
    Ok { id; name; position; birth_date; height; weight }
  in
  let t = t2 in
  custom ~encode ~decode (t string (t string (t (option string) (t (option string) (t (option int) (option int))))))

let player_draft =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (draft_year, rest) = rest in
    let (draft_round, rest) = rest in
    let (pick_in_round, rest) = rest in
    let (overall_pick, rest) = rest in
    let (draft_team, rest) = rest in
    let (raw_text, rest) = rest in
    let (source_url, scraped_at) = rest in
    Ok {
      pd_player_id = player_id;
      pd_draft_year = draft_year;
      pd_draft_round = draft_round;
      pd_pick_in_round = pick_in_round;
      pd_overall_pick = overall_pick;
      pd_draft_team = draft_team;
      pd_raw_text = raw_text;
      pd_source_url = source_url;
      pd_scraped_at = scraped_at;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t (option int)
          (t (option int)
             (t (option int)
                (t (option int)
                   (t (option string) (t string (t string string))))))))

let draft_pick_row =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (player_name, rest) = rest in
    let (draft_year, rest) = rest in
    let (draft_round, rest) = rest in
    let (pick_in_round, rest) = rest in
    let (overall_pick, rest) = rest in
    let (draft_team, rest) = rest in
    let (raw_text, rest) = rest in
    let (source_url, scraped_at) = rest in
    Ok {
      dpr_player_id = player_id;
      dpr_player_name = player_name;
      dpr_draft_year = draft_year;
      dpr_draft_round = draft_round;
      dpr_pick_in_round = pick_in_round;
      dpr_overall_pick = overall_pick;
      dpr_draft_team = draft_team;
      dpr_raw_text = raw_text;
      dpr_source_url = source_url;
      dpr_scraped_at = scraped_at;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t (option int)
             (t (option int)
                (t (option int)
                   (t (option int)
                      (t (option string) (t string (t string string)))))))))

	  let official_trade_event =
	    let encode _ = Error "Encode not supported: read-only type" in
	    let decode (event_date, rest) =
	      let (event_year, rest) = rest in
	      let (event_text, rest) = rest in
	      let (source_url, scraped_at) = rest in
	      Ok {
	        ote_event_date = event_date;
	        ote_event_year = event_year;
	        ote_event_text = event_text;
	        ote_source_url = source_url;
	        ote_scraped_at = scraped_at;
	      }
	    in
	    let t = t2 in
	    custom ~encode ~decode (t string (t int (t string (t string string))))

	  let player_external_link =
	    let encode _ = Error "Encode not supported: read-only type" in
	    let decode (player_id, rest) =
	      let (link_type, rest) = rest in
	      let (url, rest) = rest in
	      let (source_url, scraped_at) = rest in
	      Ok {
	        pel_player_id = player_id;
	        pel_link_type = link_type;
	        pel_url = url;
	        pel_source_url = source_url;
	        pel_scraped_at = scraped_at;
	      }
	    in
	    let t = t2 in
	    custom ~encode ~decode (t string (t string (t string (t (option string) string))))

	  let season_stats =
	    let encode _ = Error "Encode not supported: read-only type" in
	    let decode (season_code, rest) =
	      let (season_name, rest) = rest in
    let (team_name, rest) = rest in
    let (gp, rest) = rest in
    let (min, rest) = rest in
    let (pts, rest) = rest in
    let (reb, rest) = rest in
    let (ast, rest) = rest in
    let (stl, rest) = rest in
    let (blk, rest) = rest in
    let (tov, rest) = rest in
    let (eff, rest) = rest in
    let (margin, rest) = rest in
    let (fg_pct, rest) = rest in
    let (fg3_pct, rest) = rest in
    let (ft_pct, rest) = rest in
    let (ts, rest) = rest in
    let (efg, usg) = rest in
    Ok {
      ss_season_code = season_code;
      ss_season_name = season_name;
      ss_team_name = team_name;
      ss_games_played = gp;
      ss_total_minutes = min;
      ss_avg_points = pts;
      ss_avg_rebounds = reb;
      ss_avg_assists = ast;
      ss_avg_steals = stl;
      ss_avg_blocks = blk;
      ss_avg_turnovers = tov;
      ss_efficiency = eff;
      ss_margin = margin;
      ss_fg_pct = fg_pct;
      ss_fg3_pct = fg3_pct;
      ss_ft_pct = ft_pct;
      ss_ts_pct = ts;
      ss_efg_pct = efg;
      ss_usg_pct = usg;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t string (t int (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float float))))))))))))))))))

let player_game_stat =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (fg_made, (fg_att, (fg3_made, (fg3_att, (ft_made, (ft_att, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus)))))))))))))))))))) =
    let is_home = is_home_int = 1 in
    Ok { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; fg_made; fg_att; fg3_made; fg3_att; ft_made; ft_att; pts; reb; ast; stl; blk; tov; plus_minus }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t int
                (t (option int)
                   (t (option int)
                      (t int
                         (t float
                            (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (option int)))))))))))))))))))))

let player_game_stat_with_id =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (fg_made, (fg_att, (fg3_made, (fg3_att, (ft_made, (ft_att, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus))))))))))))))))))))) =
    let is_home = is_home_int = 1 in
    let stat = { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; fg_made; fg_att; fg3_made; fg3_att; ft_made; ft_att; pts; reb; ast; stl; blk; tov; plus_minus } in
    Ok { pgs_player_id = player_id; pgs_stat = stat }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string
       (t string
          (t string
             (t string
                (t int
                   (t (option int)
                      (t (option int)
                         (t int
                            (t float
                               (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (option int))))))))))))))))))))))

let team_game_result =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opp_score, is_win_int)))))) =
    Ok { tgr_game_id = game_id; tgr_game_date = game_date; tgr_opponent = opponent; tgr_is_home = is_home_int = 1; tgr_team_score = team_score; tgr_opponent_score = opp_score; tgr_is_win = is_win_int = 1 }
  in
  let t = t2 in
  custom ~encode ~decode (t string (t string (t string (t int (t int (t int int))))))

let h2h_game =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (game_id, (game_date, (p1_team, (p2_team, (p1_pts, (p1_reb, (p1_ast, (p1_stl, (p1_blk, (p2_pts, (p2_reb, (p2_ast, (p2_stl, (p2_blk, (winner, score_diff))))))))))))))) =
    Ok { hg_game_id = game_id; hg_game_date = game_date; player1_team = p1_team; player2_team = p2_team;
         player1_pts = p1_pts; player1_reb = p1_reb; player1_ast = p1_ast;
         player1_stl = p1_stl; player1_blk = p1_blk;
         player2_pts = p2_pts; player2_reb = p2_reb; player2_ast = p2_ast;
         player2_stl = p2_stl; player2_blk = p2_blk;
         winner_team = winner; score_diff }
  in
  let t = t2 in
  custom ~encode ~decode (t string (t string (t string (t string (t int (t int (t int (t int (t int (t int (t int (t int (t int (t int (t string int)))))))))))))))

let pbp_event =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (period_code, rest) =
    let (event_index, rest) = rest in
    let (team_side, rest) = rest in
    let (description, rest) = rest in
    let (team1_score, rest) = rest in
    let (team2_score, clock) = rest in
    Ok {
      pe_period_code = period_code;
      pe_event_index = event_index;
      pe_team_side = team_side;
      pe_description = description;
      pe_team1_score = team1_score;
      pe_team2_score = team2_score;
      pe_clock = clock;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t int (t int (t string (t (option int) (t (option int) string))))))

(** Historical season type *)
let historical_season =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (season_id, rest) =
    let (season_name, rest) = rest in
    let (champion_team, rest) = rest in
    let (runner_up, rest) = rest in
    let (regular_mvp, rest) = rest in
    let (finals_mvp, rest) = rest in
    let (rookie_of_year, rest) = rest in
    let (scoring_leader, notes) = rest in
    Ok {
      hs_season_id = season_id;
      hs_season_name = season_name;
      hs_champion_team = champion_team;
      hs_runner_up = runner_up;
      hs_regular_mvp = regular_mvp;
      hs_finals_mvp = finals_mvp;
      hs_rookie_of_year = rookie_of_year;
      hs_scoring_leader = scoring_leader;
      hs_notes = notes;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t (option string) (t (option string) (t (option string) (t (option string) (t (option string) (t (option string) (option string)))))))))

(** Legend player type *)
let legend_player =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_name, rest) =
    let (career_years, rest) = rest in
    let (teams, rest) = rest in
    let (championships, rest) = rest in
    let (mvp_count, rest) = rest in
    let (all_star_count, rest) = rest in
    let (career_points, rest) = rest in
    let (career_rebounds, rest) = rest in
    let (career_assists, rest) = rest in
    let (notable_achievements, is_hall_of_fame) = rest in
    Ok {
      lp_player_name = player_name;
      lp_career_years = career_years;
      lp_teams = teams;
      lp_championships = championships;
      lp_mvp_count = mvp_count;
      lp_all_star_count = all_star_count;
      lp_career_points = career_points;
      lp_career_rebounds = career_rebounds;
      lp_career_assists = career_assists;
      lp_notable_achievements = notable_achievements;
      lp_is_hall_of_fame = is_hall_of_fame;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t (option string) (t (option string) (t int (t int (t int (t int (t int (t int (t (option string) bool))))))))))

(** Coach type *)
let coach =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (coach_name, rest) =
    let (team, rest) = rest in
    let (tenure_start, rest) = rest in
    let (tenure_end, rest) = rest in
    let (championships, rest) = rest in
    let (regular_season_wins, rest) = rest in
    let (playoff_wins, rest) = rest in
    let (former_player, rest) = rest in
    let (player_career_years, notable_achievements) = rest in
    Ok {
      c_coach_name = coach_name;
      c_team = team;
      c_tenure_start = tenure_start;
      c_tenure_end = tenure_end;
      c_championships = championships;
      c_regular_season_wins = regular_season_wins;
      c_playoff_wins = playoff_wins;
      c_former_player = former_player;
      c_player_career_years = player_career_years;
      c_notable_achievements = notable_achievements;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t (option string) (t (option int) (t (option int) (t int (t int (t int (t bool (t (option string) (option string))))))))))

(** Player career entry type *)
let player_career_entry =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_name, rest) =
    let (season_id, rest) = rest in
    let (team, rest) = rest in
    let (jersey_number, rest) = rest in
    let (games_played, rest) = rest in
    let (points_per_game, rest) = rest in
    let (rebounds_per_game, rest) = rest in
    let (assists_per_game, rest) = rest in
    let (is_allstar, awards) = rest in
    Ok {
      pce_player_name = player_name;
      pce_season_id = season_id;
      pce_team = team;
      pce_jersey_number = jersey_number;
      pce_games_played = games_played;
      pce_points_per_game = points_per_game;
      pce_rebounds_per_game = rebounds_per_game;
      pce_assists_per_game = assists_per_game;
      pce_is_allstar = is_allstar;
      pce_awards = awards;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t string (t (option int) (t (option int) (t (option float) (t (option float) (t (option float) (t bool (option string))))))))))

(** MVP candidate type for MVP Race feature *)
let mvp_candidate =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (player_name, rest) = rest in
    let (team_name, rest) = rest in
    let (games_played, rest) = rest in
    let (ppg, rest) = rest in
    let (rpg, rest) = rest in
    let (apg, rest) = rest in
    let (spg, rest) = rest in
    let (bpg, rest) = rest in
    let (efficiency, rest) = rest in
    let (team_wins, rest) = rest in
    let (team_losses, team_win_pct) = rest in
    let team_code = team_code_of_string team_name in
    let (base_score, win_bonus, final_score) = calculate_mvp_score ~ppg ~rpg ~apg ~spg ~bpg ~efficiency ~win_pct:team_win_pct in
    Ok {
      mvp_rank = 0;  (* Will be set during list processing *)
      mvp_player_id = player_id;
      mvp_player_name = player_name;
      mvp_team_name = team_name;
      mvp_team_code = team_code;
      mvp_games_played = games_played;
      mvp_ppg = ppg;
      mvp_rpg = rpg;
      mvp_apg = apg;
      mvp_spg = spg;
      mvp_bpg = bpg;
      mvp_efficiency = efficiency;
      mvp_team_wins = team_wins;
      mvp_team_losses = team_losses;
      mvp_team_win_pct = team_win_pct;
      mvp_base_score = base_score;
      mvp_win_bonus = win_bonus;
      mvp_final_score = final_score;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t string (t int (t float (t float (t float (t float (t float (t float (t int (t int float))))))))))))

(** Clutch time statistics type
    Aggregates scoring events from Q4 last 5 minutes with score diff <= 5 *)
let clutch_stats =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (player_name, rest) = rest in
    let (team_name, rest) = rest in
    let (clutch_games, rest) = rest in
    let (clutch_points, rest) = rest in
    let (clutch_fg_made, rest) = rest in
    let (clutch_fg_att, rest) = rest in
    let (clutch_fg_pct, rest) = rest in
    let (clutch_ft_made, rest) = rest in
    let (clutch_ft_att, clutch_3p_made) = rest in
    Ok {
      cs_player_id = player_id;
      cs_player_name = player_name;
      cs_team_name = team_name;
      cs_clutch_games = clutch_games;
      cs_clutch_points = clutch_points;
      cs_clutch_fg_made = clutch_fg_made;
      cs_clutch_fg_att = clutch_fg_att;
      cs_clutch_fg_pct = clutch_fg_pct;
      cs_clutch_ft_made = clutch_ft_made;
      cs_clutch_ft_att = clutch_ft_att;
      cs_clutch_3p_made = clutch_3p_made;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t string (t int (t int (t int (t int (t float (t int (t int int))))))))))

(** Player shooting stats for shot distribution chart *)
let player_shooting_stats =
  let encode _ = Error "Encode not supported: read-only type" in
  let decode (player_id, rest) =
    let (name, rest) = rest in
    let (games, rest) = rest in
    let (fg_made, rest) = rest in
    let (fg_attempted, rest) = rest in
    let (fg_pct, rest) = rest in
    let (fg3_made, rest) = rest in
    let (fg3_attempted, rest) = rest in
    let (fg3_pct, rest) = rest in
    let (ft_made, rest) = rest in
    let (ft_attempted, ft_pct) = rest in
    Ok {
      pss_player_id = player_id;
      pss_name = name;
      pss_games = games;
      pss_fg_made = fg_made;
      pss_fg_attempted = fg_attempted;
      pss_fg_pct = fg_pct;
      pss_fg3_made = fg3_made;
      pss_fg3_attempted = fg3_attempted;
      pss_fg3_pct = fg3_pct;
      pss_ft_made = ft_made;
      pss_ft_attempted = ft_attempted;
      pss_ft_pct = ft_pct;
    }
  in
  let t = t2 in
  custom ~encode ~decode
    (t string (t string (t int (t int (t int (t float (t int (t int (t float (t int (t int float)))))))))))
