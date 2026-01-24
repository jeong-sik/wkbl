(** Database Layer using Caqti

    Principles:
    - Type-safe queries
    - Connection pooling via Caqti
    - All errors as Result, never exceptions
    - Memory caching for performance (TTL-based)
*)

open Domain

(** Database error type - explicit, not string *)
type db_error =
  | ConnectionFailed of string
  | QueryFailed of string
  | ParseError of string
[@@deriving show]

(** Result type alias for convenience - Direct style with Eio *)
type 'a db_result = ('a, db_error) result

(** DB QA (data quality) report types. *)
type qa_score_mismatch = {
  qsm_game_id: string;
  qsm_game_date: string;
  qsm_home_team: string;
  qsm_away_team: string;
  qsm_home_score: int option;
  qsm_away_score: int option;
  qsm_home_sum: int option;
  qsm_away_sum: int option;
}

type qa_team_count_anomaly = {
  qtca_game_id: string;
  qtca_team_count: int;
}

type qa_duplicate_player_row = {
  qdpr_game_id: string;
  qdpr_team_code: string;
  qdpr_team_name: string;
  qdpr_player_id: string;
  qdpr_player_name: string;
  qdpr_row_count: int;
}

type qa_duplicate_player_name = {
  qdpn_player_name: string;
  qdpn_id_count: int;
  qdpn_player_ids: string list;
}

type qa_db_report = {
  qdr_generated_at: string;
  qdr_games_total: int;
  qdr_games_with_stats: int;
  qdr_plus_minus_games: int;
  qdr_plus_minus_coverage_pct: float;
  qdr_score_mismatch_count: int;
  qdr_score_mismatch_sample: qa_score_mismatch list;
  qdr_team_count_anomaly_count: int;
  qdr_team_count_anomaly_sample: qa_team_count_anomaly list;
  qdr_duplicate_player_row_count: int;
  qdr_duplicate_player_row_sample: qa_duplicate_player_row list;
  qdr_duplicate_player_name_count: int;
  qdr_duplicate_player_name_sample: qa_duplicate_player_name list;
}

type leader_base = {
  lb_player_id: string;
  lb_player_name: string;
  lb_team_name: string;
  lb_gp: int;
  lb_min_seconds: int;
  lb_pts: int;
  lb_reb: int;
  lb_ast: int;
  lb_stl: int;
  lb_blk: int;
  lb_tov: int;
  lb_eff: float;
  lb_fg_m: int;
  lb_fg_a: int;
  lb_fg3_m: int;
  lb_fg3_a: int;
  lb_ft_m: int;
  lb_ft_a: int;
}

type player_base = {
  pb_player_id: string;
  pb_player_name: string;
  pb_team_name: string;
  pb_gp: int;
  pb_min_seconds: int;
  pb_pts: int;
  pb_reb: int;
  pb_ast: int;
  pb_stl: int;
  pb_blk: int;
  pb_tov: int;
  pb_avg_pts: float;
  pb_margin: float;
  pb_avg_reb: float;
  pb_avg_ast: float;
  pb_avg_stl: float;
  pb_avg_blk: float;
  pb_avg_tov: float;
  pb_eff: float;
  pb_margin_seconds: int;
}

(** Database connection URI *)
let default_db_path = "../data/wkbl.db"

let normalize_search_pattern search =
  let trimmed = String.trim search in
  if trimmed = "" then "%" else "%" ^ trimmed ^ "%"

module MarginKey = struct
  type t = string * string
  let compare = compare
end

module MarginMap = Map.Make (MarginKey)

(** Caqti type definitions for our domain *)
module Types = struct
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
      let (avg_turnovers, efficiency) = rest in
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
                                          (t float float))))))))))))))))))

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
      let (gp, rest) = rest in
      let (min, rest) = rest in
      let (pts, rest) = rest in
      let (reb, rest) = rest in
      let (ast, rest) = rest in
      let (stl, rest) = rest in
      let (blk, rest) = rest in
      let (tov, rest) = rest in
      let (eff, rest) = rest in
      let (margin, (ts, efg)) = rest in
      Ok {
        ss_season_code = season_code;
        ss_season_name = season_name;
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
        ss_ts_pct = ts;
        ss_efg_pct = efg;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t string (t string (t int (t float (t float (t float (t float (t float (t float (t float (t float (t float (t float float)))))))))))))

  let player_game_stat =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus)))))))))))))) =
      let is_home = is_home_int = 1 in
      Ok { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; pts; reb; ast; stl; blk; tov; plus_minus }
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
                              (t int (t int (t int (t int (t int (t int (option int)))))))))))))))

  let player_game_stat_with_id =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (player_id, (game_id, (game_date, (opponent, (is_home_int, (team_score, (opponent_score, (score_quality_int, (min, (pts, (reb, (ast, (stl, (blk, (tov, plus_minus))))))))))))))) =
      let is_home = is_home_int = 1 in
      let stat = { game_id; game_date; opponent; is_home; team_score; opponent_score; score_quality = game_score_quality_of_int score_quality_int; min; pts; reb; ast; stl; blk; tov; plus_minus } in
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
                                 (t int (t int (t int (t int (t int (t int (option int))))))))))))))))

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
      Ok { game_id; game_date; player1_team = p1_team; player2_team = p2_team;
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
end

(** Use oneshot queries to avoid prepared-statement conflicts with PgBouncer. *)
module Request_oneshot = struct
  include Caqti_request.Infix

  let ( ->. ) pt rt ?(oneshot = true) s =
    Caqti_request.Infix.( ->. ) pt rt ~oneshot s

  let ( ->? ) pt rt ?(oneshot = true) s =
    Caqti_request.Infix.( ->? ) pt rt ~oneshot s

  let ( ->* ) pt rt ?(oneshot = true) s =
    Caqti_request.Infix.( ->* ) pt rt ~oneshot s
end

(** SQL Queries *)
module Queries = struct
  open Request_oneshot
  open Caqti_type
  let ensure_player_plus_minus_table = (unit ->. unit) {|
    CREATE TABLE IF NOT EXISTS player_plus_minus (
      game_id TEXT NOT NULL,
      player_id TEXT NOT NULL,
      plus_minus INTEGER NOT NULL,
      PRIMARY KEY (game_id, player_id)
    )
  |}
  let ensure_player_plus_minus_index = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_player_plus_minus_player_id ON player_plus_minus(player_id)
  |}
  let ensure_play_by_play_events_table = (unit ->. unit) {|
    CREATE TABLE IF NOT EXISTS play_by_play_events (
      id SERIAL PRIMARY KEY,
      game_id TEXT NOT NULL,
      period_code TEXT NOT NULL,
      event_index INTEGER NOT NULL,
      team_side INTEGER NOT NULL,
      description TEXT NOT NULL,
      team1_score INTEGER,
      team2_score INTEGER,
      clock TEXT NOT NULL,
      player_id TEXT,
      UNIQUE (game_id, period_code, event_index)
    )
  |}

  (* Migration: Add player_id column if missing (for existing databases) *)
  let ensure_play_by_play_events_player_id_column = (unit ->. unit) {|
    ALTER TABLE play_by_play_events ADD COLUMN IF NOT EXISTS player_id TEXT
  |}
  let ensure_play_by_play_events_index_game = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_pbp_game_id ON play_by_play_events(game_id)
  |}
  let ensure_play_by_play_events_index_game_period = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_pbp_game_period ON play_by_play_events(game_id, period_code)
  |}
  let ensure_play_by_play_events_index_player = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_pbp_player_id ON play_by_play_events(player_id) WHERE player_id IS NOT NULL
  |}
  let ensure_player_drafts_table = (unit ->. unit) {|
    CREATE TABLE IF NOT EXISTS player_drafts (
      player_id TEXT PRIMARY KEY,
      draft_year INTEGER,
      draft_round INTEGER,
      pick_in_round INTEGER,
      overall_pick INTEGER,
      draft_team TEXT,
      raw_text TEXT NOT NULL,
      source_url TEXT NOT NULL,
      scraped_at TEXT NOT NULL
    )
  |}
  let ensure_player_drafts_index_year = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_player_drafts_year ON player_drafts(draft_year)
  |}
  let ensure_official_trade_events_table = (unit ->. unit) {|
    CREATE TABLE IF NOT EXISTS official_trade_events (
      id SERIAL PRIMARY KEY,
      event_date TEXT NOT NULL,
      event_year INTEGER NOT NULL,
      event_text TEXT NOT NULL,
      event_text_norm TEXT NOT NULL,
      source_url TEXT NOT NULL,
      scraped_at TEXT NOT NULL,
      UNIQUE(event_date, event_text)
    )
  |}
  let ensure_official_trade_events_index_date = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_trade_events_date ON official_trade_events(event_date)
  |}
  let ensure_official_trade_events_index_year = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_trade_events_year ON official_trade_events(event_year)
  |}
  let ensure_player_external_links_table = (unit ->. unit) {|
    CREATE TABLE IF NOT EXISTS player_external_links (
      player_id TEXT NOT NULL,
      link_type TEXT NOT NULL,
      url TEXT NOT NULL,
      source_url TEXT,
      scraped_at TEXT NOT NULL,
      PRIMARY KEY (player_id, link_type)
    )
  |}
  let ensure_player_external_links_index_player = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_player_external_links_player_id ON player_external_links(player_id)
  |}
  let ensure_game_stats_index_game = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_game_stats_game_id ON game_stats(game_id)
  |}
  let ensure_game_stats_index_player = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_game_stats_player_id ON game_stats(player_id)
  |}
  let ensure_game_stats_index_team = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_game_stats_team_code ON game_stats(team_code)
  |}
  let ensure_game_stats_index_game_team = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_game_stats_game_team ON game_stats(game_id, team_code)
  |}
  let ensure_games_index_season_type = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_games_season_type ON games(season_code, game_type)
  |}
  let ensure_games_index_season_date = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_games_season_date ON games(season_code, game_date)
  |}
  (* Drop old MATERIALIZED VIEW to allow schema change *)
  let drop_leaders_base_cache = (unit ->. unit) {|
    DROP MATERIALIZED VIEW IF EXISTS leaders_base_cache CASCADE
  |}
  let ensure_leaders_base_cache_view = (unit ->. unit) {|
    CREATE MATERIALIZED VIEW IF NOT EXISTS leaders_base_cache AS
    WITH player_team_games AS (
      -- Count games per player per team to find primary team
      SELECT s.player_id, s.team_code, COUNT(*) as game_count
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.game_type != '10'
      GROUP BY s.player_id, s.team_code
    ),
    primary_team AS (
      -- Select team with most games for each player
      SELECT DISTINCT ON (player_id) player_id, team_code
      FROM player_team_games
      ORDER BY player_id, game_count DESC
    )
    SELECT
      g.season_code,
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(*)::int AS gp,
      COALESCE(SUM(s.min_seconds), 0)::int AS min_seconds,
      COALESCE(SUM(s.pts), 0)::int AS pts,
      COALESCE(SUM(s.reb_tot), 0)::int AS reb,
      COALESCE(SUM(s.ast), 0)::int AS ast,
      COALESCE(SUM(s.stl), 0)::int AS stl,
      COALESCE(SUM(s.blk), 0)::int AS blk,
      COALESCE(SUM(s.tov), 0)::int AS tov,
      COALESCE(SUM(s.game_score), 0)::float8 AS eff,
      COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0)::int AS fg_m,
      COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0)::int AS fg_a,
      COALESCE(SUM(s.fg_3p_m), 0)::int AS fg3_m,
      COALESCE(SUM(s.fg_3p_a), 0)::int AS fg3_a,
      COALESCE(SUM(s.ft_m), 0)::int AS ft_m,
      COALESCE(SUM(s.ft_a), 0)::int AS ft_a
    FROM game_stats s
    JOIN games g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN primary_team pt ON pt.player_id = s.player_id
    JOIN teams t ON t.team_code = pt.team_code
    WHERE g.game_type != '10'
    GROUP BY g.season_code, p.player_id, p.player_name, t.team_name_kr
  |}
  let ensure_leaders_base_cache_unique = (unit ->. unit) {|
    CREATE UNIQUE INDEX IF NOT EXISTS idx_leaders_base_cache_unique
    ON leaders_base_cache(season_code, player_id, team_name_kr)
  |}
  let ensure_leaders_base_cache_index_season = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_leaders_base_cache_season
    ON leaders_base_cache(season_code)
  |}
  let refresh_leaders_base_cache = (unit ->. unit) {|
    REFRESH MATERIALIZED VIEW leaders_base_cache
  |}
  (* Migration: Drop old VIEWs before creating MATERIALIZED VIEWs *)
  let drop_score_mismatch_view = (unit ->. unit) {|
    DROP MATERIALIZED VIEW IF EXISTS score_mismatch_games CASCADE
  |}
  (* Materialized View: score_mismatch_games - cached for performance *)
  let ensure_score_mismatch_matview = (unit ->. unit) {|
    CREATE MATERIALIZED VIEW IF NOT EXISTS score_mismatch_games AS
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    ),
    joined AS (
      SELECT
        g.game_id,
        g.home_score AS home_score,
        g.away_score AS away_score,
        sh.pts_sum AS home_sum,
        sa.pts_sum AS away_sum
      FROM games g
      LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
      LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
      WHERE g.game_type != '10'
    )
    SELECT game_id
    FROM joined
    WHERE
      (home_score IS NOT NULL AND home_sum IS NOT NULL AND home_score != home_sum)
      OR
      (away_score IS NOT NULL AND away_sum IS NOT NULL AND away_score != away_sum)
  |}
  let ensure_score_mismatch_index = (unit ->. unit) {|
    CREATE UNIQUE INDEX IF NOT EXISTS idx_score_mismatch_game_id ON score_mismatch_games(game_id)
  |}
  let refresh_score_mismatch = (unit ->. unit) {|
    REFRESH MATERIALIZED VIEW CONCURRENTLY score_mismatch_games
  |}
  let drop_games_calc_view = (unit ->. unit) {|
    DROP MATERIALIZED VIEW IF EXISTS games_calc CASCADE
  |}
  (* Materialized View: games_calc - pre-computed game scores for performance *)
  let ensure_games_calc_matview = (unit ->. unit) {|
    CREATE MATERIALIZED VIEW IF NOT EXISTS games_calc AS
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    )
    SELECT
      g.*,
      sh.pts_sum AS home_sum,
      sa.pts_sum AS away_sum,
      COALESCE(g.home_score, sh.pts_sum) AS home_score_calc,
      COALESCE(g.away_score, sa.pts_sum) AS away_score_calc
    FROM games g
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
  |}
  let ensure_games_calc_index = (unit ->. unit) {|
    CREATE UNIQUE INDEX IF NOT EXISTS idx_games_calc_game_id ON games_calc(game_id)
  |}
  let ensure_games_calc_season_index = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_games_calc_season ON games_calc(season_code)
  |}
  let refresh_games_calc = (unit ->. unit) {|
    REFRESH MATERIALIZED VIEW CONCURRENTLY games_calc
  |}
  let pbp_periods_by_game = (string ->* string) {|
    SELECT period_code
    FROM (
      SELECT DISTINCT period_code,
        CASE period_code
          WHEN 'Q1' THEN 1
          WHEN 'Q2' THEN 2
          WHEN 'Q3' THEN 3
          WHEN 'Q4' THEN 4
          WHEN 'X1' THEN 5
          WHEN 'X2' THEN 6
          WHEN 'X3' THEN 7
          WHEN 'X4' THEN 8
          ELSE 99
        END AS sort_key
      FROM play_by_play_events
      WHERE game_id = ?
    ) t
    ORDER BY t.sort_key, t.period_code
  |}
  let pbp_events_by_game_period = (t2 string string ->* Types.pbp_event) {|
    SELECT
      period_code,
      event_index,
      team_side,
      description,
      team1_score,
      team2_score,
      clock
    FROM play_by_play_events
    WHERE game_id = ?
      AND period_code = ?
    ORDER BY event_index ASC
  |}
  let all_teams = (unit ->* Types.team_info) "SELECT team_code, team_name_kr FROM teams ORDER BY team_name_kr"
  let all_seasons = (unit ->* Types.season_info) "SELECT season_code, season_name FROM seasons ORDER BY season_code"

  (** Historical seasons with champion and MVP data *)
  let all_historical_seasons = (unit ->* Types.historical_season) {|
    SELECT
      season_id,
      season_name,
      champion_team,
      runner_up,
      regular_mvp,
      finals_mvp,
      rookie_of_year,
      scoring_leader,
      notes
    FROM historical_seasons
    ORDER BY season_id DESC
  |}

  (** Legend players *)
  let all_legend_players = (unit ->* Types.legend_player) {|
    SELECT
      player_name,
      career_years,
      teams,
      championships,
      mvp_count,
      all_star_count,
      career_points,
      career_rebounds,
      career_assists,
      notable_achievements,
      is_hall_of_fame
    FROM legend_players
    ORDER BY championships DESC, mvp_count DESC, career_points DESC
  |}

  (** All coaches *)
  let all_coaches = (unit ->* Types.coach) {|
    SELECT
      coach_name,
      team,
      tenure_start,
      tenure_end,
      championships,
      regular_season_wins,
      playoff_wins,
      former_player,
      player_career_years,
      notable_achievements
    FROM coaches
    ORDER BY championships DESC, tenure_start DESC
  |}

  (** Player career history by player name *)
  let player_career_by_name = (string ->* Types.player_career_entry) {|
    SELECT
      player_name,
      season_id,
      team,
      jersey_number,
      games_played,
      points_per_game,
      rebounds_per_game,
      assists_per_game,
      is_allstar,
      awards
    FROM player_career_history
    WHERE player_name = ?
    ORDER BY season_id DESC
  |}

  let player_stats_base = (t2 (t2 string string) int ->* Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds) / 60.0, 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(AVG(s.pts), 0),
      COALESCE(
        (SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN (
              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
              -
              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
            ) * s.min_seconds
            ELSE 0
          END
        ) * 1.0) / NULLIF(
          SUM(
            CASE
              WHEN g.home_score_calc IS NOT NULL
                AND g.away_score_calc IS NOT NULL
                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
              THEN s.min_seconds
              ELSE 0
            END
          ),
          0
        ),
        0
      ) as margin,
      COALESCE(AVG(s.reb_tot), 0),
      COALESCE(AVG(s.ast), 0),
      COALESCE(AVG(s.stl), 0),
      COALESCE(AVG(s.blk), 0),
      COALESCE(AVG(s.tov), 0),
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE g.game_type != '10'
      AND (? = 'ALL' OR t.team_name_kr = ?)
    GROUP BY p.player_id, p.player_name, t.team_name_kr
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let player_stats_by_season_base = (t2 string (t2 string int) ->* Types.player_base) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds), 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(AVG(s.pts), 0),
      COALESCE(
        (SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN (
              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
              -
              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
            ) * s.min_seconds
            ELSE 0
          END
        ) * 1.0) / NULLIF(
          SUM(
            CASE
              WHEN g.home_score_calc IS NOT NULL
                AND g.away_score_calc IS NOT NULL
                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
              THEN s.min_seconds
              ELSE 0
            END
          ),
          0
        ),
        0
      ) as margin,
      COALESCE(AVG(s.reb_tot), 0),
      COALESCE(AVG(s.ast), 0),
      COALESCE(AVG(s.stl), 0),
      COALESCE(AVG(s.blk), 0),
      COALESCE(AVG(s.tov), 0),
      COALESCE(AVG(s.game_score), 0),
      COALESCE(
        SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN s.min_seconds
            ELSE 0
          END
        ),
        0
      ) as margin_seconds
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id, p.player_name, t.team_name_kr
  |}
  let player_career_aggregate = (string ->? Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      COALESCE(
        (
          SELECT t2.team_name_kr
          FROM game_stats s2
          JOIN games g2 ON g2.game_id = s2.game_id
          JOIN teams t2 ON t2.team_code = s2.team_code
          WHERE s2.player_id = p.player_id AND g2.game_type != '10'
          ORDER BY g2.game_date DESC
          LIMIT 1
        ),
        ''
      ) AS team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds) / 60.0, 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(AVG(s.pts), 0),
      COALESCE(
        (SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN (
              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
              -
              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
            ) * s.min_seconds
            ELSE 0
          END
        ) * 1.0) / NULLIF(
          SUM(
            CASE
              WHEN g.home_score_calc IS NOT NULL
                AND g.away_score_calc IS NOT NULL
                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
              THEN s.min_seconds
              ELSE 0
            END
          ),
          0
        ),
        0
      ) as margin,
      COALESCE(AVG(s.reb_tot), 0),
      COALESCE(AVG(s.ast), 0),
      COALESCE(AVG(s.stl), 0),
      COALESCE(AVG(s.blk), 0),
      COALESCE(AVG(s.tov), 0),
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    WHERE s.player_id = ?
      AND g.game_type != '10'
    GROUP BY p.player_id, p.player_name
  |}

  let player_aggregate_by_id = (t2 string (t2 string (t2 string (t2 string string))) ->? Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      COALESCE(
        (
          SELECT t2.team_name_kr
          FROM game_stats s2
          JOIN games g2 ON g2.game_id = s2.game_id
          JOIN teams t2 ON t2.team_code = s2.team_code
          WHERE s2.player_id = p.player_id
            AND g2.game_type != '10'
            AND (? = 'ALL' OR g2.season_code = ?)
          ORDER BY g2.game_date DESC, g2.game_id DESC
          LIMIT 1
        ),
        ''
      ) AS team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds) / 60.0, 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(AVG(s.pts), 0),
      COALESCE(
        (SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN (
              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
              -
              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
            ) * s.min_seconds
            ELSE 0
          END
        ) * 1.0) / NULLIF(
          SUM(
            CASE
              WHEN g.home_score_calc IS NOT NULL
                AND g.away_score_calc IS NOT NULL
                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
              THEN s.min_seconds
              ELSE 0
            END
          ),
          0
        ),
        0
      ) as margin,
      COALESCE(AVG(s.reb_tot), 0),
      COALESCE(AVG(s.ast), 0),
      COALESCE(AVG(s.stl), 0),
      COALESCE(AVG(s.blk), 0),
      COALESCE(AVG(s.tov), 0),
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    WHERE s.player_id = ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
    GROUP BY p.player_id, p.player_name
  |}

  (** Player shooting stats aggregated from all games *)
  let player_shooting_stats_by_id = (t2 string (t2 string string) ->? Types.player_shooting_stats) {|
    SELECT
      p.player_id,
      TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')) AS player_name,
      COUNT(DISTINCT s.game_id) AS games,
      COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0) AS fg_made,
      COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0) AS fg_attempted,
      CASE
        WHEN SUM(s.fg_2p_a + s.fg_3p_a) > 0
        THEN SUM(s.fg_2p_m + s.fg_3p_m)::float / SUM(s.fg_2p_a + s.fg_3p_a)
        ELSE 0
      END AS fg_pct,
      COALESCE(SUM(s.fg_3p_m), 0) AS fg3_made,
      COALESCE(SUM(s.fg_3p_a), 0) AS fg3_attempted,
      CASE
        WHEN SUM(s.fg_3p_a) > 0
        THEN SUM(s.fg_3p_m)::float / SUM(s.fg_3p_a)
        ELSE 0
      END AS fg3_pct,
      COALESCE(SUM(s.ft_m), 0) AS ft_made,
      COALESCE(SUM(s.ft_a), 0) AS ft_attempted,
      CASE
        WHEN SUM(s.ft_a) > 0
        THEN SUM(s.ft_m)::float / SUM(s.ft_a)
        ELSE 0
      END AS ft_pct
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    WHERE s.player_id = ?
      AND g.game_type != '10'
      AND (? = 'ALL' OR g.season_code = ?)
    GROUP BY p.player_id, p.player_name
  |}

	  let team_totals_by_season = (t2 string (t2 string (t2 string int)) ->* Types.team_totals) {|
	    SELECT
	      CASE WHEN ? = 'ALL' THEN 'ALL' ELSE g.season_code END as season,
	      t.team_name_kr,
	      COUNT(DISTINCT s.game_id) as gp,
      COALESCE(SUM(s.min_seconds) / 60.0, 0),
      COALESCE(SUM(s.fg_2p_m), 0),
      COALESCE(SUM(s.fg_2p_a), 0),
      COALESCE(SUM(s.fg_3p_m), 0),
      COALESCE(SUM(s.fg_3p_a), 0),
      COALESCE(SUM(s.ft_m), 0),
      COALESCE(SUM(s.ft_a), 0),
      COALESCE(SUM(s.reb_off), 0),
      COALESCE(SUM(s.reb_def), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
	      COALESCE(SUM(s.pts), 0)
	    FROM game_stats s
	    JOIN games_calc g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    WHERE g.game_type != '10'
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    GROUP BY season, t.team_code, t.team_name_kr
	    ORDER BY t.team_name_kr ASC
	  |}
	  let team_margin_by_season = (t2 string int ->* Types.team_margin) {|
	    WITH params AS (
	      SELECT ? AS season, ? AS include_mismatch
	    ),
	    filtered AS (
	      SELECT
	        g.season_code,
	        g.home_team_code AS team_code,
	        g.home_score_calc AS pts_for,
	        g.away_score_calc AS pts_against,
	        g.game_id
	      FROM games_calc g, params p
	      WHERE (p.season = 'ALL' OR g.season_code = p.season)
	        AND g.game_type != '10'
	        AND g.home_score_calc IS NOT NULL
	        AND g.away_score_calc IS NOT NULL
	        AND (p.include_mismatch = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	      UNION ALL
	      SELECT
	        g.season_code,
	        g.away_team_code AS team_code,
	        g.away_score_calc AS pts_for,
	        g.home_score_calc AS pts_against,
	        g.game_id
	      FROM games_calc g, params p
	      WHERE (p.season = 'ALL' OR g.season_code = p.season)
	        AND g.game_type != '10'
	        AND g.home_score_calc IS NOT NULL
	        AND g.away_score_calc IS NOT NULL
	        AND (p.include_mismatch = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
	    )
	    SELECT
	      CASE
	        WHEN (SELECT season FROM params) = 'ALL' THEN 'ALL'
	        ELSE season_code
	      END as season,
	      t.team_name_kr,
	      COUNT(*) as gp,
	      COALESCE(SUM(pts_for), 0) as pts_for,
	      COALESCE(SUM(pts_against), 0) as pts_against
	    FROM filtered f
	    JOIN teams t ON t.team_code = f.team_code
	    GROUP BY season, t.team_code, t.team_name_kr
	    ORDER BY t.team_name_kr ASC
	  |}

  let team_standings_by_season = (string ->* Types.team_standing) {|
    WITH params AS (
      SELECT ? AS season
    ),
    results AS (
      SELECT
        g.season_code,
        g.home_team_code AS team_code,
        g.home_score_calc AS pts_for,
        g.away_score_calc AS pts_against
      FROM games_calc g, params p
      WHERE (p.season = 'ALL' OR g.season_code = p.season)
        AND g.game_type != '10'
        AND g.home_score_calc IS NOT NULL
        AND g.away_score_calc IS NOT NULL
      UNION ALL
      SELECT
        g.season_code,
        g.away_team_code AS team_code,
        g.away_score_calc AS pts_for,
        g.home_score_calc AS pts_against
      FROM games_calc g, params p
      WHERE (p.season = 'ALL' OR g.season_code = p.season)
        AND g.game_type != '10'
        AND g.home_score_calc IS NOT NULL
        AND g.away_score_calc IS NOT NULL
    )
    SELECT
      t.team_name_kr,
      COUNT(*) as gp,
      SUM(CASE WHEN pts_for > pts_against THEN 1 ELSE 0 END) as wins,
      SUM(CASE WHEN pts_for < pts_against THEN 1 ELSE 0 END) as losses,
      AVG(pts_for) as avg_pts,
      AVG(pts_against) as avg_opp_pts
    FROM results r
    JOIN teams t ON t.team_code = r.team_code
    GROUP BY t.team_code
    ORDER BY wins DESC
  |}

  let all_games_by_season = (t2 string string ->* Types.game_summary) {|
    SELECT
      g.game_id,
      COALESCE(g.game_date::text, 'Unknown'),
      t1.team_name_kr as home_team,
      t2.team_name_kr as away_team,
      g.home_score_calc,
      g.away_score_calc,
      g.game_type
    FROM games_calc g
    JOIN teams t1 ON g.home_team_code = t1.team_code
    JOIN teams t2 ON g.away_team_code = t2.team_code
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    ORDER BY g.game_date DESC, g.game_id DESC
    LIMIT 100
  |}

  let scored_games_by_season = (t2 string (t2 string int) ->* Types.game_summary) {|
    SELECT
      g.game_id,
      COALESCE(g.game_date::text, 'Unknown'),
      t1.team_name_kr as home_team,
      t2.team_name_kr as away_team,
      g.home_score_calc,
      g.away_score_calc,
      g.game_type
    FROM games_calc g
    JOIN teams t1 ON g.home_team_code = t1.team_code
    JOIN teams t2 ON g.away_team_code = t2.team_code
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
      AND g.home_score_calc IS NOT NULL
      AND g.away_score_calc IS NOT NULL
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    ORDER BY g.game_date ASC, g.game_id ASC
  |}

  (** Small helper queries for prediction context (schedule/roster). *)
  let game_season_by_id = (string ->? string) "SELECT season_code FROM games WHERE game_id = ?"

  let team_core_player_ids =
    (let t = t2 string (t2 (t2 string string) int) in
     t ->* string)
    {|
      SELECT s.player_id
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      JOIN teams t ON t.team_code = s.team_code
      WHERE t.team_name_kr = ?
        AND (? = 'ALL' OR g.season_code = ?)
        AND g.game_type != '10'
      GROUP BY s.player_id
      ORDER BY SUM(s.min_seconds) DESC
      LIMIT ?
    |}

  let team_active_player_ids =
    (t2 string string ->* string)
    {|
      SELECT s.player_id
      FROM game_stats s
      JOIN teams t ON t.team_code = s.team_code
      WHERE t.team_name_kr = ?
        AND s.game_id = ?
        AND COALESCE(s.min_seconds, 0) > 0
      ORDER BY s.min_seconds DESC
    |}

  (** DB QA (data quality) queries. *)
  let qa_games_total = (unit ->? int) {|
    SELECT COUNT(*)
    FROM games
    WHERE game_type != '10'
  |}

  let qa_games_with_stats = (unit ->? int) {|
    SELECT COUNT(DISTINCT s.game_id)
    FROM game_stats s
    JOIN games g ON g.game_id = s.game_id
    WHERE g.game_type != '10'
  |}

  let qa_plus_minus_games = (unit ->? int) {|
    SELECT COUNT(DISTINCT pm.game_id)
    FROM player_plus_minus pm
    JOIN games g ON g.game_id = pm.game_id
    WHERE g.game_type != '10'
  |}

  let qa_score_mismatch_count = (unit ->? int) {|
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    ),
    joined AS (
      SELECT
        g.game_id,
        g.home_score AS home_score,
        g.away_score AS away_score,
        sh.pts_sum AS home_sum,
        sa.pts_sum AS away_sum
      FROM games g
      LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
      LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
      WHERE g.game_type != '10'
    )
    SELECT COUNT(*)
    FROM joined
    WHERE
      (home_score IS NOT NULL AND home_sum IS NOT NULL AND home_score != home_sum)
      OR
      (away_score IS NOT NULL AND away_sum IS NOT NULL AND away_score != away_sum)
  |}

  let qa_score_mismatch_sample = (unit ->* Types.qa_score_mismatch) {|
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    )
    SELECT
      g.game_id,
      COALESCE(g.game_date::text, 'Unknown'),
      t1.team_name_kr,
      t2.team_name_kr,
      g.home_score,
      g.away_score,
      sh.pts_sum,
      sa.pts_sum
    FROM games g
    JOIN teams t1 ON g.home_team_code = t1.team_code
    JOIN teams t2 ON g.away_team_code = t2.team_code
    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
    WHERE g.game_type != '10'
      AND (
        (g.home_score IS NOT NULL AND sh.pts_sum IS NOT NULL AND g.home_score != sh.pts_sum)
        OR
        (g.away_score IS NOT NULL AND sa.pts_sum IS NOT NULL AND g.away_score != sa.pts_sum)
      )
    ORDER BY g.game_date DESC, g.game_id DESC
    LIMIT 50
  |}

  let qa_team_count_anomaly_count = (unit ->? int) {|
    SELECT COUNT(*)
    FROM (
      SELECT s.game_id
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.game_type != '10'
      GROUP BY s.game_id
      HAVING COUNT(DISTINCT s.team_code) != 2
    )
  |}

  let qa_team_count_anomaly_sample = (unit ->* Types.qa_team_count_anomaly) {|
    SELECT
      s.game_id,
      COUNT(DISTINCT s.team_code) AS team_count
    FROM game_stats s
    JOIN games g ON g.game_id = s.game_id
    WHERE g.game_type != '10'
    GROUP BY s.game_id
    HAVING COUNT(DISTINCT s.team_code) != 2
    ORDER BY team_count DESC, s.game_id DESC
    LIMIT 50
  |}

  let qa_duplicate_player_row_count = (unit ->? int) {|
    SELECT COUNT(*)
    FROM (
      SELECT s.game_id, s.team_code, s.player_id
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.game_type != '10'
      GROUP BY s.game_id, s.team_code, s.player_id
      HAVING COUNT(*) > 1
    )
  |}

	  let qa_duplicate_player_row_sample = (unit ->* Types.qa_duplicate_player_row) {|
	    SELECT
	      s.game_id,
	      s.team_code,
	      t.team_name_kr,
	      s.player_id,
	      TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')) AS player_name,
	      COUNT(*) AS row_count
	    FROM game_stats s
	    JOIN games g ON g.game_id = s.game_id
	    JOIN teams t ON t.team_code = s.team_code
	    JOIN players p ON p.player_id = s.player_id
    WHERE g.game_type != '10'
    GROUP BY
      s.game_id,
      s.team_code,
      t.team_name_kr,
      s.player_id,
      TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', ''))
    HAVING COUNT(*) > 1
    ORDER BY row_count DESC, s.game_id DESC
    LIMIT 50
  |}

	  let qa_duplicate_player_name_count = (unit ->? int) {|
	    SELECT COUNT(*)
	    FROM (
	      SELECT TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')) AS normalized_name
	      FROM players
	      GROUP BY TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', ''))
	      HAVING COUNT(DISTINCT player_id) > 1
	    )
	  |}
	
	  let qa_duplicate_player_name_sample = (unit ->* Types.qa_duplicate_player_name_row) {|
	    SELECT
	      TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', '')) AS normalized_name,
	      COUNT(DISTINCT player_id) AS id_count,
	      STRING_AGG(DISTINCT player_id, ',') AS player_ids
	    FROM players
	    GROUP BY TRIM(REPLACE(REPLACE(player_name, chr(92), ''), '"', ''))
	    HAVING COUNT(DISTINCT player_id) > 1
	    ORDER BY id_count DESC, normalized_name ASC
	    LIMIT 50
	  |}

  let game_info_by_id = (string ->? Types.game_info) {|
    WITH sums AS (
      SELECT game_id, team_code, SUM(pts) AS pts_sum
      FROM game_stats
      GROUP BY game_id, team_code
    ),
    scored_games AS (
      SELECT
        g.*,
        sh.pts_sum AS home_sum,
        sa.pts_sum AS away_sum,
        COALESCE(g.home_score, sh.pts_sum) AS home_score_calc,
        COALESCE(g.away_score, sa.pts_sum) AS away_score_calc,
        CASE
          WHEN g.home_score IS NOT NULL
            AND g.away_score IS NOT NULL
            AND sh.pts_sum IS NOT NULL
            AND sa.pts_sum IS NOT NULL
            AND g.home_score = sh.pts_sum
            AND g.away_score = sa.pts_sum
            THEN 2
          WHEN g.home_score IS NOT NULL
            AND g.away_score IS NOT NULL
            AND sh.pts_sum IS NOT NULL
            AND sa.pts_sum IS NOT NULL
            AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
            THEN 0
          ELSE 1
        END AS score_quality
      FROM games g
      LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
      LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
    )
    SELECT
      g.game_id,
      COALESCE(g.game_date::text, 'Unknown'),
      g.home_team_code,
      t1.team_name_kr,
      g.away_team_code,
      t2.team_name_kr,
      COALESCE(g.home_score_calc, 0),
      COALESCE(g.away_score_calc, 0),
      COALESCE(g.score_quality, 1)
    FROM scored_games g
    JOIN teams t1 ON g.home_team_code = t1.team_code
    JOIN teams t2 ON g.away_team_code = t2.team_code
    WHERE g.game_id = ?
  |}
  let boxscore_stats_by_game_id = (string ->* Types.boxscore_player_stat) {|
    WITH player_total_minutes AS (
      SELECT player_id, SUM(min_seconds) AS total_min_seconds
      FROM game_stats
      GROUP BY player_id
    ),
    ranked AS (
      SELECT
        p.player_id,
        TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')) AS player_name,
        p.position,
        t.team_code,
	        t.team_name_kr,
	        s.min_seconds,
	        s.pts,
	        pm.plus_minus,
	        s.reb_tot,
	        s.ast,
	        s.stl,
	        s.blk,
	        s.tov,
        s.fg_2p_m,
        s.fg_2p_a,
        s.fg_3p_m,
        s.fg_3p_a,
        s.ft_m,
        s.ft_a,
        ROW_NUMBER() OVER (
          PARTITION BY
            s.game_id,
            s.team_code,
            TRIM(REPLACE(REPLACE(p.player_name, chr(92), ''), '"', '')),
            CAST(ROUND(s.min_seconds / 6.0) AS INT),
            s.pts,
            s.reb_tot,
            s.ast,
            s.stl,
            s.blk,
            s.tov,
            s.fg_2p_m,
            s.fg_2p_a,
            s.fg_3p_m,
            s.fg_3p_a,
            s.ft_m,
            s.ft_a
	          ORDER BY
              (pm.plus_minus IS NOT NULL) DESC,
              (p.position IS NOT NULL AND TRIM(p.position) <> '') DESC,
              COALESCE(ptm.total_min_seconds, 0) DESC,
              s.min_seconds DESC,
              p.player_id DESC
	        ) AS rn
	      FROM game_stats s
	      JOIN players p ON s.player_id = p.player_id
	      JOIN teams t ON s.team_code = t.team_code
	      LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
	      LEFT JOIN player_total_minutes ptm ON ptm.player_id = p.player_id
	      WHERE s.game_id = ?
	    )
	    SELECT
      player_id,
      player_name,
      position,
      team_code,
      team_name_kr,
	      min_seconds,
	      pts,
	      plus_minus,
	      reb_tot,
	      ast,
	      stl,
	      blk,
      tov,
      fg_2p_m,
      fg_2p_a,
      fg_3p_m,
      fg_3p_a,
      ft_m,
      ft_a
    FROM ranked
    WHERE rn = 1
    ORDER BY min_seconds DESC
  |}
  let top_players = (int ->* Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds) / 60.0, 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(AVG(s.pts), 0),
      COALESCE(
        (SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN (
              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
              -
              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
            ) * s.min_seconds
            ELSE 0
          END
        ) * 1.0) / NULLIF(
          SUM(
            CASE
              WHEN g.home_score_calc IS NOT NULL
                AND g.away_score_calc IS NOT NULL
                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
              THEN s.min_seconds
              ELSE 0
            END
          ),
          0
        ),
        0
      ) as margin,
      COALESCE(AVG(s.reb_tot), 0),
      COALESCE(AVG(s.ast), 0),
      COALESCE(AVG(s.stl), 0),
      COALESCE(AVG(s.blk), 0),
      COALESCE(AVG(s.tov), 0),
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE g.game_type != '10'
    GROUP BY p.player_id, p.player_name, t.team_name_kr
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let players_by_team = (t2 string (t2 string (t2 string int)) ->* Types.player_aggregate) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds) / 60.0, 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(AVG(s.pts), 0),
      COALESCE(
        (SUM(
          CASE
            WHEN g.home_score_calc IS NOT NULL
              AND g.away_score_calc IS NOT NULL
              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
            THEN (
              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
              -
              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
            ) * s.min_seconds
            ELSE 0
          END
        ) * 1.0) / NULLIF(
          SUM(
            CASE
              WHEN g.home_score_calc IS NOT NULL
                AND g.away_score_calc IS NOT NULL
                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
              THEN s.min_seconds
              ELSE 0
            END
          ),
          0
        ),
        0
      ) as margin,
      COALESCE(AVG(s.reb_tot), 0),
      COALESCE(AVG(s.ast), 0),
      COALESCE(AVG(s.stl), 0),
      COALESCE(AVG(s.blk), 0),
      COALESCE(AVG(s.tov), 0),
      COALESCE(AVG(s.game_score), 0)
    FROM game_stats s
    JOIN games_calc g ON g.game_id = s.game_id
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    WHERE t.team_name_kr = ?
      AND (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    GROUP BY p.player_id, p.player_name, t.team_name_kr
    ORDER BY AVG(s.game_score) DESC
    LIMIT ?
  |}
  let leaders_base_stats = (t2 string string ->* Types.leader_base) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(*) as gp,
      COALESCE(SUM(s.min_seconds), 0),
      COALESCE(SUM(s.pts), 0),
      COALESCE(SUM(s.reb_tot), 0),
      COALESCE(SUM(s.ast), 0),
      COALESCE(SUM(s.stl), 0),
      COALESCE(SUM(s.blk), 0),
      COALESCE(SUM(s.tov), 0),
      COALESCE(SUM(s.game_score) * 1.0, 0),
      COALESCE(SUM(s.fg_2p_m + s.fg_3p_m), 0),
      COALESCE(SUM(s.fg_2p_a + s.fg_3p_a), 0),
      COALESCE(SUM(s.fg_3p_m), 0),
      COALESCE(SUM(s.fg_3p_a), 0),
      COALESCE(SUM(s.ft_m), 0),
      COALESCE(SUM(s.ft_a), 0)
    FROM game_stats s
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    JOIN games g ON g.game_id = s.game_id
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
    GROUP BY p.player_id, p.player_name, t.team_name_kr
  |}
  let leaders_base_cached = (string ->* Types.leader_base) {|
    SELECT
      player_id,
      player_name,
      team_name_kr,
      gp,
      min_seconds,
      pts,
      reb,
      ast,
      stl,
      blk,
      tov,
      eff,
      fg_m,
      fg_a,
      fg3_m,
      fg3_a,
      ft_m,
      ft_a
    FROM leaders_base_cache
    WHERE season_code = ?
  |}
  let leaders_base_cached_all = (unit ->* Types.leader_base) {|
    SELECT
      player_id,
      player_name,
      team_name_kr,
      COALESCE(SUM(gp), 0)::int,
      COALESCE(SUM(min_seconds), 0)::int,
      COALESCE(SUM(pts), 0)::int,
      COALESCE(SUM(reb), 0)::int,
      COALESCE(SUM(ast), 0)::int,
      COALESCE(SUM(stl), 0)::int,
      COALESCE(SUM(blk), 0)::int,
      COALESCE(SUM(tov), 0)::int,
      COALESCE(SUM(eff), 0)::float8,
      COALESCE(SUM(fg_m), 0)::int,
      COALESCE(SUM(fg_a), 0)::int,
      COALESCE(SUM(fg3_m), 0)::int,
      COALESCE(SUM(fg3_a), 0)::int,
      COALESCE(SUM(ft_m), 0)::int,
      COALESCE(SUM(ft_a), 0)::int
    FROM leaders_base_cache
    GROUP BY player_id, player_name, team_name_kr
  |}

  let leaders_pts = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.pts) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.pts) DESC LIMIT 5 |}
  let leaders_pts_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.pts) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_reb = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.reb_tot) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.reb_tot) DESC LIMIT 5 |}
  let leaders_reb_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.reb_tot) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.reb_tot) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_ast = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.ast) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.ast) DESC LIMIT 5 |}
  let leaders_ast_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ast) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.ast) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_stl = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.stl) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.stl) DESC LIMIT 5 |}
  let leaders_stl_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.stl) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.stl) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_blk = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.blk) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.blk) DESC LIMIT 5 |}
  let leaders_blk_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.blk) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.blk) / SUM(s.min_seconds)) DESC LIMIT 5 |}

  (* Leaders - extended (basketball-reference style) *)
  let leaders_gp = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, COUNT(*) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY COUNT(*) DESC LIMIT 5 |}
  let leaders_min = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.min_seconds) / 60.0 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.min_seconds) DESC LIMIT 5 |}
  let leaders_min_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.min_seconds) / 60.0 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr ORDER BY SUM(s.min_seconds) DESC LIMIT 5 |}

  let leaders_pts_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.pts) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.pts) DESC LIMIT 5 |}
  let leaders_reb_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.reb_tot) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.reb_tot) DESC LIMIT 5 |}
  let leaders_ast_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.ast) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.ast) DESC LIMIT 5 |}
  let leaders_stl_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.stl) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.stl) DESC LIMIT 5 |}
  let leaders_blk_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.blk) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.blk) DESC LIMIT 5 |}

  let leaders_tov = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.tov) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.tov) DESC LIMIT 5 |}
  let leaders_tov_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.tov) * 1.0 / SUM(s.min_seconds)) * 36 * 60 FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.tov) / SUM(s.min_seconds)) DESC LIMIT 5 |}
  let leaders_tov_totals = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, SUM(s.tov) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY SUM(s.tov) DESC LIMIT 5 |}

  let leaders_eff = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(s.game_score) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 ORDER BY AVG(s.game_score) DESC LIMIT 5 |}
  let leaders_eff_per36 = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds)) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.min_seconds) >= 6000 ORDER BY (SUM(s.game_score) / SUM(s.min_seconds)) DESC LIMIT 5 |}

  let leaders_fg_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY ((SUM(s.fg_2p_m + s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}
  let leaders_fg3_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_3p_a) >= 20 ORDER BY ((SUM(s.fg_3p_m) * 1.0) / NULLIF(SUM(s.fg_3p_a), 0)) DESC LIMIT 5 |}
  let leaders_ft_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.ft_a) >= 20 ORDER BY ((SUM(s.ft_m) * 1.0) / NULLIF(SUM(s.ft_a), 0)) DESC LIMIT 5 |}
  let leaders_ts_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, (SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING (SUM(s.fg_2p_a + s.fg_3p_a) + SUM(s.ft_a)) >= 50 ORDER BY ((SUM(s.pts) * 1.0) / NULLIF(2.0 * (SUM(s.fg_2p_a + s.fg_3p_a) + 0.44 * SUM(s.ft_a)), 0)) DESC LIMIT 5 |}
  let leaders_efg_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, ((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0) FROM game_stats s JOIN players p ON s.player_id = p.player_id JOIN teams t ON s.team_code = t.team_code JOIN games g ON g.game_id = s.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING SUM(s.fg_2p_a + s.fg_3p_a) >= 50 ORDER BY (((SUM(s.fg_2p_m + s.fg_3p_m) + 0.5 * SUM(s.fg_3p_m)) * 1.0) / NULLIF(SUM(s.fg_2p_a + s.fg_3p_a), 0)) DESC LIMIT 5 |}
  let leaders_usg_pct = (t2 string string ->* Types.leader_entry) {| SELECT p.player_id, p.player_name, t.team_name_kr, AVG(u.usg_pct) FROM player_usg_stats u JOIN players p ON u.player_id = p.player_id JOIN teams t ON u.team_code = t.team_code JOIN games g ON g.game_id = u.game_id WHERE (? = 'ALL' OR g.season_code = ?) AND g.game_type != '10' AND u.usg_pct IS NOT NULL GROUP BY p.player_id, p.player_name, t.team_name_kr HAVING COUNT(*) >= 5 AND SUM(u.min_seconds) >= 6000 ORDER BY AVG(u.usg_pct) DESC LIMIT 5 |}

  (** Stat Awards (unofficial) *)
  let stat_mvp_eff = (t2 string (t2 string int) ->* Types.leader_entry) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      AVG(s.game_score)
    FROM game_stats s
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    JOIN games g ON g.game_id = s.game_id
    WHERE (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
    GROUP BY p.player_id, p.player_name, t.team_name_kr
    HAVING COUNT(*) >= 5
    ORDER BY AVG(s.game_score) DESC
    LIMIT 5
  |}

  let stat_mip_eff_delta = (t2 string (t2 int (t2 string int)) ->* Types.leader_entry) {|
    WITH cur AS (
      SELECT
        s.player_id AS player_id,
        AVG(s.game_score) AS eff_cur,
        COUNT(*) AS gp_cur,
        MAX(t.team_name_kr) AS team_name
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      JOIN teams t ON t.team_code = s.team_code
      WHERE g.season_code = ?
        AND g.game_type != '10'
        AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
      GROUP BY s.player_id
    ),
    prev AS (
      SELECT
        s.player_id AS player_id,
        AVG(s.game_score) AS eff_prev,
        COUNT(*) AS gp_prev
      FROM game_stats s
      JOIN games g ON g.game_id = s.game_id
      WHERE g.season_code = ?
        AND g.game_type != '10'
        AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
      GROUP BY s.player_id
    )
    SELECT
      p.player_id,
      p.player_name,
      cur.team_name,
      (cur.eff_cur - prev.eff_prev) AS delta
    FROM cur
    JOIN prev ON prev.player_id = cur.player_id
    JOIN players p ON p.player_id = cur.player_id
    WHERE cur.gp_cur >= 10
      AND prev.gp_prev >= 10
    ORDER BY delta DESC
    LIMIT 5
  |}
  let player_info = (string ->? Types.player_info) "SELECT player_id, player_name, position, birth_date, height, weight FROM players WHERE player_id = ?"

  let player_draft_by_player_id = (string ->? Types.player_draft) {|
    SELECT
      player_id,
      draft_year,
      draft_round,
      pick_in_round,
      overall_pick,
      draft_team,
      raw_text,
      source_url,
      scraped_at
    FROM player_drafts
    WHERE player_id = ?
  |}

  let draft_years = (unit ->* int) {|
    SELECT DISTINCT draft_year
    FROM player_drafts
    WHERE draft_year IS NOT NULL
    ORDER BY draft_year DESC
  |}

  let draft_picks_filtered = (t2 (t2 int int) (t2 string string) ->* Types.draft_pick_row) {|
    SELECT
      d.player_id,
      p.player_name,
      d.draft_year,
      d.draft_round,
      d.pick_in_round,
      d.overall_pick,
      d.draft_team,
      d.raw_text,
      d.source_url,
      d.scraped_at
    FROM player_drafts d
    JOIN players p ON p.player_id = d.player_id
    WHERE (? = 0 OR d.draft_year = ?)
      AND (p.player_name LIKE ? OR d.raw_text LIKE ?)
    ORDER BY
      COALESCE(d.draft_year, 0) DESC,
      COALESCE(d.overall_pick, 999) ASC,
      p.player_name ASC
    LIMIT 300
  |}

  let official_trade_events_by_player_name_norm = (string ->* Types.official_trade_event) {|
    SELECT
      event_date,
      event_year,
      event_text,
      source_url,
      scraped_at
    FROM official_trade_events
    WHERE event_text_norm LIKE ?
    ORDER BY event_date DESC, id DESC
    LIMIT 30
  |}

  let official_trade_years = (unit ->* int) {|
    SELECT DISTINCT event_year
    FROM official_trade_events
    ORDER BY event_year DESC
  |}

  let official_trade_events_filtered = (t2 (t2 int int) string ->* Types.official_trade_event) {|
    SELECT
      event_date,
      event_year,
      event_text,
      source_url,
      scraped_at
    FROM official_trade_events
    WHERE (? = 0 OR event_year = ?)
      AND event_text_norm LIKE ?
    ORDER BY event_date DESC, id DESC
    LIMIT 300
  |}

  let player_external_links_by_player_id = (string ->* Types.player_external_link) {|
    SELECT
      player_id,
      link_type,
      url,
      source_url,
      scraped_at
    FROM player_external_links
    WHERE player_id = ?
    ORDER BY link_type ASC
  |}
  
  (** Queries for Per Game (Average) *)
		  let player_seasons_per_game = (string ->* Types.season_stats) {|
	    SELECT
	      g.season_code,
	      se.season_name,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      COALESCE(AVG(s.pts), 0),
	      COALESCE(AVG(s.reb_tot), 0),
	      COALESCE(AVG(s.ast), 0),
	      COALESCE(AVG(s.stl), 0),
	      COALESCE(AVG(s.blk), 0),
	      COALESCE(AVG(s.tov), 0),
		      COALESCE(AVG(s.game_score), 0),
		      COALESCE(
		        (SUM(
		          CASE
		            WHEN g.home_score_calc IS NOT NULL
		              AND g.away_score_calc IS NOT NULL
		              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
		            THEN (
		              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
		              -
		              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
		            ) * s.min_seconds
		            ELSE 0
		          END
		        ) * 1.0) / NULLIF(
		          SUM(
		            CASE
		              WHEN g.home_score_calc IS NOT NULL
		                AND g.away_score_calc IS NOT NULL
		                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
		              THEN s.min_seconds
		              ELSE 0
		            END
		          ),
		          0
		        ),
		        0
		      ) as margin,
	      0.0 as ts_pct,
	      0.0 as efg_pct
	    FROM game_stats s
		    JOIN games_calc g ON s.game_id = g.game_id
		    JOIN seasons se ON g.season_code = se.season_code
		    WHERE s.player_id = ?
		      AND g.game_type != '10'
		    GROUP BY g.season_code, se.season_name
		    ORDER BY g.season_code DESC
		  |}

  (** Queries for Totals (Sum) *)
	  let player_seasons_totals = (string ->* Types.season_stats) {|
	    SELECT
	      g.season_code,
	      se.season_name,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      COALESCE(SUM(s.pts), 0),
	      COALESCE(SUM(s.reb_tot), 0),
	      COALESCE(SUM(s.ast), 0),
	      COALESCE(SUM(s.stl), 0),
	      COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
		      COALESCE(SUM(s.game_score), 0),
		      COALESCE(
		        (SUM(
		          CASE
		            WHEN g.home_score_calc IS NOT NULL
		              AND g.away_score_calc IS NOT NULL
		              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
		            THEN (
		              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
		              -
		              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
		            ) * s.min_seconds
		            ELSE 0
		          END
		        ) * 1.0) / NULLIF(
		          SUM(
		            CASE
		              WHEN g.home_score_calc IS NOT NULL
		                AND g.away_score_calc IS NOT NULL
		                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
		              THEN s.min_seconds
		              ELSE 0
		            END
		          ),
		          0
		        ),
		        0
		      ) as margin,
	      0.0 as ts_pct,
	      0.0 as efg_pct
	    FROM game_stats s
		    JOIN games_calc g ON s.game_id = g.game_id
		    JOIN seasons se ON g.season_code = se.season_code
		    WHERE s.player_id = ?
		      AND g.game_type != '10'
		    GROUP BY g.season_code, se.season_name
		    ORDER BY g.season_code DESC
		  |}

  (** Queries for Per 36 Minutes (Normalized) *)
	  let player_seasons_per36 = (string ->* Types.season_stats) {|
	    SELECT
	      g.season_code,
	      se.season_name,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      (SUM(s.pts) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.reb_tot) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.ast) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.stl) * 2160.0 / SUM(s.min_seconds)),
	      (SUM(s.blk) * 2160.0 / SUM(s.min_seconds)),
		      (SUM(s.tov) * 2160.0 / SUM(s.min_seconds)),
		      (SUM(s.game_score) * 2160.0 / SUM(s.min_seconds)),
		      COALESCE(
		        (SUM(
		          CASE
		            WHEN g.home_score_calc IS NOT NULL
		              AND g.away_score_calc IS NOT NULL
		              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
		            THEN (
		              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
		              -
		              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
		            ) * s.min_seconds
		            ELSE 0
		          END
		        ) * 1.0) / NULLIF(
		          SUM(
		            CASE
		              WHEN g.home_score_calc IS NOT NULL
		                AND g.away_score_calc IS NOT NULL
		                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
		              THEN s.min_seconds
		              ELSE 0
		            END
		          ),
		          0
		        ),
		        0
		      ) as margin,
	      0.0 as ts_pct,
	      0.0 as efg_pct
	    FROM game_stats s
		    JOIN games_calc g ON s.game_id = g.game_id
		    JOIN seasons se ON g.season_code = se.season_code
		    WHERE s.player_id = ?
		      AND g.game_type != '10'
		    GROUP BY g.season_code, se.season_name
		    HAVING SUM(s.min_seconds) > 0
		    ORDER BY g.season_code DESC
		  |}

		  let player_recent_games = (string ->* Types.player_game_stat) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
		      GROUP BY game_id, team_code
		    )
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ? AND g.game_type != '10'
		    ORDER BY g.game_date DESC
		    LIMIT 10
		  |}
		  let player_game_logs = (t2 string (t2 string (t2 string int)) ->* Types.player_game_stat) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
		      GROUP BY game_id, team_code
		    )
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ?
		      AND (? = 'ALL' OR g.season_code = ?)
		      AND g.game_type != '10'
		      AND (? = 1 OR g.game_id NOT IN (SELECT game_id FROM score_mismatch_games))
		    ORDER BY g.game_date DESC
		  |}
		  (** Batch query for multiple players - returns player_id with each row *)
		  let batch_player_game_logs = (t2 string (t2 string string) ->* Types.player_game_stat_with_id) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
		      GROUP BY game_id, team_code
		    )
		    SELECT
		      s.player_id,
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ANY(string_to_array(?, ','))
		      AND (? = 'ALL' OR g.season_code = ?)
		      AND g.game_type != '10'
		      AND g.game_id NOT IN (SELECT game_id FROM score_mismatch_games)
		    ORDER BY s.player_id, g.game_date DESC
		  |}
		  let player_all_star_games = (string ->* Types.player_game_stat) {|
		    WITH sums AS (
		      SELECT game_id, team_code, SUM(pts) AS pts_sum
		      FROM game_stats
		      GROUP BY game_id, team_code
		    )
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.home_score, sh.pts_sum) ELSE COALESCE(g.away_score, sa.pts_sum) END as team_score,
		      CASE WHEN g.home_team_code = s.team_code THEN COALESCE(g.away_score, sa.pts_sum) ELSE COALESCE(g.home_score, sh.pts_sum) END as opponent_score,
		      CASE
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND g.home_score = sh.pts_sum
		          AND g.away_score = sa.pts_sum
		          THEN 2
		        WHEN g.home_score IS NOT NULL
		          AND g.away_score IS NOT NULL
		          AND sh.pts_sum IS NOT NULL
		          AND sa.pts_sum IS NOT NULL
		          AND (g.home_score != sh.pts_sum OR g.away_score != sa.pts_sum)
		          THEN 0
		        ELSE 1
		      END as score_quality,
		      s.min_seconds / 60.0,
		      s.pts,
		      s.reb_tot,
		      s.ast,
		      s.stl,
		      s.blk,
		      s.tov,
		      pm.plus_minus
		    FROM game_stats s
		    JOIN games g ON g.game_id = s.game_id
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    LEFT JOIN sums sh ON sh.game_id = g.game_id AND sh.team_code = g.home_team_code
		    LEFT JOIN sums sa ON sa.game_id = g.game_id AND sa.team_code = g.away_team_code
		    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
		    WHERE s.player_id = ? AND g.game_type = '10'
		    ORDER BY g.game_date DESC
		  |}
		  let player_team_games = (string ->* (t2 string string)) {| SELECT g.game_date, t.team_name_kr FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t ON t.team_code = s.team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY g.game_date ASC |}

  (** Career Highs Queries (best single game per category) *)
	  let career_high_points_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date::text, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.pts DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_rebounds_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date::text, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.reb_tot DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_assists_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date::text, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.ast DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_steals_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date::text, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.stl DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}
	  let career_high_blocks_game = (string ->? Types.player_game_stat) {| SELECT g.game_id, COALESCE(g.game_date::text, 'Unknown'), CASE WHEN g.home_team_code = s.team_code THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent, CASE WHEN g.home_team_code = s.team_code THEN 1 ELSE 0 END as is_home, NULL as team_score, NULL as opponent_score, 1 as score_quality, COALESCE(s.min_seconds, 0) / 60.0, s.pts, s.reb_tot, s.ast, s.stl, s.blk, s.tov, NULL as plus_minus FROM game_stats s JOIN games g ON g.game_id = s.game_id JOIN teams t1 ON t1.team_code = g.home_team_code JOIN teams t2 ON t2.team_code = g.away_team_code WHERE s.player_id = ? AND g.game_type != '10' ORDER BY s.blk DESC, g.game_date DESC, g.game_id DESC LIMIT 1 |}

		  let team_recent_games = (let t = t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string string)))))))) in t ->* Types.team_game_result) {|
		    SELECT
		      g.game_id,
		      g.game_date,
	      CASE WHEN t1.team_name_kr = ? THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
	      CASE WHEN t1.team_name_kr = ? THEN 1 ELSE 0 END as is_home,
	      CASE WHEN t1.team_name_kr = ? THEN g.home_score_calc ELSE g.away_score_calc END as team_score,
	      CASE WHEN t1.team_name_kr = ? THEN g.away_score_calc ELSE g.home_score_calc END as opp_score,
	      CASE
	        WHEN (t1.team_name_kr = ? AND g.home_score_calc > g.away_score_calc)
	          OR (t2.team_name_kr = ? AND g.away_score_calc > g.home_score_calc)
	        THEN 1
	        ELSE 0
	      END as is_win
	    FROM games_calc g
	    JOIN teams t1 ON t1.team_code = g.home_team_code
	    JOIN teams t2 ON t2.team_code = g.away_team_code
	    WHERE (t1.team_name_kr = ? OR t2.team_name_kr = ?)
	      AND g.game_type != '10'
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.home_score_calc IS NOT NULL
	      AND g.away_score_calc IS NOT NULL
		    ORDER BY g.game_date DESC
		    LIMIT 10
		  |}
		  let team_games = (let t = t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string (t2 string string)))))))) in t ->* Types.team_game_result) {|
		    SELECT
		      g.game_id,
		      g.game_date,
		      CASE WHEN t1.team_name_kr = ? THEN t2.team_name_kr ELSE t1.team_name_kr END as opponent,
		      CASE WHEN t1.team_name_kr = ? THEN 1 ELSE 0 END as is_home,
		      CASE WHEN t1.team_name_kr = ? THEN g.home_score_calc ELSE g.away_score_calc END as team_score,
		      CASE WHEN t1.team_name_kr = ? THEN g.away_score_calc ELSE g.home_score_calc END as opp_score,
		      CASE
		        WHEN (t1.team_name_kr = ? AND g.home_score_calc > g.away_score_calc)
		          OR (t2.team_name_kr = ? AND g.away_score_calc > g.home_score_calc)
		        THEN 1
		        ELSE 0
		      END as is_win
		    FROM games_calc g
		    JOIN teams t1 ON t1.team_code = g.home_team_code
		    JOIN teams t2 ON t2.team_code = g.away_team_code
		    WHERE (t1.team_name_kr = ? OR t2.team_name_kr = ?)
		      AND g.game_type != '10'
		      AND (? = 'ALL' OR g.season_code = ?)
		      AND g.home_score_calc IS NOT NULL
		      AND g.away_score_calc IS NOT NULL
		    ORDER BY g.game_date ASC, g.game_id ASC
		  |}
		  let player_by_name = (t2 string (t2 string string) ->? Types.player_aggregate) {|
		    SELECT
		      p.player_id,
		      p.player_name,
	      t.team_name_kr,
	      COUNT(*) as gp,
	      COALESCE(SUM(s.min_seconds) / 60.0, 0),
	      COALESCE(SUM(s.pts), 0),
	      COALESCE(SUM(s.reb_tot), 0),
	      COALESCE(SUM(s.ast), 0),
	      COALESCE(SUM(s.stl), 0),
	      COALESCE(SUM(s.blk), 0),
	      COALESCE(SUM(s.tov), 0),
	      COALESCE(AVG(s.pts), 0),
	      COALESCE(
	        (SUM(
	          CASE
	            WHEN g.home_score_calc IS NOT NULL
	              AND g.away_score_calc IS NOT NULL
	              AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
	            THEN (
	              (CASE WHEN g.home_team_code = s.team_code THEN g.home_score_calc ELSE g.away_score_calc END)
	              -
	              (CASE WHEN g.home_team_code = s.team_code THEN g.away_score_calc ELSE g.home_score_calc END)
	            ) * s.min_seconds
	            ELSE 0
	          END
	        ) * 1.0) / NULLIF(
	          SUM(
	            CASE
	              WHEN g.home_score_calc IS NOT NULL
	                AND g.away_score_calc IS NOT NULL
	                AND (s.team_code = g.home_team_code OR s.team_code = g.away_team_code)
	              THEN s.min_seconds
	              ELSE 0
	            END
	          ),
	          0
	        ),
	        0
	      ) as margin,
	      COALESCE(AVG(s.reb_tot), 0),
	      COALESCE(AVG(s.ast), 0),
	      COALESCE(AVG(s.stl), 0),
	      COALESCE(AVG(s.blk), 0),
	      COALESCE(AVG(s.tov), 0),
	      COALESCE(AVG(s.game_score), 0)
	    FROM game_stats s
	    JOIN players p ON s.player_id = p.player_id
	    JOIN teams t ON s.team_code = t.team_code
	    JOIN games_calc g ON g.game_id = s.game_id
	    WHERE p.player_name = ?
	      AND (? = 'ALL' OR g.season_code = ?)
	      AND g.game_type != '10'
	    GROUP BY p.player_id
	  |}
  let player_h2h_games = (t2 (t2 string string) (t2 string string) ->* Types.h2h_game) {|
    WITH scored_games AS (
      SELECT
        g.*,
        COALESCE(
          g.home_score,
          (SELECT SUM(sx.pts) FROM game_stats sx WHERE sx.game_id = g.game_id AND sx.team_code = g.home_team_code)
        ) AS home_score_calc,
        COALESCE(
          g.away_score,
          (SELECT SUM(sx.pts) FROM game_stats sx WHERE sx.game_id = g.game_id AND sx.team_code = g.away_team_code)
        ) AS away_score_calc
      FROM games g
    )
    SELECT
      g.game_id,
      g.game_date,
      t1.team_name_kr as p1_team,
      t2.team_name_kr as p2_team,
      s1.pts as p1_pts,
      s1.reb_tot as p1_reb,
      s1.ast as p1_ast,
      COALESCE(s1.stl, 0) as p1_stl,
      COALESCE(s1.blk, 0) as p1_blk,
      s2.pts as p2_pts,
      s2.reb_tot as p2_reb,
      s2.ast as p2_ast,
      COALESCE(s2.stl, 0) as p2_stl,
      COALESCE(s2.blk, 0) as p2_blk,
      CASE WHEN g.home_score_calc > g.away_score_calc THEN t1.team_name_kr ELSE t2.team_name_kr END as winner,
      COALESCE(ABS(g.home_score_calc - g.away_score_calc), 0) as diff
    FROM scored_games g
    JOIN game_stats s1 ON g.game_id = s1.game_id
    JOIN game_stats s2 ON g.game_id = s2.game_id
    JOIN teams t1 ON s1.team_code = t1.team_code
    JOIN teams t2 ON s2.team_code = t2.team_code
    WHERE s1.player_id = ?
      AND s2.player_id = ?
      AND (? = 'ALL' OR g.season_code = ?)
      AND g.game_type != '10'
      AND g.home_score_calc IS NOT NULL
      AND g.away_score_calc IS NOT NULL
    ORDER BY g.game_date DESC
  |}

  (* ===== Schedule Table Queries ===== *)
  let ensure_schedule_table = (unit ->. unit) {|
    CREATE TABLE IF NOT EXISTS schedule (
      schedule_id SERIAL PRIMARY KEY,
      game_date TEXT NOT NULL,
      game_time TEXT,
      season_code TEXT NOT NULL,
      home_team_code TEXT NOT NULL,
      away_team_code TEXT NOT NULL,
      venue TEXT,
      status TEXT DEFAULT 'scheduled',
      game_id INTEGER,
      UNIQUE(game_date, home_team_code, away_team_code)
    )
  |}
  let ensure_schedule_index_date = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_schedule_date ON schedule(game_date)
  |}
  let ensure_schedule_index_status = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_schedule_status ON schedule(status)
  |}
  let ensure_schedule_index_season = (unit ->. unit) {|
    CREATE INDEX IF NOT EXISTS idx_schedule_season ON schedule(season_code)
  |}

  let schedule_entry_row =
    let encode _ = Error "Encode not supported: read-only type" in
    let decode (schedule_id, (game_date, (game_time, (season_code, (home_team_code, (away_team_code, (home_team_name, (away_team_name, (venue, status))))))))) =
      Ok {
        Domain.sch_id = schedule_id;
        sch_game_date = game_date;
        sch_game_time = game_time;
        sch_season_code = season_code;
        sch_home_team_code = home_team_code;
        sch_away_team_code = away_team_code;
        sch_home_team_name = home_team_name;
        sch_away_team_name = away_team_name;
        sch_venue = venue;
        sch_status = status;
      }
    in
    let t = t2 in
    custom ~encode ~decode
      (t int
        (t string
          (t (option string)
            (t string
              (t string
                (t string
                  (t (option string)
                    (t (option string)
                      (t (option string) string)))))))))

  let get_upcoming_schedule =
    (t2 string int ->* schedule_entry_row)
    {|
    SELECT
      s.schedule_id,
      s.game_date,
      s.game_time,
      s.season_code,
      s.home_team_code,
      s.away_team_code,
      th.team_name_kr AS home_team_name,
      ta.team_name_kr AS away_team_name,
      s.venue,
      s.status
    FROM schedule s
    LEFT JOIN teams th ON s.home_team_code = th.team_code
    LEFT JOIN teams ta ON s.away_team_code = ta.team_code
    WHERE s.status = ?
      AND s.game_date >= to_char(CURRENT_DATE, 'YYYY-MM-DD')
    ORDER BY s.game_date ASC, s.game_time ASC
    LIMIT ?
  |}

  let get_schedule_by_date_range =
    let params = t2 string (t2 string (t2 string string)) in
    (params ->* schedule_entry_row)
    {|
    SELECT
      s.schedule_id,
      s.game_date,
      s.game_time,
      s.season_code,
      s.home_team_code,
      s.away_team_code,
      th.team_name_kr AS home_team_name,
      ta.team_name_kr AS away_team_name,
      s.venue,
      s.status
    FROM schedule s
    LEFT JOIN teams th ON s.home_team_code = th.team_code
    LEFT JOIN teams ta ON s.away_team_code = ta.team_code
    WHERE s.game_date >= ?
      AND s.game_date <= ?
      AND (? = 'ALL' OR s.status = ?)
    ORDER BY s.game_date ASC, s.game_time ASC
  |}

  (** MVP Race query - combines player stats with team standings *)
  let mvp_race_candidates =
    let params = t2 string string in
    (params ->* Types.mvp_candidate)
    {|
    WITH params AS (
      SELECT ? AS season
    ),
    team_records AS (
      SELECT
        CASE
          WHEN g.home_score > g.away_score THEN t_home.team_name_kr
          ELSE t_away.team_name_kr
        END AS winner,
        CASE
          WHEN g.home_score < g.away_score THEN t_home.team_name_kr
          ELSE t_away.team_name_kr
        END AS loser
      FROM games g
      JOIN teams t_home ON g.home_team_code = t_home.team_code
      JOIN teams t_away ON g.away_team_code = t_away.team_code
      CROSS JOIN params p
      WHERE g.home_score IS NOT NULL AND g.away_score IS NOT NULL
        AND (p.season = 'ALL' OR g.season_code = p.season)
    ),
    team_standings AS (
      SELECT
        team_name,
        SUM(CASE WHEN is_win THEN 1 ELSE 0 END) AS wins,
        SUM(CASE WHEN NOT is_win THEN 1 ELSE 0 END) AS losses
      FROM (
        SELECT winner AS team_name, TRUE AS is_win FROM team_records
        UNION ALL
        SELECT loser AS team_name, FALSE AS is_win FROM team_records
      ) combined
      GROUP BY team_name
    ),
    player_aggs AS (
      SELECT
        gs.player_id,
        p.player_name,
        t.team_name_kr AS team_name,
        COUNT(DISTINCT gs.game_id) AS gp,
        ROUND(CAST(SUM(gs.pts) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS ppg,
        ROUND(CAST(SUM(gs.reb_tot) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS rpg,
        ROUND(CAST(SUM(gs.ast) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS apg,
        ROUND(CAST(SUM(gs.stl) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS spg,
        ROUND(CAST(SUM(gs.blk) AS NUMERIC) / NULLIF(COUNT(DISTINCT gs.game_id), 0), 1) AS bpg,
        ROUND(
          CAST(SUM(gs.pts + gs.reb_tot + gs.ast + gs.stl + gs.blk - (gs.fg_2p_a + gs.fg_3p_a - gs.fg_2p_m - gs.fg_3p_m) - (gs.ft_a - gs.ft_m) - gs.tov) AS NUMERIC)
          / NULLIF(COUNT(DISTINCT gs.game_id), 0),
          1
        ) AS efficiency
      FROM game_stats gs
      JOIN players p ON gs.player_id = p.player_id
      JOIN teams t ON gs.team_code = t.team_code
      JOIN games g ON gs.game_id = g.game_id
      CROSS JOIN params pp
      WHERE (pp.season = 'ALL' OR g.season_code = pp.season)
        AND gs.min_seconds > 0
      GROUP BY gs.player_id, p.player_name, t.team_name_kr
      HAVING COUNT(DISTINCT gs.game_id) >= 5
    )
    SELECT
      pa.player_id,
      pa.player_name,
      pa.team_name,
      pa.gp,
      pa.ppg,
      pa.rpg,
      pa.apg,
      pa.spg,
      pa.bpg,
      pa.efficiency,
      COALESCE(ts.wins, 0) AS team_wins,
      COALESCE(ts.losses, 0) AS team_losses,
      ROUND(
        CASE WHEN (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0)) > 0
          THEN CAST(COALESCE(ts.wins, 0) AS NUMERIC) / (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0))
          ELSE 0
        END,
        3
      ) AS team_win_pct
    FROM player_aggs pa
    LEFT JOIN team_standings ts ON pa.team_name = ts.team_name
    WHERE pa.gp >= ?
    ORDER BY
      (pa.ppg * 2 + pa.rpg * 1.2 + pa.apg * 1.5 + pa.spg * 2 + pa.bpg * 2 + pa.efficiency * 0.5
       + CASE WHEN (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0)) > 0
           THEN (CAST(COALESCE(ts.wins, 0) AS NUMERIC) / (COALESCE(ts.wins, 0) + COALESCE(ts.losses, 0))) * 20
           ELSE 0
         END) DESC
    LIMIT 20
  |}

  (** Get all Q4 PBP events for clutch time analysis
      Clutch time = Q4 + clock <= 5:00 + score diff <= 5 *)
  let clutch_pbp_events = (string ->* Types.pbp_event) {|
    SELECT
      period_code, event_index, team_side, description,
      team1_score, team2_score, clock
    FROM play_by_play_events p
    JOIN games g ON p.game_id = g.game_id
    WHERE period_code = 'Q4'
      AND (
        CASE
          WHEN clock ~ '^\d+:\d+$' THEN
            CAST(SPLIT_PART(clock, ':', 1) AS INTEGER) * 60 +
            CAST(SPLIT_PART(clock, ':', 2) AS INTEGER)
          ELSE 600
        END
      ) <= 300
      AND ABS(COALESCE(team1_score, 0) - COALESCE(team2_score, 0)) <= 5
      AND ($1 = 'ALL' OR g.season_code = $1)
    ORDER BY p.game_id, event_index
  |}

  (** Aggregated clutch time statistics per player
      Parses description for scoring events:
      - "N점슛성공" = field goal made (N=2 or 3)
      - "N점슛실패" = field goal attempted but missed
      - "자유투성공" = free throw made
      - "자유투실패" = free throw missed *)
  let clutch_stats_by_season = (string ->* Types.clutch_stats) {|
    WITH clutch_events AS (
      SELECT
        p.player_id,
        p.game_id,
        p.description,
        CASE
          WHEN p.description ~ '2점슛성공' THEN 2
          WHEN p.description ~ '3점슛성공' THEN 3
          WHEN p.description ~ '자유투성공' THEN 1
          ELSE 0
        END AS points_scored,
        CASE WHEN p.description ~ '[23]점슛성공' THEN 1 ELSE 0 END AS fg_made,
        CASE WHEN p.description ~ '[23]점슛' THEN 1 ELSE 0 END AS fg_att,
        CASE WHEN p.description ~ '3점슛성공' THEN 1 ELSE 0 END AS three_made,
        CASE WHEN p.description ~ '자유투성공' THEN 1 ELSE 0 END AS ft_made,
        CASE WHEN p.description ~ '자유투' THEN 1 ELSE 0 END AS ft_att
      FROM play_by_play_events p
      JOIN games g ON p.game_id = g.game_id
      WHERE p.period_code = 'Q4'
        AND p.player_id IS NOT NULL
        AND (
          CASE
            WHEN p.clock ~ '^\d+:\d+$' THEN
              CAST(SPLIT_PART(p.clock, ':', 1) AS INTEGER) * 60 +
              CAST(SPLIT_PART(p.clock, ':', 2) AS INTEGER)
            ELSE 600
          END
        ) <= 300
        AND ABS(COALESCE(p.team1_score, 0) - COALESCE(p.team2_score, 0)) <= 5
        AND ($1 = 'ALL' OR g.season_code = $1)
        AND g.game_type != '10'
    )
    SELECT
      ce.player_id,
      pl.player_name,
      COALESCE(t.team_name_kr, 'Unknown') AS team_name,
      COUNT(DISTINCT ce.game_id) AS clutch_games,
      SUM(ce.points_scored) AS clutch_points,
      SUM(ce.fg_made) AS clutch_fg_made,
      SUM(ce.fg_att) AS clutch_fg_att,
      CASE
        WHEN SUM(ce.fg_att) > 0
        THEN CAST(SUM(ce.fg_made) AS FLOAT) / SUM(ce.fg_att)
        ELSE 0.0
      END AS clutch_fg_pct,
      SUM(ce.ft_made) AS clutch_ft_made,
      SUM(ce.ft_att) AS clutch_ft_att,
      SUM(ce.three_made) AS clutch_3p_made
    FROM clutch_events ce
    JOIN players pl ON ce.player_id = pl.player_id
    LEFT JOIN (
      SELECT DISTINCT ON (gs.player_id)
        gs.player_id, gs.team_code
      FROM game_stats gs
      JOIN games g2 ON gs.game_id = g2.game_id
      WHERE ($1 = 'ALL' OR g2.season_code = $1)
      ORDER BY gs.player_id, g2.game_date DESC
    ) latest_team ON ce.player_id = latest_team.player_id
    LEFT JOIN teams t ON latest_team.team_code = t.team_code
    GROUP BY ce.player_id, pl.player_name, t.team_name_kr
    HAVING SUM(ce.points_scored) > 0 OR SUM(ce.fg_att) > 0 OR SUM(ce.ft_att) > 0
    ORDER BY SUM(ce.points_scored) DESC, clutch_fg_pct DESC
    LIMIT 50
  |}

  (* On/Off Impact Queries *)
  let on_off_impact_stats = (t2 string string ->* (t2 string (t2 string (t2 string (t2 int (t2 int (t2 int int))))))) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(DISTINCT s.game_id) as games_played,
      COALESCE(SUM(s.min_seconds), 0) as total_min_seconds,
      COALESCE(SUM(pm.plus_minus), 0) as total_plus_minus,
      COUNT(pm.plus_minus) as games_with_pm
    FROM game_stats s
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    JOIN games g ON g.game_id = s.game_id
    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
    WHERE ($1 = 'ALL' OR g.season_code = $1)
      AND g.game_type != '10'
      AND s.min_seconds > 0
    GROUP BY p.player_id, p.player_name, t.team_name_kr
    HAVING COUNT(DISTINCT s.game_id) >= 5
      AND COALESCE(SUM(s.min_seconds), 0) / 60.0 >= 50
    ORDER BY
      CASE WHEN COUNT(pm.plus_minus) > 0
        THEN CAST(COALESCE(SUM(pm.plus_minus), 0) AS FLOAT) / COUNT(pm.plus_minus)
        ELSE -999
      END DESC
    LIMIT CASE WHEN $2 = '' THEN 50 ELSE CAST($2 AS INTEGER) END
  |}

  let on_off_impact_for_player = (t2 string string ->? (t2 string (t2 string (t2 string (t2 int (t2 int (t2 int int))))))) {|
    SELECT
      p.player_id,
      p.player_name,
      t.team_name_kr,
      COUNT(DISTINCT s.game_id) as games_played,
      COALESCE(SUM(s.min_seconds), 0) as total_min_seconds,
      COALESCE(SUM(pm.plus_minus), 0) as total_plus_minus,
      COUNT(pm.plus_minus) as games_with_pm
    FROM game_stats s
    JOIN players p ON s.player_id = p.player_id
    JOIN teams t ON s.team_code = t.team_code
    JOIN games g ON g.game_id = s.game_id
    LEFT JOIN player_plus_minus pm ON pm.game_id = s.game_id AND pm.player_id = s.player_id
    WHERE s.player_id = $1
      AND ($2 = 'ALL' OR g.season_code = $2)
      AND g.game_type != '10'
      AND s.min_seconds > 0
    GROUP BY p.player_id, p.player_name, t.team_name_kr
  |}
end

(** Database operations *)
  module Repo = struct
    let ensure_schema (module Db : Caqti_eio.CONNECTION) =
      let (let*) = Result.bind in
      let* () = Db.exec Queries.ensure_player_plus_minus_table () in
      let* () = Db.exec Queries.ensure_player_plus_minus_index () in
      let* () = Db.exec Queries.ensure_play_by_play_events_table () in
      let* () = Db.exec Queries.ensure_play_by_play_events_player_id_column () in
      let* () = Db.exec Queries.ensure_play_by_play_events_index_game () in
      let* () = Db.exec Queries.ensure_play_by_play_events_index_game_period () in
      let* () = Db.exec Queries.ensure_play_by_play_events_index_player () in
      let* () = Db.exec Queries.ensure_player_drafts_table () in
      let* () = Db.exec Queries.ensure_player_drafts_index_year () in
	      let* () = Db.exec Queries.ensure_official_trade_events_table () in
	      let* () = Db.exec Queries.ensure_official_trade_events_index_date () in
      let* () = Db.exec Queries.ensure_official_trade_events_index_year () in
      let* () = Db.exec Queries.ensure_player_external_links_table () in
      let* () = Db.exec Queries.ensure_player_external_links_index_player () in
      (* Performance indexes for game_stats - critical for fast queries *)
      let* () = Db.exec Queries.ensure_game_stats_index_game () in
      let* () = Db.exec Queries.ensure_game_stats_index_player () in
      let* () = Db.exec Queries.ensure_game_stats_index_team () in
      let* () = Db.exec Queries.ensure_game_stats_index_game_team () in
      let* () = Db.exec Queries.ensure_games_index_season_type () in
      let* () = Db.exec Queries.ensure_games_index_season_date () in
      (* Schedule table for upcoming games *)
      let* () = Db.exec Queries.ensure_schedule_table () in
      let* () = Db.exec Queries.ensure_schedule_index_date () in
      let* () = Db.exec Queries.ensure_schedule_index_status () in
      let* () = Db.exec Queries.ensure_schedule_index_season () in
      (* Leaders base cache MATERIALIZED VIEW - drop first to update schema *)
      let* () = Db.exec Queries.drop_leaders_base_cache () in
      let* () = Db.exec Queries.ensure_leaders_base_cache_view () in
      let* () = Db.exec Queries.ensure_leaders_base_cache_unique () in
      let* () = Db.exec Queries.ensure_leaders_base_cache_index_season () in
      let* () = Db.exec Queries.refresh_leaders_base_cache () in
      (* Migration: Drop old regular VIEWs before creating MATERIALIZED VIEWs *)
      let* () = Db.exec Queries.drop_score_mismatch_view () in
      let* () = Db.exec Queries.drop_games_calc_view () in
      (* Materialized Views for performance - pre-computed and cached *)
      let* () = Db.exec Queries.ensure_score_mismatch_matview () in
      let* () = Db.exec Queries.ensure_score_mismatch_index () in
      let* () = Db.exec Queries.ensure_games_calc_matview () in
      let* () = Db.exec Queries.ensure_games_calc_index () in
      Db.exec Queries.ensure_games_calc_season_index ()

    (* Refresh materialized views - call after data sync *)
    let refresh_matviews (module Db : Caqti_eio.CONNECTION) =
      let (let*) = Result.bind in
      let* () = Db.exec Queries.refresh_leaders_base_cache () in
      let* () = Db.exec Queries.refresh_games_calc () in
      Db.exec Queries.refresh_score_mismatch ()
  let get_teams (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_teams ()
  let get_seasons (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_seasons ()
  let get_historical_seasons (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_historical_seasons ()
  let get_legend_players (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_legend_players ()
  let get_coaches (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_coaches ()
  let get_player_career ~player_name (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.player_career_by_name player_name
  let get_player_by_name ~name ~season (module Db : Caqti_eio.CONNECTION) = let s = if season = "" then "ALL" else season in Db.find_opt Queries.player_by_name (name, (s, s))
  let get_player_aggregate_by_id ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt Queries.player_aggregate_by_id (s, (s, (player_id, (s, s))))

  let get_player_shooting_stats ~player_id ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.find_opt Queries.player_shooting_stats_by_id (player_id, (s, s))

  let get_draft_years (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.draft_years ()

  let get_draft_picks ~year ~search (module Db : Caqti_eio.CONNECTION) =
    let pattern = normalize_search_pattern (normalize_label search) in
    Db.collect_list Queries.draft_picks_filtered ((year, year), (pattern, pattern))

  let get_official_trade_years (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.official_trade_years ()

  let get_official_trade_events ~year ~search (module Db : Caqti_eio.CONNECTION) =
    let pattern = normalize_search_pattern (normalize_label search) in
    Db.collect_list Queries.official_trade_events_filtered ((year, year), pattern)

  (* Schedule queries *)
  let get_upcoming_schedule ~status ~limit (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.get_upcoming_schedule (status, limit)

  let get_schedule_by_date_range ~start_date ~end_date ~status (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.get_schedule_by_date_range (start_date, (end_date, (status, status)))

  let get_players_base ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.player_stats_by_season_base (s, (s, include_int))
  let get_top_players ?(team_name="ALL") ~limit (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.player_stats_base ((team_name, team_name), limit)
  let get_players_by_team ~team_name ~season ~limit (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.players_by_team (team_name, (s, (s, limit)))
  let get_team_totals ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.team_totals_by_season (season, (season, (season, include_int)))

  let get_team_margins ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.team_margin_by_season (season, include_int)
  let get_standings ~season (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.team_standings_by_season season
  let get_games ~season (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.all_games_by_season (season, season)
  let get_scored_games ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.scored_games_by_season (season, (season, include_int))
  let get_game_season_code ~game_id (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.game_season_by_id game_id
  let get_pbp_periods ~game_id (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.pbp_periods_by_game game_id
  let get_pbp_events ~game_id ~period_code (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.pbp_events_by_game_period (game_id, period_code)
  let get_team_core_player_ids ~season ~team_name ~limit (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.team_core_player_ids (team_name, ((season, season), limit))
  let get_team_active_player_ids ~team_name ~game_id (module Db : Caqti_eio.CONNECTION) =
    Db.collect_list Queries.team_active_player_ids (team_name, game_id)

  let qa_games_total (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_games_total ()
  let qa_games_with_stats (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_games_with_stats ()
  let qa_plus_minus_games (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_plus_minus_games ()
  let qa_score_mismatch_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_score_mismatch_count ()
  let qa_score_mismatch_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_score_mismatch_sample ()
  let qa_team_count_anomaly_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_team_count_anomaly_count ()
  let qa_team_count_anomaly_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_team_count_anomaly_sample ()
  let qa_duplicate_player_row_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_row_count ()
  let qa_duplicate_player_row_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_row_sample ()
  let qa_duplicate_player_name_count (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.qa_duplicate_player_name_count ()
  let qa_duplicate_player_name_sample (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.qa_duplicate_player_name_sample ()
  let get_game_info ~game_id (module Db : Caqti_eio.CONNECTION) = Db.find_opt Queries.game_info_by_id game_id
  let get_boxscore_stats ~game_id (module Db : Caqti_eio.CONNECTION) = Db.collect_list Queries.boxscore_stats_by_game_id game_id
  let get_leaders ~category ~scope ~season (module Db : Caqti_eio.CONNECTION) =
    let q =
      match (category, scope) with
      | "gp", _ -> Queries.leaders_gp
      | "min", "totals" -> Queries.leaders_min_totals
      | "min", _ -> Queries.leaders_min
      | "pts", "totals" -> Queries.leaders_pts_totals
      | "pts", "per_36" -> Queries.leaders_pts_per36
      | "pts", _ -> Queries.leaders_pts
      | "reb", "totals" -> Queries.leaders_reb_totals
      | "reb", "per_36" -> Queries.leaders_reb_per36
      | "reb", _ -> Queries.leaders_reb
      | "ast", "totals" -> Queries.leaders_ast_totals
      | "ast", "per_36" -> Queries.leaders_ast_per36
      | "ast", _ -> Queries.leaders_ast
      | "stl", "totals" -> Queries.leaders_stl_totals
      | "stl", "per_36" -> Queries.leaders_stl_per36
      | "stl", _ -> Queries.leaders_stl
      | "blk", "totals" -> Queries.leaders_blk_totals
      | "blk", "per_36" -> Queries.leaders_blk_per36
      | "blk", _ -> Queries.leaders_blk
      | "tov", "totals" -> Queries.leaders_tov_totals
      | "tov", "per_36" -> Queries.leaders_tov_per36
      | "tov", _ -> Queries.leaders_tov
      | "eff", "per_36" -> Queries.leaders_eff_per36
      | "eff", _ -> Queries.leaders_eff
      | "fg_pct", _ -> Queries.leaders_fg_pct
      | "fg3_pct", _ -> Queries.leaders_fg3_pct
      | "ft_pct", _ -> Queries.leaders_ft_pct
      | "ts_pct", _ -> Queries.leaders_ts_pct
      | "efg_pct", _ -> Queries.leaders_efg_pct
      | "usg_pct", _ -> Queries.leaders_usg_pct
      | _ -> Queries.leaders_pts
    in
    Db.collect_list q (season, season)

  let get_leaders_base ~season (module Db : Caqti_eio.CONNECTION) =
    let s = if String.trim season = "" then "ALL" else season in
    if s = "ALL" then
      Db.collect_list Queries.leaders_base_cached_all ()
    else
      Db.collect_list Queries.leaders_base_cached s

  let get_stat_mvp_eff ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.stat_mvp_eff (s, (s, include_int))

  let get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    Db.collect_list Queries.stat_mip_eff_delta (season, (include_int, (prev_season, include_int)))
  
	  let get_player_profile ~player_id (module Db : Caqti_eio.CONNECTION) =
	    let (let*) = Result.bind in
	    let* info = Db.find_opt Queries.player_info player_id in
	    match info with
	    | None -> Ok None
		    | Some p -> 
		        let team_stints_of_games (games: (string * string) list) =
		          let rec loop current acc = function
		            | [] ->
		                (match current with
		                | None -> List.rev acc
		                | Some c -> List.rev (c :: acc))
		            | (game_date, team_name) :: rest ->
		                (match current with
		                | None ->
		                    loop
		                      (Some
		                        {
		                          pts_team_name = team_name;
		                          pts_start_date = game_date;
		                          pts_end_date = game_date;
		                          pts_games_played = 1;
		                        })
		                      acc
		                      rest
		                | Some stint when stint.pts_team_name = team_name ->
		                    loop
		                      (Some
		                        {
		                          stint with
		                          pts_end_date = game_date;
		                          pts_games_played = stint.pts_games_played + 1;
		                        })
		                      acc
		                      rest
		                | Some stint ->
		                    loop
		                      (Some
		                        {
		                          pts_team_name = team_name;
		                          pts_start_date = game_date;
		                          pts_end_date = game_date;
		                          pts_games_played = 1;
		                        })
		                      (stint :: acc)
		                      rest)
		          in
		          loop None [] games
		        in
			        let* seasons = Db.collect_list Queries.player_seasons_per_game player_id in 
			        let* recent = Db.collect_list Queries.player_recent_games player_id in 
			        let* all_star = Db.collect_list Queries.player_all_star_games player_id in
			        let* team_games = Db.collect_list Queries.player_team_games player_id in
			        let* averages_res = Db.find_opt Queries.player_career_aggregate player_id in
              let* draft_res = Db.find_opt Queries.player_draft_by_player_id player_id in
              let name_norm = normalize_label p.name in
              let trade_pattern = "%" ^ name_norm ^ "%" in
	              let* trade_events_res =
	                if String.trim name_norm = "" then
	                  Ok ([])
	                else
	                  Db.collect_list Queries.official_trade_events_by_player_name_norm trade_pattern
	              in
	              let* external_links_res =
	                Db.collect_list Queries.player_external_links_by_player_id player_id
	              in
				        let* ch_points = Db.find_opt Queries.career_high_points_game player_id in
				        let* ch_rebounds = Db.find_opt Queries.career_high_rebounds_game player_id in
				        let* ch_assists = Db.find_opt Queries.career_high_assists_game player_id in
				        let* ch_steals = Db.find_opt Queries.career_high_steals_game player_id in
				        let* ch_blocks = Db.find_opt Queries.career_high_blocks_game player_id in
				        let dummy_avg = { player_id = p.id; name = p.name; team_name = ""; games_played = 0; total_minutes = 0.0; total_points = 0; total_rebounds = 0; total_assists = 0; total_steals = 0; total_blocks = 0; total_turnovers = 0; avg_points = 0.0; avg_margin = 0.0; avg_rebounds = 0.0; avg_assists = 0.0; avg_steals = 0.0; avg_blocks = 0.0; avg_turnovers = 0.0; efficiency = 0.0; } in 
(* All values already unwrapped by let* - use them directly *)
			            let add_opt opt mk acc = match opt with | None -> acc | Some v -> mk v :: acc in
			            let items =
			              []
			              |> add_opt ch_points (fun (g: player_game_stat) -> { chi_label = "Points"; chi_value = g.pts; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_rebounds (fun g -> { chi_label = "Rebounds"; chi_value = g.reb; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_assists (fun g -> { chi_label = "Assists"; chi_value = g.ast; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_steals (fun g -> { chi_label = "Steals"; chi_value = g.stl; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> add_opt ch_blocks (fun g -> { chi_label = "Blocks"; chi_value = g.blk; chi_game_id = g.game_id; chi_game_date = g.game_date; chi_opponent = g.opponent; chi_is_home = g.is_home })
              |> List.rev
			            in
			            let career_highs = match items with | [] -> None | _ -> Some items in
			            let averages = match averages_res with Some a -> a | None -> dummy_avg in
			            let team_stints = team_stints_of_games team_games in
			            Ok ((Some { player = p; averages; recent_games = recent; all_star_games = all_star; draft = draft_res; official_trade_events = trade_events_res; external_links = external_links_res; team_stints; season_breakdown = seasons; career_highs }))

  let get_player_season_stats ~player_id ~scope (module Db : Caqti_eio.CONNECTION) = let q = match scope with | "totals" -> Queries.player_seasons_totals | "per_36" -> Queries.player_seasons_per36 | _ -> Queries.player_seasons_per_game in Db.collect_list q player_id
  let get_player_game_logs ~player_id ~season ~include_mismatch (module Db : Caqti_eio.CONNECTION) =
    let include_int = if include_mismatch then 1 else 0 in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.player_game_logs (player_id, (s, (s, include_int)))
  let get_batch_player_game_logs ~player_ids ~season (module Db : Caqti_eio.CONNECTION) =
    let ids_csv = String.concat "," player_ids in
    let s = if String.trim season = "" then "ALL" else season in
    Db.collect_list Queries.batch_player_game_logs (ids_csv, (s, s))
	  let get_team_recent_games ~team_name ~season (module Db : Caqti_eio.CONNECTION) =
	    let s = if String.trim season = "" then "ALL" else season in
	    let t =
	      (team_name,
	       (team_name,
	        (team_name,
	         (team_name,
	          (team_name,
	           (team_name,
	            (team_name,
	             (team_name,
	              (s, s)))))))))
	    in
	    Db.collect_list Queries.team_recent_games t
	  let get_team_games ~team_name ~season (module Db : Caqti_eio.CONNECTION) =
	    let s = if String.trim season = "" then "ALL" else season in
	    let t =
	      (team_name,
	       (team_name,
	        (team_name,
	         (team_name,
	          (team_name,
	           (team_name,
	            (team_name,
	             (team_name,
	              (s, s)))))))))
	    in
	    Db.collect_list Queries.team_games t
	  let get_player_h2h ~p1_id ~p2_id ~season (module Db : Caqti_eio.CONNECTION) = let s = if season = "" then "ALL" else season in Db.collect_list Queries.player_h2h_games ((p1_id, p2_id), (s, s))

    (** Get MVP race candidates for a season *)
    let get_mvp_race_candidates ~season ~min_games (module Db : Caqti_eio.CONNECTION) =
      let s = if String.trim season = "" then "ALL" else season in
      Db.collect_list Queries.mvp_race_candidates (s, min_games)
	end

(** Connection pool - Eio-based *)
let pool_ref : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t option ref = ref None

(** Initialize pool with Eio context - must be called inside Eio_main.run *)
let init_pool ~sw ~stdenv db_url =
  let uri = Uri.of_string db_url in
  match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
  | Ok pool -> pool_ref := Some pool; Ok ()
  | Error e -> Error (ConnectionFailed (Caqti_error.show e))

(** Run a database operation using the pool *)
let with_db f =
  match !pool_ref with
  | None -> Error (ConnectionFailed "Pool not initialized")
  | Some pool ->
      match Caqti_eio.Pool.use f pool with
      | Ok v -> Ok v
      | Error e -> Error (QueryFailed (Caqti_error.show e))

let iso8601_utc () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let split_csv_ids (ids : string) =
  ids
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")

module Cache = struct
  type 'a entry = {
    value: 'a;
    expires_at: float;
  }

  type 'a t = {
    ttl: float;
    max_entries: int;
    store: (string, 'a entry) Hashtbl.t;
  }

  let create ~ttl ~max_entries =
    { ttl; max_entries; store = Hashtbl.create max_entries }

  let now () = Unix.gettimeofday ()

  let get t key =
    match Hashtbl.find_opt t.store key with
    | Some entry when entry.expires_at > now () -> Some entry.value
    | Some _ ->
        Hashtbl.remove t.store key;
        None
    | None -> None

  (** Evict expired entries first, then oldest 25% if still over capacity *)
  let evict t =
    let current = now () in
    (* Phase 1: Remove expired entries *)
    let expired_keys = Hashtbl.fold (fun k entry acc ->
      if entry.expires_at <= current then k :: acc else acc
    ) t.store [] in
    List.iter (Hashtbl.remove t.store) expired_keys;
    (* Phase 2: If still over capacity, remove oldest 25% *)
    if Hashtbl.length t.store > t.max_entries then begin
      let entries = Hashtbl.fold (fun k e acc -> (k, e.expires_at) :: acc) t.store [] in
      let sorted = List.sort (fun (_, t1) (_, t2) -> Float.compare t1 t2) entries in
      let to_remove = max 1 (List.length sorted / 4) in
      List.iteri (fun i (k, _) ->
        if i < to_remove then Hashtbl.remove t.store k
      ) sorted
    end

  let set t key value =
    Hashtbl.replace t.store key { value; expires_at = now () +. t.ttl };
    if Hashtbl.length t.store > t.max_entries then evict t
end

let cached cache key f =
  match Cache.get cache key with
  | Some value -> Ok value
  | None ->
      let result = f () in
      (match result with
       | Ok value -> Cache.set cache key value
       | Error _ -> ());
      result

(** Public API *)
let cache_key_text value =
  let trimmed = String.trim value in
  let lowered = String.lowercase_ascii trimmed in
  if String.length lowered > 64 then String.sub lowered 0 64 else lowered

let player_sort_key = function
  | ByPoints -> "pts"
  | ByMargin -> "mg"
  | ByRebounds -> "reb"
  | ByAssists -> "ast"
  | ByEfficiency -> "eff"
  | ByMinutes -> "min"

let team_sort_key = function
  | TeamByPoints -> "pts"
  | TeamByRebounds -> "reb"
  | TeamByAssists -> "ast"
  | TeamBySteals -> "stl"
  | TeamByBlocks -> "blk"
  | TeamByEfficiency -> "eff"
  | TeamByTsPct -> "ts"
  | TeamByFg3Pct -> "fg3"
  | TeamByMinutes -> "min"

let seasons_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:4
let teams_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:4
let standings_cache = Cache.create ~ttl:120.0 ~max_entries:16
let games_cache = Cache.create ~ttl:120.0 ~max_entries:16
let scored_games_cache = Cache.create ~ttl:120.0 ~max_entries:16
let team_stats_cache = Cache.create ~ttl:120.0 ~max_entries:48
let players_cache = Cache.create ~ttl:60.0 ~max_entries:128
let players_base_cache = Cache.create ~ttl:120.0 ~max_entries:32
let players_by_team_cache = Cache.create ~ttl:120.0 ~max_entries:128
let team_detail_cache = Cache.create ~ttl:120.0 ~max_entries:64
let player_profile_cache = Cache.create ~ttl:120.0 ~max_entries:256
let player_season_stats_cache = Cache.create ~ttl:120.0 ~max_entries:256
let player_game_logs_cache = Cache.create ~ttl:120.0 ~max_entries:256
let boxscore_cache = Cache.create ~ttl:120.0 ~max_entries:128
let leaders_base_cache = Cache.create ~ttl:120.0 ~max_entries:16
let leaders_cache = Cache.create ~ttl:120.0 ~max_entries:128
let awards_cache = Cache.create ~ttl:300.0 ~max_entries:32
let draft_years_cache = Cache.create ~ttl:300.0 ~max_entries:8
let draft_picks_cache = Cache.create ~ttl:300.0 ~max_entries:32
let trade_years_cache = Cache.create ~ttl:300.0 ~max_entries:8
let trade_events_cache = Cache.create ~ttl:300.0 ~max_entries:32
let qa_report_cache = Cache.create ~ttl:300.0 ~max_entries:4
let history_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:16  (* 6h TTL for history data *)
let legends_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:16
let coaches_cache = Cache.create ~ttl:(60.0 *. 60.0 *. 6.0) ~max_entries:16
let player_career_cache = Cache.create ~ttl:300.0 ~max_entries:128
let schedule_cache = Cache.create ~ttl:60.0 ~max_entries:16  (* 1 min TTL - schedule changes frequently *)

let take n items =
  let rec loop acc count = function
    | _ when count <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> loop (x :: acc) (count - 1) xs
  in
  loop [] n items

let normalize_search_text value =
  value |> normalize_label |> String.lowercase_ascii |> String.trim

let string_contains ~needle ~hay =
  let needle_len = String.length needle in
  if needle_len = 0 then
    true
  else
    let hay_len = String.length hay in
    let rec loop i =
      if i + needle_len > hay_len then
        false
      else if String.sub hay i needle_len = needle then
        true
      else
        loop (i + 1)
    in
    loop 0

let player_base_matches search (b: player_base) =
  let trimmed = String.trim search in
  if trimmed = "" then
    true
  else
    let needle = normalize_search_text trimmed in
    let hay = normalize_search_text b.pb_player_name in
    string_contains ~needle ~hay

let player_base_to_aggregate (b: player_base) =
  { player_id = b.pb_player_id;
    name = b.pb_player_name;
    team_name = b.pb_team_name;
    games_played = b.pb_gp;
    total_minutes = (float_of_int b.pb_min_seconds) /. 60.0;
    total_points = b.pb_pts;
    total_rebounds = b.pb_reb;
    total_assists = b.pb_ast;
    total_steals = b.pb_stl;
    total_blocks = b.pb_blk;
    total_turnovers = b.pb_tov;
    avg_points = b.pb_avg_pts;
    avg_margin = b.pb_margin;
    avg_rebounds = b.pb_avg_reb;
    avg_assists = b.pb_avg_ast;
    avg_steals = b.pb_avg_stl;
    avg_blocks = b.pb_avg_blk;
    avg_turnovers = b.pb_avg_tov;
    efficiency = b.pb_eff;
  }

let sort_player_bases sort (items: player_base list) =
  let compare_name a b = String.compare a.pb_player_name b.pb_player_name in
  match sort with
  | ByPoints ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_avg_pts a.pb_avg_pts in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByMargin ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_margin a.pb_margin in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByRebounds ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_avg_reb a.pb_avg_reb in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByAssists ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_avg_ast a.pb_avg_ast in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByEfficiency ->
      List.sort
        (fun a b ->
          let primary = Float.compare b.pb_eff a.pb_eff in
          if primary <> 0 then primary else compare_name a b)
        items
  | ByMinutes ->
      List.sort
        (fun a b ->
          let primary = compare b.pb_min_seconds a.pb_min_seconds in
          if primary <> 0 then primary else compare_name a b)
        items

let filter_player_bases ~search ~sort (items: player_base list) =
  let filtered =
    items
    |> List.filter (player_base_matches search)
  in
  let with_margin_threshold =
    match sort with
    | ByMargin -> List.filter (fun b -> b.pb_margin_seconds >= 6000) filtered
    | _ -> filtered
  in
  sort_player_bases sort with_margin_threshold

let ensure_schema () = with_db (fun db -> Repo.ensure_schema db)
let refresh_matviews () = with_db (fun db -> Repo.refresh_matviews db)
let get_all_teams () =
  cached teams_cache "all" (fun () -> with_db (fun db -> Repo.get_teams db))
let get_seasons () =
  cached seasons_cache "all" (fun () -> with_db (fun db -> Repo.get_seasons db))
let get_player_by_name ?(season="ALL") name = with_db (fun db -> Repo.get_player_by_name ~name ~season db)
let get_player_aggregate_by_id ~player_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_aggregate_by_id ~player_id ~season db)
let get_player_shooting_stats ~player_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_shooting_stats ~player_id ~season db)
let get_players_base ?(season="ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "season=%s|mismatch=%b" season include_mismatch in
  cached players_base_cache key (fun () ->
    with_db (fun db -> Repo.get_players_base ~season ~include_mismatch db))
let get_players ?(season="ALL") ?(limit=50) ?(search="") ?(sort=ByEfficiency) ?(include_mismatch=false) () =
  let key =
    Printf.sprintf "season=%s|limit=%d|search=%s|sort=%s|mismatch=%b"
      season
      limit
      (cache_key_text search)
      (player_sort_key sort)
      include_mismatch
  in
  cached players_cache key (fun () ->
    let (let*) = Result.bind in
    let* base = get_players_base ~season ~include_mismatch () in
    let filtered = filter_player_bases ~search ~sort base in
    let limited = take limit filtered in
    let result = List.map player_base_to_aggregate limited in
    Ok result)
let get_players_by_team ~team_name ?(season="ALL") ?(limit=20) () =
  let key =
    Printf.sprintf "team=%s|season=%s|limit=%d"
      (cache_key_text team_name)
      season
      limit
  in
  cached players_by_team_cache key (fun () ->
    with_db (fun db -> Repo.get_players_by_team ~team_name ~season ~limit db))

let get_draft_years () =
  cached draft_years_cache "years" (fun () -> with_db (fun db -> Repo.get_draft_years db))

let get_draft_picks ?(year=0) ?(search="") () =
  let key =
    Printf.sprintf "picks|year=%d|search=%s" year (cache_key_text search)
  in
  cached draft_picks_cache key (fun () -> with_db (fun db -> Repo.get_draft_picks ~year ~search db))

let get_official_trade_years () =
  cached trade_years_cache "years" (fun () -> with_db (fun db -> Repo.get_official_trade_years db))

let get_official_trade_events ?(year=0) ?(search="") () =
  let key =
    Printf.sprintf "events|year=%d|search=%s" year (cache_key_text search)
  in
  cached trade_events_cache key (fun () ->
    with_db (fun db -> Repo.get_official_trade_events ~year ~search db))
let build_margin_map margins : team_margin MarginMap.t =
  List.fold_left
    (fun acc (row: team_margin) ->
      let key = (row.season, row.team) in
      MarginMap.add key row acc)
    MarginMap.empty
    margins

let sort_team_stats sort_field (items : team_stats list) =
  let metric = function
    | TeamByPoints -> (fun (row: team_stats) -> row.pts)
    | TeamByRebounds -> (fun row -> row.reb)
    | TeamByAssists -> (fun row -> row.ast)
    | TeamBySteals -> (fun row -> row.stl)
    | TeamByBlocks -> (fun row -> row.blk)
    | TeamByEfficiency -> (fun row -> row.eff)
    | TeamByTsPct -> (fun row -> row.ts_pct)
    | TeamByFg3Pct -> (fun row -> row.fg3_pct)
    | TeamByMinutes -> (fun row -> row.min_total)
  in
  let getter = metric sort_field in
  List.sort
    (fun a b ->
      let primary = compare (getter b) (getter a) in
      if primary <> 0 then primary else String.compare a.team b.team)
    items

let dedupe_team_stats (items : team_stats list) =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  items
  |> List.filter (fun (row: team_stats) ->
      let team = normalize_label row.team in
      let key =
        match team_code_of_string team with
        | Some code -> code
        | None -> team |> String.uppercase_ascii
      in
      if Hashtbl.mem seen key then
        false
      else (
        Hashtbl.add seen key ();
        true
      ))

let get_team_stats ?(season="ALL") ?(scope=PerGame) ?(sort=TeamByPoints) ?(include_mismatch=false) () =
  let key =
    Printf.sprintf "season=%s|scope=%s|sort=%s|mismatch=%b"
      season
      (team_scope_to_string scope)
      (team_sort_key sort)
      include_mismatch
  in
  cached team_stats_cache key (fun () ->
    let (let*) = Result.bind in
    let* totals = with_db (fun db -> Repo.get_team_totals ~season ~include_mismatch db) in
    let* margins = with_db (fun db -> Repo.get_team_margins ~season ~include_mismatch db) in
    let margin_map = build_margin_map margins in
    let stats =
      totals
      |> List.map (fun (row: team_totals) ->
          let key = (row.season, row.team) in
          let margin = MarginMap.find_opt key margin_map in
          Stats.team_stats_of_totals ~scope ~margin row)
      |> sort_team_stats sort
      |> dedupe_team_stats
    in
    Ok stats)
let calculate_gb (standings : team_standing list) = match standings with | [] -> [] | leader :: others -> let calc s = let wins_diff = Stdlib.float (leader.wins - s.wins) in let losses_diff = Stdlib.float (s.losses - leader.losses) in (wins_diff +. losses_diff) /. 2.0 in leader :: List.map (fun s -> { s with gb = calc s }) others
let get_standings ?(season = "ALL") () =
  let key = Printf.sprintf "season=%s" season in
  cached standings_cache key (fun () ->
    let (let*) = Result.bind in
    let* standings = with_db (fun db -> Repo.get_standings ~season db) in
    Ok (calculate_gb standings))
let get_games ?(season = "ALL") () =
  let key = Printf.sprintf "season=%s" season in
  cached games_cache key (fun () -> with_db (fun db -> Repo.get_games ~season db))
let get_scored_games ?(season = "ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "season=%s|mismatch=%b" season include_mismatch in
  cached scored_games_cache key (fun () ->
    with_db (fun db -> Repo.get_scored_games ~season ~include_mismatch db))
let get_game_season_code ~game_id () = with_db (fun db -> Repo.get_game_season_code ~game_id db)
let get_pbp_periods ~game_id () = with_db (fun db -> Repo.get_pbp_periods ~game_id db)
let get_pbp_events ~game_id ~period_code () =
  with_db (fun db -> Repo.get_pbp_events ~game_id ~period_code db)
let get_team_core_player_ids ~season ~team_name ?(limit=7) () =
  with_db (fun db -> Repo.get_team_core_player_ids ~season ~team_name ~limit db)
let get_team_active_player_ids ~team_name ~game_id () =
  with_db (fun db -> Repo.get_team_active_player_ids ~team_name ~game_id db)
let get_boxscore ~game_id () =
  let key = Printf.sprintf "game_id=%s" game_id in
  cached boxscore_cache key (fun () ->
    let (let*) = Result.bind in
    let* game_info_opt = with_db (fun db -> Repo.get_game_info ~game_id db) in
    let* stats = with_db (fun db -> Repo.get_boxscore_stats ~game_id db) in
    match game_info_opt with
    | Some game_info ->
        let home_players = List.filter (fun s -> s.bs_team_code = game_info.gi_home_team_code) stats in
        let away_players = List.filter (fun s -> s.bs_team_code = game_info.gi_away_team_code) stats in
        Ok { boxscore_game = game_info; boxscore_home_players = home_players; boxscore_away_players = away_players }
    | None -> Error (QueryFailed "Game not found"))
let get_leaders_base ?(season="ALL") () =
  let key = cache_key_text season in
  cached leaders_base_cache key (fun () -> with_db (fun db -> Repo.get_leaders_base ~season db))

let leaders_from_base ~scope ~category (bases: leader_base list) =
  let scope_value = scope |> String.trim |> String.lowercase_ascii in
  let category_value = category |> String.trim |> String.lowercase_ascii in
  let to_entry (b: leader_base) value =
    { le_player_id = b.lb_player_id
    ; le_player_name = b.lb_player_name
    ; le_team_name = b.lb_team_name
    ; le_stat_value = value
    }
  in
  let minutes_total (b: leader_base) = (float_of_int b.lb_min_seconds) /. 60.0 in
  let minutes_per_game (b: leader_base) =
    if b.lb_gp = 0 then 0.0 else minutes_total b /. float_of_int b.lb_gp
  in
  let per_game_int v (b: leader_base) =
    if b.lb_gp = 0 then 0.0 else (float_of_int v) /. float_of_int b.lb_gp
  in
  let per_game_float v (b: leader_base) =
    if b.lb_gp = 0 then 0.0 else v /. float_of_int b.lb_gp
  in
  let per_36_int v (b: leader_base) =
    if b.lb_min_seconds <= 0 then 0.0 else (float_of_int v) /. float_of_int b.lb_min_seconds *. 2160.0
  in
  let per_36_float v (b: leader_base) =
    if b.lb_min_seconds <= 0 then 0.0 else v /. float_of_int b.lb_min_seconds *. 2160.0
  in
  let fg_pct (b: leader_base) =
    if b.lb_fg_a = 0 then 0.0 else (float_of_int b.lb_fg_m) /. float_of_int b.lb_fg_a
  in
  let fg3_pct (b: leader_base) =
    if b.lb_fg3_a = 0 then 0.0 else (float_of_int b.lb_fg3_m) /. float_of_int b.lb_fg3_a
  in
  let ft_pct (b: leader_base) =
    if b.lb_ft_a = 0 then 0.0 else (float_of_int b.lb_ft_m) /. float_of_int b.lb_ft_a
  in
  let ts_pct (b: leader_base) =
    let denom = 2.0 *. ((float_of_int b.lb_fg_a) +. 0.44 *. (float_of_int b.lb_ft_a)) in
    if denom <= 0.0 then 0.0 else (float_of_int b.lb_pts) /. denom
  in
  let efg_pct (b: leader_base) =
    if b.lb_fg_a = 0 then 0.0
    else
      ((float_of_int b.lb_fg_m) +. 0.5 *. (float_of_int b.lb_fg3_m)) /. float_of_int b.lb_fg_a
  in
  let min_games (b: leader_base) = b.lb_gp >= 5 in
  let min_minutes (b: leader_base) = b.lb_min_seconds >= 6000 in
  let min_fga (b: leader_base) = b.lb_fg_a >= 50 in
  let min_fg3a (b: leader_base) = b.lb_fg3_a >= 20 in
  let min_fta (b: leader_base) = b.lb_ft_a >= 20 in
  let min_ts (b: leader_base) = (b.lb_fg_a + b.lb_ft_a) >= 50 in
  let value_fn, predicate =
    match scope_value with
    | "totals" ->
        (match category_value with
        | "gp" -> (fun b -> float_of_int b.lb_gp), (fun _ -> true)
        | "min" -> (fun b -> minutes_total b), (fun _ -> true)
        | "pts" -> (fun b -> float_of_int b.lb_pts), min_games
        | "reb" -> (fun b -> float_of_int b.lb_reb), min_games
        | "ast" -> (fun b -> float_of_int b.lb_ast), min_games
        | "stl" -> (fun b -> float_of_int b.lb_stl), min_games
        | "blk" -> (fun b -> float_of_int b.lb_blk), min_games
        | "tov" -> (fun b -> float_of_int b.lb_tov), min_games
        | "fg_pct" -> fg_pct, min_fga
        | "fg3_pct" -> fg3_pct, min_fg3a
        | "ft_pct" -> ft_pct, min_fta
        | "ts_pct" -> ts_pct, min_ts
        | "efg_pct" -> efg_pct, min_fga
        | _ -> (fun _ -> 0.0), (fun _ -> false))
    | "per_36" ->
        (match category_value with
        | "pts" -> (fun b -> per_36_int b.lb_pts b), min_minutes
        | "reb" -> (fun b -> per_36_int b.lb_reb b), min_minutes
        | "ast" -> (fun b -> per_36_int b.lb_ast b), min_minutes
        | "stl" -> (fun b -> per_36_int b.lb_stl b), min_minutes
        | "blk" -> (fun b -> per_36_int b.lb_blk b), min_minutes
        | "tov" -> (fun b -> per_36_int b.lb_tov b), min_minutes
        | "eff" -> (fun b -> per_36_float b.lb_eff b), min_minutes
        | "fg_pct" -> fg_pct, min_fga
        | "fg3_pct" -> fg3_pct, min_fg3a
        | "ft_pct" -> ft_pct, min_fta
        | "ts_pct" -> ts_pct, min_ts
        | "efg_pct" -> efg_pct, min_fga
        | _ -> (fun _ -> 0.0), (fun _ -> false))
    | _ ->
        (match category_value with
        | "pts" -> (fun b -> per_game_int b.lb_pts b), min_games
        | "reb" -> (fun b -> per_game_int b.lb_reb b), min_games
        | "ast" -> (fun b -> per_game_int b.lb_ast b), min_games
        | "stl" -> (fun b -> per_game_int b.lb_stl b), min_games
        | "blk" -> (fun b -> per_game_int b.lb_blk b), min_games
        | "tov" -> (fun b -> per_game_int b.lb_tov b), min_games
        | "min" -> minutes_per_game, min_games
        | "eff" -> (fun b -> per_game_float b.lb_eff b), min_games
        | "fg_pct" -> fg_pct, min_fga
        | "fg3_pct" -> fg3_pct, min_fg3a
        | "ft_pct" -> ft_pct, min_fta
        | "ts_pct" -> ts_pct, min_ts
        | "efg_pct" -> efg_pct, min_fga
        | _ -> (fun _ -> 0.0), (fun _ -> false))
  in
  bases
  |> List.filter predicate
  |> List.map (fun b -> to_entry b (value_fn b))
  |> List.sort (fun a b -> Float.compare b.le_stat_value a.le_stat_value)
  |> take 5

let get_leaders ?(season="ALL") ?(scope="per_game") category =
  let key = Printf.sprintf "season=%s|scope=%s|category=%s" season scope category in
  cached leaders_cache key (fun () ->
    let (let*) = Result.bind in
    let* bases = get_leaders_base ~season () in
    Ok (leaders_from_base ~scope ~category bases))

let get_stat_mvp_eff ?(season="ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "mvp|season=%s|mismatch=%b" season include_mismatch in
  cached awards_cache key (fun () ->
    with_db (fun db -> Repo.get_stat_mvp_eff ~season ~include_mismatch db))

let get_stat_mip_eff_delta ~season ~prev_season ?(include_mismatch=false) () =
  let key = Printf.sprintf "mip|season=%s|prev=%s|mismatch=%b" season prev_season include_mismatch in
  cached awards_cache key (fun () ->
    with_db (fun db -> Repo.get_stat_mip_eff_delta ~season ~prev_season ~include_mismatch db))

let get_player_profile ~player_id () =
  let key = Printf.sprintf "player_id=%s" player_id in
  cached player_profile_cache key (fun () -> with_db (fun db -> Repo.get_player_profile ~player_id db))
let get_player_season_stats ~player_id ~scope () =
  let key = Printf.sprintf "player_id=%s|scope=%s" player_id scope in
  cached player_season_stats_cache key (fun () ->
    with_db (fun db -> Repo.get_player_season_stats ~player_id ~scope db))
let get_player_game_logs ~player_id ?(season="ALL") ?(include_mismatch=false) () =
  let key = Printf.sprintf "player_id=%s|season=%s|mismatch=%b" player_id season include_mismatch in
  cached player_game_logs_cache key (fun () ->
    with_db (fun db -> Repo.get_player_game_logs ~player_id ~season ~include_mismatch db))

(** Batch fetch game logs for multiple players in single query.
    Returns a Hashtbl mapping player_id -> player_game_stat list *)
let get_batch_player_game_logs ~player_ids ?(season="ALL") () =
  let (let*) = Result.bind in
  let* rows = with_db (fun db -> Repo.get_batch_player_game_logs ~player_ids ~season db) in
  let tbl = Hashtbl.create (List.length player_ids) in
  List.iter (fun (row: player_game_stat_with_id) ->
    let existing = match Hashtbl.find_opt tbl row.pgs_player_id with
      | Some lst -> lst
      | None -> []
    in
    Hashtbl.replace tbl row.pgs_player_id (row.pgs_stat :: existing)
  ) rows;
  (* Reverse lists since we iterated in DESC order but appended to front *)
  Hashtbl.iter (fun k v -> Hashtbl.replace tbl k (List.rev v)) tbl;
  Ok tbl

let get_team_full_detail ~team_name ?(season="ALL") () =
  let key = Printf.sprintf "team=%s|season=%s" (cache_key_text team_name) season in
  cached team_detail_cache key (fun () ->
    let (let*) = Result.bind in
    let* standings = get_standings ~season () in
    let* roster = get_players_by_team ~team_name ~season () in
    let* games = with_db (fun db -> Repo.get_team_recent_games ~team_name ~season db) in
    let* game_results = with_db (fun db -> Repo.get_team_games ~team_name ~season db) in
    let* all_totals = with_db (fun db -> Repo.get_team_totals ~season ~include_mismatch:false db) in
    let standing = List.find_opt (fun (s: team_standing) -> s.team_name = team_name) standings in
    let team_totals = List.find_opt (fun (t: team_totals) -> t.team = team_name) all_totals in
    Ok { tfd_team_name = team_name; tfd_standing = standing; tfd_roster = roster; tfd_game_results = game_results; tfd_recent_games = games; tfd_team_totals = team_totals })
let get_player_h2h_data ~p1_id ~p2_id ?(season="ALL") () =
  with_db (fun db -> Repo.get_player_h2h ~p1_id ~p2_id ~season db)

let get_db_quality_report () : qa_db_report db_result =
  cached qa_report_cache "qa_report" (fun () ->
    let (let*) = Result.bind in
    let int_or_zero = Option.value ~default:0 in
    let round1 v = Float.round (v *. 10.0) /. 10.0 in
    let* games_total_opt = with_db (fun db -> Repo.qa_games_total db) in
    let* games_with_stats_opt = with_db (fun db -> Repo.qa_games_with_stats db) in
    let* plus_minus_games_opt = with_db (fun db -> Repo.qa_plus_minus_games db) in
    let* mismatch_count_opt = with_db (fun db -> Repo.qa_score_mismatch_count db) in
    let* mismatch_sample = with_db (fun db -> Repo.qa_score_mismatch_sample db) in
    let* team_count_anomaly_count_opt = with_db (fun db -> Repo.qa_team_count_anomaly_count db) in
    let* team_count_anomaly_sample = with_db (fun db -> Repo.qa_team_count_anomaly_sample db) in
    let* dup_row_count_opt = with_db (fun db -> Repo.qa_duplicate_player_row_count db) in
    let* dup_row_sample = with_db (fun db -> Repo.qa_duplicate_player_row_sample db) in
    let* dup_name_count_opt = with_db (fun db -> Repo.qa_duplicate_player_name_count db) in
    let* dup_name_rows = with_db (fun db -> Repo.qa_duplicate_player_name_sample db) in
    let games_total = int_or_zero games_total_opt in
    let games_with_stats = int_or_zero games_with_stats_opt in
    let plus_minus_games = int_or_zero plus_minus_games_opt in
    let plus_minus_coverage_pct =
      if games_total <= 0 then 0.0
      else round1 ((float_of_int plus_minus_games /. float_of_int games_total) *. 100.0)
    in
    let dup_name_sample =
      dup_name_rows
      |> List.map (fun (name, id_count, ids_csv) ->
          { qdpn_player_name = name;
            qdpn_id_count = id_count;
            qdpn_player_ids = split_csv_ids ids_csv;
          })
    in
    Ok { qdr_generated_at = iso8601_utc ();
        qdr_games_total = games_total;
        qdr_games_with_stats = games_with_stats;
        qdr_plus_minus_games = plus_minus_games;
        qdr_plus_minus_coverage_pct = plus_minus_coverage_pct;
        qdr_score_mismatch_count = int_or_zero mismatch_count_opt;
        qdr_score_mismatch_sample = mismatch_sample;
        qdr_team_count_anomaly_count = int_or_zero team_count_anomaly_count_opt;
        qdr_team_count_anomaly_sample = team_count_anomaly_sample;
        qdr_duplicate_player_row_count = int_or_zero dup_row_count_opt;
        qdr_duplicate_player_row_sample = dup_row_sample;
        qdr_duplicate_player_name_count = int_or_zero dup_name_count_opt;
        qdr_duplicate_player_name_sample = dup_name_sample;
      })

(* ===== History & Legends Public API ===== *)

let get_historical_seasons () =
  cached history_cache "all" (fun () ->
    with_db (fun db -> Repo.get_historical_seasons db))

let get_legend_players () =
  cached legends_cache "all" (fun () ->
    with_db (fun db -> Repo.get_legend_players db))

let get_coaches () =
  cached coaches_cache "all" (fun () ->
    with_db (fun db -> Repo.get_coaches db))

let get_player_career ~player_name () =
  let key = Printf.sprintf "player=%s" (cache_key_text player_name) in
  cached player_career_cache key (fun () ->
    with_db (fun db -> Repo.get_player_career ~player_name db))

(* ===== Schedule Public API ===== *)

let get_upcoming_schedule ?(status="scheduled") ?(limit=10) () =
  let key = Printf.sprintf "status=%s,limit=%d" status limit in
  cached schedule_cache key (fun () ->
    with_db (fun db -> Repo.get_upcoming_schedule ~status ~limit db))

let get_schedule_by_date_range ~start_date ~end_date ?(status="ALL") () =
  let key = Printf.sprintf "start=%s,end=%s,status=%s" start_date end_date status in
  cached schedule_cache key (fun () ->
    with_db (fun db -> Repo.get_schedule_by_date_range ~start_date ~end_date ~status db))

(* ===== MVP Race Public API ===== *)

let mvp_race_cache = Cache.create ~ttl:120.0 ~max_entries:16

let get_mvp_race ?(season="ALL") ?(min_games=5) () =
  let key = Printf.sprintf "season=%s,min_games=%d" season min_games in
  let min_games_str = string_of_int min_games in
  let (let*) = Result.bind in
  let* candidates = cached mvp_race_cache key (fun () ->
    with_db (fun db -> Repo.get_mvp_race_candidates ~season ~min_games:min_games_str db)) in
  (* Add rank to each candidate *)
  let ranked = candidates |> List.mapi (fun i c -> { c with mvp_rank = i + 1 }) in
  Ok ranked

(* ===== Clutch Time Stats Public API ===== *)

(** Get clutch time stats for a season.
    Clutch time = Q4 remaining 5 min + score diff <= 5 points.
    Uses play_by_play_events with player_id to aggregate scoring stats. *)
let get_clutch_stats ~season () =
  let (let*) = Result.bind in
  let s = if String.trim season = "" then "ALL" else season in
  let* rows = with_db (fun (module Db : Caqti_eio.CONNECTION) ->
    Db.collect_list Queries.clutch_stats_by_season s
  ) in
  Ok rows

(* ===== On/Off Impact Public API ===== *)

(** Convert DB row to on_off_impact *)
let row_to_on_off_impact (player_id, (player_name, (team_name, (games_played, (total_min_seconds, (total_plus_minus, games_with_pm)))))) : Domain.on_off_impact =
  let total_minutes = float_of_int total_min_seconds /. 60.0 in
  let avg_pm = if games_with_pm > 0 then float_of_int total_plus_minus /. float_of_int games_with_pm else 0.0 in
  {
    ooi_player_id = player_id;
    ooi_player_name = player_name;
    ooi_team_name = team_name;
    ooi_games_played = games_played;
    ooi_total_minutes = total_minutes;
    ooi_on_court = {
      ocs_games = games_with_pm;
      ocs_minutes = total_minutes;
      ocs_team_pts = 0;
      ocs_opp_pts = 0;
      ocs_possessions = 0.0;
    };
    ooi_off_court = {
      ofcs_games = 0;
      ofcs_minutes = 0.0;
      ofcs_team_pts = 0;
      ofcs_opp_pts = 0;
      ofcs_possessions = 0.0;
    };
    ooi_net_rating_on = 0.0;
    ooi_net_rating_off = 0.0;
    ooi_net_rating_diff = 0.0;
    ooi_plus_minus_total = total_plus_minus;
    ooi_plus_minus_avg = avg_pm;
  }

(** Get on/off impact stats for all qualifying players in a season *)
let get_on_off_impact_stats ~season ?(limit=50) () =
  let (let*) = Result.bind in
  let limit_str = string_of_int limit in
  let* rows = with_db (fun (module Db : Caqti_eio.CONNECTION) ->
    Db.collect_list Queries.on_off_impact_stats (season, limit_str)
  ) in
  Ok (List.map row_to_on_off_impact rows)

(** Get on/off impact for a specific player *)
let get_on_off_impact_for_player ~player_id ~season () =
  let (let*) = Result.bind in
  let* row_opt = with_db (fun (module Db : Caqti_eio.CONNECTION) ->
    Db.find_opt Queries.on_off_impact_for_player (player_id, season)
  ) in
  match row_opt with
  | Some row -> Ok (Some (row_to_on_off_impact row))
  | None -> Ok None

(* ===== Lineup Chemistry Public API ===== *)

(** Lineup analysis data - player stats per game for lineup analysis *)
type lineup_game_data = {
  lgd_game_id: string;
  lgd_player_id: string;
  lgd_player_name: string;
  lgd_team_code: string;
  lgd_team_name: string;
  lgd_minutes: float;
  lgd_pts: int;
  lgd_plus_minus: int option;
}

let lineup_cache = Cache.create ~ttl:300.0 ~max_entries:32

(** Get all player game stats for lineup analysis *)
let get_lineup_data ~season ~team_name () =
  let key = Printf.sprintf "season=%s,team=%s" season team_name in
  cached lineup_cache key (fun () ->
    with_db (fun (module Db : Caqti_eio.CONNECTION) ->
      let open Caqti_type in
      let (let*) = Result.bind in
      let query = Request_oneshot.(
        t2 string (t2 string (t2 string string)) ->*
        (t2 string (t2 string (t2 string (t2 string (t2 string (t2 int (t2 int (option int))))))))
      ) {|
        SELECT
          s.game_id,
          s.player_id,
          p.player_name,
          s.team_code,
          t.team_name,
          s.min_seconds,
          s.pts,
          s.plus_minus
        FROM game_stats s
        JOIN games g ON g.game_id = s.game_id
        JOIN players p ON p.player_id = s.player_id
        JOIN teams t ON t.team_code = s.team_code
        WHERE g.game_type != '10'
          AND (g.season_id = ? OR ? = 'ALL')
          AND (s.team_code = ? OR ? = 'ALL')
          AND s.min_seconds > 0
        ORDER BY s.game_id, s.team_code, s.min_seconds DESC
      |} in
      let* data = Db.collect_list query (season, (season, (team_name, team_name))) in
      let parsed = data |> List.map (fun (game_id, (player_id, (player_name, (team_code, (team_name, (min_secs, (pts, plus_minus))))))) ->
        { lgd_game_id = game_id;
          lgd_player_id = player_id;
          lgd_player_name = player_name;
          lgd_team_code = team_code;
          lgd_team_name = team_name;
          lgd_minutes = float_of_int min_secs /. 60.0;
          lgd_pts = pts;
          lgd_plus_minus = plus_minus;
        })
      in
      Ok parsed))

(** Module for lineup analysis calculations *)
module LineupAnalysis = struct
  open Domain

  (** Group game data by (game_id, team_code) *)
  let group_by_game_team (data: lineup_game_data list)
      : (string * string, lineup_game_data list) Hashtbl.t =
    let tbl = Hashtbl.create 256 in
    List.iter (fun d ->
      let key = (d.lgd_game_id, d.lgd_team_code) in
      let existing = try Hashtbl.find tbl key with Not_found -> [] in
      Hashtbl.replace tbl key (d :: existing)
    ) data;
    tbl

  (** Get top 5 players by minutes in a game-team combo *)
  let get_starting_five (players: lineup_game_data list) : lineup_game_data list =
    players
    |> List.sort (fun a b -> compare b.lgd_minutes a.lgd_minutes)
    |> (fun l ->
        let rec take n acc = function
          | [] -> List.rev acc
          | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
        in take 5 [] l)

  (** Create lineup key from players *)
  let players_to_key (players: lineup_game_data list) : string =
    players
    |> List.map (fun p -> p.lgd_player_id)
    |> List.sort String.compare
    |> String.concat ","

  (** Analyze lineups from game data *)
  let analyze_lineups ~team_name (data: lineup_game_data list) : lineup_stats list =
    let grouped = group_by_game_team data in
    (* Collect lineup stats *)
    let lineup_tbl : (string, lineup_stats * lineup_game_data list) Hashtbl.t =
      Hashtbl.create 128 in

    Hashtbl.iter (fun (_game_id, team_code) players ->
      if team_name = "ALL" || team_code = team_name then begin
        let top5 = get_starting_five players in
        if List.length top5 >= 5 then begin
          let key = players_to_key top5 in
          let team = (List.hd top5).lgd_team_name in
          let total_min = List.fold_left (fun acc p -> acc +. p.lgd_minutes) 0.0 top5 in
          let total_pts = List.fold_left (fun acc p -> acc + p.lgd_pts) 0 top5 in
          let total_pm = List.fold_left (fun acc p ->
            acc + (match p.lgd_plus_minus with Some pm -> pm | None -> 0)
          ) 0 top5 in

          let (existing, players_ref) =
            try Hashtbl.find lineup_tbl key
            with Not_found -> (empty_lineup_stats ~team_name:team, []) in

          let updated = {
            existing with
            ls_games_together = existing.ls_games_together + 1;
            ls_total_minutes = existing.ls_total_minutes +. total_min;
            ls_total_pts = existing.ls_total_pts + total_pts;
            ls_plus_minus = existing.ls_plus_minus + total_pm;
          } in
          Hashtbl.replace lineup_tbl key (updated, top5 @ players_ref)
        end
      end
    ) grouped;

    (* Convert to list and calculate averages *)
    Hashtbl.fold (fun _key (stats, players_sample) acc ->
      let player_infos =
        players_sample
        |> List.sort_uniq (fun a b -> String.compare a.lgd_player_id b.lgd_player_id)
        |> (fun l ->
            let rec take n acc = function
              | [] -> List.rev acc
              | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
            in take 5 [] l)
        |> List.map (fun p -> {
            lp_player_id = p.lgd_player_id;
            lp_player_name = p.lgd_player_name;
            lp_position = None;
          })
      in
      let avg_pts_per_min =
        if stats.ls_total_minutes > 0.0
        then float_of_int stats.ls_total_pts /. stats.ls_total_minutes
        else 0.0 in
      let avg_margin_per_min =
        if stats.ls_total_minutes > 0.0
        then float_of_int stats.ls_plus_minus /. stats.ls_total_minutes
        else 0.0 in
      { stats with
        ls_players = player_infos;
        ls_avg_pts_per_min = avg_pts_per_min;
        ls_avg_margin_per_min = avg_margin_per_min;
      } :: acc
    ) lineup_tbl []

  (** Calculate player pair synergies *)
  let analyze_synergies (lineups: lineup_stats list) : lineup_synergy list =
    let pair_tbl : (string * string, int * float * int) Hashtbl.t = Hashtbl.create 256 in

    (* Collect pair data from lineups *)
    List.iter (fun lineup ->
      let players = lineup.ls_players in
      (* Generate all pairs *)
      let rec pairs acc = function
        | [] -> acc
        | p1 :: rest ->
          let new_pairs = List.map (fun p2 -> (p1, p2)) rest in
          pairs (new_pairs @ acc) rest
      in
      let all_pairs = pairs [] players in
      List.iter (fun (p1, p2) ->
        let (id1, id2) =
          if p1.lp_player_id < p2.lp_player_id
          then (p1.lp_player_id, p2.lp_player_id)
          else (p2.lp_player_id, p1.lp_player_id) in
        let key = (id1, id2) in
        let (games, mins, pm) =
          try Hashtbl.find pair_tbl key
          with Not_found -> (0, 0.0, 0) in
        Hashtbl.replace pair_tbl key
          (games + lineup.ls_games_together,
           mins +. lineup.ls_total_minutes,
           pm + lineup.ls_plus_minus)
      ) all_pairs
    ) lineups;

    (* Build player name lookup *)
    let name_tbl = Hashtbl.create 64 in
    List.iter (fun lineup ->
      List.iter (fun p ->
        Hashtbl.replace name_tbl p.lp_player_id p.lp_player_name
      ) lineup.ls_players
    ) lineups;

    (* Convert to synergy list *)
    Hashtbl.fold (fun (id1, id2) (games, mins, pm) acc ->
      let avg_pm = if mins > 0.0 then float_of_int pm /. mins else 0.0 in
      let score = calculate_synergy_score
        ~games_together:games ~total_minutes:mins ~avg_plus_minus:avg_pm in
      let name1 = try Hashtbl.find name_tbl id1 with Not_found -> id1 in
      let name2 = try Hashtbl.find name_tbl id2 with Not_found -> id2 in
      { syn_player1_id = id1;
        syn_player1_name = name1;
        syn_player2_id = id2;
        syn_player2_name = name2;
        syn_games_together = games;
        syn_total_minutes = mins;
        syn_avg_plus_minus = avg_pm;
        syn_synergy_score = score;
      } :: acc
    ) pair_tbl []
    |> List.sort (fun a b -> compare b.syn_synergy_score a.syn_synergy_score)
end

(** Get full lineup chemistry analysis for a team *)
let get_lineup_chemistry ~season ~team_name () : (Domain.lineup_chemistry, db_error) result =
  let (let*) = Result.bind in
  let open Domain in
  let* data = get_lineup_data ~season ~team_name () in
  let lineups = LineupAnalysis.analyze_lineups ~team_name data in
  let synergies = LineupAnalysis.analyze_synergies lineups in

  (* Sort lineups by different criteria *)
  let by_minutes =
    lineups
    |> List.sort (compare_lineup_stats LineupByMinutes)
    |> (fun l -> let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
      in take 10 [] l) in

  let by_plus_minus =
    lineups
    |> List.filter (fun l -> l.ls_total_minutes >= 20.0)  (* Min 20 min filter *)
    |> List.sort (compare_lineup_stats LineupByPlusMinus)
    |> (fun l -> let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
      in take 10 [] l) in

  let top_synergies =
    synergies
    |> List.filter (fun s -> s.syn_games_together >= 2)
    |> (fun l -> let rec take n acc = function
        | [] -> List.rev acc
        | x :: xs -> if n <= 0 then List.rev acc else take (n-1) (x::acc) xs
      in take 20 [] l) in

  Ok {
    lc_team_name = team_name;
    lc_season = season;
    lc_top_lineups = by_plus_minus;
    lc_frequent_lineups = by_minutes;
    lc_synergies = top_synergies;
  }
