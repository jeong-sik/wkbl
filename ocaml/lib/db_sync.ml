module BoxscoreSync = struct
  open Caqti_request.Infix
  open Caqti_type

  let i = int
  let s = string

  (* 16 ints built via powers of 2 *)
  let i2 = t2 i i
  let i4 = t2 i2 i2
  let i8 = t2 i4 i4
  let i16 = t2 i8 i8
  
  (* 3 strings + 16 ints = 19 fields *)
  let stat_type = t4 s s s i16

  let upsert_game_stat_query =
    (stat_type ->. unit)
    {|INSERT INTO game_stats
      (game_id, team_code, player_id, min_seconds, fg_2p_m, fg_2p_a, fg_3p_m, fg_3p_a, ft_m, ft_a, reb_off, reb_def, reb_tot, ast, stl, blk, tov, pf, pts)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)
      ON CONFLICT (game_id, player_id) DO UPDATE SET
        team_code = EXCLUDED.team_code,
        min_seconds = EXCLUDED.min_seconds,
        pts = EXCLUDED.pts,
        ast = EXCLUDED.ast,
        reb_tot = EXCLUDED.reb_tot,
        stl = EXCLUDED.stl,
        blk = EXCLUDED.blk,
        tov = EXCLUDED.tov|}

  let games_without_stats_query =
    (t2 s s ->* s)
    {|SELECT game_id FROM games
      WHERE game_id NOT IN (SELECT DISTINCT game_id FROM game_stats)
        AND ($1 = 'ALL' OR season_code = $2)
      ORDER BY game_date DESC|}

  let game_teams_query =
    (s ->? t2 s s)
    {|SELECT home_team_code, away_team_code FROM games WHERE game_id = ?|}

  let upsert_game_stat (stat : Scraper.boxscore_entry) ~game_id ~team_code (module Db_conn : Caqti_eio.CONNECTION) =
    let tuple16 = 
      ((((stat.bs_min_seconds, stat.bs_fg2_m), (stat.bs_fg2_a, stat.bs_fg3_m)),
        ((stat.bs_fg3_a, stat.bs_ft_m), (stat.bs_ft_a, stat.bs_off_reb))),
       (((stat.bs_def_reb, stat.bs_tot_reb), (stat.bs_ast, stat.bs_stl)),
        ((stat.bs_blk, stat.bs_tov), (stat.bs_pf, stat.bs_pts))))
    in
    Db_conn.exec upsert_game_stat_query (game_id, team_code, stat.bs_player_id, tuple16)

  let get_games_without_stats ~season (module Db_conn : Caqti_eio.CONNECTION) =
    Db_conn.collect_list games_without_stats_query (season, season)

  let get_game_teams ~game_id (module Db_conn : Caqti_eio.CONNECTION) =
    Db_conn.find_opt game_teams_query game_id
end

let upsert_game_stat stat ~game_id ~team_code =
  Db.with_db (fun db -> BoxscoreSync.upsert_game_stat stat ~game_id ~team_code db)

let get_games_without_stats ?(season="ALL") () =
  Db.with_db (fun db -> BoxscoreSync.get_games_without_stats ~season db)

let get_game_teams ~game_id =
  Db.with_db (fun db -> BoxscoreSync.get_game_teams ~game_id db)