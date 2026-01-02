(** Database Layer using Caqti

    Principles:
    - Type-safe queries
    - Connection pooling via Caqti
    - All errors as Result, never exceptions
*)

open Domain

(** Database error type - explicit, not string *)
type db_error =
  | ConnectionFailed of string
  | QueryFailed of string
  | ParseError of string
[@@deriving show]

(** Result type alias for convenience *)
type 'a db_result = ('a, db_error) result Lwt.t

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
    let encode _ = assert false in  (* We only decode *)
    let decode (name, (team_name, (games_played, (total_minutes,
               (avg_points, (avg_rebounds, (avg_assists,
               (avg_steals, (avg_blocks, (avg_turnovers, efficiency)))))))))) =
      Ok {
        name;
        team_name;
        games_played;
        total_minutes;
        avg_points;
        avg_rebounds;
        avg_assists;
        avg_steals;
        avg_blocks;
        avg_turnovers;
        efficiency;
      }
    in
    custom ~encode ~decode
      (tup2 string (tup2 string (tup2 int (tup2 float (tup2 float (tup2 float
       (tup2 float (tup2 float (tup2 float (tup2 float float))))))))))

  let season_info =
    let encode _ = assert false in
    let decode (code, name) =
      Ok { code; name }
    in
    custom ~encode ~decode (tup2 string string)

  let team_totals =
    let encode _ = assert false in
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
                                          pts ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
      =
      Ok {
        season;
        team;
        gp;
        min_total;
        fg2_m;
        fg2_a;
        fg3_m;
        fg3_a;
        ft_m;
        ft_a;
        reb_off;
        reb_def;
        reb;
        ast;
        stl;
        blk;
        turnovers;
        pts;
      }
    in
    custom ~encode ~decode
      (tup2 string (tup2 string (tup2 int (tup2 float (tup2 int (tup2 int
       (tup2 int (tup2 int (tup2 int (tup2 int (tup2 int (tup2 int
       (tup2 int (tup2 int (tup2 int (tup2 int (tup2 int int)))))))))))))))))

  let team_margin =
    let encode _ = assert false in
    let decode (season, (team, (gp, (pts_for, pts_against)))) =
      Ok { season; team; gp; pts_for; pts_against }
    in
    custom ~encode ~decode (tup2 string (tup2 string (tup2 int (tup2 int int))))
end

(** SQL Queries - typed and validated at compile time *)
module Queries = struct
  open Caqti_request.Infix
  open Caqti_type

  (** Get all teams *)
  let all_teams =
    (unit ->* string)
    "SELECT team_name_kr FROM teams ORDER BY team_name_kr"

  let all_seasons =
    (unit ->* Types.season_info)
    "SELECT season_code, season_name FROM seasons ORDER BY season_code"

  (** Get player aggregates with filters *)
  let player_stats_base =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        SUM(s.min_seconds) / 60.0,
        AVG(s.pts),
        AVG(s.reb_tot),
        AVG(s.ast),
        AVG(s.stl),
        AVG(s.blk),
        AVG(s.tov),
        AVG(s.game_score)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE (? = 'ALL' OR t.team_name_kr = ?)
      GROUP BY p.player_id
      ORDER BY AVG(s.game_score) DESC
      LIMIT ?
    |}

  let team_totals_by_season =
    (tup2 string string ->* Types.team_totals)
    {|
      SELECT
        g.season_code,
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
      JOIN games g ON g.game_id = s.game_id
      JOIN teams t ON t.team_code = s.team_code
      WHERE (? = 'ALL' OR g.season_code = ?)
      GROUP BY g.season_code, t.team_code
      ORDER BY t.team_name_kr ASC
    |}

  let team_margin_by_season =
    (tup2 string string ->* Types.team_margin)
    {|
      SELECT
        g.season_code,
        t.team_name_kr,
        COUNT(*) as gp,
        SUM(CASE WHEN g.home_team_code = t.team_code THEN g.home_score ELSE g.away_score END) as pts_for,
        SUM(CASE WHEN g.home_team_code = t.team_code THEN g.away_score ELSE g.home_score END) as pts_against
      FROM games g
      JOIN teams t ON t.team_code = g.home_team_code OR t.team_code = g.away_team_code
      WHERE (? = 'ALL' OR g.season_code = ?)
      GROUP BY g.season_code, t.team_code
      ORDER BY t.team_name_kr ASC
    |}

  (** Simplified query - just get top players *)
  let top_players =
    (int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      GROUP BY p.player_id
      ORDER BY AVG(s.game_score) DESC
      LIMIT ?
    |}

  let player_stats_by_points =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE p.player_name LIKE ?
      GROUP BY p.player_id
      ORDER BY AVG(s.pts) DESC
      LIMIT ?
    |}

  let player_stats_by_rebounds =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE p.player_name LIKE ?
      GROUP BY p.player_id
      ORDER BY AVG(s.reb_tot) DESC
      LIMIT ?
    |}

  let player_stats_by_assists =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE p.player_name LIKE ?
      GROUP BY p.player_id
      ORDER BY AVG(s.ast) DESC
      LIMIT ?
    |}

  let player_stats_by_efficiency =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE p.player_name LIKE ?
      GROUP BY p.player_id
      ORDER BY AVG(s.game_score) DESC
      LIMIT ?
    |}

  let player_stats_by_minutes =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE p.player_name LIKE ?
      GROUP BY p.player_id
      ORDER BY SUM(s.min_seconds) DESC
      LIMIT ?
    |}

  (** Get players by team name *)
  let players_by_team =
    (tup2 string int ->* Types.player_aggregate)
    {|
      SELECT
        p.player_name,
        t.team_name_kr,
        COUNT(*) as gp,
        COALESCE(SUM(s.min_seconds) / 60.0, 0),
        COALESCE(AVG(s.pts), 0),
        COALESCE(AVG(s.reb_tot), 0),
        COALESCE(AVG(s.ast), 0),
        COALESCE(AVG(s.stl), 0),
        COALESCE(AVG(s.blk), 0),
        COALESCE(AVG(s.tov), 0),
        COALESCE(AVG(s.game_score), 0)
      FROM game_stats s
      JOIN players p ON s.player_id = p.player_id
      JOIN teams t ON s.team_code = t.team_code
      WHERE t.team_name_kr = ?
      GROUP BY p.player_id
      ORDER BY AVG(s.game_score) DESC
      LIMIT ?
    |}
end

(** Database operations - return Caqti_error for pool compatibility *)
module Repo = struct
  (** Get all team names *)
  let get_teams (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.all_teams ()

  let get_seasons (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.all_seasons ()

  let query_for_sort = function
    | ByPoints -> Queries.player_stats_by_points
    | ByRebounds -> Queries.player_stats_by_rebounds
    | ByAssists -> Queries.player_stats_by_assists
    | ByEfficiency -> Queries.player_stats_by_efficiency
    | ByMinutes -> Queries.player_stats_by_minutes

  let get_players_filtered ~sort ~search ~limit (module Db : Caqti_lwt.CONNECTION) =
    let pattern = normalize_search_pattern search in
    Db.collect_list (query_for_sort sort) (pattern, limit)

  (** Get top players by efficiency *)
  let get_top_players ~limit (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.top_players limit

  (** Get players by team *)
  let get_players_by_team ~team_name ~limit (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.players_by_team (team_name, limit)

  let get_team_totals ~season (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.team_totals_by_season (season, season)

  let get_team_margins ~season (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.team_margin_by_season (season, season)
end

(** Connection pool *)
let pool_ref : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t option ref =
  ref None

let init_pool db_path =
  let uri = Uri.of_string ("sqlite3:" ^ db_path) in
  match Caqti_lwt.connect_pool uri with
  | Ok pool ->
      pool_ref := Some pool;
      Ok ()
  | Error e ->
      Error (ConnectionFailed (Caqti_error.show e))

let with_db f =
  let open Lwt.Syntax in
  match !pool_ref with
  | None -> Lwt.return (Error (ConnectionFailed "Pool not initialized"))
  | Some pool ->
      let* result = Caqti_lwt.Pool.use f pool in
      match result with
      | Ok v -> Lwt.return (Ok v)
      | Error e -> Lwt.return (Error (QueryFailed (Caqti_error.show e)))

(** Public API - all return Result, never raise *)

let get_all_teams () =
  with_db (fun db -> Repo.get_teams db)

let get_seasons () =
  with_db (fun db -> Repo.get_seasons db)

let get_players ?(limit=50) ?(search="") ?(sort=ByEfficiency) () =
  with_db (fun db -> Repo.get_players_filtered ~sort ~search ~limit db)

let get_players_by_team ~team_name ?(limit=20) () =
  with_db (fun db -> Repo.get_players_by_team ~team_name ~limit db)

let build_margin_map (margins: team_margin list) : team_margin MarginMap.t =
  List.fold_left
    (fun (acc: team_margin MarginMap.t) (row: team_margin) ->
      let key = (row.season, row.team) in
      MarginMap.add key row acc)
    (MarginMap.empty : team_margin MarginMap.t)
    margins

let sort_team_stats sort_field items =
  let metric = function
    | TeamByPoints -> (fun row -> row.pts)
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

let get_team_stats ?(season="ALL") ?(scope=PerGame) ?(sort=TeamByPoints) () =
  let open Lwt.Syntax in
  let* totals_result = with_db (fun db -> Repo.get_team_totals ~season db) in
  let* margins_result = with_db (fun db -> Repo.get_team_margins ~season db) in
  match totals_result, margins_result with
  | Ok totals, Ok margins ->
      let margin_map = build_margin_map margins in
      let stats =
        totals
        |> List.map (fun (row: team_totals) ->
            let key = (row.season, row.team) in
            let margin = MarginMap.find_opt key margin_map in
            Stats.team_stats_of_totals ~scope ~margin row)
        |> sort_team_stats sort
      in
      Lwt.return (Ok stats)
  | Error err, _ -> Lwt.return (Error err)
  | _, Error err -> Lwt.return (Error err)
