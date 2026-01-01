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
end

(** SQL Queries - typed and validated at compile time *)
module Queries = struct
  open Caqti_request.Infix
  open Caqti_type

  (** Get all teams *)
  let all_teams =
    (unit ->* string)
    "SELECT team_name_kr FROM teams ORDER BY team_name_kr"

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

  (** Get top players by efficiency *)
  let get_top_players ~limit (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.top_players limit

  (** Get players by team *)
  let get_players_by_team ~team_name ~limit (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Queries.players_by_team (team_name, limit)
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

let get_players ?(limit=50) () =
  with_db (fun db -> Repo.get_top_players ~limit db)

let get_players_by_team ~team_name ?(limit=20) () =
  with_db (fun db -> Repo.get_players_by_team ~team_name ~limit db)
