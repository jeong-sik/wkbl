(** GraphQL Schema for WKBL Analytics API.

    Uses ocaml-graphql-server's synchronous Graphql.Schema directly.
    Since Graphql.Schema has ['a Io.t = 'a], all DB calls via Caqti-Eio
    work correctly inside Eio fibers without any async adapter. *)

open Domain

module S = Graphql.Schema

(* --- GraphQL Object Types --- *)

let season_type : (unit, season_info option) S.typ =
  S.(obj "Season"
    ~doc:"WKBL season"
    ~fields:[
      field "code" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_info) -> s.code);
      field "name" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_info) -> s.name);
    ])

let team_info_type : (unit, team_info option) S.typ =
  S.(obj "TeamInfo"
    ~doc:"Basic team info"
    ~fields:[
      field "teamCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_info) -> t.team_code);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_info) -> t.team_name);
    ])

let player_type : (unit, player_aggregate option) S.typ =
  S.(obj "Player"
    ~doc:"Player aggregate stats"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.player_id);
      field "name" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.name);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.team_name);
      field "gamesPlayed" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.games_played);
      field "totalMinutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_minutes);
      field "avgPoints" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_points);
      field "avgMargin" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_margin);
      field "avgRebounds" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_rebounds);
      field "avgAssists" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_assists);
      field "avgSteals" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_steals);
      field "avgBlocks" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_blocks);
      field "avgTurnovers" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.avg_turnovers);
      field "efficiency" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.efficiency);
      field "totalPoints" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_points);
      field "totalRebounds" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_rebounds);
      field "totalAssists" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_assists);
      field "totalSteals" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_steals);
      field "totalBlocks" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_blocks);
      field "totalTurnovers" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_turnovers);
      field "fgMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_fg_made);
      field "fgAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_fg_att);
      field "fg3Made" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_fg3_made);
      field "fg3Att" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_fg3_att);
      field "ftMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_ft_made);
      field "ftAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (p : player_aggregate) -> p.total_ft_att);
    ])

let team_standing_type : (unit, team_standing option) S.typ =
  S.(obj "TeamStanding"
    ~doc:"Team standing in season"
    ~fields:[
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.team_name);
      field "gamesPlayed" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.games_played);
      field "wins" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.wins);
      field "losses" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.losses);
      field "winPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.win_pct);
      field "gb" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.gb);
      field "avgPts" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.avg_pts);
      field "avgOppPts" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.avg_opp_pts);
      field "diff" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_standing) -> t.diff);
    ])

let team_stats_type : (unit, team_stats option) S.typ =
  S.(obj "TeamStats"
    ~doc:"Team per-game or total stats"
    ~fields:[
      field "team" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.team);
      field "gp" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.gp);
      field "pts" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.pts);
      field "margin" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.margin);
      field "reb" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.reb);
      field "ast" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.ast);
      field "stl" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.stl);
      field "blk" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.blk);
      field "turnovers" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.turnovers);
      field "fgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.fg_pct);
      field "fg3Pct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.fg3_pct);
      field "ftPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.ft_pct);
      field "efgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.efg_pct);
      field "tsPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.ts_pct);
      field "pace" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.pace);
      field "eff" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (t : team_stats) -> t.eff);
    ])

let game_summary_type : (unit, game_summary option) S.typ =
  S.(obj "GameSummary"
    ~doc:"Game result summary"
    ~fields:[
      field "gameId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.game_id);
      field "gameDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.game_date);
      field "homeTeam" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.home_team);
      field "awayTeam" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.away_team);
      field "homeScore" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.home_score);
      field "awayScore" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.away_score);
      field "gameType" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_summary) -> g.game_type);
    ])

(* --- Enum types for arguments --- *)

let player_sort_enum =
  S.Arg.enum "PlayerSort"
    ~doc:"Sort criteria for players"
    ~values:[
      S.enum_value "POINTS" ~value:ByPoints ~doc:"Sort by points";
      S.enum_value "MARGIN" ~value:ByMargin ~doc:"Sort by margin";
      S.enum_value "REBOUNDS" ~value:ByRebounds ~doc:"Sort by rebounds";
      S.enum_value "ASSISTS" ~value:ByAssists ~doc:"Sort by assists";
      S.enum_value "EFFICIENCY" ~value:ByEfficiency ~doc:"Sort by efficiency";
      S.enum_value "MINUTES" ~value:ByMinutes ~doc:"Sort by minutes";
    ]

(* --- Helper: db_result to value, raising on error --- *)

let resolve_db : ('a, Db_common.db_error) result -> 'a = function
  | Ok v -> v
  | Error e ->
      let msg = match e with
        | Db_common.ConnectionFailed s -> "DB connection failed: " ^ s
        | Db_common.QueryFailed s -> "Query failed: " ^ s
        | Db_common.ParseError s -> "Parse error: " ^ s
      in
      failwith msg

(* --- Query Root --- *)

let query_fields : (unit, unit) S.field list =
  S.[
    field "seasons"
      ~doc:"List available seasons"
      ~typ:(non_null (list (non_null season_type)))
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        Db.get_seasons () |> resolve_db);

    field "teams"
      ~doc:"List all teams"
      ~typ:(non_null (list (non_null team_info_type)))
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        Db.get_all_teams () |> resolve_db);

    field "players"
      ~doc:"Query players with optional filters"
      ~typ:(non_null (list (non_null player_type)))
      ~args:Arg.[
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "limit" ~typ:int ~doc:"Max results (default: 50)";
        arg "search" ~typ:string ~doc:"Search by name";
        arg "sort" ~typ:player_sort_enum ~doc:"Sort criteria";
      ]
      ~resolve:(fun _info () season limit search sort ->
        let season = match season with Some s -> s | None -> "ALL" in
        let limit = match limit with Some l -> l | None -> 50 in
        let search = match search with Some s -> s | None -> "" in
        let sort = match sort with Some s -> s | None -> ByEfficiency in
        Db.get_players ~season ~limit ~search ~sort () |> resolve_db);

    field "player"
      ~doc:"Get single player by ID"
      ~typ:player_type
      ~args:Arg.[
        arg "playerId" ~typ:(non_null string) ~doc:"Player ID";
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
      ]
      ~resolve:(fun _info () player_id season ->
        let season = match season with Some s -> s | None -> "ALL" in
        match Db.get_player_aggregate_by_id ~player_id ~season () with
        | Ok p -> p
        | Error _ -> None);

    field "standings"
      ~doc:"League standings"
      ~typ:(non_null (list (non_null team_standing_type)))
      ~args:Arg.[
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
      ]
      ~resolve:(fun _info () season ->
        let season = match season with Some s -> s | None -> "ALL" in
        Db.get_standings ~season () |> resolve_db);

    field "teamStats"
      ~doc:"Team statistics"
      ~typ:(non_null (list (non_null team_stats_type)))
      ~args:Arg.[
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "scope" ~typ:string ~doc:"totals or per_game (default: per_game)";
      ]
      ~resolve:(fun _info () season scope ->
        let season = match season with Some s -> s | None -> "ALL" in
        let scope = match scope with
          | Some s -> team_scope_of_string s
          | None -> PerGame
        in
        Db.get_team_stats ~season ~scope () |> resolve_db);

    field "games"
      ~doc:"Game results with pagination"
      ~typ:(non_null (list (non_null game_summary_type)))
      ~args:Arg.[
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "page" ~typ:int ~doc:"Page number (default: 1)";
        arg "pageSize" ~typ:int ~doc:"Results per page (default: 50)";
      ]
      ~resolve:(fun _info () season page page_size ->
        let season = match season with Some s -> s | None -> "ALL" in
        let page = match page with Some p -> p | None -> 1 in
        let page_size = match page_size with Some ps -> ps | None -> 50 in
        Db.get_games ~page ~page_size ~season () |> resolve_db);
  ]

let schema : unit S.schema =
  S.schema query_fields

(* --- Query execution --- *)

let execute_query ?variables ?operation_name query_str =
  match Graphql_parser.parse query_str with
  | Error err -> Error (`String ("Parse error: " ^ err))
  | Ok doc ->
      let result = S.execute schema () ?variables ?operation_name doc in
      match result with
      | Ok (`Response json) -> Ok json
      | Ok (`Stream _) -> Error (`String "Subscriptions not supported")
      | Error (`String msg) -> Error (`String msg)
      | Error json -> Error json
