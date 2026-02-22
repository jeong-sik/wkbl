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

(* --- Phase 2: Leaders, MVP Race, Clutch Stats, Awards --- *)

let leader_entry_type : (unit, leader_entry option) S.typ =
  S.(obj "LeaderEntry"
    ~doc:"Statistical category leader"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (e : leader_entry) -> e.le_player_id);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (e : leader_entry) -> e.le_player_name);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (e : leader_entry) -> e.le_team_name);
      field "statValue" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (e : leader_entry) -> e.le_stat_value);
    ])

let mvp_candidate_type : (unit, mvp_candidate option) S.typ =
  S.(obj "MvpCandidate"
    ~doc:"MVP race candidate with scoring formula"
    ~fields:[
      field "rank" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_rank);
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_player_id);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_player_name);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_team_name);
      field "teamCode" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_team_code);
      field "gamesPlayed" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_games_played);
      field "ppg" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_ppg);
      field "rpg" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_rpg);
      field "apg" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_apg);
      field "spg" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_spg);
      field "bpg" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_bpg);
      field "efficiency" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_efficiency);
      field "teamWins" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_team_wins);
      field "teamLosses" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_team_losses);
      field "teamWinPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_team_win_pct);
      field "baseScore" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_base_score);
      field "winBonus" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_win_bonus);
      field "finalScore" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (m : mvp_candidate) -> m.mvp_final_score);
    ])

let clutch_stats_type : (unit, clutch_stats option) S.typ =
  S.(obj "ClutchStats"
    ~doc:"Player clutch game performance"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_player_id);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_player_name);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_team_name);
      field "clutchGames" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_games);
      field "clutchPoints" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_points);
      field "clutchFgMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_fg_made);
      field "clutchFgAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_fg_att);
      field "clutchFgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_fg_pct);
      field "clutchFtMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_ft_made);
      field "clutchFtAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_ft_att);
      field "clutch3pMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : clutch_stats) -> c.cs_clutch_3p_made);
    ])

(* Award records: Caqti returns raw tuples, convert to GraphQL-friendly type *)
type award_record = {
  ar_season_name: string;
  ar_category: string;
  ar_player_name: string;
  ar_stat_value: string option;
  ar_votes: string option;
}

let award_of_tuple (sn, (cat, (pn, (sv, votes)))) =
  { ar_season_name = sn; ar_category = cat; ar_player_name = pn;
    ar_stat_value = sv; ar_votes = votes }

let award_record_type : (unit, award_record option) S.typ =
  S.(obj "AwardRecord"
    ~doc:"WKBL award entry"
    ~fields:[
      field "seasonName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.ar_season_name);
      field "category" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.ar_category);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.ar_player_name);
      field "statValue" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.ar_stat_value);
      field "votes" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.ar_votes);
    ])

type award_leader = {
  al_player_name: string;
  al_count: int;
}

let award_leader_type : (unit, award_leader option) S.typ =
  S.(obj "AwardLeader"
    ~doc:"Player with most awards in a category"
    ~fields:[
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.al_player_name);
      field "count" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info a -> a.al_count);
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

    (* --- Phase 2: Leaders, MVP Race, Clutch Stats, Awards --- *)

    field "leaders"
      ~doc:"Statistical category leaders (pts, reb, ast, stl, blk, etc.)"
      ~typ:(non_null (list (non_null leader_entry_type)))
      ~args:Arg.[
        arg "category" ~typ:(non_null string) ~doc:"Stat category (pts, reb, ast, stl, blk, tov, min, fg_pct, etc.)";
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "scope" ~typ:string ~doc:"per_game, totals, or per_36 (default: per_game)";
      ]
      ~resolve:(fun _info () category season scope ->
        let season = match season with Some s -> s | None -> "ALL" in
        let scope = match scope with Some s -> s | None -> "per_game" in
        Db.get_leaders ~season ~scope category |> resolve_db);

    field "mvpRace"
      ~doc:"MVP race rankings with scoring formula"
      ~typ:(non_null (list (non_null mvp_candidate_type)))
      ~args:Arg.[
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "minGames" ~typ:int ~doc:"Minimum games played (default: 5)";
      ]
      ~resolve:(fun _info () season min_games ->
        let season = match season with Some s -> s | None -> "ALL" in
        let min_games = match min_games with Some m -> m | None -> 5 in
        Db.get_mvp_race ~season ~min_games () |> resolve_db);

    field "clutchStats"
      ~doc:"Player clutch game performance"
      ~typ:(non_null (list (non_null clutch_stats_type)))
      ~args:Arg.[
        arg "season" ~typ:(non_null string) ~doc:"Season code";
      ]
      ~resolve:(fun _info () season ->
        Db.get_clutch_stats ~season () |> resolve_db);

    field "awardsByCategory"
      ~doc:"Awards filtered by category"
      ~typ:(non_null (list (non_null award_record_type)))
      ~args:Arg.[
        arg "category" ~typ:(non_null string) ~doc:"Award category (e.g. Scoring, Rebounding, Best5)";
      ]
      ~resolve:(fun _info () category ->
        Db.get_awards_by_category ~category ()
        |> resolve_db
        |> List.map award_of_tuple);

    field "awardsBySeason"
      ~doc:"Awards filtered by season"
      ~typ:(non_null (list (non_null award_record_type)))
      ~args:Arg.[
        arg "seasonName" ~typ:(non_null string) ~doc:"Season name";
      ]
      ~resolve:(fun _info () season_name ->
        Db.get_awards_by_season ~season_name ()
        |> resolve_db
        |> List.map award_of_tuple);

    field "awardsByPlayer"
      ~doc:"Awards filtered by player name (partial match)"
      ~typ:(non_null (list (non_null award_record_type)))
      ~args:Arg.[
        arg "playerName" ~typ:(non_null string) ~doc:"Player name (partial match)";
      ]
      ~resolve:(fun _info () player_name ->
        Db.get_awards_by_player ~player_name ()
        |> resolve_db
        |> List.map award_of_tuple);

    field "awardLeaders"
      ~doc:"Players with most awards in a category"
      ~typ:(non_null (list (non_null award_leader_type)))
      ~args:Arg.[
        arg "category" ~typ:(non_null string) ~doc:"Award category";
      ]
      ~resolve:(fun _info () category ->
        Db.get_award_leaders ~category ()
        |> resolve_db
        |> List.map (fun (name, count) -> { al_player_name = name; al_count = count }));

    field "awardCount"
      ~doc:"Total number of awards in database"
      ~typ:int
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        Db.get_award_count () |> resolve_db);
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
