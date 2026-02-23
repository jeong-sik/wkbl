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

(* --- Phase 3: Schedule, Boxscore, Player Profile, H2H, Legends --- *)

let string_of_score_quality = function
  | Verified -> "verified"
  | Derived -> "derived"
  | Mismatch -> "mismatch"

let schedule_entry_type : (unit, schedule_entry option) S.typ =
  S.(obj "ScheduleEntry"
    ~doc:"Scheduled or completed game entry"
    ~fields:[
      field "id" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_id);
      field "gameDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_game_date);
      field "gameTime" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_game_time);
      field "seasonCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_season_code);
      field "homeTeamCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_home_team_code);
      field "awayTeamCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_away_team_code);
      field "homeTeamName" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_home_team_name);
      field "awayTeamName" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_away_team_name);
      field "venue" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_venue);
      field "status" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : schedule_entry) -> s.sch_status);
    ])

let boxscore_player_stat_type : (unit, boxscore_player_stat option) S.typ =
  S.(obj "BoxscorePlayerStat"
    ~doc:"Player statistics in a single game boxscore"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_player_id);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_player_name);
      field "position" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_position);
      field "teamCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_team_code);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_team_name);
      field "minutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_minutes);
      field "points" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_pts);
      field "plusMinus" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_plus_minus);
      field "rebounds" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_reb);
      field "assists" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_ast);
      field "steals" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_stl);
      field "blocks" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_blk);
      field "turnovers" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_tov);
      field "fgMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_fg_made);
      field "fgAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_fg_att);
      field "fgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_fg_pct);
      field "fg3Made" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_fg3_made);
      field "fg3Att" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_fg3_att);
      field "fg3Pct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_fg3_pct);
      field "ftMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_ft_made);
      field "ftAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_ft_att);
      field "ftPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (b : boxscore_player_stat) -> b.bs_ft_pct);
    ])

let game_info_type : (unit, game_info option) S.typ =
  S.(obj "GameInfo"
    ~doc:"Detailed game information with score quality"
    ~fields:[
      field "gameId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_game_id);
      field "gameDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_game_date);
      field "homeTeamCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_home_team_code);
      field "homeTeamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_home_team_name);
      field "awayTeamCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_away_team_code);
      field "awayTeamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_away_team_name);
      field "homeScore" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_home_score);
      field "awayScore" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> g.gi_away_score);
      field "scoreQuality" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : game_info) -> string_of_score_quality g.gi_score_quality);
    ])

let game_boxscore_type : (unit, game_boxscore option) S.typ =
  S.(obj "GameBoxscore"
    ~doc:"Full game boxscore with both teams' player stats"
    ~fields:[
      field "game" ~typ:(non_null game_info_type)
        ~args:Arg.[]
        ~resolve:(fun _info (b : game_boxscore) -> b.boxscore_game);
      field "homePlayers" ~typ:(non_null (list (non_null boxscore_player_stat_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (b : game_boxscore) -> b.boxscore_home_players);
      field "awayPlayers" ~typ:(non_null (list (non_null boxscore_player_stat_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (b : game_boxscore) -> b.boxscore_away_players);
    ])

let player_game_stat_type : (unit, player_game_stat option) S.typ =
  S.(obj "PlayerGameStat"
    ~doc:"Player statistics for a single game"
    ~fields:[
      field "gameId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.game_id);
      field "gameDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.game_date);
      field "opponent" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.opponent);
      field "isHome" ~typ:(non_null bool)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.is_home);
      field "teamScore" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.team_score);
      field "opponentScore" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.opponent_score);
      field "scoreQuality" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> string_of_score_quality g.score_quality);
      field "minutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.min);
      field "fgMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.fg_made);
      field "fgAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.fg_att);
      field "fg3Made" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.fg3_made);
      field "fg3Att" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.fg3_att);
      field "ftMade" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.ft_made);
      field "ftAtt" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.ft_att);
      field "points" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.pts);
      field "rebounds" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.reb);
      field "assists" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.ast);
      field "steals" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.stl);
      field "blocks" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.blk);
      field "turnovers" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.tov);
      field "plusMinus" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (g : player_game_stat) -> g.plus_minus);
    ])

let season_stats_type : (unit, season_stats option) S.typ =
  S.(obj "SeasonStats"
    ~doc:"Player per-season statistical breakdown"
    ~fields:[
      field "seasonCode" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_season_code);
      field "seasonName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_season_name);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_team_name);
      field "gamesPlayed" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_games_played);
      field "totalMinutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_total_minutes);
      field "avgPoints" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_avg_points);
      field "avgRebounds" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_avg_rebounds);
      field "avgAssists" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_avg_assists);
      field "avgSteals" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_avg_steals);
      field "avgBlocks" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_avg_blocks);
      field "avgTurnovers" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_avg_turnovers);
      field "efficiency" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_efficiency);
      field "margin" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_margin);
      field "fgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_fg_pct);
      field "fg3Pct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_fg3_pct);
      field "ftPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_ft_pct);
      field "tsPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_ts_pct);
      field "efgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_efg_pct);
      field "usgPct" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : season_stats) -> s.ss_usg_pct);
    ])

let career_high_item_type : (unit, career_high_item option) S.typ =
  S.(obj "CareerHighItem"
    ~doc:"Player career high record"
    ~fields:[
      field "label" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : career_high_item) -> c.chi_label);
      field "value" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : career_high_item) -> c.chi_value);
      field "gameId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : career_high_item) -> c.chi_game_id);
      field "gameDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : career_high_item) -> c.chi_game_date);
      field "opponent" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : career_high_item) -> c.chi_opponent);
      field "isHome" ~typ:(non_null bool)
        ~args:Arg.[]
        ~resolve:(fun _info (c : career_high_item) -> c.chi_is_home);
    ])

let player_team_stint_type : (unit, player_team_stint option) S.typ =
  S.(obj "PlayerTeamStint"
    ~doc:"Period a player spent on a team"
    ~fields:[
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : player_team_stint) -> s.pts_team_name);
      field "startDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : player_team_stint) -> s.pts_start_date);
      field "endDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : player_team_stint) -> s.pts_end_date);
      field "gamesPlayed" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : player_team_stint) -> s.pts_games_played);
    ])

let player_draft_type : (unit, player_draft option) S.typ =
  S.(obj "PlayerDraft"
    ~doc:"Player draft information"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_player_id);
      field "draftYear" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_draft_year);
      field "draftRound" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_draft_round);
      field "pickInRound" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_pick_in_round);
      field "overallPick" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_overall_pick);
      field "draftTeam" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_draft_team);
      field "rawText" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (d : player_draft) -> d.pd_raw_text);
    ])

let official_trade_event_type : (unit, official_trade_event option) S.typ =
  S.(obj "OfficialTradeEvent"
    ~doc:"Official player trade/transfer event"
    ~fields:[
      field "eventDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : official_trade_event) -> t.ote_event_date);
      field "eventYear" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (t : official_trade_event) -> t.ote_event_year);
      field "eventText" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : official_trade_event) -> t.ote_event_text);
      field "sourceUrl" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (t : official_trade_event) -> t.ote_source_url);
    ])

let player_external_link_type : (unit, player_external_link option) S.typ =
  S.(obj "PlayerExternalLink"
    ~doc:"External link for a player (e.g. WKBL profile)"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (l : player_external_link) -> l.pel_player_id);
      field "linkType" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (l : player_external_link) -> l.pel_link_type);
      field "url" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (l : player_external_link) -> l.pel_url);
    ])

let player_career_entry_type : (unit, player_career_entry option) S.typ =
  S.(obj "PlayerCareerEntry"
    ~doc:"Player career history entry (year by year)"
    ~fields:[
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_player_name);
      field "seasonId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_season_id);
      field "team" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_team);
      field "jerseyNumber" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_jersey_number);
      field "gamesPlayed" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_games_played);
      field "pointsPerGame" ~typ:float
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_points_per_game);
      field "reboundsPerGame" ~typ:float
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_rebounds_per_game);
      field "assistsPerGame" ~typ:float
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_assists_per_game);
      field "isAllStar" ~typ:(non_null bool)
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_is_allstar);
      field "awards" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (e : player_career_entry) -> e.pce_awards);
    ])

let player_profile_type : (unit, player_profile option) S.typ =
  S.(obj "PlayerProfile"
    ~doc:"Comprehensive player profile with all stats and history"
    ~fields:[
      field "player" ~typ:(non_null (obj "PlayerInfo"
        ~doc:"Basic player information"
        ~fields:[
          field "id" ~typ:(non_null string)
            ~args:Arg.[]
            ~resolve:(fun _info (p : player_info) -> p.id);
          field "name" ~typ:(non_null string)
            ~args:Arg.[]
            ~resolve:(fun _info (p : player_info) -> p.name);
          field "position" ~typ:string
            ~args:Arg.[]
            ~resolve:(fun _info (p : player_info) -> p.position);
          field "birthDate" ~typ:string
            ~args:Arg.[]
            ~resolve:(fun _info (p : player_info) -> p.birth_date);
          field "height" ~typ:int
            ~args:Arg.[]
            ~resolve:(fun _info (p : player_info) -> p.height);
          field "weight" ~typ:int
            ~args:Arg.[]
            ~resolve:(fun _info (p : player_info) -> p.weight);
        ]))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.player);
      field "averages" ~typ:(non_null player_type)
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.averages);
      field "recentGames" ~typ:(non_null (list (non_null player_game_stat_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.recent_games);
      field "allStarGames" ~typ:(non_null (list (non_null player_game_stat_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.all_star_games);
      field "draft" ~typ:player_draft_type
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.draft);
      field "officialTradeEvents" ~typ:(non_null (list (non_null official_trade_event_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.official_trade_events);
      field "externalLinks" ~typ:(non_null (list (non_null player_external_link_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.external_links);
      field "teamStints" ~typ:(non_null (list (non_null player_team_stint_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.team_stints);
      field "seasonBreakdown" ~typ:(non_null (list (non_null season_stats_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.season_breakdown);
      field "careerHighs" ~typ:(list (non_null career_high_item_type))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.career_highs);
      field "careerEntries" ~typ:(non_null (list (non_null player_career_entry_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (pp : player_profile) -> pp.career_entries);
    ])

let draft_pick_row_type : (unit, draft_pick_row option) S.typ =
  S.(obj "DraftPick"
    ~doc:"Draft pick database record"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_player_id);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_player_name);
      field "draftYear" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_draft_year);
      field "draftRound" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_draft_round);
      field "pickInRound" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_pick_in_round);
      field "overallPick" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_overall_pick);
      field "draftTeam" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_draft_team);
      field "rawText" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (d : draft_pick_row) -> d.dpr_raw_text);
    ])

let h2h_game_type : (unit, h2h_game option) S.typ =
  S.(obj "H2HGame"
    ~doc:"Head-to-head game between two players"
    ~fields:[
      field "gameId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.hg_game_id);
      field "gameDate" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.hg_game_date);
      field "player1Team" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player1_team);
      field "player2Team" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player2_team);
      field "player1Pts" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player1_pts);
      field "player1Reb" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player1_reb);
      field "player1Ast" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player1_ast);
      field "player1Stl" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player1_stl);
      field "player1Blk" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player1_blk);
      field "player2Pts" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player2_pts);
      field "player2Reb" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player2_reb);
      field "player2Ast" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player2_ast);
      field "player2Stl" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player2_stl);
      field "player2Blk" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.player2_blk);
      field "winnerTeam" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.winner_team);
      field "scoreDiff" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (h : h2h_game) -> h.score_diff);
    ])

let h2h_summary_type : (unit, h2h_summary option) S.typ =
  S.(obj "H2HSummary"
    ~doc:"Aggregated head-to-head comparison summary"
    ~fields:[
      field "totalGames" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_total_games);
      field "player1Wins" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p1_wins);
      field "player2Wins" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p2_wins);
      field "player1AvgPts" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p1_avg_pts);
      field "player1AvgReb" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p1_avg_reb);
      field "player1AvgAst" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p1_avg_ast);
      field "player1AvgStl" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p1_avg_stl);
      field "player1AvgBlk" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p1_avg_blk);
      field "player2AvgPts" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p2_avg_pts);
      field "player2AvgReb" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p2_avg_reb);
      field "player2AvgAst" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p2_avg_ast);
      field "player2AvgStl" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p2_avg_stl);
      field "player2AvgBlk" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : h2h_summary) -> s.h2h_p2_avg_blk);
    ])

(** H2H result container for both games list and summary *)
type h2h_result = {
  hr_games: h2h_game list;
  hr_summary: h2h_summary;
}

let h2h_result_type : (unit, h2h_result option) S.typ =
  S.(obj "H2HResult"
    ~doc:"Head-to-head comparison result with games and summary"
    ~fields:[
      field "games" ~typ:(non_null (list (non_null h2h_game_type)))
        ~args:Arg.[]
        ~resolve:(fun _info (r : h2h_result) -> r.hr_games);
      field "summary" ~typ:(non_null h2h_summary_type)
        ~args:Arg.[]
        ~resolve:(fun _info (r : h2h_result) -> r.hr_summary);
    ])

let legend_player_type : (unit, legend_player option) S.typ =
  S.(obj "LegendPlayer"
    ~doc:"WKBL legendary player"
    ~fields:[
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_player_name);
      field "careerYears" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_career_years);
      field "teams" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_teams);
      field "championships" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_championships);
      field "mvpCount" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_mvp_count);
      field "allStarCount" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_all_star_count);
      field "careerPoints" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_career_points);
      field "careerRebounds" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_career_rebounds);
      field "careerAssists" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_career_assists);
      field "notableAchievements" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_notable_achievements);
      field "isHallOfFame" ~typ:(non_null bool)
        ~args:Arg.[]
        ~resolve:(fun _info (l : legend_player) -> l.lp_is_hall_of_fame);
    ])

let coach_type : (unit, coach option) S.typ =
  S.(obj "Coach"
    ~doc:"WKBL coach"
    ~fields:[
      field "coachName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_coach_name);
      field "team" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_team);
      field "tenureStart" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_tenure_start);
      field "tenureEnd" ~typ:int
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_tenure_end);
      field "championships" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_championships);
      field "regularSeasonWins" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_regular_season_wins);
      field "playoffWins" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_playoff_wins);
      field "formerPlayer" ~typ:(non_null bool)
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_former_player);
      field "playerCareerYears" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_player_career_years);
      field "notableAchievements" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (c : coach) -> c.c_notable_achievements);
    ])

let on_court_stats_type : (unit, on_court_stats option) S.typ =
  S.(obj "OnCourtStats"
    ~doc:"Team performance when player is on court"
    ~fields:[
      field "games" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : on_court_stats) -> s.ocs_games);
      field "minutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : on_court_stats) -> s.ocs_minutes);
      field "teamPts" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : on_court_stats) -> s.ocs_team_pts);
      field "oppPts" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : on_court_stats) -> s.ocs_opp_pts);
      field "possessions" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : on_court_stats) -> s.ocs_possessions);
    ])

let off_court_stats_type : (unit, off_court_stats option) S.typ =
  S.(obj "OffCourtStats"
    ~doc:"Team performance when player is off court"
    ~fields:[
      field "games" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : off_court_stats) -> s.ofcs_games);
      field "minutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : off_court_stats) -> s.ofcs_minutes);
      field "teamPts" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : off_court_stats) -> s.ofcs_team_pts);
      field "oppPts" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (s : off_court_stats) -> s.ofcs_opp_pts);
      field "possessions" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (s : off_court_stats) -> s.ofcs_possessions);
    ])

let on_off_impact_type : (unit, on_off_impact option) S.typ =
  S.(obj "OnOffImpact"
    ~doc:"Player on/off court impact analysis"
    ~fields:[
      field "playerId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_player_id);
      field "playerName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_player_name);
      field "teamName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_team_name);
      field "gamesPlayed" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_games_played);
      field "totalMinutes" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_total_minutes);
      field "onCourt" ~typ:(non_null on_court_stats_type)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_on_court);
      field "offCourt" ~typ:(non_null off_court_stats_type)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_off_court);
      field "netRatingOn" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_net_rating_on);
      field "netRatingOff" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_net_rating_off);
      field "netRatingDiff" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_net_rating_diff);
      field "plusMinusTotal" ~typ:(non_null int)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_plus_minus_total);
      field "plusMinusAvg" ~typ:(non_null float)
        ~args:Arg.[]
        ~resolve:(fun _info (o : on_off_impact) -> o.ooi_plus_minus_avg);
    ])

let historical_season_type : (unit, historical_season option) S.typ =
  S.(obj "HistoricalSeason"
    ~doc:"Historical season record with champions and awards"
    ~fields:[
      field "seasonId" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_season_id);
      field "seasonName" ~typ:(non_null string)
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_season_name);
      field "championTeam" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_champion_team);
      field "runnerUp" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_runner_up);
      field "regularMvp" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_regular_mvp);
      field "finalsMvp" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_finals_mvp);
      field "rookieOfYear" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_rookie_of_year);
      field "scoringLeader" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_scoring_leader);
      field "notes" ~typ:string
        ~args:Arg.[]
        ~resolve:(fun _info (s : historical_season) -> s.hs_notes);
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

    (* --- Phase 3: Mobile MVP queries --- *)

    field "upcomingSchedule"
      ~doc:"Upcoming games schedule"
      ~typ:(non_null (list (non_null schedule_entry_type)))
      ~args:Arg.[
        arg "status" ~typ:string ~doc:"Filter by status (default: scheduled)";
        arg "limit" ~typ:int ~doc:"Max results (default: 10)";
      ]
      ~resolve:(fun _info () status limit ->
        let status = match status with Some s -> s | None -> "scheduled" in
        let limit = match limit with Some l -> l | None -> 10 in
        Db.get_upcoming_schedule ~status ~limit () |> resolve_db);

    field "scheduleByDateRange"
      ~doc:"Games in a date range"
      ~typ:(non_null (list (non_null schedule_entry_type)))
      ~args:Arg.[
        arg "startDate" ~typ:(non_null string) ~doc:"Start date (YYYY-MM-DD)";
        arg "endDate" ~typ:(non_null string) ~doc:"End date (YYYY-MM-DD)";
        arg "status" ~typ:string ~doc:"Filter by status (default: ALL)";
      ]
      ~resolve:(fun _info () start_date end_date status ->
        let status = match status with Some s -> s | None -> "ALL" in
        Db.get_schedule_by_date_range ~start_date ~end_date ~status () |> resolve_db);

    field "boxscore"
      ~doc:"Full boxscore for a game"
      ~typ:game_boxscore_type
      ~args:Arg.[
        arg "gameId" ~typ:(non_null string) ~doc:"Game ID";
      ]
      ~resolve:(fun _info () game_id ->
        match Db.get_boxscore ~game_id () with
        | Ok bs -> Some bs
        | Error _ -> None);

    field "gameInfo"
      ~doc:"Game metadata"
      ~typ:game_info_type
      ~args:Arg.[
        arg "gameId" ~typ:(non_null string) ~doc:"Game ID";
      ]
      ~resolve:(fun _info () game_id ->
        match Db.get_game_info ~game_id () with
        | Ok info -> info
        | Error _ -> None);

    field "playerProfile"
      ~doc:"Full player profile with career stats, game logs, and more"
      ~typ:player_profile_type
      ~args:Arg.[
        arg "playerId" ~typ:(non_null string) ~doc:"Player ID";
      ]
      ~resolve:(fun _info () player_id ->
        match Db.get_player_profile ~player_id () with
        | Ok profile -> profile
        | Error _ -> None);

    field "playerGameLogs"
      ~doc:"Player game-by-game statistics"
      ~typ:(non_null (list (non_null player_game_stat_type)))
      ~args:Arg.[
        arg "playerId" ~typ:(non_null string) ~doc:"Player ID";
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "includeMismatch" ~typ:bool ~doc:"Include mismatched scores (default: false)";
      ]
      ~resolve:(fun _info () player_id season include_mismatch ->
        let season = match season with Some s -> s | None -> "ALL" in
        let include_mismatch = match include_mismatch with Some b -> b | None -> false in
        Db.get_player_game_logs ~player_id ~season ~include_mismatch () |> resolve_db);

    field "playerSeasonStats"
      ~doc:"Player season-by-season statistics"
      ~typ:(non_null (list (non_null season_stats_type)))
      ~args:Arg.[
        arg "playerId" ~typ:(non_null string) ~doc:"Player ID";
        arg "scope" ~typ:string ~doc:"totals or per_game (default: per_game)";
      ]
      ~resolve:(fun _info () player_id scope ->
        let scope = match scope with Some s -> s | None -> "per_game" in
        Db.get_player_season_stats ~player_id ~scope () |> resolve_db);

    field "playersByTeam"
      ~doc:"Players filtered by team"
      ~typ:(non_null (list (non_null player_type)))
      ~args:Arg.[
        arg "teamName" ~typ:(non_null string) ~doc:"Team name";
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
        arg "limit" ~typ:int ~doc:"Max results (default: 20)";
      ]
      ~resolve:(fun _info () team_name season limit ->
        let season = match season with Some s -> s | None -> "ALL" in
        let limit = match limit with Some l -> l | None -> 20 in
        Db.get_players_by_team ~team_name ~season ~limit () |> resolve_db);

    field "draftPicks"
      ~doc:"Draft history"
      ~typ:(non_null (list (non_null draft_pick_row_type)))
      ~args:Arg.[
        arg "year" ~typ:int ~doc:"Draft year (0 = all years)";
        arg "search" ~typ:string ~doc:"Search by player name";
      ]
      ~resolve:(fun _info () year search ->
        let year = match year with Some y -> y | None -> 0 in
        let search = match search with Some s -> s | None -> "" in
        Db.get_draft_picks ~year ~search () |> resolve_db);

    field "playerH2H"
      ~doc:"Head-to-head comparison between two players"
      ~typ:h2h_result_type
      ~args:Arg.[
        arg "p1Id" ~typ:(non_null string) ~doc:"Player 1 ID";
        arg "p2Id" ~typ:(non_null string) ~doc:"Player 2 ID";
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
      ]
      ~resolve:(fun _info () p1_id p2_id season ->
        let season = match season with Some s -> s | None -> "ALL" in
        match Db.get_player_h2h_data ~p1_id ~p2_id ~season () with
        | Ok games ->
            let p1_team =
              match games with
              | g :: _ -> g.player1_team
              | [] -> ""
            in
            let summary = Domain.calculate_h2h_summary ~p1_team games in
            Some { hr_games = games; hr_summary = summary }
        | Error _ -> None);

    field "teamH2H"
      ~doc:"Head-to-head game history between two teams"
      ~typ:(non_null (list (non_null game_info_type)))
      ~args:Arg.[
        arg "team1" ~typ:(non_null string) ~doc:"Team 1 code";
        arg "team2" ~typ:(non_null string) ~doc:"Team 2 code";
        arg "season" ~typ:string ~doc:"Season code (default: ALL)";
      ]
      ~resolve:(fun _info () team1 team2 season ->
        let season = match season with Some s -> s | None -> "ALL" in
        Db.get_team_h2h_data ~team1 ~team2 ~season () |> resolve_db);

    field "onOffImpact"
      ~doc:"On/off court impact statistics"
      ~typ:(non_null (list (non_null on_off_impact_type)))
      ~args:Arg.[
        arg "season" ~typ:(non_null string) ~doc:"Season code";
        arg "limit" ~typ:int ~doc:"Max results (default: 50)";
      ]
      ~resolve:(fun _info () season limit ->
        let limit = match limit with Some l -> l | None -> 50 in
        Db.get_on_off_impact_stats ~season ~limit () |> resolve_db);

    field "onOffImpactForPlayer"
      ~doc:"On/off court impact for a specific player"
      ~typ:on_off_impact_type
      ~args:Arg.[
        arg "playerId" ~typ:(non_null string) ~doc:"Player ID";
        arg "season" ~typ:(non_null string) ~doc:"Season code";
      ]
      ~resolve:(fun _info () player_id season ->
        match Db.get_on_off_impact_for_player ~player_id ~season () with
        | Ok impact -> impact
        | Error _ -> None);

    field "legendPlayers"
      ~doc:"WKBL legendary players"
      ~typ:(non_null (list (non_null legend_player_type)))
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        Db.get_legend_players () |> resolve_db);

    field "coaches"
      ~doc:"Current WKBL coaches"
      ~typ:(non_null (list (non_null coach_type)))
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        Db.get_coaches () |> resolve_db);

    field "historicalSeasons"
      ~doc:"Historical season records with champions and awards"
      ~typ:(non_null (list (non_null historical_season_type)))
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        Db.get_historical_seasons () |> resolve_db);
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
