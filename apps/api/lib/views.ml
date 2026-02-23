(** Page view functions for WKBL Analytics — Thin facade.

    Functions are organized in domain-specific modules:
    {ul
      {li {!Views_live} — Live scores, live page}
      {li {!Views_home} — Home page, players table}
      {li {!Views_players} — Players page}
      {li {!Views_standings} — Teams stats, standings}
      {li {!Views_games} — Games, boxscores, play-by-play}
      {li {!Views_compare} — Player/team comparison}
      {li {!Views_predict} — Game prediction}
      {li {!Views_leaders} — Statistical leaders}
      {li {!Views_awards} — Season awards}
      {li {!Views_clutch} — Clutch performance}
    }

    This module re-exports all page functions for backward compatibility
    with existing route modules (Routes_*.ml). *)

(* ── Live ─────────────────────────────────────── *)

let live_scores_widget = Views_live.live_scores_widget
let live_scores_htmx = Views_live.live_scores_htmx
let live_page = Views_live.live_page
let position_leaders_page = Views_live.position_leaders_page

(* ── Home ─────────────────────────────────────── *)

let format_float = Views_home.format_float
let players_table = Views_home.players_table
let home_page = Views_home.home_page

(* ── Players ─────────────────────────────────── *)

let players_page = Views_players.players_page

(* ── Standings ────────────────────────────────── *)

let team_stat_row = Views_standings.team_stat_row
let teams_table = Views_standings.teams_table
let teams_page = Views_standings.teams_page
let standings_table = Views_standings.standings_table
let standings_page = Views_standings.standings_page

(* ── Games ────────────────────────────────────── *)

let games_table = Views_games.games_table
let games_page = Views_games.games_page
let boxscores_table = Views_games.boxscores_table
let boxscores_page = Views_games.boxscores_page
let boxscore_player_table = Views_games.boxscore_player_table
let quarter_flow_section = Views_games.quarter_flow_section
let boxscore_pbp_link_html = Views_games.boxscore_pbp_link_html
let boxscore_flow_link_html = Views_games.boxscore_flow_link_html
let boxscore_disabled_chip_html = Views_games.boxscore_disabled_chip_html
let boxscore_pbp_chip_html = Views_games.boxscore_pbp_chip_html
let boxscore_flow_chip_html = Views_games.boxscore_flow_chip_html
let boxscore_data_notes_html = Views_games.boxscore_data_notes_html
let boxscore_page = Views_games.boxscore_page
let pbp_period_label = Views_games.pbp_period_label
let pbp_page = Views_games.pbp_page

(* ── Compare ──────────────────────────────────── *)

let compare_stat_row = Views_compare.compare_stat_row
let compare_table_empty = Views_compare.compare_table_empty
let compare_table_row = Views_compare.compare_table_row
let compare_table_fragment = Views_compare.compare_table_fragment
let h2h_game_row = Views_compare.h2h_game_row
let h2h_game_table = Views_compare.h2h_game_table
let compare_page = Views_compare.compare_page
let compare_seasons_page = Views_compare.compare_seasons_page

(* ── Predict ──────────────────────────────────── *)

let prediction_result_card = Views_predict.prediction_result_card
let upcoming_games_section = Views_predict.upcoming_games_section
let predict_page = Views_predict.predict_page

(* ── Leaders ──────────────────────────────────── *)

let leader_card = Views_leaders.leader_card
let leader_card_signed = Views_leaders.leader_card_signed
let leaders_page = Views_leaders.leaders_page

(* ── Awards ───────────────────────────────────── *)

let awards_page = Views_awards.awards_page

(* ── Clutch ───────────────────────────────────── *)

let clutch_page = Views_clutch.clutch_page

(* ── Error / Not Found ────────────────────────── *)

let error_page = Views_common.error_page
let not_found_page = Views_common.not_found_page

(* ── Re-exports from existing view modules ────── *)

let team_profile_page = Views_team.team_profile_page
let qa_dashboard_page = Views_tools.qa_dashboard_page
let transactions_page = Views_tools.transactions_page
let history_page = Views_history.history_page
let legends_page = Views_history.legends_page
let coaches_page = Views_history.coaches_page
