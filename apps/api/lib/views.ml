(** Page view functions for WKBL Analytics — Thin facade.

    Functions are organized in domain-specific modules:
    {ul
      {li {!Views_live} — Live scores, live page}
      {li {!Views_home} — Home page, players table}
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

open Domain
open Views_common

(* ── Live ─────────────────────────────────────── *)

let live_scores_widget = Views_live.live_scores_widget
let live_scores_htmx = Views_live.live_scores_htmx
let live_page = Views_live.live_page
let position_leaders_page = Views_live.position_leaders_page

(* ── Home ─────────────────────────────────────── *)

let format_float = Views_home.format_float
let players_table = Views_home.players_table
let home_page = Views_home.home_page

(* ── Players (PR #8: will extract to Views_players) ── *)

let players_page ?(lang=I18n.Ko) ?(player_info_map=None) ~season ~seasons ~search ~sort ~include_mismatch players =
 let season_options =
  let base =
   seasons
   |> List.map (fun (s: season_info) ->
     let selected = if s.code = season then "selected" else "" in
     Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
   |> String.concat "\n"
  in
  Printf.sprintf {html|<option value="ALL" %s>전체 시즌</option>%s|html} (if season = "ALL" then "selected" else "") base
 in
 let sort_value = match String.lowercase_ascii sort with | "pts" | "points" -> "pts" | "mg" | "margin" -> "mg" | "reb" | "rebounds" -> "reb" | "ast" | "assists" -> "ast" | "min" | "minutes" -> "min" | "eff" | "efficiency" -> "eff" | _ -> "eff" in
 let sort_option value label = let selected = if sort_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
 let table = players_table ~lang ~player_info_map players in
 let include_checked = if include_mismatch then "checked" else "" in
 let mg_note =
  if sort_value = "mg" then
	   {html|<details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-600 dark:text-slate-400">
	    <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">MG 안내 (왜 몇 명은 안 보이나요?)</summary>
	    <div class="mt-2 space-y-1 leading-relaxed">
		     <div><span class="font-mono text-slate-900 dark:text-slate-200">MG</span> = (팀 득점 - 상대 득점)의 출전시간 가중 평균입니다.</div>
		     <div>개인 <span class="font-mono text-slate-900 dark:text-slate-200">+/-</span>는 문자중계 기반이며, 데이터가 없거나 문자중계/박스스코어 최종 스코어 불일치 등 품질 문제가 있으면 <span class="font-mono text-slate-900 dark:text-slate-200">-</span>로 표시합니다.</div>
	             <div>표본 안정성을 위해 <span class="font-mono text-slate-900 dark:text-slate-200">MG</span> 정렬 시 <span class="font-mono text-slate-900 dark:text-slate-200">스코어가 있는 경기</span> 기준 총 출전 <span class="font-mono text-slate-900 dark:text-slate-200">100분 이상</span> 선수만 표시합니다.</div>
	            </div>
	           </details>|html}
  else
   ""
 in
 layout ~lang ~title:"WKBL 선수" ~canonical_path:"/players"
  ~description:"WKBL 여자농구 선수 통계 - 효율성, 득점, 리바운드, 어시스트 순위를 시즌별로 비교하세요."
	  ~content:(Printf.sprintf
	   {html|<div class="space-y-6">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">선수</h2><p class="text-slate-600 dark:text-slate-400 text-sm">시즌 기준 선수 기록입니다.</p></div><a class="text-orange-700 dark:text-orange-400 hover:text-orange-700 text-sm" href="/players">초기화</a></div><form id="players-filter" class="grid grid-cols-1 md:grid-cols-4 gap-3" hx-get="/players/table" hx-target="#players-table" hx-trigger="change, keyup delay:250ms"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" name="search" aria-label="선수 검색" placeholder="선수 검색..." value="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none"><select name="sort" aria-label="정렬 기준" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s</select><div class="flex items-center justify-between gap-3 text-xs"><div class="text-slate-600 dark:text-slate-400 flex items-center">정렬: %s</div><label class="flex items-center gap-2 text-slate-600 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="최종 스코어와 득점 합계가 다른 경기 포함"><span>불일치 포함</span></label></div></form>%s<div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 scroll-shadow overflow-y-hidden" data-skeleton="table" data-skeleton-count="15" data-skeleton-cols="8">%s</div></div>|html}
		   (breadcrumb [("홈", "/"); ("선수", "")])
		   season_options
		   (escape_html search)
	   (sort_option "eff" "EFF")
   (sort_option "pts" "PTS")
   (sort_option "mg" "MG")
   (sort_option "reb" "REB")
   (sort_option "ast" "AST")
   (sort_option "min" "MIN")
   (String.uppercase_ascii sort_value)
   include_checked
   mg_note
   table) ()

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

let error_page ?(lang=I18n.Ko) message =
  let tr = I18n.t lang in
  let title = tr { ko = "오류"; en = "Error" } in
  let heading = tr { ko = "문제가 발생했습니다"; en = "Something went wrong" } in
  let body = tr { ko = "잠시 후 다시 시도해주세요."; en = "Please try again later." } in
  let back = tr { ko = "홈으로"; en = "Back to home" } in
  let details_label = tr { ko = "자세히"; en = "Details" } in
  let details_html =
    if String.trim message = "" then
      ""
    else
      Printf.sprintf
        {html|<details class="mt-4 w-full max-w-xl">
          <summary class="cursor-pointer select-none text-xs text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-200">%s</summary>
          <pre class="mt-2 p-3 rounded-lg bg-slate-100 dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 text-[11px] leading-relaxed text-slate-700 dark:text-slate-200 overflow-auto">%s</pre>
        </details>|html}
        (escape_html details_label)
        (escape_html message)
  in
  layout
    ~lang
    ~title
    ~content:
      (Printf.sprintf
         {html|<div class="flex flex-col items-center justify-center py-20 text-center">
           <h2 class="text-xl font-bold text-slate-900 dark:text-slate-200 mb-2">%s</h2>
           <p class="text-slate-600 dark:text-slate-400">%s</p>
           %s
           <a href="/" class="mt-6 inline-flex items-center text-orange-700 dark:text-orange-400 hover:underline">%s</a>
         </div>|html}
         (escape_html heading)
         (escape_html body)
         details_html
         (escape_html back))
    ()

let not_found_page ?(lang=I18n.Ko) ?(what="페이지") () =
  let tr = I18n.t lang in
  let title = tr { ko = "찾을 수 없음"; en = "Not Found" } in
  let heading = tr { ko = Printf.sprintf "%s를 찾을 수 없습니다" what;
                     en = Printf.sprintf "%s not found" what } in
  let body = tr { ko = "요청하신 항목이 존재하지 않거나 삭제되었을 수 있습니다.";
                  en = "The requested item may not exist or has been removed." } in
  let back = tr { ko = "홈으로"; en = "Back to home" } in
  layout
    ~lang
    ~title
    ~description:(Printf.sprintf "WKBL - %s" title)
    ~content:
      (Printf.sprintf
         {html|<div class="flex flex-col items-center justify-center py-20 text-center">
           <div class="text-6xl mb-4" aria-hidden="true">🏀</div>
           <h2 class="text-xl font-bold text-slate-900 dark:text-slate-200 mb-2">%s</h2>
           <p class="text-slate-600 dark:text-slate-400 mb-6">%s</p>
           <a href="/" class="inline-flex items-center gap-2 px-4 py-2 rounded-lg bg-orange-500 text-white hover:bg-orange-600 transition">
             <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"/></svg>
             %s
           </a>
         </div>|html}
         (escape_html heading)
         (escape_html body)
         (escape_html back))
    ()

(* ── Re-exports from existing view modules ────── *)

let team_profile_page = Views_team.team_profile_page
let qa_dashboard_page = Views_tools.qa_dashboard_page
let transactions_page = Views_tools.transactions_page
let history_page = Views_history.history_page
let legends_page = Views_history.legends_page
let coaches_page = Views_history.coaches_page
