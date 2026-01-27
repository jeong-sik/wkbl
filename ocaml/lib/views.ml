(** Page view functions for WKBL Analytics *)
(** This module contains page-specific rendering functions. *)
(** Common helpers are in Views_common module. *)

open Domain
open Views_common

(* Re-export table function from Views_common for external use *)
let players_table = players_table

(** Live scores widget for homepage *)
let live_scores_widget (games: Live.live_game list) =
  if List.length games = 0 then
    empty_state ~icon:BasketballIcon "오늘 경기가 없습니다" "경기 일정이 있는 날 다시 확인해주세요."
  else
    let game_cards = games |> List.map (fun (g: Live.live_game) ->
      let status_badge =
        if g.is_live then
          {html|<span class="live-badge"><span class="live-dot"></span>LIVE</span>|html}
        else
          Printf.sprintf {html|<span class="px-2 py-0.5 rounded-full bg-slate-200 dark:bg-slate-700 text-slate-600 dark:text-slate-300 text-[10px]">%s</span>|html}
            (escape_html g.quarter)
      in
      Printf.sprintf
        {html|<a href="/boxscore/%s" class="flex items-center justify-between gap-3 p-3 bg-slate-50 dark:bg-slate-800/50 rounded-lg hover:bg-slate-100 dark:hover:bg-slate-700/50 transition">
          <div class="flex items-center gap-2">
            <span class="font-medium text-sm text-slate-900 dark:text-slate-200">%s</span>
            <span class="text-xl font-bold text-slate-900 dark:text-slate-200">%d</span>
          </div>
          <div class="flex flex-col items-center gap-1">%s<span class="text-[10px] text-slate-500">vs</span></div>
          <div class="flex items-center gap-2">
            <span class="text-xl font-bold text-slate-900 dark:text-slate-200">%d</span>
            <span class="font-medium text-sm text-slate-900 dark:text-slate-200">%s</span>
          </div>
        </a>|html}
        (escape_html g.game_id)
        (escape_html g.home_team) g.home_score
        status_badge
        g.away_score (escape_html g.away_team)
    ) |> String.concat "\n" in
    Printf.sprintf {html|<div class="space-y-2">%s</div>|html} game_cards

(** HTMX endpoint for live scores widget *)
let live_scores_htmx () =
  let games = Live.get_todays_games () |> List.map Live.game_to_live in
  live_scores_widget games

let home_page ~season ~seasons players =
  let season_options =
    seasons
    |> List.map (fun (s: season_info) ->
        let selected = if s.code = season then "selected" else "" in
        Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
    |> String.concat "\n"
  in
  let table = players_table players in
  let live_games = Live.get_todays_games () |> List.map Live.game_to_live in
  let live_widget = live_scores_widget live_games in
  layout ~title:"WKBL Analytics" ~canonical_path:"/"
    ~description:"WKBL 여자농구 효율성 순위, 팀 순위, 선수 통계를 한눈에 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6">
        <!-- Live Scores Widget -->
        <div class="bg-gradient-to-r from-orange-50 to-amber-50 dark:from-orange-950/30 dark:to-amber-950/30 border border-orange-200 dark:border-orange-800/50 rounded-lg p-4">
          <div class="flex items-center justify-between mb-3">
            <div class="flex items-center gap-2">
              <span class="text-lg">🏀</span>
              <span class="text-xs font-bold text-orange-700 dark:text-orange-300 uppercase tracking-wider">오늘의 경기</span>
            </div>
            <a href="/games" class="text-xs text-orange-600 dark:text-orange-400 hover:text-orange-700 dark:hover:text-orange-300">전체 일정 →</a>
          </div>
          <div id="live-scores" hx-get="/api/live/widget" hx-trigger="every 30s" hx-swap="innerHTML">%s</div>
        </div>
        <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3"><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">Top Players by Efficiency</h2><form class="flex gap-2" hx-get="/home/table" hx-target="#players-table" hx-trigger="change"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" placeholder="Search player..." aria-label="선수 검색" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none" hx-get="/home/table" hx-trigger="keyup changed delay:300ms" hx-target="#players-table" name="search"></form></div><div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 scroll-shadow overflow-y-hidden" data-skeleton="table" data-skeleton-count="10" data-skeleton-cols="8">%s</div></div>|html}
      live_widget season_options table) ()

let players_page ~season ~seasons ~search ~sort ~include_mismatch players =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let sort_value = match String.lowercase_ascii sort with | "pts" | "points" -> "pts" | "mg" | "margin" -> "mg" | "reb" | "rebounds" -> "reb" | "ast" | "assists" -> "ast" | "min" | "minutes" -> "min" | "eff" | "efficiency" -> "eff" | _ -> "eff" in
  let sort_option value label = let selected = if sort_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let table = players_table players in
  let include_checked = if include_mismatch then "checked" else "" in
  let mg_note =
    if sort_value = "mg" then
      {html|<details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-600 dark:text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">MG 안내 (왜 몇 명은 안 보이나요?)</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-900 dark:text-slate-200">MG</span> = (팀 득점 - 상대 득점)의 출전시간 가중 평균입니다.</div>
          <div>개인 <span class="font-mono text-slate-900 dark:text-slate-200">+/-</span>는 문자중계(PBP) 기반이며, 데이터가 없거나 PBP/박스스코어 최종 스코어 불일치 등 품질 이슈가 있으면 <span class="font-mono text-slate-900 dark:text-slate-200">-</span>로 표시합니다.</div>
	          <div>표본 안정성을 위해 <span class="font-mono text-slate-900 dark:text-slate-200">MG</span> 정렬 시 <span class="font-mono text-slate-900 dark:text-slate-200">스코어가 있는 경기</span> 기준 총 출전 <span class="font-mono text-slate-900 dark:text-slate-200">100분 이상</span> 선수만 표시합니다.</div>
	        </div>
	      </details>|html}
    else
      ""
  in
  layout ~title:"WKBL Players" ~canonical_path:"/players"
    ~description:"WKBL 여자농구 선수 통계 - 효율성, 득점, 리바운드, 어시스트 순위를 시즌별로 비교하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Players</h2><p class="text-slate-600 dark:text-slate-400 text-sm">Season-filtered player aggregates.</p></div><a class="text-orange-600 dark:text-orange-400 hover:text-orange-700 text-sm" href="/players">Reset</a></div><form id="players-filter" class="grid grid-cols-1 md:grid-cols-4 gap-3" hx-get="/players/table" hx-target="#players-table" hx-trigger="change, keyup delay:250ms"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" name="search" placeholder="Search player..." value="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none"><select name="sort" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s</select><div class="flex items-center justify-between gap-3 text-xs"><div class="text-slate-600 dark:text-slate-400 flex items-center">Sorted by %s</div><label class="flex items-center gap-2 text-slate-600 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="Final score != sum(points) 경기 포함"><span>Mismatch 포함</span></label></div></form>%s<div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 scroll-shadow overflow-y-hidden" data-skeleton="table" data-skeleton-count="15" data-skeleton-cols="8">%s</div></div>|html}
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

let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

(** Two-line stat cell helper for team tables *)
let team_cell ?(hide="") ?(color="text-slate-700 dark:text-slate-300") label value =
  Printf.sprintf {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">%s</span></div></td>|html}
    hide color value label

(** Team stat row component to avoid too many sprintf arguments *)
let team_stat_row ~season (row: team_stats) =
  let team_href =
    if season = "ALL" then
      Printf.sprintf "/team/%s" (Uri.pct_encode row.team)
    else
      Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode row.team) (Uri.pct_encode season)
  in
  let margin_color = if row.margin >= 0.0 then "text-sky-600 dark:text-sky-400 font-bold" else "text-rose-600 dark:text-rose-400 font-bold" in
  let margin_str = if row.margin > 0.0 then Printf.sprintf "+%.1f" row.margin else format_float row.margin in
  let name_cell = Printf.sprintf {html|<td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 flex items-center gap-2 whitespace-nowrap break-keep w-auto min-w-[140px]"><div class="flex items-center min-w-0">%s<a href="%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors ml-2 truncate">%s</a></div></td>|html} (team_logo_tag ~class_name:"w-5 h-5 shrink-0" row.team) (escape_html team_href) (escape_html row.team) in
  let gp_cell = Printf.sprintf {html|<td class="px-3 py-2 text-right w-14"><span class="text-slate-600 dark:text-slate-400 font-mono">%d</span></td>|html} row.gp in
  
  (* Helper for clean stat cells *)
  let cell ?(hide="") ?(color="text-slate-700 dark:text-slate-300") ?(width="w-18") value =
    Printf.sprintf {html|<td class="px-3 py-2 text-right %s %s"><span class="%s font-mono">%s</span></td>|html} hide width color value
  in

  let cells = String.concat "" [
    cell ~hide:"hidden md:table-cell" ~width:"w-18" (format_float row.min_total);
    cell ~width:"w-18" (format_float row.pts);
    cell ~color:margin_color ~width:"w-18" margin_str;
    cell ~hide:"hidden md:table-cell" ~width:"w-18" (format_float row.pts_against);
    cell ~hide:"hidden sm:table-cell" ~width:"w-18" (format_float row.reb);
    cell ~hide:"hidden sm:table-cell" ~width:"w-18" (format_float row.ast);
    cell ~hide:"hidden md:table-cell" ~width:"w-18" (format_float row.stl);
    cell ~hide:"hidden md:table-cell" ~width:"w-18" (format_float row.blk);
    cell ~hide:"hidden md:table-cell" ~width:"w-18" (format_float row.turnovers);
    cell ~hide:"hidden lg:table-cell" ~width:"w-18" (format_float row.fg_pct);
    cell ~hide:"hidden lg:table-cell" ~width:"w-18" (format_float row.fg3_pct);
    cell ~hide:"hidden lg:table-cell" ~width:"w-18" (format_float row.ft_pct);
    cell ~hide:"hidden lg:table-cell" ~width:"w-18" (format_float row.efg_pct);
    cell ~hide:"hidden lg:table-cell" ~color:"text-emerald-600 dark:text-emerald-400" ~width:"w-18" (format_float row.ts_pct);
    cell ~color:"text-orange-600 dark:text-orange-400 font-bold" ~width:"w-18" (format_float row.eff);
  ] in
  Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">%s%s%s</tr>|html} name_cell gp_cell cells

let teams_table ~season ~scope (stats: team_stats list) =
  let rows =
    stats
    |> List.map (team_stat_row ~season)
    |> String.concat "\n"
  in
  let min_label = if scope = PerGame then "MIN/G" else "MIN" in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden"><table class="w-full min-w-[980px] text-xs sm:text-sm font-mono tabular-nums table-fixed" data-sortable id="teams-table-inner" aria-label="팀 통계 테이블"><thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-3 py-2 text-left w-auto min-w-[140px]">Team</th><th class="px-3 py-2 text-right w-14" data-sortable data-sort-key="gp">GP</th><th class="px-3 py-2 text-right hidden md:table-cell w-18" data-sortable data-sort-key="min">%s</th><th class="px-3 py-2 text-right w-18" data-sortable data-sort-key="pts">PTS</th><th class="px-3 py-2 text-right w-18" data-sortable data-sort-key="mg" title="MG: 팀 득실마진(PTS - PA)">MG</th><th class="px-3 py-2 text-right hidden md:table-cell w-18" data-sortable data-sort-key="pa">PA</th><th class="px-3 py-2 text-right hidden sm:table-cell w-18" data-sortable data-sort-key="reb">REB</th><th class="px-3 py-2 text-right hidden sm:table-cell w-18" data-sortable data-sort-key="ast">AST</th><th class="px-3 py-2 text-right hidden md:table-cell w-18" data-sortable data-sort-key="stl">STL</th><th class="px-3 py-2 text-right hidden md:table-cell w-18" data-sortable data-sort-key="blk">BLK</th><th class="px-3 py-2 text-right hidden md:table-cell w-18" data-sortable data-sort-key="to">TO</th><th class="px-3 py-2 text-right hidden lg:table-cell w-18" data-sortable data-sort-key="fg">FG%%</th><th class="px-3 py-2 text-right hidden lg:table-cell w-18" data-sortable data-sort-key="3p">3P%%</th><th class="px-3 py-2 text-right hidden lg:table-cell w-18" data-sortable data-sort-key="ft">FT%%</th><th class="px-3 py-2 text-right hidden lg:table-cell w-18" data-sortable data-sort-key="efg">eFG%%</th><th class="px-3 py-2 text-right hidden lg:table-cell text-emerald-600 dark:text-emerald-400 w-18" data-sortable data-sort-key="ts" title="True Shooting %%">TS%%</th><th class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 w-18" data-sortable data-sort-key="eff">EFF</th></tr></thead><tbody>%s</tbody></table></div>|html}
    min_label rows

let teams_page ~season ~seasons ~scope ~sort ~include_mismatch stats =
  let scope_value = team_scope_to_string scope in
  let scope_option value label = let selected = if scope_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let sort_option value label = let selected = if String.lowercase_ascii sort = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
  let season_options = let base = seasons |> List.map (fun s -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = teams_table ~season ~scope stats in
  let include_checked = if include_mismatch then "checked" else "" in
  layout ~title:"WKBL Teams" ~canonical_path:"/teams"
    ~description:"WKBL 여자농구 팀 통계 - 6개 구단의 득점, 리바운드, 어시스트 등 시즌별 성적을 비교하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Teams</h2><p class="text-slate-600 dark:text-slate-400 text-sm">Team aggregates by season and scope.</p></div><a class="text-orange-600 dark:text-orange-400 hover:text-orange-700 text-sm" href="/teams">Reset</a></div><form id="teams-filter" class="grid grid-cols-1 md:grid-cols-3 gap-3" hx-get="/teams/table" hx-target="#teams-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><select name="scope" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s</select><select name="sort" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s%s%s%s</select><label class="md:col-span-3 flex items-center justify-end gap-2 text-xs text-slate-600 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="Final score != sum(points) 경기 포함"><span>Mismatch 포함</span></label></form><div id="teams-table" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="12">%s</div><div class="mt-8 grid grid-cols-1 lg:grid-cols-2 gap-6"><div id="teams-shooting-chart" hx-get="/teams/shooting-chart?season=%s" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4 text-slate-400"><span class="htmx-indicator">차트 로딩 중...</span></div></div><div id="teams-radar-chart" hx-get="/teams/radar-chart?season=%s" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4 text-slate-400"><span class="htmx-indicator">차트 로딩 중...</span></div></div></div></div>|html}
      season_options (scope_option "per_game" "Per Game") (scope_option "totals" "Totals") (sort_option "pts" "PTS") (sort_option "reb" "REB") (sort_option "ast" "AST") (sort_option "stl" "STL") (sort_option "blk" "BLK") (sort_option "eff" "EFF") (sort_option "ts_pct" "TS%") (sort_option "fg3_pct" "3P%") (sort_option "min_total" "MIN") include_checked table season season) ()

let standings_table ~season (standings : team_standing list) =
  let rows =
    standings
    |> List.mapi (fun i (s : team_standing) ->
        let win_pct_fmt = Printf.sprintf "%.3f" s.win_pct in
        let gb_fmt = if s.gb = 0.0 then "-" else Printf.sprintf "%.1f" s.gb in
        let team_href =
          if season = "ALL" then
            Printf.sprintf "/team/%s" (Uri.pct_encode s.team_name)
          else
            Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode s.team_name) (Uri.pct_encode season)
        in
        let row_class = if i = 0 then
          "border-b border-slate-200 dark:border-slate-800/60 bg-amber-50/50 dark:bg-amber-900/10 border-l-4 border-l-amber-400"
        else
          "border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/30 transition-colors"
        in
        let rank_class = if i = 0 then
          "px-4 py-3 text-amber-600 dark:text-amber-400 font-mono text-sm font-bold"
        else
          "px-4 py-3 text-slate-600 dark:text-slate-400 font-mono text-sm"
        in
        let diff_color = if s.diff >= 0.0 then "text-emerald-600 dark:text-emerald-400" else "text-rose-600 dark:text-rose-400" in
        let diff_str = if s.diff > 0.0 then Printf.sprintf "+%.1f" s.diff else Printf.sprintf "%.1f" s.diff in
        Printf.sprintf
          {html|<tr class="%s"><td class="%s">%d</td><td class="px-4 py-3 font-bold text-slate-900 dark:text-slate-200" style="width: max-content; white-space: nowrap; word-break: keep-all;"><span class="inline-flex items-center gap-2" style="white-space: nowrap;">%s<a href="%s" class="team-name hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors" style="white-space: nowrap; word-break: keep-all;">%s</a></span></td><td class="px-4 py-3 text-right"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%d</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">GP</span></div></td><td class="px-4 py-3 text-right"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%d</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">W</span></div></td><td class="px-4 py-3 text-right"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%d</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">L</span></div></td><td class="px-4 py-3 text-right"><div class="flex flex-col items-end leading-tight"><span class="text-orange-600 dark:text-orange-400 font-bold font-mono">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">PCT</span></div></td><td class="px-4 py-3 text-right hidden sm:table-cell"><div class="flex flex-col items-end leading-tight"><span class="text-slate-600 dark:text-slate-400 font-mono">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">GB</span></div></td><td class="px-4 py-3 text-right hidden md:table-cell"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">PS/G</span></div></td><td class="px-4 py-3 text-right hidden md:table-cell"><div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%.1f</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">PA/G</span></div></td><td class="px-4 py-3 text-right hidden sm:table-cell"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono font-bold">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">DIFF</span></div></td></tr>|html}
          row_class rank_class (i + 1) (team_logo_tag ~class_name:"w-5 h-5" s.team_name) (escape_html team_href) (escape_html s.team_name) s.games_played s.wins s.losses win_pct_fmt gb_fmt s.avg_pts s.avg_opp_pts diff_color diff_str)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl"><table class="min-w-[560px] sm:min-w-[760px] text-xs sm:text-sm font-mono tabular-nums" data-sortable id="standings-table-inner" aria-label="순위표"><thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-4 py-3 text-left w-12">Rk</th><th class="px-4 py-3 text-left font-sans w-[200px] min-w-[200px]">Team</th><th class="px-4 py-3 text-right" data-sortable data-sort-key="gp">GP</th><th class="px-4 py-3 text-right" data-sortable data-sort-key="w">W</th><th class="px-4 py-3 text-right" data-sortable data-sort-key="l">L</th><th class="px-4 py-3 text-right" data-sortable data-sort-key="pct">PCT</th><th class="px-4 py-3 text-right hidden sm:table-cell" data-sortable data-sort-key="gb">GB</th><th class="px-4 py-3 text-right hidden md:table-cell" data-sortable data-sort-key="ps">PS/G</th><th class="px-4 py-3 text-right hidden md:table-cell" data-sortable data-sort-key="pa">PA/G</th><th class="px-4 py-3 text-right hidden sm:table-cell" data-sortable data-sort-key="diff">DIFF</th></tr></thead><tbody id="standings-body">%s</tbody></table></div>|html}
    rows

let standings_page ~season ~seasons standings =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = standings_table ~season standings in
  layout ~title:"WKBL Standings" ~canonical_path:"/standings"
    ~description:"WKBL 여자농구 순위표 - 시즌별 팀 순위, 승률, 승패 기록을 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Standings</h2><p class="text-slate-600 dark:text-slate-400 text-sm">League rank and win percentage.</p></div></div><form id="standings-filter" class="flex gap-3" hx-get="/standings/table" hx-target="#standings-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="standings-table" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="10">%s</div></div>|html}
      season_options table) ()

let games_table (games : game_summary list) =
  let mobile_cards =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> string_of_int s | None -> "-" in
        let score_b = match g.away_score with Some s -> string_of_int s | None -> "-" in
        let status_class = if g.home_score = None then "text-slate-600 dark:text-slate-400" else "text-slate-900 dark:text-slate-200" in
        let action_html =
          if g.home_score = None then
            {html|<span class="text-[10px] text-slate-600 dark:text-slate-400">TBD</span>|html}
          else
            Printf.sprintf
              {html|<a href="/boxscore/%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">Boxscore</a>|html}
              (escape_html g.game_id)
        in
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 hover:border-orange-300 dark:hover:border-orange-700 rounded-lg p-3 shadow-sm hover:shadow-md space-y-2 transition-all duration-200 cursor-pointer group active:scale-[0.98]" onclick="window.location='/boxscore/%s'">
  <div class="flex items-center justify-between text-[11px] text-slate-600 dark:text-slate-400 font-mono">
    <span>#%d · %s</span>
    %s
  </div>
  <div class="flex items-center justify-between gap-3">
    <div class="flex flex-col gap-1 min-w-0">
      <div class="flex items-center gap-2 text-sm font-medium">%s<a href="/team/%s" class="group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors truncate">%s</a></div>
      <div class="flex items-center gap-2 text-sm font-medium">%s<a href="/team/%s" class="group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors truncate">%s</a></div>
    </div>
    <div class="text-right font-mono text-sm %s whitespace-nowrap group-hover:scale-110 transition-transform">%s - %s</div>
  </div>
</div>|html}
          (escape_html g.game_id)
          (i + 1)
          (escape_html g.game_date)
          action_html
          (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
          (Uri.pct_encode g.home_team)
          (escape_html g.home_team)
          (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
          (Uri.pct_encode g.away_team)
          (escape_html g.away_team)
          status_class
          score_a
          score_b)
    |> String.concat "\n"
  in
  let rows =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> string_of_int s | None -> "-" in
        let score_b = match g.away_score with Some s -> string_of_int s | None -> "-" in
        let status_class = if g.home_score = None then "text-slate-600 dark:text-slate-400" else "text-slate-900 dark:text-slate-200" in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors group %s"><td class="px-4 py-3 text-slate-600 dark:text-slate-400 font-mono text-sm hidden sm:table-cell group-hover:text-slate-900 dark:group-hover:text-slate-200 transition-colors text-center w-14">%d</td><td class="px-4 py-3 text-slate-600 dark:text-slate-400 font-mono text-sm whitespace-nowrap group-hover:text-slate-900 dark:group-hover:text-slate-200 transition-colors w-32">%s</td><td class="px-4 py-3 font-medium text-right w-[160px]"><span class="inline-flex items-center justify-end gap-2 whitespace-nowrap">%s<a href="/team/%s" class="team-name group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors" style="white-space: nowrap;">%s</a></span></td><td class="px-4 py-3 text-center font-bold text-orange-600 dark:text-orange-400 font-mono group-hover:scale-110 transition-transform whitespace-nowrap w-28">%s - %s</td><td class="px-4 py-3 font-medium text-left w-[160px]"><span class="inline-flex items-center gap-2 whitespace-nowrap"><a href="/team/%s" class="team-name group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors" style="white-space: nowrap;">%s</a>%s</span></td><td class="px-4 py-3 text-right w-24"><a href="/boxscore/%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition group-hover:ring-2 group-hover:ring-orange-500/50 whitespace-nowrap">Boxscore</a></td></tr>|html}
          status_class (i + 1) (escape_html g.game_date) (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) score_a score_b (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) (escape_html g.game_id))
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="space-y-3 sm:hidden">%s</div><div class="hidden sm:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl"><table class="min-w-[720px] sm:min-w-[860px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="경기 일정"><thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-4 py-3 text-center hidden sm:table-cell w-14">#</th><th class="px-4 py-3 text-left font-sans w-32">Date</th><th class="px-4 py-3 text-right font-sans w-[160px]">Home</th><th class="px-4 py-3 text-center w-28">Score</th><th class="px-4 py-3 text-left font-sans w-[160px]">Away</th><th class="px-4 py-3 text-right font-sans w-24">Action</th></tr></thead><tbody id="games-body">%s</tbody></table></div>|html}
    mobile_cards rows

let games_page ~season ~seasons games =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = games_table games in
  layout ~title:"WKBL Games" ~canonical_path:"/games"
    ~description:"WKBL 여자농구 경기 일정 - 시즌별 경기 결과와 스케줄을 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Games</h2><p class="text-slate-600 dark:text-slate-400 text-sm">Season schedule and results.</p></div></div><form id="games-filter" class="flex gap-3" hx-get="/games/table" hx-target="#games-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="games-table" data-skeleton="cards" data-skeleton-count="8">%s</div></div>|html}
      season_options table) ()

let boxscores_table (games : game_summary list) =
  let mobile_cards =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> s | None -> 0 in
        let score_b = match g.away_score with Some s -> s | None -> 0 in
        let margin = score_a - score_b in
        let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0" in
        let margin_color = if margin > 0 then "text-sky-600 dark:text-sky-400" else if margin < 0 then "text-rose-600 dark:text-rose-400" else "text-slate-600 dark:text-slate-400" in
        if g.home_score = None then ""
        else
          Printf.sprintf
            {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-3 shadow-sm space-y-2">
  <div class="flex items-center justify-between text-[11px] text-slate-600 dark:text-slate-400 font-mono">
    <span>#%d · %s</span>
    <span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono %s whitespace-nowrap">Δ %s</span>
  </div>
  <div class="flex items-center justify-between gap-3">
    <div class="flex flex-col gap-1 min-w-0 w-full">
      <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
      <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
    </div>
    <a href="/boxscore/%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition">View</a>
  </div>
</div>|html}
            (i + 1)
            (escape_html g.game_date)
            margin_color
            margin_str
            (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
            (Uri.pct_encode g.home_team)
            (escape_html g.home_team)
            score_a
            (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
            (Uri.pct_encode g.away_team)
            (escape_html g.away_team)
            score_b
            (escape_html g.game_id))
    |> String.concat "\n"
  in
  let rows =
    games
    |> List.mapi (fun i (g : game_summary) ->
        let score_a = match g.home_score with Some s -> s | None -> 0 in
        let score_b = match g.away_score with Some s -> s | None -> 0 in
        let margin = score_a - score_b in
        let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0" in
        let margin_color = if margin > 0 then "text-sky-600 dark:text-sky-400 font-bold" else if margin < 0 then "text-rose-600 dark:text-rose-400 font-bold" else "text-slate-600 dark:text-slate-400 font-bold" in
        if g.home_score = None then ""
        else
          Printf.sprintf
            {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-4 py-3 text-slate-600 dark:text-slate-400 font-mono text-sm hidden sm:table-cell text-center w-14">%d</td><td class="px-4 py-3 text-slate-600 dark:text-slate-400 font-mono text-sm whitespace-nowrap w-32">%s</td><td class="px-4 py-3 font-medium text-right w-[160px]"><span class="inline-flex items-center justify-end gap-2 whitespace-nowrap">%s<a href="/team/%s" class="team-name hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a></span></td><td class="px-4 py-3 text-center font-bold text-slate-900 dark:text-slate-200 font-mono w-20">%d</td><td class="px-4 py-3 text-center font-bold text-slate-900 dark:text-slate-200 font-mono w-20">%d</td><td class="px-4 py-3 font-medium text-left w-[160px]"><span class="inline-flex items-center gap-2 whitespace-nowrap"><a href="/team/%s" class="team-name hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors whitespace-nowrap">%s</a>%s</span></td><td class="px-4 py-3 text-right font-mono %s hidden sm:table-cell w-20">%s</td><td class="px-4 py-3 text-right w-20">
                  <a href="/boxscore/%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition whitespace-nowrap">View</a>
                </td></tr>|html}
            (i + 1) (escape_html g.game_date) (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) score_a score_b (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) margin_color margin_str (escape_html g.game_id))
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="space-y-3 sm:hidden">%s</div><div class="hidden sm:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-2xl"><table class="min-w-[720px] sm:min-w-[900px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="박스스코어 목록"><thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider whitespace-nowrap"><tr><th class="px-4 py-3 text-center hidden sm:table-cell w-14">#</th><th class="px-4 py-3 text-left font-sans w-32">Date</th><th class="px-4 py-3 text-right font-sans w-[160px]">Home</th><th class="px-4 py-3 text-center w-20 font-sans">PTS</th><th class="px-4 py-3 text-center w-20 font-sans">PTS</th><th class="px-4 py-3 text-left font-sans w-[160px]">Away</th><th class="px-4 py-3 text-right font-sans hidden sm:table-cell w-20">Margin</th><th class="px-4 py-3 text-right font-sans w-20">Link</th></tr></thead><tbody id="boxscores-body">%s</tbody></table></div>|html}
    mobile_cards rows

let boxscores_page ~season ~seasons games =
  let season_options = let base = seasons |> List.map (fun (s: season_info) -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base in
  let table = boxscores_table games in
  layout ~title:"WKBL Boxscores" ~canonical_path:"/boxscores"
    ~description:"WKBL 여자농구 경기 박스스코어 - 경기별 개인 기록과 팀 통계를 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Boxscores</h2><p class="text-slate-600 dark:text-slate-400 text-sm">Game results and margins.</p></div></div><form id="boxscores-filter" class="flex gap-3" hx-get="/boxscores/table" hx-target="#boxscores-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="boxscores-table" data-skeleton="cards" data-skeleton-count="10">%s</div></div>|html}
      season_options table) ()

let boxscore_player_table (title: string) (players: boxscore_player_stat list) =
  (* Dedupe: if the same stat line is duplicated due to name matching, keep 1 row. *)
  let players =
    let key_of (p: boxscore_player_stat) =
      let minutes_key = int_of_float (floor (p.bs_minutes *. 10.0 +. 0.5)) in
      Printf.sprintf
        "%s|%s|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d|%d"
        (normalize_name p.bs_player_name)
        (String.trim p.bs_team_code)
        minutes_key
        p.bs_pts
        p.bs_reb
        p.bs_ast
        p.bs_stl
        p.bs_blk
        p.bs_tov
        p.bs_fg_made
        p.bs_fg_att
        p.bs_fg3_made
        p.bs_fg3_att
        p.bs_ft_made
        p.bs_ft_att
    in
    let quality_score (p: boxscore_player_stat) =
      let pm_score = if Option.is_some p.bs_plus_minus then 2 else 0 in
      let pos_score =
        match p.bs_position with
        | Some pos when String.trim pos <> "" -> 1
        | _ -> 0
      in
      pm_score + pos_score
    in
    let best_by_key : (string, int * boxscore_player_stat) Hashtbl.t = Hashtbl.create 64 in
    players
    |> List.iteri (fun idx p ->
        let key = key_of p in
        match Hashtbl.find_opt best_by_key key with
        | None -> Hashtbl.add best_by_key key (idx, p)
        | Some (best_idx, best_p) ->
            let s_new = quality_score p in
            let s_best = quality_score best_p in
            if s_new > s_best then
              Hashtbl.replace best_by_key key (idx, p)
            else if s_new = s_best && idx < best_idx then
              Hashtbl.replace best_by_key key (idx, p)
            else
              ())
    ;
    best_by_key
    |> Hashtbl.to_seq_values
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  let name_counts : (string, int) Hashtbl.t = Hashtbl.create 32 in
  players
  |> List.iter (fun (p: boxscore_player_stat) ->
      let key = normalize_name p.bs_player_name in
      let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
      Hashtbl.replace name_counts key (prev + 1));
  let rows =
    players
    |> List.map (fun (p: boxscore_player_stat) ->
        (* Clean plus/minus string *)
        let pm_str, pm_cls = match p.bs_plus_minus with
          | Some v when v > 0 -> (Printf.sprintf "+%d" v, "text-sky-600 dark:text-sky-400 font-bold")
          | Some v when v < 0 -> (Printf.sprintf "%d" v, "text-rose-600 dark:text-rose-400 font-bold")
          | Some 0 -> ("0", "text-slate-500 dark:text-slate-500")
          | None -> ("-", "text-slate-400 dark:text-slate-600")
          | _ -> ("0", "text-slate-500 dark:text-slate-500")
        in
        
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors font-mono tabular-nums">
            <td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 flex items-center gap-3 min-w-[160px] font-sans">
              %s
              <div class="flex flex-col min-w-0">
                <a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a>
                <span class="text-[10px] text-slate-500 dark:text-slate-400 font-normal">%s</span>
              </div>
            </td>
            <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 text-sm w-16 hidden sm:table-cell">%.1f</td>
            <td class="px-3 py-2 text-right text-slate-900 dark:text-slate-200 font-bold w-16">%d</td>
            <td class="px-3 py-2 text-right w-16 %s hidden sm:table-cell">%s</td>
            <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-16">%d</td>
            <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-16">%d</td>
            <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-16 hidden md:table-cell">%d</td>
            <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-16 hidden md:table-cell">%d</td>
            <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-16 hidden md:table-cell">%d</td>
            <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 text-xs w-28 hidden lg:table-cell">%d-%d (%.1f%%)</td>
            <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 text-xs w-28 hidden lg:table-cell">%d-%d (%.1f%%)</td>
            <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 text-xs w-28 hidden lg:table-cell">%d-%d (%.1f%%)</td>
          </tr>|html}
          (player_img_tag ~class_name:"w-8 h-8 rounded-full object-cover bg-slate-100 dark:bg-slate-800" p.bs_player_id p.bs_player_name)
          p.bs_player_id
          (escape_html (normalize_name p.bs_player_name))
          (escape_html (Option.value ~default:"-" p.bs_position))
          p.bs_minutes
          p.bs_pts
          pm_cls
          (escape_html pm_str)
          p.bs_reb
          p.bs_ast
          p.bs_stl
          p.bs_blk
          p.bs_tov
          p.bs_fg_made p.bs_fg_att p.bs_fg_pct
          p.bs_fg3_made p.bs_fg3_att p.bs_fg3_pct
          p.bs_ft_made p.bs_ft_att p.bs_ft_pct)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="space-y-3"><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 flex items-center gap-2"><span class="w-1 h-6 bg-orange-500 rounded-full"></span>%s</h3><div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-lg"><table class="min-w-[560px] sm:min-w-[720px] lg:min-w-[980px] w-full text-sm font-mono table-auto" aria-label="팀별 경기 스탯"><thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-xs uppercase tracking-wider"><tr><th class="px-3 py-2 text-left font-sans">Player</th><th class="px-3 py-2 text-right w-16 hidden sm:table-cell">MP</th><th class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 w-16">PTS</th><th class="px-3 py-2 text-right w-16 hidden sm:table-cell">+/-</th><th class="px-3 py-2 text-right w-16">TRB</th><th class="px-3 py-2 text-right w-16">AST</th><th class="px-3 py-2 text-right w-16 hidden md:table-cell">STL</th><th class="px-3 py-2 text-right w-16 hidden md:table-cell">BLK</th><th class="px-3 py-2 text-right w-16 hidden md:table-cell">TOV</th><th class="px-3 py-2 text-right w-28 hidden lg:table-cell">FG</th><th class="px-3 py-2 text-right w-28 hidden lg:table-cell">3P</th><th class="px-3 py-2 text-right w-28 hidden lg:table-cell">FT</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
    (escape_html title) rows

(** Render quarter flow section for boxscore page *)
let quarter_flow_section ~home_name ~away_name (quarters: quarter_score list) =
  if quarters = [] then "" else
  let rows = List.mapi (fun i q ->
    let prev_home = if i = 0 then 0 else (List.nth quarters (i-1)).qs_home_score in
    let prev_away = if i = 0 then 0 else (List.nth quarters (i-1)).qs_away_score in
    let home_q = q.qs_home_score - prev_home in
    let away_q = q.qs_away_score - prev_away in
    let diff = home_q - away_q in
    let flow_indicator =
      if diff > 5 then Printf.sprintf {html|<span class="text-sky-500">🔵 +%d</span>|html} diff
      else if diff > 0 then Printf.sprintf {html|<span class="text-sky-400">▲ +%d</span>|html} diff
      else if diff < -5 then Printf.sprintf {html|<span class="text-orange-500">🔴 %d</span>|html} diff
      else if diff < 0 then Printf.sprintf {html|<span class="text-orange-400">▼ %d</span>|html} diff
      else {html|<span class="text-slate-400">━</span>|html}
    in
    let period_label = match q.qs_period with
      | "Q1" -> "1Q" | "Q2" -> "2Q" | "Q3" -> "3Q" | "Q4" -> "4Q"
      | "X1" -> "OT1" | "X2" -> "OT2" | p -> p
    in
    Printf.sprintf
      {html|<tr class="border-b border-slate-200 dark:border-slate-700/50">
        <td class="px-3 py-2 font-bold text-slate-700 dark:text-slate-300">%s</td>
        <td class="px-3 py-2 text-right font-mono text-sky-600 dark:text-sky-400">%d</td>
        <td class="px-3 py-2 text-right font-mono text-sky-500 dark:text-sky-500">%d</td>
        <td class="px-3 py-2 text-center text-slate-500 dark:text-slate-400">vs</td>
        <td class="px-3 py-2 text-right font-mono text-orange-600 dark:text-orange-400">%d</td>
        <td class="px-3 py-2 text-right font-mono text-orange-500 dark:text-orange-500">%d</td>
        <td class="px-3 py-2 text-center">%s</td>
      </tr>|html}
      period_label q.qs_home_score home_q q.qs_away_score away_q flow_indicator
  ) quarters in
  Printf.sprintf
    {html|<div class="max-w-2xl mx-auto bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800 p-4">
      <div class="flex items-center gap-2 mb-3">
        <span class="text-lg">📊</span>
        <span class="text-xs font-bold text-slate-700 dark:text-slate-300 uppercase tracking-wider">쿼터별 흐름</span>
      </div>
      <div class="overflow-x-auto">
        <table class="w-full text-sm">
          <thead class="text-xs uppercase text-slate-500 dark:text-slate-400 border-b border-slate-200 dark:border-slate-700">
            <tr>
              <th class="px-3 py-2 text-left">QTR</th>
              <th class="px-3 py-2 text-right text-sky-600 dark:text-sky-400" colspan="2">%s</th>
              <th class="px-3 py-2 text-center"></th>
              <th class="px-3 py-2 text-right text-orange-600 dark:text-orange-400" colspan="2">%s</th>
              <th class="px-3 py-2 text-center">흐름</th>
            </tr>
            <tr class="text-[10px]">
              <th></th>
              <th class="px-3 py-1 text-right">누적</th>
              <th class="px-3 py-1 text-right">Q득점</th>
              <th></th>
              <th class="px-3 py-1 text-right">누적</th>
              <th class="px-3 py-1 text-right">Q득점</th>
              <th></th>
            </tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>
    </div>|html}
    (escape_html home_name) (escape_html away_name) (String.concat "" rows)

let boxscore_page (bs: game_boxscore) =
  let gi = bs.boxscore_game in
  let margin = gi.gi_home_score - gi.gi_away_score in
  let margin_str =
    if margin > 0 then Printf.sprintf "+%d" margin else if margin < 0 then Printf.sprintf "%d" margin else "0"
  in
  let margin_class =
    if margin > 0 then "text-sky-600 dark:text-sky-400"
    else if margin < 0 then "text-orange-600 dark:text-orange-400"
    else "text-slate-600 dark:text-slate-400"
  in
  let margin_badge =
    Printf.sprintf
      {html|<span class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider %s">MARGIN %s</span>|html}
      margin_class
      (escape_html margin_str)
  in
  let quality_badge = score_quality_badge gi.gi_score_quality in
  let official_link =
    match wkbl_official_game_result_url gi.gi_game_id with
    | None -> ""
    | Some url ->
        Printf.sprintf
          {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
          (escape_html url)
  in
  let pbp_link =
    Printf.sprintf
      {html|<a href="/boxscore/%s/pbp" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">PBP</a>|html}
      (escape_html gi.gi_game_id)
  in
  let flow_link =
    Printf.sprintf
      {html|<a href="/boxscore/%s/flow" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">FLOW</a>|html}
      (escape_html gi.gi_game_id)
  in
  let data_notes =
    Printf.sprintf
      {html|<details class="max-w-2xl mx-auto bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-xs text-slate-600 dark:text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">출처 / 검증 기준</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-900 dark:text-slate-200">MARGIN</span>은 경기 최종 득점(팀-상대)입니다.</div>
          <div class="pt-1 text-slate-600 dark:text-slate-400 font-bold">출처</div>
          <div>• 스코어: WKBL 공식 경기 결과 페이지(<span class="font-mono text-slate-700 dark:text-slate-300">/game/result.asp</span>) %s</div>
          <div>• 박스스코어: WKBL 공식 박스스코어(AJAX) → <span class="font-mono text-slate-700 dark:text-slate-300">game_stats</span></div>
          <div>• 개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>: WKBL 문자중계(PBP) → <span class="font-mono text-slate-700 dark:text-slate-300">player_plus_minus</span> (일부 경기만)</div>
          <div class="pt-1 text-slate-600 dark:text-slate-400 font-bold">검증 기준</div>
          <div>• <span class="font-mono text-slate-900 dark:text-slate-200">VERIFIED (✓)</span>: <span class="font-mono text-slate-700 dark:text-slate-300">games.home/away_score</span>가 있고, 양 팀 <span class="font-mono text-slate-700 dark:text-slate-300">SUM(game_stats.pts)</span>와 모두 일치</div>
          <div>• <span class="font-mono text-slate-900 dark:text-slate-200">DERIVED (Σ)</span>: 스코어/합계 누락으로 교차검증 불가. 표시 스코어는 <span class="font-mono text-slate-700 dark:text-slate-300">COALESCE(games score, sum pts)</span></div>
          <div>• <span class="font-mono text-slate-900 dark:text-slate-200">MISMATCH (!)</span>: 스코어와 합계가 모두 있는데 값이 다름 (<a href="/qa" class="text-orange-600 dark:text-orange-400 hover:underline">QA</a>)</div>
          <div class="pt-1">※ 이 검증은 “최종 득점”만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 검증이 필요합니다.</div>
          <div>※ 동명이인 매칭 오류로 동일 스탯 라인이 중복될 수 있어, 동일 라인은 1개만 표시합니다.</div>
        </div>
      </details>|html}
      official_link
  in
  let home_table = boxscore_player_table gi.gi_home_team_name bs.boxscore_home_players in
  let away_table = boxscore_player_table gi.gi_away_team_name bs.boxscore_away_players in
  (* Quarter flow from PBP data *)
  let quarters = match Db.get_quarter_scores gi.gi_game_id with
    | Ok qs -> qs
    | Error _ -> []
  in
  let quarter_section = quarter_flow_section ~home_name:gi.gi_home_team_name ~away_name:gi.gi_away_team_name quarters in
  (* AI Game Summary *)
  let ai_summary = Ai.generate_game_summary bs in
  let ai_summary_section =
    Printf.sprintf
      {html|<div class="max-w-2xl mx-auto bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 border border-indigo-200 dark:border-indigo-800/50 rounded-lg p-4">
        <div class="flex items-center gap-2 mb-2">
          <span class="text-lg">📰</span>
          <span class="text-xs font-bold text-indigo-700 dark:text-indigo-300 uppercase tracking-wider">AI 경기 요약</span>
        </div>
        <p class="text-sm text-slate-700 dark:text-slate-300 leading-relaxed">%s</p>
      </div>|html}
      (escape_html ai_summary)
  in
  layout ~title:(Printf.sprintf "Boxscore: %s vs %s" gi.gi_home_team_name gi.gi_away_team_name)
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-2xl"><div class="flex flex-col items-center gap-6"><div class="text-slate-600 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div><div class="flex items-center justify-between w-full max-w-2xl gap-2 sm:gap-6"><div class="flex flex-col items-center gap-2 sm:gap-3 flex-shrink-0"><div class="text-sm sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-1 sm:gap-3">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a></div><div class="text-[10px] sm:text-sm text-slate-600 dark:text-slate-400">HOME</div></div><div class="flex items-center gap-2 sm:gap-8"><div class="text-3xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%d</div><div class="flex flex-col items-center gap-1 sm:gap-2"><div class="text-base sm:text-2xl text-slate-700 dark:text-slate-300 font-light">vs</div><div class="flex flex-wrap items-center justify-center gap-1 sm:gap-2">%s%s%s%s</div></div><div class="text-3xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%d</div></div><div class="flex flex-col items-center gap-2 sm:gap-3 flex-shrink-0"><div class="text-sm sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-1 sm:gap-3"><a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a>%s</div><div class="text-[10px] sm:text-sm text-slate-600 dark:text-slate-400">AWAY</div></div></div></div></div>%s%s%s<div class="grid grid-cols-1 gap-8">%s%s</div><div class="flex justify-center"><a href="/games" class="text-slate-600 dark:text-slate-400 hover:text-orange-500 transition text-sm">← Back to Games</a></div></div>|html}
      (escape_html gi.gi_game_date)
      (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_home_team_name) (Uri.pct_encode gi.gi_home_team_name) (escape_html gi.gi_home_team_name)
      gi.gi_home_score
      margin_badge
      quality_badge
      pbp_link
      flow_link
      gi.gi_away_score
      (Uri.pct_encode gi.gi_away_team_name) (escape_html gi.gi_away_team_name) (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_away_team_name)
      ai_summary_section
      quarter_section
      data_notes
      home_table away_table) ()

let pbp_period_label = function
  | "Q1" -> "1Q"
  | "Q2" -> "2Q"
  | "Q3" -> "3Q"
  | "Q4" -> "4Q"
  | "X1" -> "OT1"
  | "X2" -> "OT2"
  | "X3" -> "OT3"
  | "X4" -> "OT4"
  | p -> p

let pbp_page ~(game: game_info) ~(periods: string list) ~(selected_period: string) ~(events: pbp_event list) =
  let official_link =
    match wkbl_official_game_result_url game.gi_game_id with
    | None -> ""
    | Some url ->
        Printf.sprintf
          {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
          (escape_html url)
  in
  let tabs =
    match periods with
    | [] -> ""
    | _ ->
        periods
        |> List.map (fun p ->
            let is_selected = p = selected_period in
            let cls =
              if is_selected then "bg-orange-500/15 border-orange-500/30 text-orange-700"
              else "bg-slate-100 dark:bg-slate-800/40 border-slate-300 dark:border-slate-700/40 text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white"
            in
            Printf.sprintf
              {html|<a href="/boxscore/%s/pbp?period=%s" class="px-3 py-1.5 rounded-full border text-xs font-mono tracking-wider transition %s">%s</a>|html}
              (escape_html game.gi_game_id)
              (Uri.pct_encode p)
              cls
              (escape_html (pbp_period_label p)))
        |> String.concat "\n"
        |> fun s -> Printf.sprintf {html|<div class="flex flex-wrap items-center justify-center gap-2">%s</div>|html} s
  in
  let rows =
    events
    |> List.map (fun (e: pbp_event) ->
        let team_name, side_label =
          match e.pe_team_side with
          | 1 -> (game.gi_away_team_name, "AWAY")
          | 2 -> (game.gi_home_team_name, "HOME")
          | _ -> ("", "—")
        in
        let side_badge =
          match e.pe_team_side with
          | 1 | 2 ->
              Printf.sprintf
                {html|<div class="flex items-center gap-2">%s<span class="text-[10px] font-mono text-slate-600 dark:text-slate-400 tracking-wider">%s</span></div>|html}
                (team_badge ~max_width:"max-w-[110px] sm:max-w-[140px]" team_name)
                (escape_html side_label)
          | _ ->
              {html|<div class="flex items-center gap-2"><div class="w-6 h-6 rounded bg-slate-100 dark:bg-slate-800 flex items-center justify-center text-xs">🏀</div><span class="text-[10px] font-mono text-slate-600 dark:text-slate-400 tracking-wider">—</span></div>|html}
        in
        let score_badge =
          match e.pe_team1_score, e.pe_team2_score with
          | Some a, Some h ->
              Printf.sprintf
                {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap" title="AWAY %s / HOME %s">A %d - %d H</span>|html}
                (escape_html game.gi_away_team_name)
                (escape_html game.gi_home_team_name)
                a
                h
          | _ -> ""
        in
        let is_score = score_badge <> "" in
        let row_cls =
          if is_score then "bg-slate-100 dark:bg-slate-800/15"
          else "hover:bg-slate-100 dark:hover:bg-slate-800/50"
        in
        Printf.sprintf
          {html|<li class="flex items-start gap-3 px-4 py-3 border-b border-slate-200 dark:border-slate-800/60 last:border-0 %s transition-colors">
            <div class="w-[56px] text-right text-xs font-mono text-slate-600 dark:text-slate-400 pt-1">%s</div>
            <div class="min-w-0 flex-1">
              <div class="flex flex-wrap items-center gap-2">
                %s
                %s
                <span class="text-sm text-slate-900 dark:text-slate-200 break-words">%s</span>
              </div>
            </div>
          </li>|html}
          row_cls
          (escape_html e.pe_clock)
          side_badge
          score_badge
          (escape_html e.pe_description))
    |> String.concat "\n"
  in
  let body =
    match periods with
    | [] ->
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-slate-600 dark:text-slate-400 text-sm">
            <div class="font-bold text-slate-900 dark:text-slate-200 mb-2">PBP 데이터가 없습니다</div>
            <div class="leading-relaxed">
              WKBL 문자중계(PBP)가 제공되지 않았거나 아직 수집되지 않은 경기입니다.
              개인 <span class="font-mono text-slate-900 dark:text-slate-200">+/-</span> 계산도 PBP 기반이라 이 경기에는 표시되지 않을 수 있습니다.
            </div>
          </div>|html}
    | _ ->
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">%s<ul class="divide-y divide-slate-800/60">%s</ul></div>|html}
          tabs
          rows
  in
  layout
    ~title:(Printf.sprintf "PBP: %s vs %s" game.gi_home_team_name game.gi_away_team_name)
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-4">
          <div class="space-y-2">
            <div class="text-slate-600 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div>
            <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Play-by-Play</h2>
            <div class="flex flex-wrap items-center gap-2 text-sm">
              %s
              <span class="text-slate-600">vs</span>
              %s
              <span class="text-slate-600 dark:text-slate-400 font-mono">%d - %d</span>
            </div>
          </div>
          <div class="flex items-center gap-3">
            %s
            <a href="/boxscore/%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">← Boxscore</a>
          </div>
        </div>
        <div class="text-xs text-slate-600 dark:text-slate-400 leading-relaxed">
          <span class="font-mono text-slate-900 dark:text-slate-200">team1/team2</span> 스코어는 WKBL 문자중계(XML) 기준이며, 일반적으로 <span class="font-mono text-slate-900 dark:text-slate-200">A(원정)</span> / <span class="font-mono text-slate-900 dark:text-slate-200">H(홈)</span> 순으로 표시됩니다.
        </div>
        %s
      </div>|html}
      (escape_html game.gi_game_date)
      (team_badge ~max_width:"max-w-[160px]" game.gi_home_team_name)
      (team_badge ~max_width:"max-w-[160px]" game.gi_away_team_name)
      game.gi_home_score
      game.gi_away_score
      official_link
      (escape_html game.gi_game_id)
      body)
    ()

let compare_stat_row ?(signed=false) label val1 val2 =
  let max_val = max (abs_float val1) (abs_float val2) in
  let pct1 = if max_val = 0.0 then 0.0 else abs_float val1 /. max_val *. 100.0 in
  let pct2 = if max_val = 0.0 then 0.0 else abs_float val2 /. max_val *. 100.0 in
  let value_str v =
    if signed && v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v
  in
  Printf.sprintf
    {html|<div class="flex flex-col gap-1"><div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-600 dark:text-slate-400"><span>%s</span><span class="text-slate-600 dark:text-slate-400">%s</span><span>%s</span></div><div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden"><div class="flex justify-end w-1/2 border-r border-slate-300 dark:border-slate-700"><div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div><div class="flex justify-start w-1/2"><div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div></div></div>|html}
    (escape_html (value_str val1)) (escape_html label) (escape_html (value_str val2)) pct1 pct2

let h2h_game_row (g: h2h_game) =
  let diff_color = if g.score_diff > 0 then "bg-orange-100 dark:bg-orange-900/30 text-orange-600 dark:text-orange-400"
                   else if g.score_diff < 0 then "bg-sky-100 dark:bg-sky-900/30 text-sky-600 dark:text-sky-400"
                   else "bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400" in
  Printf.sprintf
    {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 font-mono transition-colors group">
      <td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-xs w-[100px] truncate group-hover:text-slate-900 dark:group-hover:text-slate-200 transition-colors">%s</td>
      <td class="px-3 py-2 text-right text-slate-900 dark:text-slate-200 w-[60px] font-bold">%d</td>
      <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-center text-slate-600 dark:text-slate-400 text-xs font-sans w-[40px]">vs</td>
      <td class="px-3 py-2 text-left text-slate-900 dark:text-slate-200 w-[60px] font-bold">%d</td>
      <td class="px-3 py-2 text-left text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-left text-slate-700 dark:text-slate-300 w-[60px]">%d</td>
      <td class="px-3 py-2 text-right text-xs font-sans w-[80px]"><span class="px-1.5 py-0.5 rounded %s font-bold">%+d</span></td>
    </tr>|html}
    (escape_html g.game_date) g.player1_pts g.player1_reb g.player1_ast g.player2_pts g.player2_reb g.player2_ast diff_color g.score_diff

let h2h_game_table (p1_name: string) (p2_name: string) (games: h2h_game list) =
  let rows = games |> List.map h2h_game_row |> String.concat "\n" in
  Printf.sprintf {html|<div class="space-y-3 mt-8"><h3 class="text-center text-slate-600 dark:text-slate-400 text-sm font-bold uppercase tracking-widest">Match History</h3><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-xl"><table class="w-full text-sm table-fixed" aria-label="선수 맞대결 기록"><thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase text-[10px] tracking-tighter"><tr><th class="px-3 py-2 text-left font-sans w-[100px]">Date</th><th class="px-3 py-2 text-right font-sans text-orange-600 dark:text-orange-400 w-[60px]">%s PTS</th><th class="px-3 py-2 text-right font-sans w-[60px]">REB</th><th class="px-3 py-2 text-right font-sans w-[60px]">AST</th><th class="px-3 py-2 w-[40px]"></th><th class="px-3 py-2 text-left font-sans text-sky-600 dark:text-sky-400 w-[60px]">%s PTS</th><th class="px-3 py-2 text-left font-sans w-[60px]">REB</th><th class="px-3 py-2 text-left font-sans w-[60px]">AST</th><th class="px-3 py-2 w-[80px]"></th></tr></thead><tbody>%s</tbody></table></div></div>|html} (escape_html p1_name) (escape_html p2_name) rows

let compare_page
    ~season
    ~seasons
    ~p1_season
    ~p2_season
    ~p1_query
    ~p2_query
    ~p1_id
    ~p2_id
    ~p1_candidates
    ~p2_candidates
    ~error
    ~h2h_disabled_reason
    ~p1_season_history:(p1_season_history: season_stats list)
    ~p2_season_history:(p2_season_history: season_stats list)
    (p1_selected: player_aggregate option)
    (p2_selected: player_aggregate option)
    (h2h: h2h_game list)
  =
  let season_options ~selected =
    let base =
      seasons
      |> List.map (fun (s : season_info) ->
          let is_selected = if s.code = selected then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code is_selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<option value="ALL" %s>All Seasons</option>%s|html}
      (if selected = "ALL" then "selected" else "")
      base
  in
  let season_label code =
    if code = "ALL" then "All Seasons"
    else
      seasons
      |> List.find_opt (fun (s: season_info) -> s.code = code)
      |> Option.map (fun (s: season_info) -> s.name)
      |> Option.value ~default:code
  in
  let season_badge code =
    Printf.sprintf
      {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-600 dark:text-slate-400 whitespace-nowrap">%s</span>|html}
      (escape_html (season_label code))
  in
  let query_display (query : string) (selected : player_aggregate option) =
    if String.trim query <> "" then query
    else
      match selected with
      | None -> ""
      | Some p -> normalize_name p.name
  in
  let p1_display = query_display p1_query p1_selected in
  let p2_display = query_display p2_query p2_selected in
  let compare_url ~season ~p1_season ~p2_season ~p1 ~p2 ~p1_id ~p2_id =
    let params =
      [ ("season", season); ("p1_season", p1_season); ("p2_season", p2_season); ("p1", p1); ("p2", p2) ]
      @ (match p1_id with None -> [] | Some id -> [ ("p1_id", id) ])
      @ (match p2_id with None -> [] | Some id -> [ ("p2_id", id) ])
    in
    let encode (k, v) = k ^ "=" ^ Uri.pct_encode v in
    "/compare?" ^ String.concat "&" (List.map encode params)
  in
  let selected_card ~accent_border ~season_code (p : player_aggregate) =
    let mp =
      if p.games_played <= 0 then 0.0 else p.total_minutes /. float_of_int p.games_played
    in
    let id_badge_html = Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge p.player_id) in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg border-t-4 %s">
        <div class="flex items-center gap-4">
          %s
          <div class="min-w-0">
            <div class="text-lg font-black text-slate-900 dark:text-slate-200 truncate">
              <a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors">%s</a>%s
            </div>
            <div class="mt-1 text-xs text-slate-600 dark:text-slate-400 flex flex-wrap items-center gap-2">
              <span class="truncate">%s</span>
              %s
              <span class="font-mono">GP %d</span>
              <span class="font-mono">MP %.1f</span>
              <span class="font-mono">EFF %.1f</span>
            </div>
          </div>
        </div>
        <div class="mt-3 text-[11px] text-slate-600 dark:text-slate-400">이름을 다시 입력하면 선택이 초기화됩니다.</div>
      </div>|html}
      accent_border
      (player_img_tag ~class_name:"w-14 h-14" p.player_id p.name)
      p.player_id
      (escape_html (normalize_name p.name))
      id_badge_html
      (escape_html p.team_name)
      (season_badge season_code)
      p.games_played
      mp
      p.efficiency
  in
  let candidate_row ~accent_btn_class ~slot (c : player_aggregate) =
    let mp =
      if c.games_played <= 0 then 0.0 else c.total_minutes /. float_of_int c.games_played
    in
    let p1_for_link, p2_for_link, p1_id_for_link, p2_id_for_link =
      match slot with
      | `P1 ->
          ( normalize_name c.name,
            p2_display,
            Some c.player_id,
            p2_id )
      | `P2 ->
          ( p1_display,
            normalize_name c.name,
            p1_id,
            Some c.player_id )
    in
    let url =
      compare_url
        ~season
        ~p1_season
        ~p2_season
        ~p1:p1_for_link
        ~p2:p2_for_link
        ~p1_id:p1_id_for_link
        ~p2_id:p2_id_for_link
    in
    let id_badge_html = Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge c.player_id) in
    Printf.sprintf
      {html|<div class="flex items-center justify-between gap-3 py-2 border-b border-slate-200 dark:border-slate-800/60 last:border-0">
        <div class="flex items-center gap-3 min-w-0">
          %s
          <div class="min-w-0">
            <div class="text-sm font-medium text-slate-700 dark:text-slate-300 truncate">
              <a href="%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors">%s</a>%s
            </div>
            <div class="mt-0.5 text-[11px] text-slate-600 dark:text-slate-400 flex flex-wrap items-center gap-2">
              <span class="truncate">%s</span>
              <span class="font-mono">GP %d</span>
              <span class="font-mono">MP %.1f</span>
              <span class="font-mono">EFF %.1f</span>
            </div>
          </div>
        </div>
        <a href="%s" class="shrink-0 px-3 py-1 rounded-lg border border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800/60 text-xs font-bold %s hover:brightness-125 transition">Select</a>
      </div>|html}
      (player_img_tag ~class_name:"w-10 h-10" c.player_id c.name)
      (escape_html url)
      (escape_html (normalize_name c.name))
      id_badge_html
      (escape_html c.team_name)
      c.games_played
      mp
      c.efficiency
      (escape_html url)
      accent_btn_class
  in
  let candidates_panel ~title ~slot ~accent_btn_class (query : string) (candidates : player_aggregate list) =
    let body =
      if String.trim query = "" then
        {html|<div class="text-slate-600 dark:text-slate-400 text-sm">이름을 입력해 검색하세요.</div>|html}
      else if candidates = [] then
        {html|<div class="text-slate-600 dark:text-slate-400 text-sm">검색 결과가 없습니다.</div>|html}
      else
        candidates |> List.map (candidate_row ~accent_btn_class ~slot) |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
        <div class="flex items-center justify-between mb-3">
          <div class="text-xs font-bold uppercase tracking-wider text-slate-600 dark:text-slate-400">%s</div>
          <div class="text-[10px] text-slate-600 font-mono">ID 선택</div>
        </div>
        <div class="space-y-1">%s</div>
      </div>|html}
      (escape_html title)
      body
  in
  let error_html =
    match error with
    | None -> ""
    | Some msg ->
        Printf.sprintf
          {html|<div class="bg-rose-500/10 border border-rose-500/30 text-rose-700 dark:text-rose-400 rounded-xl p-5">%s</div>|html}
          (escape_html msg)
  in
  let compare_result_html =
    match p1_selected, p2_selected with
    | Some a, Some b ->
        let h2h_html =
          match h2h_disabled_reason with
          | Some msg ->
              Printf.sprintf
                {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-600 dark:text-slate-400">%s</div>|html}
                (escape_html msg)
          | None ->
              if h2h = [] then
                {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-600 dark:text-slate-400">No match history for this season.</div>|html}
              else
                let p1_n = normalize_name a.name in
                let p2_n = normalize_name b.name in
                let match_history = h2h_game_table p1_n p2_n h2h in
                let h2h_advanced = Views_tools.h2h_advanced_section ~p1_name:p1_n ~p2_name:p2_n h2h in
                match_history ^ h2h_advanced
        in
        (* Radar chart for visual stat comparison *)
        let radar_chart_html =
          let labels = ["PTS"; "REB"; "AST"; "STL"; "BLK"; "EFF"] in
          let values_a = [
            normalize_stat_for_radar `Points a.avg_points;
            normalize_stat_for_radar `Rebounds a.avg_rebounds;
            normalize_stat_for_radar `Assists a.avg_assists;
            normalize_stat_for_radar `Steals a.avg_steals;
            normalize_stat_for_radar `Blocks a.avg_blocks;
            normalize_stat_for_radar `Efficiency a.efficiency;
          ] in
          let values_b = [
            normalize_stat_for_radar `Points b.avg_points;
            normalize_stat_for_radar `Rebounds b.avg_rebounds;
            normalize_stat_for_radar `Assists b.avg_assists;
            normalize_stat_for_radar `Steals b.avg_steals;
            normalize_stat_for_radar `Blocks b.avg_blocks;
            normalize_stat_for_radar `Efficiency b.efficiency;
          ] in
          Printf.sprintf
            {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6">
              <div class="text-center mb-4">
                <h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">Visual Comparison</h3>
                <div class="flex justify-center gap-6 mt-2 text-xs">
                  <div class="flex items-center gap-2">
                    <span class="w-3 h-3 rounded-full bg-orange-500"></span>
                    <span class="text-slate-600 dark:text-slate-400">%s</span>
                  </div>
                  <div class="flex items-center gap-2">
                    <span class="w-3 h-3 rounded-full bg-sky-500"></span>
                    <span class="text-slate-600 dark:text-slate-400">%s</span>
                  </div>
                </div>
              </div>
              %s
            </div>|html}
            (escape_html (normalize_name a.name))
            (escape_html (normalize_name b.name))
            (radar_chart ~show_league_avg:true ~labels ~values_a ~values_b ~color_a:"#f97316" ~color_b:"#0ea5e9" ())
        in
        (* Season trend line chart - shows PTS progression across seasons *)
        let trend_chart_html =
          let has_history = List.length p1_season_history > 1 || List.length p2_season_history > 1 in
          if not has_history then ""
          else
            let max_pts =
              let p1_max = p1_season_history |> List.map (fun (s: season_stats) -> s.ss_avg_points) |> List.fold_left max 0.0 in
              let p2_max = p2_season_history |> List.map (fun (s: season_stats) -> s.ss_avg_points) |> List.fold_left max 0.0 in
              max 15.0 (max p1_max p2_max *. 1.1)
            in
            let width = 400 in
            let height = 200 in
            let padding = 40 in
            let chart_w = width - padding * 2 in
            let chart_h = height - padding * 2 in
            let all_seasons =
              (List.map (fun (s: season_stats) -> s.ss_season_code) p1_season_history @
               List.map (fun (s: season_stats) -> s.ss_season_code) p2_season_history)
              |> List.sort_uniq compare
              |> List.rev (* Most recent on right *)
            in
            let n_seasons = List.length all_seasons in
            if n_seasons < 2 then ""
            else
              let x_of_season s =
                match List.find_opt (fun x -> x = s) all_seasons with
                | None -> padding
                | Some _ ->
                    let idx = match List.find_index (fun x -> x = s) all_seasons with
                      | Some i -> i | None -> 0 in
                    padding + (chart_w * idx / (n_seasons - 1))
              in
              let y_of_pts pts = padding + int_of_float ((1.0 -. pts /. max_pts) *. float_of_int chart_h) in
              let make_line color data =
                let points =
                  data
                  |> List.map (fun (s: season_stats) ->
                      let x = x_of_season s.ss_season_code in
                      let y = y_of_pts s.ss_avg_points in
                      Printf.sprintf "%d,%d" x y)
                  |> String.concat " "
                in
                if List.length data < 2 then ""
                else Printf.sprintf {svg|<polyline points="%s" fill="none" stroke="%s" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/>|svg} points color
              in
              let make_dots color data =
                data
                |> List.map (fun (s: season_stats) ->
                    let x = x_of_season s.ss_season_code in
                    let y = y_of_pts s.ss_avg_points in
                    Printf.sprintf {svg|<circle cx="%d" cy="%d" r="4" fill="%s"/><title>%s: %.1f PTS</title>|svg} x y color s.ss_season_name s.ss_avg_points)
                |> String.concat "\n"
              in
              let x_labels =
                all_seasons
                |> List.mapi (fun i s ->
                    let x = padding + (chart_w * i / (n_seasons - 1)) in
                    let label = if String.length s >= 3 then String.sub s (String.length s - 2) 2 else s in
                    Printf.sprintf {svg|<text x="%d" y="%d" text-anchor="middle" class="fill-slate-500 dark:fill-slate-400 text-[10px]">%s</text>|svg} x (height - 10) label)
                |> String.concat "\n"
              in
              let y_labels =
                [0.0; max_pts /. 2.0; max_pts]
                |> List.map (fun v ->
                    let y = y_of_pts v in
                    Printf.sprintf {svg|<text x="%d" y="%d" text-anchor="end" class="fill-slate-400 dark:fill-slate-500 text-[9px]">%.0f</text><line x1="%d" y1="%d" x2="%d" y2="%d" stroke="currentColor" class="text-slate-200 dark:text-slate-700" stroke-dasharray="2,2"/>|svg}
                      (padding - 5) (y + 3) v padding y (width - padding) y)
                |> String.concat "\n"
              in
              Printf.sprintf
                {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6 mt-6">
                  <div class="text-center mb-4">
                    <h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">Season Trend (PTS)</h3>
                    <p class="text-[10px] text-slate-400 dark:text-slate-500 mt-1">시즌별 평균 득점 추이</p>
                  </div>
                  <div class="flex justify-center">
                    <svg viewBox="0 0 %d %d" class="w-full max-w-md">
                      %s
                      %s
                      %s
                      %s
                      %s
                    </svg>
                  </div>
                </div>|html}
                width height
                y_labels
                x_labels
                (make_line "#f97316" p1_season_history)
                (make_line "#0ea5e9" p2_season_history)
                ((make_dots "#f97316" p1_season_history) ^ "\n" ^ (make_dots "#0ea5e9" p2_season_history))
        in
        Printf.sprintf
          {html|<div class="space-y-8 animate-fade-in"><div class="grid grid-cols-1 md:grid-cols-3 gap-8 items-start"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-orange-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400"><a href="/player/%s">%s</a></div><div class="text-slate-600 dark:text-slate-400">%s</div>%s</div></div><div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6 space-y-6"><div class="text-center space-y-2"><h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">Average Stats</h3><p class="text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed"><span class="font-mono">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 +/-는 문자중계(PBP) 기반으로 일부 경기에서만 제공됩니다. (데이터가 없으면 -)</p></div>%s%s%s%s%s%s%s%s</div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-sky-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-sky-600 dark:text-sky-400"><a href="/player/%s">%s</a></div><div class="text-slate-600 dark:text-slate-400">%s</div>%s</div></div></div>%s%s%s</div>|html}
          (player_img_tag ~class_name:"w-32 h-32" a.player_id a.name)
          a.player_id
          (escape_html (normalize_name a.name))
          (escape_html a.team_name)
          (season_badge p1_season)
          (compare_stat_row "Points" a.avg_points b.avg_points)
          (compare_stat_row ~signed:true "MG" a.avg_margin b.avg_margin)
          (compare_stat_row "Rebounds" a.avg_rebounds b.avg_rebounds)
          (compare_stat_row "Assists" a.avg_assists b.avg_assists)
          (compare_stat_row "Steals" a.avg_steals b.avg_steals)
          (compare_stat_row "Blocks" a.avg_blocks b.avg_blocks)
          (compare_stat_row "Turnovers" a.avg_turnovers b.avg_turnovers)
          (compare_stat_row "Efficiency" a.efficiency b.efficiency)
          (player_img_tag ~class_name:"w-32 h-32" b.player_id b.name)
          b.player_id
          (escape_html (normalize_name b.name))
          (escape_html b.team_name)
          (season_badge p2_season)
          radar_chart_html
          trend_chart_html
          h2h_html
    | _ -> ""
  in
  layout ~title:"WKBL Compare" ~canonical_path:"/compare"
    ~description:"WKBL 여자농구 선수 비교 - 두 선수의 스탯을 레이더 차트로 비교하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-8">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
          <div>
            <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Compare Players</h2>
            <p class="text-slate-600 dark:text-slate-400 text-sm">동명이인/표기 이슈를 피하기 위해 <span class="font-mono text-slate-900 dark:text-slate-200">player_id</span>를 선택해 비교합니다.</p>
            <p class="text-slate-600 dark:text-slate-400 text-xs mt-1">Player 1/2는 시즌을 각각 선택할 수 있습니다. 시즌이 다르면 Match History는 표시하지 않습니다.</p>
          </div>
          <button id="share-compare-btn" type="button" onclick="shareCompareUrl()" class="hidden md:flex items-center gap-2 px-4 py-2 rounded-lg border border-slate-300 dark:border-slate-600 text-slate-600 dark:text-slate-400 hover:bg-slate-100 dark:hover:bg-slate-800 transition text-sm font-medium" aria-label="비교 URL 복사">
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z"></path></svg>
            <span>Share</span>
          </button>
        </div>

        <form action="/compare" method="get" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
          <input type="hidden" id="p1_id" name="p1_id" value="%s">
          <input type="hidden" id="p2_id" name="p2_id" value="%s">
          <div class="grid grid-cols-1 md:grid-cols-6 gap-3">
            <select name="p1_season" aria-label="Player 1 season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-2">%s</select>
            <input name="p1" value="%s" placeholder="Player 1 (search name)" oninput="document.getElementById('p1_id').value='';" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-4">
            <select name="p2_season" aria-label="Player 2 season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-2">%s</select>
            <input name="p2" value="%s" placeholder="Player 2 (search name)" oninput="document.getElementById('p2_id').value='';" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-4">
            <div class="md:col-span-6 flex justify-end">
              <button type="submit" class="px-4 py-2 rounded-lg bg-orange-500 text-slate-900 dark:text-slate-200 font-bold hover:bg-orange-400 transition">Search</button>
            </div>
          </div>
        </form>

        %s

        <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
          %s
          %s
        </div>

        %s
      </div>|html}
      (escape_html (Option.value ~default:"" p1_id))
      (escape_html (Option.value ~default:"" p2_id))
      (season_options ~selected:p1_season)
      (escape_html p1_display)
      (season_options ~selected:p2_season)
      (escape_html p2_display)
      error_html
      (match p1_selected with
      | Some p -> selected_card ~accent_border:"border-t-orange-500" ~season_code:p1_season p
      | None -> candidates_panel ~title:"Player 1" ~slot:`P1 ~accent_btn_class:"text-orange-700" p1_display p1_candidates)
      (match p2_selected with
      | Some p -> selected_card ~accent_border:"border-t-sky-500" ~season_code:p2_season p
      | None -> candidates_panel ~title:"Player 2" ~slot:`P2 ~accent_btn_class:"text-sky-700" p2_display p2_candidates)
      compare_result_html) ()

let prediction_result_card ~(home: string) ~(away: string) (output: prediction_output) =
  let pct value = value *. 100.0 in
  let result = output.result in
  let breakdown = output.breakdown in
  let home_pct = pct result.prob_a in
  let away_pct = pct result.prob_b in
  let elo_home_pct = pct breakdown.pb_elo_prob in
  let elo_away_pct = pct (1.0 -. breakdown.pb_elo_prob) in
  let pyth_home_pct = pct breakdown.pb_pyth_prob in
  let pyth_away_pct = pct (1.0 -. breakdown.pb_pyth_prob) in
  let stats_home_pct = pct breakdown.pb_stats_prob in
  let stats_away_pct = pct (1.0 -. breakdown.pb_stats_prob) in
  (* Generate AI explanation *)
  let ai_explanation = Ai.get_explanation ~home ~away output in
  let context_card_html, context_note_html =
    match breakdown.pb_context with
    | None ->
        ("",
          {html|<div>기본 모델(Elo/Pythag(Log5)/Stats)만 사용합니다.</div>
          <div class="text-slate-600 dark:text-slate-400">컨텍스트 옵션을 켜면 “최근 5경기 폼/코어 로스터/휴식”을 Δ로 소폭 반영합니다. (부상/전술/PBP 등은 미반영)</div>|html})
    | Some ctx ->
        let delta_pp = ctx.pcb_delta *. 100.0 in
        let delta_cls =
          if delta_pp > 0.0 then "text-sky-600 dark:text-sky-400"
          else if delta_pp < 0.0 then "text-rose-600 dark:text-rose-400"
          else "text-slate-700 dark:text-slate-300"
        in
        let delta_str =
          if delta_pp > 0.0 then Printf.sprintf "+%.1f%%p" delta_pp else Printf.sprintf "%.1f%%p" delta_pp
        in
        let form_home_pct = pct ctx.pcb_form_home in
        let form_away_pct = pct ctx.pcb_form_away in
        let roster_text =
          match ctx.pcb_roster_home, ctx.pcb_roster_away with
          | Some h, Some a ->
              Printf.sprintf "%d/%d vs %d/%d" h.rcs_present h.rcs_total a.rcs_present a.rcs_total
          | _ -> "-"
        in
        let rest_text =
          match ctx.pcb_rest_home_days, ctx.pcb_rest_away_days with
          | Some h, Some a -> Printf.sprintf "%dd vs %dd" h a
          | _ -> "-"
        in
        ( Printf.sprintf
            {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
              <div class="flex items-center justify-between">
                <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">CONTEXT</div>
                <div class="text-[10px] font-mono %s">%s</div>
              </div>
              <div class="mt-2 space-y-1 text-[10px] text-slate-600 dark:text-slate-400 font-mono">
                <div class="flex justify-between"><span>FORM (L5 win%%)</span><span class="text-slate-700 dark:text-slate-300">%.0f%% vs %.0f%%</span></div>
                <div class="flex justify-between"><span>ROSTER (core)</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
                <div class="flex justify-between"><span>REST</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
              </div>
            </div>|html}
            delta_cls
            (escape_html delta_str)
            form_home_pct
            form_away_pct
            (escape_html roster_text)
            (escape_html rest_text),
          {html|<div>컨텍스트(최근 5경기 폼/코어 로스터/휴식)를 Δ로 소폭 반영합니다. (최대 ±8%p)</div><div class="text-slate-600 dark:text-slate-400">로스터는 “마지막 경기 출전” 기준으로 추정하며, 부상/전술/PBP 컨텍스트는 여전히 미반영입니다.</div>|html}
        )
  in
  let winner_class =
    if normalize_label result.winner = normalize_label home then "text-orange-600 dark:text-orange-400"
    else "text-sky-600 dark:text-sky-400"
  in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl space-y-4">
      <div class="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-4">
        <div class="min-w-0">
          <div class="text-sm text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">Prediction</div>
          <div class="mt-1 text-2xl font-black text-slate-900 dark:text-slate-200">%s</div>
          <div class="mt-1 text-xs text-slate-600 dark:text-slate-400 truncate">%s%s vs %s%s</div>
        </div>
        <div class="text-right shrink-0">
          <div class="text-xs text-slate-600 dark:text-slate-400">Winner</div>
          <div class="text-lg font-black %s">%s</div>
        </div>
      </div>
      <div class="space-y-2">
        <div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-600 dark:text-slate-400">
          <span class="text-slate-700 dark:text-slate-300">%s</span>
          <span class="text-slate-700 dark:text-slate-300">%s</span>
        </div>
        <div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
          <div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div>
          <div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div>
        </div>
        <div class="flex justify-between font-mono text-sm">
          <span class="text-orange-600 dark:text-orange-400 font-bold">%.1f%%</span>
          <span class="text-sky-600 dark:text-sky-400 font-bold">%.1f%%</span>
        </div>
      </div>
      <!-- AI Analysis -->
      <div class="bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 border border-indigo-200 dark:border-indigo-800/50 rounded-lg p-4">
        <div class="flex items-center gap-2 mb-2">
          <span class="text-lg">🤖</span>
          <span class="text-xs font-bold text-indigo-700 dark:text-indigo-300 uppercase tracking-wider">AI 분석</span>
        </div>
        <p class="text-sm text-slate-700 dark:text-slate-300 leading-relaxed">%s</p>
      </div>
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-3 text-xs">
        <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
          <div class="flex items-center justify-between">
            <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">ELO</div>
            <div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono">%d games</div>
          </div>
          <div class="mt-2 flex items-center justify-between font-mono">
            <div class="text-orange-700">%.0f</div>
            <div class="text-sky-700">%.0f</div>
          </div>
          <div class="mt-1 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
            <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
          </div>
        </div>
        <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
          <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">PYTHAG</div>
          <div class="mt-2 flex items-center justify-between font-mono">
            <div class="text-orange-700">%.3f</div>
            <div class="text-sky-700">%.3f</div>
          </div>
          <div class="mt-1 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
            <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
          </div>
        </div>
        <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
          <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">STATS</div>
          <div class="mt-2 flex items-center justify-between font-mono font-bold">
            <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
            <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
          </div>
          <div class="mt-1 text-[10px] text-slate-600 dark:text-slate-400 font-mono">Win%% + EFF blend</div>
        </div>
        %s
      </div>
      <details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-600 dark:text-slate-400">
        <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">예측 안내</summary>
        <div class="mt-2 space-y-1 leading-relaxed">
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Final</span> = 0.60×Elo + 0.25×Pythag(Log5) + 0.15×Stats.</div>
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Elo</span>는 경기 결과(득점차 반영)로 레이팅을 업데이트합니다.</div>
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Pythag</span>는 득실 기반 기대 승률이며 Log5로 매치업 확률을 계산합니다.</div>
          <div><span class="font-mono text-slate-900 dark:text-slate-200">Stats</span>는 승률과 EFF를 단순 결합합니다.</div>
          <div>중립경기(Neutral)면 홈 어드밴티지는 예측에 반영하지 않습니다.</div>
          %s
        </div>
      </details>
    </div>|html}
    (escape_html (Printf.sprintf "%.1f%% - %.1f%%" home_pct away_pct))
    (escape_html home)
    (if breakdown.pb_is_neutral then " (Neutral)" else " (Home)")
    (escape_html away)
    (if breakdown.pb_is_neutral then " (Neutral)" else " (Away)")
    winner_class
    (escape_html result.winner)
    (escape_html home)
    (escape_html away)
    home_pct
    away_pct
    home_pct
    away_pct
    (escape_html ai_explanation)
    breakdown.pb_games_used
    breakdown.pb_elo_home
    breakdown.pb_elo_away
    elo_home_pct
    elo_away_pct
    breakdown.pb_pyth_home
    breakdown.pb_pyth_away
    pyth_home_pct
    pyth_away_pct
    stats_home_pct
    stats_away_pct
    context_card_html
    context_note_html

let upcoming_games_section (upcoming: Domain.schedule_entry list) =
  if upcoming = [] then ""
  else
    let game_card (g: Domain.schedule_entry) =
      let home_name = Option.value g.sch_home_team_name ~default:g.sch_home_team_code in
      let away_name = Option.value g.sch_away_team_name ~default:g.sch_away_team_code in
      let time_str = match g.sch_game_time with
        | Some t -> Printf.sprintf " %s" t
        | None -> ""
      in
      Printf.sprintf
        {html|<a href="/predict?home=%s&away=%s" class="block bg-slate-800/50 hover:bg-slate-700/50 border border-slate-700 hover:border-orange-500/50 rounded-lg p-3 transition-all group">
          <div class="text-xs text-slate-400 mb-1">%s%s</div>
          <div class="flex items-center justify-between">
            <span class="text-sm font-medium text-slate-200 group-hover:text-orange-400">%s</span>
            <span class="text-xs text-slate-500">vs</span>
            <span class="text-sm font-medium text-slate-200 group-hover:text-orange-400">%s</span>
          </div>
        </a>|html}
        (escape_html home_name) (escape_html away_name)
        (escape_html g.sch_game_date) time_str
        (escape_html home_name) (escape_html away_name)
    in
    let cards = upcoming |> List.map game_card |> String.concat "\n" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 mb-6">
        <h3 class="text-sm font-bold text-slate-600 dark:text-slate-400 uppercase tracking-wider mb-3">📅 Upcoming Games</h3>
        <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-2">%s</div>
      </div>|html}
      cards

let predict_page ~season ~seasons ~teams ~home ~away ~is_neutral ~context_enabled ~include_mismatch ~upcoming (result: prediction_output option) (error: string option) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s : season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let team_option current name =
    let selected = if normalize_label current = normalize_label name then "selected" else "" in
    Printf.sprintf {html|<option value="%s" %s>%s</option>|html} (escape_html name) selected (escape_html name)
  in
  let team_options current =
    let base = teams |> List.map (team_option current) |> String.concat "\n" in
    Printf.sprintf {html|<option value="" %s>Select team…</option>%s|html} (if String.trim current = "" then "selected" else "") base
  in
  let result_html =
    match result, error with
    | Some r, _ -> prediction_result_card ~home ~away r
    | None, Some msg ->
        Printf.sprintf
          {html|<div class="bg-rose-500/10 border border-rose-500/30 text-rose-700 dark:text-rose-400 rounded-xl p-5">%s</div>|html}
          (escape_html msg)
    | None, None ->
        {html|<div class="text-slate-600 dark:text-slate-400 text-sm">Select teams to see a prediction.</div>|html}
  in
  let upcoming_html = upcoming_games_section upcoming in
  layout ~title:"WKBL Predict" ~canonical_path:"/predict"
    ~description:"WKBL 여자농구 경기 예측 - AI 기반 승률 예측과 분석을 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
          <div>
            <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Match Prediction</h2>
            <p class="text-slate-600 dark:text-slate-400 text-sm">농구광 모드: Elo + Pythagorean + Stats 근거를 함께 보여줍니다.</p>
          </div>
        </div>
        %s
        <form action="/predict" method="get" class="grid grid-cols-1 md:grid-cols-3 gap-3 bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
          <select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <select name="home" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <select name="away" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
            <input type="checkbox" name="context" value="1" class="accent-orange-500" %s>
            컨텍스트 반영 (폼/로스터/휴식)
          </label>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
            <input type="checkbox" name="neutral" value="1" class="accent-orange-500" %s>
            Neutral site (no home advantage)
          </label>
          <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
            <input type="checkbox" name="include_mismatch" value="1" class="accent-orange-500" %s>
            Mismatch 포함 (스코어 불일치 경기 포함)
          </label>
          <div class="md:col-span-3 flex justify-end">
            <button type="submit" class="bg-orange-500 hover:bg-orange-400 text-black font-bold px-4 py-2 rounded text-sm transition">Predict</button>
          </div>
        </form>
        %s
      </div>|html}
      upcoming_html
      season_options
      (team_options home)
      (team_options away)
      (if context_enabled then "checked" else "")
      (if is_neutral then "checked" else "")
      (if include_mismatch then "checked" else "")
      result_html) ()

(** Podium-style leader card (Gemini UX feedback: 1-2-3위 시각적 강조) *)
let leader_card ?(value_fmt=(fun v -> Printf.sprintf "%.1f" v)) title (leaders: leader_entry list) =
  if leaders = [] then ""
  else
    let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
    leaders |> List.iter (fun (l: leader_entry) ->
        let key = normalize_name l.le_player_name in
        Hashtbl.replace name_counts key ((Hashtbl.find_opt name_counts key |> Option.value ~default:0) + 1));
    let show_id (l: leader_entry) =
      Hashtbl.find_opt name_counts (normalize_name l.le_player_name) |> Option.map (fun c -> c > 1) |> Option.value ~default:false
    in
    (* Split into podium (1-3) and rest (4+) *)
    let top3, rest = match leaders with
      | a :: b :: c :: tl -> ([a; b; c], tl)
      | a :: b :: tl -> ([a; b], tl)
      | a :: tl -> ([a], tl)
      | [] -> ([], [])
    in
    (* Podium item: rank 1=gold/large, 2=silver/left, 3=bronze/right *)
    let podium_item rank (l: leader_entry) =
      let (bg, ring, size, mt, hover_scale) = match rank with
        | 1 -> ("bg-amber-400/20", "ring-amber-400", "w-14 h-14", "mt-0", "hover:scale-110")
        | 2 -> ("bg-slate-300/20", "ring-slate-400", "w-11 h-11", "mt-4", "hover:scale-105")
        | _ -> ("bg-amber-700/20", "ring-amber-700", "w-11 h-11", "mt-6", "hover:scale-105")
      in
      let id_badge = if show_id l then player_id_badge l.le_player_id else "" in
      Printf.sprintf
        {html|<a href="/player/%s" class="flex flex-col items-center %s group cursor-pointer transition-transform duration-200 %s">
          <div class="relative"><div class="%s rounded-full %s ring-2 overflow-hidden group-hover:ring-4 transition-all duration-200">%s</div>
            <div class="absolute -bottom-1 left-1/2 -translate-x-1/2 %s text-xs font-bold w-5 h-5 rounded-full flex items-center justify-center text-white group-hover:scale-110 transition-transform">%d</div>
          </div>
          <div class="text-lg font-bold text-slate-900 dark:text-slate-200 mt-2 group-hover:text-orange-500 transition-colors">%s</div>
          <span class="text-xs text-slate-600 dark:text-slate-400 truncate max-w-[80px]">%s</span>%s
        </a>|html}
        l.le_player_id mt hover_scale size bg (player_img_tag ~class_name:(size ^ " object-cover") l.le_player_id l.le_player_name)
        ring rank
        (escape_html (value_fmt l.le_stat_value))
        (escape_html (normalize_name l.le_player_name)) id_badge
    in
    (* Podium layout: 2nd | 1st | 3rd *)
    let podium_html = match top3 with
      | [first] -> Printf.sprintf {html|<div class="flex justify-center mb-4">%s</div>|html} (podium_item 1 first)
      | [first; second] ->
          Printf.sprintf {html|<div class="flex justify-center items-end gap-4 mb-4">%s%s</div>|html}
            (podium_item 2 second) (podium_item 1 first)
      | [first; second; third] ->
          Printf.sprintf {html|<div class="flex justify-center items-end gap-3 mb-4">%s%s%s</div>|html}
            (podium_item 2 second) (podium_item 1 first) (podium_item 3 third)
      | _ -> ""
    in
    (* 4th and beyond as compact list *)
    let others_rows =
      rest |> List.mapi (fun i l ->
          let id_badge = if show_id l then Printf.sprintf {html|<span class="ml-1">%s</span>|html} (player_id_badge l.le_player_id) else "" in
          Printf.sprintf {html|<a href="/player/%s" class="flex items-center justify-between py-1.5 border-b border-slate-200/50 dark:border-slate-800/40 last:border-0 hover:bg-slate-100 dark:hover:bg-slate-800/50 -mx-2 px-2 rounded transition-colors group">
            <div class="flex items-center gap-2">
              <span class="text-slate-500 dark:text-slate-500 font-mono text-xs w-4 group-hover:text-orange-500 transition-colors">%d</span>
              %s
              <span class="text-xs text-slate-700 dark:text-slate-300 group-hover:text-orange-500 truncate transition-colors">%s</span>%s
            </div>
            <span class="font-mono text-xs text-slate-600 dark:text-slate-400 group-hover:font-bold transition-all">%s</span>
          </a>|html}
            l.le_player_id
            (i + 4) (player_img_tag ~class_name:"w-6 h-6" l.le_player_id l.le_player_name)
            (escape_html (normalize_name l.le_player_name)) id_badge
            (escape_html (value_fmt l.le_stat_value)))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg hover:shadow-xl hover:border-orange-300 dark:hover:border-orange-700 transition-all duration-300 card-enter">
        <h3 class="text-slate-500 dark:text-slate-400 font-bold uppercase tracking-wider text-[10px] mb-3 text-center">%s</h3>
        %s
        <div class="space-y-0">%s</div>
      </div>|html}
      (escape_html title) podium_html others_rows

(** Signed value format for +/- stats (reuses podium style) *)
let leader_card_signed title leaders =
  let signed v = if v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v in
  leader_card ~value_fmt:signed title leaders

let leaders_page ~season ~seasons ~scope (leaders_by_category: (string * leader_entry list) list) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<option value="ALL" %s>All Seasons</option>%s|html}
      (if season = "ALL" then "selected" else "")
      base
  in
  let scope_value = scope |> String.trim |> String.lowercase_ascii in
  let scope_options =
    let opt v label =
      let sel = if scope_value = v then "selected" else "" in
      Printf.sprintf {html|<option value="%s" %s>%s</option>|html} v sel label
    in
    opt "per_game" "Per Game" ^ opt "totals" "Totals" ^ opt "per_36" "Per 36"
  in
  let lookup category =
    leaders_by_category
    |> List.find_opt (fun (k, _) -> k = category)
    |> Option.map snd
    |> Option.value ~default:[]
  in
  let fmt_int v = Printf.sprintf "%.0f" v in
  let fmt_f1 v = Printf.sprintf "%.1f" v in
  let fmt_f3 v = Printf.sprintf "%.3f" v in
  let main_categories, shooting_categories =
    match scope_value with
    | "totals" ->
        ( [ ("Games", "gp", fmt_int)
          ; ("Minutes", "min", fmt_f1)
          ; ("Points", "pts", fmt_int)
          ; ("Rebounds", "reb", fmt_int)
          ; ("Assists", "ast", fmt_int)
          ; ("Steals", "stl", fmt_int)
          ; ("Blocks", "blk", fmt_int)
          ; ("Turnovers", "tov", fmt_int)
          ]
        , [ ("FG%", "fg_pct", fmt_f3)
          ; ("3P%", "fg3_pct", fmt_f3)
          ; ("FT%", "ft_pct", fmt_f3)
          ; ("TS%", "ts_pct", fmt_f3)
          ; ("eFG%", "efg_pct", fmt_f3)
          ; ("USG%", "usg_pct", fmt_f1)
          ]
        )
    | "per_36" ->
        ( [ ("PTS/36", "pts", fmt_f1)
          ; ("REB/36", "reb", fmt_f1)
          ; ("AST/36", "ast", fmt_f1)
          ; ("STL/36", "stl", fmt_f1)
          ; ("BLK/36", "blk", fmt_f1)
          ; ("TOV/36", "tov", fmt_f1)
          ; ("EFF/36", "eff", fmt_f1)
          ]
        , [ ("FG%", "fg_pct", fmt_f3)
          ; ("3P%", "fg3_pct", fmt_f3)
          ; ("FT%", "ft_pct", fmt_f3)
          ; ("TS%", "ts_pct", fmt_f3)
          ; ("eFG%", "efg_pct", fmt_f3)
          ; ("USG%", "usg_pct", fmt_f1)
          ]
        )
    | _ ->
        ( [ ("Points", "pts", fmt_f1)
          ; ("Rebounds", "reb", fmt_f1)
          ; ("Assists", "ast", fmt_f1)
          ; ("Steals", "stl", fmt_f1)
          ; ("Blocks", "blk", fmt_f1)
          ; ("Turnovers", "tov", fmt_f1)
          ; ("Minutes", "min", fmt_f1)
          ; ("Efficiency", "eff", fmt_f1)
          ]
        , [ ("FG%", "fg_pct", fmt_f3)
          ; ("3P%", "fg3_pct", fmt_f3)
          ; ("FT%", "ft_pct", fmt_f3)
          ; ("TS%", "ts_pct", fmt_f3)
          ; ("eFG%", "efg_pct", fmt_f3)
          ; ("USG%", "usg_pct", fmt_f1)
          ]
        )
  in
  let render_cards categories =
    categories
    |> List.map (fun (title, key, fmt) ->
        leader_card ~value_fmt:fmt title (lookup key))
    |> String.concat ""
  in
  let main_grid =
    Printf.sprintf
      {html|<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6">%s</div>|html}
      (render_cards main_categories)
  in
  let shooting_grid =
    Printf.sprintf
      {html|<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6">%s</div>|html}
      (render_cards shooting_categories)
  in
  let note_html =
    {html|<div class="text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">Shooting leaders require minimum attempts: <span class="font-mono text-slate-700 dark:text-slate-300">FG≥50</span>, <span class="font-mono text-slate-700 dark:text-slate-300">3P≥20</span>, <span class="font-mono text-slate-700 dark:text-slate-300">FT≥20</span>. Per-36 leaders require <span class="font-mono text-slate-700 dark:text-slate-300">MIN≥100</span>.</div>|html}
  in
  let content =
    Printf.sprintf
      {html|<div class="space-y-8">%s%s<div class="flex items-baseline justify-between"><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">Shooting</h3><div class="text-xs text-slate-600 dark:text-slate-400">FG / 3P / FT / TS / eFG</div></div>%s</div>|html}
      main_grid note_html shooting_grid
  in
  layout
    ~title:"WKBL Leaders"
    ~canonical_path:"/leaders"
    ~description:"WKBL 여자농구 리더보드 - 득점, 리바운드, 어시스트, 슛 퍼센트 등 부문별 선두 선수 순위"
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">League Leaders</h2><p class="text-slate-600 dark:text-slate-400">Basketball-reference style leaderboards.</p></div><form action="/leaders" method="get" class="grid grid-cols-1 sm:grid-cols-2 gap-3 w-full md:w-auto"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select><select name="scope" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select></form></div>%s</div>|html}
      season_options scope_options content)
    ()

let awards_page ~(season: string) ~(seasons: season_info list) ~(include_mismatch: bool) ~(prev_season_name: string option) ~(mvp: leader_entry list) ~(mip: leader_entry list) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let include_checked = if include_mismatch then "checked" else "" in
  let mvp_card =
    if mvp = [] then
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MVP (EFF)</h3><div class="text-slate-600 dark:text-slate-400 text-sm leading-relaxed">표시할 데이터가 없습니다.</div></div>|html}
    else
      leader_card "MVP (EFF)" mvp
  in
  let mip_card =
    match prev_season_name with
    | None ->
        Printf.sprintf
          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MIP (ΔEFF)</h3><div class="text-slate-600 dark:text-slate-400 text-sm leading-relaxed">전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">EFF(AVG(game_score))</span> 변화(Δ)를 계산합니다. 시즌 선택 시에만 표시됩니다.</div></div>|html}
    | Some prev_name ->
        if mip = [] then
          Printf.sprintf
            {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MIP (ΔEFF)</h3><div class="text-slate-600 dark:text-slate-400 text-sm leading-relaxed">%s 대비 ΔEFF 계산에 필요한 표본이 부족합니다. (두 시즌 모두 <span class="font-mono text-slate-700 dark:text-slate-300">GP≥10</span>)</div></div>|html}
            (escape_html prev_name)
        else
          leader_card_signed (Printf.sprintf "MIP (ΔEFF vs %s)" prev_name) mip
  in
  let disclaimer_html =
    {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/60 p-5 text-slate-600 dark:text-slate-400 text-sm leading-relaxed"><div class="font-bold text-slate-700 dark:text-slate-300 mb-2">Stat Awards (unofficial)</div><ul class="list-disc list-inside space-y-1"><li>MVP: <span class="font-mono text-slate-700 dark:text-slate-300">EFF = AVG(game_score)</span> 상위</li><li>MIP: 전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">ΔEFF</span> 상위 (두 시즌 모두 <span class="font-mono text-slate-700 dark:text-slate-300">GP≥10</span>)</li><li>공식 수상 데이터가 DB에 없어서, 현재는 박스스코어 기반 지표로만 추정합니다.</li></ul></div>|html}
  in
  layout
    ~title:"WKBL Awards"
    ~canonical_path:"/awards"
    ~description:"WKBL 여자농구 시상 - MVP, MIP 등 시즌별 통계 기반 어워드 추정 순위"
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Awards</h2><p class="text-slate-600 dark:text-slate-400">Stat-based awards for quick reference.</p></div><form action="/awards" method="get" class="flex flex-wrap items-center gap-3"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-40" onchange="this.form.submit()">%s</select><label class="flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" onchange="this.form.submit()" title="Final score != sum(points) 경기 포함"><span>Mismatch 포함</span></label></form></div><div class="grid grid-cols-1 md:grid-cols-2 gap-6">%s%s</div>%s</div>|html}
      season_options
      include_checked
      mvp_card
      mip_card
      disclaimer_html)

(** Clutch Time Leaderboard Page
    Clutch time = Q4 remaining 5 minutes + score diff <= 5 points *)
let clutch_page ~season ~seasons (stats: clutch_stats list) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<option value="ALL" %s>All Seasons</option>%s|html}
      (if season = "ALL" then "selected" else "")
      base
  in
  let rows =
    stats
    |> List.mapi (fun i (s: clutch_stats) ->
        let rank = i + 1 in
        let fg_pct_str = if s.cs_clutch_fg_att > 0
          then Printf.sprintf "%.1f%%" (s.cs_clutch_fg_pct *. 100.0)
          else "-"
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-700 hover:bg-slate-50 dark:hover:bg-slate-800/50">
            <td class="px-3 py-2 text-center font-medium text-slate-500">%d</td>
            <td class="px-3 py-2"><a href="/player/%s" class="text-orange-500 hover:underline font-medium">%s</a></td>
            <td class="px-3 py-2 text-slate-600 dark:text-slate-400">%s</td>
            <td class="px-3 py-2 text-center">%d</td>
            <td class="px-3 py-2 text-center font-bold text-orange-500">%d</td>
            <td class="px-3 py-2 text-center">%d-%d</td>
            <td class="px-3 py-2 text-center">%s</td>
            <td class="px-3 py-2 text-center">%d</td>
            <td class="px-3 py-2 text-center">%d-%d</td>
          </tr>|html}
          rank
          (escape_html s.cs_player_id)
          (escape_html s.cs_player_name)
          (escape_html s.cs_team_name)
          s.cs_clutch_games
          s.cs_clutch_points
          s.cs_clutch_fg_made s.cs_clutch_fg_att
          fg_pct_str
          s.cs_clutch_3p_made
          s.cs_clutch_ft_made s.cs_clutch_ft_att)
    |> String.concat "\n"
  in
  let empty_row =
    if List.length stats = 0 then
      Printf.sprintf {html|<tr><td colspan="9">%s</td></tr>|html}
        (Views_common.empty_state ~icon:BasketballIcon
          "No clutch time data"
          "Clutch time = Q4 remaining 5 min + score diff ≤ 5 pts")
    else ""
  in
  let table_html = Printf.sprintf
    {html|<div class="overflow-x-auto">
      <table class="w-full text-sm">
        <thead class="bg-slate-100 dark:bg-slate-800 text-slate-700 dark:text-slate-300">
          <tr>
            <th class="px-3 py-2 text-center">#</th>
            <th class="px-3 py-2 text-left">Player</th>
            <th class="px-3 py-2 text-left">Team</th>
            <th class="px-3 py-2 text-center" title="Clutch Games">GP</th>
            <th class="px-3 py-2 text-center" title="Clutch Points">PTS</th>
            <th class="px-3 py-2 text-center" title="Field Goals Made-Attempted">FGM-A</th>
            <th class="px-3 py-2 text-center" title="Field Goal Percentage">FG%%</th>
            <th class="px-3 py-2 text-center" title="3-Pointers Made">3PM</th>
            <th class="px-3 py-2 text-center" title="Free Throws Made-Attempted">FTM-A</th>
          </tr>
        </thead>
        <tbody>%s%s</tbody>
      </table>
    </div>|html}
    rows empty_row
  in
  let content = Printf.sprintf
    {html|<div class="space-y-6 animate-fade-in">
      <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
        <div>
          <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">Clutch Time Leaders</h2>
          <p class="text-slate-600 dark:text-slate-400">
            Q4 remaining 5 minutes + score difference ≤ 5 points
          </p>
        </div>
        <form action="/clutch" method="get" class="flex items-center gap-3">
          <select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48" onchange="this.form.submit()">
            %s
          </select>
        </form>
      </div>
      <div class="bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
        %s
      </div>
      <div class="text-xs text-slate-600 dark:text-slate-400 text-center">
        Data sourced from play-by-play records. Clutch time scoring events are aggregated per player.
      </div>
    </div>|html}
    season_options table_html
  in
  layout ~title:"Clutch Time Leaders - WKBL" ~canonical_path:"/clutch"
    ~description:"WKBL 여자농구 클러치 타임 리더 - 4쿼터 5분 이내 접전 상황에서의 득점 기록."
    ~content ()

let live_page () =
  let content = {html|
    <div class="space-y-6">
      <div class="flex flex-col sm:flex-row sm:items-center justify-between gap-4">
        <div>
          <h1 class="text-2xl font-bold text-slate-900 dark:text-slate-200">🔴 Live Scores</h1>
          <p class="text-slate-600 dark:text-slate-400">실시간 경기 점수</p>
        </div>
        <div id="connection-status" class="flex items-center gap-2">
          <span class="w-2 h-2 bg-yellow-500 rounded-full animate-pulse"></span>
          <span class="text-sm text-slate-600 dark:text-slate-400">Connecting...</span>
        </div>
      </div>

      <div id="live-games" class="grid gap-4 md:grid-cols-2">
        <div class="bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-6 text-center">
          <div class="animate-pulse space-y-3">
            <div class="h-4 bg-slate-200 dark:bg-slate-700 rounded w-3/4 mx-auto"></div>
            <div class="h-8 bg-slate-200 dark:bg-slate-700 rounded w-1/2 mx-auto"></div>
            <div class="h-4 bg-slate-200 dark:bg-slate-700 rounded w-2/3 mx-auto"></div>
          </div>
        </div>
      </div>

      <div class="text-xs text-slate-500 dark:text-slate-400 text-center">
        Updates automatically via Server-Sent Events (SSE)
      </div>
    </div>

    <script>
    (function() {
      const gamesContainer = document.getElementById('live-games');
      const statusEl = document.getElementById('connection-status');

      function updateStatus(connected) {
        statusEl.innerHTML = connected
          ? '<span class="w-2 h-2 bg-green-500 rounded-full"></span><span class="text-sm text-slate-600 dark:text-slate-400">Connected</span>'
          : '<span class="w-2 h-2 bg-red-500 rounded-full"></span><span class="text-sm text-slate-600 dark:text-slate-400">Disconnected</span>';
      }

      function renderGame(game) {
        const isScheduled = game.status === 'scheduled';
        const scoreHtml = isScheduled
          ? '<div class="text-2xl font-bold text-slate-400 dark:text-slate-500">vs</div>'
          : `<div class="flex items-center justify-center gap-4 text-3xl font-bold">
               <span class="${game.home_score > game.away_score ? 'text-orange-500' : 'text-slate-900 dark:text-slate-200'}">${game.home_score}</span>
               <span class="text-slate-400">-</span>
               <span class="${game.away_score > game.home_score ? 'text-orange-500' : 'text-slate-900 dark:text-slate-200'}">${game.away_score}</span>
             </div>`;

        return `
          <a href="/game/${game.game_id}" class="block bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-6 hover:border-orange-400 transition-colors">
            <div class="flex justify-between items-center mb-4">
              <span class="text-lg font-semibold text-slate-900 dark:text-slate-200">${game.home}</span>
              <span class="text-xs px-2 py-1 rounded ${isScheduled ? 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400' : 'bg-green-100 dark:bg-green-900 text-green-600 dark:text-green-400'}">
                ${isScheduled ? 'Scheduled' : 'Final'}
              </span>
              <span class="text-lg font-semibold text-slate-900 dark:text-slate-200">${game.away}</span>
            </div>
            ${scoreHtml}
          </a>
        `;
      }

      function renderGames(data) {
        if (data.games.length === 0) {
          gamesContainer.innerHTML = `
            <div class="col-span-2 bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-12 text-center">
              <span class="text-4xl mb-4 block">🏀</span>
              <p class="text-slate-600 dark:text-slate-400">No games scheduled for today (${data.date})</p>
              <a href="/games" class="text-orange-500 hover:underline mt-2 inline-block">View all games →</a>
            </div>
          `;
        } else {
          gamesContainer.innerHTML = data.games.map(renderGame).join('');
        }
      }

      // Connect to SSE
      const evtSource = new EventSource('/api/live/scores');

      evtSource.addEventListener('scores', function(e) {
        updateStatus(true);
        const data = JSON.parse(e.data);
        renderGames(data);
      });

      evtSource.addEventListener('error', function(e) {
        console.error('SSE Error:', e);
        updateStatus(false);
      });

      evtSource.onerror = function() {
        updateStatus(false);
        // Reconnect after 5 seconds
        setTimeout(function() {
          location.reload();
        }, 5000);
      };
    })();
    </script>
  |html}
  in
  layout ~title:"Live Scores - WKBL" ~canonical_path:"/live"
    ~description:"WKBL 여자농구 실시간 스코어 - 오늘 경기 라이브 점수를 확인하세요."
    ~content ()

let error_page message = layout ~title:"Error" ~content:(Printf.sprintf {html|<div class="flex flex-col items-center justify-center py-20"><span class="text-6xl mb-4">😵</span><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200 mb-2">Something went wrong</h2><p class="text-slate-600 dark:text-slate-400">%s</p><a href="/" class="mt-4 text-orange-500 hover:underline">← Back to home</a></div>|html} (escape_html message)) ()

(* Re-export common functions for external access *)
let players_table = players_table

(* Re-export from Views_team *)
let team_profile_page = Views_team.team_profile_page

(* Re-export from Views_tools *)
let qa_dashboard_page = Views_tools.qa_dashboard_page
let transactions_page = Views_tools.transactions_page

(* Re-export from Views_history *)
let history_page = Views_history.history_page
let legends_page = Views_history.legends_page
let coaches_page = Views_history.coaches_page
