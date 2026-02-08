(** Page view functions for WKBL Analytics *)
(** This module contains page-specific rendering functions. *)
(** Common helpers are in Views_common module. *)

open Domain
open Views_common

(** Live scores widget for homepage *)
let live_scores_widget ?(lang=I18n.Ko) (games: Domain.live_game list) =
  let tr = I18n.t lang in
  let label_no_games_title = tr { ko = "오늘 경기가 없습니다"; en = "No games today" } in
  let label_no_games_sub = tr { ko = "경기 일정이 있는 날 다시 확인해주세요."; en = "Check back on game days." } in
  let label_live = tr { ko = "진행중"; en = "LIVE" } in
  let label_scheduled = tr { ko = "예정"; en = "Scheduled" } in
  let label_final = tr { ko = "경기종료"; en = "Final" } in
  if List.length games = 0 then
    empty_state ~icon:BasketballIcon label_no_games_title label_no_games_sub
  else
    let game_cards =
      games
      |> List.map (fun (g : Domain.live_game) ->
          let q = String.trim g.lg_quarter in
          let is_pre_game =
            (not g.lg_is_live)
            && (q = "경기전" || q = "경기 전" || q = "예정"
                || (q = "" && g.lg_home_score = 0 && g.lg_away_score = 0))
          in
          let show_badge_text =
            if g.lg_is_live then
              label_live
            else if is_pre_game then
              label_scheduled
            else if q = "" then
              label_scheduled
            else
              match lang with
              | I18n.En -> (
                  match q with
                  | "경기전" | "경기 전" | "예정" -> label_scheduled
                  | "경기종료" -> label_final
                  | _ -> q)
              | _ -> q
          in
          let time_html =
            let t = String.trim g.lg_time_remaining in
            if t = "" then ""
            else
              Printf.sprintf
                {html|<span class="text-xs font-mono text-slate-500">%s</span>|html}
                (escape_html t)
          in
          let status_badge =
            if g.lg_is_live then
              Printf.sprintf
                {html|<span class="live-badge"><span class="live-dot"></span>%s</span>|html}
                (escape_html label_live)
            else
              Printf.sprintf
                {html|<span class="px-2 py-0.5 rounded-full bg-slate-200 dark:bg-slate-700 text-slate-600 dark:text-slate-300 text-[10px]">%s</span>|html}
                (escape_html show_badge_text)
          in
          let score_center =
            if is_pre_game then
              {html|<span class="text-xl font-black text-slate-500 dark:text-slate-400">VS</span>|html}
            else
              Printf.sprintf
                {html|<span class="text-xl font-black font-mono text-slate-900 dark:text-white">%d : %d</span>|html}
                g.lg_home_score
                g.lg_away_score
          in
          Printf.sprintf
            {html|
     <div class="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-lg p-4 shadow-sm">
      <div class="flex items-center justify-between mb-2">
       %s
       %s
      </div>
      <div class="flex items-center justify-between">
       <div class="flex items-center gap-2">
        %s
        <span class="font-bold text-slate-900 dark:text-white">%s</span>
       </div>
       %s
       <div class="flex items-center gap-2">
        <span class="font-bold text-slate-900 dark:text-white text-right">%s</span>
        %s
       </div>
      </div>
     </div>
    |html}
            status_badge
            time_html
            (team_logo_tag ~class_name:"w-6 h-6" g.lg_home_team)
            (escape_html g.lg_home_team)
            score_center
            (escape_html g.lg_away_team)
            (team_logo_tag ~class_name:"w-6 h-6" g.lg_away_team))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="grid grid-cols-1 md:grid-cols-2 gap-4">%s</div>|html}
      game_cards

(** HTMX endpoint for live scores widget *)
let live_scores_htmx () =
 let games = Live.get_current_games () in
 live_scores_widget games

let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

(** Players table - HTMX partial *)
let players_table ?(lang=I18n.Ko) ?(player_info_map=None) (players: player_aggregate list) =
  let tr = I18n.t lang in
  let label_player = tr { ko = "선수"; en = "Player" } in
  let label_team = tr { ko = "팀"; en = "Team" } in
  let label_total = tr { ko = "누적"; en = "Total" } in
  (* 1. Define Columns *)
  let cols = [
    col "#" ~w:(px 50) ~align:`Center;
    col label_player ~w:(px 200);
    col label_team ~w:(px 130);
    col "GP" ~w:(px 60) ~align:`Right ~resp:`Hidden_sm;
    col "PTS" ~w:(px 90) ~align:`Right ~sort:"pts";
    col "MG" ~w:(px 80) ~align:`Right ~resp:`Hidden_md ~sort:"mg";
    col "REB" ~w:(px 85) ~align:`Right ~sort:"reb";
    col "AST" ~w:(px 85) ~align:`Right ~resp:`Hidden_md ~sort:"ast";
    col "STL" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg;
    col "BLK" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg;
    col "TO" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg;
    col "EFF ↓" ~w:(px 80) ~align:`Right ~sort:"eff" ~highlight:true;
    col "PER" ~w:(px 80) ~align:`Right ~resp:`Hidden_sm;
  ] in

  (* 2. Prepare Data (Dedupe logic preserved) *)
  let name_counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
  players |> List.iter (fun (p: player_aggregate) ->
      let key = normalize_name p.name in
      let prev = Hashtbl.find_opt name_counts key |> Option.value ~default:0 in
      Hashtbl.replace name_counts key (prev + 1));

  let rows_data = 
    players
    |> List.mapi (fun i (p: player_aggregate) ->
        let key = normalize_name p.name in
        let show_player_id =
          match Hashtbl.find_opt name_counts key with
          | Some c when c > 1 -> true
          | _ -> false
        in
        
        (* Helper components logic extracted from player_row *)
        let id_badge = "" in
        let display_name = normalize_name p.name in
        let info_opt =
          match player_info_map with
          | Some map -> Hashtbl.find_opt map p.player_id
          | None -> None
        in
        let disambiguation =
          if show_player_id then player_disambiguation_line ~team_name:p.team_name ~player_id:p.player_id info_opt
          else ""
        in
        let player_cell = 
          Printf.sprintf {html|<div class="flex items-center gap-3 min-w-0">%s<div class="flex flex-col min-w-0"><div class="flex items-center gap-2 min-w-0"><a href="/player/%s" class="player-name hover:text-orange-600 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a><span class="%s">%s</span></div>%s</div></div>|html}
          (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
          p.player_id
          (escape_html display_name)
          (if show_player_id then "inline-flex" else "hidden")
          id_badge
          disambiguation
        in
        
        let per =
          if p.total_minutes <= 0.0 then 0.0
          else
            let per_min = p.efficiency /. p.total_minutes in
            let per_48 = per_min *. 48.0 in
            let pace_factor = 40.0 /. 48.0 in
            let normalized = per_48 *. pace_factor *. 1.2 in
            max 0.0 (min 40.0 normalized)
        in
        
        (* Complex cells constructed as strings *)
        let pts_cell = 
          Printf.sprintf {html|<div class="flex flex-col items-end leading-tight"><span class="text-orange-600 dark:text-orange-400 font-bold font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-500 text-[9px] font-mono whitespace-nowrap" title="%s">Σ%s</span></div>|html} 
          p.avg_points (escape_html label_total) (format_int_compact p.total_points)
        in
        let stat_cell v total =
          Printf.sprintf {html|<div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-500 text-[9px] font-mono whitespace-nowrap" title="%s">Σ%s</span></div>|html}
          v (escape_html label_total) (format_int_compact total)
        in
        
        [
          Printf.sprintf {html|<span class="text-slate-500 dark:text-slate-500 font-bold">%d</span>|html} (i + 1);
          player_cell;
          team_badge p.team_name;
          Printf.sprintf {html|<span class="text-slate-500 dark:text-slate-400 font-mono">%d</span>|html} p.games_played;
          pts_cell;
          if p.avg_margin > 0.0 then Printf.sprintf "+%.1f" p.avg_margin else Printf.sprintf "%.1f" p.avg_margin;
          stat_cell p.avg_rebounds p.total_rebounds;
          stat_cell p.avg_assists p.total_assists;
          stat_cell p.avg_steals p.total_steals;
          stat_cell p.avg_blocks p.total_blocks;
          stat_cell p.avg_turnovers p.total_turnovers;
          format_float p.efficiency;
          format_float per;
        ]
    )
  in
  
  render_fixed_table ~id:"players-table-inner" ~min_width:"min-w-[1100px]" ~cols rows_data

let home_page ?(lang=I18n.Ko) ?(player_info_map=None) ?(live_games=Live.get_current_games ()) ~season ~seasons ~data_as_of players =
 let tr = I18n.t lang in
 let label_today_games = tr { ko = "오늘의 경기"; en = "Today's Games" } in
 let label_all_games = tr { ko = "전체 일정 →"; en = "All games →" } in
 let label_latest_game_date = tr { ko = "최근 경기일"; en = "Latest game date" } in
 let label_updating_sr = tr { ko = "업데이트 중"; en = "Updating" } in
 let label_eff_top_players = tr { ko = "EFF 상위 선수"; en = "Top EFF Players" } in
 let label_season_select = tr { ko = "시즌 선택"; en = "Season" } in
 let label_player_search_ph = tr { ko = "선수 검색..."; en = "Search players..." } in
 let label_player_search_aria = tr { ko = "선수 검색"; en = "Search players" } in
 let page_description =
   tr
     {
       ko = "WKBL 여자농구 효율성 순위, 팀 순위, 선수 통계를 한눈에 확인하세요.";
       en = "See WKBL efficiency rankings, team standings, and player stats at a glance.";
     }
 in
 let season_options =
  seasons
  |> List.map (fun (s: season_info) ->
    let selected = if s.code = season then "selected" else "" in
    Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
  |> String.concat "\n"
 in
   let table = players_table ~lang ~player_info_map players in
 let live_widget = live_scores_widget ~lang live_games in
 layout ~lang ~title:(tr { ko = "WKBL 통계"; en = "WKBL Stats" }) ~canonical_path:"/"
  ~description:page_description
  ~content:(Printf.sprintf
		   {html|<div class="space-y-6">
		    <!-- Live Scores Widget -->
		    <div class="bg-gradient-to-r from-orange-50 to-amber-50 dark:from-orange-950/30 dark:to-amber-950/30 border border-orange-200 dark:border-orange-800/50 rounded-lg p-4">
	     <div class="flex items-center justify-between mb-3">
	      <div class="flex items-center gap-2">
	       <span class="text-lg">🏀</span>
	       <span class="text-xs font-bold text-orange-700 dark:text-orange-300 uppercase tracking-wider">%s</span>
	      </div>
	      <div class="flex items-center gap-3">
	       <span class="text-xs text-slate-400 dark:text-slate-500">📅 %s: %s</span>
	       <a href="/games" class="text-xs text-orange-600 dark:text-orange-400 hover:text-orange-700 dark:hover:text-orange-300">%s</a>
	      </div>
	     </div>
	     <div id="live-scores" hx-get="/api/live/widget" hx-trigger="every 30s" hx-swap="innerHTML" hx-indicator="#live-loading">
	       <span id="live-loading" class="htmx-indicator"><span class="w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin inline-block" aria-hidden="true"></span><span class="sr-only">%s</span></span>
	       %s
	     </div>
	    </div>
		    <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3"><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">%s</h2><form class="flex gap-2 items-center" hx-get="/home/table" hx-target="#players-table-inner tbody" hx-trigger="change" hx-indicator="#table-loading"><select name="season" aria-label="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" placeholder="%s" aria-label="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none" hx-get="/home/table" hx-trigger="keyup changed delay:300ms" hx-target="#players-table-inner tbody" hx-indicator="#table-loading" name="search"><span id="table-loading" class="htmx-indicator"><span class="w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin inline-block"></span></span></form></div><div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 scroll-shadow overflow-y-hidden" data-skeleton="table" data-skeleton-count="10" data-skeleton-cols="8">%s</div></div>|html}
		   (escape_html label_today_games)
		   (escape_html label_latest_game_date)
		   (escape_html data_as_of)
		   (escape_html label_all_games)
		   (escape_html label_updating_sr)
		   live_widget
		   (escape_html label_eff_top_players)
		   (escape_html label_season_select)
		   season_options
		   (escape_html label_player_search_ph)
		   (escape_html label_player_search_aria)
		   table) ()

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
	   {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">선수</h2><p class="text-slate-600 dark:text-slate-400 text-sm">시즌 기준 선수 기록입니다.</p></div><a class="text-orange-600 dark:text-orange-400 hover:text-orange-700 text-sm" href="/players">초기화</a></div><form id="players-filter" class="grid grid-cols-1 md:grid-cols-4 gap-3" hx-get="/players/table" hx-target="#players-table" hx-trigger="change, keyup delay:250ms"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" name="search" placeholder="선수 검색..." value="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none"><select name="sort" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s</select><div class="flex items-center justify-between gap-3 text-xs"><div class="text-slate-600 dark:text-slate-400 flex items-center">정렬: %s</div><label class="flex items-center gap-2 text-slate-600 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="최종 스코어와 득점 합계가 다른 경기 포함"><span>불일치 포함</span></label></div></form>%s<div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 scroll-shadow overflow-y-hidden" data-skeleton="table" data-skeleton-count="15" data-skeleton-cols="8">%s</div></div>|html}
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

let team_stat_row ~season (row: team_stats) =
 let team_href =
  if season = "ALL" then
   Printf.sprintf "/team/%s" (Uri.pct_encode row.team)
  else
   Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode row.team) (Uri.pct_encode season)
 in
 let margin_color = if row.margin >= 0.0 then "text-sky-600 dark:text-sky-400 font-bold" else "text-rose-600 dark:text-rose-400 font-bold" in
 let margin_str = if row.margin > 0.0 then Printf.sprintf "+%.1f" row.margin else format_float row.margin in
     let name_cell = Printf.sprintf {html|<td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 flex items-center gap-2 whitespace-nowrap break-keep w-auto"><div class="flex items-center min-w-0">%s<a href="%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors ml-2 truncate">%s</a></div></td>|html} (team_logo_tag ~class_name:"w-5 h-5 shrink-0" row.team) (escape_html team_href) (escape_html row.team) in
     let gp_cell = Printf.sprintf {html|<td class="px-3 py-2 text-right whitespace-nowrap" ><span class="text-slate-600 dark:text-slate-400 font-mono">%d</span></td>|html} row.gp in
     
     
       (* Helper for clean stat cells *)
     
       let cell ?(hide="") ?(color="text-slate-700 dark:text-slate-300") ?(width="width: 80px; min-width: 80px;") value =
     
         Printf.sprintf {html|<td class="px-3 py-2 text-right whitespace-nowrap %s" style="%s"><span class="%s font-mono">%s</span></td>|html} hide width color value
     
       in let cells = String.concat "" [
  cell ~hide:"hidden md:table-cell" (format_float row.min_total);
  cell (format_float row.pts);
  cell ~color:margin_color margin_str;
  cell ~hide:"hidden md:table-cell" (format_float row.pts_against);
  cell ~hide:"hidden sm:table-cell" (format_float row.reb);
  cell ~hide:"hidden sm:table-cell" (format_float row.ast);
  cell ~hide:"hidden md:table-cell" (format_float row.stl);
  cell ~hide:"hidden md:table-cell" (format_float row.blk);
  cell ~hide:"hidden md:table-cell" (format_float row.turnovers);
  cell ~hide:"hidden lg:table-cell" (format_float row.fg_pct);
  cell ~hide:"hidden lg:table-cell" (format_float row.fg3_pct);
  cell ~hide:"hidden lg:table-cell" (format_float row.ft_pct);
  cell ~hide:"hidden lg:table-cell" (format_float row.efg_pct);
  cell ~hide:"hidden lg:table-cell" ~color:"text-emerald-600 dark:text-emerald-400" (format_float row.ts_pct);
  cell ~color:"text-orange-600 dark:text-orange-400 font-bold" (format_float row.eff);
 ] in
 Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">%s%s%s</tr>|html} name_cell gp_cell cells

let teams_table ?(lang=I18n.Ko) ~season ~scope (stats: Domain.team_stats list) =
  let tr = I18n.t lang in
  let min_label = if scope = Domain.PerGame then "MIN/G" else "MIN" in
  let label_team = tr { ko = "팀"; en = "Team" } in
  let title_mg = tr { ko = "MG: 팀 득실마진(PTS - PA)"; en = "MG: Points margin (PTS - PA)" } in
  let title_pace = tr { ko = "Pace: 40분 기준 공격 횟수 추정"; en = "Est. possessions per 40 min" } in
  let title_ts = tr { ko = "TS%: True Shooting %"; en = "True Shooting %" } in
  let team_link (s: Domain.team_stats) =
    if season = "ALL" then Printf.sprintf "/team/%s" (Uri.pct_encode s.team)
    else Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode s.team) (Uri.pct_encode season)
  in
  let margin_color v =
    if v > 0.0 then "text-sky-600 dark:text-sky-400 font-bold"
    else if v < 0.0 then "text-rose-600 dark:text-rose-400 font-bold"
    else "text-slate-700 dark:text-slate-300 font-bold"
  in
  
  (* 1. Define Columns (Metadata only) *)
  let cols = [
    col "#" ~w:(px 40) ~align:`Center;
    col label_team;
    col "GP" ~w:(px 60) ~align:`Right ~sort:"gp";
    col min_label ~w:(px 80) ~align:`Right ~resp:`Hidden_md ~sort:"min";
    col "PTS" ~w:(px 60) ~align:`Right ~sort:"pts";
    col "MG" ~w:(px 60) ~align:`Right ~sort:"mg" ~title:title_mg;
    col "PA" ~w:(px 60) ~align:`Right ~resp:`Hidden_md ~sort:"pa";
    col "REB" ~w:(px 60) ~align:`Right ~resp:`Hidden_sm ~sort:"reb";
    col "AST" ~w:(px 60) ~align:`Right ~resp:`Hidden_sm ~sort:"ast";
    col "STL" ~w:(px 60) ~align:`Right ~resp:`Hidden_md ~sort:"stl";
    col "BLK" ~w:(px 60) ~align:`Right ~resp:`Hidden_md ~sort:"blk";
    col "TO" ~w:(px 60) ~align:`Right ~resp:`Hidden_md ~sort:"to";
    col "Pace" ~w:(px 60) ~align:`Right ~resp:`Hidden_md ~sort:"pace" ~title:title_pace;
    col "FG%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"fg";
    col "3P%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"3p";
    col "FT%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"ft";
    col "eFG%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"efg";
    col "TS%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"ts" ~title:title_ts ~highlight:true;
    col "EFF" ~w:(px 80) ~align:`Right ~sort:"eff" ~highlight:true;
  ] in

  (* 2. Transform Data to Raw Strings (Data-Oriented) *)
  let rows_data =
	    stats
	    |> List.mapi (fun idx s ->
	        let rank = Printf.sprintf {html|<span class="font-bold text-slate-500 dark:text-slate-400">%d</span>|html} (idx + 1) in
	        let logo = team_logo_tag ~class_name:"w-5 h-5 shrink-0" s.team in
		        let team_cell =
		          Printf.sprintf
		            {html|<a href="%s" class="team-link flex w-full items-center gap-2 min-w-0 hover:text-orange-600 dark:text-orange-400 transition-colors cursor-pointer -mx-3 -my-2 px-3 py-2">%s<span class="truncate">%s</span></a>|html}
		            (escape_html (team_link s))
		            logo
		            (escape_html s.team)
	        in
	        let margin_str =
	          let v = s.margin in
	          let s_str = if v > 0.0 then Printf.sprintf "+%.1f" v else format_float v in
	          Printf.sprintf {html|<span class="%s">%s</span>|html} (margin_color v) s_str
	        in

        [
          rank;
          team_cell;
          string_of_int s.gp;
          format_float s.min_total;
          format_float s.pts;
          margin_str;
          format_float s.pts_against;
          format_float s.reb;
          format_float s.ast;
          format_float s.stl;
          format_float s.blk;
          format_float s.turnovers;
          format_float s.pace;
          format_float s.fg_pct;
          format_float s.fg3_pct;
          format_float s.ft_pct;
          format_float s.efg_pct;
          format_float s.ts_pct;
          format_float s.eff;
        ]
      )
  in

  (* 3. Render Table *)
  render_fixed_table ~table_attrs:{|data-row-link="team"|} ~id:"teams-table-inner" ~min_width:"min-w-[1040px]" ~cols rows_data

let teams_page ?(lang=I18n.Ko) ~season ~seasons ~scope ~sort ~include_mismatch stats =
 let scope_value = team_scope_to_string scope in
 let scope_option value label = let selected = if scope_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
 let sort_option value label = let selected = if String.lowercase_ascii sort = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
 let season_options = let base = seasons |> List.map (fun s -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>전체 시즌</option>%s|html} (if season = "ALL" then "selected" else "") base in
 let table = teams_table ~lang ~season ~scope stats in
 let include_checked = if include_mismatch then "checked" else "" in
 layout ~lang ~title:"WKBL 팀" ~canonical_path:"/teams"
  ~description:"WKBL 여자농구 팀 통계 - 6개 구단의 득점, 리바운드, 어시스트 등 시즌별 성적을 비교하세요."
  ~content:(Printf.sprintf
		   {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">팀</h2><p class="text-slate-600 dark:text-slate-400 text-sm">시즌과 기준(경기당/누적)으로 팀 기록을 봅니다.</p></div><a class="text-orange-600 dark:text-orange-400 hover:text-orange-700 text-sm" href="/teams">초기화</a></div><form id="teams-filter" class="grid grid-cols-1 md:grid-cols-3 gap-3" hx-get="/teams/table" hx-target="#teams-table-inner tbody" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><select name="scope" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s</select><select name="sort" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s%s%s%s%s%s%s%s</select><label class="md:col-span-3 flex items-center justify-end gap-2 text-xs text-slate-600 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="최종 스코어와 득점 합계가 다른 경기 포함"><span>불일치 포함</span></label></form><div id="teams-table-container" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="12">%s</div><div class="mt-8 grid grid-cols-1 lg:grid-cols-2 gap-6"><div id="teams-shooting-chart" hx-get="/teams/shooting-chart?season=%s" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4 text-slate-400"><span class="htmx-indicator">차트 로딩 중...</span></div></div><div id="teams-radar-chart" hx-get="/teams/radar-chart?season=%s" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4 text-slate-400"><span class="htmx-indicator">차트 로딩 중...</span></div></div></div></div>|html}
	   season_options (scope_option "per_game" "경기당") (scope_option "totals" "누적") (sort_option "pts" "PTS") (sort_option "reb" "REB") (sort_option "ast" "AST") (sort_option "stl" "STL") (sort_option "blk" "BLK") (sort_option "eff" "EFF") (sort_option "ts_pct" "TS%") (sort_option "fg3_pct" "3P%") (sort_option "min_total" "MIN") include_checked table season season) ()

let standings_table ?(lang=I18n.Ko) ~season (standings : team_standing list) =
  let tr = I18n.t lang in
  let label_team = tr { ko = "팀"; en = "Team" } in
  let label_diff = tr { ko = "득실"; en = "Diff" } in
  let cols = [
    col label_team;
    col "GP" ~w:(px 60) ~align:`Right ~sort:"gp";
    col "W" ~w:(px 60) ~align:`Right ~sort:"w";
    col "L" ~w:(px 60) ~align:`Right ~sort:"l";
    col "PCT" ~w:(px 80) ~align:`Right ~sort:"pct" ~highlight:true;
    col "GB" ~w:(px 60) ~align:`Right ~sort:"gb" ~resp:`Hidden_sm;
    col "PS/G" ~w:(px 80) ~align:`Right ~sort:"ps" ~resp:`Hidden_md;
    col "PA/G" ~w:(px 80) ~align:`Right ~sort:"pa" ~resp:`Hidden_md;
    col label_diff ~w:(px 80) ~align:`Right ~sort:"diff" ~resp:`Hidden_sm;
  ] in

  let rows_data =
    standings
    |> List.map (fun (s : team_standing) ->
      let win_pct_fmt = Printf.sprintf "%.3f" s.win_pct in
      let gb_fmt = if s.gb = 0.0 then "-" else Printf.sprintf "%.1f" s.gb in
      let team_href =
       if season = "ALL" then
        Printf.sprintf "/team/%s" (Uri.pct_encode s.team_name)
	       else
	        Printf.sprintf "/team/%s?season=%s" (Uri.pct_encode s.team_name) (Uri.pct_encode season)
	      in
		      let team_cell =
		        Printf.sprintf
		          {html|<a href="%s" class="team-link team-name flex w-full items-center gap-2 min-w-0 hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors cursor-pointer -mx-3 -my-2 px-3 py-2" style="white-space: nowrap; word-break: keep-all;">%s<span class="truncate">%s</span></a>|html}
		          (escape_html team_href)
		          (team_logo_tag ~class_name:"w-5 h-5 shrink-0" s.team_name)
		          (escape_html s.team_name)
	      in
	      let pct_bar_width = int_of_float (s.win_pct *. 100.0) in
	      let pct_cell = Printf.sprintf {html|<div class="flex flex-col items-end gap-1 leading-tight">
	        <span class="text-orange-600 dark:text-orange-400 font-bold font-mono">%s</span>
	        <div class="w-16 h-1.5 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden">
          <div class="h-full bg-gradient-to-r from-orange-400 to-orange-600 rounded-full transition-all" style="width: %d%%"></div>
        </div>
      </div>|html} win_pct_fmt pct_bar_width in
      
      let diff_color = if s.diff >= 0.0 then "text-emerald-600 dark:text-emerald-400" else "text-rose-600 dark:text-rose-400" in
      let diff_str = if s.diff > 0.0 then Printf.sprintf "+%.1f" s.diff else Printf.sprintf "%.1f" s.diff in
      let diff_cell = Printf.sprintf {html|<div class="flex flex-col items-end leading-tight"><span class="%s font-mono font-bold">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">%s</span></div>|html} diff_color diff_str (escape_html label_diff) in

      [
        team_cell;
        string_of_int s.games_played;
        string_of_int s.wins;
        string_of_int s.losses;
        pct_cell;
        gb_fmt;
        Printf.sprintf "%.1f" s.avg_pts;
        Printf.sprintf "%.1f" s.avg_opp_pts;
        diff_cell;
      ]
    )
  in
  
  render_fixed_table ~table_attrs:{|data-row-link="team"|} ~id:"standings-table-inner" ~min_width:"min-w-[560px]" ~cols rows_data

let standings_page ?(lang=I18n.Ko) ~season ~seasons standings =
 let tr = I18n.t lang in
 let label_all_seasons = tr { ko = "전체 시즌"; en = "All seasons" } in
 let label_heading = tr { ko = "순위"; en = "Standings" } in
 let label_subtitle = tr { ko = "승률 기준 순위표입니다."; en = "Ranked by win percentage." } in
 let page_title = tr { ko = "WKBL 순위"; en = "WKBL Standings" } in
 let season_options =
   let base =
     seasons
     |> List.map (fun (s: season_info) ->
       let selected = if s.code = season then "selected" else "" in
       Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
     |> String.concat "\n"
   in
   Printf.sprintf
     {html|<option value="ALL" %s>%s</option>%s|html}
     (if season = "ALL" then "selected" else "")
     (escape_html label_all_seasons)
     base
 in
 let table = standings_table ~lang ~season standings in
 layout ~lang ~title:page_title ~canonical_path:"/standings"
  ~description:"WKBL 여자농구 순위표 - 시즌별 팀 순위, 승률, 승패 기록을 확인하세요."
  ~content:(Printf.sprintf
   {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h2><p class="text-slate-600 dark:text-slate-400 text-sm">%s</p></div></div><form id="standings-filter" class="flex gap-3" hx-get="/standings/table" hx-target="#standings-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="standings-table" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="10">%s</div></div>|html}
   (escape_html label_heading)
   (escape_html label_subtitle)
   season_options
   table) ()

let games_table ?(lang=I18n.Ko) (games : game_summary list) =
  let tr = I18n.t lang in
  let label_date = tr { ko = "날짜"; en = "Date" } in
  let label_home = tr { ko = "홈"; en = "Home" } in
  let label_score = tr { ko = "점수"; en = "Score" } in
  let label_away = tr { ko = "원정"; en = "Away" } in
  let label_action = tr { ko = "보기"; en = "Action" } in
  let label_scheduled = tr { ko = "예정"; en = "Scheduled" } in
  let label_missing_score = tr { ko = "점수 없음"; en = "No score" } in
  let label_boxscore = tr { ko = "박스스코어"; en = "Boxscore" } in
  let today_ymd =
    (* KST is UTC+9 and has no DST. Compare ISO date strings lexicographically. *)
    let tm = Unix.gmtime (Unix.time () +. (9. *. 3600.)) in
    Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
	  in
	  let status_label_for_game (g : game_summary) =
	    let game_ymd =
	      if String.length g.game_date >= 10 then String.sub g.game_date 0 10 else g.game_date
	    in
	    if String.length game_ymd = 10 && String.compare game_ymd today_ymd < 0 then
	      label_missing_score
	    else
	      label_scheduled
	  in
  let cols = [
    col label_date ~w:(px 120);
    col label_home;
    col label_score ~w:(px 120) ~align:`Center;
    col label_away;
    col label_action ~w:(px 100) ~align:`Right;
  ] in

  let mobile_cards =
    games
    |> List.mapi (fun _i (g : game_summary) ->
      let has_score =
        match g.home_score, g.away_score with
        | Some a, Some b -> a > 0 && b > 0
        | _ -> false
      in
      let score_a = match g.home_score with Some s when has_score -> string_of_int s | _ -> "-" in
      let score_b = match g.away_score with Some s when has_score -> string_of_int s | _ -> "-" in
      let score_html =
        if has_score then Printf.sprintf "%s - %s" score_a score_b
        else escape_html (status_label_for_game g)
      in
      let status_class = if has_score then "text-slate-900 dark:text-slate-200" else "text-slate-600 dark:text-slate-400" in
      let action_html =
       if not has_score then
        Printf.sprintf
          {html|<span class="text-[10px] text-slate-600 dark:text-slate-400">%s</span>|html}
          (escape_html (status_label_for_game g))
       else
        Printf.sprintf
         {html|<a href="/boxscore/%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">%s</a>|html}
         (escape_html g.game_id)
         (escape_html label_boxscore)
      in
      let card_class =
        if has_score then
          "bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 hover:border-orange-300 dark:hover:border-orange-700 rounded-lg p-3 shadow-sm hover:shadow-md space-y-2 transition-colors transition-shadow transition-transform duration-200 cursor-pointer group active:scale-[0.98] focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40"
        else
          "bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-3 shadow-sm space-y-2 transition-colors duration-200 group"
      in
      let card_attrs =
        if has_score then
          let aria = tr { ko = "박스스코어 보기"; en = "View boxscore" } in
          Printf.sprintf
            {|
role="link" tabindex="0" aria-label="%s"
onclick="window.location='/boxscore/%s'"
onkeydown="if(event.key==='Enter'||event.key===' ') { event.preventDefault(); window.location='/boxscore/%s'; }"|} (* keep as a raw attribute fragment *)
            (escape_html aria)
            (escape_html g.game_id)
            (escape_html g.game_id)
	        else
	          Printf.sprintf {|role="group" aria-label="%s"|} (escape_html (status_label_for_game g))
	      in
	      Printf.sprintf
	       {html|<div %s class="%s">
	   <div class="flex items-center justify-between text-[11px] text-slate-600 dark:text-slate-400 font-mono">
	    <span>%s</span>
	    %s
	   </div>
	     <div class="flex items-center justify-between gap-3">
	    <div class="flex flex-col gap-1 min-w-0">
		     <a href="/team/%s" class="team-link flex items-center gap-2 text-sm font-medium group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors truncate">%s<span class="truncate">%s</span></a>
		     <a href="/team/%s" class="team-link flex items-center gap-2 text-sm font-medium group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors truncate">%s<span class="truncate">%s</span></a>
		    </div>
			    <div class="text-right font-mono text-sm %s whitespace-nowrap group-hover:scale-110 transition-transform">%s</div>
			   </div>
				  </div>|html}
         card_attrs
         card_class
		       (escape_html g.game_date)
		       action_html
	       (Uri.pct_encode g.home_team)
		       (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
	       (escape_html g.home_team)
	       (Uri.pct_encode g.away_team)
	       (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
	       (escape_html g.away_team)
	       status_class
	       score_html)
	    |> String.concat "\n"
	  in

  let rows_data =
    games
	    |> List.mapi (fun _i (g : game_summary) ->
	        let has_score =
	          match g.home_score, g.away_score with
	          | Some a, Some b -> a > 0 && b > 0
	          | _ -> false
	        in
	        let score_a = match g.home_score with Some s when has_score -> string_of_int s | _ -> "-" in
	        let score_b = match g.away_score with Some s when has_score -> string_of_int s | _ -> "-" in
	        
		        let home_cell =
		          Printf.sprintf
		            {html|<a href="/team/%s" class="team-link team-name inline-flex items-center justify-end gap-2 whitespace-nowrap group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors" style="white-space: nowrap;">%s<span>%s</span></a>|html}
		            (Uri.pct_encode g.home_team)
		            (team_logo_tag ~class_name:"w-4 h-4 shrink-0" g.home_team)
		            (escape_html g.home_team)
		        in
		        let score_cell =
	          if has_score then
	            Printf.sprintf
		              {html|<span class="font-bold text-orange-600 dark:text-orange-400 font-mono group-hover:scale-110 transition-transform whitespace-nowrap w-28">%s - %s</span>|html}
	              score_a
	              score_b
	          else
	            Printf.sprintf
	              {html|<span class="text-xs text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap">%s</span>|html}
	              (escape_html (status_label_for_game g))
	        in
		        let away_cell =
		          Printf.sprintf
		            {html|<a href="/team/%s" class="team-link team-name inline-flex items-center gap-2 whitespace-nowrap group-hover:text-orange-600 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors" style="white-space: nowrap;"><span>%s</span>%s</a>|html}
		            (Uri.pct_encode g.away_team)
		            (escape_html g.away_team)
		            (team_logo_tag ~class_name:"w-4 h-4 shrink-0" g.away_team)
		        in
				        let action_cell =
				          if not has_score then
				            Printf.sprintf
				              {html|<span class="text-xs text-slate-400 font-mono">%s</span>|html}
			              (escape_html (status_label_for_game g))
			          else
			            Printf.sprintf
			              {html|<a href="/boxscore/%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition group-hover:ring-2 group-hover:ring-orange-500/50 whitespace-nowrap">%s</a>|html}
			              (escape_html g.game_id)
			              (escape_html label_boxscore)
		        in

	        [
	          (escape_html g.game_date);
	          home_cell;
          score_cell;
          away_cell;
          action_cell;
        ]
    )
  in
  
  let desktop_table = render_fixed_table ~id:"games-body" ~min_width:"min-w-[720px]" ~cols rows_data in
  
  Printf.sprintf
    {html|<div class="space-y-3 sm:hidden">%s</div><div class="hidden sm:block">%s</div>|html}
    mobile_cards desktop_table

			let games_page ?(lang=I18n.Ko) ~page ~season ~seasons games =
      let tr = I18n.t lang in
      let label_all_seasons = tr { ko = "전체 시즌"; en = "All seasons" } in
      let label_heading = tr { ko = "경기 결과"; en = "Game Results" } in
      let label_prev = tr { ko = "이전 페이지"; en = "Prev" } in
      let label_next = tr { ko = "다음 페이지"; en = "Next" } in
      let label_page = tr { ko = "페이지"; en = "Page" } in
      let page_title = tr { ko = "WKBL 경기"; en = "WKBL Games" } in
		  let season_options =
		    let base =
		      seasons
		      |> List.map (fun (s: season_info) ->
		        Printf.sprintf
	          {|<option value="%s" %s>%s</option>|}
	          (escape_html s.code)
	          (if s.code = season then "selected" else "")
	          (escape_html s.name)
	      )
		      |> String.concat ""
		    in
		    Printf.sprintf
			      {html|<option value="ALL" %s>%s</option>%s|html}
			      (if season = "ALL" then "selected" else "")
            (escape_html label_all_seasons)
			      base
		  in

	  let pagination_html = 
	    let prev_btn = if page > 1 then 
	      Printf.sprintf {html|<a href="/games?season=%s&page=%d" class="px-4 py-2 bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg text-sm font-bold hover:bg-slate-50 dark:hover:bg-slate-700 transition-colors">%s</a>|html} season (page - 1) (escape_html label_prev)
	    else "" in
	    let next_btn = if List.length games >= 50 then
	      Printf.sprintf {html|<a href="/games?season=%s&page=%d" class="px-4 py-2 bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg text-sm font-bold hover:bg-slate-50 dark:hover:bg-slate-700 transition-colors">%s</a>|html} season (page + 1) (escape_html label_next)
	    else "" in
		    Printf.sprintf {html|<div class="mt-8 flex items-center justify-center gap-4">%s <span class="text-xs font-mono text-slate-400">%s %d</span> %s</div>|html} prev_btn (escape_html label_page) page next_btn
		  in

		  layout ~lang ~title:page_title ~canonical_path:"/games"
		    ~content:(Printf.sprintf {html|
	      <div class="space-y-6">
	        <div class="flex items-center justify-between">
		          <h2 class="text-2xl font-black text-slate-900 dark:text-white tracking-tight">%s</h2>
			          <form action="/games" method="get" class="flex items-center gap-2">
			            <select name="season" onchange="this.form.submit()" class="bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-1.5 text-sm font-medium focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40 transition-colors">
			              %s
			            </select>
			          </form>
		        </div>
		        %s
		        %s
		      </div>|html} (escape_html label_heading) season_options (games_table ~lang games) pagination_html) ()

let boxscores_table ?(lang=I18n.Ko) (games : game_summary list) =
  let tr = I18n.t lang in
  let label_date = tr { ko = "날짜"; en = "Date" } in
  let label_home = tr { ko = "홈"; en = "Home" } in
  let label_away = tr { ko = "원정"; en = "Away" } in
  let label_diff = tr { ko = "점수차"; en = "Diff" } in
  let label_view = tr { ko = "보기"; en = "Boxscore" } in
  let scored_games =
    games
    |> List.filter (fun (g : game_summary) ->
      match (g.home_score, g.away_score) with
      | Some a, Some b -> a > 0 && b > 0
      | _ -> false)
  in
  let cols = [
    col label_date ~w:(px 120);
    col label_home;
    col "PTS" ~w:(px 60) ~align:`Center;
    col "PTS" ~w:(px 60) ~align:`Center;
    col label_away;
    col label_diff ~w:(px 80) ~align:`Right ~resp:`Hidden_sm;
    col label_view ~w:(px 80) ~align:`Right;
  ] in

  let mobile_cards =
    scored_games
    |> List.mapi (fun _i (g : game_summary) ->
      let score_a = Option.value ~default:0 g.home_score in
      let score_b = Option.value ~default:0 g.away_score in
      let margin = score_a - score_b in
      let margin_str = if margin > 0 then Printf.sprintf "+%.0d" margin else if margin < 0 then Printf.sprintf "%+.0d" margin else "0" in
      let margin_color = if margin > 0 then "text-sky-600 dark:text-sky-400" else if margin < 0 then "text-rose-600 dark:text-rose-400" else "text-slate-600 dark:text-slate-400" in
      Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-3 shadow-sm space-y-2">
		   <div class="flex items-center justify-between text-[11px] text-slate-600 dark:text-slate-400 font-mono">
		    <span>%s</span>
		    <span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono %s whitespace-nowrap">%s %s</span>
	   </div>
   <div class="flex items-center justify-between gap-3">
    <div class="flex flex-col gap-1 min-w-0 w-full">
     <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
     <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
    </div>
			    <a href="/boxscore/%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition">%s</a>
		   </div>
		  </div>|html}
	        (escape_html g.game_date)
	        margin_color
	        (escape_html label_diff)
	        margin_str
	        (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
	        (Uri.pct_encode g.home_team)
	        (escape_html g.home_team)
	        score_a
	        (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
	        (Uri.pct_encode g.away_team)
	        (escape_html g.away_team)
		        score_b
		        (escape_html g.game_id)
		        (escape_html label_view))
    |> String.concat "\n"
  in

  let rows_data =
    scored_games
    |> List.mapi (fun _i (g : game_summary) ->
      let score_a = Option.value ~default:0 g.home_score in
      let score_b = Option.value ~default:0 g.away_score in
      let margin = score_a - score_b in
      let margin_str = if margin > 0 then Printf.sprintf "+%.0d" margin else if margin < 0 then Printf.sprintf "%+.0d" margin else "0" in
      let margin_color = if margin > 0 then "text-sky-600 dark:text-sky-400 font-bold" else if margin < 0 then "text-rose-600 dark:text-rose-400 font-bold" else "text-slate-600 dark:text-slate-400 font-bold" in
      
      let home_cell = Printf.sprintf {html|<span class="inline-flex items-center gap-2">%s<a href="/team/%s">%s</a></span>|html} (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (Uri.pct_encode g.home_team) (escape_html g.home_team) in
      let away_cell = Printf.sprintf {html|<span class="inline-flex items-center gap-2"><a href="/team/%s">%s</a>%s</span>|html} (Uri.pct_encode g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) in
      let link_cell =
        Printf.sprintf
          {html|<a href="/boxscore/%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition whitespace-nowrap">%s</a>|html}
          (escape_html g.game_id)
          (escape_html label_view)
      in
      let margin_cell = Printf.sprintf {html|<span class="%s font-mono">%s</span>|html} margin_color margin_str in

      [
        (escape_html g.game_date);
        home_cell;
        string_of_int score_a;
        string_of_int score_b;
        away_cell;
        margin_cell;
        link_cell;
      ]
    )
  in
  
  let desktop_table = render_fixed_table ~id:"boxscores-body" ~min_width:"min-w-[720px]" ~cols rows_data in
  Printf.sprintf
    {html|<div class="space-y-3 sm:hidden">%s</div><div class="hidden sm:block">%s</div>|html}
    mobile_cards desktop_table

let boxscores_page ?(lang=I18n.Ko) ~season ~seasons games =
   let tr = I18n.t lang in
   let label_all_seasons = tr { ko = "전체 시즌"; en = "All seasons" } in
   let label_heading = tr { ko = "박스스코어"; en = "Boxscores" } in
   let label_subtitle = tr { ko = "경기 결과와 점수차를 봅니다."; en = "Final scores and point differentials." } in
   let page_title = tr { ko = "WKBL 박스스코어"; en = "WKBL Boxscores" } in
	 let season_options =
     let base =
       seasons
       |> List.map (fun (s: season_info) ->
         let selected = if s.code = season then "selected" else "" in
         Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
       |> String.concat "\n"
     in
     Printf.sprintf
       {html|<option value="ALL" %s>%s</option>%s|html}
       (if season = "ALL" then "selected" else "")
       (escape_html label_all_seasons)
       base
   in
	 let table = boxscores_table ~lang games in
	 layout ~lang ~title:page_title ~canonical_path:"/boxscores"
	  ~description:"WKBL 여자농구 경기 박스스코어 - 경기별 개인 기록과 팀 통계를 확인하세요."
	  ~content:(Printf.sprintf
	   {html|<div class="space-y-6"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h2><p class="text-slate-600 dark:text-slate-400 text-sm">%s</p></div></div><form id="boxscores-filter" class="flex gap-3" hx-get="/boxscores/table" hx-target="#boxscores-table" hx-trigger="change"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="boxscores-table" data-skeleton="cards" data-skeleton-count="10">%s</div></div>|html}
	   (escape_html label_heading)
     (escape_html label_subtitle)
     season_options
     table) ()

let boxscore_player_table (title: string) (players: boxscore_player_stat list) =
  let cols = [
    col "선수" ~w:(px 180);
    col "MP" ~w:(px 60) ~align:`Right ~resp:`Hidden_sm;
    col "PTS" ~w:(px 60) ~align:`Right ~highlight:true;
    col "+/- " ~w:(px 60) ~align:`Right ~resp:`Hidden_sm;
    col "TRB" ~w:(px 60) ~align:`Right;
    col "AST" ~w:(px 60) ~align:`Right;
    col "STL" ~w:(px 60) ~align:`Right ~resp:`Hidden_md;
    col "BLK" ~w:(px 60) ~align:`Right ~resp:`Hidden_md;
    col "TOV" ~w:(px 60) ~align:`Right ~resp:`Hidden_md;
    col "FG" ~w:(px 100) ~align:`Right ~resp:`Hidden_lg;
    col "3P" ~w:(px 100) ~align:`Right ~resp:`Hidden_lg;
    col "FT" ~w:(px 100) ~align:`Right ~resp:`Hidden_lg;
  ] in

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
 
 let rows_data =
  players
  |> List.map (fun (p: boxscore_player_stat) ->
    (* Clean plus/minus string *)
    let pm_str, pm_cls = match p.bs_plus_minus with
     | Some v when v > 0 -> (Printf.sprintf "+%.0d" v, "text-sky-600 dark:text-sky-400 font-bold")
     | Some v when v < 0 -> (Printf.sprintf "%+.0d" v, "text-rose-600 dark:text-rose-400 font-bold")
     | Some 0 -> ("0", "text-slate-500 dark:text-slate-500")
     | None -> ("-", "text-slate-400 dark:text-slate-600")
     | _ -> ("0", "text-slate-500 dark:text-slate-500")
    in
    
    let player_cell = Printf.sprintf {html|<div class="flex items-center gap-3 min-w-0 font-sans font-medium text-slate-900 dark:text-slate-200">%s<div class="flex flex-col min-w-0 overflow-hidden"><a href="/player/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate block">%s</a><span class="text-[10px] text-slate-500 dark:text-slate-400 font-normal truncate block">%s</span></div></div>|html}
      (player_img_tag ~class_name:"w-8 h-8 rounded-full object-cover bg-slate-100 dark:bg-slate-800 flex-shrink-0" p.bs_player_id p.bs_player_name)
      p.bs_player_id
      (escape_html (normalize_name p.bs_player_name))
      (escape_html (Option.value ~default:"-" p.bs_position))
	    in
	    let pm_cell = Printf.sprintf {html|<span class="%s">%s</span>|html} pm_cls (escape_html pm_str) in
	    
	    let shot_cell made att pct =
	      if att <= 0 then
	        {html|<span class="text-slate-400 dark:text-slate-600">-</span>|html}
	      else
	        Printf.sprintf
	          {html|<span class="text-slate-600 dark:text-slate-400 text-xs">%d-%d (%.1f%%)</span>|html}
	          made att pct
	    in
	    let fg_cell = shot_cell p.bs_fg_made p.bs_fg_att p.bs_fg_pct in
	    let fg3_cell = shot_cell p.bs_fg3_made p.bs_fg3_att p.bs_fg3_pct in
	    let ft_cell = shot_cell p.bs_ft_made p.bs_ft_att p.bs_ft_pct in

	    [
	      player_cell;
	      Printf.sprintf "%.1f" p.bs_minutes;
      string_of_int p.bs_pts;
      pm_cell;
      string_of_int p.bs_reb;
      string_of_int p.bs_ast;
      string_of_int p.bs_stl;
      string_of_int p.bs_blk;
      string_of_int p.bs_tov;
      fg_cell;
      fg3_cell;
      ft_cell;
    ]
  )
 in
 
 let table = render_fixed_table ~id:"boxscore-table" ~min_width:"min-w-[1020px]" ~cols rows_data in
 
 Printf.sprintf
  {html|<div class="space-y-3"><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 flex items-center gap-2"><span class="w-1 h-6 bg-orange-500 rounded-full"></span>%s</h3>%s</div>|html}
  (escape_html title) table

(** Render quarter flow section for boxscore page *)
let quarter_flow_section ?(lang=I18n.Ko) ~home_name ~away_name (quarters: quarter_score list) =
  if quarters = [] then ""
  else
    let tr = I18n.t lang in
    let label_sum = tr { ko = "누적"; en = "Sum" } in
    let label_q = tr { ko = "Q"; en = "Q" } in
    let label_diff = tr { ko = "득실"; en = "Diff" } in
    let label_heading = tr { ko = "쿼터별 흐름"; en = "Quarter Flow" } in
    let title_q_points = tr { ko = "이번 쿼터 득점"; en = "Points in this quarter" } in
    let title_q_diff = tr { ko = "이번 쿼터 득실(홈-원정)"; en = "Quarter diff (home-away)" } in

    let cols = [
      col (tr { ko = "Q"; en = "QTR" });
      col (home_name ^ " " ^ label_sum) ~w:(px 60) ~align:`Right ~highlight:true;
      col label_q ~w:(px 60) ~align:`Right ~resp:`Hidden_sm ~title:title_q_points;
      col "" ~w:(px 40) ~align:`Center;
      col (away_name ^ " " ^ label_sum) ~w:(px 60) ~align:`Right ~highlight:true;
      col label_q ~w:(px 60) ~align:`Right ~resp:`Hidden_sm ~title:title_q_points;
      col label_diff ~w:(px 80) ~align:`Center ~title:title_q_diff;
    ] in

    let rows_data =
      List.mapi (fun i q ->
        let prev_home = if i = 0 then 0 else (List.nth quarters (i - 1)).qs_home_score in
        let prev_away = if i = 0 then 0 else (List.nth quarters (i - 1)).qs_away_score in
        let home_q = q.qs_home_score - prev_home in
        let away_q = q.qs_away_score - prev_away in
        let diff = home_q - away_q in
        let diff_str = if diff > 0 then Printf.sprintf "+%d" diff else string_of_int diff in
        let diff_cls =
          if diff > 0 then "text-sky-600 dark:text-sky-400 font-bold"
          else if diff < 0 then "text-orange-600 dark:text-orange-400 font-bold"
          else "text-slate-400 dark:text-slate-500"
        in
        let flow_indicator =
          Printf.sprintf {html|<span class="%s font-mono">%s</span>|html} diff_cls diff_str
        in
        let period_label =
          match q.qs_period with
          | "Q1" -> "1Q" | "Q2" -> "2Q" | "Q3" -> "3Q" | "Q4" -> "4Q"
          | "X1" -> "OT1" | "X2" -> "OT2" | p -> p
        in
        let home_q_cell = Printf.sprintf {html|<span class="font-mono text-xs text-slate-400">%d</span>|html} home_q in
        let away_q_cell = Printf.sprintf {html|<span class="font-mono text-xs text-slate-400">%d</span>|html} away_q in
        let vs_cell = {html|<span class="text-slate-500 dark:text-slate-400">vs</span>|html} in
        [
          Printf.sprintf {html|<span class="font-bold text-slate-700 dark:text-slate-300">%s</span>|html} period_label;
          string_of_int q.qs_home_score;
          home_q_cell;
          vs_cell;
          string_of_int q.qs_away_score;
          away_q_cell;
          flow_indicator;
        ]
      ) quarters
    in

    let table = render_fixed_table ~id:"quarter-flow" ~min_width:"w-full" ~cols rows_data in

    Printf.sprintf
      {html|<div class="max-w-2xl mx-auto bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800 p-4">
       <div class="flex items-center gap-2 mb-3">
        <span class="text-xs font-bold text-slate-700 dark:text-slate-300 uppercase tracking-wider">%s</span>
       </div>
       %s
      </div>|html}
      (escape_html label_heading)
      table

let boxscore_pbp_link_html ?(lang=I18n.Ko) game_id =
  let tr = I18n.t lang in
  let label = tr { ko = "문자중계"; en = "Play-by-play" } in
  Printf.sprintf
    {html|<a href="/boxscore/%s/pbp" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">%s</a>|html}
    (escape_html game_id)
    (escape_html label)

let boxscore_flow_link_html ?(lang=I18n.Ko) game_id =
  let tr = I18n.t lang in
  let label = tr { ko = "득점흐름"; en = "Scoring flow" } in
  Printf.sprintf
    {html|<a href="/boxscore/%s/flow" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition">%s</a>|html}
    (escape_html game_id)
    (escape_html label)

let boxscore_disabled_chip_html text =
  Printf.sprintf
    {html|<span class="px-2 py-1 rounded-full bg-slate-50 dark:bg-slate-900/40 border border-slate-200 dark:border-slate-800 text-[10px] font-mono tracking-wider text-slate-500 dark:text-slate-500 cursor-default">%s</span>|html}
    (escape_html text)

let boxscore_pbp_chip_html ?(lang=I18n.Ko) ~has_pbp game_id =
  let tr = I18n.t lang in
  if has_pbp then
    boxscore_pbp_link_html ~lang game_id
  else
    boxscore_disabled_chip_html (tr { ko = "문자중계 없음"; en = "No play-by-play" })

let boxscore_flow_chip_html ?(lang=I18n.Ko) ~has_pbp game_id =
  let tr = I18n.t lang in
  if has_pbp then
    boxscore_flow_link_html ~lang game_id
  else
    boxscore_disabled_chip_html (tr { ko = "득점흐름 없음"; en = "No scoring flow" })

let boxscore_data_notes_html ~official_link =
  Printf.sprintf
    {html|<details class="max-w-2xl mx-auto bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-xs text-slate-600 dark:text-slate-400">
    <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">출처 / 검증 기준</summary>
    <div class="mt-2 space-y-1 leading-relaxed">
     <div>점수차는 <span class="font-mono text-slate-900 dark:text-slate-200">홈팀 점수 - 원정팀 점수</span>입니다.</div>
     <div class="pt-1 text-slate-600 dark:text-slate-400 font-bold">출처</div>
     <div>• 스코어: WKBL 공식 경기 결과 페이지 %s</div>
     <div>• 박스스코어: WKBL 공식 박스스코어(선수 기록)</div>
     <div>• 개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>: WKBL 문자중계(일부 경기만 제공)</div>
     <div class="pt-1 text-slate-600 dark:text-slate-400 font-bold">검증 기준</div>
     <div>• <span class="font-mono text-slate-900 dark:text-slate-200">일치 (✓)</span>: 최종 스코어와 양 팀 선수 득점 합계가 모두 맞습니다.</div>
     <div>• <span class="font-mono text-slate-900 dark:text-slate-200">추정 (Σ)</span>: 최종 스코어 또는 득점 합계가 일부 비어 있어 교차 확인이 어렵습니다. 화면에는 가능한 데이터로 계산한 스코어를 표시합니다.</div>
     <div>• <span class="font-mono text-slate-900 dark:text-slate-200">불일치 (!)</span>: 최종 스코어와 선수 득점 합계가 모두 있는데 값이 다릅니다. (<a href="/qa" class="text-orange-600 dark:text-orange-400 hover:underline">데이터 점검</a>)</div>
     <div class="pt-1">※ 이 검증은 “최종 득점”만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 검증이 필요합니다.</div>
     <div>※ 동명이인 매칭 오류로 같은 기록 줄이 중복될 수 있어, 같은 줄은 1개만 표시합니다.</div>
    </div>
   </details>|html}
   official_link

let boxscore_page ?(lang=I18n.Ko) (bs: game_boxscore) =
 let tr = I18n.t lang in
 let label_home = tr { ko = "홈"; en = "Home" } in
 let label_away = tr { ko = "원정"; en = "Away" } in
 let label_scheduled = tr { ko = "예정"; en = "Scheduled" } in
 let label_diff = tr { ko = "점수차"; en = "Diff" } in
 let label_points_unit = tr { ko = "점"; en = "" } in
 let label_summary = tr { ko = "요약"; en = "Summary" } in
 let label_stats_missing =
  tr {
    ko = "선수 기록이 아직 없습니다. 잠시 후 다시 확인해 주세요.";
    en = "Player stats aren't available yet. Please check back later.";
  }
 in
 let gi = bs.boxscore_game in
 let has_player_stats =
  bs.boxscore_home_players <> [] || bs.boxscore_away_players <> []
 in
 let has_scores = gi.gi_home_score > 0 && gi.gi_away_score > 0 in
 let is_scheduled = (not has_scores) && (not has_player_stats) in
 let margin = gi.gi_home_score - gi.gi_away_score in
 let margin_str =
  if margin > 0 then Printf.sprintf "+%.0d" margin else if margin < 0 then Printf.sprintf "%+.0d" margin else "0"
 in
 let margin_class =
  if margin > 0 then "text-sky-600 dark:text-sky-400"
  else if margin < 0 then "text-orange-600 dark:text-orange-400"
  else "text-slate-600 dark:text-slate-400"
 in
 let margin_badge =
  if is_scheduled then
    Printf.sprintf
      {html|<span class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-600 dark:text-slate-400">%s</span>|html}
      (escape_html label_scheduled)
  else
    Printf.sprintf
      {html|<span class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider %s">%s %s%s</span>|html}
      margin_class
      (escape_html label_diff)
      (escape_html margin_str)
      (escape_html label_points_unit)
 in
 let quality_badge = if is_scheduled then "" else score_quality_badge ~lang gi.gi_score_quality in
	 let official_link =
	  match wkbl_official_game_result_url ~game_id:gi.gi_game_id ~game_date:gi.gi_game_date with
	  | None -> ""
	  | Some url ->
    Printf.sprintf
     {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
	     (escape_html url)
		 in
			 let pbp_periods =
			   if is_scheduled then []
			   else
			     match Db.get_pbp_periods ~game_id:gi.gi_game_id () with
			     | Ok ps -> ps
			     | Error _ -> []
			 in
			 let has_pbp = pbp_periods <> [] in
			 let pbp_link = if is_scheduled then "" else boxscore_pbp_chip_html ~lang ~has_pbp gi.gi_game_id in
			 let flow_link = if is_scheduled then "" else boxscore_flow_chip_html ~lang ~has_pbp gi.gi_game_id in
			 let data_notes = if is_scheduled then "" else boxscore_data_notes_html ~official_link in
			 let home_table =
			  if bs.boxscore_home_players = [] then ""
			  else boxscore_player_table gi.gi_home_team_name bs.boxscore_home_players
		 in
		 let away_table =
		  if bs.boxscore_away_players = [] then ""
		  else boxscore_player_table gi.gi_away_team_name bs.boxscore_away_players
		 in
 (* Quarter flow from PBP data *)
 let quarters = match Db.get_quarter_scores gi.gi_game_id with
  | Ok qs -> qs
  | Error _ -> []
 in
 let quarter_section = quarter_flow_section ~lang ~home_name:gi.gi_home_team_name ~away_name:gi.gi_away_team_name quarters in
	 (* Game summary (guarded to avoid making up results) *)
	 let ai_summary = Ai.generate_game_summary ~lang bs in
	 let stats_notice =
	  if has_scores && not has_player_stats then
	    Printf.sprintf
	      {html|<div class="max-w-2xl mx-auto bg-slate-50 dark:bg-slate-900/40 border border-slate-200 dark:border-slate-800 rounded-lg p-4 text-sm text-slate-700 dark:text-slate-300">%s</div>|html}
	      (escape_html label_stats_missing)
	  else ""
	 in
	 let ai_summary_section =
	  Printf.sprintf
	   {html|<div class="max-w-2xl mx-auto bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 border border-indigo-200 dark:border-indigo-800/50 rounded-lg p-4">
	    <div class="flex items-center gap-2 mb-2">
	     <span class="text-lg">📰</span>
	     <span class="text-xs font-bold text-indigo-700 dark:text-indigo-300 uppercase tracking-wider">%s</span>
	    </div>
	    <p class="text-sm text-slate-700 dark:text-slate-300 leading-relaxed">%s</p>
	   </div>%s|html}
	   (escape_html label_summary)
	   (escape_html ai_summary)
	   stats_notice
	 in
	 let home_score_display = if has_scores then string_of_int gi.gi_home_score else "-" in
	 let away_score_display = if has_scores then string_of_int gi.gi_away_score else "-" in
	 layout ~lang ~title:(Printf.sprintf "박스스코어: %s vs %s" gi.gi_home_team_name gi.gi_away_team_name)
	  ~content:(Printf.sprintf
	   {html|<div class="space-y-8 animate-fade-in"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-2xl"><div class="flex flex-col items-center gap-6"><div class="text-slate-600 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div><div class="flex items-center justify-between w-full max-w-2xl gap-2 sm:gap-6"><div class="flex flex-col items-center gap-2 sm:gap-3 flex-shrink-0"><div class="text-sm sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-1 sm:gap-3">%s<a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a></div><div class="text-[10px] sm:text-sm text-slate-600 dark:text-slate-400">%s</div></div><div class="flex items-center gap-2 sm:gap-8"><div class="text-3xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%s</div><div class="flex flex-col items-center gap-1 sm:gap-2"><div class="text-base sm:text-2xl text-slate-700 dark:text-slate-300 font-light">vs</div><div class="flex flex-wrap items-center justify-center gap-1 sm:gap-2">%s%s%s%s</div></div><div class="text-3xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%s</div></div><div class="flex flex-col items-center gap-2 sm:gap-3 flex-shrink-0"><div class="text-sm sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-1 sm:gap-3"><a href="/team/%s" class="hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a>%s</div><div class="text-[10px] sm:text-sm text-slate-600 dark:text-slate-400">%s</div></div></div></div></div>%s%s%s<div class="grid grid-cols-1 gap-8">%s%s</div><div class="flex justify-center"><a href="/games" class="text-slate-600 dark:text-slate-400 hover:text-orange-500 transition text-sm">← 경기 목록</a></div></div>|html}
   (escape_html gi.gi_game_date)
   (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_home_team_name) (Uri.pct_encode gi.gi_home_team_name) (escape_html gi.gi_home_team_name)
   (escape_html label_home)
   (escape_html home_score_display)
   margin_badge
   quality_badge
   pbp_link
   flow_link
   (escape_html away_score_display)
   (Uri.pct_encode gi.gi_away_team_name) (escape_html gi.gi_away_team_name) (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_away_team_name)
   (escape_html label_away)
   ai_summary_section
   quarter_section
   data_notes
   home_table away_table) ()

let pbp_period_label =
 function
 | "Q1" -> "1Q"
 | "Q2" -> "2Q"
 | "Q3" -> "3Q"
 | "Q4" -> "4Q"
 | "X1" -> "OT1"
 | "X2" -> "OT2"
 | "X3" -> "OT3"
 | "X4" -> "OT4"
 | p -> p

let pbp_page ?(lang=I18n.Ko) ~(game: game_info) ~(periods: string list) ~(selected_period: string) ~(events: pbp_event list) () =
 let tr = I18n.t lang in
 let official_link =
  match wkbl_official_game_result_url ~game_id:game.gi_game_id ~game_date:game.gi_game_date with
  | None -> ""
  | Some url ->
    Printf.sprintf
     {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
     (escape_html url)
 in
 let tabs =
  match periods with
  | [] ->
    ""
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
     | 1 -> (game.gi_away_team_name, tr { ko = "원정"; en = "AWAY" })
     | 2 -> (game.gi_home_team_name, tr { ko = "홈"; en = "HOME" })
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
       let title =
         tr
           {
             ko = Printf.sprintf "원정 %s / 홈 %s" game.gi_away_team_name game.gi_home_team_name;
             en = Printf.sprintf "AWAY %s / HOME %s" game.gi_away_team_name game.gi_home_team_name;
           }
       in
       let text =
         tr { ko = Printf.sprintf "원정 %d - %d 홈" a h; en = Printf.sprintf "A %d - %d H" a h }
       in
       Printf.sprintf
        {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap" title="%s">%s</span>|html}
        (escape_html title)
        (escape_html text)
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
	        <div class="font-bold text-slate-900 dark:text-slate-200 mb-2">문자중계가 없어요</div>
	        <div class="leading-relaxed">
	          WKBL에서 문자중계를 제공하지 않았거나, 아직 준비되지 않은 경기입니다.
	          개인 <span class="font-mono text-slate-900 dark:text-slate-200">+/-</span> 계산도 문자중계 기반이라 이 경기에는 표시되지 않을 수 있습니다.
	        </div>
	      </div>|html}
	  | _ ->
	    if events = [] then
	      Printf.sprintf
	        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">%s
	          <div class="p-6 text-slate-600 dark:text-slate-400 text-sm">
	            <div class="font-bold text-slate-900 dark:text-slate-200 mb-2">이 쿼터에는 기록이 없어요</div>
	            <div class="leading-relaxed">다른 쿼터를 눌러보거나, WKBL 원본 링크가 있으면 확인해 주세요.</div>
	          </div>
	        </div>|html}
	        tabs
	    else
	      Printf.sprintf
	        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">%s<ul class="divide-y divide-slate-800/60">%s</ul></div>|html}
	        tabs
	        rows
	 in
 layout
  ~title:(Printf.sprintf "문자중계: %s vs %s" game.gi_home_team_name game.gi_away_team_name)
  ~content:(Printf.sprintf
   {html|<div class="space-y-6 animate-fade-in">
    <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-4">
     <div class="space-y-2">
      <div class="text-slate-600 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div>
      <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">문자중계</h2>
      <div class="flex flex-wrap items-center gap-2 text-sm">
       %s
       <span class="text-slate-600">vs</span>
       %s
       <span class="text-slate-600 dark:text-slate-400 font-mono">%d - %d</span>
      </div>
     </div>
     <div class="flex items-center gap-3">
      %s
      <a href="/boxscore/%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:text-slate-900 dark:hover:text-white transition">← 박스스코어</a>
     </div>
    </div>
    <div class="text-xs text-slate-600 dark:text-slate-400 leading-relaxed">
     문자중계의 득점 표기는 일반적으로 <span class="font-mono text-slate-900 dark:text-slate-200">A(원정)</span> / <span class="font-mono text-slate-900 dark:text-slate-200">H(홈)</span> 순서입니다.
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
   body) ()

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

let compare_table_empty ?(message="비교 대상을 선택하세요.") () =
 Printf.sprintf
  {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-sm text-slate-600 dark:text-slate-400">선택: %s</div>|html}
  (escape_html message)

let compare_table_row ?(signed=false) label val1 val2 =
 let max_val = max (abs_float val1) (abs_float val2) in
 let pct1 = if max_val = 0.0 then 0.0 else abs_float val1 /. max_val *. 100.0 in
 let pct2 = if max_val = 0.0 then 0.0 else abs_float val2 /. max_val *. 100.0 in
 let value_str v =
  if signed && v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v
 in
 Printf.sprintf
  {html|<tr class="border-b border-slate-200 dark:border-slate-800/60">
    <td class="px-3 py-2 text-left font-sans text-slate-600 dark:text-slate-400 whitespace-nowrap">%s</td>
    <td class="px-3 py-2 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap">%s</td>
    <td class="px-3 py-2 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap">%s</td>
    <td class="px-3 py-2">
      <div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
        <div class="flex justify-end w-1/2 border-r border-slate-300 dark:border-slate-700">
          <div class="bg-orange-500 h-full" style="width: %.1f%%"></div>
        </div>
        <div class="flex justify-start w-1/2">
          <div class="bg-sky-500 h-full" style="width: %.1f%%"></div>
        </div>
      </div>
    </td>
  </tr>|html}
  (escape_html label)
  (escape_html (value_str val1))
  (escape_html (value_str val2))
  pct1 pct2

let compare_table_fragment ?(lang=I18n.Ko) ~left_label ~right_label rows =
 let tr = I18n.t lang in
 let rows_html =
  rows
  |> List.map (fun (label, v1, v2, signed) -> compare_table_row ~signed label v1 v2)
  |> String.concat "\n"
 in
 Printf.sprintf
  {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
    <table class="w-full text-sm table-fixed font-mono tabular-nums" aria-label="비교 테이블">
      <colgroup>
        <col style="width: 140px;">
        <col style="width: 90px;">
        <col style="width: 90px;">
        <col style="width: auto;">
      </colgroup>
      <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-600 dark:text-slate-400 text-[10px] uppercase tracking-wider">
        <tr>
          <th class="px-3 py-2 text-left font-sans">%s</th>
          <th class="px-3 py-2 text-right font-sans">%s</th>
          <th class="px-3 py-2 text-right font-sans">%s</th>
          <th class="px-3 py-2 text-right font-sans">%s</th>
        </tr>
      </thead>
      <tbody>%s</tbody>
    </table>
  </div>|html}
  (escape_html (tr { ko = "항목"; en = "Stat" }))
  (escape_html left_label)
  (escape_html right_label)
  (escape_html (tr { ko = "그래프"; en = "Graph" }))
  rows_html

let h2h_game_row (g: h2h_game) =
 let diff_color = if g.score_diff > 0 then "bg-orange-100 dark:bg-orange-900/30 text-orange-600 dark:text-orange-400"
          else if g.score_diff < 0 then "bg-sky-100 dark:bg-sky-900/30 text-sky-600 dark:text-sky-400"
          else "bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400" in
 Printf.sprintf
  {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 font-mono transition-colors group">
   <td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-xs w-[100px] truncate group-hover:text-slate-900 dark:group-hover:text-slate-200 transition-colors">%s</td>
   <td class="px-3 py-2 text-right font-bold text-slate-900 dark:text-slate-200 w-[60px]">%d</td>
   <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-center text-slate-600 dark:text-slate-400 text-xs font-sans w-[40px]">vs</td>
   <td class="px-3 py-2 text-left font-bold text-slate-900 dark:text-slate-200 w-[60px]">%d</td>
   <td class="px-3 py-2 text-left text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-left text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-right text-xs font-sans w-[80px]"><span class="px-1.5 py-0.5 rounded %s font-bold">%+d</span></td>
  </tr>|html}
  (escape_html g.hg_game_date) g.player1_pts g.player1_reb g.player1_ast g.player2_pts g.player2_reb g.player2_ast diff_color g.score_diff

let h2h_game_table (p1_name: string) (p2_name: string) (games: h2h_game list) =
 let rows = games |> List.map h2h_game_row |> String.concat "\n" in
 Printf.sprintf {html|<div class="space-y-3 mt-8"><h3 class="text-center text-slate-600 dark:text-slate-400 text-sm font-bold tracking-wider">맞대결 기록</h3><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-xl"><table class="w-full text-sm table-fixed" aria-label="선수 맞대결 기록">
  <colgroup>
    <col style="width: 100px;"> <!-- Date -->
    <col style="width: 60px;">  <!-- P1 PTS -->
    <col style="width: 60px;">  <!-- REB -->
    <col style="width: 60px;">  <!-- AST -->
    <col style="width: 40px;">  <!-- vs -->
    <col style="width: 60px;">  <!-- P2 PTS -->
    <col style="width: 60px;">  <!-- REB -->
    <col style="width: 60px;">  <!-- AST -->
    <col style="width: 80px;">  <!-- DIFF -->
  </colgroup>
  <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-[10px] tracking-tighter"><tr><th class="px-3 py-2 text-left font-sans">날짜</th><th class="px-3 py-2 text-right font-sans text-orange-600 dark:text-orange-400">%s 득점</th><th class="px-3 py-2 text-right font-sans">리바</th><th class="px-3 py-2 text-right font-sans">어시</th><th class="px-3 py-2"></th><th class="px-3 py-2 text-left font-sans text-sky-600 dark:text-sky-400">%s 득점</th><th class="px-3 py-2 text-left font-sans">리바</th><th class="px-3 py-2 text-left font-sans">어시</th><th class="px-3 py-2 text-right font-sans">차이</th></tr></thead><tbody>%s</tbody></table></div></div>|html} (escape_html p1_name) (escape_html p2_name) rows

let compare_page
  ?(lang=I18n.Ko)
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
	   {html|<option value="ALL" %s>전체 시즌</option>%s|html}
	   (if selected = "ALL" then "selected" else "")
	   base
 in
 let season_label code =
	  if code = "ALL" then "전체 시즌"
	  else
	   seasons
   |> List.find_opt (fun (s: season_info) -> s.code = code)
   |> Option.map (fun (s: season_info) -> s.name)
   |> Option.value ~default:code
 in
 let season_badge code =
  Printf.sprintf
   {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-600 dark:text-slate-400">%s</span>|html}
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
       <span class="font-mono">경기 %d</span>
       <span class="font-mono">시간 %.1f</span>
       <span class="font-mono">효율 %.1f</span>
      </div>
     </div>
    </div>
    <a href="%s" class="shrink-0 px-3 py-1 rounded-lg border border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800/60 text-xs font-bold %s hover:brightness-125 transition">선택</a>
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
    (* Trend chart *)
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
          <h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">시즌 추이 (PTS)</h3>
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
		     {html|<div class="space-y-8 animate-fade-in"><div class="grid grid-cols-1 md:grid-cols-3 gap-8 items-start"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-orange-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400"><a href="/player/%s">%s</a></div><div class="text-slate-600 dark:text-slate-400">%s</div>%s</div></div><div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6 space-y-6"><div class="text-center space-y-2"><h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">평균 기록</h3><p class="text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed"><span class="font-mono">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 +/-는 문자중계 기반으로 일부 경기에서만 제공됩니다. (데이터가 없으면 -)</p></div>%s%s%s%s%s%s%s%s</div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-sky-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-sky-600 dark:text-sky-400"><a href="/player/%s">%s</a></div><div class="text-slate-600 dark:text-slate-400">%s</div>%s</div></div></div>%s%s%s</div>|html}
     (player_img_tag ~class_name:"w-32 h-32" a.player_id a.name)
     a.player_id
     (escape_html (normalize_name a.name))
     (escape_html a.team_name)
     (season_badge p1_season)
	     (compare_stat_row "득점" a.avg_points b.avg_points)
	     (compare_stat_row ~signed:true "MG" a.avg_margin b.avg_margin)
	     (compare_stat_row "리바운드" a.avg_rebounds b.avg_rebounds)
	     (compare_stat_row "어시스트" a.avg_assists b.avg_assists)
	     (compare_stat_row "스틸" a.avg_steals b.avg_steals)
	     (compare_stat_row "블록" a.avg_blocks b.avg_blocks)
	     (compare_stat_row "턴오버" a.avg_turnovers b.avg_turnovers)
	     (compare_stat_row "EFF" a.efficiency b.efficiency)
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
 layout ~lang ~title:"WKBL 비교" ~canonical_path:"/compare"
  ~description:"WKBL 여자농구 선수 비교 - 두 선수의 스탯을 레이더 차트로 비교하세요."
  ~content:(Printf.sprintf
   {html|<div class="space-y-8">
    <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
     <div>
      <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">선수 비교</h2>
      <p class="text-slate-600 dark:text-slate-400 text-sm">동명이인/표기 차이를 피하려면 <span class="font-mono text-slate-900 dark:text-slate-200">선수 고유번호</span>를 선택해 비교합니다.</p>
      <p class="text-slate-600 dark:text-slate-400 text-xs mt-1">선수 1/2는 시즌을 각각 선택할 수 있습니다. 시즌이 다르면 맞대결 기록은 표시하지 않습니다.</p>
     </div>
     <button id="share-compare-btn" type="button" onclick="shareCompareUrl()" class="hidden md:flex items-center gap-2 px-4 py-2 rounded-lg border border-slate-300 dark:border-slate-600 text-slate-600 dark:text-slate-400 hover:bg-slate-100 dark:hover:bg-slate-800 transition text-sm font-medium" aria-label="비교 링크 복사">
      <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z"></path></svg>
      <span>공유</span>
     </button>
    </div>

    <form action="/compare" method="get" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
     <input type="hidden" id="p1_id" name="p1_id" value="%s">
     <input type="hidden" id="p2_id" name="p2_id" value="%s">
     <div class="grid grid-cols-1 md:grid-cols-6 gap-3">
      <select name="p1_season" aria-label="선수 1 시즌" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-2">%s</select>
      <input name="p1" value="%s" placeholder="선수 1 (이름 검색)" oninput="document.getElementById('p1_id').value='';" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-4">
      <select name="p2_season" aria-label="선수 2 시즌" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-2">%s</select>
      <input name="p2" value="%s" placeholder="선수 2 (이름 검색)" oninput="document.getElementById('p2_id').value='';" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-4">
      <div class="md:col-span-6 flex justify-end">
	       <button type="submit" class="px-4 py-2 rounded-lg bg-orange-500 text-slate-900 dark:text-slate-200 font-bold hover:bg-orange-400 transition">검색</button>
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
	   | None -> candidates_panel ~title:"선수 1" ~slot:`P1 ~accent_btn_class:"text-orange-700" p1_display p1_candidates)
   (match p2_selected with
   | Some p -> selected_card ~accent_border:"border-t-sky-500" ~season_code:p2_season p
	   | None -> candidates_panel ~title:"선수 2" ~slot:`P2 ~accent_btn_class:"text-sky-700" p2_display p2_candidates)
   compare_result_html) ()

(** Season Comparison Page - Compare a player's performance across different seasons *)
let compare_seasons_page
  ?(lang=I18n.Ko)
  ~seasons
  ~player_id
  ~player_name
  ~s1  (* Season 1 code *)
  ~s2  (* Season 2 code *)
  ~(s1_stats: season_stats option)
 ~(s2_stats: season_stats option)
 ~(all_seasons: season_stats list)  (* All available seasons for this player *)
  ~error
  ()
 =
 let season_label code =
  if code = "" then "시즌 선택"
  else
   seasons
   |> List.find_opt (fun (s: season_info) -> s.code = code)
   |> Option.map (fun (s: season_info) -> s.name)
   |> Option.value ~default:code
 in
 let season_options ~selected available_codes =
  available_codes
  |> List.map (fun code ->
    let is_selected = if code = selected then "selected" else "" in
    Printf.sprintf {html|<option value="%s" %s>%s</option>|html}
     code is_selected (escape_html (season_label code)))
  |> String.concat "\n"
 in
 let available_season_codes = List.map (fun (s: season_stats) -> s.ss_season_code) all_seasons in
 let error_html =
  match error with
  | None -> ""
  | Some msg ->
    Printf.sprintf
     {html|<div class="mb-4 p-3 bg-red-100 dark:bg-red-900/20 border border-red-300 dark:border-red-700 rounded-lg text-red-700 dark:text-red-400 text-sm">⚠️ %s</div>|html}
     (escape_html msg)
 in
 let stat_row ?(signed=false) label get_val =
  match s1_stats, s2_stats with
  | Some a, Some b ->
    let v1 = get_val a in
    let v2 = get_val b in
    compare_table_row ~signed label v1 v2
  | _ -> ""
 in
 let comparison_table =
	   match s1_stats, s2_stats with
	  | Some _, Some _ ->
	    Printf.sprintf
	     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden">
	      <div class="grid grid-cols-[1fr_100px_100px] text-sm font-bold text-slate-600 dark:text-slate-400 bg-slate-100 dark:bg-slate-800/50">
	       <div class="p-3">항목</div>
	       <div class="p-3 text-center text-orange-600">%s</div>
	       <div class="p-3 text-center text-sky-600">%s</div>
	      </div>
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
	     </div>|html}
	     (escape_html (season_label s1))
	     (escape_html (season_label s2))
	     (stat_row "경기" (fun s -> float_of_int s.ss_games_played))
	     (stat_row "출전 시간" (fun s -> s.ss_total_minutes))
	     (stat_row ~signed:true "득점" (fun s -> s.ss_avg_points))
	     (stat_row ~signed:true "리바운드" (fun s -> s.ss_avg_rebounds))
	     (stat_row ~signed:true "어시스트" (fun s -> s.ss_avg_assists))
	     (stat_row ~signed:true "스틸" (fun s -> s.ss_avg_steals))
	     (stat_row ~signed:true "블록" (fun s -> s.ss_avg_blocks))
	     (stat_row ~signed:true "턴오버" (fun s -> s.ss_avg_turnovers))
	     (stat_row ~signed:true "효율" (fun s -> s.ss_efficiency))
	     (stat_row ~signed:true "TS%" (fun s -> s.ss_ts_pct *. 100.0))
	     (stat_row ~signed:true "득실마진" (fun s -> s.ss_margin))
	  | _ ->
	    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-8 text-center text-slate-500 dark:text-slate-400">
	     비교할 두 시즌을 선택하세요.
	    </div>|html}
 in
 let trend_indicator (s: season_stats) =
  if s.ss_efficiency >= 15.0 then "🔥"
  else if s.ss_efficiency >= 10.0 then "📈"
  else if s.ss_efficiency >= 5.0 then "➖"
  else "📉"
 in
 let season_card (s: season_stats) accent =
  Printf.sprintf
	   {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 border-t-4 %s">
	    <div class="flex items-center justify-between mb-3">
	     <span class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</span>
	     <span class="text-2xl">%s</span>
	    </div>
	    <div class="grid grid-cols-2 gap-2 text-sm">
	     <div class="text-slate-600 dark:text-slate-400">경기</div>
	     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%d</div>
	     <div class="text-slate-600 dark:text-slate-400">득점</div>
	     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%.1f</div>
     <div class="text-slate-600 dark:text-slate-400">리바</div>
     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%.1f</div>
     <div class="text-slate-600 dark:text-slate-400">어시</div>
     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%.1f</div>
     <div class="text-slate-600 dark:text-slate-400">효율</div>
     <div class="text-right font-mono font-bold text-slate-900 dark:text-slate-200">%.1f</div>
    </div>
   </div>|html}
   accent
   (escape_html s.ss_season_name)
   (trend_indicator s)
   s.ss_games_played
   s.ss_avg_points
   s.ss_avg_rebounds
   s.ss_avg_assists
   s.ss_efficiency
 in
 let season_cards_html =
  let s1_card = match s1_stats with
   | Some s -> season_card s "border-t-orange-500"
   | None -> ""
  in
  let s2_card = match s2_stats with
   | Some s -> season_card s "border-t-sky-500"
   | None -> ""
  in
  if s1_card <> "" || s2_card <> "" then
   Printf.sprintf {html|<div class="grid grid-cols-1 md:grid-cols-2 gap-4 mb-6">%s%s</div>|html} s1_card s2_card
  else ""
 in
 let content = Printf.sprintf
  {html|<div class="max-w-4xl mx-auto py-8 px-4">
   <div class="mb-6">
    <a href="/player/%s" class="text-orange-500 hover:underline text-sm">← 선수 프로필로</a>
   </div>

   <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200 mb-2">
    📊 시즌 비교
   </h1>
   <p class="text-slate-600 dark:text-slate-400 mb-6">
    <a href="/player/%s" class="text-orange-500 hover:underline font-semibold">%s</a>의 시즌별 기록을 비교합니다.
   </p>

   %s

   <form method="GET" action="/compare/seasons" class="mb-6 bg-slate-100 dark:bg-slate-800/50 rounded-xl p-4">
    <input type="hidden" name="player" value="%s">
    <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
     <div>
      <label class="block text-sm font-bold text-slate-700 dark:text-slate-300 mb-2">시즌 1 (주황)</label>
      <select name="s1" class="w-full px-3 py-2 rounded-lg border border-slate-300 dark:border-slate-700 bg-white dark:bg-slate-900 text-slate-900 dark:text-slate-200">
       <option value="">시즌 선택</option>
       %s
      </select>
     </div>
     <div>
      <label class="block text-sm font-bold text-slate-700 dark:text-slate-300 mb-2">시즌 2 (파랑)</label>
      <select name="s2" class="w-full px-3 py-2 rounded-lg border border-slate-300 dark:border-slate-700 bg-white dark:bg-slate-900 text-slate-900 dark:text-slate-200">
       <option value="">시즌 선택</option>
       %s
      </select>
     </div>
    </div>
    <button type="submit" class="mt-4 w-full md:w-auto px-6 py-2 bg-orange-500 hover:bg-orange-600 text-white font-bold rounded-lg transition-colors">
     시즌 비교
    </button>
   </form>

   %s

   %s

   <div class="mt-8 text-center">
    <a href="/compare?p1=%s" class="text-orange-500 hover:underline text-sm">
     다른 선수와 비교 →
    </a>
   </div>
  </div>|html}
  (escape_html player_id)
  (escape_html player_id)
  (escape_html player_name)
  error_html
  (escape_html player_id)
  (season_options ~selected:s1 available_season_codes)
  (season_options ~selected:s2 available_season_codes)
  season_cards_html
  comparison_table
  (Uri.pct_encode player_name)
 in
 layout
  ~lang
  ~title:(Printf.sprintf "WKBL 시즌 비교 - %s" player_name)
  ~canonical_path:(Printf.sprintf "/compare/seasons?player=%s" (Uri.pct_encode player_id))
  ~description:(Printf.sprintf "%s의 시즌별 성적 비교 - WKBL 여자농구 분석" player_name)
  ~content ()

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
 
 (* Margin Logic *)
 let margin = abs_float result.predicted_margin in
 let spread_str = Printf.sprintf "%.1f" margin in
 let spread_badge =
   if margin < 1.5 then "초접전"
   else if margin < 5.5 then "접전"
   else if margin < 10.5 then "우세"
   else "압승"
 in
 let spread_class = 
   if margin < 1.5 then "bg-rose-100 text-rose-700 dark:bg-rose-900/30 dark:text-rose-300"
   else if margin < 5.5 then "bg-amber-100 text-amber-700 dark:bg-amber-900/30 dark:text-amber-300"
   else "bg-emerald-100 text-emerald-700 dark:bg-emerald-900/30 dark:text-emerald-300"
 in

 (* Generate AI explanation *)
 let ai_explanation = Ai.get_explanation ~home ~away output in
 let context_card_html, context_note_html =
  match breakdown.pb_context with
	  | None ->
	    ("",
	     {html|<div>기본 모델(전력/득실 기대/기록)만 사용합니다.</div>
		     <div class="text-slate-600 dark:text-slate-400">옵션을 켜면 최근 5경기 흐름/주요 선수 출전/휴식일을 조금 반영합니다. (부상/전술/실시간 상황은 반영하지 못합니다.)</div>|html})
  | Some ctx ->
    let delta_pp = ctx.pcb_delta *. 100.0 in
    let delta_cls =
     if delta_pp > 0.0 then "text-sky-600 dark:text-sky-400"
     else if delta_pp < 0.0 then "text-rose-600 dark:text-rose-400"
     else "text-slate-700 dark:text-slate-300"
    in
    let delta_str =
     if delta_pp > 0.0 then Printf.sprintf "+%.1f%%" delta_pp else Printf.sprintf "%.1f%%" delta_pp
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
     | Some h, Some a -> Printf.sprintf "%d일 vs %d일" h a
     | _ -> "-"
    in
    ( Printf.sprintf
      {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
	       <div class="flex items-center justify-between">
	        <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">컨텍스트</div>
	        <div class="text-[10px] font-mono %s">%s</div>
	       </div>
	       <div class="mt-2 space-y-1 text-[10px] text-slate-600 dark:text-slate-400 font-mono">
	        <div class="flex justify-between"><span>최근 5경기 승률</span><span class="text-slate-700 dark:text-slate-300">%.0f%% vs %.0f%%</span></div>
	        <div class="flex justify-between"><span>코어 로스터</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
	        <div class="flex justify-between"><span>휴식</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
	       </div>
	      </div>|html}
      delta_cls
      (escape_html delta_str)
      form_home_pct
      form_away_pct
      (escape_html roster_text)
      (escape_html rest_text),
	     {html|<div>최근 5경기 흐름/주요 선수 출전/휴식일을 조금 반영합니다. (반영 폭은 작게 제한합니다.)</div><div class="text-slate-600 dark:text-slate-400">주요 선수는 “최근 경기 출전” 기준으로 추정합니다. 부상/전술/실시간 상황은 반영하지 못합니다.</div>|html}
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
     <div class="text-sm text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest mb-1">예측</div>
     <div class="text-3xl font-black text-slate-900 dark:text-slate-200 tracking-tight">%s</div>
     <div class="mt-1 text-xs text-slate-500 dark:text-slate-400 font-medium truncate flex items-center gap-2">
	       <span>%s 대 %s</span>
	       <span class="px-1.5 py-0.5 rounded text-[10px] font-bold %s">%s</span>
	     </div>
	    </div>
	    <div class="flex gap-4 text-right shrink-0">
	     <div>
	       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase font-bold">총점</div>
	       <div class="text-xl font-black font-mono text-slate-800 dark:text-slate-300">%.1f</div>
	     </div>
	     <div>
	       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase font-bold">점수차</div>
	       <div class="text-xl font-black font-mono text-slate-800 dark:text-slate-300">+%s</div>
	     </div>
	     <div>
	       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase font-bold">승리 팀</div>
	       <div class="text-xl font-black %s">%s</div>
	     </div>
	    </div>
	   </div>
   <div class="space-y-3 pt-2">
    <div class="flex justify-between text-[10px] font-black uppercase tracking-widest text-slate-500">
     <span class="flex items-center gap-1.5"><span class="w-2 h-2 rounded-full bg-orange-500"></span>%s</span>
     <span class="flex items-center gap-1.5">%s<span class="w-2 h-2 rounded-full bg-sky-500"></span></span>
    </div>
    <div class="relative h-4 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden shadow-inner border border-slate-200/50 dark:border-slate-700/30">
     <div class="absolute top-0 left-1/2 -ml-px w-0.5 h-full bg-slate-300 dark:bg-slate-600 z-10 opacity-30"></div>
     <div class="flex h-full w-full">
      <div class="h-full transition-all duration-1000 ease-out bg-gradient-to-r from-orange-600 to-orange-400 shadow-[0_0_15px_rgba(249,115,22,0.3)]" style="width: %.1f%%"></div>
      <div class="h-full transition-all duration-1000 ease-out bg-gradient-to-l from-sky-600 to-sky-400 shadow-[0_0_15px_rgba(14,165,233,0.3)]" style="width: %.1f%%"></div>
     </div>
    </div>
    <div class="flex justify-between font-mono">
	     <div class="flex flex-col">
	       <span class="text-2xl font-black text-orange-600 dark:text-orange-400 leading-none">%.1f%%</span>
	       <span class="text-[9px] text-slate-400 font-bold uppercase mt-1">승리 확률</span>
	     </div>
	     <div class="flex flex-col items-end">
	       <span class="text-2xl font-black text-sky-600 dark:text-sky-400 leading-none">%.1f%%</span>
	       <span class="text-[9px] text-slate-400 font-bold uppercase mt-1">승리 확률</span>
	     </div>
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
	      <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">전력</div>
	      <div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono">%d경기</div>
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
	     <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">득실 기대</div>
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
	     <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">기록</div>
     <div class="mt-2 flex items-center justify-between font-mono font-bold">
      <div class="text-orange-600 dark:text-orange-400">%.1f%%</div>
      <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
     </div>
	     <div class="mt-1 text-[10px] text-slate-600 dark:text-slate-400 font-mono">승률 + 효율 결합</div>
	    </div>
    %s
   </div>
   <details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-600 dark:text-slate-400">
	    <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">예측 안내</summary>
	    <div class="mt-2 space-y-1 leading-relaxed">
	     <div>종합 확률은 <span class="font-mono text-slate-900 dark:text-slate-200">전력(60%%)</span> + <span class="font-mono text-slate-900 dark:text-slate-200">득실 기대(25%%)</span> + <span class="font-mono text-slate-900 dark:text-slate-200">기록(15%%)</span>을 섞어 계산합니다.</div>
	     <div><span class="font-mono text-slate-900 dark:text-slate-200">전력</span>은 최근 경기 결과(점수차 반영)를 바탕으로 업데이트합니다.</div>
	     <div><span class="font-mono text-slate-900 dark:text-slate-200">득실 기대</span>는 득점/실점으로 기대 승률을 계산해 매치업 확률로 바꿉니다.</div>
	     <div><span class="font-mono text-slate-900 dark:text-slate-200">기록</span>은 승률과 효율을 단순 결합합니다.</div>
	     <div>중립 경기면 홈 이점을 예측에 반영하지 않습니다.</div>
	     %s
	    </div>
	   </details>
  </div>|html}
  (escape_html breakdown.pb_season)
  (escape_html home)
  (escape_html away)
  spread_class
  spread_badge
  result.predicted_total_score
  spread_str
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
	      <span class="text-xs text-slate-500">대</span>
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
	    <h3 class="text-sm font-bold text-slate-600 dark:text-slate-400 uppercase tracking-wider mb-3">📅 예정 경기</h3>
	    <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-2">%s</div>
	   </div>|html}
	   cards

let predict_page ?(lang=I18n.Ko) ~season ~seasons ~teams ~home ~away ~is_neutral ~context_enabled ~include_mismatch ~upcoming ?(games=[]) (result: prediction_output option) (error: string option) =
 let season_options =
  let base =
   seasons
   |> List.map (fun (s : season_info) ->
     let selected = if s.code = season then "selected" else "" in
     Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
   |> String.concat "\n"
  in
  Printf.sprintf {html|<option value="ALL" %s>전체 시즌</option>%s|html} (if season = "ALL" then "selected" else "") base
 in

 let result_html =
  match result, error with
  | _, Some e -> Views_common.error_with_retry ~message:e ()
	  | None, None ->
	    Printf.sprintf
	    {html|<div class="text-slate-600 dark:text-slate-400 text-sm">팀을 선택해 예측을 확인하세요.</div>|html}
	  | Some r, _ ->
      let insight_html =
        if games <> [] then
          Printf.sprintf
            {html|
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mt-6">
                <!-- Home Recent Form -->
                <div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
	                  <h3 class="text-xs font-bold text-slate-500 dark:text-slate-400 uppercase tracking-wider mb-3 flex items-center gap-2">
	                    %s 최근 흐름
	                  </h3>
                  %s
                </div>
                <!-- Away Recent Form -->
                <div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
	                  <h3 class="text-xs font-bold text-slate-500 dark:text-slate-400 uppercase tracking-wider mb-3 flex items-center gap-2">
	                    %s 최근 흐름
	                  </h3>
                  %s
                </div>
              </div>
              %s
            |html}
            (Views_common.team_badge home)
            (Views_common.render_recent_games_for_predict home games 5)
            (Views_common.team_badge away)
            (Views_common.render_recent_games_for_predict away games 5)
            (Views_common.render_h2h_summary home away games season)
        else ""
      in
      (prediction_result_card ~home ~away r) ^ insight_html
 in

 let team_option current name =
  let selected = if normalize_label current = normalize_label name then "selected" else "" in
  Printf.sprintf {html|<option value="%s" %s>%s</option>|html} (escape_html name) selected (escape_html name)
 in
	 let team_options current =
	  let base = teams |> List.map (team_option current) |> String.concat "\n" in
	  Printf.sprintf {html|<option value="" %s>팀 선택…</option>%s|html} (if String.trim current = "" then "selected" else "") base
	 in

 let upcoming_html = upcoming_games_section upcoming in
 let og_image =
   if home <> "" && away <> "" then
     Some (Printf.sprintf "https://wkbl.win/api/og/predict?home=%s&away=%s&season=%s" (Uri.pct_encode home) (Uri.pct_encode away) (Uri.pct_encode season))
   else None
 in
 layout ~lang ~title:"WKBL 예측" ~canonical_path:"/predict"
  ~description:"WKBL 여자농구 경기 예측 - AI 기반 승률 예측과 분석을 확인하세요."
  ?og_image
	  ~content:(Printf.sprintf
	   {html|<div class="space-y-6 animate-fade-in">
	    <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
	     <div>
	      <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">경기 예측</h2>
	      <p class="text-slate-600 dark:text-slate-400 text-sm">예측 결과와 함께 근거 요약을 보여줍니다.</p>
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
	      중립 경기 (홈 이점 없음)
	     </label>
	     <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
	      <input type="checkbox" name="include_mismatch" value="1" class="accent-orange-500" %s>
	      불일치 포함 (최종 스코어와 득점 합계가 다른 경기 포함)
	     </label>
	     <div class="md:col-span-3 flex justify-end">
	      <button type="submit" class="bg-orange-500 hover:bg-orange-400 text-black font-bold px-4 py-2 rounded text-sm transition">예측하기</button>
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
let leader_card ?(player_info_map=None) ?(value_fmt=(fun v -> Printf.sprintf "%.1f" v)) title (leaders: leader_entry list) =
 if leaders = [] then "" 
 else
  let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
  leaders |> List.iter (fun (l: leader_entry) ->
    let key = normalize_name l.le_player_name in
    Hashtbl.replace name_counts key ((Hashtbl.find_opt name_counts key |> Option.value ~default:0) + 1));
  let show_id (l: leader_entry) =
   Hashtbl.find_opt name_counts (normalize_name l.le_player_name) |> Option.map (fun c -> c > 1) |> Option.value ~default:false
  in
  let info_opt player_id =
   match player_info_map with
   | Some map -> Hashtbl.find_opt map player_id
   | None -> None
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
   let disambiguation =
    if show_id l then
     player_disambiguation_line ~team_name:l.le_team_name ~player_id:l.le_player_id (info_opt l.le_player_id)
    else ""
   in
   Printf.sprintf
    {html|<a href="/player/%s" class="flex flex-col items-center %s group cursor-pointer transition-transform duration-200 %s">
     <div class="relative"><div class="%s rounded-full %s ring-2 overflow-hidden group-hover:ring-4 transition-all duration-200">%s</div>
      <div class="absolute -bottom-1 left-1/2 -translate-x-1/2 %s text-xs font-bold w-5 h-5 rounded-full flex items-center justify-center text-white group-hover:scale-110 transition-transform">%d</div>
     </div>
     <div class="text-lg font-bold text-slate-900 dark:text-slate-200 mt-2 group-hover:text-orange-500 transition-colors">%s</div>
     <span class="text-xs text-slate-600 dark:text-slate-400 truncate max-w-[80px]">%s</span>
     %s
    </a>|html}
    l.le_player_id mt hover_scale size bg (player_img_tag ~class_name:(size ^ " object-cover") l.le_player_id l.le_player_name)
    ring rank
    (escape_html (value_fmt l.le_stat_value))
    (escape_html (normalize_name l.le_player_name)) disambiguation
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
     let disambiguation =
      if show_id l then
       player_disambiguation_line ~team_name:l.le_team_name ~player_id:l.le_player_id (info_opt l.le_player_id)
      else ""
     in
     Printf.sprintf {html|<a href="/player/%s" class="flex items-center justify-between py-1.5 border-b border-slate-200/50 dark:border-slate-800/40 last:border-0 hover:bg-slate-100 dark:hover:bg-slate-800/50 -mx-2 px-2 rounded transition-colors group">
      <div class="flex items-center gap-2 min-w-0">
       <span class="text-slate-500 dark:text-slate-500 font-mono text-xs w-4 group-hover:text-orange-500 transition-colors">%d</span>
       %s
       <div class="flex flex-col min-w-0">
        <span class="text-xs text-slate-700 dark:text-slate-300 group-hover:text-orange-500 transition-colors truncate">%s</span>
        %s
       </div>
      </div>
      <span class="font-mono text-xs text-slate-600 dark:text-slate-400 group-hover:font-bold transition-all">%s</span>
     </a>|html}
      l.le_player_id
      (i + 4) (player_img_tag ~class_name:"w-6 h-6" l.le_player_id l.le_player_name)
      (escape_html (normalize_name l.le_player_name)) disambiguation
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
let leader_card_signed ?(player_info_map=None) title leaders =
 let signed v = if v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v in
 leader_card ~player_info_map ~value_fmt:signed title leaders

let leaders_page ?(lang=I18n.Ko) ?(player_info_map=None) ~season ~seasons ~scope (leaders_by_category: (string * leader_entry list) list) =
 let season_options =
  let base =
   seasons
   |> List.map (fun (s: season_info) ->
     let selected = if s.code = season then "selected" else "" in
     Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
   |> String.concat "\n"
  in
	  Printf.sprintf
	   {html|<option value="ALL" %s>전체 시즌</option>%s|html}
	   (if season = "ALL" then "selected" else "")
	   base
 in
 let scope_value = scope |> String.trim |> String.lowercase_ascii in
 let scope_options =
  let opt v label =
   let sel = if scope_value = v then "selected" else "" in
   Printf.sprintf {html|<option value="%s" %s>%s</option>|html} v sel label
  in
  opt "per_game" "경기당" ^ opt "totals" "누적" ^ opt "per_36" "36분 환산"
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
	    ( [ ("경기", "gp", fmt_int)
	     ; ("출전시간", "min", fmt_f1)
	     ; ("득점", "pts", fmt_int)
	     ; ("리바운드", "reb", fmt_int)
	     ; ("어시스트", "ast", fmt_int)
	     ; ("스틸", "stl", fmt_int)
	     ; ("블록", "blk", fmt_int)
	     ; ("턴오버", "tov", fmt_int)
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
	    ( [ ("득점/36", "pts", fmt_f1)
	     ; ("리바운드/36", "reb", fmt_f1)
	     ; ("어시스트/36", "ast", fmt_f1)
	     ; ("스틸/36", "stl", fmt_f1)
	     ; ("블록/36", "blk", fmt_f1)
	     ; ("턴오버/36", "tov", fmt_f1)
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
	    ( [ ("득점", "pts", fmt_f1)
	     ; ("리바운드", "reb", fmt_f1)
	     ; ("어시스트", "ast", fmt_f1)
	     ; ("스틸", "stl", fmt_f1)
	     ; ("블록", "blk", fmt_f1)
	     ; ("턴오버", "tov", fmt_f1)
	     ; ("출전시간", "min", fmt_f1)
	     ; ("EFF", "eff", fmt_f1)
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
    leader_card ~player_info_map ~value_fmt:fmt title (lookup key))
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
  {html|<div class="text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">슈팅 부문은 최소 시도 기준이 있습니다: <span class="font-mono text-slate-700 dark:text-slate-300">FG≥50</span>, <span class="font-mono text-slate-700 dark:text-slate-300">3P≥20</span>, <span class="font-mono text-slate-700 dark:text-slate-300">FT≥20</span>. Per-36 부문은 <span class="font-mono text-slate-700 dark:text-slate-300">MIN≥100</span> 기준입니다.</div>|html}
 in
 let content =
  Printf.sprintf
   {html|<div class="space-y-8">%s%s<div class="flex items-baseline justify-between"><h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">슈팅</h3><div class="text-xs text-slate-600 dark:text-slate-400">FG / 3P / FT / TS / eFG</div></div>%s</div>|html}
   main_grid note_html shooting_grid
 in
 layout
  ~lang
  ~title:"WKBL 리더"
  ~canonical_path:"/leaders"
  ~description:"WKBL 여자농구 리더보드 - 득점, 리바운드, 어시스트, 슛 퍼센트 등 부문별 선두 선수 순위"
  ~content:(Printf.sprintf
	   {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">부문별 1위</h2><p class="text-slate-600 dark:text-slate-400">부문별 선두 선수 기록입니다.</p></div><form action="/leaders" method="get" class="grid grid-cols-1 sm:grid-cols-2 gap-3 w-full md:w-auto"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select><select name="scope" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select></form></div>%s</div>|html}
	   season_options scope_options content)
	  ()

let awards_page ?(lang=I18n.Ko) ~player_info_map ~season ~seasons ~include_mismatch ~prev_season_name ~mvp:mvp_list ~mip:mip_list () =
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
 let include_checked = if include_mismatch then "checked" else "" in
 let mvp_card =
  if mvp_list = [] then
   {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MVP (EFF)</h3><div class="text-slate-600 dark:text-slate-400 text-sm leading-relaxed">표시할 데이터가 없습니다.</div></div>|html}
  else
   leader_card ~player_info_map "MVP (EFF)" mvp_list
 in
 let mip_card =
  match prev_season_name with
  | None ->
    Printf.sprintf
     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MIP (EFF 변화)</h3><div class="text-slate-600 dark:text-slate-400 text-sm leading-relaxed">전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">EFF(경기 기여도) 평균</span> 변화를 계산합니다. 시즌 선택 시에만 표시됩니다.</div></div>|html}
  | Some prev_name ->
    if mip_list = [] then
     Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">MIP (EFF 변화)</h3><div class="text-slate-600 dark:text-slate-400 text-sm leading-relaxed">%s 대비 EFF 변화 계산에 필요한 표본이 부족합니다. (두 시즌 모두 10경기 이상)</div></div>|html}
      (escape_html prev_name)
    else
     leader_card_signed ~player_info_map (Printf.sprintf "MIP (EFF 변화 vs %s)" prev_name) mip_list
 in
 let disclaimer_html =
  {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/60 p-5 text-slate-600 dark:text-slate-400 text-sm leading-relaxed"><div class="font-bold text-slate-700 dark:text-slate-300 mb-2">통계 기반 수상 (비공식)</div><ul class="list-disc list-inside space-y-1"><li>MVP: <span class="font-mono text-slate-700 dark:text-slate-300">EFF(경기 기여도) 평균</span> 상위</li><li>MIP: 전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">EFF 변화</span> 상위 (두 시즌 모두 10경기 이상)</li><li>공식 수상 데이터가 아직 없어서, 현재는 박스스코어 기반 지표로만 추정합니다.</li></ul></div>|html}
 in
 layout
  ~lang
  ~title:"WKBL 수상"
  ~canonical_path:"/awards"
  ~description:"WKBL 여자농구 시상 - MVP, MIP 등 시즌별 통계 기반 어워드 추정 순위"
  ~content:(Printf.sprintf
	   {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">수상</h2><p class="text-slate-600 dark:text-slate-400">통계 기반으로 참고용으로 정리했습니다.</p></div><form action="/awards" method="get" class="flex flex-wrap items-center gap-3"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-40" onchange="this.form.submit()">%s</select><label class="flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" onchange="this.form.submit()" title="최종 스코어와 득점 합계가 다른 경기 포함"><span>불일치 포함</span></label></form></div><div class="grid grid-cols-1 md:grid-cols-2 gap-6">%s%s</div>%s</div>|html}
	   season_options
	   include_checked
   mvp_card
   mip_card
   disclaimer_html) ()

(** Podium-style leader card (Gemini UX feedback: 1-2-3위 시각적 강조) *)
let leader_card ?(player_info_map=None) ?(value_fmt=(fun v -> Printf.sprintf "%.1f" v)) title (leaders: leader_entry list) =
 if leaders = [] then "" 
 else
  let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
  leaders |> List.iter (fun (l: leader_entry) ->
    let key = normalize_name l.le_player_name in
    Hashtbl.replace name_counts key ((Hashtbl.find_opt name_counts key |> Option.value ~default:0) + 1));
  let show_id (l: leader_entry) =
   Hashtbl.find_opt name_counts (normalize_name l.le_player_name) |> Option.map (fun c -> c > 1) |> Option.value ~default:false
  in
  let info_opt player_id =
   match player_info_map with
   | Some map -> Hashtbl.find_opt map player_id
   | None -> None
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
   let disambiguation =
    if show_id l then
     player_disambiguation_line ~team_name:l.le_team_name ~player_id:l.le_player_id (info_opt l.le_player_id)
    else ""
   in
   Printf.sprintf
    {html|<a href="/player/%s" class="flex flex-col items-center %s group cursor-pointer transition-transform duration-200 %s">
     <div class="relative"><div class="%s rounded-full %s ring-2 overflow-hidden group-hover:ring-4 transition-all duration-200">%s</div>
      <div class="absolute -bottom-1 left-1/2 -translate-x-1/2 %s text-xs font-bold w-5 h-5 rounded-full flex items-center justify-center text-white group-hover:scale-110 transition-transform">%d</div>
     </div>
     <div class="text-lg font-bold text-slate-900 dark:text-slate-200 mt-2 group-hover:text-orange-500 transition-colors">%s</div>
     <span class="text-xs text-slate-600 dark:text-slate-400 truncate max-w-[80px]">%s</span>
     %s
    </a>|html}
    l.le_player_id mt hover_scale size bg (player_img_tag ~class_name:(size ^ " object-cover") l.le_player_id l.le_player_name)
    ring rank
    (escape_html (value_fmt l.le_stat_value))
    (escape_html (normalize_name l.le_player_name)) disambiguation
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
     let disambiguation =
      if show_id l then
       player_disambiguation_line ~team_name:l.le_team_name ~player_id:l.le_player_id (info_opt l.le_player_id)
      else ""
     in
     Printf.sprintf {html|<a href="/player/%s" class="flex items-center justify-between py-1.5 border-b border-slate-200/50 dark:border-slate-800/40 last:border-0 hover:bg-slate-100 dark:hover:bg-slate-800/50 -mx-2 px-2 rounded transition-colors group">
      <div class="flex items-center gap-2 min-w-0">
       <span class="text-slate-500 dark:text-slate-500 font-mono text-xs w-4 group-hover:text-orange-500 transition-colors">%d</span>
       %s
       <div class="flex flex-col min-w-0">
        <span class="text-xs text-slate-700 dark:text-slate-300 group-hover:text-orange-500 transition-colors truncate">%s</span>
        %s
       </div>
      </div>
      <span class="font-mono text-xs text-slate-600 dark:text-slate-400 group-hover:font-bold transition-all">%s</span>
     </a>|html}
      l.le_player_id
      (i + 4) (player_img_tag ~class_name:"w-6 h-6" l.le_player_id l.le_player_name)
      (escape_html (normalize_name l.le_player_name)) disambiguation
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

(** Clutch Time Leaderboard Page
  Clutch time = Q4 remaining 5 minutes + score diff <= 5 points *)
let clutch_page ?(lang=I18n.Ko) ~season ~seasons (stats: clutch_stats list) =
 let tr = I18n.t lang in
 let all_seasons_label = tr { ko = "전체 시즌"; en = "All seasons" } in
 let h2_title = tr { ko = "클러치 리더"; en = "Clutch Leaders" } in
 let subtitle =
  tr
   { ko = "4쿼터 5분 이하 + 점수차 5점 이하"
   ; en = "Q4 last 5 minutes and score diff within 5"
   }
 in
 let empty_title = tr { ko = "클러치 기록이 없습니다"; en = "No clutch stats" } in
 let empty_desc =
  tr
   { ko = "클러치 = 4쿼터 5분 이하 + 점수차 5점 이하"
   ; en = "Clutch = Q4 last 5 minutes and score diff within 5"
   }
 in
 let footnote =
  tr
   { ko = "문자중계 기록을 기반으로 집계합니다."
   ; en = "Aggregated from play-by-play records."
   }
 in
 let th_player = tr { ko = "선수"; en = "Player" } in
 let th_team = tr { ko = "팀"; en = "Team" } in
 let season_options =
  let base =
   seasons
   |> List.map (fun (s: season_info) ->
     let selected = if s.code = season then "selected" else "" in
     Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
   |> String.concat "\n"
	  in
		  Printf.sprintf
		   {html|<option value="ALL" %s>%s</option>%s|html}
		   (if season = "ALL" then "selected" else "")
		   (escape_html all_seasons_label)
		   base
 in
 let rows =
  stats
  |> List.mapi (fun _i (s: clutch_stats) ->
    let fg_pct_str = if s.cs_clutch_fg_att > 0
     then Printf.sprintf "%.1f%%" (s.cs_clutch_fg_pct *. 100.0) else "-" in
    Printf.sprintf
     {html|<tr class="border-b border-slate-200 dark:border-slate-700 hover:bg-slate-50 dark:hover:bg-slate-800/50 text-sm font-mono tabular-nums">
      
      <td class="px-3 py-2 font-sans"><a href="/player/%s" class="text-orange-500 hover:underline font-medium">%s</a></td>
      <td class="px-3 py-2 text-slate-600 dark:text-slate-400 font-sans">%s</td>
      <td class="px-3 py-2 text-right w-16">%d</td>
      <td class="px-3 py-2 text-right font-bold text-slate-900 dark:text-slate-200 w-16">%d</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-24">%d-%d</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-20">%s</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-16">%d</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-24">%d-%d</td>
     </tr>|html}
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
		   Printf.sprintf {html|<tr><td colspan="8">%s</td></tr>|html}
		    (Views_common.empty_state ~icon:BasketballIcon
		     empty_title
		     empty_desc)
		  else ""
 in
 let table_head_html =
  Printf.sprintf
   {html|<thead class="bg-slate-100 dark:bg-slate-800 text-slate-700 dark:text-slate-300">
     <tr>
      <th class="px-3 py-2 text-left font-sans">%s</th>
      <th class="px-3 py-2 text-left font-sans">%s</th>
      <th class="px-3 py-2 text-center font-sans" title="클러치 경기">GP</th>
      <th class="px-3 py-2 text-center font-sans" title="클러치 득점">PTS</th>
      <th class="px-3 py-2 text-center font-sans" title="야투 성공-시도">FGM-A</th>
      <th class="px-3 py-2 text-center font-sans" title="야투 성공률">FG%%</th>
      <th class="px-3 py-2 text-center font-sans" title="3점 성공">3PM</th>
      <th class="px-3 py-2 text-center font-sans" title="자유투 성공-시도">FTM-A</th>
     </tr>
    </thead>|html}
   (escape_html th_player)
   (escape_html th_team)
 in
 let table_html =
  Printf.sprintf
   {html|<div class="overflow-x-auto">
     <table class="w-full text-sm table-fixed font-mono tabular-nums">
      <colgroup>
        <col style="width: auto;"> <!-- Player -->
        <col style="width: 140px;"> <!-- Team -->
        <col style="width: 60px;">  <!-- GP -->
        <col style="width: 60px;">  <!-- PTS -->
        <col style="width: 100px;"> <!-- FGM-A -->
        <col style="width: 80px;">  <!-- FG%% -->
        <col style="width: 60px;">  <!-- 3PM -->
        <col style="width: 100px;"> <!-- FTM-A -->
      </colgroup>
      %s
      <tbody>%s%s</tbody>
     </table>
    </div>|html}
   table_head_html
   rows
   empty_row
 in
		 let content = Printf.sprintf
		  {html|<div class="space-y-6 animate-fade-in">
		   <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
		    <div>
		     <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">%s</h2>
		     <p class="text-slate-600 dark:text-slate-400">
		      %s
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
	   <div class="text-xs text-slate-600 dark:text-slate-400 text-center">%s</div>
	  </div>|html}
	  (escape_html h2_title)
	  (escape_html subtitle)
	  season_options
	  table_html
	  (escape_html footnote)
	 in
 layout
  ~lang
  ~title:(tr { ko = "클러치 리더 | WKBL"; en = "Clutch Leaders | WKBL" })
  ~canonical_path:"/clutch"
  ~description:
    (tr
       { ko = "WKBL 여자농구 클러치 타임 리더 - 접전 상황(4쿼터 5분 이내) 득점 기록."
       ; en = "Clutch leaders in close games (Q4 last 5 minutes)."
       })
  ~content () 

let live_page ?(lang=I18n.Ko) () =
 let tr = I18n.t lang in
 let title = tr { ko = "실시간 점수 | WKBL"; en = "Live Scores | WKBL" } in
 let h1 = tr { ko = "실시간 점수"; en = "Live Scores" } in
 let desc = tr { ko = "실시간 경기 점수"; en = "Live game scores" } in
 let connecting = tr { ko = "업데이트 중..."; en = "Updating..." } in
 let connected = tr { ko = "업데이트 됨"; en = "Updated" } in
 let disconnected = tr { ko = "업데이트 지연"; en = "Update delayed" } in
 let status_scheduled = tr { ko = "예정"; en = "Scheduled" } in
 let status_live = tr { ko = "진행중"; en = "Live" } in
 let status_final = tr { ko = "종료"; en = "Final" } in
 let empty_today = tr { ko = "오늘 예정된 경기가 없습니다."; en = "No games scheduled today." } in
 let view_all_games = tr { ko = "전체 경기 보기"; en = "View all games" } in
 let auto_updates = tr { ko = "자동으로 업데이트됩니다."; en = "Updates automatically." } in
 let page_html =
  Printf.sprintf
   {html|
	  <div class="space-y-6">
	   <div class="flex flex-col sm:flex-row sm:items-center justify-between gap-4">
	    <div>
	     <h1 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h1>
   <p class="text-slate-600 dark:text-slate-400">%s</p>
    </div>
    <div id="connection-status" class="flex items-center gap-2">
     <span class="w-2 h-2 bg-yellow-500 rounded-full animate-pulse"></span>
     <span class="text-sm text-slate-600 dark:text-slate-400">%s</span>
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
    %s
   </div>
  </div>

  <script>
  (function() {
   const gamesContainer = document.getElementById('live-games');
   const statusEl = document.getElementById('connection-status');

   function updateStatus(ok) {
    statusEl.innerHTML = ok
     ? '<span class="w-2 h-2 bg-green-500 rounded-full"></span><span class="text-sm text-slate-600 dark:text-slate-400">%s</span>'
     : '<span class="w-2 h-2 bg-red-500 rounded-full"></span><span class="text-sm text-slate-600 dark:text-slate-400">%s</span>';
   }

   function renderGame(game) {
    const q = (game.quarter || '').trim();
    const isLive = game.is_live === true;
    const isScheduled = !isLive && (q === '경기전' || q === '경기 전' || q === '예정' || (game.home_score === 0 && game.away_score === 0) || game.home_score == null || game.away_score == null);
    const isFinal = !isLive && !isScheduled;

    const badgeText = isLive ? (q || '%s') : (isScheduled ? '%s' : '%s');
    const badgeClass = isLive
     ? 'bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300'
     : (isFinal
        ? 'bg-emerald-100 dark:bg-emerald-900/30 text-emerald-700 dark:text-emerald-300'
        : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400');

    const timeText = (game.time || '').trim();
    const subText = isLive
     ? (timeText ? `${q} · ${timeText}` : q)
     : '';

    const scoreHtml = isScheduled
     ? '<div class="text-2xl font-black text-slate-500 dark:text-slate-400">VS</div>'
     : `<div class="flex items-center justify-center gap-4 text-3xl font-black">
        <span class="${game.home_score > game.away_score ? 'text-orange-500' : 'text-slate-900 dark:text-slate-200'}">${game.home_score}</span>
        <span class="text-slate-400 font-light">-</span>
        <span class="${game.away_score > game.home_score ? 'text-orange-500' : 'text-slate-900 dark:text-slate-200'}">${game.away_score}</span>
       </div>`;

	    return `
	     <a href="/boxscore/${game.game_id}" class="block bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-6 hover:border-orange-400 transition-colors">
	      <div class="flex justify-between items-center mb-4">
	       <span class="text-lg font-semibold text-slate-900 dark:text-slate-200">${game.home_team}</span>
	       <span class="text-xs px-2 py-1 rounded ${badgeClass}">${badgeText}</span>
	       <span class="text-lg font-semibold text-slate-900 dark:text-slate-200">${game.away_team}</span>
      </div>
      ${scoreHtml}
      ${subText ? `<div class="mt-3 text-xs text-slate-500 dark:text-slate-400 text-center">${subText}</div>` : ''}
     </a>
    `;
   }

   function renderGames(data) {
	    if (!data || !data.games || data.games.length === 0) {
	     gamesContainer.innerHTML = `
	      <div class="col-span-2 bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-12 text-center">
		       <p class="text-slate-600 dark:text-slate-400">%s (${data && data.today ? data.today : ''})</p>
		       <a href="/games" class="text-orange-500 hover:underline mt-2 inline-block">%s →</a>
	      </div>
	     `;
	    } else {
     gamesContainer.innerHTML = data.games.map(renderGame).join('');
    }
   }

   async function fetchStatus() {
    try {
     const res = await fetch('/api/live/status', { cache: 'no-store' });
     if (!res.ok) throw new Error('bad status');
     const data = await res.json();
     updateStatus(true);
     renderGames(data);
    } catch (_) {
     updateStatus(false);
    }
   }

   fetchStatus();
   setInterval(fetchStatus, 20000);
	  })();
	  </script>
	 |html}
   (escape_html h1)
   (escape_html desc)
   (escape_html connecting)
   (escape_html auto_updates)
   (escape_js_string connected)
   (escape_js_string disconnected)
   (escape_js_string status_live)
   (escape_js_string status_scheduled)
   (escape_js_string status_final)
   (escape_js_string empty_today)
   (escape_js_string view_all_games)
 in
 layout
	  ~lang
	  ~title
	  ~canonical_path:"/live"
  ~description:
    (tr
       { ko = "WKBL 여자농구 실시간 스코어 - 오늘 경기 점수를 확인하세요."
	       ; en = "Live scores for today's games."
	       })
	  ~content:page_html
	    () 

(** Position-based leaderboard page *)
let position_leaders_page ?(lang=I18n.Ko) ?(player_info_map=None) ~season ~seasons ~position (leaders: (string * leader_entry list) list) =
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
  let position_btn pos label emoji =
    let active = if pos = position then "bg-orange-600 text-white" else "bg-slate-100 dark:bg-slate-800 text-slate-700 dark:text-slate-300 hover:bg-slate-200 dark:hover:bg-slate-700" in
    Printf.sprintf {html|<a href="/leaders/by-position?position=%s&season=%s" class="px-4 py-2 rounded-lg font-bold transition-colors %s">%s %s</a>|html} pos season active emoji (escape_html label)
  in
  let lookup stat =
    leaders |> List.find_opt (fun (k, _) -> k = stat) |> Option.map snd |> Option.value ~default:[]
  in
  let leader_table title stat entries =
    if List.length entries = 0 then
      Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6">
        <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-4">%s</h3>
        <p class="text-slate-500 dark:text-slate-400 text-center py-4">데이터가 없습니다</p>
      </div>|html} (escape_html title)
    else
      let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
      entries |> List.iter (fun (e: leader_entry) ->
        let key = normalize_name e.le_player_name in
        Hashtbl.replace name_counts key ((Hashtbl.find_opt name_counts key |> Option.value ~default:0) + 1));
      let show_id (e: leader_entry) =
        Hashtbl.find_opt name_counts (normalize_name e.le_player_name) |> Option.map (fun c -> c > 1) |> Option.value ~default:false
      in
      let info_opt player_id =
        match player_info_map with
        | Some map -> Hashtbl.find_opt map player_id
        | None -> None
      in
      let rows = entries |> List.mapi (fun i (e: leader_entry) ->
        let rank_badge = match i with
          | 0 -> {html|<span class="text-amber-500">🥇</span>|html}
          | 1 -> {html|<span class="text-slate-400">🥈</span>|html}
          | 2 -> {html|<span class="text-amber-700">🥉</span>|html}
          | _ -> Printf.sprintf {html|<span class="text-slate-500">%d</span>|html} (i + 1)
        in
        let disambiguation =
          if show_id e then
            player_disambiguation_line ~team_name:e.le_team_name ~player_id:e.le_player_id (info_opt e.le_player_id)
          else ""
        in
        Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-800/50">
          <td class="px-3 py-2 text-center font-bold">%s</td>
          <td class="px-3 py-2">
            <div class="flex flex-col">
              <a href="/player/%s" class="text-slate-900 dark:text-slate-200 hover:text-orange-600 font-medium">%s</a>
              %s
              <span class="text-slate-500 text-xs">%s</span>
            </div>
          </td>
          <td class="px-3 py-2 text-right font-bold font-mono text-orange-600 dark:text-orange-400">%.1f</td>
        </tr>|html}
        rank_badge e.le_player_id (escape_html e.le_player_name) disambiguation (escape_html e.le_team_name) e.le_stat_value
      ) |> String.concat "\n" in
      Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-lg">
        <div class="px-4 py-3 bg-slate-50 dark:bg-slate-800/50 border-b border-slate-200 dark:border-slate-700">
          <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</h3>
        </div>
        <table class="w-full text-sm">
          <thead class="bg-slate-100 dark:bg-slate-800 text-xs uppercase text-slate-500 dark:text-slate-400">
            <tr><th class="px-3 py-2 text-center w-12">순위</th><th class="px-3 py-2 text-left">선수</th><th class="px-3 py-2 text-right w-20">%s</th></tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>|html} (escape_html title) (escape_html stat) rows
  in
  let position_label = match position with
    | "G" -> "가드 (Guard)"
    | "F" -> "포워드 (Forward)"
    | "C" -> "센터 (Center)"
    | _ -> "전체 포지션"
  in
  layout ~lang ~title:(position_label ^ " 리더보드 | WKBL")
    ~content:(Printf.sprintf {html|<div class="space-y-6 animate-fade-in">
      <!-- Header -->
      <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl">
        <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200 text-center mb-4">포지션별 리더보드</h1>
        <div class="flex flex-col md:flex-row items-center justify-center gap-4">
          <!-- Position Tabs -->
          <div class="flex flex-wrap justify-center gap-2">
            %s
            %s
            %s
            %s
          </div>
          <!-- Season Filter -->
          <form action="/leaders/by-position" method="get" class="flex items-center gap-2">
            <input type="hidden" name="position" value="%s" />
            <select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm" onchange="this.form.submit()">
              %s
            </select>
          </form>
        </div>
        <p class="text-center text-sm text-slate-500 dark:text-slate-400 mt-4">%s</p>
      </div>

      <!-- Stats Grid -->
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        %s
        %s
        %s
        %s
      </div>
    </div>|html}
    (position_btn "ALL" "전체" "👥")
    (position_btn "G" "가드" "🏃")
    (position_btn "F" "포워드" "💪")
    (position_btn "C" "센터" "🏀")
    position
    season_options
    (escape_html position_label)
    (leader_table "득점 (PTS)" "PTS" (lookup "pts"))
    (leader_table "리바운드 (REB)" "REB" (lookup "reb"))
    (leader_table "어시스트 (AST)" "AST" (lookup "ast"))
    (leader_table "효율 (EFF)" "EFF" (lookup "eff"))
  ) ()

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
           <a href="/" class="mt-6 inline-flex items-center text-orange-600 dark:text-orange-400 hover:underline">%s</a>
         </div>|html}
         (escape_html heading)
         (escape_html body)
         details_html
         (escape_html back))
    ()

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
