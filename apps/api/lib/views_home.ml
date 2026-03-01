(** Home page view - extracted from Views *)

open Domain
open Views_common

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
    col label_player ~w:(px 200) ~sticky:true;
    col label_team ~w:(px 130);
    col "GP" ~w:(px 60) ~align:`Right;
    col "PTS" ~w:(px 90) ~align:`Right ~sort:"pts";
    col "MG" ~w:(px 80) ~align:`Right ~sort:"mg";
    col "REB" ~w:(px 85) ~align:`Right ~sort:"reb";
    col "AST" ~w:(px 85) ~align:`Right ~sort:"ast";
    col "STL" ~w:(px 80) ~align:`Right;
    col "BLK" ~w:(px 80) ~align:`Right;
    col "TO" ~w:(px 80) ~align:`Right;
    col "EFF ↓" ~w:(px 80) ~align:`Right ~sort:"eff" ~highlight:true;
    col "PER" ~w:(px 80) ~align:`Right;
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
          Printf.sprintf {html|<div class="flex items-center gap-3 min-w-0">%s<div class="flex flex-col min-w-0"><div class="flex items-center gap-2 min-w-0"><a href="%s" class="player-name hover:text-orange-700 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a><span class="%s">%s</span></div>%s</div></div>|html}
          (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
          (player_href p.player_id)
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
          Printf.sprintf {html|<div class="flex flex-col items-end leading-tight"><span class="text-orange-700 dark:text-orange-400 font-bold font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap leading-tight" title="%s">%s %s</span></div>|html}
          p.avg_points (escape_html label_total) (escape_html label_total) (format_int_commas p.total_points)
        in
        let stat_cell v total =
          Printf.sprintf {html|<div class="flex flex-col items-end leading-tight"><span class="text-slate-700 dark:text-slate-300 font-mono">%.1f</span><span class="text-slate-400 dark:text-slate-400 text-[10px] font-mono whitespace-nowrap leading-tight" title="%s">%s %s</span></div>|html}
          v (escape_html label_total) (escape_html label_total) (format_int_commas total)
        in

        [
          Printf.sprintf {html|<span class="text-slate-500 dark:text-slate-400 font-bold">%d</span>|html} (i + 1);
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
 let live_widget = Views_live.live_scores_widget ~lang live_games in
 let home_json_ld = {|{"@context":"https://schema.org","@type":"WebSite","name":"WKBL.win","url":"https://wkbl.win","potentialAction":{"@type":"SearchAction","target":{"@type":"EntryPoint","urlTemplate":"https://wkbl.win/players?search={search_term_string}"},"query-input":"required name=search_term_string"}}|} in
 layout ~lang ~title:(tr { ko = "WKBL 통계"; en = "WKBL Stats" }) ~canonical_path:"/"
  ~description:page_description
  ~json_ld:home_json_ld
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
	       <span class="text-xs text-slate-400 dark:text-slate-400">📅 %s: %s</span>
	       <a href="/games" class="text-xs text-orange-700 dark:text-orange-400 hover:text-orange-700 dark:hover:text-orange-300">%s</a>
	      </div>
	     </div>
	     <div id="live-scores" hx-get="/api/live/widget" hx-trigger="every 30s" hx-swap="innerHTML" hx-indicator="#live-loading">
	       <span id="live-loading" class="htmx-indicator"><span class="w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin inline-block" aria-hidden="true"></span><span class="sr-only">%s</span></span>
	       %s
	     </div>
	    </div>
		    <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3"><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">%s</h2><form class="flex gap-2 items-center" hx-get="/home/table" hx-target="#players-table" hx-trigger="change" hx-indicator="#table-loading"><select name="season" aria-label="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none">%s</select><input type="text" placeholder="%s" aria-label="%s" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none" hx-get="/home/table" hx-trigger="keyup changed delay:300ms" hx-target="#players-table" hx-indicator="#table-loading" name="search"><span id="table-loading" class="htmx-indicator"><span class="w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin inline-block"></span></span></form></div><div id="players-table" class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 scroll-shadow overflow-y-hidden" data-skeleton="table" data-skeleton-count="10" data-skeleton-cols="8">%s</div></div>|html}
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
