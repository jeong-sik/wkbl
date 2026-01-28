(** Team-specific view functions for WKBL Analytics *)
(** Contains team profile page. *)

open Domain
open Views_common

let team_profile_page (detail: team_full_detail) ~season ~seasons =
 let t = detail.tfd_team_name in
 let s = detail.tfd_standing in
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
 let standing_info = match s with | Some st -> Printf.sprintf {html|<div class="flex gap-6 text-sm"><div class="flex flex-col"><span>WINS</span><span class="text-2xl font-black text-slate-900 dark:text-slate-200">%d</span></div><div class="flex flex-col"><span>LOSSES</span><span class="text-2xl font-black text-slate-900 dark:text-slate-200">%d</span></div><div class="flex flex-col"><span>WIN %%</span><span class="text-2xl font-black text-orange-600 dark:text-orange-400">%.3f</span></div><div class="flex flex-col"><span>GB</span><span class="text-2xl font-black text-slate-500 dark:text-slate-400">%.1f</span></div></div>|html} st.wins st.losses st.win_pct st.gb | None -> "" in
 let roster_name_counts : (string, int) Hashtbl.t = Hashtbl.create 32 in
 detail.tfd_roster
 |> List.iter (fun (p: player_aggregate) ->
   let key = normalize_name p.name in
   let prev = Hashtbl.find_opt roster_name_counts key |> Option.value ~default:0 in
   Hashtbl.replace roster_name_counts key (prev + 1));
  let roster_rows =
   detail.tfd_roster
   |> List.mapi (fun i (p: player_aggregate) ->
    let key = normalize_name p.name in
    let show_player_id =
     match Hashtbl.find_opt roster_name_counts key with
     | Some c when c > 1 -> true
     | _ -> false
    in
    player_row ~show_player_id ~include_team:false ~team_cell_class:"px-3 py-2" (i + 1) p)
  |> String.concat "\n"
 in
 let roster_cards =
  let margin_chip v =
   let cls =
    if v > 0.0 then "text-sky-600 dark:text-sky-400"
    else if v < 0.0 then "text-rose-600 dark:text-rose-400"
    else "text-slate-700 dark:text-slate-300"
   in
   let s = if v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v in
   Printf.sprintf {html|<span class="%s font-mono font-bold tabular-nums">%s</span>|html} cls (escape_html s)
  in
  detail.tfd_roster
  |> List.mapi (fun _i (p: player_aggregate) ->
    let key = normalize_name p.name in
    let show_player_id =
     match Hashtbl.find_opt roster_name_counts key with
     | Some c when c > 1 -> true
     | _ -> false
    in
    let id_badge =
     if show_player_id then Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge p.player_id) else ""
    in
    Printf.sprintf
     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
      <div class="flex items-center justify-between gap-3">
       <div class="flex items-center gap-3 min-w-0">
        <div class="shrink-0">%s</div>
        <div class="min-w-0">
         <div class="text-sm font-bold text-slate-900 dark:text-slate-200 truncate">
          <a href="/player/%s" class="player-name hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a>%s
         </div>
         <div class="mt-0.5 text-[11px] text-slate-500 dark:text-slate-400 font-mono tabular-nums">GP %d • EFF %.1f</div>
        </div>
       </div>
       <div class="text-right shrink-0">
        <div class="text-[10px] text-slate-500 dark:text-slate-400 font-mono uppercase tracking-widest">PTS</div>
        <div class="text-lg font-black text-orange-600 dark:text-orange-400 font-mono tabular-nums">%.1f</div>
       </div>
      </div>
      <div class="mt-3 grid grid-cols-3 gap-2 text-xs font-mono tabular-nums">
       <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
        <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest">MG</div>
        <div class="mt-0.5">%s</div>
       </div>
       <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
        <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest">REB</div>
        <div class="mt-0.5 text-slate-900 dark:text-slate-200 font-bold">%.1f</div>
       </div>
       <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
        <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest">AST</div>
        <div class="mt-0.5 text-slate-900 dark:text-slate-200 font-bold">%.1f</div>
       </div>
      </div>
     </div>|html}
     (player_img_tag ~class_name:"w-10 h-10 border border-slate-300 dark:border-slate-700 shadow-sm" p.player_id p.name)
     p.player_id
     (escape_html (normalize_name p.name))
     id_badge
     p.games_played
     p.efficiency
     p.avg_points
     (margin_chip p.avg_margin)
     p.avg_rebounds
     p.avg_assists)
  |> String.concat "\n"
 in
	                    let roster_table_inner =
	                     Printf.sprintf
	                      {html|<table class="roster-table min-w-[980px] w-full text-xs sm:text-sm font-mono table-fixed tabular-nums" aria-label="팀 로스터">
	                       <colgroup>
	                         <col style="width: 50px;">  <!-- # -->
	                         <col style="width: 200px;"> <!-- Player -->
	                         <col class="hidden sm:table-column" style="width: 60px;">  <!-- GP -->
	                         <col style="width: 90px;">  <!-- PTS -->
	                         <col class="hidden md:table-column" style="width: 80px;">  <!-- MG -->
	                         <col style="width: 85px;">  <!-- REB -->
	                         <col class="hidden md:table-column" style="width: 85px;">  <!-- AST -->
	                         <col class="hidden lg:table-column" style="width: 80px;">  <!-- STL -->
	                         <col class="hidden lg:table-column" style="width: 80px;">  <!-- BLK -->
	                         <col class="hidden lg:table-column" style="width: 80px;">  <!-- TO -->
	                         <col style="width: 80px;">  <!-- EFF -->
	                         <col class="hidden sm:table-column" style="width: 80px;">  <!-- PER -->
	                       </colgroup>
	                       <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap"><tr><th scope="col" class="px-2 py-2 text-center font-sans whitespace-nowrap">#</th><th scope="col" class="px-3 py-2 text-left font-sans whitespace-nowrap">Player</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell whitespace-nowrap" title="Games Played">GP</th><th scope="col" class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 whitespace-nowrap" title="Points per game">PTS</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell whitespace-nowrap" title="Team Margin (minutes weighted)">MG</th><th scope="col" class="px-3 py-2 text-right whitespace-nowrap" title="Rebounds per game">REB</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell whitespace-nowrap" title="Assists per game">AST</th><th scope="col" class="px-3 py-2 text-right hidden lg:table-cell whitespace-nowrap" title="Steals per game">STL</th><th scope="col" class="px-3 py-2 text-right hidden lg:table-cell whitespace-nowrap" title="Blocks per game">BLK</th><th scope="col" class="px-3 py-2 text-right hidden lg:table-cell whitespace-nowrap" title="Turnovers per game">TO</th><th scope="col" class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 whitespace-nowrap" title="Efficiency">EFF</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell whitespace-nowrap" title="Player Efficiency Rating">PER</th></tr></thead><tbody>%s</tbody></table>|html}
	                      roster_rows
	                    in	 let season_label =
	  if season = "ALL" then
	   "All Seasons"
	  else
	   seasons
	   |> List.find_opt (fun (s: season_info) -> s.code = season)
	   |> Option.map (fun (s: season_info) -> s.name)
	   |> Option.value ~default:season
	 in
	 let game_results_chart =
	  let games = detail.tfd_game_results in
	  match games with
	  | [] ->
	    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-500 dark:text-slate-400 shadow-lg">No game results</div>|html}
	  | _ ->
	    let max_abs_margin =
	     games
	     |> List.fold_left
	        (fun acc (g: team_game_result) ->
	         let margin = g.tgr_team_score - g.tgr_opponent_score in
	         Stdlib.max acc (Stdlib.abs margin))
	        1
	    in
	    let wins =
	     games
	     |> List.fold_left (fun acc (g: team_game_result) -> if g.tgr_is_win then acc + 1 else acc) 0
	    in
	    let losses = List.length games - wins in
	    let avg_margin =
	     let sum =
	      games
	      |> List.fold_left
	         (fun acc (g: team_game_result) -> acc + (g.tgr_team_score - g.tgr_opponent_score))
	         0
	     in
	     float_of_int sum /. float_of_int (List.length games)
	    in
	    let bars =
	     games
	     |> List.map (fun (g: team_game_result) ->
	        let margin = g.tgr_team_score - g.tgr_opponent_score in
	        let abs_margin = Stdlib.abs margin in
	        let height_pct =
	         (float_of_int abs_margin /. float_of_int max_abs_margin) *. 100.0
	        in
	        let opponent_label =
	         if g.tgr_is_home then "vs " ^ g.tgr_opponent else "@ " ^ g.tgr_opponent
	        in
	        let margin_str =
	         if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin
	        in
	        let title =
	         Printf.sprintf "%s • %s • %d-%d (%s)"
	          g.tgr_game_date
	          opponent_label
	          g.tgr_team_score
	          g.tgr_opponent_score
	          margin_str
	         |> escape_html
	        in
	        let pos_bar =
	         if margin > 0 then
	          Printf.sprintf
	           {html|<div class="w-full rounded-sm bg-sky-500/80 group-hover:bg-sky-400 transition-colors" style="height: %.0f%%"></div>|html}
	           height_pct
	         else
	          ""
	        in
	        let neg_bar =
	         if margin < 0 then
	          Printf.sprintf
	           {html|<div class="w-full rounded-sm bg-rose-500/80 group-hover:bg-rose-400 transition-colors" style="height: %.0f%%"></div>|html}
	           height_pct
	         else
	          ""
		        in
		        Printf.sprintf
		         {html|<a href="/boxscore/%s" class="group block w-2 sm:w-1.5 h-24" title="%s"><div class="h-1/2 flex items-end">%s</div><div class="h-1/2 flex items-start">%s</div></a>|html}
		         (Uri.pct_encode g.tgr_game_id)
		         title
		         pos_bar
		         neg_bar)
		     |> String.concat "\n"
		    in
		    Printf.sprintf
		     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg"><div class="flex items-center justify-between gap-3"><div class="text-[11px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">%s</div><div class="text-xs font-mono text-slate-500 dark:text-slate-400 tabular-nums">%d-%d</div></div><div class="mt-2 flex items-center justify-between gap-3 text-[11px] text-slate-500 dark:text-slate-400 font-mono tabular-nums"><span>AVG MG %+.1f</span><span>±%d</span></div><div class="mt-3 overflow-x-auto pb-1"><div class="relative h-24 w-max"><div class="absolute left-0 right-0 top-1/2 h-px bg-slate-100 dark:bg-slate-800/80"></div><div class="flex items-stretch gap-0.5 h-24 w-max pr-1">%s</div></div></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">막대: 득실마진(MG=팀-상대). 클릭하면 박스스코어.</div></div>|html}
		     (escape_html season_label)
		     wins
		     losses
		     avg_margin
		     max_abs_margin
		     bars
		 in
		 let game_rows =
		  detail.tfd_recent_games
		  |> List.map (fun (g: team_game_result) ->
		    let res_class = if g.tgr_is_win then "text-sky-600 dark:text-sky-400" else "text-rose-600 dark:text-rose-400" in
	    let res_label = if g.tgr_is_win then "W" else "L" in
	    let margin = g.tgr_team_score - g.tgr_opponent_score in
    let margin_class =
     if margin > 0 then "text-sky-600 dark:text-sky-400"
     else if margin < 0 then "text-rose-600 dark:text-rose-400"
     else "text-slate-500 dark:text-slate-400"
    in
    let margin_str =
     if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin
    in
	    let opponent_label =
	     if g.tgr_is_home then "vs " ^ g.tgr_opponent else "@ " ^ g.tgr_opponent
	    in
	    let opponent_href = "/team/" ^ Uri.pct_encode g.tgr_opponent in
	    let date_short =
	     if String.length g.tgr_game_date >= 10 then
	      String.sub g.tgr_game_date 5 5
	     else
	      g.tgr_game_date
	    in
	    Printf.sprintf
	     {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-24"><a href="/boxscore/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors" title="%s"><span class="sm:hidden">%s</span><span class="hidden sm:inline">%s</span></a></td><td class="px-3 py-2 text-slate-900 dark:text-slate-200"><a class="block truncate hover:text-orange-600 dark:text-orange-400 transition-colors" href="%s" title="%s">%s</a></td><td class="px-3 py-2 text-center font-bold %s whitespace-nowrap w-14"><span class="inline-flex items-center justify-center w-7">%s</span></td><td class="px-3 py-2 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap w-36"><div class="flex items-center justify-end gap-2 flex-nowrap tabular-nums"><span class="whitespace-nowrap">%d - %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono %s whitespace-nowrap min-w-[48px] text-center">%s</span></div></td></tr>|html}
	     (Uri.pct_encode g.tgr_game_id)
	     (escape_html g.tgr_game_date)
	     (escape_html date_short)
	     (escape_html g.tgr_game_date)
	     (escape_html opponent_href)
	     (escape_html opponent_label)
	     (escape_html opponent_label)
	     res_class
	     res_label
     g.tgr_team_score
     g.tgr_opponent_score
     margin_class
     (escape_html margin_str))
  |> String.concat "\n"
 in
 (* Four Factors Section *)
 let four_factors_section =
  match detail.tfd_team_totals with
  | None -> ""
  | Some totals ->
    let ff = Stats.four_factors_of_totals totals in
    let stat_card label value desc color =
     Printf.sprintf
      {html|<div class="bg-slate-100 dark:bg-slate-800/60 rounded-lg p-3 border border-slate-200 dark:border-slate-700/50">
       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">%s</div>
       <div class="mt-1 text-xl font-black %s font-mono tabular-nums">%.1f%%</div>
       <div class="mt-1 text-[10px] text-slate-500 dark:text-slate-400 leading-tight">%s</div>
      </div>|html}
      label color value desc
    in
    Printf.sprintf
     {html|<div class="space-y-4">
      <h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Four Factors</h3>
      <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
       <div class="grid grid-cols-2 gap-3">
        %s
        %s
        %s
        %s
       </div>
       <div class="mt-3 text-[10px] text-slate-500 dark:text-slate-400 leading-relaxed">
        Dean Oliver's Four Factors: 팀 성과를 결정짓는 4가지 핵심 지표
       </div>
      </div>
     </div>|html}
     (stat_card "eFG%%" ff.efg_pct "슈팅 효율" "text-emerald-600 dark:text-emerald-400")
     (stat_card "TOV%%" ff.tov_pct "턴오버율 (낮을수록 좋음)" "text-rose-500 dark:text-rose-400")
     (stat_card "ORB%%" ff.orb_pct "공격 리바운드율" "text-sky-600 dark:text-sky-400")
     (stat_card "FTR" ff.ftr "자유투 시도율" "text-orange-600 dark:text-orange-400")
 in
	 layout ~title:(t ^ " | WKBL Team Profile")
	  ~content:(Printf.sprintf {html|<div class="space-y-6 sm:space-y-8 animate-fade-in"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 sm:p-8 shadow-2xl flex flex-col md:flex-row items-center md:items-start gap-6 sm:gap-8"><div class="w-24 h-24 sm:w-32 sm:h-32 bg-slate-100 dark:bg-slate-800 rounded-xl flex items-center justify-center p-3 sm:p-4 border-2 border-slate-300 dark:border-slate-700 shadow-inner">%s</div><div class="text-center md:text-left space-y-4 w-full"><div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-3"><h1 class="text-3xl sm:text-4xl font-black text-slate-900 dark:text-slate-200">%s</h1><form action="/team/%s" method="get" class="flex flex-col sm:flex-row items-stretch sm:items-center justify-center sm:justify-end gap-2 w-full sm:w-auto"><span class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">Season</span><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select></form></div>%s</div></div><div class="grid grid-cols-1 lg:grid-cols-3 gap-6 sm:gap-8"><div class="space-y-4 lg:col-span-2"><h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Roster</h3><div class="sm:hidden space-y-3">%s</div><details class="sm:hidden bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4"><summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">Full table</summary><div class="mt-3 overflow-x-auto">%s</div></details><div class="hidden sm:block bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">%s</div></div><div class="space-y-6 lg:col-span-1"><h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Game Results</h3>%s%s<h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">Recent Results</h3><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-lg"><table class="min-w-[480px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="최근 경기 결과">
          <colgroup>
            <col style="width: 96px;"> <!-- Date -->
            <col style="width: auto;"> <!-- Opponent -->
            <col style="width: 56px;"> <!-- Result -->
            <col style="width: 144px;"> <!-- Score -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap"><tr><th scope="col" class="px-3 py-2 text-left font-sans">Date</th><th scope="col" class="px-3 py-2 text-left font-sans">Opponent</th><th scope="col" class="px-3 py-2 text-center font-sans" title="Win/Loss">Result</th><th scope="col" class="px-3 py-2 text-right font-sans">Score</th></tr></thead><tbody>%s</tbody></table></div></div></div></div>|html}
	     (team_logo_tag ~class_name:"w-16 h-16 sm:w-24 sm:h-24" t)
	     (escape_html t)
	     (Uri.pct_encode t)
	     season_options
	     standing_info
	     roster_cards
	     roster_table_inner
	     roster_table_inner
	     game_results_chart
	     four_factors_section
	     game_rows) ()

(** DB QA dashboard page *)
