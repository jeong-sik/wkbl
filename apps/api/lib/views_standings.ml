open Domain
open Views_common

let standings_table ?(lang=I18n.Ko) ~season (standings : team_standing list) =
  let tr = I18n.t lang in
  let label_team = tr { ko = "팀"; en = "Team" } in
  let label_diff = tr { ko = "득실"; en = "Diff" } in
  let cols = [
    col ~w:(px 150) label_team;
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
        let base = team_href s.team_name in
        if season = "ALL" then base
        else base ^ "?season=" ^ Uri.pct_encode season
      in
		      let team_cell =
		        Printf.sprintf
		          {html|<a href="%s" class="team-link team-name flex w-full items-center gap-2 min-w-0 hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors cursor-pointer -mx-3 -my-2 px-3 py-2" style="white-space: nowrap; word-break: keep-all;">%s<span class="truncate">%s</span></a>|html}
		          (escape_html team_href)
		          (team_logo_tag ~class_name:"w-5 h-5 shrink-0" s.team_name)
		          (escape_html s.team_name)
	      in
	      let pct_bar_width = int_of_float (s.win_pct *. 100.0) in
	      let pct_cell = Printf.sprintf {html|<div class="flex flex-col items-end gap-1 leading-tight">
	        <span class="text-orange-700 dark:text-orange-400 font-bold font-mono">%s</span>
	        <div class="w-16 h-1.5 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden">
          <div class="h-full bg-gradient-to-r from-orange-400 to-orange-600 rounded-full transition-all" style="width: %d%%"></div>
        </div>
      </div>|html} win_pct_fmt pct_bar_width in
      
      let diff_color = if s.diff >= 0.0 then "text-emerald-600 dark:text-emerald-400" else "text-rose-600 dark:text-rose-400" in
      let diff_str = if s.diff > 0.0 then Printf.sprintf "+%.1f" s.diff else Printf.sprintf "%.1f" s.diff in
      let diff_cell = Printf.sprintf {html|<span class="%s font-mono font-bold">%s</span>|html} diff_color diff_str in

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
   {html|<div class="space-y-6">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h2><p class="text-slate-600 dark:text-slate-400 text-sm">%s</p></div></div><form id="standings-filter" class="flex gap-3" hx-get="/standings/table" hx-target="#standings-table" hx-trigger="change"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="standings-table" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="10">%s</div></div>|html}
   (breadcrumb [("홈", "/"); ("순위", "")])
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
         {html|<a href="%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">%s</a>|html}
         (boxscore_href g.game_id)
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
data-card-link="/boxscore/%s"|}
            (escape_html aria)
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
		     <a href="%s" class="team-link flex items-center gap-2 text-sm font-medium group-hover:text-orange-700 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors truncate">%s<span class="truncate">%s</span></a>
		     <a href="%s" class="team-link flex items-center gap-2 text-sm font-medium group-hover:text-orange-700 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors truncate">%s<span class="truncate">%s</span></a>
		    </div>
			    <div class="text-right font-mono text-sm %s whitespace-nowrap group-hover:scale-110 transition-transform">%s</div>
			   </div>
				  </div>|html}
         card_attrs
         card_class
		       (escape_html g.game_date)
		       action_html
	       (team_href g.home_team)
		       (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
	       (escape_html g.home_team)
	       (team_href g.away_team)
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
		            {html|<a href="%s" class="team-link team-name inline-flex items-center justify-end gap-2 whitespace-nowrap group-hover:text-orange-700 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors" style="white-space: nowrap;">%s<span>%s</span></a>|html}
		            (team_href g.home_team)
		            (team_logo_tag ~class_name:"w-4 h-4 shrink-0" g.home_team)
		            (escape_html g.home_team)
		        in
		        let score_cell =
	          if has_score then
	            Printf.sprintf
		              {html|<span class="font-bold text-orange-700 dark:text-orange-400 font-mono group-hover:scale-110 transition-transform whitespace-nowrap w-28">%s - %s</span>|html}
	              score_a
	              score_b
	          else
	            Printf.sprintf
	              {html|<span class="text-xs text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap">%s</span>|html}
	              (escape_html (status_label_for_game g))
	        in
		        let away_cell =
		          Printf.sprintf
		            {html|<a href="%s" class="team-link team-name inline-flex items-center gap-2 whitespace-nowrap group-hover:text-orange-700 dark:text-orange-400 dark:group-hover:text-orange-300 transition-colors" style="white-space: nowrap;"><span>%s</span>%s</a>|html}
		            (team_href g.away_team)
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
			              {html|<a href="%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition group-hover:ring-2 group-hover:ring-orange-500/50 whitespace-nowrap">%s</a>|html}
			              (boxscore_href g.game_id)
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
		    ~description:"WKBL 시즌별 경기 일정 및 결과"
		    ~content:(Printf.sprintf {html|
	      <div class="space-y-6">%s
	        <div class="flex items-center justify-between">
		          <h2 class="text-2xl font-black text-slate-900 dark:text-white tracking-tight">%s</h2>
			          <form action="/games" method="get" class="flex items-center gap-2">
			            <select name="season" aria-label="시즌 선택" data-auto-submit="change" class="bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-1.5 text-sm font-medium focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40 transition-colors">
			              %s
			            </select>
			          </form>
		        </div>
		        %s
		        %s
		      </div>|html} (breadcrumb [("홈", "/"); ("경기", "")]) (escape_html label_heading) season_options (games_table ~lang games) pagination_html) ()


