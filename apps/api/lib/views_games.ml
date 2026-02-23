(** Games and boxscore view functions extracted from Views *)

open Domain
open Views_common

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
     <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
     <div class="flex items-center gap-2 text-sm font-medium w-full">%s<a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate">%s</a><span class="ml-auto font-mono text-slate-900 dark:text-slate-200">%d</span></div>
    </div>
			    <a href="%s" class="text-[10px] bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition">%s</a>
		   </div>
		  </div>|html}
	        (escape_html g.game_date)
	        margin_color
	        (escape_html label_diff)
	        margin_str
	        (team_logo_tag ~class_name:"w-4 h-4" g.home_team)
	        (team_href g.home_team)
	        (escape_html g.home_team)
	        score_a
	        (team_logo_tag ~class_name:"w-4 h-4" g.away_team)
	        (team_href g.away_team)
	        (escape_html g.away_team)
		        score_b
		        (boxscore_href g.game_id)
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

      let home_cell = Printf.sprintf {html|<span class="inline-flex items-center gap-2">%s<a href="%s">%s</a></span>|html} (team_logo_tag ~class_name:"w-4 h-4" g.home_team) (team_href g.home_team) (escape_html g.home_team) in
      let away_cell = Printf.sprintf {html|<span class="inline-flex items-center gap-2"><a href="%s">%s</a>%s</span>|html} (team_href g.away_team) (escape_html g.away_team) (team_logo_tag ~class_name:"w-4 h-4" g.away_team) in
      let link_cell =
        Printf.sprintf
          {html|<a href="%s" class="text-[10px] sm:text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-2 py-1 rounded text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white transition whitespace-nowrap">%s</a>|html}
          (boxscore_href g.game_id)
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
	   {html|<div class="space-y-6">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h2><p class="text-slate-600 dark:text-slate-400 text-sm">%s</p></div></div><form id="boxscores-filter" class="flex gap-3" hx-get="/boxscores/table" hx-target="#boxscores-table" hx-trigger="change"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="boxscores-table" data-skeleton="cards" data-skeleton-count="10">%s</div></div>|html}
	   (breadcrumb [("홈", "/"); ("박스스코어", "")])
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
     | Some 0 -> ("0", "text-slate-500 dark:text-slate-400")
     | None -> ("-", "text-slate-400 dark:text-slate-600")
     | _ -> ("0", "text-slate-500 dark:text-slate-400")
    in

    let player_cell = Printf.sprintf {html|<div class="flex items-center gap-3 min-w-0 font-sans font-medium text-slate-900 dark:text-slate-200">%s<div class="flex flex-col min-w-0 overflow-hidden"><a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors truncate block">%s</a><span class="text-[10px] text-slate-500 dark:text-slate-400 font-normal truncate block">%s</span></div></div>|html}
      (player_img_tag ~class_name:"w-8 h-8 rounded-full object-cover bg-slate-100 dark:bg-slate-800 flex-shrink-0" p.bs_player_id p.bs_player_name)
      (player_href p.bs_player_id)
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
      List.combine quarters (quarter_deltas quarters)
      |> List.map (fun (q, (_period, home_q, away_q)) ->
        let diff = home_q - away_q in
        let diff_str = if diff > 0 then Printf.sprintf "+%d" diff else string_of_int diff in
        let diff_cls =
          if diff > 0 then "text-sky-600 dark:text-sky-400 font-bold"
          else if diff < 0 then "text-orange-700 dark:text-orange-400 font-bold"
          else "text-slate-400 dark:text-slate-400"
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
      )
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
  let label_full = tr { ko = "문자중계"; en = "Play-by-play" } in
  let label_short = tr { ko = "중계"; en = "Plays" } in
  let label_html =
    Printf.sprintf
      {html|<span class="sm:hidden">%s</span><span class="hidden sm:inline">%s</span>|html}
      (escape_html label_short)
      (escape_html label_full)
  in
  Printf.sprintf
    {html|<a href="%s" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition whitespace-nowrap">%s</a>|html}
    (boxscore_pbp_href game_id)
    label_html

let boxscore_flow_link_html ?(lang=I18n.Ko) game_id =
  let tr = I18n.t lang in
  let label_full = tr { ko = "득점흐름"; en = "Scoring flow" } in
  let label_short = tr { ko = "흐름"; en = "Flow" } in
  let label_html =
    Printf.sprintf
      {html|<span class="sm:hidden">%s</span><span class="hidden sm:inline">%s</span>|html}
      (escape_html label_short)
      (escape_html label_full)
  in
  Printf.sprintf
    {html|<a href="/boxscore/%s/flow" class="px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800/70 border border-slate-300 dark:border-slate-700 text-[10px] font-mono tracking-wider text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:border-slate-500 transition whitespace-nowrap">%s</a>|html}
    (escape_html game_id)
    label_html

let boxscore_disabled_chip_html ?(text_short=None) text_full =
  let content_html =
    match text_short with
    | None -> escape_html text_full
    | Some short ->
        Printf.sprintf
          {html|<span class="sm:hidden">%s</span><span class="hidden sm:inline">%s</span>|html}
          (escape_html short)
          (escape_html text_full)
  in
  Printf.sprintf
    {html|<span class="px-2 py-1 rounded-full bg-slate-50 dark:bg-slate-900/40 border border-slate-200 dark:border-slate-800 text-[10px] font-mono tracking-wider text-slate-500 dark:text-slate-400 cursor-default whitespace-nowrap">%s</span>|html}
    content_html

let boxscore_pbp_chip_html ?(lang=I18n.Ko) ~has_pbp game_id =
  let tr = I18n.t lang in
  if has_pbp then
    boxscore_pbp_link_html ~lang game_id
  else
    boxscore_disabled_chip_html
      ~text_short:(Some (tr { ko = "없음"; en = "None" }))
      (tr { ko = "문자중계 없음"; en = "No play-by-play" })

let boxscore_flow_chip_html ?(lang=I18n.Ko) ~has_pbp game_id =
  let tr = I18n.t lang in
  if has_pbp then
    boxscore_flow_link_html ~lang game_id
  else
    boxscore_disabled_chip_html
      ~text_short:(Some (tr { ko = "없음"; en = "None" }))
      (tr { ko = "득점흐름 없음"; en = "No scoring flow" })

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
     <div>• <span class="font-mono text-slate-900 dark:text-slate-200">일치</span>: 최종 스코어와 양 팀 선수 득점 합계가 모두 맞습니다.</div>
     <div>• <span class="font-mono text-slate-900 dark:text-slate-200">추정</span>: 최종 스코어 또는 득점 합계가 일부 비어 있어 비교가 어렵습니다. 화면에는 가능한 데이터로 계산한 스코어를 표시합니다.</div>
     <div>• <span class="font-mono text-slate-900 dark:text-slate-200">불일치</span>: 최종 스코어와 선수 득점 합계가 모두 있는데 값이 다릅니다. (<a href="/qa" class="text-orange-700 dark:text-orange-400 hover:underline">데이터 점검</a>)</div>
     <div class="pt-1">※ 이 검증은 "최종 득점"만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 검증이 필요합니다.</div>
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
 let label_refresh = tr { ko = "새로고침"; en = "Refresh" } in
 let label_stats_missing =
  tr {
    ko = "선수 기록을 가져오는 중입니다. 잠시 후 다시 확인해 주세요.";
    en = "Player stats are being fetched. Please check back later.";
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
  else if margin < 0 then "text-orange-700 dark:text-orange-400"
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
     {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-700 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">WKBL 원본</a>|html}
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
	      {html|<div class="max-w-2xl mx-auto bg-slate-50 dark:bg-slate-900/40 border border-slate-200 dark:border-slate-800 rounded-lg p-4 text-sm text-slate-700 dark:text-slate-300"><div class="flex items-start justify-between gap-3"><div class="min-w-0">%s</div><a href="%s" class="shrink-0 px-3 py-1.5 rounded-lg bg-white/80 dark:bg-slate-800/60 border border-slate-200 dark:border-slate-700 text-[11px] font-bold text-slate-700 dark:text-slate-200 hover:border-orange-300 dark:hover:border-orange-700 hover:text-orange-700 dark:hover:text-orange-300 transition whitespace-nowrap">%s</a></div></div>|html}
	      (escape_html label_stats_missing)
	      (boxscore_href gi.gi_game_id)
	      (escape_html label_refresh)
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
	  ~canonical_path:(Printf.sprintf "/boxscore/%s" gi.gi_game_id)
	  ~description:(Printf.sprintf "%s vs %s 박스스코어 - %s" gi.gi_home_team_name gi.gi_away_team_name gi.gi_game_date)
	  ~content:(Printf.sprintf
	   {html|<div class="space-y-8 animate-fade-in">%s<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-2xl"><div class="flex flex-col items-center gap-6"><div class="text-slate-600 dark:text-slate-400 font-mono text-sm uppercase tracking-widest">%s</div><div class="flex items-center justify-between w-full max-w-2xl gap-2 sm:gap-6"><div class="flex flex-col items-center gap-2 sm:gap-3 flex-shrink-0"><div class="text-sm sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-1 sm:gap-3">%s<a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a></div><div class="text-[10px] sm:text-sm text-slate-600 dark:text-slate-400">%s</div></div><div class="flex items-center gap-2 sm:gap-8"><div class="text-3xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%s</div><div class="flex flex-col items-center gap-1 sm:gap-2"><div class="text-base sm:text-2xl text-slate-700 dark:text-slate-300 font-light">vs</div><div class="flex flex-wrap items-center justify-center gap-1 sm:gap-2">%s%s%s%s</div></div><div class="text-3xl sm:text-5xl font-black text-slate-900 dark:text-slate-200">%s</div></div><div class="flex flex-col items-center gap-2 sm:gap-3 flex-shrink-0"><div class="text-sm sm:text-2xl font-black text-slate-900 dark:text-slate-200 flex items-center gap-1 sm:gap-3"><a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors whitespace-nowrap">%s</a>%s</div><div class="text-[10px] sm:text-sm text-slate-600 dark:text-slate-400">%s</div></div></div></div></div>%s%s%s<div class="grid grid-cols-1 gap-8">%s%s</div><div class="flex justify-center"><a href="/games" class="text-slate-600 dark:text-slate-400 hover:text-orange-500 transition text-sm">← 경기 목록</a></div></div>|html}
   (breadcrumb [("홈", "/"); ("경기", "/games"); (Printf.sprintf "%s vs %s" gi.gi_home_team_name gi.gi_away_team_name, "")])
   (escape_html gi.gi_game_date)
   (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_home_team_name) (team_href gi.gi_home_team_name) (escape_html gi.gi_home_team_name)
   (escape_html label_home)
   (escape_html home_score_display)
   margin_badge
   quality_badge
   pbp_link
   flow_link
   (escape_html away_score_display)
   (team_href gi.gi_away_team_name) (escape_html gi.gi_away_team_name) (team_logo_tag ~class_name:"w-10 h-10 sm:w-16 sm:h-16" gi.gi_away_team_name)
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
 let label_pbp = tr { ko = "문자중계"; en = "Play by play" } in
 let label_back_boxscore = tr { ko = "← 박스스코어"; en = "← Boxscore" } in
 let label_wkbl_source = tr { ko = "WKBL 원본"; en = "WKBL source" } in
 let title_page =
  tr
   {
     ko = Printf.sprintf "문자중계: %s vs %s" game.gi_home_team_name game.gi_away_team_name;
     en = Printf.sprintf "Play by play: %s vs %s" game.gi_home_team_name game.gi_away_team_name;
   }
 in
 let official_link =
  match wkbl_official_game_result_url ~game_id:game.gi_game_id ~game_date:game.gi_game_date with
  | None -> ""
  | Some url ->
    Printf.sprintf
     {html|<a href="%s" target="_blank" rel="noreferrer" class="text-orange-700 dark:text-orange-400 hover:text-orange-700 underline-offset-2 hover:underline">%s</a>|html}
     (escape_html url)
     (escape_html label_wkbl_source)
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
    let team_name =
     match e.pe_team_side with
     | 1 -> game.gi_away_team_name
     | 2 -> game.gi_home_team_name
     | _ -> ""
    in
    let side_badge =
     match e.pe_team_side with
     | 1 | 2 ->
       team_badge ~max_width:"max-w-[110px] sm:max-w-[140px]" team_name
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
             en = Printf.sprintf "Away %s / Home %s" game.gi_away_team_name game.gi_home_team_name;
           }
       in
       let text = Printf.sprintf "%d - %d" a h in
       Printf.sprintf
        {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[11px] tabular-nums font-semibold text-slate-700 dark:text-slate-300 whitespace-nowrap" title="%s">%s</span>|html}
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
 let title_no_pbp = tr { ko = "문자중계가 없어요"; en = "No play by play for this game" } in
 let desc_no_pbp =
  tr
   {
     ko = "WKBL에서 문자중계를 제공하지 않았거나, 아직 준비되지 않은 경기입니다. 문자중계가 없으면 일부 세부 기록이 표시되지 않을 수 있어요.";
     en = "WKBL did not provide play by play for this game, or it is not available yet. When the feed is missing, some detailed stats may not be shown.";
   }
 in
 let title_no_rows = tr { ko = "이 쿼터에는 기록이 없어요"; en = "No events in this period" } in
 let desc_no_rows =
  tr
   {
     ko = "다른 쿼터를 눌러보거나, WKBL 원본 링크가 있으면 확인해 주세요.";
     en = "Try another period, or check the WKBL source link if available.";
   }
 in
 let note_score_order =
  tr
   {
     ko = "문자중계 점수는 보통 원정 / 홈 순서로 표시됩니다.";
     en = "In the feed, the score is usually shown as Away / Home.";
   }
 in
	 let body =
	  match periods with
	  | [] ->
	    Printf.sprintf
	      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-slate-600 dark:text-slate-400 text-sm">
	        <div class="font-bold text-slate-900 dark:text-slate-200 mb-2">%s</div>
	        <div class="leading-relaxed">%s</div>
	      </div>|html}
	      (escape_html title_no_pbp)
	      (escape_html desc_no_pbp)
	  | _ ->
	    if events = [] then
	      Printf.sprintf
	        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">%s
	          <div class="p-6 text-slate-600 dark:text-slate-400 text-sm">
	            <div class="font-bold text-slate-900 dark:text-slate-200 mb-2">%s</div>
	            <div class="leading-relaxed">%s</div>
	          </div>
	        </div>|html}
	        tabs
	        (escape_html title_no_rows)
	        (escape_html desc_no_rows)
	    else
	      Printf.sprintf
	        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">%s<ul class="divide-y divide-slate-800/60">%s</ul></div>|html}
	        tabs
	        rows
 in
 layout
  ~lang
  ~title:title_page
  ~canonical_path:"/players/compare"
  ~description:(Printf.sprintf "%s 선수 비교 - 시즌 스탯 상세 대조" title_page)
  ~content:(Printf.sprintf
   {html|<div class="space-y-6 animate-fade-in">%s
    <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-4">
     <div class="space-y-2">
      <div class="text-slate-600 dark:text-slate-400 text-sm font-semibold">%s</div>
      <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s</h2>
      <div class="flex flex-wrap items-center gap-2 text-sm">
       %s
       <span class="text-slate-600">vs</span>
       %s
       <span class="text-slate-600 dark:text-slate-400 tabular-nums font-semibold">%d - %d</span>
      </div>
     </div>
     <div class="flex items-center gap-3">
      %s
      <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:text-slate-900 dark:hover:text-white transition">%s</a>
     </div>
    </div>
    <div class="text-xs text-slate-600 dark:text-slate-400 leading-relaxed">
     %s
    </div>
    %s
   </div>|html}
   (breadcrumb [("홈", "/"); ("경기", "/games"); ("박스스코어", boxscore_href game.gi_game_id); ("문자중계", "")])
   (escape_html game.gi_game_date)
   (escape_html label_pbp)
   (team_badge ~max_width:"max-w-[160px]" game.gi_home_team_name)
   (team_badge ~max_width:"max-w-[160px]" game.gi_away_team_name)
   game.gi_home_score
   game.gi_away_score
   official_link
   (boxscore_href game.gi_game_id)
   (escape_html label_back_boxscore)
   (escape_html note_score_order)
   body) ()
