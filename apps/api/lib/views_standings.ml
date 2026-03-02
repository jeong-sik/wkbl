(** Standings and teams view functions extracted from Views *)

open Domain
open Views_common

let team_stat_row ~season (row: team_stats) =
 let team_href =
  let base = team_href row.team in
  if season = "ALL" then base
  else base ^ "?season=" ^ Uri.pct_encode season
 in
 let margin_color = if row.margin >= 0.0 then "text-sky-600 dark:text-sky-400 font-bold" else "text-rose-600 dark:text-rose-400 font-bold" in
 let margin_str = if row.margin > 0.0 then Printf.sprintf "+%.1f" row.margin else format_float row.margin in
     let name_cell = Printf.sprintf {html|<td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 flex items-center gap-2 whitespace-nowrap break-keep w-auto"><div class="flex items-center min-w-0">%s<a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors ml-2 truncate">%s</a></div></td>|html} (team_logo_tag ~class_name:"w-5 h-5 shrink-0" row.team) (escape_html team_href) (escape_html row.team) in
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
  cell ~color:"text-orange-700 dark:text-orange-400 font-bold" (format_float row.eff);
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
    let base = team_href s.team in
    if season = "ALL" then base
    else base ^ "?season=" ^ Uri.pct_encode season
  in
  let margin_color v =
    if v > 0.0 then "text-sky-600 dark:text-sky-400 font-bold"
    else if v < 0.0 then "text-rose-600 dark:text-rose-400 font-bold"
    else "text-slate-700 dark:text-slate-300 font-bold"
  in

  (* 1. Define Columns (Metadata only) *)
  let cols = [
    col "#" ~w:(px 40) ~align:`Center;
    col ~w:(px 150) label_team;
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
    col "TOV%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"tov";
    col "ORB%" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"orb";
    col "FTR" ~w:(px 80) ~align:`Right ~resp:`Hidden_lg ~sort:"ftr";
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
		            {html|<a href="%s" class="team-link flex w-full items-center gap-2 min-w-0 hover:text-orange-700 dark:text-orange-400 transition-colors cursor-pointer -mx-3 -my-2 px-3 py-2">%s<span class="truncate">%s</span></a>|html}
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
          format_float s.tov_pct;
          format_float s.orb_pct;
          format_float s.ftr;
          format_float s.eff;
        ]
      )
  in

  (* 3. Render Table *)
  render_fixed_table ~table_attrs:{|data-row-link="team"|} ~id:"teams-table-inner" ~min_width:"min-w-[1240px]" ~cols rows_data

let teams_page ?(lang=I18n.Ko) ~season ~seasons ~scope ~sort ~include_mismatch stats =
 let scope_value = team_scope_to_string scope in
 let scope_option value label = let selected = if scope_value = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
 let sort_option value label = let selected = if String.lowercase_ascii sort = value then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} value selected label in
 let season_options = let base = seasons |> List.map (fun s -> let selected = if s.code = season then "selected" else "" in Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name)) |> String.concat "\n" in Printf.sprintf {html|<option value="ALL" %s>전체 시즌</option>%s|html} (if season = "ALL" then "selected" else "") base in
 let sort_options =
   [ sort_option "pts" "PTS";
     sort_option "reb" "REB";
     sort_option "ast" "AST";
     sort_option "stl" "STL";
     sort_option "blk" "BLK";
     sort_option "eff" "EFF";
     sort_option "efg" "eFG%";
     sort_option "ts_pct" "TS%";
     sort_option "tov" "TOV%";
     sort_option "orb" "ORB%";
     sort_option "ftr" "FTR";
     sort_option "fg3_pct" "3P%";
     sort_option "min_total" "MIN"; ]
   |> String.concat ""
 in
 let table = teams_table ~lang ~season ~scope stats in
 let include_checked = if include_mismatch then "checked" else "" in
 layout ~lang ~title:"WKBL 팀" ~canonical_path:"/teams"
  ~description:"WKBL 여자농구 팀 통계 - 6개 구단의 득점, 리바운드, 어시스트 등 시즌별 성적을 비교하세요."
  ~content:(Printf.sprintf
		   {html|<div class="space-y-6">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">팀</h2><p class="text-slate-600 dark:text-slate-400 text-sm">시즌과 기준(경기당/누적)으로 팀 기록을 봅니다.</p></div><a class="text-orange-700 dark:text-orange-400 hover:text-orange-700 text-sm" href="/teams">초기화</a></div><form id="teams-filter" class="grid grid-cols-1 md:grid-cols-3 gap-3" hx-get="/teams/table" hx-target="#teams-table-container" hx-trigger="change"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><select name="scope" aria-label="기준 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s%s</select><select name="sort" aria-label="정렬 기준" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select><label class="md:col-span-3 flex items-center justify-end gap-2 text-xs text-slate-600 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" title="최종 스코어와 득점 합계가 다른 경기 포함"><span>불일치 포함</span></label></form><div id="teams-table-container" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="12">%s</div><div class="mt-8 grid grid-cols-1 lg:grid-cols-2 gap-6"><div id="teams-shooting-chart" hx-get="/teams/shooting-chart?season=%s" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4 text-slate-400"><span class="htmx-indicator">차트 로딩 중...</span></div></div><div id="teams-radar-chart" hx-get="/teams/radar-chart?season=%s" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4 text-slate-400"><span class="htmx-indicator">차트 로딩 중...</span></div></div></div></div>|html}
	   (breadcrumb [("홈", "/"); ("팀", "")])
	   season_options (scope_option "per_game" "경기당") (scope_option "totals" "누적") sort_options include_checked table season season) ()

let standings_table ?(lang=I18n.Ko) ?(team_stats = []) ~season (standings : team_standing list) =
  let tr = I18n.t lang in
  let label_team = tr { ko = "팀"; en = "Team" } in
  let label_diff = tr { ko = "득실"; en = "Diff" } in
  let ff_map : (string, team_stats) Hashtbl.t = Hashtbl.create 16 in
  let team_key name =
    match team_code_of_string name with
    | Some code -> "CODE:" ^ code
    | None -> "NAME:" ^ (normalize_label name |> String.uppercase_ascii)
  in
  team_stats
  |> List.iter (fun (row: team_stats) ->
      Hashtbl.replace ff_map (team_key row.team) row);
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
    col "eFG%" ~w:(px 80) ~align:`Right ~sort:"efg" ~resp:`Hidden_lg;
    col "TOV%" ~w:(px 80) ~align:`Right ~sort:"tov" ~resp:`Hidden_lg;
    col "ORB%" ~w:(px 80) ~align:`Right ~sort:"orb" ~resp:`Hidden_lg;
    col "FTR" ~w:(px 80) ~align:`Right ~sort:"ftr" ~resp:`Hidden_lg;
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
      let ff_opt = Hashtbl.find_opt ff_map (team_key s.team_name) in
      let ff_value extract =
        match ff_opt with
        | None -> "-"
        | Some row -> Printf.sprintf "%.1f" (extract row)
      in

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
        ff_value (fun row -> row.efg_pct);
        ff_value (fun row -> row.tov_pct);
        ff_value (fun row -> row.orb_pct);
        ff_value (fun row -> row.ftr);
      ]
    )
  in

  render_fixed_table ~table_attrs:{|data-row-link="team"|} ~id:"standings-table-inner" ~min_width:"min-w-[1200px]" ~cols rows_data

let standings_page ?(lang=I18n.Ko) ~season ~seasons ~team_stats standings =
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
 let table = standings_table ~lang ~season ~team_stats standings in
 layout ~lang ~title:page_title ~canonical_path:"/standings"
  ~description:"WKBL 여자농구 순위표 - 시즌별 팀 순위, 승률, 승패 기록을 확인하세요."
  ~content:(Printf.sprintf
   {html|<div class="space-y-6">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h2><p class="text-slate-600 dark:text-slate-400 text-sm">%s</p></div></div><form id="standings-filter" class="flex gap-3" hx-get="/standings/table" hx-target="#standings-table" hx-trigger="change"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48">%s</select></form><div id="standings-table" data-skeleton="table" data-skeleton-count="6" data-skeleton-cols="10">%s</div></div>|html}
   (breadcrumb [("홈", "/"); ("순위", "")])
   (escape_html label_heading)
   (escape_html label_subtitle)
   season_options
   table) ()
