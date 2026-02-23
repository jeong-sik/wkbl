(** Clutch Time Leaderboard Page
  Clutch time = Q4 remaining 5 minutes + score diff <= 5 points *)

open Domain
open Views_common

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

      <td class="px-3 py-2 font-sans"><a href="%s" class="text-orange-500 hover:underline font-medium">%s</a></td>
      <td class="px-3 py-2 text-slate-600 dark:text-slate-400 font-sans">%s</td>
      <td class="px-3 py-2 text-right w-16">%d</td>
      <td class="px-3 py-2 text-right font-bold text-slate-900 dark:text-slate-200 w-16">%d</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-24">%d-%d</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-20">%s</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-16">%d</td>
      <td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 w-24">%d-%d</td>
     </tr>|html}
     (player_href s.cs_player_id)
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
      <th scope="col" class="px-3 py-2 text-left font-sans">%s</th>
      <th scope="col" class="px-3 py-2 text-left font-sans">%s</th>
      <th scope="col" class="px-3 py-2 text-center font-sans" title="클러치 경기">GP</th>
      <th scope="col" class="px-3 py-2 text-center font-sans" title="클러치 득점">PTS</th>
      <th scope="col" class="px-3 py-2 text-center font-sans" title="야투 성공-시도">FGM-A</th>
      <th scope="col" class="px-3 py-2 text-center font-sans" title="야투 성공률">FG%%</th>
      <th scope="col" class="px-3 py-2 text-center font-sans" title="3점 성공">3PM</th>
      <th scope="col" class="px-3 py-2 text-center font-sans" title="자유투 성공-시도">FTM-A</th>
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
		  {html|<div class="space-y-6 animate-fade-in">%s
		   <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3">
		    <div>
		     <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">%s</h2>
		     <p class="text-slate-600 dark:text-slate-400">
		      %s
		     </p>
		    </div>
	    <form action="/clutch" method="get" class="flex items-center gap-3">
     <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-48" data-auto-submit="change">
      %s
     </select>
    </form>
   </div>
	   <div class="bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
	    %s
	   </div>
	   <div class="text-xs text-slate-600 dark:text-slate-400 text-center">%s</div>
	  </div>|html}
	  (breadcrumb [("홈", "/"); ("클러치 리더", "")])
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
