(** Players page view — extracted from Views *)

open Domain
open Views_common

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
 let table = Views_home.players_table ~lang ~player_info_map players in
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
