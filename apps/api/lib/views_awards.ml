(** Awards page view *)

open Domain
open Views_common
open Views_leaders

let awards_page ?(lang=I18n.Ko) ~player_info_map ~season ~seasons ~include_mismatch ~prev_season_name ?(official_awards=[]) ~mvp:mvp_list ~mip:mip_list () =
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
 let official_section =
  if official_awards = [] then ""
  else begin
    (* Group by category *)
    let tbl : (string, (string * string option * string option) list) Hashtbl.t = Hashtbl.create 16 in
    let cat_order = ref [] in
    official_awards |> List.iter (fun (_sn, (cat, (pname, (sval, votes)))) ->
      if not (Hashtbl.mem tbl cat) then cat_order := cat :: !cat_order;
      let prev = try Hashtbl.find tbl cat with Not_found -> [] in
      Hashtbl.replace tbl cat (prev @ [(pname, sval, votes)])
    );
    let categories = List.rev !cat_order in
    let cards = categories |> List.map (fun cat ->
      let entries = try Hashtbl.find tbl cat with Not_found -> [] in
      let rows = entries |> List.mapi (fun i (pname, sval, votes) ->
        let detail = match sval, votes with
          | Some sv, _ -> Printf.sprintf {html| <span class="text-orange-500 font-mono text-sm">%s</span>|html} (escape_html sv)
          | _, Some v -> Printf.sprintf {html| <span class="text-blue-500 font-mono text-sm">%s표</span>|html} (escape_html v)
          | None, None -> ""
        in
        let rank_badge = if i = 0 then {html|<span class="text-amber-500 font-bold">1</span>|html}
          else Printf.sprintf {html|<span class="text-slate-500">%d</span>|html} (i + 1)
        in
        let name_linked = match player_info_map with
          | Some map ->
            let norm = normalize_name pname in
            (match Hashtbl.find_opt map norm with
             | Some pi -> Printf.sprintf {html|<a href="/player/%s" class="text-orange-500 hover:text-orange-400 hover:underline">%s</a>|html} pi.id (escape_html pname)
             | None -> escape_html pname)
          | None -> escape_html pname
        in
        Printf.sprintf {html|<div class="flex items-center gap-3 py-1.5 %s"><div class="w-6 text-center text-xs">%s</div><div class="flex-1 font-medium text-slate-900 dark:text-slate-200 text-sm">%s%s</div></div>|html}
          (if i mod 2 = 1 then "bg-slate-50/50 dark:bg-slate-800/20 rounded" else "")
          rank_badge name_linked detail
      ) |> String.concat "\n" in
      Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><h3 class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-wider text-xs mb-4">%s</h3>%s</div>|html}
        (escape_html cat) rows
    ) |> String.concat "\n" in
    Printf.sprintf {html|<div class="space-y-4"><h3 class="text-xl font-bold text-slate-900 dark:text-slate-200">공식 수상</h3><div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">%s</div></div>|html} cards
  end
 in
 let disclaimer_html =
  if official_awards <> [] then
   {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/60 p-5 text-slate-600 dark:text-slate-400 text-sm leading-relaxed"><div class="font-bold text-slate-700 dark:text-slate-300 mb-2">참고</div><ul class="list-disc list-inside space-y-1"><li>공식 수상: WKBL 공식 사이트 데이터</li><li>MVP (EFF): <span class="font-mono text-slate-700 dark:text-slate-300">EFF(경기 기여도) 평균</span> 상위 (통계 추정)</li><li>MIP (EFF 변화): 전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">EFF 변화</span> 상위 (통계 추정)</li></ul></div>|html}
  else
   {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/60 p-5 text-slate-600 dark:text-slate-400 text-sm leading-relaxed"><div class="font-bold text-slate-700 dark:text-slate-300 mb-2">통계 기반 수상 (비공식)</div><ul class="list-disc list-inside space-y-1"><li>MVP: <span class="font-mono text-slate-700 dark:text-slate-300">EFF(경기 기여도) 평균</span> 상위</li><li>MIP: 전 시즌 대비 <span class="font-mono text-slate-700 dark:text-slate-300">EFF 변화</span> 상위 (두 시즌 모두 10경기 이상)</li><li>공식 수상 데이터가 아직 없어서, 현재는 박스스코어 기반 지표로만 추정합니다.</li></ul></div>|html}
 in
 layout
  ~lang
  ~title:"WKBL 수상"
  ~canonical_path:"/awards"
  ~description:"WKBL 여자농구 시상 - MVP, MIP 등 시즌별 통계 기반 어워드 추정 순위"
  ~content:(Printf.sprintf
	   {html|<div class="space-y-8 animate-fade-in">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">수상</h2><p class="text-slate-600 dark:text-slate-400">시즌별 수상 기록과 통계 기반 추정입니다.</p></div><form action="/awards" method="get" class="flex flex-wrap items-center gap-3"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-40" data-auto-submit="change">%s</select><label class="flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" data-auto-submit="change" title="최종 스코어와 득점 합계가 다른 경기 포함"><span>불일치 포함</span></label></form></div>%s<div class="grid grid-cols-1 md:grid-cols-2 gap-6">%s%s</div>%s</div>|html}
	   (breadcrumb [("홈", "/"); ("수상", "")])
	   season_options
	   include_checked
   official_section
   mvp_card
   mip_card
   disclaimer_html) ()
