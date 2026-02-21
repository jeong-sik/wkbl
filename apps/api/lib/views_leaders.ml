open Domain
open Views_common

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
    {html|<a href="%s" class="flex flex-col items-center %s group cursor-pointer transition-transform duration-200 %s">
     <div class="relative"><div class="%s rounded-full %s ring-2 overflow-hidden group-hover:ring-4 transition-all duration-200">%s</div>
      <div class="absolute -bottom-1 left-1/2 -translate-x-1/2 %s text-xs font-bold w-5 h-5 rounded-full flex items-center justify-center text-white group-hover:scale-110 transition-transform">%d</div>
     </div>
     <div class="text-lg font-bold text-slate-900 dark:text-slate-200 mt-2 group-hover:text-orange-500 transition-colors">%s</div>
     <span class="text-xs text-slate-600 dark:text-slate-400 truncate max-w-[80px]">%s</span>
     %s
    </a>|html}
    (player_href l.le_player_id) mt hover_scale size bg (player_img_tag ~class_name:(size ^ " object-cover") l.le_player_id l.le_player_name)
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
     Printf.sprintf {html|<a href="%s" class="flex items-center justify-between py-1.5 border-b border-slate-200/50 dark:border-slate-800/40 last:border-0 hover:bg-slate-100 dark:hover:bg-slate-800/50 -mx-2 px-2 rounded transition-colors group">
      <div class="flex items-center gap-2 min-w-0">
       <span class="text-slate-500 dark:text-slate-400 font-mono text-xs w-4 group-hover:text-orange-500 transition-colors">%d</span>
       %s
       <div class="flex flex-col min-w-0">
        <span class="text-xs text-slate-700 dark:text-slate-300 group-hover:text-orange-500 transition-colors truncate">%s</span>
        %s
       </div>
      </div>
      <span class="font-mono text-xs text-slate-600 dark:text-slate-400 group-hover:font-bold transition-all">%s</span>
     </a>|html}
      (player_href l.le_player_id)
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
	   {html|<div class="space-y-8 animate-fade-in">%s<div class="flex flex-col md:flex-row md:items-center md:justify-between gap-3"><div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">부문별 1위</h2><p class="text-slate-600 dark:text-slate-400">부문별 선두 선수 기록입니다.</p></div><form action="/leaders" method="get" class="grid grid-cols-1 sm:grid-cols-2 gap-3 w-full md:w-auto"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" data-auto-submit="change">%s</select><select name="scope" aria-label="기준 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" data-auto-submit="change">%s</select></form></div>%s</div>|html}
	   (breadcrumb [("홈", "/"); ("리더", "")])
	   season_options scope_options content)
	  ()

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
    let cat_order_rev =
      List.fold_left (fun acc (_sn, (cat, (pname, (sval, votes)))) ->
        let is_new = not (Hashtbl.mem tbl cat) in
        let prev = Option.value (Hashtbl.find_opt tbl cat) ~default:[] in
        Hashtbl.replace tbl cat (prev @ [(pname, sval, votes)]);
        if is_new then cat :: acc else acc
      ) [] official_awards
    in
    let categories = List.rev cat_order_rev in
    let cards = categories |> List.map (fun cat ->
      let entries = Option.value (Hashtbl.find_opt tbl cat) ~default:[] in
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
    {html|<a href="%s" class="flex flex-col items-center %s group cursor-pointer transition-transform duration-200 %s">
     <div class="relative"><div class="%s rounded-full %s ring-2 overflow-hidden group-hover:ring-4 transition-all duration-200">%s</div>
      <div class="absolute -bottom-1 left-1/2 -translate-x-1/2 %s text-xs font-bold w-5 h-5 rounded-full flex items-center justify-center text-white group-hover:scale-110 transition-transform">%d</div>
     </div>
     <div class="text-lg font-bold text-slate-900 dark:text-slate-200 mt-2 group-hover:text-orange-500 transition-colors">%s</div>
     <span class="text-xs text-slate-600 dark:text-slate-400 truncate max-w-[80px]">%s</span>
     %s
    </a>|html}
    (player_href l.le_player_id) mt hover_scale size bg (player_img_tag ~class_name:(size ^ " object-cover") l.le_player_id l.le_player_name)
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
     Printf.sprintf {html|<a href="%s" class="flex items-center justify-between py-1.5 border-b border-slate-200/50 dark:border-slate-800/40 last:border-0 hover:bg-slate-100 dark:hover:bg-slate-800/50 -mx-2 px-2 rounded transition-colors group">
      <div class="flex items-center gap-2 min-w-0">
       <span class="text-slate-500 dark:text-slate-400 font-mono text-xs w-4 group-hover:text-orange-500 transition-colors">%d</span>
       %s
       <div class="flex flex-col min-w-0">
        <span class="text-xs text-slate-700 dark:text-slate-300 group-hover:text-orange-500 transition-colors truncate">%s</span>
        %s
       </div>
      </div>
      <span class="font-mono text-xs text-slate-600 dark:text-slate-400 group-hover:font-bold transition-all">%s</span>
     </a>|html}
      (player_href l.le_player_id)
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

