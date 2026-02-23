(** Leader card and leaders page views *)

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
