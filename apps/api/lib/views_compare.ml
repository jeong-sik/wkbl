open Domain
open Views_common

let compare_stat_row ?(signed=false) label val1 val2 =
 let max_val = max (abs_float val1) (abs_float val2) in
 let pct1 = if max_val = 0.0 then 0.0 else abs_float val1 /. max_val *. 100.0 in
 let pct2 = if max_val = 0.0 then 0.0 else abs_float val2 /. max_val *. 100.0 in
 let value_str v =
  if signed && v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v
 in
 Printf.sprintf
  {html|<div class="flex flex-col gap-1"><div class="flex justify-between text-xs font-bold uppercase tracking-tighter text-slate-600 dark:text-slate-400"><span>%s</span><span class="text-slate-600 dark:text-slate-400">%s</span><span>%s</span></div><div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden"><div class="flex justify-end w-1/2 border-r border-slate-300 dark:border-slate-700"><div class="bg-orange-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div><div class="flex justify-start w-1/2"><div class="bg-sky-500 h-full transition-all duration-500" style="width: %.1f%%"></div></div></div></div>|html}
  (escape_html (value_str val1)) (escape_html label) (escape_html (value_str val2)) pct1 pct2

let compare_table_empty ?(message="비교 대상을 선택하세요.") () =
 Printf.sprintf
  {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-sm text-slate-600 dark:text-slate-400">선택: %s</div>|html}
  (escape_html message)

let compare_table_row ?(signed=false) label val1 val2 =
 let max_val = max (abs_float val1) (abs_float val2) in
 let pct1 = if max_val = 0.0 then 0.0 else abs_float val1 /. max_val *. 100.0 in
 let pct2 = if max_val = 0.0 then 0.0 else abs_float val2 /. max_val *. 100.0 in
 let value_str v =
  if signed && v > 0.0 then Printf.sprintf "+%.1f" v else Printf.sprintf "%.1f" v
 in
 Printf.sprintf
  {html|<tr class="border-b border-slate-200 dark:border-slate-800/60">
    <td class="px-3 py-2 text-left font-sans text-slate-600 dark:text-slate-400 whitespace-nowrap">%s</td>
    <td class="px-3 py-2 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap">%s</td>
    <td class="px-3 py-2 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap">%s</td>
    <td class="px-3 py-2">
      <div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
        <div class="flex justify-end w-1/2 border-r border-slate-300 dark:border-slate-700">
          <div class="bg-orange-500 h-full" style="width: %.1f%%"></div>
        </div>
        <div class="flex justify-start w-1/2">
          <div class="bg-sky-500 h-full" style="width: %.1f%%"></div>
        </div>
      </div>
    </td>
  </tr>|html}
  (escape_html label)
  (escape_html (value_str val1))
  (escape_html (value_str val2))
  pct1 pct2

let compare_table_fragment ?(lang=I18n.Ko) ~left_label ~right_label rows =
 let tr = I18n.t lang in
 let rows_html =
  rows
  |> List.map (fun (label, v1, v2, signed) -> compare_table_row ~signed label v1 v2)
  |> String.concat "\n"
 in
 Printf.sprintf
  {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
    <table class="w-full text-sm table-fixed font-mono tabular-nums" aria-label="비교 테이블">
      <colgroup>
        <col style="width: 140px;">
        <col style="width: 90px;">
        <col style="width: 90px;">
        <col style="width: auto;">
      </colgroup>
      <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-600 dark:text-slate-400 text-[10px] uppercase tracking-wider">
        <tr>
          <th scope="col" class="px-3 py-2 text-left font-sans">%s</th>
          <th scope="col" class="px-3 py-2 text-right font-sans">%s</th>
          <th scope="col" class="px-3 py-2 text-right font-sans">%s</th>
          <th scope="col" class="px-3 py-2 text-right font-sans">%s</th>
        </tr>
      </thead>
      <tbody>%s</tbody>
    </table>
  </div>|html}
  (escape_html (tr { ko = "항목"; en = "Stat" }))
  (escape_html left_label)
  (escape_html right_label)
  (escape_html (tr { ko = "그래프"; en = "Graph" }))
  rows_html

let h2h_game_row (g: h2h_game) =
 let diff_color = if g.score_diff > 0 then "bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-400"
          else if g.score_diff < 0 then "bg-sky-100 dark:bg-sky-900/30 text-sky-600 dark:text-sky-400"
          else "bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400" in
 Printf.sprintf
  {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 font-mono transition-colors group">
   <td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-xs w-[100px] truncate group-hover:text-slate-900 dark:group-hover:text-slate-200 transition-colors">%s</td>
   <td class="px-3 py-2 text-right font-bold text-slate-900 dark:text-slate-200 w-[60px]">%d</td>
   <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-center text-slate-600 dark:text-slate-400 text-xs font-sans w-[40px]">vs</td>
   <td class="px-3 py-2 text-left font-bold text-slate-900 dark:text-slate-200 w-[60px]">%d</td>
   <td class="px-3 py-2 text-left text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-left text-slate-500 dark:text-slate-400 text-[10px] w-[60px]">%d</td>
   <td class="px-3 py-2 text-right text-xs font-sans w-[80px]"><span class="px-1.5 py-0.5 rounded %s font-bold">%+d</span></td>
  </tr>|html}
  (escape_html g.hg_game_date) g.player1_pts g.player1_reb g.player1_ast g.player2_pts g.player2_reb g.player2_ast diff_color g.score_diff

let h2h_game_table (p1_name: string) (p2_name: string) (games: h2h_game list) =
 let rows = games |> List.map h2h_game_row |> String.concat "\n" in
 Printf.sprintf {html|<div class="space-y-3 mt-8"><h3 class="text-center text-slate-600 dark:text-slate-400 text-sm font-bold tracking-wider">맞대결 기록</h3><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden shadow-xl"><table class="w-full text-sm table-fixed" aria-label="선수 맞대결 기록">
  <colgroup>
    <col style="width: 100px;"> <!-- Date -->
    <col style="width: 60px;">  <!-- P1 PTS -->
    <col style="width: 60px;">  <!-- REB -->
    <col style="width: 60px;">  <!-- AST -->
    <col style="width: 40px;">  <!-- vs -->
    <col style="width: 60px;">  <!-- P2 PTS -->
    <col style="width: 60px;">  <!-- REB -->
    <col style="width: 60px;">  <!-- AST -->
    <col style="width: 80px;">  <!-- DIFF -->
  </colgroup>
  <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 text-[10px] tracking-tighter"><tr><th scope="col" class="px-3 py-2 text-left font-sans">날짜</th><th scope="col" class="px-3 py-2 text-right font-sans text-orange-700 dark:text-orange-400">%s 득점</th><th scope="col" class="px-3 py-2 text-right font-sans">리바</th><th scope="col" class="px-3 py-2 text-right font-sans">어시</th><th scope="col" class="px-3 py-2"></th><th scope="col" class="px-3 py-2 text-left font-sans text-sky-600 dark:text-sky-400">%s 득점</th><th scope="col" class="px-3 py-2 text-left font-sans">리바</th><th scope="col" class="px-3 py-2 text-left font-sans">어시</th><th scope="col" class="px-3 py-2 text-right font-sans">차이</th></tr></thead><tbody>%s</tbody></table></div></div>|html} (escape_html p1_name) (escape_html p2_name) rows

let compare_page
  ?(lang=I18n.Ko)
  ~season
  ~seasons
  ~p1_season
  ~p2_season
  ~p1_query
  ~p2_query
  ~p1_id
  ~p2_id
  ~p1_candidates
  ~p2_candidates
  ~error
  ~h2h_disabled_reason
  ~p1_season_history:(p1_season_history: season_stats list)
  ~p2_season_history:(p2_season_history: season_stats list)
  (p1_selected: player_aggregate option)
  (p2_selected: player_aggregate option)
  (h2h: h2h_game list)
 =
 let season_options ~selected =
  let base =
   seasons
   |> List.map (fun (s : season_info) ->
     let is_selected = if s.code = selected then "selected" else "" in
     Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code is_selected (escape_html s.name))
   |> String.concat "\n"
  in
	  Printf.sprintf
	   {html|<option value="ALL" %s>전체 시즌</option>%s|html}
	   (if selected = "ALL" then "selected" else "")
	   base
 in
 let season_label code =
	  if code = "ALL" then "전체 시즌"
	  else
	   seasons
   |> List.find_opt (fun (s: season_info) -> s.code = code)
   |> Option.map (fun (s: season_info) -> s.name)
   |> Option.value ~default:code
 in
 let season_badge code =
  Printf.sprintf
   {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/40 text-[10px] font-mono text-slate-600 dark:text-slate-400">%s</span>|html}
   (escape_html (season_label code))
 in
 let query_display (query : string) (selected : player_aggregate option) =
  if String.trim query <> "" then query
  else
   match selected with
   | None -> ""
   | Some p -> normalize_name p.name
 in
 let p1_display = query_display p1_query p1_selected in
 let p2_display = query_display p2_query p2_selected in
 let compare_url ~season ~p1_season ~p2_season ~p1 ~p2 ~p1_id ~p2_id =
  let params =
   [ ("season", season); ("p1_season", p1_season); ("p2_season", p2_season); ("p1", p1); ("p2", p2) ]
   @ (match p1_id with None -> [] | Some id -> [ ("p1_id", id) ])
   @ (match p2_id with None -> [] | Some id -> [ ("p2_id", id) ])
  in
  let encode (k, v) = k ^ "=" ^ Uri.pct_encode v in
  "/compare?" ^ String.concat "&" (List.map encode params)
 in
 let selected_card ~accent_border ~season_code (p : player_aggregate) =
  let mp =
   if p.games_played <= 0 then 0.0 else p.total_minutes /. float_of_int p.games_played
  in
  let id_badge_html = Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge p.player_id) in
  Printf.sprintf
   {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg border-t-4 %s">
    <div class="flex items-center gap-4">
     %s
     <div class="min-w-0">
      <div class="text-lg font-black text-slate-900 dark:text-slate-200 truncate">
       <a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors">%s</a>%s
      </div>
      <div class="mt-1 text-xs text-slate-600 dark:text-slate-400 flex flex-wrap items-center gap-2">
       <span class="truncate">%s</span>
       %s
       <span class="font-mono">GP %d</span>
       <span class="font-mono">MP %.1f</span>
       <span class="font-mono">EFF %.1f</span>
      </div>
     </div>
    </div>
    <div class="mt-3 text-[11px] text-slate-600 dark:text-slate-400">이름을 다시 입력하면 선택이 초기화됩니다.</div>
   </div>|html}
   accent_border
   (player_img_tag ~class_name:"w-14 h-14" p.player_id p.name)
   (player_href p.player_id)
   (escape_html (normalize_name p.name))
   id_badge_html
   (escape_html p.team_name)
   (season_badge season_code)
   p.games_played
   mp
   p.efficiency
 in
 let candidate_row ~accent_btn_class ~slot (c : player_aggregate) =
  let mp =
   if c.games_played <= 0 then 0.0 else c.total_minutes /. float_of_int c.games_played
  in
  let p1_for_link, p2_for_link, p1_id_for_link, p2_id_for_link =
   match slot with
   | `P1 ->
     ( normalize_name c.name,
      p2_display,
      Some c.player_id,
      p2_id )
   | `P2 ->
     ( p1_display,
      normalize_name c.name,
      p1_id,
      Some c.player_id )
  in
  let url =
   compare_url
    ~season
    ~p1_season
    ~p2_season
    ~p1:p1_for_link
    ~p2:p2_for_link
    ~p1_id:p1_id_for_link
    ~p2_id:p2_id_for_link
  in
  let id_badge_html = Printf.sprintf {html|<span class="ml-2">%s</span>|html} (player_id_badge c.player_id) in
  Printf.sprintf
   {html|<div class="flex items-center justify-between gap-3 py-2 border-b border-slate-200 dark:border-slate-800/60 last:border-0">
    <div class="flex items-center gap-3 min-w-0">
     %s
     <div class="min-w-0">
      <div class="text-sm font-medium text-slate-700 dark:text-slate-300 truncate">
       <a href="%s" class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 transition-colors">%s</a>%s
      </div>
      <div class="mt-0.5 text-[11px] text-slate-600 dark:text-slate-400 flex flex-wrap items-center gap-2">
       <span class="truncate">%s</span>
       <span class="font-mono">경기 %d</span>
       <span class="font-mono">시간 %.1f</span>
       <span class="font-mono">효율 %.1f</span>
      </div>
     </div>
    </div>
    <a href="%s" class="shrink-0 px-3 py-1 rounded-lg border border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800/60 text-xs font-bold %s hover:brightness-125 transition">선택</a>
   </div>|html}
   (player_img_tag ~class_name:"w-10 h-10" c.player_id c.name)
   (escape_html url)
   (escape_html (normalize_name c.name))
   id_badge_html
   (escape_html c.team_name)
   c.games_played
   mp
   c.efficiency
   (escape_html url)
   accent_btn_class
 in
 let candidates_panel ~title ~slot ~accent_btn_class (query : string) (candidates : player_aggregate list) =
  let body =
   if String.trim query = "" then
    {html|<div class="text-slate-600 dark:text-slate-400 text-sm">이름을 입력해 검색하세요.</div>|html}
   else if candidates = [] then
    {html|<div class="text-slate-600 dark:text-slate-400 text-sm">검색 결과가 없습니다.</div>|html}
   else
    candidates |> List.map (candidate_row ~accent_btn_class ~slot) |> String.concat "\n"
  in
  Printf.sprintf
   {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
    <div class="flex items-center justify-between mb-3">
     <div class="text-xs font-bold uppercase tracking-wider text-slate-600 dark:text-slate-400">%s</div>
     <div class="text-[10px] text-slate-600 font-mono">ID 선택</div>
    </div>
    <div class="space-y-1">%s</div>
   </div>|html}
   (escape_html title)
   body
 in
 let error_html =
  match error with
  | None -> ""
  | Some msg ->
    Printf.sprintf
     {html|<div class="bg-rose-500/10 border border-rose-500/30 text-rose-700 dark:text-rose-400 rounded-xl p-5">%s</div>|html}
     (escape_html msg)
 in
 let compare_result_html =
  match p1_selected, p2_selected with
  | Some a, Some b ->
    let h2h_html =
     match h2h_disabled_reason with
     | Some msg ->
       Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-600 dark:text-slate-400">%s</div>|html}
        (escape_html msg)
     | None ->
       if h2h = [] then
        {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-600 dark:text-slate-400">No match history for this season.</div>|html}
       else
        let p1_n = normalize_name a.name in
        let p2_n = normalize_name b.name in
        let match_history = h2h_game_table p1_n p2_n h2h in
        let h2h_advanced = Views_tools.h2h_advanced_section ~p1_name:p1_n ~p2_name:p2_n h2h in
        match_history ^ h2h_advanced
    in
    (* Radar chart for visual stat comparison *)
    let radar_chart_html =
     let labels = ["PTS"; "REB"; "AST"; "STL"; "BLK"; "EFF"] in
     let values_a = [
      normalize_stat_for_radar `Points a.avg_points;
      normalize_stat_for_radar `Rebounds a.avg_rebounds;
      normalize_stat_for_radar `Assists a.avg_assists;
      normalize_stat_for_radar `Steals a.avg_steals;
      normalize_stat_for_radar `Blocks a.avg_blocks;
      normalize_stat_for_radar `Efficiency a.efficiency;
     ] in
     let values_b = [
      normalize_stat_for_radar `Points b.avg_points;
      normalize_stat_for_radar `Rebounds b.avg_rebounds;
      normalize_stat_for_radar `Assists b.avg_assists;
      normalize_stat_for_radar `Steals b.avg_steals;
      normalize_stat_for_radar `Blocks b.avg_blocks;
      normalize_stat_for_radar `Efficiency b.efficiency;
     ] in
     Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6">
       <div class="text-center mb-4">
        <h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">Visual Comparison</h3>
        <div class="flex justify-center gap-6 mt-2 text-xs">
         <div class="flex items-center gap-2">
          <span class="w-3 h-3 rounded-full bg-orange-500"></span>
          <span class="text-slate-600 dark:text-slate-400">%s</span>
         </div>
         <div class="flex items-center gap-2">
          <span class="w-3 h-3 rounded-full bg-sky-500"></span>
          <span class="text-slate-600 dark:text-slate-400">%s</span>
         </div>
        </div>
       </div>
       %s
      </div>|html}
      (escape_html (normalize_name a.name))
      (escape_html (normalize_name b.name))
      (radar_chart ~show_league_avg:true ~labels ~values_a ~values_b ~color_a:"#f97316" ~color_b:"#0ea5e9" ())
    in
    (* Trend chart *)
    let trend_chart_html =
     let has_history = List.length p1_season_history > 1 || List.length p2_season_history > 1 in
     if not has_history then ""
     else
      let max_pts =
       let p1_max = p1_season_history |> List.map (fun (s: season_stats) -> s.ss_avg_points) |> List.fold_left max 0.0 in
       let p2_max = p2_season_history |> List.map (fun (s: season_stats) -> s.ss_avg_points) |> List.fold_left max 0.0 in
       max 15.0 (max p1_max p2_max *. 1.1)
      in
      let width = 400 in
      let height = 200 in
      let padding = 40 in
      let chart_w = width - padding * 2 in
      let chart_h = height - padding * 2 in
      let all_seasons =
       (List.map (fun (s: season_stats) -> s.ss_season_code) p1_season_history @
        List.map (fun (s: season_stats) -> s.ss_season_code) p2_season_history)
       |> List.sort_uniq compare
       |> List.rev (* Most recent on right *)
      in
      let n_seasons = List.length all_seasons in
      if n_seasons < 2 then ""
      else
       let x_of_season s =
        match List.find_opt (fun x -> x = s) all_seasons with
        | None -> padding
        | Some _ ->
          let idx = match List.find_index (fun x -> x = s) all_seasons with
           | Some i -> i | None -> 0 in
          padding + (chart_w * idx / (n_seasons - 1))
       in
       let y_of_pts pts = padding + int_of_float ((1.0 -. pts /. max_pts) *. float_of_int chart_h) in
       let make_line color data =
        let points = 
         data
         |> List.map (fun (s: season_stats) ->
           let x = x_of_season s.ss_season_code in
           let y = y_of_pts s.ss_avg_points in
           Printf.sprintf "%d,%d" x y) 
         |> String.concat " "
        in
        if List.length data < 2 then "" 
        else Printf.sprintf {svg|<polyline points="%s" fill="none" stroke="%s" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"/>|svg} points color
       in
       let make_dots color data =
        data
        |> List.map (fun (s: season_stats) ->
          let x = x_of_season s.ss_season_code in
          let y = y_of_pts s.ss_avg_points in
          Printf.sprintf {svg|<circle cx="%d" cy="%d" r="4" fill="%s"/><title>%s: %.1f PTS</title>|svg} x y color s.ss_season_name s.ss_avg_points)
        |> String.concat "\n"
       in
       let x_labels =
        all_seasons
        |> List.mapi (fun i s ->
          let x = padding + (chart_w * i / (n_seasons - 1)) in
          let label = if String.length s >= 3 then String.sub s (String.length s - 2) 2 else s in
          Printf.sprintf {svg|<text x="%d" y="%d" text-anchor="middle" class="fill-slate-500 dark:fill-slate-400 text-[10px]">%s</text>|svg} x (height - 10) label)
        |> String.concat "\n"
       in
       let y_labels =
        [0.0; max_pts /. 2.0; max_pts]
        |> List.map (fun v ->
          let y = y_of_pts v in
          Printf.sprintf {svg|<text x="%d" y="%d" text-anchor="end" class="fill-slate-400 dark:fill-slate-500 text-[9px]">%.0f</text><line x1="%d" y1="%d" x2="%d" y2="%d" stroke="currentColor" class="text-slate-200 dark:text-slate-700" stroke-dasharray="2,2"/>|svg}
           (padding - 5) (y + 3) v padding y (width - padding) y)
        |> String.concat "\n"
       in
       Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6 mt-6">
         <div class="text-center mb-4">
          <h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">시즌 추이 (PTS)</h3>
          <p class="text-[10px] text-slate-400 dark:text-slate-400 mt-1">시즌별 평균 득점 추이</p>
         </div>
         <div class="flex justify-center">
          <svg viewBox="0 0 %d %d" class="w-full max-w-md">
           %s
           %s
           %s
           %s
           %s
          </svg>
         </div>
        </div>|html}
        width height
        y_labels
        x_labels
        (make_line "#f97316" p1_season_history)
        (make_line "#0ea5e9" p2_season_history)
        ((make_dots "#f97316" p1_season_history) ^ "\n" ^ (make_dots "#0ea5e9" p2_season_history))
    in
    Printf.sprintf
		     {html|<div class="space-y-8 animate-fade-in"><div class="grid grid-cols-1 md:grid-cols-3 gap-8 items-start"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-orange-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-orange-700 dark:text-orange-400"><a href="%s">%s</a></div><div class="text-slate-600 dark:text-slate-400">%s</div>%s</div></div><div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-6 space-y-6"><div class="text-center space-y-2"><h3 class="text-slate-600 dark:text-slate-400 text-sm font-bold uppercase">평균 기록</h3><p class="text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed"><span class="font-mono">MG</span>는 팀 득실마진(출전시간 가중)이며, 개인 +/-는 문자중계 기반으로 일부 경기에서만 제공됩니다. (데이터가 없으면 -)</p></div>%s%s%s%s%s%s%s%s</div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 flex flex-col items-center gap-4 shadow-xl border-t-4 border-t-sky-500">%s<div class="text-center space-y-2"><div class="text-2xl font-black text-slate-900 dark:text-slate-200 hover:text-sky-600 dark:text-sky-400"><a href="%s">%s</a></div><div class="text-slate-600 dark:text-slate-400">%s</div>%s</div></div></div>%s%s%s</div>|html}
     (player_img_tag ~class_name:"w-32 h-32" a.player_id a.name)
     (player_href a.player_id)
     (escape_html (normalize_name a.name))
     (escape_html a.team_name)
     (season_badge p1_season)
	     (compare_stat_row "득점" a.avg_points b.avg_points)
	     (compare_stat_row ~signed:true "MG" a.avg_margin b.avg_margin)
	     (compare_stat_row "리바운드" a.avg_rebounds b.avg_rebounds)
	     (compare_stat_row "어시스트" a.avg_assists b.avg_assists)
	     (compare_stat_row "스틸" a.avg_steals b.avg_steals)
	     (compare_stat_row "블록" a.avg_blocks b.avg_blocks)
	     (compare_stat_row "턴오버" a.avg_turnovers b.avg_turnovers)
	     (compare_stat_row "EFF" a.efficiency b.efficiency)
     (player_img_tag ~class_name:"w-32 h-32" b.player_id b.name)
     (player_href b.player_id)
     (escape_html (normalize_name b.name))
     (escape_html b.team_name)
     (season_badge p2_season)
     radar_chart_html
     trend_chart_html
     h2h_html
  | _ -> ""
 in
 layout ~lang ~title:"WKBL 비교" ~canonical_path:"/compare"
  ~description:"WKBL 여자농구 선수 비교 - 두 선수의 스탯을 레이더 차트로 비교하세요."
  ~scripts:With_charts
  ~content:(Printf.sprintf
   {html|<div class="space-y-8">%s
    <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
     <div>
      <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">선수 비교</h2>
      <p class="text-slate-600 dark:text-slate-400 text-sm">동명이인/표기 차이를 피하려면 <span class="font-mono text-slate-900 dark:text-slate-200">선수 고유번호</span>를 선택해 비교합니다.</p>
      <p class="text-slate-600 dark:text-slate-400 text-xs mt-1">선수 1/2는 시즌을 각각 선택할 수 있습니다. 시즌이 다르면 맞대결 기록은 표시하지 않습니다.</p>
     </div>
     <button id="share-compare-btn" type="button" data-share-compare="1" class="hidden md:flex items-center gap-2 px-4 py-2 rounded-lg border border-slate-300 dark:border-slate-600 text-slate-600 dark:text-slate-400 hover:bg-slate-100 dark:hover:bg-slate-800 transition text-sm font-medium" aria-label="비교 링크 복사">
      <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z"></path></svg>
      <span>공유</span>
     </button>
    </div>

    <form action="/compare" method="get" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
     <input type="hidden" id="p1_id" name="p1_id" value="%s">
     <input type="hidden" id="p2_id" name="p2_id" value="%s">
     <div class="grid grid-cols-1 md:grid-cols-6 gap-3">
      <select name="p1_season" aria-label="선수 1 시즌" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-2">%s</select>
      <input name="p1" value="%s" placeholder="선수 1 (이름 검색)" data-clear-target="p1_id" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none md:col-span-4">
      <select name="p2_season" aria-label="선수 2 시즌" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-2">%s</select>
      <input name="p2" value="%s" placeholder="선수 2 (이름 검색)" data-clear-target="p2_id" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-sky-500 focus:outline-none md:col-span-4">
      <div class="md:col-span-6 flex justify-end">
	       <button type="submit" class="px-4 py-2 rounded-lg bg-orange-500 text-slate-900 dark:text-slate-200 font-bold hover:bg-orange-400 transition">검색</button>
      </div>
     </div>
    </form>

    %s

    <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
     %s
     %s
    </div>

    %s
   </div>|html}
   (breadcrumb [("홈", "/"); ("선수 비교", "")])
   (escape_html (Option.value ~default:"" p1_id))
   (escape_html (Option.value ~default:"" p2_id))
   (season_options ~selected:p1_season)
   (escape_html p1_display)
   (season_options ~selected:p2_season)
   (escape_html p2_display)
   error_html
   (match p1_selected with
   | Some p -> selected_card ~accent_border:"border-t-orange-500" ~season_code:p1_season p
	   | None -> candidates_panel ~title:"선수 1" ~slot:`P1 ~accent_btn_class:"text-orange-700" p1_display p1_candidates)
   (match p2_selected with
   | Some p -> selected_card ~accent_border:"border-t-sky-500" ~season_code:p2_season p
	   | None -> candidates_panel ~title:"선수 2" ~slot:`P2 ~accent_btn_class:"text-sky-700" p2_display p2_candidates)
   compare_result_html) ()

(** Season Comparison Page - Compare a player's performance across different seasons *)
let compare_seasons_page
  ?(lang=I18n.Ko)
  ~seasons
  ~player_id
  ~player_name
  ~s1  (* Season 1 code *)
  ~s2  (* Season 2 code *)
  ~(s1_stats: season_stats option)
 ~(s2_stats: season_stats option)
 ~(all_seasons: season_stats list)  (* All available seasons for this player *)
  ~error
  ()
 =
 let season_label code =
  if code = "" then "시즌 선택"
  else
   seasons
   |> List.find_opt (fun (s: season_info) -> s.code = code)
   |> Option.map (fun (s: season_info) -> s.name)
   |> Option.value ~default:code
 in
 let season_options ~selected available_codes =
  available_codes
  |> List.map (fun code ->
    let is_selected = if code = selected then "selected" else "" in
    Printf.sprintf {html|<option value="%s" %s>%s</option>|html}
     code is_selected (escape_html (season_label code)))
  |> String.concat "\n"
 in
 let available_season_codes = List.map (fun (s: season_stats) -> s.ss_season_code) all_seasons in
 let error_html =
  match error with
  | None -> ""
  | Some msg ->
    Printf.sprintf
     {html|<div class="mb-4 p-3 bg-red-100 dark:bg-red-900/20 border border-red-300 dark:border-red-700 rounded-lg text-red-700 dark:text-red-400 text-sm">⚠️ %s</div>|html}
     (escape_html msg)
 in
 let stat_row ?(signed=false) label get_val =
  match s1_stats, s2_stats with
  | Some a, Some b ->
    let v1 = get_val a in
    let v2 = get_val b in
    compare_table_row ~signed label v1 v2
  | _ -> ""
 in
 let comparison_table =
	   match s1_stats, s2_stats with
	  | Some _, Some _ ->
	    Printf.sprintf
	     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden">
	      <div class="grid grid-cols-[1fr_100px_100px] text-sm font-bold text-slate-600 dark:text-slate-400 bg-slate-100 dark:bg-slate-800/50">
	       <div class="p-3">항목</div>
	       <div class="p-3 text-center text-orange-700 dark:text-orange-400">%s</div>
	       <div class="p-3 text-center text-sky-700 dark:text-sky-400">%s</div>
	      </div>
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
      %s
	     </div>|html}
	     (escape_html (season_label s1))
	     (escape_html (season_label s2))
	     (stat_row "경기" (fun s -> float_of_int s.ss_games_played))
	     (stat_row "출전 시간" (fun s -> s.ss_total_minutes))
	     (stat_row ~signed:true "득점" (fun s -> s.ss_avg_points))
	     (stat_row ~signed:true "리바운드" (fun s -> s.ss_avg_rebounds))
	     (stat_row ~signed:true "어시스트" (fun s -> s.ss_avg_assists))
	     (stat_row ~signed:true "스틸" (fun s -> s.ss_avg_steals))
	     (stat_row ~signed:true "블록" (fun s -> s.ss_avg_blocks))
	     (stat_row ~signed:true "턴오버" (fun s -> s.ss_avg_turnovers))
	     (stat_row ~signed:true "효율" (fun s -> s.ss_efficiency))
	     (stat_row ~signed:true "TS%" (fun s -> s.ss_ts_pct *. 100.0))
	     (stat_row ~signed:true "득실마진" (fun s -> s.ss_margin))
	  | _ ->
	    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-8 text-center text-slate-500 dark:text-slate-400">
	     비교할 두 시즌을 선택하세요.
	    </div>|html}
 in
 let trend_indicator (s: season_stats) =
  if s.ss_efficiency >= 15.0 then "🔥"
  else if s.ss_efficiency >= 10.0 then "📈"
  else if s.ss_efficiency >= 5.0 then "➖"
  else "📉"
 in
 let season_card (s: season_stats) accent =
  Printf.sprintf
	   {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 border-t-4 %s">
	    <div class="flex items-center justify-between mb-3">
	     <a href="%s" class="text-lg font-bold text-slate-900 dark:text-slate-200 hover:underline">%s</a>
	     <span class="text-2xl">%s</span>
	    </div>
	    <div class="grid grid-cols-2 gap-2 text-sm">
	     <div class="text-slate-600 dark:text-slate-400">경기</div>
	     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%d</div>
	     <div class="text-slate-600 dark:text-slate-400">득점</div>
	     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%.1f</div>
     <div class="text-slate-600 dark:text-slate-400">리바</div>
     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%.1f</div>
     <div class="text-slate-600 dark:text-slate-400">어시</div>
     <div class="text-right font-mono text-slate-900 dark:text-slate-200">%.1f</div>
     <div class="text-slate-600 dark:text-slate-400">효율</div>
     <div class="text-right font-mono font-bold text-slate-900 dark:text-slate-200">%.1f</div>
    </div>
   </div>|html}
   accent
   (season_href s.ss_season_code)
   (escape_html s.ss_season_name)
   (trend_indicator s)
   s.ss_games_played
   s.ss_avg_points
   s.ss_avg_rebounds
   s.ss_avg_assists
   s.ss_efficiency
 in
 let season_cards_html =
  let s1_card = match s1_stats with
   | Some s -> season_card s "border-t-orange-500"
   | None -> ""
  in
  let s2_card = match s2_stats with
   | Some s -> season_card s "border-t-sky-500"
   | None -> ""
  in
  if s1_card <> "" || s2_card <> "" then
   Printf.sprintf {html|<div class="grid grid-cols-1 md:grid-cols-2 gap-4 mb-6">%s%s</div>|html} s1_card s2_card
  else ""
 in
 let content = Printf.sprintf
  {html|<div class="max-w-4xl mx-auto py-8 px-4">%s
   <div class="mb-6">
    <a href="%s" class="text-orange-500 hover:underline text-sm">← 선수 프로필로</a>
   </div>

   <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200 mb-2">
    📊 시즌 비교
   </h1>
   <p class="text-slate-600 dark:text-slate-400 mb-6">
    <a href="%s" class="text-orange-500 hover:underline font-semibold">%s</a>의 시즌별 기록을 비교합니다.
   </p>

   %s

   <form method="GET" action="/compare/seasons" class="mb-6 bg-slate-100 dark:bg-slate-800/50 rounded-xl p-4">
    <input type="hidden" name="player" value="%s">
    <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
     <div>
      <label class="block text-sm font-bold text-slate-700 dark:text-slate-300 mb-2">시즌 1 (주황)</label>
      <select name="s1" aria-label="비교 스탯 1" class="w-full px-3 py-2 rounded-lg border border-slate-300 dark:border-slate-700 bg-white dark:bg-slate-900 text-slate-900 dark:text-slate-200">
       <option value="">시즌 선택</option>
       %s
      </select>
     </div>
     <div>
      <label class="block text-sm font-bold text-slate-700 dark:text-slate-300 mb-2">시즌 2 (파랑)</label>
      <select name="s2" aria-label="비교 스탯 2" class="w-full px-3 py-2 rounded-lg border border-slate-300 dark:border-slate-700 bg-white dark:bg-slate-900 text-slate-900 dark:text-slate-200">
       <option value="">시즌 선택</option>
       %s
      </select>
     </div>
    </div>
    <button type="submit" class="mt-4 w-full md:w-auto px-6 py-2 bg-orange-500 hover:bg-orange-600 text-white font-bold rounded-lg transition-colors">
     시즌 비교
    </button>
   </form>

   %s

   %s

   <div class="mt-8 text-center">
    <a href="/compare?p1=%s" class="text-orange-500 hover:underline text-sm">
     다른 선수와 비교 →
    </a>
   </div>
  </div>|html}
  (breadcrumb [("홈", "/"); (player_name, player_href player_id); ("시즌 비교", "")])
  (player_href player_id)
  (player_href player_id)
  (escape_html player_name)
  error_html
  (escape_html player_id)
  (season_options ~selected:s1 available_season_codes)
  (season_options ~selected:s2 available_season_codes)
  season_cards_html
  comparison_table
  (Uri.pct_encode player_name)
 in
 layout
  ~lang
  ~title:(Printf.sprintf "WKBL 시즌 비교 - %s" player_name)
  ~canonical_path:(Printf.sprintf "/compare/seasons?player=%s" (Uri.pct_encode player_id))
  ~description:(Printf.sprintf "%s의 시즌별 성적 비교 - WKBL 여자농구 분석" player_name)
  ~content ()
