(** Team-specific view functions for WKBL Analytics *)
(** Contains team profile page. *)

open Domain
open Views_common

let team_profile_page ?(lang=I18n.Ko) ?(player_info_map=None) (detail: team_full_detail) ~season ~seasons =
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
  Printf.sprintf {html|<option value="ALL" %s>전체 시즌</option>%s|html} (if season = "ALL" then "selected" else "") base
 in
 let standing_info = match s with | Some st -> Printf.sprintf {html|<div class="flex gap-6 text-sm"><div class="flex flex-col"><span>승</span><span class="text-2xl font-black text-slate-900 dark:text-slate-200">%d</span></div><div class="flex flex-col"><span>패</span><span class="text-2xl font-black text-slate-900 dark:text-slate-200">%d</span></div><div class="flex flex-col"><span>승률</span><span class="text-2xl font-black text-orange-700 dark:text-orange-400">%.3f</span></div><div class="flex flex-col"><span>게임차</span><span class="text-2xl font-black text-slate-500 dark:text-slate-400">%.1f</span></div></div>|html} st.wins st.losses st.win_pct st.gb | None -> "" in
 (* Prev/Next season navigation *)
 let season_nav =
  if season = "ALL" then ""
  else
   let idx = List.mapi (fun i (s: season_info) -> (i, s)) seasons
    |> List.find_opt (fun (_, s) -> s.code = season)
    |> Option.map fst in
   match idx with
   | None -> ""
   | Some i ->
    let prev_link =
     if i > 0 then
      let prev = List.nth seasons (i - 1) in
      Printf.sprintf {html|<a href="%s?season=%s" class="text-sm text-slate-500 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors" title="%s">&larr; %s</a>|html}
       (team_href t) prev.code (escape_html prev.name) (escape_html prev.name)
     else ""
    in
    let next_link =
     if i < List.length seasons - 1 then
      let next = List.nth seasons (i + 1) in
      Printf.sprintf {html|<a href="%s?season=%s" class="text-sm text-slate-500 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 transition-colors" title="%s">%s &rarr;</a>|html}
       (team_href t) next.code (escape_html next.name) (escape_html next.name)
     else ""
    in
    Printf.sprintf {html|<div class="flex items-center justify-between">%s%s</div>|html} prev_link next_link
 in
 (* Team Metrics Summary — PTS/G, OPP PTS/G, DIFF, eFG%, Pace *)
 let team_metrics_section =
  let rank_of ~compare_fn standings =
   let sorted = List.sort compare_fn standings in
   List.mapi (fun i (s: team_standing) -> (i + 1, s)) sorted
   |> List.find_opt (fun (_, s) -> s.team_name = t)
   |> Option.map fst
  in
  let rank_label = function
   | Some r ->
    let n = List.length detail.tfd_standings in
    Printf.sprintf {html|<span class="text-[10px] text-slate-400 dark:text-slate-400 font-mono">%d/%d</span>|html} r n
   | None -> ""
  in
  let efg_pct_str, pace_str =
   match detail.tfd_team_totals with
   | Some totals ->
    let fg_a = totals.fg2_a + totals.fg3_a in
    let fg_m = totals.fg2_m + totals.fg3_m in
    let efg = if fg_a = 0 then 0.0 else (float fg_m +. 0.5 *. float totals.fg3_m) /. float fg_a *. 100.0 in
    let poss = float fg_a +. 0.44 *. float totals.ft_a +. float totals.turnovers -. float totals.reb_off in
    let pace = if totals.min_total <= 0.0 then 0.0 else 40.0 *. (poss /. totals.min_total) in
    (Printf.sprintf "%.1f" efg, Printf.sprintf "%.1f" pace)
   | None -> ("-", "-")
  in
  match s with
  | None -> ""
  | Some st ->
   let pts_rank = rank_of ~compare_fn:(fun a b -> compare b.avg_pts a.avg_pts) detail.tfd_standings in
   let opp_rank = rank_of ~compare_fn:(fun a b -> compare a.avg_opp_pts b.avg_opp_pts) detail.tfd_standings in
   let diff_cls =
    if st.diff > 0.0 then "text-sky-600 dark:text-sky-400"
    else if st.diff < 0.0 then "text-rose-600 dark:text-rose-400"
    else "text-slate-500 dark:text-slate-400"
   in
   let metric_card label value rank_html color =
    Printf.sprintf
     {html|<div class="flex flex-col items-center p-3 bg-slate-100 dark:bg-slate-800/60 rounded-lg border border-slate-200 dark:border-slate-700/50">
      <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">%s</div>
      <div class="mt-1 text-xl font-black %s font-mono tabular-nums">%s</div>
      %s
     </div>|html}
     label color value rank_html
   in
   Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
     <div class="grid grid-cols-3 sm:grid-cols-5 gap-3">
      %s%s%s%s%s
     </div>
    </div>|html}
    (metric_card "PTS/G" (Printf.sprintf "%.1f" st.avg_pts) (rank_label pts_rank) "text-orange-700 dark:text-orange-400")
    (metric_card "OPP" (Printf.sprintf "%.1f" st.avg_opp_pts) (rank_label opp_rank) "text-slate-700 dark:text-slate-300")
    (metric_card "DIFF" (Printf.sprintf "%+.1f" st.diff) "" diff_cls)
    (metric_card "eFG%%" efg_pct_str "" "text-emerald-600 dark:text-emerald-400")
    (metric_card "Pace" pace_str "" "text-sky-600 dark:text-sky-400")
 in
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
    let player_info =
     match player_info_map with
     | Some map -> Hashtbl.find_opt map p.player_id
     | None -> None
    in
    player_row ~show_player_id ~include_team:false ~team_cell_class:"px-3 py-2" ~player_info (i + 1) p)
  |> String.concat "\n"
 in
 let roster_totals_data =
  detail.tfd_roster
  |> List.mapi (fun i (p: player_aggregate) ->
   let key = normalize_name p.name in
   let show_player_id =
    match Hashtbl.find_opt roster_name_counts key with
    | Some c when c > 1 -> true | _ -> false
   in
   [ string_of_int (i + 1);
     player_name_cell ~show_player_id p.player_id p.name;
     string_of_int p.games_played;
     string_of_int (int_of_float p.total_minutes);
     string_of_int p.total_points;
     string_of_int p.total_rebounds;
     string_of_int p.total_assists;
     string_of_int p.total_steals;
     string_of_int p.total_blocks;
     string_of_int p.total_turnovers;
     format_float p.efficiency ])
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
    let player_info =
     match player_info_map with
     | Some map -> Hashtbl.find_opt map p.player_id
     | None -> None
    in
    let disambiguation =
     if show_player_id then
      Printf.sprintf {html|<div class="mt-0.5">%s</div>|html}
       (player_disambiguation_line ~team_name:p.team_name ~player_id:p.player_id player_info)
     else ""
    in
    Printf.sprintf
	     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
	      <div class="flex items-center justify-between gap-3">
	       <div class="flex items-center gap-3 min-w-0">
	        <div class="shrink-0">%s</div>
	        <div class="min-w-0">
	         <div class="text-sm font-bold text-slate-900 dark:text-slate-200 truncate">
	          <a href="%s" class="player-name hover:text-orange-700 dark:text-orange-400 transition-colors">%s</a>
	         </div>
	         %s
	         <div class="mt-0.5 text-[11px] text-slate-500 dark:text-slate-400 font-mono tabular-nums">경기 %d • 기여도 %.1f</div>
	        </div>
	       </div>
	       <div class="text-right shrink-0">
	        <div class="text-[10px] text-slate-500 dark:text-slate-400 font-mono tracking-widest">득점</div>
	        <div class="text-lg font-black text-orange-700 dark:text-orange-400 font-mono tabular-nums">%.1f</div>
	       </div>
	      </div>
	      <div class="mt-3 grid grid-cols-3 gap-2 text-xs font-mono tabular-nums">
	       <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
	        <div class="text-[10px] text-slate-500 dark:text-slate-400 tracking-widest">득실</div>
	        <div class="mt-0.5">%s</div>
	       </div>
	       <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
	        <div class="text-[10px] text-slate-500 dark:text-slate-400 tracking-widest">리바</div>
	        <div class="mt-0.5 text-slate-900 dark:text-slate-200 font-bold">%.1f</div>
	       </div>
	       <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-2 text-center">
	        <div class="text-[10px] text-slate-500 dark:text-slate-400 tracking-widest">어시</div>
	        <div class="mt-0.5 text-slate-900 dark:text-slate-200 font-bold">%.1f</div>
	       </div>
	      </div>
	     </div>|html}
     (player_img_tag ~class_name:"w-10 h-10 border border-slate-300 dark:border-slate-700 shadow-sm" p.player_id p.name)
     (player_href p.player_id)
     (escape_html (normalize_name p.name))
     disambiguation
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
	                       <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap"><tr><th scope="col" class="px-2 py-2 text-center font-sans whitespace-nowrap">#</th><th scope="col" class="px-3 py-2 text-left font-sans whitespace-nowrap">선수</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell whitespace-nowrap" title="경기 수">GP</th><th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400 whitespace-nowrap" title="득점(경기당)">PTS</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell whitespace-nowrap" title="득실마진(MG, 출전시간 가중)">MG</th><th scope="col" class="px-3 py-2 text-right whitespace-nowrap" title="리바운드(경기당)">REB</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell whitespace-nowrap" title="어시스트(경기당)">AST</th><th scope="col" class="px-3 py-2 text-right hidden lg:table-cell whitespace-nowrap" title="스틸(경기당)">STL</th><th scope="col" class="px-3 py-2 text-right hidden lg:table-cell whitespace-nowrap" title="블록(경기당)">BLK</th><th scope="col" class="px-3 py-2 text-right hidden lg:table-cell whitespace-nowrap" title="턴오버(경기당)">TO</th><th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400 whitespace-nowrap" title="EFF(기여도)">EFF</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell whitespace-nowrap" title="PER">PER</th></tr></thead><tbody>%s</tbody></table>|html}
	                      roster_rows
	                    in
 (* Compute team-level footer sums from roster *)
 let team_sum_gp = List.fold_left (fun acc (p: player_aggregate) -> acc + p.games_played) 0 detail.tfd_roster in
 let team_sum_min = List.fold_left (fun acc (p: player_aggregate) -> acc +. p.total_minutes) 0.0 detail.tfd_roster in
 let team_sum_pts = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_points) 0 detail.tfd_roster in
 let team_sum_reb = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_rebounds) 0 detail.tfd_roster in
 let team_sum_ast = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_assists) 0 detail.tfd_roster in
 let team_sum_stl = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_steals) 0 detail.tfd_roster in
 let team_sum_blk = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_blocks) 0 detail.tfd_roster in
 let team_sum_tov = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_turnovers) 0 detail.tfd_roster in
 let team_sum_eff = List.fold_left (fun acc (p: player_aggregate) -> acc +. p.efficiency) 0.0 detail.tfd_roster in
 let team_sum_fgm = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_fg_made) 0 detail.tfd_roster in
 let team_sum_fga = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_fg_att) 0 detail.tfd_roster in
 let team_sum_fg3m = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_fg3_made) 0 detail.tfd_roster in
 let team_sum_fg3a = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_fg3_att) 0 detail.tfd_roster in
 let team_sum_ftm = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_ft_made) 0 detail.tfd_roster in
 let team_sum_fta = List.fold_left (fun acc (p: player_aggregate) -> acc + p.total_ft_att) 0 detail.tfd_roster in
 let roster_totals_table =
  let totals_cols = [
    col ~align:`Center ~w:(px 50) "#";
    col ~sticky:true ~w:(px 200) "선수";
    col ~align:`Right ~resp:`Hidden_sm ~w:(px 60) ~title:"경기 수" "GP";
    col ~align:`Right ~resp:`Hidden_md ~w:(px 80) ~title:"출전시간 합계" "MIN";
    col ~align:`Right ~highlight:true ~w:(px 80) ~title:"득점 합계" "PTS";
    col ~align:`Right ~w:(px 80) ~title:"리바운드 합계" "REB";
    col ~align:`Right ~resp:`Hidden_md ~w:(px 80) ~title:"어시스트 합계" "AST";
    col ~align:`Right ~resp:`Hidden_lg ~w:(px 70) ~title:"스틸 합계" "STL";
    col ~align:`Right ~resp:`Hidden_lg ~w:(px 70) ~title:"블록 합계" "BLK";
    col ~align:`Right ~resp:`Hidden_lg ~w:(px 70) ~title:"턴오버 합계" "TOV";
    col ~align:`Right ~highlight:true ~w:(px 80) ~title:"EFF(기여도)" "EFF";
  ] in
  let totals_foot = [[
    ""; {html|<span class="font-sans font-bold">팀 합계</span>|html};
    string_of_int team_sum_gp;
    string_of_int (int_of_float team_sum_min);
    string_of_int team_sum_pts;
    string_of_int team_sum_reb;
    string_of_int team_sum_ast;
    string_of_int team_sum_stl;
    string_of_int team_sum_blk;
    string_of_int team_sum_tov;
    format_float team_sum_eff;
  ]] in
  render_fixed_table ~striped:true ~aria_label:"팀 로스터 (합계)"
    ~foot_data:totals_foot
    ~id:"roster-totals" ~min_width:"min-w-[900px]" ~cols:totals_cols roster_totals_data
 in
 let roster_shooting_table =
  let pct made att =
   if att > 0 then Printf.sprintf "%.1f" (float_of_int made /. float_of_int att *. 100.0)
   else "-"
  in
  let shooting_rows =
   detail.tfd_roster
   |> List.mapi (fun i (p: player_aggregate) ->
    let key = normalize_name p.name in
    let show_player_id =
     match Hashtbl.find_opt roster_name_counts key with
     | Some c when c > 1 -> true
     | _ -> false
    in
    let id_badge = if show_player_id then player_id_badge p.player_id else "" in
    let display_name = normalize_name p.name in
    let fg_made = p.total_fg_made in
    let fg_att = p.total_fg_att in
    let fg3_made = p.total_fg3_made in
    let fg3_att = p.total_fg3_att in
    let ft_made = p.total_ft_made in
    let ft_att = p.total_ft_att in
    Printf.sprintf
     {html|<tr class="group border-b border-slate-200 dark:border-slate-700/50 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors font-mono tabular-nums">
       <td class="px-2 py-2 text-slate-500 dark:text-slate-400 text-sm text-center font-bold" style="width: 50px;">%d</td>
       <td class="px-3 py-2 font-medium text-slate-900 dark:text-white font-sans whitespace-nowrap" style="width: 180px;">
         <div class="flex items-center gap-3 min-w-0">
           %s
           <div class="flex flex-col min-w-0">
             <div class="flex items-center gap-2 min-w-0">
               <a href="%s" class="player-name hover:text-orange-700 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
               <span class="%s">%s</span>
             </div>
           </div>
         </div>
       </td>
       <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 hidden sm:table-cell" style="width: 50px;">%d</td>
       <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300" style="width: 55px;">%d</td>
       <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400" style="width: 55px;">%d</td>
       <td class="px-3 py-2 text-right text-orange-700 dark:text-orange-400 font-bold" style="width: 65px;">%s</td>
       <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300" style="width: 55px;">%d</td>
       <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400" style="width: 55px;">%d</td>
       <td class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400 font-bold" style="width: 65px;">%s</td>
       <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300" style="width: 55px;">%d</td>
       <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400" style="width: 55px;">%d</td>
       <td class="px-3 py-2 text-right text-sky-600 dark:text-sky-400 font-bold" style="width: 65px;">%s</td>
     </tr>|html}
     (i + 1)
     (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
     (player_href p.player_id)
     (escape_html display_name)
     (if show_player_id then "inline-flex" else "hidden")
     id_badge
     p.games_played
     fg_made fg_att (pct fg_made fg_att)
     fg3_made fg3_att (pct fg3_made fg3_att)
     ft_made ft_att (pct ft_made ft_att))
   |> String.concat "\n"
  in
  let shooting_foot = Printf.sprintf
   {html|<tfoot class="bg-slate-200 dark:bg-slate-700/60 font-bold text-slate-900 dark:text-slate-200">
      <tr>
        <td class="px-2 py-2" style="width: 50px;"></td>
        <td class="px-3 py-2 font-sans" style="width: 180px;">팀 합계</td>
        <td class="px-3 py-2 hidden sm:table-cell" style="width: 50px;"></td>
        <td class="px-3 py-2 text-right" style="width: 55px;">%d</td>
        <td class="px-3 py-2 text-right" style="width: 55px;">%d</td>
        <td class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" style="width: 65px;">%s</td>
        <td class="px-3 py-2 text-right" style="width: 55px;">%d</td>
        <td class="px-3 py-2 text-right" style="width: 55px;">%d</td>
        <td class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400" style="width: 65px;">%s</td>
        <td class="px-3 py-2 text-right" style="width: 55px;">%d</td>
        <td class="px-3 py-2 text-right" style="width: 55px;">%d</td>
        <td class="px-3 py-2 text-right text-sky-600 dark:text-sky-400" style="width: 65px;">%s</td>
      </tr>
    </tfoot>|html}
   team_sum_fgm team_sum_fga (pct team_sum_fgm team_sum_fga)
   team_sum_fg3m team_sum_fg3a (pct team_sum_fg3m team_sum_fg3a)
   team_sum_ftm team_sum_fta (pct team_sum_ftm team_sum_fta)
  in
  Printf.sprintf
   {html|<table class="roster-table min-w-[850px] w-full text-xs sm:text-sm font-mono table-fixed tabular-nums" aria-label="팀 로스터 (슈팅)">
    <colgroup>
      <col style="width: 50px;">
      <col style="width: 180px;">
      <col class="hidden sm:table-column" style="width: 50px;">
      <col style="width: 55px;">
      <col style="width: 55px;">
      <col style="width: 65px;">
      <col style="width: 55px;">
      <col style="width: 55px;">
      <col style="width: 65px;">
      <col style="width: 55px;">
      <col style="width: 55px;">
      <col style="width: 65px;">
    </colgroup>
    <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap">
      <tr>
        <th scope="col" class="px-2 py-2 text-center font-sans">#</th>
        <th scope="col" class="px-3 py-2 text-left font-sans">선수</th>
        <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="경기 수">GP</th>
        <th scope="col" class="px-3 py-2 text-right" title="야투 성공">FGM</th>
        <th scope="col" class="px-3 py-2 text-right" title="야투 시도">FGA</th>
        <th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" title="야투 성공률">FG%%</th>
        <th scope="col" class="px-3 py-2 text-right" title="3점 성공">3PM</th>
        <th scope="col" class="px-3 py-2 text-right" title="3점 시도">3PA</th>
        <th scope="col" class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400" title="3점 성공률">3P%%</th>
        <th scope="col" class="px-3 py-2 text-right" title="자유투 성공">FTM</th>
        <th scope="col" class="px-3 py-2 text-right" title="자유투 시도">FTA</th>
        <th scope="col" class="px-3 py-2 text-right text-sky-600 dark:text-sky-400" title="자유투 성공률">FT%%</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
    %s
   </table>|html}
   shooting_rows shooting_foot
 in
 let roster_advanced_table =
  let team_totals = detail.tfd_team_totals in
  let advanced_rows =
   detail.tfd_roster
   |> List.mapi (fun i (p: player_aggregate) ->
    let key = normalize_name p.name in
    let show_player_id =
     match Hashtbl.find_opt roster_name_counts key with
     | Some c when c > 1 -> true | _ -> false
    in
    let id_badge = if show_player_id then player_id_badge p.player_id else "" in
    let display_name = normalize_name p.name in
    let fga = p.total_fg_att in
    let fta = p.total_ft_att in
    let ts = Stats.true_shooting_pct ~pts:p.total_points ~fga ~fta in
    let efg = Stats.effective_fg_pct ~fg_made:p.total_fg_made ~fg3_made:p.total_fg3_made ~fga in
    let per = Stats.calculate_per ~total_minutes:p.total_minutes ~efficiency:p.efficiency in
    let usg =
     match team_totals with
     | Some tt ->
       let team_fga = tt.fg2_a + tt.fg3_a in
       let team_poss = Stats.estimate_team_possessions
         ~team_fga ~team_fta:tt.ft_a ~team_tov:tt.turnovers ~team_oreb:tt.reb_off in
       let games_ratio = float_of_int p.games_played /. float_of_int (max 1 tt.gp) in
       let player_poss = float_of_int fga +. 0.44 *. float_of_int fta +. float_of_int p.total_turnovers in
       let scaled = team_poss *. games_ratio in
       if scaled <= 0.0 then 0.0 else player_poss /. scaled *. 100.0
     | None -> 0.0
    in
    let mpg = if p.games_played = 0 then 0.0 else p.total_minutes /. float_of_int p.games_played in
    Printf.sprintf
     {html|<tr class="group border-b border-slate-200 dark:border-slate-700/50 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors font-mono tabular-nums">
       <td class="px-2 py-2 text-slate-500 dark:text-slate-400 text-sm text-center font-bold" style="width: 50px;">%d</td>
       <td class="px-3 py-2 font-medium text-slate-900 dark:text-white font-sans whitespace-nowrap" style="width: 180px;">
         <div class="flex items-center gap-3 min-w-0">
           %s
           <div class="flex flex-col min-w-0">
             <div class="flex items-center gap-2 min-w-0">
               <a href="%s" class="player-name hover:text-orange-700 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
               <span class="%s">%s</span>
             </div>
           </div>
         </div>
       </td>
       <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 hidden sm:table-cell" style="width: 50px;">%d</td>
       <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300" style="width: 70px;">%.1f</td>
       <td class="px-3 py-2 text-right text-orange-700 dark:text-orange-400 font-bold" style="width: 70px;">%.1f</td>
       <td class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400 font-bold" style="width: 70px;">%.1f</td>
       <td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300" style="width: 70px;">%.1f</td>
       <td class="px-3 py-2 text-right text-sky-600 dark:text-sky-400 font-bold" style="width: 70px;">%.1f</td>
     </tr>|html}
     (i + 1)
     (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
     (player_href p.player_id)
     (escape_html display_name)
     (if show_player_id then "inline-flex" else "hidden")
     id_badge
     p.games_played mpg
     ts efg usg per)
   |> String.concat "\n"
  in
  let advanced_foot =
   match team_totals with
   | Some tt ->
     let t_fga = tt.fg2_a + tt.fg3_a in
     let t_fgm = tt.fg2_m + tt.fg3_m in
     let t_ts = Stats.true_shooting_pct ~pts:tt.pts ~fga:t_fga ~fta:tt.ft_a in
     let t_efg = Stats.effective_fg_pct ~fg_made:t_fgm ~fg3_made:tt.fg3_m ~fga:t_fga in
     let t_per = Stats.calculate_per ~total_minutes:tt.min_total ~efficiency:team_sum_eff in
     Printf.sprintf
      {html|<tfoot class="bg-slate-200 dark:bg-slate-700/60 font-bold text-slate-900 dark:text-slate-200">
        <tr>
          <td class="px-2 py-2" style="width: 50px;"></td>
          <td class="px-3 py-2 font-sans" style="width: 180px;">팀 평균</td>
          <td class="px-3 py-2 hidden sm:table-cell" style="width: 50px;"></td>
          <td class="px-3 py-2 text-right" style="width: 70px;">-</td>
          <td class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" style="width: 70px;">%.1f</td>
          <td class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400" style="width: 70px;">%.1f</td>
          <td class="px-3 py-2 text-right" style="width: 70px;">-</td>
          <td class="px-3 py-2 text-right text-sky-600 dark:text-sky-400" style="width: 70px;">%.1f</td>
        </tr>
      </tfoot>|html}
      t_ts t_efg t_per
   | None -> ""
  in
  Printf.sprintf
   {html|<table class="roster-table min-w-[700px] w-full text-xs sm:text-sm font-mono table-fixed tabular-nums" aria-label="팀 로스터 (어드밴스드)">
    <colgroup>
      <col style="width: 50px;">
      <col style="width: 180px;">
      <col class="hidden sm:table-column" style="width: 50px;">
      <col style="width: 70px;">
      <col style="width: 70px;">
      <col style="width: 70px;">
      <col style="width: 70px;">
      <col style="width: 70px;">
    </colgroup>
    <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap">
      <tr>
        <th scope="col" class="px-2 py-2 text-center font-sans">#</th>
        <th scope="col" class="px-3 py-2 text-left font-sans">선수</th>
        <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="경기 수">GP</th>
        <th scope="col" class="px-3 py-2 text-right" title="경기당 출전시간">MPG</th>
        <th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" title="True Shooting %%">TS%%</th>
        <th scope="col" class="px-3 py-2 text-right text-emerald-600 dark:text-emerald-400" title="Effective FG %%">eFG%%</th>
        <th scope="col" class="px-3 py-2 text-right" title="Usage Rate %%">USG%%</th>
        <th scope="col" class="px-3 py-2 text-right" title="Player Efficiency Rating">PER</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
    %s
   </table>|html}
   advanced_rows advanced_foot
 in
 let roster_desktop_section =
  Printf.sprintf
   {html|<div class="space-y-3">
    <div class="flex gap-1 bg-slate-100 dark:bg-slate-800/60 rounded-lg p-1 w-fit">
     <button onclick="switchRosterTab('pergame')" id="tab-pergame" class="px-4 py-1.5 rounded-md text-sm font-bold transition-colors bg-white dark:bg-slate-700 text-slate-900 dark:text-slate-200 shadow-sm">경기당</button>
     <button onclick="switchRosterTab('totals')" id="tab-totals" class="px-4 py-1.5 rounded-md text-sm font-bold transition-colors text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-300">합계</button>
     <button onclick="switchRosterTab('shooting')" id="tab-shooting" class="px-4 py-1.5 rounded-md text-sm font-bold transition-colors text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-300">슈팅</button>
     <button onclick="switchRosterTab('advanced')" id="tab-advanced" class="px-4 py-1.5 rounded-md text-sm font-bold transition-colors text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-300">어드밴스드</button>
    </div>
    <div id="panel-pergame" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">%s</div>
    <div id="panel-totals" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg hidden">%s</div>
    <div id="panel-shooting" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg hidden">%s</div>
    <div id="panel-advanced" class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg hidden">%s</div>
    <script>
    function switchRosterTab(tab) {
     var tabs = ['pergame', 'totals', 'shooting', 'advanced'];
     tabs.forEach(function(t) {
      var panel = document.getElementById('panel-' + t);
      var btn = document.getElementById('tab-' + t);
      if (t === tab) {
       panel.classList.remove('hidden');
       btn.className = 'px-4 py-1.5 rounded-md text-sm font-bold transition-colors bg-white dark:bg-slate-700 text-slate-900 dark:text-slate-200 shadow-sm';
      } else {
       panel.classList.add('hidden');
       btn.className = 'px-4 py-1.5 rounded-md text-sm font-bold transition-colors text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-300';
      }
     });
    }
    </script>
   </div>|html}
   roster_table_inner
   roster_totals_table
   roster_shooting_table
   roster_advanced_table
 in
	 let season_label =
		  if season = "ALL" then
		   "전체 시즌"
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
		    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 text-sm text-slate-500 dark:text-slate-400 shadow-lg">경기 결과 없음</div>|html}
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
		         {html|<a href="%s" class="group block w-2 sm:w-1.5 h-24" title="%s"><div class="h-1/2 flex items-end">%s</div><div class="h-1/2 flex items-start">%s</div></a>|html}
		         (boxscore_href g.tgr_game_id)
		         title
		         pos_bar
		         neg_bar)
		     |> String.concat "\n"
			    in
					    Printf.sprintf
					     {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg"><div class="flex items-center justify-between gap-3"><div class="text-[11px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">%s</div><div class="text-xs font-mono text-slate-500 dark:text-slate-400 tabular-nums">%d-%d</div></div><div class="mt-2 flex items-center justify-between gap-3 text-[11px] text-slate-500 dark:text-slate-400 font-mono tabular-nums"><span>평균 득실 %+.1f</span><span>±%d</span></div><div class="mt-3 overflow-x-auto pb-1"><div class="relative h-24 w-max"><div class="absolute left-0 right-0 top-1/2 h-px bg-slate-100 dark:bg-slate-800/80"></div><div class="flex items-stretch gap-0.5 h-24 w-max pr-1">%s</div></div></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">막대: 득실(팀-상대). 클릭하면 박스스코어.</div></div>|html}
				     (escape_html season_label)
				     wins
				     losses
			     avg_margin
			     max_abs_margin
			     bars
			 in
			 let today_ymd =
			   (* KST is UTC+9 and has no DST. Compare ISO date strings lexicographically. *)
			   let tm = Unix.gmtime (Unix.time () +. (9. *. 3600.)) in
			   Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
			 in
			 let status_label_for_date game_date =
			   let game_ymd =
			     if String.length game_date >= 10 then String.sub game_date 0 10 else game_date
			   in
			   if String.length game_ymd = 10 && String.compare game_ymd today_ymd < 0 then
			     "점수 없음"
			   else
			     "예정"
			 in
			 let game_rows =
			  let (_, _, rows_rev) =
			   List.fold_left (fun (w, l, acc) (g: team_game_result) ->
			    let has_score = g.tgr_team_score > 0 && g.tgr_opponent_score > 0 in
			    let new_w, new_l =
			     if not has_score then (w, l)
			     else if g.tgr_is_win then (w + 1, l) else (w, l + 1) in
			    let res_class =
			      if not has_score then "text-slate-500 dark:text-slate-400"
			      else if g.tgr_is_win then "text-sky-600 dark:text-sky-400"
			      else "text-rose-600 dark:text-rose-400"
			    in
			    let res_label = if not has_score then "—" else if g.tgr_is_win then "승" else "패" in
			    let record_str =
			     if has_score then Printf.sprintf "(%d-%d)" new_w new_l else "" in
			    let score_inner =
			      if has_score then
			        let margin = g.tgr_team_score - g.tgr_opponent_score in
			        let margin_class =
			          if margin > 0 then "text-sky-600 dark:text-sky-400"
			          else if margin < 0 then "text-rose-600 dark:text-rose-400"
			          else "text-slate-500 dark:text-slate-400"
			        in
			        let margin_str =
			          if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin
			        in
			        Printf.sprintf
			          {html|<div class="flex items-center justify-end gap-2 flex-nowrap tabular-nums"><span class="whitespace-nowrap">%d - %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono %s whitespace-nowrap min-w-[48px] text-center">%s</span></div>|html}
			          g.tgr_team_score
			          g.tgr_opponent_score
			          margin_class
			          (escape_html margin_str)
			      else
			        let label = status_label_for_date g.tgr_game_date in
			        Printf.sprintf
			          {html|<div class="flex items-center justify-end"><span class="text-xs text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap">%s</span></div>|html}
			          (escape_html label)
			    in
			    let opponent_label =
			     if g.tgr_is_home then "vs " ^ g.tgr_opponent else "@ " ^ g.tgr_opponent
			    in
			    let opponent_href = team_href g.tgr_opponent in
			    let date_short =
			     if String.length g.tgr_game_date >= 10 then
			      String.sub g.tgr_game_date 5 5
			     else
			      g.tgr_game_date
			    in
			    let row = Printf.sprintf
			     {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-24"><a href="%s" class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors" title="%s"><span class="sm:hidden">%s</span><span class="hidden sm:inline">%s</span></a></td><td class="px-3 py-2 text-slate-900 dark:text-slate-200"><a class="block truncate hover:text-orange-600 dark:hover:text-orange-400 transition-colors" href="%s" title="%s">%s</a></td><td class="px-3 py-2 text-center font-bold %s whitespace-nowrap w-14"><span class="inline-flex items-center justify-center w-7">%s</span></td><td class="px-3 py-2 text-right font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap w-36">%s</td><td class="px-2 py-2 text-right text-[10px] font-mono text-slate-400 dark:text-slate-400 whitespace-nowrap hidden sm:table-cell">%s</td></tr>|html}
			     (boxscore_href g.tgr_game_id)
			     (escape_html g.tgr_game_date)
			     (escape_html date_short)
			     (escape_html g.tgr_game_date)
			     (escape_html opponent_href)
			     (escape_html opponent_label)
			     (escape_html opponent_label)
			     res_class
			     res_label
			     score_inner
			     (escape_html record_str)
			    in
			    (new_w, new_l, row :: acc))
			   (0, 0, []) detail.tfd_game_results
			  in
			  rows_rev |> List.rev |> String.concat "\n"
			 in
 (* Four Factors Section — with league rankings *)
 let four_factors_section =
  match detail.tfd_team_totals with
  | None -> ""
  | Some totals ->
    let ff = Stats.four_factors_of_totals totals in
    (* Compute Four Factors for all teams and rank *)
    let all_ff = detail.tfd_all_totals
     |> List.map (fun (tt: team_totals) -> (tt.team, Stats.four_factors_of_totals tt)) in
    let n = List.length all_ff in
    let ff_rank_of ~extract ~higher_is_better =
     let sorted = List.sort (fun (_, a) (_, b) ->
      let va = extract a and vb = extract b in
      if higher_is_better then compare vb va else compare va vb)
      all_ff in
     List.mapi (fun i (name, _) -> (i + 1, name)) sorted
     |> List.find_opt (fun (_, name) -> name = t)
     |> Option.map fst
    in
    let rank_badge = function
     | Some r ->
      Printf.sprintf {html|<span class="text-[10px] text-slate-400 dark:text-slate-400 font-mono">%d/%d</span>|html} r n
     | None -> ""
    in
    let efg_rank = ff_rank_of ~extract:(fun f -> f.Stats.efg_pct) ~higher_is_better:true in
    let tov_rank = ff_rank_of ~extract:(fun f -> f.Stats.tov_pct) ~higher_is_better:false in
    let orb_rank = ff_rank_of ~extract:(fun f -> f.Stats.orb_pct) ~higher_is_better:true in
    let ftr_rank = ff_rank_of ~extract:(fun f -> f.Stats.ftr) ~higher_is_better:true in
    let stat_card label value desc color rank_html =
     Printf.sprintf
      {html|<div class="bg-slate-100 dark:bg-slate-800/60 rounded-lg p-3 border border-slate-200 dark:border-slate-700/50">
       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">%s</div>
       <div class="mt-1 text-xl font-black %s font-mono tabular-nums">%.1f%%</div>
       %s
       <div class="mt-1 text-[10px] text-slate-500 dark:text-slate-400 leading-tight">%s</div>
      </div>|html}
      label color value rank_html desc
    in
    Printf.sprintf
     {html|<div class="space-y-4">
      <h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">Four Factors</h2>
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
     (stat_card "eFG%%" ff.efg_pct "슈팅 효율" "text-emerald-600 dark:text-emerald-400" (rank_badge efg_rank))
     (stat_card "TOV%%" ff.tov_pct "턴오버율 (낮을수록 좋음)" "text-rose-500 dark:text-rose-400" (rank_badge tov_rank))
     (stat_card "ORB%%" ff.orb_pct "공격 리바운드율" "text-sky-600 dark:text-sky-400" (rank_badge orb_rank))
     (stat_card "FTR" ff.ftr "자유투 시도율" "text-orange-700 dark:text-orange-400" (rank_badge ftr_rank))
 in
 (* JSON-LD structured data for SportsTeam schema *)
 let json_ld_data =
   let record_str = match s with
     | Some st -> Printf.sprintf {|,"record":"%d-%d"|} st.wins st.losses
     | None -> ""
   in
   Printf.sprintf {|{"@context":"https://schema.org","@type":"SportsTeam","name":"%s","sport":"Basketball","memberOf":{"@type":"SportsOrganization","name":"WKBL"},"url":"https://wkbl.win/team/%s"%s}|}
     (escape_html t) (Uri.pct_encode t) record_str
 in
				 layout ~lang ~title:(t ^ " | WKBL 팀")
				  ~canonical_path:(team_href t)
				  ~description:(Printf.sprintf "%s WKBL 여자농구 팀 프로필 - 로스터, 경기 결과, 시즌 기록" (escape_html t))
				  ~json_ld:json_ld_data
				  ~content:(Printf.sprintf {html|<div class="space-y-6 sm:space-y-8 animate-fade-in">%s<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 sm:p-8 shadow-2xl flex flex-col md:flex-row items-center md:items-start gap-6 sm:gap-8"><div class="w-24 h-24 sm:w-32 sm:h-32 bg-slate-100 dark:bg-slate-800 rounded-xl flex items-center justify-center p-3 sm:p-4 border-2 border-slate-300 dark:border-slate-700 shadow-inner">%s</div><div class="text-center md:text-left space-y-4 w-full"><div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-3"><h1 class="text-3xl sm:text-4xl font-black text-slate-900 dark:text-slate-200">%s</h1><form action="%s" method="get" class="flex flex-col sm:flex-row items-stretch sm:items-center justify-center sm:justify-end gap-2 w-full sm:w-auto"><span class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-widest font-bold">시즌</span><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none w-full sm:w-48" onchange="this.form.submit()">%s</select></form></div>%s</div></div>%s%s<div class="grid grid-cols-1 lg:grid-cols-3 gap-6 sm:gap-8"><div class="space-y-4 lg:col-span-2"><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">로스터</h2><div id="roster-cards" class="sm:hidden space-y-3">%s</div><details id="roster-table-details" class="sm:hidden bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4" ontoggle="(function(d){var c=document.getElementById('roster-cards'); if(c) c.hidden = d.open;})(this)"><summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">표로 보기</summary><div class="mt-3 overflow-x-auto">%s</div></details><div class="hidden sm:block">%s</div></div><div class="space-y-6 lg:col-span-1"><h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">경기 결과</h2>%s%s<h2 class="text-xl font-bold text-slate-900 dark:text-slate-200">전체 경기</h2><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto max-h-[600px] overflow-y-auto shadow-lg"><table class="min-w-[480px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="시즌 전체 경기">
  	          <colgroup>
  	            <col style="width: 96px;"> <!-- Date -->
  	            <col style="width: auto;"> <!-- Opponent -->
  	            <col style="width: 56px;"> <!-- Result -->
            <col style="width: 144px;"> <!-- Score -->
            <col class="hidden sm:table-column" style="width: 56px;"> <!-- Record -->
          </colgroup>
	          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap"><tr><th scope="col" class="px-3 py-2 text-left font-sans">날짜</th><th scope="col" class="px-3 py-2 text-left font-sans">상대</th><th scope="col" class="px-3 py-2 text-center font-sans" title="승/패">결과</th><th scope="col" class="px-3 py-2 text-right font-sans">점수</th><th scope="col" class="px-2 py-2 text-right font-sans hidden sm:table-cell">기록</th></tr></thead><tbody>%s</tbody></table></div></div></div></div>|html}
	     (breadcrumb [("홈", "/"); ("팀", "/teams"); (t, "")])
	     (team_logo_tag ~class_name:"w-16 h-16 sm:w-24 sm:h-24" t)
	     (escape_html t)
	     (team_href t)
	     season_options
	     standing_info
	     season_nav
	     team_metrics_section
	     roster_cards
	     roster_table_inner
	     roster_desktop_section
	     game_results_chart
	     four_factors_section
	     game_rows) ()

(** Team H2H comparison page *)
let team_h2h_page ?(lang=I18n.Ko) ~team1 ~team2 ~season ~seasons (games : Domain.game_info list) =
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
  (* Calculate H2H record *)
  let t1_wins, t2_wins, total_diff =
    games |> List.fold_left (fun (w1, w2, diff) (g: game_info) ->
      let is_t1_home = g.gi_home_team_name = team1 in
      let t1_score = if is_t1_home then g.gi_home_score else g.gi_away_score in
      let t2_score = if is_t1_home then g.gi_away_score else g.gi_home_score in
      let d = t1_score - t2_score in
      if d > 0 then (w1 + 1, w2, diff + d)
      else if d < 0 then (w1, w2 + 1, diff + d)
      else (w1, w2, diff)
    ) (0, 0, 0)
  in
  let total_games = List.length games in
  let avg_diff = if total_games > 0 then float_of_int total_diff /. float_of_int total_games else 0.0 in
  let diff_sign = if avg_diff > 0.0 then "+" else "" in
  let diff_color =
    if avg_diff > 0.0 then "text-emerald-600 dark:text-emerald-400"
    else if avg_diff < 0.0 then "text-rose-600 dark:text-rose-400"
    else "text-slate-500"
  in
  (* Game rows *)
  let game_rows =
    games |> List.map (fun (g: game_info) ->
      let is_t1_home = g.gi_home_team_name = team1 in
      let t1_score = if is_t1_home then g.gi_home_score else g.gi_away_score in
      let t2_score = if is_t1_home then g.gi_away_score else g.gi_home_score in
      let result = if t1_score > t2_score then "W" else if t1_score < t2_score then "L" else "D" in
      let result_color = match result with
        | "W" -> "bg-emerald-100 text-emerald-700 dark:bg-emerald-900/30 dark:text-emerald-400"
        | "L" -> "bg-rose-100 text-rose-700 dark:bg-rose-900/30 dark:text-rose-400"
        | _ -> "bg-slate-100 text-slate-700 dark:bg-slate-800 dark:text-slate-400"
      in
      let home_away = if is_t1_home then "🏠" else "✈️" in
      Printf.sprintf
        {html|<tr class="border-b border-slate-100 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-800/50 transition-colors">
          <td class="px-3 py-2 text-slate-600 dark:text-slate-400">%s</td>
          <td class="px-3 py-2 text-center">%s</td>
          <td class="px-3 py-2 text-center">
            <span class="px-2 py-0.5 rounded text-xs font-bold %s">%s</span>
          </td>
          <td class="px-3 py-2 text-right font-bold">
            <span class="%s">%d</span> - <span class="%s">%d</span>
          </td>
          <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400">%+d</td>
        </tr>|html}
        (escape_html g.gi_game_date)
        home_away
        result_color result
        (if t1_score > t2_score then "text-emerald-600 dark:text-emerald-400" else "text-slate-700 dark:text-slate-300")
        t1_score
        (if t2_score > t1_score then "text-emerald-600 dark:text-emerald-400" else "text-slate-700 dark:text-slate-300")
        t2_score
        (t1_score - t2_score)
    )
    |> String.concat "\n"
  in
  let no_games_msg =
    if List.length games = 0 then
      {html|<div class="text-center text-slate-500 dark:text-slate-400 py-8">
        <p class="text-lg">선택한 기간에 두 팀 간 경기 기록이 없습니다.</p>
        <p class="text-sm mt-2">다른 시즌을 선택하거나 '전체 시즌'을 선택해보세요.</p>
      </div>|html}
    else ""
  in
  layout ~lang ~title:(Printf.sprintf "%s 대 %s | 팀 전적 비교" team1 team2)
    ~canonical_path:(Printf.sprintf "/teams/h2h?team1=%s&team2=%s" (escape_html team1) (escape_html team2))
    ~description:(Printf.sprintf "%s와 %s의 상대 전적, 경기별 결과 비교" (escape_html team1) (escape_html team2))
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">%s
        <!-- Header with team selection -->
        <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl">
          <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200 text-center mb-6">팀 간 전적 비교</h1>
          <form action="/teams/h2h" method="get" class="flex flex-col md:flex-row items-center justify-center gap-4">
            <div class="flex items-center gap-3">
              %s
              <label for="h2h-team1" class="sr-only">첫 번째 팀</label>
              <input type="text" id="h2h-team1" name="team1" value="%s" placeholder="팀 1" aria-label="첫 번째 팀"
                class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm w-36 focus:border-orange-500 focus:outline-none" />
            </div>
            <span class="text-xl font-bold text-slate-400" aria-hidden="true">대</span>
            <div class="flex items-center gap-3">
              <label for="h2h-team2" class="sr-only">두 번째 팀</label>
              <input type="text" id="h2h-team2" name="team2" value="%s" placeholder="팀 2" aria-label="두 번째 팀"
                class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm w-36 focus:border-orange-500 focus:outline-none" />
              %s
            </div>
            <label for="h2h-season" class="sr-only">시즌 선택</label>
            <select id="h2h-season" name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
              %s
            </select>
            <button type="submit" class="px-4 py-2 bg-orange-600 hover:bg-orange-700 text-white rounded-lg font-bold transition-colors">
              비교
            </button>
          </form>
        </div>

        <!-- H2H Summary -->
        <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl">
          <div class="flex flex-col md:flex-row items-center justify-between gap-6">
            <!-- Team 1 -->
            <div class="flex flex-col items-center gap-2">
              %s
              <span class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</span>
              <span class="text-3xl font-black text-emerald-600 dark:text-emerald-400">%d승</span>
            </div>
            <!-- VS & Stats -->
            <div class="text-center">
              <div class="text-4xl font-black text-slate-400 mb-2">대</div>
              <div class="text-sm text-slate-500 dark:text-slate-400">총 %d경기</div>
              <div class="text-lg font-bold %s">평균 %s%.1f점</div>
            </div>
            <!-- Team 2 -->
            <div class="flex flex-col items-center gap-2">
              %s
              <span class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</span>
              <span class="text-3xl font-black text-rose-600 dark:text-rose-400">%d승</span>
            </div>
          </div>
        </div>

        %s

        <!-- Game List -->
        <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-xl">
          <table class="w-full text-sm font-mono tabular-nums">
            <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-xs">
              <tr>
                <th scope="col" class="px-3 py-2 text-left">날짜</th>
                <th scope="col" class="px-3 py-2 text-center whitespace-nowrap" title="홈/원정">홈/원정</th>
                <th scope="col" class="px-3 py-2 text-center">결과</th>
                <th scope="col" class="px-3 py-2 text-right">스코어</th>
                <th scope="col" class="px-3 py-2 text-right">점차</th>
              </tr>
            </thead>
            <tbody>%s</tbody>
          </table>
        </div>
      </div>|html}
      (breadcrumb [("홈", "/"); ("팀", "/teams"); (team1, team_href team1); ("전적 비교", "")])
      (team_logo_tag ~class_name:"w-12 h-12" team1)
      (escape_html team1)
      (escape_html team2)
      (team_logo_tag ~class_name:"w-12 h-12" team2)
      season_options
      (team_logo_tag ~class_name:"w-20 h-20" team1)
      (escape_html team1)
      t1_wins
      total_games
      diff_color diff_sign avg_diff
      (team_logo_tag ~class_name:"w-20 h-20" team2)
      (escape_html team2)
      t2_wins
      no_games_msg
      game_rows
    ) ()

(** DB QA dashboard page *)
