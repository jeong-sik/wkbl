(** Player row component *)
let player_row ?(show_player_id=false) ?(team_cell_class="px-3 py-2") ?(include_team=true) (rank: int) (p: player_aggregate) =
  let id_badge =
    if show_player_id then
      player_id_badge p.player_id
    else
      ""
  in
  let display_name = normalize_name p.name in
  (* PER calculation inline *)
  let per =
    if p.total_minutes <= 0.0 then 0.0
    else
      let per_min = p.efficiency /. p.total_minutes in
      let per_48 = per_min *. 48.0 in
      let pace_factor = 40.0 /. 48.0 in
      let normalized = per_48 *. pace_factor *. 1.2 in
      max 0.0 (min 40.0 normalized)
  in
  let team_cell =
    if include_team then
      Printf.sprintf {html|<td class="%s whitespace-nowrap" style="width: 130px;">%s</td>|html} (escape_html team_cell_class) (team_badge p.team_name)
    else
      ""
  in
  Printf.sprintf
    {html|<tr class="group border-b border-slate-200 dark:border-slate-700/50 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-all duration-200 hover:scale-[1.01] hover:shadow-md relative z-0 hover:z-10 font-mono tabular-nums">
      <td class="px-2 py-2 text-slate-500 dark:text-slate-400 text-sm text-center font-bold whitespace-nowrap" style="width: 50px;">%d</td>
      <td class="px-3 py-2 font-medium text-slate-900 dark:text-white font-sans whitespace-nowrap" style="width: 200px;">
        <div class="flex items-center gap-3 min-w-0">
          %s
          <div class="flex items-center gap-2 min-w-0">
            <a href="%s" class="player-name hover:text-orange-600 dark:text-orange-400 transition-colors truncate break-keep min-w-0">%s</a>
            <span class="%s">%s</span>
          </div>
        </div>
      </td>
      %s
      <td class="px-3 py-2 text-right whitespace-nowrap hidden sm:table-cell text-slate-500 dark:text-slate-400 font-mono" style="width: 60px;">%d</td>
      %s%s%s%s%s%s%s%s%s
    </tr>|html}
    rank
    (player_img_tag ~class_name:"w-8 h-8 shrink-0" p.player_id p.name)
    (player_href p.player_id)
    (escape_html display_name)
    (if show_player_id then "inline-flex" else "hidden")
    id_badge
    team_cell
    p.games_played
    (points_total_cell ~extra_classes:"whitespace-nowrap" ~width_style:"width: 90px;" p.avg_points p.total_points)
    (margin_cell ~extra_classes:"hidden md:table-cell whitespace-nowrap" ~width_style:"width: 80px;" p.avg_margin)
    (stat_total_cell ~extra_classes:"whitespace-nowrap" ~width_style:"width: 85px;" p.avg_rebounds p.total_rebounds)
    (stat_total_cell ~extra_classes:"hidden md:table-cell whitespace-nowrap" ~width_style:"width: 85px;" p.avg_assists p.total_assists)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell whitespace-nowrap" ~width_style:"width: 80px;" p.avg_steals p.total_steals)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell whitespace-nowrap" ~width_style:"width: 80px;" p.avg_blocks p.total_blocks)
    (stat_total_cell ~extra_classes:"hidden lg:table-cell whitespace-nowrap" ~width_style:"width: 80px;" p.avg_turnovers p.total_turnovers)
    (stat_cell ~highlight:true ~extra_classes:"whitespace-nowrap" ~width_style:"width: 80px;" p.efficiency)
    (stat_cell ~extra_classes:"hidden sm:table-cell whitespace-nowrap" ~width_style:"width: 80px;" per)
