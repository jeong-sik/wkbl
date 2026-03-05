(** Player-specific view functions for WKBL Analytics *)
(** Contains player profile and game logs pages. *)

open Domain
open Views_common

(** Milestone Tracker - Career milestones for players *)

(** Milestone thresholds for each stat category *)
let points_milestones = [500; 1000; 1500; 2000; 3000; 4000]
let rebounds_milestones = [200; 400; 600; 800; 1000; 1500]
let assists_milestones = [100; 200; 300; 500; 700; 1000]
let games_milestones = [50; 100; 150; 200; 300]

type milestone_status = {
  ms_category: string;
  ms_label: string;
  ms_icon: string;
  ms_current: int;
  ms_achieved: int list;  (* milestones already achieved *)
  ms_next: int option;    (* next milestone to reach *)
  ms_progress: float;     (* progress towards next milestone (0.0-1.0) *)
}

(** Calculate milestone status for a stat *)
let calc_milestone_status ~category ~label ~icon ~current ~milestones : milestone_status =
  let achieved = List.filter (fun m -> current >= m) milestones in
  let next = List.find_opt (fun m -> current < m) milestones in
  let progress =
    match next with
    | None -> 1.0 (* All milestones achieved *)
    | Some target ->
        let prev = match List.rev achieved with [] -> 0 | h :: _ -> h in
        let range = float_of_int (target - prev) in
        let current_in_range = float_of_int (current - prev) in
        if range > 0.0 then current_in_range /. range else 0.0
  in
  { ms_category = category;
    ms_label = label;
    ms_icon = icon;
    ms_current = current;
    ms_achieved = achieved;
    ms_next = next;
    ms_progress = progress;
  }

(** Format milestone number with commas *)
let format_milestone n =
  if n >= 1000 then
    Printf.sprintf "%dk" (n / 1000)
  else
    string_of_int n

(** Generate a single milestone progress bar *)
let milestone_progress_bar (ms: milestone_status) =
  let achieved_badges =
    ms.ms_achieved
    |> List.map (fun m ->
        Printf.sprintf
          {html|<span class="inline-flex items-center justify-center w-6 h-6 rounded-full bg-amber-400/20 border border-amber-400/50 text-amber-500 text-[9px] font-bold shadow-sm" title="%s %s 달성">🏅</span>|html}
          (escape_html ms.ms_label)
          (format_int_commas m))
    |> String.concat ""
  in
  let progress_pct = int_of_float (ms.ms_progress *. 100.0) in
  let progress_bar_html =
    match ms.ms_next with
    | None ->
        (* All milestones achieved - show completion bar *)
        {html|<div class="relative w-full h-2 bg-amber-400/20 rounded-full overflow-hidden"><div class="absolute inset-0 bg-gradient-to-r from-amber-400 to-amber-500 rounded-full"></div></div>|html}
    | Some target ->
        let prev = match List.rev ms.ms_achieved with [] -> 0 | h :: _ -> h in
        Printf.sprintf
          {html|<div class="relative w-full h-2 bg-slate-200 dark:bg-slate-700 rounded-full overflow-hidden"><div class="absolute inset-0 bg-gradient-to-r from-orange-400 to-amber-400 rounded-full transition-all duration-500" style="width: %d%%"></div></div><div class="flex justify-between text-[10px] font-mono text-slate-600 dark:text-slate-400 mt-1"><span>%s</span><span class="text-orange-500 font-bold">%s</span></div>|html}
          progress_pct
          (format_int_commas prev)
          (format_int_commas target)
  in
  let remaining_html =
    match ms.ms_next with
    | None ->
        {html|<span class="text-amber-500 font-bold text-xs">완료!</span>|html}
    | Some target ->
        let remaining = target - ms.ms_current in
        Printf.sprintf
          {html|<span class="text-slate-600 dark:text-slate-400 text-xs font-mono">%s 남음</span>|html}
          (format_int_commas remaining)
  in
  Printf.sprintf
    {html|<div class="space-y-2">
      <div class="flex items-center justify-between gap-2">
        <div class="flex items-center gap-2">
          <span class="text-lg">%s</span>
          <span class="text-slate-700 dark:text-slate-300 font-bold text-sm">%s</span>
        </div>
        <div class="flex items-center gap-1">
          %s
        </div>
      </div>
      <div class="flex items-center gap-3">
        <span class="text-orange-500 font-black text-xl font-mono min-w-[60px]">%s</span>
        <div class="flex-1 space-y-0">
          %s
        </div>
        %s
      </div>
    </div>|html}
    (escape_html ms.ms_icon)
    (escape_html ms.ms_label)
    achieved_badges
    (format_int_commas ms.ms_current)
    progress_bar_html
    remaining_html

(** Generate the full milestone tracker card *)
let milestone_tracker_card (avg: player_aggregate) =
  if avg.games_played <= 0 then
    "" (* No games played, don't show milestones *)
  else
    let points_ms = calc_milestone_status
      ~category:"points" ~label:"득점" ~icon:"🏀"
      ~current:avg.total_points ~milestones:points_milestones in
    let rebounds_ms = calc_milestone_status
      ~category:"rebounds" ~label:"리바운드" ~icon:"📊"
      ~current:avg.total_rebounds ~milestones:rebounds_milestones in
    let assists_ms = calc_milestone_status
      ~category:"assists" ~label:"어시스트" ~icon:"🎯"
      ~current:avg.total_assists ~milestones:assists_milestones in
    let games_ms = calc_milestone_status
      ~category:"games" ~label:"출전 경기" ~icon:"📅"
      ~current:avg.games_played ~milestones:games_milestones in
    let total_achieved =
      List.length points_ms.ms_achieved +
      List.length rebounds_ms.ms_achieved +
      List.length assists_ms.ms_achieved +
      List.length games_ms.ms_achieved
    in
    let total_milestones =
      List.length points_milestones +
      List.length rebounds_milestones +
      List.length assists_milestones +
      List.length games_milestones
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg">
        <div class="flex items-start justify-between gap-4 mb-4">
          <div class="min-w-0">
	            <h2 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2">
	              <span class="text-lg">🎖️</span> 커리어 기록
	            </h2>
	            <div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">
	              커리어 누적 기록 마일스톤 달성 현황 (개인 누적 구간 기준)
	            </div>
          </div>
          <div class="flex items-center gap-2">
            <span class="px-2 py-0.5 rounded bg-amber-500/10 border border-amber-500/30 text-[10px] font-mono text-amber-700 dark:text-amber-400 whitespace-nowrap">
              🏅 %d/%d
            </span>
          </div>
        </div>
        <div class="space-y-5">
          %s
          %s
          %s
          %s
        </div>
      </div>|html}
      total_achieved
      total_milestones
      (milestone_progress_bar points_ms)
      (milestone_progress_bar rebounds_ms)
      (milestone_progress_bar assists_ms)
      (milestone_progress_bar games_ms)

(** Advanced Stats Card - TS%, eFG%, PER, Usage% *)
let advanced_stats_card (avg: player_aggregate) (seasons: season_stats list) =
  if avg.games_played <= 0 then ""
  else
    let per = Stats.per_of_player_aggregate avg in
    let rpg = avg.avg_rebounds in
    let apg = avg.avg_assists in
    (* efficiency is already AVG(game_score), i.e. per-game average *)
    let eff_per_game = avg.efficiency in
    (* Compute career TS% and eFG% from season breakdown via weighted averages *)
    let career_ts_pct, career_efg_pct =
      let total_gp = List.fold_left (fun acc (s: season_stats) -> acc + s.ss_games_played) 0 seasons in
      if total_gp = 0 then (0.0, 0.0)
      else
        let gp_f = float_of_int total_gp in
        let wavg field = (List.fold_left (fun acc (s: season_stats) -> acc +. field s *. float_of_int s.ss_games_played) 0.0 seasons) /. gp_f in
        (wavg (fun s -> s.ss_ts_pct), wavg (fun s -> s.ss_efg_pct))
    in
    let per_color =
      if per >= 25.0 then "text-emerald-600 dark:text-emerald-400"
      else if per >= 20.0 then "text-sky-600 dark:text-sky-400"
      else if per >= 15.0 then "text-slate-700 dark:text-slate-300"
      else "text-rose-600 dark:text-rose-400"
    in
    let pct_color v =
      if v >= 0.60 then "text-emerald-600 dark:text-emerald-400"
      else if v >= 0.50 then "text-sky-600 dark:text-sky-400"
      else if v >= 0.40 then "text-slate-700 dark:text-slate-300"
      else if v > 0.0 then "text-rose-600 dark:text-rose-400"
      else "text-slate-400 dark:text-slate-600"
    in
    let stat_box label value unit description color =
      Printf.sprintf
        {html|<div class="bg-slate-50 dark:bg-slate-800/50 rounded-lg p-3 text-center">
          <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-wider font-bold">%s</div>
          <div class="text-2xl font-black %s font-mono tabular-nums">%s<span class="text-xs text-slate-400">%s</span></div>
          <div class="text-[9px] text-slate-400 dark:text-slate-400 mt-1">%s</div>
        </div>|html}
        (escape_html label) color value unit (escape_html description)
    in
    let fmt_f1 v = Printf.sprintf "%.1f" v in
    let fmt_pct v = if v > 0.0 then Printf.sprintf "%.1f" (v *. 100.0) else "-" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg">
        <div class="flex items-start justify-between gap-4 mb-4">
          <div class="min-w-0">
            <h2 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2">
              <span class="text-lg">📊</span> 고급 지표
            </h2>
            <div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">
              커리어 효율 지표 및 영향력 분석
            </div>
          </div>
        </div>
        <div class="grid grid-cols-2 md:grid-cols-4 gap-3">
          %s
          %s
          %s
          %s
        </div>
        <div class="grid grid-cols-2 md:grid-cols-4 gap-3 mt-3">
          %s
          %s
          %s
          %s
        </div>
        <div class="mt-4 pt-3 border-t border-slate-200 dark:border-slate-700">
          <div class="text-[10px] text-slate-500 dark:text-slate-400 leading-relaxed">
            <strong>PER</strong>: 효율 지수 (리그 평균 ≈ 15.0) |
            <strong>TS%%</strong>: 실질 슈팅 효율 |
            <strong>eFG%%</strong>: 유효 야투율 |
            <strong>EFF/G</strong>: 경기당 효율
          </div>
        </div>
      </div>|html}
      (stat_box "PER" (fmt_f1 per) "" "효율 레이팅" per_color)
      (stat_box "TS%" (fmt_pct career_ts_pct) "" "실질 슈팅" (pct_color career_ts_pct))
      (stat_box "eFG%" (fmt_pct career_efg_pct) "" "유효 야투" (pct_color career_efg_pct))
      (stat_box "EFF/G" (fmt_f1 eff_per_game) "" "경기당 효율" "text-orange-700 dark:text-orange-400")
      (stat_box "PPG" (fmt_f1 avg.avg_points) "" "경기당 득점" "text-slate-700 dark:text-slate-300")
      (stat_box "RPG" (fmt_f1 rpg) "" "경기당 리바운드" "text-slate-700 dark:text-slate-300")
      (stat_box "APG" (fmt_f1 apg) "" "경기당 어시스트" "text-slate-700 dark:text-slate-300")
      (stat_box "MIN/G" (fmt_f1 (avg.total_minutes /. float_of_int avg.games_played)) "" "경기당 출전" "text-slate-700 dark:text-slate-300")

let player_profile_page ?(lang=I18n.Ko) ?(leaderboards=None) ?(show_ops=false) (profile: player_profile) ~scope ~(seasons_catalog: season_info list) =
  let _ = leaderboards in (* suppress unused warning *)
  let _ = seasons_catalog in
  let p = profile.player in
  let pos = match p.position with Some s -> s | None -> "-" in
  let height_text =
    match p.height with
    | Some h when h > 0 && h <= 210 -> Printf.sprintf "%dcm" h  (* valid basketball height range *)
    | _ -> "-"
  in
  let birth_text =
    match p.birth_date with
    | Some d when String.trim d <> "" -> d
    | _ -> "-"
  in
  let avg = profile.averages in
  (* Use most recent team from team_stints instead of averages *)
  let current_team =
    match List.rev profile.team_stints with
    | latest :: _ -> String.trim latest.pts_team_name
    | [] -> String.trim avg.team_name  (* fallback to averages if no stints *)
  in
  let info_text =
    if current_team = "" then
      Printf.sprintf "%s | %s" pos height_text
    else
      let team_code = team_code_of_string current_team |> Option.value ~default:"" in
      Printf.sprintf
        {html|<a href="%s" class="inline-flex items-center gap-1.5 hover:text-white transition-colors">%s %s</a> <span class="text-white/40">·</span> %s | %s|html}
        (team_href current_team)
        (team_logo_tag ~class_name:"w-5 h-5 inline-block" team_code)
        (escape_html current_team)
        pos
        height_text
  in
  let team_color =
    team_code_of_string current_team
    |> Option.map team_code_to_color
    |> Option.value ~default:"#64748b"
  in
  let career_chips =
    if avg.games_played <= 0 then ""
    else
      Printf.sprintf
        {html|<span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">GP %d</span><span class="bg-orange-500/20 backdrop-blur-md text-orange-700 dark:text-orange-400 border border-orange-500/30 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm font-bold">PTS %.1f</span><span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">TOT %s</span><span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">REB %.1f</span><span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-full text-sm font-mono shadow-sm">AST %.1f</span>|html}
        avg.games_played
        avg.avg_points
        (format_int_commas avg.total_points)
        avg.avg_rebounds
        avg.avg_assists
  in
  let team_badge_html =
    if current_team = "" then "" else team_badge current_team
  in
  let video_links_html =
    let find_external_link link_type =
      let key = String.lowercase_ascii (String.trim link_type) in
      profile.external_links
      |> List.find_opt (fun (l: player_external_link) ->
          String.lowercase_ascii (String.trim l.pel_link_type) = key)
    in
    let link_button ~label ~url ?(title="") () =
      let title_attr = if title = "" then "" else Printf.sprintf {html| title="%s"|html} (escape_html title) in
      Printf.sprintf
        {html|<a href="%s" target="_blank" rel="noopener noreferrer" class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-3 py-1.5 rounded-lg text-sm font-mono hover:bg-white/40 dark:hover:bg-slate-700/60 transition-all shadow-sm"%s>%s</a>|html}
        (escape_html url)
        title_attr
        label
    in
	    let source_chip ~source_url =
	      Printf.sprintf
	        {html|<a href="%s" target="_blank" rel="noopener noreferrer" class="bg-white/10 dark:bg-slate-800/20 backdrop-blur-sm text-slate-600 dark:text-slate-400 px-2 py-1 rounded text-xs font-mono hover:text-slate-900 dark:hover:text-white transition" title="출처">출처</a>|html}
	        (escape_html source_url)
	    in
    let name = normalize_label p.name in
    let team = normalize_label current_team in  (* Use current team, not career average *)
    let base_query = if team = "" then name else Printf.sprintf "%s %s" name team in
    let wkbl_profile =
      "https://www.wkbl.or.kr/player/detail2.asp?pno=" ^ Uri.pct_encode p.id
    in
    let youtube_search =
      "https://www.youtube.com/results?search_query="
      ^ Uri.pct_encode (Printf.sprintf "%s WKBL 하이라이트" base_query)
    in
    let instagram_search =
      "https://www.google.com/search?q="
      ^ Uri.pct_encode (Printf.sprintf "site:instagram.com %s WKBL 인스타" base_query)
    in
    let youtube_html =
      match find_external_link "youtube" with
      | None ->
          link_button ~label:"▶ 검색" ~url:youtube_search ~title:"유튜브 검색" ()
      | Some l ->
          let title =
            match l.pel_source_url with
            | None -> "유튜브(확인된 링크)"
            | Some src -> "유튜브(출처: " ^ src ^ ")"
          in
          let src_html =
            match l.pel_source_url with
            | Some src when String.trim src <> "" && src <> l.pel_url -> source_chip ~source_url:src
            | _ -> ""
          in
          Printf.sprintf {html|%s%s|html} (link_button ~label:"▶ 영상" ~url:l.pel_url ~title ()) src_html
    in
    let instagram_html =
      match find_external_link "instagram" with
      | None ->
          link_button ~label:"인스타 검색" ~url:instagram_search ~title:"인스타그램 검색" ()
      | Some l ->
          let title =
            match l.pel_source_url with
            | None -> "인스타그램(확인된 링크)"
            | Some src -> "인스타그램(출처: " ^ src ^ ")"
          in
          let src_html =
            match l.pel_source_url with
            | Some src when String.trim src <> "" && src <> l.pel_url -> source_chip ~source_url:src
            | _ -> ""
          in
          Printf.sprintf {html|%s%s|html} (link_button ~label:"인스타" ~url:l.pel_url ~title ()) src_html
    in
      Printf.sprintf
      {html|%s%s%s|html}
      (link_button ~label:"WKBL" ~url:wkbl_profile ~title:"공식 프로필" ())
      youtube_html
      instagram_html
  in
  let game_rows games =
    games
    |> List.map (fun (g: player_game_stat) ->
        let res_color = if g.pts >= 20 then "text-orange-700 dark:text-orange-400" else "text-slate-700 dark:text-slate-300" in
        let pm_class, pm_str =
          match g.plus_minus, g.team_score, g.opponent_score with
          | Some v, _, _ ->
              let cls =
                if v > 0 then "text-sky-600 dark:text-sky-400"
                else if v < 0 then "text-rose-600 dark:text-rose-400"
                else "text-slate-500 dark:text-slate-400"
              in
              let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
              (cls, s)
          | None, Some team_score, Some opponent_score ->
              let margin = team_score - opponent_score in
              let cls =
                if margin > 0 then "text-sky-600 dark:text-sky-400"
                else if margin < 0 then "text-rose-600 dark:text-rose-400"
                else "text-slate-500 dark:text-slate-400"
              in
              let s =
                if margin > 0 then Printf.sprintf "M +%d" margin
                else Printf.sprintf "M %d" margin
              in
              (cls, s)
          | None, _, _ -> ("text-slate-500 dark:text-slate-400", "-")
        in
        let margin_badge =
          match g.team_score, g.opponent_score with
          | Some team_score, Some opponent_score ->
              let margin = team_score - opponent_score in
              let cls =
                if margin > 0 then "bg-sky-500/10 text-sky-600 dark:text-sky-400 border-sky-500/30"
                else if margin < 0 then "bg-rose-500/10 text-rose-600 dark:text-rose-400 border-rose-500/30"
                else "bg-slate-500/10 text-slate-700 dark:text-slate-300 border-slate-500/30"
              in
              let wl = if margin > 0 then "W" else if margin < 0 then "L" else "T" in
              Printf.sprintf
                {html|<span class="inline-flex items-center justify-center px-2 py-0.5 rounded border text-[10px] font-mono min-w-[52px] %s"><span class="font-bold">%s</span> %d-%d</span>|html}
                cls wl team_score opponent_score
          | _ ->
              {html|<span class="inline-flex items-center justify-center px-2 py-0.5 rounded border border-slate-200 dark:border-slate-700/60 text-[10px] font-mono text-slate-500 dark:text-slate-400 min-w-[52px]">-</span>|html}
        in
        let quality_badge = score_quality_badge ~lang ~compact:true g.score_quality in
        let ha_prefix = if g.is_home then "vs" else "@" in
        let opp_logo = team_logo_tag ~class_name:"w-4 h-4" g.opponent in
        let opponent_href = team_href g.opponent in
        let shooting_cell made att =
          if att = 0 then {html|<td class="px-3 py-2 text-right font-mono text-slate-400 dark:text-slate-600 w-[60px] sm:w-[72px] hidden md:table-cell">-</td>|html}
          else
            let pct = float_of_int made /. float_of_int att *. 100.0 in
            let pct_cls =
              if pct >= 60.0 then "text-emerald-600 dark:text-emerald-400"
              else if pct >= 45.0 then "text-slate-700 dark:text-slate-300"
              else "text-rose-500 dark:text-rose-400"
            in
            Printf.sprintf {html|<td class="px-3 py-2 text-right font-mono w-[60px] sm:w-[72px] hidden md:table-cell"><span class="text-slate-700 dark:text-slate-300">%d-%d</span> <span class="text-[10px] %s">%.0f</span></td>|html}
              made att pct_cls pct
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-[90px] sm:w-[110px]"><a href="%s" class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a></td><td class="px-3 py-2 text-slate-900 dark:text-white"><div class="flex items-center gap-x-3"><span class="text-[10px] text-slate-500 dark:text-slate-400 font-sans w-3">%s</span>%s<a href="%s" class="player-name min-w-0 flex-1 truncate hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a><div class="flex items-center gap-2 shrink-0">%s%s</div></div></td><td class="px-3 py-2 text-right font-mono text-slate-600 dark:text-slate-400 w-[60px] sm:w-[72px] whitespace-nowrap">%.1f</td><td class="px-3 py-2 text-right font-bold %s w-[60px] sm:w-[72px] whitespace-nowrap">%d</td>%s%s%s<td class="px-3 py-2 text-right font-mono w-[60px] sm:w-[72px] whitespace-nowrap %s">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] whitespace-nowrap">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td></tr>|html}
          (boxscore_href g.game_id)
          (escape_html g.game_date)
          ha_prefix
          opp_logo
          (escape_html opponent_href)
          (escape_html g.opponent)
          margin_badge
          quality_badge
          g.min
          res_color
          g.pts
          (shooting_cell g.fg_made g.fg_att)
          (shooting_cell g.fg3_made g.fg3_att)
          (shooting_cell g.ft_made g.ft_att)
          pm_class
          (escape_html pm_str)
          g.reb
          g.ast
          g.stl
          g.blk
          g.tov)
    |> String.concat "\n"
  in
  let birth_chip =
    Printf.sprintf
      {html|<span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-2.5 py-1 rounded-lg text-[11px] sm:text-sm shadow-sm"><span class="text-slate-600 dark:text-slate-400">생년</span> %s</span>|html}
      (escape_html birth_text)
  in
  (* Achievement badges derived from profile data + career award history *)
  let achievement_badges_html =
    (* --- Tier 1: Individual Awards from career_entries (gold/silver/bronze) --- *)
    let str_has needle hay =
      let n = String.length needle and h = String.length hay in
      if n = 0 then true
      else if n > h then false
      else
        let rec check i =
          if i > h - n then false
          else if String.sub hay i n = needle then true
          else check (i + 1)
        in check 0
    in
    let contains_any needles s =
      let s_lower = String.lowercase_ascii s in
      List.exists (fun n -> str_has n s_lower) needles
    in
    
    let (mvp_count, finals_mvp_count, scoring_leader_count, rebound_leader_count, assist_leader_count, rookie_award) =
      List.fold_left (fun (m, f, s, r, a, ro) (e: player_career_entry) ->
        match e.pce_awards with
        | None -> (m, f, s, r, a, ro)
        | Some awards ->
            let m' = if contains_any ["mvp"; "정규시즌mvp"; "정규시즌 mvp"] awards && not (contains_any ["파이널"; "final"; "챔프"] awards) then m + 1 else m in
            let f' = if contains_any ["파이널mvp"; "파이널스mvp"; "파이널 mvp"; "파이널스 mvp"; "finals mvp"; "챔프mvp"; "챔프 mvp"] awards then f + 1 else f in
            let s' = if contains_any ["득점왕"; "scoring leader"; "scoring"] awards then s + 1 else s in
            let r' = if contains_any ["리바운드왕"; "rebound leader"] awards then r + 1 else r in
            let a' = if contains_any ["어시스트왕"; "assist leader"] awards then a + 1 else a in
            let ro' = ro || contains_any ["신인왕"; "rookie"] awards in
            (m', f', s', r', a', ro')
      ) (0, 0, 0, 0, 0, false) profile.career_entries
    in

    let badges = [] in
    let badges = if mvp_count > 0 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-yellow-400/25 text-yellow-600 dark:text-yellow-300 border border-yellow-400/40 px-2.5 py-1 rounded-full text-xs font-bold shadow-sm">MVP x%d</span>|html} mvp_count) :: badges else badges in
    let badges = if finals_mvp_count > 0 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-yellow-500/20 text-yellow-700 dark:text-yellow-400 border border-yellow-500/30 px-2.5 py-1 rounded-full text-xs font-bold shadow-sm">Finals MVP x%d</span>|html} finals_mvp_count) :: badges else badges in
    let badges = if scoring_leader_count > 0 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-slate-300/30 text-slate-700 dark:text-slate-300 border border-slate-400/40 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">득점왕 x%d</span>|html} scoring_leader_count) :: badges else badges in
    let badges = if rebound_leader_count > 0 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-slate-300/30 text-slate-700 dark:text-slate-300 border border-slate-400/40 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">리바운드왕 x%d</span>|html} rebound_leader_count) :: badges else badges in
    let badges = if assist_leader_count > 0 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-slate-300/30 text-slate-700 dark:text-slate-300 border border-slate-400/40 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">어시스트왕 x%d</span>|html} assist_leader_count) :: badges else badges in
    let badges = if rookie_award then
      {html|<span class="inline-flex items-center gap-1 bg-orange-800/20 text-orange-800 dark:text-orange-400 border border-orange-700/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">신인왕</span>|html} :: badges else badges in

    (* --- Tier 2: All-Star & Draft --- *)
    let all_star_count = List.length profile.all_star_games in
    let badges = if all_star_count > 0 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-amber-500/20 text-amber-700 dark:text-amber-400 border border-amber-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">All-Star x%d</span>|html} all_star_count) :: badges else badges in
    let badges = match profile.draft with
     | Some d ->
       (match d.pd_draft_round, d.pd_overall_pick with
        | Some 1, Some pick ->
          (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-purple-500/20 text-purple-700 dark:text-purple-400 border border-purple-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">1R #%d Pick</span>|html} pick) :: badges
        | _ -> badges)
     | None -> badges
    in
    (* --- Tier 3: Career milestones --- *)
    let season_count = List.length profile.season_breakdown in
    let badges = if season_count >= 10 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-emerald-500/20 text-emerald-700 dark:text-emerald-400 border border-emerald-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">%d시즌</span>|html} season_count) :: badges else badges in
    let badges = if avg.total_points >= 3000 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-rose-500/20 text-rose-700 dark:text-rose-400 border border-rose-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">%s득점</span>|html} (format_int_commas avg.total_points)) :: badges
    else if avg.total_points >= 1000 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-orange-500/20 text-orange-700 dark:text-orange-400 border border-orange-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">%s득점</span>|html} (format_int_commas avg.total_points)) :: badges
    else badges in
    let badges = if avg.total_rebounds >= 2000 then
      (Printf.sprintf {html|<span class="inline-flex items-center gap-1 bg-sky-500/20 text-sky-700 dark:text-sky-400 border border-sky-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">%s리바운드</span>|html} (format_int_commas avg.total_rebounds)) :: badges else badges in
    let badges = if avg.avg_points >= 15.0 && avg.games_played >= 50 then
      {html|<span class="inline-flex items-center gap-1 bg-orange-600/20 text-orange-700 dark:text-orange-300 border border-orange-600/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">PPG 15+</span>|html} :: badges else badges in
    let badges = if avg.efficiency >= 20.0 && avg.games_played >= 50 then
      {html|<span class="inline-flex items-center gap-1 bg-yellow-500/20 text-yellow-700 dark:text-yellow-400 border border-yellow-500/30 px-2.5 py-1 rounded-full text-xs font-semibold shadow-sm">EFF 20+</span>|html} :: badges else badges in
    match badges with
    | [] -> ""
    | bs -> Printf.sprintf
        {html|<div class="flex flex-wrap gap-1.5 justify-center md:justify-start">%s</div>|html}
        (String.concat "" (List.rev bs))
  in
  let recent_rows = game_rows profile.recent_games in
  let recent_tfoot =
    let gs = profile.recent_games in
    let n = List.length gs in
    if n = 0 then ""
    else
      let nf = float_of_int n in
      let sum_f f = List.fold_left (fun acc (g: player_game_stat) -> acc +. f g) 0.0 gs in
      let sum_i f = List.fold_left (fun acc (g: player_game_stat) -> acc + f g) 0 gs in
      let avg_min = sum_f (fun g -> g.min) /. nf in
      let avg_pts = float_of_int (sum_i (fun g -> g.pts)) /. nf in
      let avg_reb = float_of_int (sum_i (fun g -> g.reb)) /. nf in
      let avg_ast = float_of_int (sum_i (fun g -> g.ast)) /. nf in
      let avg_stl = float_of_int (sum_i (fun g -> g.stl)) /. nf in
      let avg_blk = float_of_int (sum_i (fun g -> g.blk)) /. nf in
      let avg_tov = float_of_int (sum_i (fun g -> g.tov)) /. nf in
      let t_fg_m = sum_i (fun g -> g.fg_made) and t_fg_a = sum_i (fun g -> g.fg_att) in
      let t_3p_m = sum_i (fun g -> g.fg3_made) and t_3p_a = sum_i (fun g -> g.fg3_att) in
      let t_ft_m = sum_i (fun g -> g.ft_made) and t_ft_a = sum_i (fun g -> g.ft_att) in
      let pct_str m a = if a = 0 then "-" else Printf.sprintf "%.0f" (float_of_int m /. float_of_int a *. 100.0) in
      Printf.sprintf
        {html|<tfoot class="bg-slate-100/80 dark:bg-slate-800/60 border-t-2 border-slate-300 dark:border-slate-700 font-bold text-xs">
          <tr>
            <td class="px-3 py-2 text-slate-600 dark:text-slate-400">%d경기 평균</td>
            <td class="px-3 py-2"></td>
            <td class="px-3 py-2 text-right">%.1f</td>
            <td class="px-3 py-2 text-right text-orange-700 dark:text-orange-400">%.1f</td>
            <td class="px-3 py-2 text-right hidden md:table-cell"><span class="text-slate-700 dark:text-slate-300">%s</span></td>
            <td class="px-3 py-2 text-right hidden md:table-cell"><span class="text-slate-700 dark:text-slate-300">%s</span></td>
            <td class="px-3 py-2 text-right hidden md:table-cell"><span class="text-slate-700 dark:text-slate-300">%s</span></td>
            <td class="px-3 py-2"></td>
            <td class="px-3 py-2 text-right">%.1f</td>
            <td class="px-3 py-2 text-right hidden sm:table-cell">%.1f</td>
            <td class="px-3 py-2 text-right hidden sm:table-cell">%.1f</td>
            <td class="px-3 py-2 text-right hidden sm:table-cell">%.1f</td>
            <td class="px-3 py-2 text-right hidden sm:table-cell">%.1f</td>
          </tr>
        </tfoot>|html}
        n avg_min avg_pts
        (pct_str t_fg_m t_fg_a) (pct_str t_3p_m t_3p_a) (pct_str t_ft_m t_ft_a)
        avg_reb avg_ast avg_stl avg_blk avg_tov
  in
  let all_star_rows = game_rows profile.all_star_games in
  let season_stats_component = player_season_stats_component ~player_id:p.id ~scope profile.season_breakdown in
  let summary_comparison_html = player_summary_comparison profile.season_breakdown in
  let career_trajectory_html = career_trajectory_chart profile.season_breakdown in

  let player_radar_chart_html =
    if avg.games_played <= 0 then "" else
    let n_pts = min 100.0 (avg.avg_points /. 25.0 *. 100.0) in
    let n_reb = min 100.0 (avg.avg_rebounds /. 15.0 *. 100.0) in
    let n_ast = min 100.0 (avg.avg_assists /. 10.0 *. 100.0) in
    let n_stl = min 100.0 (avg.avg_steals /. 3.0 *. 100.0) in
    let n_blk = min 100.0 (avg.avg_blocks /. 2.0 *. 100.0) in
    let chart_data_json = Printf.sprintf
      {|{"labels":["PTS","REB","AST","STL","BLK"], "datasetLabel":"%s", "data":[%.1f, %.1f, %.1f, %.1f, %.1f], "maxScale": 100}|}
      (escape_js_string p.name) n_pts n_reb n_ast n_stl n_blk
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg mb-6">
        <h3 class="font-bold text-slate-900 dark:text-slate-200 text-sm mb-2">선수 밸런스 (최근 시즌 Top 수준 대비)</h3>
        <div class="relative w-full h-[250px] sm:h-[300px]">
          <canvas id="playerRadarChart" data-chart-data='%s'></canvas>
        </div>
      </div>|html}
      (escape_html chart_data_json)
  in

  let rec take n xs =
    match n, xs with
    | n, _ when n <= 0 -> []
    | _, [] -> []
    | n, x :: rest -> x :: take (n - 1) rest
  in
  let recent_wl_summary_html =
    let games = take 5 profile.recent_games in
    let n = List.length games in
    if n = 0 then
      ""
    else
      let chips_rev, wins, losses, unknown =
        games
        |> List.fold_left
          (fun (chips, w, l, u) (g: player_game_stat) ->
            match g.team_score, g.opponent_score with
            | Some team_score, Some opp_score ->
                let margin = team_score - opp_score in
                let label, cls, w', l' =
                  if margin > 0 then ("W", "bg-sky-500/10 border-sky-500/30 text-sky-600 dark:text-sky-300", w + 1, l)
                  else if margin < 0 then ("L", "bg-rose-500/10 border-rose-500/30 text-rose-600 dark:text-rose-300", w, l + 1)
                  else ("T", "bg-slate-100 dark:bg-slate-800/60 border-slate-200 dark:border-slate-700/60 text-slate-700 dark:text-slate-300", w, l)
                in
                let margin_str = if margin > 0 then Printf.sprintf "+%d" margin else string_of_int margin in
                let title = Printf.sprintf "%s %s (%s)" label margin_str g.game_date in
                let chip =
                  Printf.sprintf
                    {html|<span title="%s" class="inline-flex items-center justify-center w-6 h-6 rounded border %s text-[11px] font-mono shadow-sm">%s</span>|html}
                    (escape_html title)
                    cls
                    label
                in
                (chip :: chips, w', l', u)
            | _ ->
                let chip =
                  {html|<span title="점수 정보 없음" class="inline-flex items-center justify-center w-6 h-6 rounded border bg-slate-100 dark:bg-slate-800/40 border-slate-200 dark:border-slate-700/40 text-[11px] font-mono text-slate-400 dark:text-slate-400 shadow-sm">?</span>|html}
                in
                (chip :: chips, w, l, u + 1))
          ([], 0, 0, 0)
      in
      let record =
        let base =
          if wins + losses = 0 then "-"
          else Printf.sprintf "%d-%d" wins losses
        in
        if unknown > 0 then Printf.sprintf "%s (%d ?)" base unknown else base
      in
      let label = Printf.sprintf "최근 %d경기 %s" n record in
      let chips_html = chips_rev |> List.rev |> String.concat "" in
      Printf.sprintf
        {html|<div class="flex items-center gap-2"><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap">%s</span><div class="flex items-center gap-1">%s</div></div>|html}
        (escape_html label)
        chips_html
  in
  let recent_games_header_html =
    Printf.sprintf
      {html|<div class="flex items-start justify-between gap-3"><h2 class="text-xl font-bold text-slate-900 dark:text-white">최근 경기</h2><div class="flex flex-wrap items-center justify-end gap-2 shrink-0">%s<a href="%s/splits" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">스플릿</a><a href="%s/games" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">전체 경기</a><a href="%s/shots" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">샷 차트</a></div></div><p class="text-[11px] text-slate-500 dark:text-slate-400 mt-1">개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계가 있는 경기에서만 계산할 수 있어요. 문자중계가 없으면 이 항목이 비어 있을 수 있습니다.</p>|html}
      recent_wl_summary_html
      (player_href p.id)
      (player_href p.id)
      (player_href p.id)
  in
  let all_star_section_html =
    if profile.all_star_games = [] then
      ""
    else
      Printf.sprintf
        {html|<div class="space-y-4"><div class="flex flex-col gap-1"><h2 class="text-xl font-bold text-slate-900 dark:text-white">올스타전</h2><p class="text-[11px] text-slate-500 dark:text-slate-400">올스타전 기록은 시즌/커리어 테이블에서 제외하고, 여기에서만 보여줍니다.</p></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg scroll-shadow"><table class="min-w-[680px] sm:min-w-[920px] w-full text-sm font-mono table-auto" aria-label="올스타전 기록">
                <colgroup>
                  <col style="width: 110px;"> <!-- Date -->
                  <col style="width: 240px;"> <!-- Opponent -->
                  <col style="width: 72px;">  <!-- MIN -->
                  <col style="width: 72px;">  <!-- PTS -->
                  <col style="width: 72px;">  <!-- +/- -->
                  <col style="width: 72px;">  <!-- REB -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- AST -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- STL -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- BLK -->
                </colgroup>
                <thead class="bg-slate-100/80 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase tracking-wider text-xs whitespace-nowrap"><tr><th scope="col" class="px-3 py-2 text-left font-sans">날짜</th><th scope="col" class="px-3 py-2 text-left font-sans">상대</th><th scope="col" class="px-3 py-2 text-right" title="출전시간">MIN</th><th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" title="득점">PTS</th><th scope="col" class="px-3 py-2 text-right" title="+/-">+/-</th><th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="어시스트">AST</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="스틸">STL</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="블록">BLK</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
        all_star_rows
  in
  let team_movement_html =
    match profile.team_stints with
    | [] -> ""
    | stints ->
        let badge_max_width = "max-w-full" in
        let stints_asc = stints in
        let stints_desc = List.rev stints_asc in
        let current_team_html =
          let t = String.trim avg.team_name in
          if t = "" then {html|<span class="text-slate-600 dark:text-slate-400">-</span>|html}
          else team_badge ~max_width:badge_max_width t
        in
        let transfers = max 0 (List.length stints_asc - 1) in
        let last_move_value_html =
          match stints_desc with
          | current :: prev :: _ when prev.pts_team_name <> current.pts_team_name ->
              Printf.sprintf
                {html|<div class="space-y-1 min-w-0"><div class="flex flex-col gap-1 min-w-0"><div class="flex items-center gap-2 flex-wrap min-w-0">%s<span class="text-slate-600">→</span>%s</div></div><div class="text-[11px] text-slate-600 dark:text-slate-400 font-mono whitespace-nowrap">%s~</div></div>|html}
                (team_badge ~max_width:badge_max_width prev.pts_team_name)
                (team_badge ~max_width:badge_max_width current.pts_team_name)
                (escape_html current.pts_start_date)
          | _ -> {html|<span class="text-slate-600 dark:text-slate-400">-</span>|html}
        in
        let total_stints = List.length stints_asc in
	        let stint_rows =
	          stints_asc
	          |> List.mapi (fun idx (s: player_team_stint) ->
              let is_current = idx = total_stints - 1 in
              let range_html =
                if s.pts_start_date = s.pts_end_date then
                  Printf.sprintf {html|<span class="whitespace-nowrap">%s</span>|html} (escape_html s.pts_start_date)
                else
                  Printf.sprintf
                    {html|<span class="inline-flex flex-wrap items-baseline gap-x-1 gap-y-1"><span class="whitespace-nowrap">%s</span><span class="text-slate-600 whitespace-nowrap">~</span><span class="whitespace-nowrap">%s</span></span>|html}
                    (escape_html s.pts_start_date)
                    (escape_html s.pts_end_date)
              in
              let dot_class =
                if is_current then "bg-orange-500 border-orange-400"
                else "bg-slate-100 dark:bg-slate-800 border-slate-300 dark:border-slate-700"
              in
                let current_chip =
                  if is_current then
                  {html|<span class="px-2 py-0.5 rounded bg-orange-500/10 border border-orange-500/30 text-[10px] font-mono text-orange-700 whitespace-nowrap">현재</span>|html}
                else
                  ""
              in
              Printf.sprintf
                {html|<li class="relative pl-6"><span class="absolute -left-1.5 top-2 w-3 h-3 rounded-full border %s"></span><div class="flex items-start justify-between gap-3"><div class="min-w-0"><div class="flex items-center gap-2 flex-wrap min-w-0">%s%s</div><div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 font-mono">%s</div></div><div class="text-xs text-slate-600 dark:text-slate-400 font-mono shrink-0 whitespace-nowrap">GP %d</div></div></li>|html}
                dot_class
                (team_badge ~max_width:badge_max_width s.pts_team_name)
                current_chip
	                range_html
	                s.pts_games_played)
	          |> String.concat "\n"
	        in
	        let draft_value_html =
	          match profile.draft with
	          | None -> {html|<span class="text-slate-600 dark:text-slate-400">-</span>|html}
	          | Some (d: player_draft) ->
	              let team_html =
	                match d.pd_draft_team with
	                | None -> ""
	                | Some team_name ->
	                    Printf.sprintf
	                      {html|<div class="mt-2">%s</div>|html}
	                      (team_badge ~max_width:badge_max_width team_name)
	              in
	              Printf.sprintf
		                {html|<div class="text-slate-700 dark:text-slate-300 font-mono text-[11px] whitespace-pre-line break-words">%s</div>%s<div class="mt-2 text-[11px]"><a class="text-slate-600 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline" href="%s" target="_blank" rel="noreferrer">출처</a></div>|html}
		                (escape_html d.pd_raw_text)
		                team_html
		                (escape_html d.pd_source_url)
	        in
	        let trade_count = List.length profile.official_trade_events in
	        let trade_source =
	          match profile.official_trade_events with
	          | (e: official_trade_event) :: _ -> e.ote_source_url
	          | [] -> "https://www.wkbl.or.kr/player/trade_info.asp"
	        in
	        let trade_details_html =
	          match profile.official_trade_events with
	          | [] ->
	              {html|<div class="mt-3 text-xs text-slate-500 dark:text-slate-400 italic">공식 이적 기록 없음</div>|html}
	          | events ->
	              let items =
	                events
	                |> List.map (fun (e: official_trade_event) ->
                      let contract_chip_html =
                        match extract_contract_years e.ote_event_text with
                        | None -> ""
                        | Some years ->
                            Printf.sprintf
                              {html|<span title="계약 %d년" class="shrink-0 px-2 py-0.5 rounded bg-emerald-500/10 border border-emerald-500/30 text-[10px] font-mono text-emerald-700 dark:text-emerald-400 whitespace-nowrap">%dY</span>|html}
                              years
                              years
                      in
	                    Printf.sprintf
	                      {html|<li class="flex flex-wrap items-start gap-2"><span class="shrink-0 font-mono text-[11px] text-slate-600 dark:text-slate-400 whitespace-nowrap">%s</span>%s<span class="text-slate-700 dark:text-slate-300 text-xs leading-relaxed break-words">%s</span></li>|html}
	                      (escape_html e.ote_event_date)
                        contract_chip_html
	                      (escape_html e.ote_event_text))
	                |> String.concat "\n"
	              in
	              Printf.sprintf
	                {html|<details class="mt-3"><summary class="cursor-pointer select-none text-[11px] text-slate-600 dark:text-slate-400 font-mono">Events (%d)</summary><ol class="mt-3 space-y-2">%s</ol></details>|html}
	                trade_count
	                items
	        in
	        let official_html =
	          Printf.sprintf
		            {html|<div class="mt-4 pt-4 border-t border-slate-200 dark:border-slate-800/60"><div class="flex items-start justify-between gap-4"><div class="min-w-0"><div class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🧩</span> 공식 드래프트 / 이적</div><div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed break-words">WKBL 공식 페이지 원문 기준입니다. 이적은 <span class="font-mono text-slate-700 dark:text-slate-300">이름 포함</span>으로만 매칭합니다.</div></div><a class="text-[11px] text-slate-600 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono shrink-0 whitespace-nowrap" href="%s" target="_blank" rel="noreferrer">출처</a></div><div class="mt-4 grid grid-cols-1 lg:grid-cols-2 gap-3 text-xs"><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">드래프트</div><div class="mt-2 min-w-0">%s</div></div><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 min-w-0"><div class="flex items-center justify-between gap-3"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">이적</div><span class="text-[11px] font-mono text-slate-600 dark:text-slate-400 whitespace-nowrap">n=%d</span></div>%s</div></div></div>|html}
	            (escape_html trade_source)
	            draft_value_html
	            trade_count
	            trade_details_html
	        in
	        Printf.sprintf
		          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><div class="min-w-0"><h2 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🔁</span> 팀 이동</h2><div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed break-words">박스스코어 출전팀 변화로 추정한 연보입니다. (기간=첫/마지막 출전일)</div></div><span class="text-[11px] text-slate-600 dark:text-slate-400 font-mono shrink-0">박스스코어</span></div><div class="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-1 xl:grid-cols-3 gap-3 text-xs"><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">현재</div><div class="mt-2 text-slate-700 dark:text-slate-300 min-w-0">%s</div></div><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">이적</div><div class="mt-2 font-mono text-slate-900 dark:text-slate-200 text-lg font-black">%d</div></div><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">최근</div><div class="mt-2 text-slate-700 dark:text-slate-300 min-w-0">%s</div></div></div><div class="mt-4 rounded-lg border border-slate-200 dark:border-slate-800/60 bg-slate-950/30 p-4"><ol class="relative border-l border-slate-200 dark:border-slate-800/60 ml-2 space-y-4">%s</ol></div>%s</div>|html}
	          current_team_html transfers last_move_value_html stint_rows official_html
  in

  let leaderboards_html =
    match leaderboards with
    | None -> ""
    | Some (lb_season_code, lb_season_name, lb_leaders) ->
        let scope_value = scope |> String.trim |> String.lowercase_ascii in
        let scope_label =
          match scope_value with
          | "totals" -> "누적"
          | "per_36" -> "36분 환산"
          | _ -> "경기당"
        in
        let leaders_href =
          Printf.sprintf
            "/leaders?season=%s&scope=%s"
            (Uri.pct_encode lb_season_code)
            (Uri.pct_encode scope_value)
        in
        let lookup category =
          lb_leaders
          |> List.find_opt (fun (k, _) -> k = category)
          |> Option.map snd
          |> Option.value ~default:[]
        in
        let fmt_int v = Printf.sprintf "%.0f" v in
        let fmt_f1 v = Printf.sprintf "%.1f" v in
        let fmt_f3 v = Printf.sprintf "%.3f" v in
        let categories =
          match scope_value with
          | "totals" ->
              [ ("GP", "gp", fmt_int)
              ; ("MIN", "min", fmt_f1)
              ; ("PTS", "pts", fmt_int)
              ; ("REB", "reb", fmt_int)
              ; ("AST", "ast", fmt_int)
              ; ("STL", "stl", fmt_int)
              ; ("BLK", "blk", fmt_int)
              ; ("TO", "tov", fmt_int)
              ; ("FG%", "fg_pct", fmt_f3)
              ; ("3P%", "fg3_pct", fmt_f3)
              ; ("FT%", "ft_pct", fmt_f3)
              ; ("TS%", "ts_pct", fmt_f3)
              ; ("eFG%", "efg_pct", fmt_f3)
              ]
          | "per_36" ->
              [ ("PTS/36", "pts", fmt_f1)
              ; ("REB/36", "reb", fmt_f1)
              ; ("AST/36", "ast", fmt_f1)
              ; ("STL/36", "stl", fmt_f1)
              ; ("BLK/36", "blk", fmt_f1)
              ; ("TO/36", "tov", fmt_f1)
              ; ("EFF/36", "eff", fmt_f1)
              ; ("FG%", "fg_pct", fmt_f3)
              ; ("3P%", "fg3_pct", fmt_f3)
              ; ("FT%", "ft_pct", fmt_f3)
              ; ("TS%", "ts_pct", fmt_f3)
              ; ("eFG%", "efg_pct", fmt_f3)
              ]
          | _ ->
              [ ("PTS", "pts", fmt_f1)
              ; ("REB", "reb", fmt_f1)
              ; ("AST", "ast", fmt_f1)
              ; ("STL", "stl", fmt_f1)
              ; ("BLK", "blk", fmt_f1)
              ; ("TO", "tov", fmt_f1)
              ; ("MIN", "min", fmt_f1)
              ; ("EFF", "eff", fmt_f1)
              ; ("FG%", "fg_pct", fmt_f3)
              ; ("3P%", "fg3_pct", fmt_f3)
              ; ("FT%", "ft_pct", fmt_f3)
              ; ("TS%", "ts_pct", fmt_f3)
              ; ("eFG%", "efg_pct", fmt_f3)
              ]
        in
        let find_rank category =
          lookup category
          |> List.mapi (fun idx (l: leader_entry) -> (idx + 1, l))
          |> List.find_opt (fun (_, (l: leader_entry)) -> l.le_player_id = p.id)
        in
        let rows =
          categories
          |> List.filter_map (fun (label, category, fmt) ->
              match find_rank category with
              | None -> None
              | Some (rank, l) ->
                  Some
                    (Printf.sprintf
                       {html|<div class="flex items-center justify-between gap-3"><div class="min-w-0 flex items-center gap-2"><span class="text-slate-600 dark:text-slate-400 font-mono text-xs w-10">#%d</span><span class="text-slate-700 dark:text-slate-300 font-mono whitespace-nowrap">%s</span></div><div class="text-slate-900 dark:text-slate-200 font-mono font-bold">%s</div></div>|html}
                       rank
                       (escape_html label)
                       (escape_html (fmt l.le_stat_value))))
        in
        let rows_html =
          match rows with
          | [] ->
              Printf.sprintf
                {html|<div class="text-xs text-slate-600 dark:text-slate-400 leading-relaxed">해당 시즌 <span class="font-mono text-slate-700 dark:text-slate-300">%s</span> 기준 Top 5 리더보드 진입 기록이 없습니다.</div>|html}
                (escape_html scope_label)
          | _ ->
              Printf.sprintf {html|<div class="space-y-2">%s</div>|html} (String.concat "\n" rows)
        in
	        Printf.sprintf
		          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><div class="min-w-0"><h2 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🏅</span> 리더보드</h2><div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed"><span class="font-mono text-slate-700 dark:text-slate-300">%s</span> · <span class="font-mono text-slate-700 dark:text-slate-300">%s</span> · 상위 5</div></div><a href="%s" class="text-[11px] text-slate-600 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono shrink-0 whitespace-nowrap">리더</a></div>%s</div>|html}
	          (Printf.sprintf {|<a href="%s" class="hover:underline">%s</a>|} (season_href lb_season_code) (escape_html lb_season_name))
	          (escape_html scope_label)
	          (escape_html leaders_href)
	          rows_html
	  in
	  let data_notes_html =
	    let season_range_html ~count ~oldest ~latest =
	      if count <= 0 then
	        {html|<span class="text-slate-600 dark:text-slate-400">-</span>|html}
      else if count = 1 then
        Printf.sprintf {html|<span class="text-slate-700 dark:text-slate-300 whitespace-nowrap">%s</span>|html} (escape_html oldest)
      else
        Printf.sprintf
          {html|<span class="inline-flex flex-wrap items-baseline gap-x-1 gap-y-1"><span class="text-slate-700 dark:text-slate-300 whitespace-nowrap">%s</span><span class="whitespace-nowrap"><span class="text-slate-600">→</span><span class="ml-1 text-slate-700 dark:text-slate-300 whitespace-nowrap">%s</span></span></span>|html}
          (escape_html oldest)
          (escape_html latest)
    in

    let db_seasons_value_html, show_backfill, db_season_count =
      match seasons_catalog with
      | [] -> ({html|<span class="font-mono text-slate-600 dark:text-slate-400">-</span>|html}, false, 0)
      | oldest :: _ ->
          let count = List.length seasons_catalog in
          let latest =
            match List.rev seasons_catalog with
            | latest :: _ -> latest
            | [] -> oldest
          in
          (season_range_html ~count ~oldest:oldest.name ~latest:latest.name, count <= 3, count)
    in

    let player_seasons_value_html, player_season_count =
      match profile.season_breakdown with
      | [] -> ({html|<span class="font-mono text-slate-600 dark:text-slate-400">-</span>|html}, 0)
      | latest :: _ ->
          let count = List.length profile.season_breakdown in
          let rec last = function
            | [] -> None
            | [x] -> Some x
            | _ :: xs -> last xs
          in
          let oldest = last profile.season_breakdown |> Option.value ~default:latest in
          (season_range_html ~count ~oldest:oldest.ss_season_name ~latest:latest.ss_season_name, count)
    in
		    let backfill_row_html =
		      if show_backfill then
		        if show_ops then
		          Printf.sprintf
		            {html|<div class="text-slate-600 dark:text-slate-400 leading-relaxed">현재는 약 %d시즌만 수집되어 있습니다. 추가 수집이 필요합니다.</div>|html}
		            db_season_count
		        else
		          Printf.sprintf
		            {html|<div class="text-slate-600 dark:text-slate-400 leading-relaxed">현재는 일부 시즌 데이터만 제공됩니다. 순차적으로 추가됩니다. (약 %d시즌)</div>|html}
	            db_season_count
	      else
	        ""
    in
	    let backfill_details_html =
	      if backfill_row_html = "" then
	        ""
	      else
	        let summary = if show_ops then "운영자용: 데이터 수집" else "데이터 안내" in
	        Printf.sprintf
	          {html|<details class="mt-3 text-[11px] text-slate-600 dark:text-slate-400"><summary class="cursor-pointer select-none text-slate-600 dark:text-slate-400 font-bold">%s</summary><div class="mt-2">%s</div></details>|html}
	          summary
	          backfill_row_html
	    in
	    let season_title = "시즌" in
	    let season_label_data = "데이터" in
	    let season_label_player = "선수" in
	    let season_count_chips_html =
	      Printf.sprintf
	        {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-slate-700 dark:text-slate-300 whitespace-nowrap">수집된 시즌 %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-slate-700 dark:text-slate-300 whitespace-nowrap">출전 시즌 %d</span>|html}
	        db_season_count
	        player_season_count
	    in
    let seasons_card_html =
      Printf.sprintf
        {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-4 sm:p-5 min-w-0"><div class="flex flex-wrap items-start justify-between gap-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🗓</span> %s</div><div class="flex flex-wrap items-center gap-2 text-[10px] font-mono min-w-0">%s</div></div><div class="mt-3 space-y-2 text-slate-900 dark:text-slate-200"><div><div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">%s</div><div class="mt-1 font-mono">%s</div></div><div><div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">%s</div><div class="mt-1 font-mono">%s</div></div></div>%s</div>|html}
        season_title
        season_count_chips_html
        (escape_html season_label_data)
        db_seasons_value_html
        (escape_html season_label_player)
        player_seasons_value_html
        backfill_details_html
    in
    let total_recent = List.length profile.recent_games in
    let count_pm, count_m, count_issue =
      profile.recent_games
      |> List.fold_left
        (fun (pm_cnt, m_cnt, issue_cnt) (g: player_game_stat) ->
          let issue_cnt' = if g.score_quality = Mismatch then issue_cnt + 1 else issue_cnt in
          match g.plus_minus, g.team_score, g.opponent_score with
          | Some _, _, _ -> (pm_cnt + 1, m_cnt, issue_cnt')
          | None, Some _, Some _ -> (pm_cnt, m_cnt + 1, issue_cnt')
          | None, _, _ -> (pm_cnt, m_cnt, issue_cnt'))
        (0, 0, 0)
    in
    let count_dash = max 0 (total_recent - count_pm - count_m) in
    let pbp_stat_line =
      if total_recent <= 0 then
        {html|<div class="mt-2 text-[11px] text-slate-600 dark:text-slate-400 font-mono">최근 경기 데이터 없음</div>|html}
      else
        Printf.sprintf
	          {html|<div class="mt-2 text-[11px] text-slate-600 dark:text-slate-400 font-mono flex flex-wrap items-center gap-x-2 gap-y-1"><span class="whitespace-nowrap">최근 %d경기:</span><span class="whitespace-nowrap">+/- %d</span><span class="whitespace-nowrap">· M %d</span><span class="whitespace-nowrap">· - %d</span>%s</div>|html}
	          total_recent
	          count_pm
	          count_m
	          count_dash
	          (if count_issue > 0 then Printf.sprintf {html|<span class="whitespace-nowrap">· 불일치 %d</span>|html} count_issue else "")
	    in
		    let pbp_card_html =
		      Printf.sprintf
			        {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-4 sm:p-5 min-w-0"><div class="flex flex-wrap items-start justify-between gap-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🎥</span> 문자중계 +/-</div><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">부분</span></div>%s<div class="mt-3 text-slate-600 dark:text-slate-400 text-xs leading-relaxed space-y-1"><div><span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>: 문자중계 기반</div><div><span class="font-mono text-slate-700 dark:text-slate-300">M</span>: 문자중계가 없을 때 팀 득실마진(경기 최종 점수)</div><div><span class="font-mono text-slate-700 dark:text-slate-300">-</span>: 데이터 없음/품질 문제</div></div></div>|html}
		        pbp_stat_line
		    in
			    let draft_card_html =
			      let ops_sync_html =
			        if show_ops then
			          {html|<details class="text-[11px] text-slate-600 dark:text-slate-400"><summary class="cursor-pointer select-none text-slate-600 dark:text-slate-400 font-bold">수집</summary><div class="mt-2"><div class="leading-relaxed">드래프트/이적 데이터는 별도의 수집 과정이 필요합니다.</div></div></details>|html}
			        else
			          ""
			      in
		      let draft_chip =
		        match profile.draft with
			        | Some _ ->
			            {html|<span class="px-2 py-0.5 rounded bg-emerald-500/10 border border-emerald-500/30 text-[10px] font-mono text-emerald-700 dark:text-emerald-400 whitespace-nowrap">드래프트 ✓</span>|html}
		        | None ->
		            {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">드래프트 -</span>|html}
		      in
	      let trade_count = List.length profile.official_trade_events in
		      let trade_chip =
		        Printf.sprintf
		          {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">이적 %d건</span>|html}
		          trade_count
		      in
	      let draft_value_html =
	        match profile.draft with
	        | None -> {html|<span class="text-slate-600 dark:text-slate-400">-</span>|html}
		        | Some (d: player_draft) ->
		            Printf.sprintf
		              {html|<div class="text-slate-900 dark:text-slate-200 font-mono text-[11px] whitespace-pre-line break-words">%s</div><a class="mt-2 inline-block text-[11px] text-slate-600 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">출처</a>|html}
		              (escape_html d.pd_raw_text)
		              (escape_html d.pd_source_url)
	      in
	      let trade_value_html =
		        if trade_count <= 0 then
		          {html|<span class="text-slate-600 dark:text-slate-400">-</span>|html}
		        else
		          {html|<span class="text-slate-900 dark:text-slate-200 font-mono text-[11px]">매칭된 이벤트가 있습니다.</span>|html}
			      in
			      Printf.sprintf
			        {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-4 sm:p-5 min-w-0"><div class="flex flex-wrap items-start justify-between gap-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🧩</span> 드래프트 / 이적</div><div class="flex flex-wrap items-center gap-2">%s%s<a class="px-2 py-0.5 rounded bg-white dark:bg-slate-900/40 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:bg-slate-100 dark:bg-slate-800/60 transition whitespace-nowrap" href="/transactions">목록</a></div></div><div class="mt-3 text-slate-600 dark:text-slate-400 text-xs leading-relaxed space-y-3"><div><div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">드래프트</div><div class="mt-1">%s</div></div><div><div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">이적</div><div class="mt-1">%s</div><div class="mt-2 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">공식 이적현황 원문에서 <span class="font-mono text-slate-700 dark:text-slate-300">이름 포함</span>으로만 매칭합니다. (동명이인/표기 차이로 오매칭/누락 가능)</div></div>%s</div></div>|html}
			        draft_chip
			        trade_chip
			        draft_value_html
		        trade_value_html
		        ops_sync_html
		    in
		    Printf.sprintf
		      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><h2 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🧾</span> 데이터 안내</h2><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">현황</span></div><div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-1 xl:grid-cols-2 gap-3 md:gap-4">%s%s<div class="md:col-span-2 lg:col-span-1 xl:col-span-2">%s</div></div></div>|html}
		      seasons_card_html
		      pbp_card_html
		      draft_card_html
  in
		  let missing_data_html =
		    if profile.season_breakdown = [] && profile.recent_games = [] && profile.all_star_games = [] && profile.draft = None && profile.official_trade_events = [] then
		      {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/50 p-4 sm:p-6 flex items-center justify-center text-slate-600 dark:text-slate-400 text-sm"><div class="text-center"><div class="font-bold">기록이 아직 없습니다</div><div>공식 기록이 올라오면 자동으로 업데이트됩니다.</div></div></div>|html}
		    else
		      ""
		  in
	  (* Trend analysis panel - requires at least 2 recent games *)
	  let trends_panel_html =
	    if List.length profile.recent_games >= 2 then
	      Views_charts.player_radar_chart ~player:avg ~league_avg:None ^ Views_charts.player_trends_panel profile.recent_games
	    else
	      ""
	  in

  let display_name = normalize_name p.name in
  let og_card_url = Printf.sprintf "https://wkbl.win/card/player/png/%s" p.id in
  let canonical = player_href p.id in
  let seo_desc = Printf.sprintf "%s (%s) - PPG %.1f, RPG %.1f, APG %.1f, EFF %.1f | WKBL 여자농구 선수 통계"
    display_name current_team avg.avg_points avg.avg_rebounds avg.avg_assists avg.efficiency in
  (* JSON-LD structured data for Person schema *)
  let json_ld_data =
    let height_val = match p.height with Some h when h > 0 -> Printf.sprintf {|,"height":{"@type":"QuantitativeValue","value":%d,"unitCode":"CMT"}|} h | _ -> "" in
    let birth_val = match p.birth_date with Some d when String.trim d <> "" -> Printf.sprintf {|,"birthDate":"%s"|} (escape_html d) | _ -> "" in
    let team_val = if current_team <> "" then Printf.sprintf {|,"memberOf":{"@type":"SportsTeam","name":"%s"}|} (escape_html current_team) else "" in
    Printf.sprintf {|{"@context":"https://schema.org","@type":"Person","name":"%s","url":"https://wkbl.win%s","image":"%s","jobTitle":"Professional Basketball Player"%s%s%s}|}
      (escape_html display_name) canonical og_card_url height_val birth_val team_val
  in
  layout ~lang ~title:(display_name ^ " | WKBL 선수")
    ~canonical_path:canonical
    ~description:seo_desc
    ~og_image:og_card_url
    ~json_ld:json_ld_data
    ~scripts:With_player_features_and_charts
    ~content:(Printf.sprintf {html|<div class="space-y-6 sm:space-y-8 animate-fade-in">
      %s
      <!-- 히어로 섹션 개편 -->
      <div class="relative overflow-hidden rounded-3xl shadow-2xl border border-white/20 dark:border-slate-800/50">
        <!-- 팀 컬러 그라데이션 배경 -->
        <div class="absolute inset-0 opacity-80 dark:opacity-60" style="background: linear-gradient(135deg, %s 0%%, #000 150%%)"></div>
        <div class="absolute inset-0 bg-grid-white/[0.05] bg-[center_top_-1px]"></div>
        
        <div class="relative p-6 sm:p-10 flex flex-col md:flex-row items-center md:items-end gap-8">
          <div class="relative group">
            <div class="absolute -inset-1 bg-white/30 rounded-full blur opacity-25 group-hover:opacity-50 transition duration-1000 group-hover:duration-200"></div>
            %s
            <div class="absolute -bottom-2 -right-2 bg-white dark:bg-slate-800 border-4 border-slate-50 dark:border-[#0b0e14] text-slate-900 dark:text-white text-[11px] sm:text-xs font-black px-3 py-1 rounded-full shadow-lg transform rotate-3">#%s</div>
          </div>
          
          <div class="text-center md:text-left space-y-4 flex-1">
            <div class="space-y-1">
              <div class="flex items-center gap-3 justify-center md:justify-start flex-wrap">
                <h1 class="text-4xl sm:text-6xl font-black text-white tracking-tighter drop-shadow-lg">%s</h1>
                %s
              </div>
              <div class="text-white/80 text-lg sm:text-xl font-medium tracking-wide flex items-center justify-center md:justify-start gap-3">
                %s <span class="w-1.5 h-1.5 rounded-full bg-white/40"></span> %s
              </div>
            </div>
            
            <div class="flex flex-wrap gap-2.5 justify-center md:justify-start pt-2">
              %s
              %s
              %s
            </div>
            
            <div class="flex flex-wrap gap-3 justify-center md:justify-start pt-4 border-t border-white/10">
              %s
            </div>
          </div>
        </div>
      </div>

      <div class="grid grid-cols-1 lg:grid-cols-6 gap-6 sm:gap-8">
        <div class="lg:col-span-4 space-y-6 sm:space-y-8">
          %s
          %s
          %s
          %s
          <div class="space-y-4">
            %s
            <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg scroll-shadow">
              <table class="min-w-[680px] md:min-w-[1100px] w-full text-xs sm:text-sm font-mono table-auto tabular-nums" aria-label="최근 경기 기록">
                <colgroup>
                  <col style="width: 110px;"> <!-- Date -->
                  <col style="width: 240px;"> <!-- Opponent -->
                  <col style="width: 72px;">  <!-- MIN -->
                  <col style="width: 72px;">  <!-- PTS -->
                  <col class="hidden md:table-column" style="width: 72px;"> <!-- FG -->
                  <col class="hidden md:table-column" style="width: 72px;"> <!-- 3P -->
                  <col class="hidden md:table-column" style="width: 72px;"> <!-- FT -->
                  <col style="width: 72px;">  <!-- +/- -->
                  <col style="width: 72px;">  <!-- REB -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- AST -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- STL -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- BLK -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- TOV -->
                </colgroup>
                <thead class="bg-slate-100/80 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap">
                  <tr>
                    <th scope="col" class="px-3 py-2 text-left font-sans">날짜</th>
                    <th scope="col" class="px-3 py-2 text-left font-sans">상대</th>
                    <th scope="col" class="px-3 py-2 text-right" title="출전시간">MIN</th>
                    <th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" title="득점">PTS</th>
                    <th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="필드골 (성공-시도)">FG</th>
                    <th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="3점슛 (성공-시도)">3P</th>
                    <th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="자유투 (성공-시도)">FT</th>
                    <th scope="col" class="px-3 py-2 text-right" title="+/-">+/-</th>
                    <th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="어시스트">AST</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="스틸">STL</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="블록">BLK</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="턴오버">TOV</th>
                  </tr>
                </thead>
                <tbody>%s</tbody>
                %s
              </table>
            </div>
          </div>
          %s
        </div>
        <div class="space-y-6 sm:space-y-8 lg:col-span-2">
          <div class="space-y-4">
            %s
            %s
            %s
            %s
            %s
            %s
            %s
            %s
          </div>
        </div>
      </div>
      %s
    </div>|html}
          (breadcrumb [("홈", "/"); ("선수", "/players"); (display_name, "")])
          team_color
          (player_img_tag ~class_name:"w-32 h-32 sm:w-48 sm:h-48 border-[6px] border-white/20 dark:border-slate-700/30 shadow-2xl transition-transform duration-500 group-hover:scale-105" p.id p.name)
          (escape_html p.id)
          (escape_html display_name)
          (eff_badge ~show_label:true avg.efficiency)
          info_text
          birth_chip
          team_badge_html
          video_links_html
          career_chips
          achievement_badges_html
          summary_comparison_html
          season_stats_component
          career_trajectory_html
          player_radar_chart_html
          recent_games_header_html
          recent_rows
          recent_tfoot
          all_star_section_html
          team_movement_html
          leaderboards_html
          (Printf.sprintf {html|<div hx-get="%s/shot-chart" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4"><span class="htmx-indicator inline-flex items-center gap-2 text-slate-400"><span class="w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin" aria-hidden="true"></span><span>슛 차트 로딩 중...</span></span></div></div>|html} (player_href p.id))
          trends_panel_html
          (advanced_stats_card profile.averages profile.season_breakdown)
          (career_highs_card profile.career_highs)
          (milestone_tracker_card profile.averages)
          missing_data_html
          data_notes_html) ()
let player_game_logs_page ?(lang=I18n.Ko) (profile: player_profile) ~(season: string) ~(seasons: season_info list) ~(include_mismatch: bool) (games: player_game_stat list) =
  let p = profile.player in
  let display_name = normalize_name p.name in
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
  let verified_cnt, derived_cnt, mismatch_cnt =
    games
    |> List.fold_left
      (fun (v, d, m) (g: player_game_stat) ->
        match g.score_quality with
        | Verified -> (v + 1, d, m)
        | Derived -> (v, d + 1, m)
        | Mismatch -> (v, d, m + 1))
      (0, 0, 0)
  in
  let quality_chips =
    Printf.sprintf
      {html|<div class="flex flex-wrap items-center gap-2 text-[11px] text-slate-600 dark:text-slate-400"><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">일치 %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">추정 %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">불일치 %d</span></div>|html}
      verified_cnt derived_cnt mismatch_cnt
  in
  (* Accumulate totals for tfoot *)
  let n_games = List.length games in
  let tot_min, tot_fg_m, tot_fg_a, tot_fg3_m, tot_fg3_a, tot_ft_m, tot_ft_a,
      tot_pts, tot_reb, tot_ast, tot_stl, tot_blk, tot_tov =
    games |> List.fold_left (fun (mn, fgm, fga, f3m, f3a, ftm, fta, p, r, a, s, b, t) (g: player_game_stat) ->
      (mn +. g.min, fgm + g.fg_made, fga + g.fg_att, f3m + g.fg3_made, f3a + g.fg3_att,
       ftm + g.ft_made, fta + g.ft_att, p + g.pts, r + g.reb, a + g.ast, s + g.stl, b + g.blk, t + g.tov))
      (0.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  in
  let tfoot_html =
    if n_games = 0 then ""
    else
      let n = float_of_int n_games in
      let pct a b = if b = 0 then "-" else Printf.sprintf ".%03d" (a * 1000 / b) in
      Printf.sprintf
        {html|<tfoot class="bg-slate-50 dark:bg-slate-800/40 font-bold text-sm border-t-2 border-slate-300 dark:border-slate-700">
          <tr><td class="px-3 py-2 text-slate-700 dark:text-slate-300" colspan="2">합계 (%d경기)</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden sm:table-cell">%.0f</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden md:table-cell">%d-%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden md:table-cell">%d-%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden md:table-cell">%d-%d</td><td class="px-3 py-2 text-right text-orange-700 dark:text-orange-400">%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400">-</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400">%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden sm:table-cell">%d</td></tr>
          <tr class="text-xs text-slate-500 dark:text-slate-400 font-normal"><td class="px-3 py-1.5" colspan="2">평균 (%%)</td><td class="px-3 py-1.5 text-right hidden sm:table-cell">%.1f</td><td class="px-3 py-1.5 text-right hidden md:table-cell">%s</td><td class="px-3 py-1.5 text-right hidden md:table-cell">%s</td><td class="px-3 py-1.5 text-right hidden md:table-cell">%s</td><td class="px-3 py-1.5 text-right">%.1f</td><td class="px-3 py-1.5 text-right">-</td><td class="px-3 py-1.5 text-right">%.1f</td><td class="px-3 py-1.5 text-right hidden sm:table-cell">%.1f</td><td class="px-3 py-1.5 text-right hidden sm:table-cell">%.1f</td><td class="px-3 py-1.5 text-right hidden sm:table-cell">%.1f</td><td class="px-3 py-1.5 text-right hidden sm:table-cell">%.1f</td></tr>
        </tfoot>|html}
        n_games tot_min
        tot_fg_m tot_fg_a tot_fg3_m tot_fg3_a tot_ft_m tot_ft_a
        tot_pts tot_reb tot_ast tot_stl tot_blk tot_tov
        (tot_min /. n)
        (pct tot_fg_m tot_fg_a) (pct tot_fg3_m tot_fg3_a) (pct tot_ft_m tot_ft_a)
        (float_of_int tot_pts /. n) (float_of_int tot_reb /. n) (float_of_int tot_ast /. n)
        (float_of_int tot_stl /. n) (float_of_int tot_blk /. n) (float_of_int tot_tov /. n)
  in
  let rows =
    if games = [] then
      {html|<tr><td colspan="6" class="sm:hidden px-4 py-10 text-center text-slate-600 dark:text-slate-400 text-sm">경기 기록이 없습니다.</td><td colspan="13" class="hidden sm:table-cell px-4 py-10 text-center text-slate-600 dark:text-slate-400 text-sm">경기 기록이 없습니다.</td></tr>|html}
    else
      games
      |> List.map (fun (g: player_game_stat) ->
          let res_color = if g.pts >= 20 then "text-orange-700 dark:text-orange-400" else "text-slate-700 dark:text-slate-300" in
          let pm_class, pm_str =
            match g.plus_minus, g.team_score, g.opponent_score with
            | Some v, _, _ ->
                let cls =
                  if v > 0 then "text-sky-600 dark:text-sky-400"
                  else if v < 0 then "text-rose-600 dark:text-rose-400"
                  else "text-slate-600 dark:text-slate-400"
                in
                let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
                (cls, s)
            | None, Some team_score, Some opponent_score ->
                let margin = team_score - opponent_score in
                let cls =
                  if margin > 0 then "text-sky-600 dark:text-sky-400"
                  else if margin < 0 then "text-rose-600 dark:text-rose-400"
                  else "text-slate-600 dark:text-slate-400"
                in
                let s =
                  if margin > 0 then Printf.sprintf "M +%d" margin
                  else Printf.sprintf "M %d" margin
                in
                (cls, s)
            | None, _, _ -> ("text-slate-600 dark:text-slate-400", "-")
          in
          let margin_badge =
            match g.team_score, g.opponent_score with
            | Some team_score, Some opponent_score ->
                let margin = team_score - opponent_score in
                let cls =
                  if margin > 0 then "bg-sky-500/10 text-sky-600 dark:text-sky-400 border-sky-500/30"
                  else if margin < 0 then "bg-rose-500/10 text-rose-600 dark:text-rose-400 border-rose-500/30"
                  else "bg-slate-500/10 text-slate-700 dark:text-slate-300 border-slate-500/30"
                in
                let wl = if margin > 0 then "W" else if margin < 0 then "L" else "T" in
                Printf.sprintf
                  {html|<span class="inline-flex items-center px-2 py-0.5 rounded border text-[10px] font-mono %s"><span class="font-bold">%s</span> %d-%d</span>|html}
                  cls wl team_score opponent_score
            | _ ->
                {html|<span class="inline-flex items-center px-2 py-0.5 rounded border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-600 dark:text-slate-400">-</span>|html}
          in
          let quality_badge = score_quality_badge ~lang ~compact:true g.score_quality in
          let ha_prefix = if g.is_home then "vs " else "@ " in
          let opponent_html = Printf.sprintf "%s<a href=\"%s\" class=\"hover:text-orange-600 dark:hover:text-orange-400 transition-colors\">%s</a>"
            (escape_html ha_prefix) (escape_html (team_href g.opponent)) (escape_html g.opponent) in
          let fg_str = Printf.sprintf "%d-%d" g.fg_made g.fg_att in
          let fg3_str = Printf.sprintf "%d-%d" g.fg3_made g.fg3_att in
          let ft_str = Printf.sprintf "%d-%d" g.ft_made g.ft_att in
          Printf.sprintf
            {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-[90px] sm:w-[110px]"><a href="%s" class="hover:text-orange-700 dark:text-orange-400 transition-colors">%s</a></td><td class="px-3 py-2 text-slate-900 dark:text-slate-200"><div class="flex items-center justify-between gap-3"><span class="player-name truncate">%s</span><div class="flex items-center gap-2 shrink-0">%s%s</div></div></td><td class="px-3 py-2 text-right font-mono text-slate-600 dark:text-slate-400 hidden sm:table-cell">%.1f</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right text-slate-600 dark:text-slate-400 hidden md:table-cell">%s</td><td class="px-3 py-2 text-right font-bold %s w-[60px] sm:w-[72px]">%d</td><td class="px-3 py-2 text-right font-mono w-[60px] sm:w-[72px] %s">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px]">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 hidden sm:table-cell">%d</td></tr>|html}
            (boxscore_href g.game_id)
            (escape_html g.game_date)
            opponent_html
            margin_badge
            quality_badge
            g.min
            fg_str fg3_str ft_str
            res_color
            g.pts
            pm_class
            (escape_html pm_str)
            g.reb
            g.ast
            g.stl
            g.blk
            g.tov)
      |> String.concat "\n"
  in
  let canonical = player_href p.id ^ "/games" in
  let seo_desc = Printf.sprintf "%s 경기별 기록 - %d경기 출전, WKBL 여자농구 선수 게임 로그"
    display_name (List.length games) in
  (* JSON-LD: ItemList of SportsEvent (max 20 for SEO) *)
  let player_team =
    match List.rev profile.team_stints with
    | latest :: _ -> String.trim latest.pts_team_name
    | [] -> String.trim profile.averages.team_name
  in
  let json_ld_data =
    let top_games = if List.length games > 20 then List.filteri (fun i _ -> i < 20) games else games in
    let items = top_games |> List.mapi (fun i (g: player_game_stat) ->
      let home_team, away_team =
        if g.is_home then (player_team, g.opponent)
        else (g.opponent, player_team)
      in
      Printf.sprintf {|{"@type":"ListItem","position":%d,"item":{"@type":"SportsEvent","startDate":"%s","name":"%s vs %s","homeTeam":{"@type":"SportsTeam","name":"%s"},"awayTeam":{"@type":"SportsTeam","name":"%s"},"sport":"Basketball","location":{"@type":"Place","name":"WKBL"}}}|}
        (i + 1) (escape_html g.game_date)
        (escape_html home_team) (escape_html away_team)
        (escape_html home_team) (escape_html away_team))
    |> String.concat ","
    in
    Printf.sprintf {|{"@context":"https://schema.org","@type":"ItemList","name":"%s 경기 로그","numberOfItems":%d,"itemListElement":[%s]}|}
      (escape_html display_name) (List.length top_games) items
  in
  layout ~lang ~title:(display_name ^ " | 경기 로그")
    ~canonical_path:canonical
    ~description:seo_desc
    ~json_ld:json_ld_data
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in">%s<div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-4"><div class="flex items-center gap-4"><div class="shrink-0">%s</div><div class="min-w-0"><div class="text-sm text-slate-600 dark:text-slate-400"><a href="%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">← 프로필</a></div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200 truncate">%s <span class="text-slate-600 dark:text-slate-400 text-lg font-mono">경기 로그</span></h2><div class="mt-1 text-slate-600 dark:text-slate-400 text-sm">총 %d경기</div></div></div><div class="flex flex-col items-start sm:items-end gap-3"><form action="%s/games" method="get" class="flex flex-wrap items-center justify-end gap-2"><select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none" data-auto-submit="change">%s</select><label class="flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" data-auto-submit="change"><span>불일치 포함</span></label></form><a href="%s/splits?season=%s" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">스플릿 분석</a><a href="%s/shots?season=%s" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">샷 차트</a>%s</div></div><p class="text-[11px] text-slate-600 dark:text-slate-400">개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계 기반입니다. 문자중계가 없으면 <span class="font-mono text-slate-700 dark:text-slate-300">M</span>으로 팀 득실마진(경기 최종 점수)을 대신 표시합니다. (데이터가 없거나 품질 문제면 <span class="font-mono text-slate-700 dark:text-slate-300">-</span>)</p><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg scroll-shadow"><table class="min-w-[680px] sm:min-w-[920px] w-full text-sm font-mono table-auto tabular-nums" aria-label="선수별 게임 로그">
          <colgroup>
            <col style="width: 110px;"> <!-- Date -->
            <col style="width: auto;">  <!-- Opponent -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- MIN -->
            <col class="hidden md:table-column" style="width: 72px;"> <!-- FG -->
            <col class="hidden md:table-column" style="width: 72px;"> <!-- 3P -->
            <col class="hidden md:table-column" style="width: 72px;"> <!-- FT -->
            <col style="width: 72px;">  <!-- PTS -->
            <col style="width: 72px;">  <!-- +/- -->
            <col style="width: 72px;">  <!-- REB -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- AST -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- STL -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- BLK -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- TOV -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase tracking-wider text-xs"><tr><th scope="col" class="px-3 py-2 text-left font-sans whitespace-nowrap">날짜</th><th scope="col" class="px-3 py-2 text-left font-sans">상대</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="출전시간">MIN</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="야투">FG</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="3점슛">3P</th><th scope="col" class="px-3 py-2 text-right hidden md:table-cell" title="자유투">FT</th><th scope="col" class="px-3 py-2 text-right text-orange-700 dark:text-orange-400" title="득점">PTS</th><th scope="col" class="px-3 py-2 text-right" title="+/-">+/-</th><th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="어시스트">AST</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="스틸">STL</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="블록">BLK</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="턴오버">TOV</th></tr></thead><tbody>%s</tbody>%s</table></div>|html></div>|html}
      (breadcrumb [("홈", "/"); ("선수", "/players"); (display_name, player_href p.id); ("경기 로그", "")])
      (player_img_tag ~class_name:"w-14 h-14 border border-slate-300 dark:border-slate-700 shadow-lg" p.id p.name)
      (player_href p.id)
      (escape_html display_name)
      (List.length games)
      (player_href p.id)
      season_options
      include_checked
      (player_href p.id)
      season
      (player_href p.id)
      season
      quality_chips
      rows
      tfoot_html) ()

(** Splits page — Home/Away, per-opponent, per-month breakdown *)
let player_splits_page ?(lang=I18n.Ko) (profile: player_profile) ~(season: string) ~(seasons: season_info list) (games: player_game_stat list) =
  let p = profile.player in
  let display_name = normalize_name p.name in
  let season_options =
    seasons
    |> List.map (fun (s: season_info) ->
        let selected = if s.code = season then "selected" else "" in
        Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
    |> String.concat "\n"
  in
  let (home_away, per_opponent, per_month) = compute_splits games in
  let pct a b = if b = 0 then "-" else Printf.sprintf ".%03d" (a * 1000 / b) in
  let f = format_float in
  let split_cols = [
    col ~sticky:true ~w:(px 120) "구분";
    col ~align:`Center ~title:"경기수" "G";
    col ~align:`Center ~resp:`Hidden_sm ~title:"승-패" "W-L";
    col ~align:`Right ~title:"평균 출전시간" "MPG";
    col ~align:`Right ~highlight:true ~title:"평균 득점" "PPG";
    col ~align:`Right ~title:"평균 리바운드" "RPG";
    col ~align:`Right ~title:"평균 어시스트" "APG";
    col ~align:`Right ~resp:`Hidden_md ~title:"평균 스틸" "SPG";
    col ~align:`Right ~resp:`Hidden_md ~title:"평균 블록" "BPG";
    col ~align:`Right ~resp:`Hidden_md ~title:"평균 턴오버" "TOV";
    col ~align:`Right ~resp:`Hidden_lg ~title:"야투 성공률" "FG%";
    col ~align:`Right ~resp:`Hidden_lg ~title:"3점슛 성공률" "3P%";
    col ~align:`Right ~resp:`Hidden_lg ~title:"자유투 성공률" "FT%";
  ] in
  let split_to_row ?(linkify_label=false) (s: split_aggregate) =
    let gp = float_of_int (max 1 s.sa_games) in
    let wl =
      if s.sa_wins + s.sa_losses = 0 then "-"
      else Printf.sprintf "%d-%d" s.sa_wins s.sa_losses
    in
    let label_html =
      if linkify_label then
        Printf.sprintf {|<a href="%s" class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>|}
          (escape_html (team_href s.sa_label)) (escape_html s.sa_label)
      else
        escape_html s.sa_label
    in
    [ label_html;
      string_of_int s.sa_games; wl;
      f (s.sa_min /. gp);
      f (float_of_int s.sa_pts /. gp);
      f (float_of_int s.sa_reb /. gp);
      f (float_of_int s.sa_ast /. gp);
      f (float_of_int s.sa_stl /. gp);
      f (float_of_int s.sa_blk /. gp);
      f (float_of_int s.sa_tov /. gp);
      pct s.sa_fg_made s.sa_fg_att;
      pct s.sa_fg3_made s.sa_fg3_att;
      pct s.sa_ft_made s.sa_ft_att ]
  in
  let split_table ?(linkify_label=false) ~title ~icon splits =
    let rows = List.map (split_to_row ~linkify_label) splits in
    Printf.sprintf
      {html|<div>
        <h3 class="flex items-center gap-2 text-base font-bold text-slate-900 dark:text-slate-200 mb-2">
          <span>%s</span> %s
        </h3>
        %s
      </div>|html}
      icon (escape_html title)
      (render_fixed_table ~striped:true
        ~aria_label:title
        ~id:("split-" ^ String.lowercase_ascii (String.map (fun c -> if c = '/' then '-' else c) title))
        ~min_width:"min-w-[700px]" ~cols:split_cols rows)
  in
  let home_away_html =
    split_table ~title:"홈/원정" ~icon:"🏠" home_away
  in
  let opponent_html =
    split_table ~linkify_label:true ~title:"상대팀별" ~icon:"⚔️" per_opponent
  in
  let month_html =
    split_table ~title:"월별" ~icon:"📅" per_month
  in
  let season_name =
    match List.find_opt (fun (s: season_info) -> s.code = season) seasons with
    | Some s -> s.name | None -> season
  in
  let page_title = Printf.sprintf "%s 스플릿 | WKBL" (escape_html display_name) in
  let page_desc = Printf.sprintf "%s 선수의 %s 시즌 홈/원정, 상대팀별, 월별 성적 분석" (escape_html display_name) (escape_html season_name) in
  layout ~lang ~title:page_title
    ~canonical_path:(player_href p.id ^ "/splits")
    ~description:page_desc
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        %s
        <div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-4">
          <div class="flex items-center gap-4">
            <div class="shrink-0">%s</div>
            <div class="min-w-0">
              <div class="text-sm text-slate-600 dark:text-slate-400">
                <a href="%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">← 프로필</a>
              </div>
              <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200 truncate">%s
                <span class="text-slate-600 dark:text-slate-400 text-lg font-mono">스플릿</span>
              </h2>
              <div class="mt-1 text-slate-600 dark:text-slate-400 text-sm">%s · %d경기</div>
            </div>
          </div>
          <div class="flex items-center gap-2">
            <a href="%s/games?season=%s" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">경기 로그</a>
            <a href="%s/shots?season=%s" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">샷 차트</a>
            <form action="%s/splits" method="get" class="flex items-center gap-2">
              <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none" data-auto-submit="change">
                %s
              </select>
            </form>
          </div>
        </div>
        %s
        %s
        %s
      </div>|html}
      (breadcrumb [("홈", "/"); ("선수", "/players"); (display_name, player_href p.id); ("스플릿", "")])
      (player_img_tag ~class_name:"w-14 h-14 border border-slate-300 dark:border-slate-700 shadow-lg" p.id p.name)
      (player_href p.id)
      (escape_html display_name)
      (escape_html season_name)
      (List.length games)
      (player_href p.id)
      season
      (player_href p.id)
      season
      (player_href p.id)
      season_options
      home_away_html
      opponent_html
      month_html) ()

(** Shot chart page — zone FG% visualization *)
let player_shot_chart_page ?(lang=I18n.Ko) (profile: player_profile) ~(season: string) ~(seasons: season_info list) (chart: player_shot_chart) =
  let p = profile.player in
  let display_name = normalize_name p.name in
  let season_options =
    seasons
    |> List.map (fun (s: season_info) ->
        let selected = if s.code = season then "selected" else "" in
        Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
    |> String.concat "\n"
  in
  let season_name =
    match List.find_opt (fun (s: season_info) -> s.code = season) seasons with
    | Some s -> s.name | None -> season
  in
  let zone_pct_str (zs: zone_stats) =
    if zs.zs_attempts = 0 then "-"
    else Printf.sprintf "%.1f%%" zs.zs_pct
  in
  let zone_detail (zs: zone_stats) =
    Printf.sprintf "%d/%d" zs.zs_made zs.zs_attempts
  in
  (* Color intensity based on FG% — higher = more green, lower = more red *)
  let zone_color (zs: zone_stats) =
    if zs.zs_attempts = 0 then "bg-slate-200 dark:bg-slate-700 text-slate-500 dark:text-slate-400"
    else if zs.zs_pct >= 50.0 then "bg-green-100 dark:bg-green-900/40 text-green-800 dark:text-green-300 ring-2 ring-green-300 dark:ring-green-700"
    else if zs.zs_pct >= 40.0 then "bg-emerald-50 dark:bg-emerald-900/30 text-emerald-700 dark:text-emerald-400"
    else if zs.zs_pct >= 30.0 then "bg-amber-50 dark:bg-amber-900/30 text-amber-700 dark:text-amber-400"
    else "bg-red-50 dark:bg-red-900/30 text-red-700 dark:text-red-400"
  in
  (* The visual court: 2 zones (2PT combined + 3PT) + paint indicator *)
  let court_html = Printf.sprintf
    {html|<div class="flex justify-center">
      <div class="relative w-[340px] h-[280px] sm:w-[400px] sm:h-[320px]">
        <!-- Three-point arc (outermost) -->
        <div class="absolute inset-0 rounded-t-[170px] sm:rounded-t-[200px] border-2 border-slate-300 dark:border-slate-600 %s flex flex-col items-center justify-start pt-6 sm:pt-8 cursor-default transition-colors">
          <div class="text-2xl sm:text-3xl font-black">%s</div>
          <div class="text-[11px] font-medium mt-0.5">3점</div>
          <div class="text-[10px] opacity-70">%s</div>
        </div>
        <!-- 2PT zone (paint + mid-range combined) -->
        <div class="absolute left-[18%%] right-[18%%] top-[30%%] bottom-0 rounded-t-[110px] sm:rounded-t-[130px] border-2 border-slate-300 dark:border-slate-600 %s flex flex-col items-center justify-start pt-4 sm:pt-5 cursor-default transition-colors">
          <div class="text-2xl sm:text-3xl font-black">%s</div>
          <div class="text-[11px] font-medium mt-0.5">2점</div>
          <div class="text-[10px] opacity-70">%s</div>
          <div class="text-[9px] opacity-50 mt-1">페인트존 %d성공</div>
        </div>
        <!-- Basket indicator -->
        <div class="absolute left-1/2 -translate-x-1/2 bottom-1 w-4 h-4 rounded-full border-2 border-orange-400 dark:border-orange-500 bg-orange-100 dark:bg-orange-900/40"></div>
      </div>
    </div>|html}
    (zone_color chart.psc_three) (zone_pct_str chart.psc_three) (zone_detail chart.psc_three)
    (zone_color chart.psc_two_pt) (zone_pct_str chart.psc_two_pt) (zone_detail chart.psc_two_pt)
    chart.psc_paint_made
  in
  (* Summary stats table *)
  let total_pct_str = if chart.psc_total_attempts = 0 then "-"
    else Printf.sprintf "%.1f%%" chart.psc_total_pct in
  let summary_html = Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900/80 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-sm">
      <h3 class="text-sm font-bold text-slate-700 dark:text-slate-300 uppercase tracking-wide mb-3">슈팅 요약</h3>
      <div class="grid grid-cols-2 sm:grid-cols-4 gap-4">
        <div class="text-center">
          <div class="text-2xl font-black text-slate-900 dark:text-slate-100 tabular-nums font-mono">%s</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 mt-1">전체 FG%%</div>
          <div class="text-[10px] text-slate-400 dark:text-slate-400">%d/%d</div>
        </div>
        <div class="text-center">
          <div class="text-2xl font-black text-slate-900 dark:text-slate-100 tabular-nums font-mono">%s</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 mt-1">2점슛</div>
          <div class="text-[10px] text-slate-400 dark:text-slate-400">%s</div>
        </div>
        <div class="text-center">
          <div class="text-2xl font-black text-emerald-600 dark:text-emerald-400 tabular-nums font-mono">%d</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 mt-1">페인트존 성공</div>
          <div class="text-[10px] text-slate-400 dark:text-slate-400">미스 미집계</div>
        </div>
        <div class="text-center">
          <div class="text-2xl font-black text-slate-900 dark:text-slate-100 tabular-nums font-mono">%s</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 mt-1">3점슛</div>
          <div class="text-[10px] text-slate-400 dark:text-slate-400">%s</div>
        </div>
      </div>
    </div>|html}
    total_pct_str chart.psc_total_made chart.psc_total_attempts
    (zone_pct_str chart.psc_two_pt) (zone_detail chart.psc_two_pt)
    chart.psc_paint_made
    (zone_pct_str chart.psc_three) (zone_detail chart.psc_three)
  in
  let page_title = Printf.sprintf "%s 샷 차트 | WKBL" (escape_html display_name) in
  let page_desc = Printf.sprintf "%s 선수의 %s 시즌 슈팅 존별 야투 성공률 분석" (escape_html display_name) (escape_html season_name) in
  layout ~lang ~title:page_title
    ~canonical_path:(player_href p.id ^ "/shot-chart")
    ~description:page_desc
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        %s
        <div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-4">
          <div class="flex items-center gap-4">
            <div class="shrink-0">%s</div>
            <div class="min-w-0">
              <div class="text-sm text-slate-600 dark:text-slate-400">
                <a href="%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">← 프로필</a>
              </div>
              <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200 truncate">%s
                <span class="text-slate-600 dark:text-slate-400 text-lg font-mono">샷 차트</span>
              </h2>
              <div class="mt-1 text-slate-600 dark:text-slate-400 text-sm">%s</div>
            </div>
          </div>
          <div class="flex items-center gap-2">
            <a href="%s/games?season=%s" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">경기 로그</a>
            <a href="%s/splits?season=%s" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">스플릿</a>
            <form action="%s/shots" method="get" class="flex items-center">
              <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none" data-auto-submit="change">
                <option value="ALL" %s>전체 시즌</option>
                %s
              </select>
            </form>
          </div>
        </div>
        %s
        %s
        <p class="text-[11px] text-slate-500 dark:text-slate-400 text-center">문자중계(PBP) 데이터 기반 집계입니다. PBP가 없는 경기는 포함되지 않습니다.</p>
      </div>|html}
      (breadcrumb [("홈", "/"); ("선수", "/players"); (display_name, player_href p.id); ("샷 차트", "")])
      (player_img_tag ~class_name:"w-14 h-14 border border-slate-300 dark:border-slate-700 shadow-lg" p.id p.name)
      (player_href p.id)
      (escape_html display_name)
      (escape_html season_name)
      (player_href p.id)
      season
      (player_href p.id)
      season
      (player_href p.id)
      (if season = "ALL" then "selected" else "")
      season_options
      court_html
      summary_html) ()

(* Re-export from Views_history for convenient access *)
let player_career_page = Views_history.player_career_page
