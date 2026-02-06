(** Player-specific view functions for WKBL Analytics *)
(** Contains player profile and game logs pages. *)

open Domain
open Views_common

(** Milestone Tracker - Career milestones for players *)

(** Milestone thresholds for each stat category *)
let points_milestones = [1000; 2000; 3000; 5000; 7000; 10000]
let rebounds_milestones = [500; 1000; 2000; 3000; 5000]
let assists_milestones = [300; 500; 1000; 2000; 3000]
let games_milestones = [100; 200; 300; 500]

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
            <h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2">
              <span class="text-lg">🎖️</span> Career Milestones
            </h3>
            <div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">
              커리어 누적 기록 마일스톤 달성 현황
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
let advanced_stats_card (avg: player_aggregate) =
  if avg.games_played <= 0 then ""
  else
    (* Calculate PER using existing function *)
    let per = Stats.per_of_player_aggregate avg in
    (* Simple efficiency-based estimates without full shooting data *)
    let pts_per_game = avg.avg_points in
    let eff_per_game = avg.efficiency /. float_of_int avg.games_played in
    let minutes_per_game = avg.total_minutes /. float_of_int avg.games_played in
    (* PER rating color *)
    let per_color =
      if per >= 25.0 then "text-emerald-600 dark:text-emerald-400"
      else if per >= 20.0 then "text-sky-600 dark:text-sky-400"
      else if per >= 15.0 then "text-slate-700 dark:text-slate-300"
      else "text-rose-600 dark:text-rose-400"
    in
    let stat_box label value unit description color =
      Printf.sprintf
        {html|<div class="bg-slate-50 dark:bg-slate-800/50 rounded-lg p-3 text-center">
          <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase tracking-wider font-bold">%s</div>
          <div class="text-2xl font-black %s font-mono tabular-nums">%.1f<span class="text-xs text-slate-400">%s</span></div>
          <div class="text-[9px] text-slate-400 dark:text-slate-500 mt-1">%s</div>
        </div>|html}
        (escape_html label) color value unit (escape_html description)
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg">
        <div class="flex items-start justify-between gap-4 mb-4">
          <div class="min-w-0">
            <h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2">
              <span class="text-lg">📊</span> Advanced Stats
            </h3>
            <div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed">
              효율 지표 및 영향력 분석
            </div>
          </div>
        </div>
        <div class="grid grid-cols-2 md:grid-cols-4 gap-3">
          %s
          %s
          %s
          %s
        </div>
        <div class="mt-4 pt-3 border-t border-slate-200 dark:border-slate-700">
          <div class="text-[10px] text-slate-500 dark:text-slate-400 leading-relaxed">
            <strong>PER</strong>: Player Efficiency Rating (리그 평균 ≈ 15.0) |
            <strong>EFF/G</strong>: 경기당 효율 |
            <strong>PTS/G</strong>: 경기당 득점 |
            <strong>MIN/G</strong>: 경기당 출전 시간
          </div>
        </div>
      </div>|html}
      (stat_box "PER" per "" "효율 레이팅" per_color)
      (stat_box "EFF/G" eff_per_game "" "경기당 효율" "text-orange-600 dark:text-orange-400")
      (stat_box "PTS/G" pts_per_game "" "경기당 득점" "text-slate-700 dark:text-slate-300")
      (stat_box "MIN/G" minutes_per_game "" "경기당 출전" "text-slate-700 dark:text-slate-300")

let player_profile_page ?(leaderboards=None) ?(show_ops=false) (profile: player_profile) ~scope ~(seasons_catalog: season_info list) =
  let _ = leaderboards in (* suppress unused warning *)
  let _ = seasons_catalog in
  let p = profile.player in
  let pos = match p.position with Some s -> s | None -> "-" in
  let height_text =
    match p.height with
    | Some h when h > 0 && h <= 210 -> Printf.sprintf "%dcm" h  (* valid basketball height range *)
    | _ -> "-"
  in
  let info_text = Printf.sprintf "%s | %s" pos height_text in
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
          link_button ~label:"▶ 검색" ~url:youtube_search ~title:"YouTube search" ()
      | Some l ->
          let title =
            match l.pel_source_url with
            | None -> "YouTube (verified link)"
            | Some src -> "YouTube (source: " ^ src ^ ")"
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
          link_button ~label:"인스타 검색" ~url:instagram_search ~title:"Instagram search" ()
      | Some l ->
          let title =
            match l.pel_source_url with
            | None -> "Instagram (verified link)"
            | Some src -> "Instagram (source: " ^ src ^ ")"
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
      (link_button ~label:"WKBL" ~url:wkbl_profile ~title:"Official profile" ())
      youtube_html
      instagram_html
  in
  let game_rows games =
    games
    |> List.map (fun (g: player_game_stat) ->
        let res_color = if g.pts >= 20 then "text-orange-600 dark:text-orange-400" else "text-slate-700 dark:text-slate-300" in
        let pm_class, pm_str =
          match g.plus_minus, g.team_score, g.opponent_score with
          | Some v, _, _ ->
              let cls =
                if v > 0 then "text-sky-600 dark:text-sky-400"
                else if v < 0 then "text-rose-600 dark:text-rose-400"
                else "text-slate-500 dark:text-slate-500"
              in
              let s = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
              (cls, s)
          | None, Some team_score, Some opponent_score ->
              let margin = team_score - opponent_score in
              let cls =
                if margin > 0 then "text-sky-600 dark:text-sky-400"
                else if margin < 0 then "text-rose-600 dark:text-rose-400"
                else "text-slate-500 dark:text-slate-500"
              in
              let s =
                if margin > 0 then Printf.sprintf "M +%d" margin
                else Printf.sprintf "M %d" margin
              in
              (cls, s)
          | None, _, _ -> ("text-slate-500 dark:text-slate-500", "-")
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
              let label =
                if margin > 0 then Printf.sprintf "W +%d" margin
                else if margin < 0 then Printf.sprintf "L %d" margin
                else "T 0"
              in
              Printf.sprintf
                {html|<span class="inline-flex items-center justify-center px-2 py-0.5 rounded border text-[10px] font-mono min-w-[52px] %s">%s</span>|html}
                cls label
          | _ ->
              {html|<span class="inline-flex items-center justify-center px-2 py-0.5 rounded border border-slate-200 dark:border-slate-700/60 text-[10px] font-mono text-slate-500 dark:text-slate-500 min-w-[52px]">-</span>|html}
        in
        let quality_badge = score_quality_badge ~compact:true g.score_quality in
        let opponent_label = if g.is_home then "vs " ^ g.opponent else "@ " ^ g.opponent in
        let opponent_href = "/team/" ^ Uri.pct_encode g.opponent in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-[90px] sm:w-[110px]"><a href="/boxscore/%s" class="hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a></td><td class="px-3 py-2 text-slate-900 dark:text-white"><div class="flex flex-wrap items-center gap-x-3 gap-y-2"><a href="%s" class="player-name min-w-0 flex-1 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a><div class="flex items-center gap-2 shrink-0">%s%s</div></div></td><td class="px-3 py-2 text-right font-mono text-slate-600 dark:text-slate-400 w-[60px] sm:w-[72px] whitespace-nowrap">%.1f</td><td class="px-3 py-2 text-right font-bold %s w-[60px] sm:w-[72px] whitespace-nowrap">%d</td><td class="px-3 py-2 text-right font-mono w-[60px] sm:w-[72px] whitespace-nowrap %s">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] whitespace-nowrap">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td></tr>|html}
          (Uri.pct_encode g.game_id)
          (escape_html g.game_date)
          (escape_html opponent_href)
          (escape_html opponent_label)
          margin_badge
          quality_badge
          g.min
          res_color
          g.pts
          pm_class
          (escape_html pm_str)
          g.reb
          g.ast
          g.stl
          g.blk)
    |> String.concat "\n"
  in
  let birth_chip =
    Printf.sprintf
      {html|<span class="bg-white/20 dark:bg-slate-800/40 backdrop-blur-md text-slate-900 dark:text-slate-200 border border-white/30 dark:border-slate-700/50 px-2.5 py-1 rounded-lg text-[11px] sm:text-sm shadow-sm"><span class="text-slate-600 dark:text-slate-500">생년</span> %s</span>|html}
      (escape_html birth_text)
  in
  let recent_rows = game_rows profile.recent_games in
  let all_star_rows = game_rows profile.all_star_games in
  let season_stats_component = player_season_stats_component ~player_id:p.id ~scope profile.season_breakdown in
  let career_trajectory_html = career_trajectory_chart profile.season_breakdown in
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
                  {html|<span title="Score unavailable" class="inline-flex items-center justify-center w-6 h-6 rounded border bg-slate-100 dark:bg-slate-800/40 border-slate-200 dark:border-slate-700/40 text-[11px] font-mono text-slate-400 dark:text-slate-500 shadow-sm">?</span>|html}
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
        {html|<div class="flex items-center gap-2"><span class="text-[11px] text-slate-500 dark:text-slate-500 font-mono whitespace-nowrap">%s</span><div class="flex items-center gap-1">%s</div></div>|html}
        (escape_html label)
        chips_html
  in
  let recent_games_header_html =
    Printf.sprintf
      {html|<div class="flex items-start justify-between gap-3"><h3 class="text-xl font-bold text-slate-900 dark:text-white">최근 경기</h3><div class="flex flex-wrap items-center justify-end gap-2 shrink-0">%s<a href="/player/%s/games" class="text-xs bg-slate-100 dark:bg-slate-800 hover:bg-slate-200 dark:hover:bg-slate-700 px-3 py-2 rounded-lg text-slate-700 dark:text-slate-300 hover:text-slate-900 dark:hover:text-white transition-all shadow-sm whitespace-nowrap border border-slate-200 dark:border-slate-700">전체 경기</a></div></div><p class="text-[11px] text-slate-500 dark:text-slate-500 mt-1">개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계 기반입니다. 문자중계가 없으면 <span class="font-mono text-slate-700 dark:text-slate-300">M</span>으로 팀 득실마진(경기 최종 점수)을 대신 표시합니다. <span class="font-mono text-slate-700 dark:text-slate-300">Σ</span>는 득점합 기반 보정 결과입니다.</p>|html}
      recent_wl_summary_html
      (Uri.pct_encode p.id)
  in
  let all_star_section_html =
    if profile.all_star_games = [] then
      ""
    else
      Printf.sprintf
        {html|<div class="space-y-4"><div class="flex flex-col gap-1"><h3 class="text-xl font-bold text-slate-900 dark:text-white">올스타전</h3><p class="text-[11px] text-slate-500 dark:text-slate-500">올스타전(<span class="font-mono text-slate-700 dark:text-slate-300">game_type=10</span>) 기록은 시즌/커리어 테이블에서 제외하고 여기서만 보여줍니다.</p></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg"><table class="min-w-[680px] sm:min-w-[920px] w-full text-sm font-mono table-fixed" aria-label="올스타전 기록">
                <colgroup>
                  <col style="width: 110px;"> <!-- Date -->
                  <col style="width: auto;">  <!-- Opponent -->
                  <col style="width: 72px;">  <!-- MIN -->
                  <col style="width: 72px;">  <!-- PTS -->
                  <col style="width: 72px;">  <!-- +/- -->
                  <col style="width: 72px;">  <!-- REB -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- AST -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- STL -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- BLK -->
                </colgroup>
                <thead class="bg-slate-100/80 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase tracking-wider text-xs whitespace-nowrap"><tr><th scope="col" class="px-3 py-2 text-left font-sans">날짜</th><th scope="col" class="px-3 py-2 text-left font-sans">상대</th><th scope="col" class="px-3 py-2 text-right" title="출전시간">MIN</th><th scope="col" class="px-3 py-2 text-right text-orange-600 dark:text-orange-400" title="득점">PTS</th><th scope="col" class="px-3 py-2 text-right" title="+/-">+/-</th><th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="어시스트">AST</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="스틸">STL</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="블록">BLK</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
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
	              {html|<div class="mt-3 text-xs text-slate-600 dark:text-slate-400 leading-relaxed">이름 기반 매칭 결과가 없습니다. (동명이인/표기 차이/기간 외)</div>|html}
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
		          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><div class="min-w-0"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🔁</span> 팀 이동</h3><div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed break-words">박스스코어 출전팀 변화로 추정한 연보입니다. (기간=첫/마지막 출전일)</div></div><span class="text-[11px] text-slate-600 dark:text-slate-400 font-mono shrink-0">박스스코어</span></div><div class="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-1 xl:grid-cols-3 gap-3 text-xs"><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">현재</div><div class="mt-2 text-slate-700 dark:text-slate-300 min-w-0">%s</div></div><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">이적</div><div class="mt-2 font-mono text-slate-900 dark:text-slate-200 text-lg font-black">%d</div></div><div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest text-[11px]">최근</div><div class="mt-2 text-slate-700 dark:text-slate-300 min-w-0">%s</div></div></div><div class="mt-4 rounded-lg border border-slate-200 dark:border-slate-800/60 bg-slate-950/30 p-4"><ol class="relative border-l border-slate-200 dark:border-slate-800/60 ml-2 space-y-4">%s</ol></div>%s</div>|html}
	          current_team_html transfers last_move_value_html stint_rows official_html
  in

  let _career_highs_html = career_highs_card profile.career_highs in
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
		          {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><div class="min-w-0"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🏅</span> 리더보드</h3><div class="mt-1 text-[11px] text-slate-600 dark:text-slate-400 leading-relaxed"><span class="font-mono text-slate-700 dark:text-slate-300">%s</span> · <span class="font-mono text-slate-700 dark:text-slate-300">%s</span> · 상위 5</div></div><a href="%s" class="text-[11px] text-slate-600 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono shrink-0 whitespace-nowrap">리더</a></div>%s</div>|html}
	          (escape_html lb_season_name)
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
            {html|<div class="text-slate-600 dark:text-slate-400 leading-relaxed">현재 DB에 %d시즌 정도만 있어요. 더 채우려면 아래를 실행하세요:</div><code class="mt-2 block font-mono text-slate-700 dark:text-slate-300 bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/60 px-3 py-2 rounded overflow-x-auto whitespace-nowrap">dune exec bin/scraper_tool.exe sync history</code>|html}
            db_season_count
        else
          Printf.sprintf
            {html|<div class="text-slate-600 dark:text-slate-400 leading-relaxed">현재 데이터는 아직 일부 시즌만 있어요. 조금씩 채워지고 있습니다. (현재 약 %d시즌)</div>|html}
            db_season_count
      else
        ""
    in
    let backfill_details_html =
      if backfill_row_html = "" then
        ""
      else
        let summary = if show_ops then "Backfill" else "데이터 안내" in
        Printf.sprintf
          {html|<details class="mt-3 text-[11px] text-slate-600 dark:text-slate-400"><summary class="cursor-pointer select-none text-slate-600 dark:text-slate-400 font-bold">%s</summary><div class="mt-2">%s</div></details>|html}
          summary
          backfill_row_html
    in
    let season_title = if show_ops then "Seasons" else "시즌" in
    let season_label_data = if show_ops then "DB" else "데이터" in
    let season_label_player = if show_ops then "Player" else "선수" in
    let season_count_chips_html =
      if show_ops then
        Printf.sprintf
          {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-slate-700 dark:text-slate-300 whitespace-nowrap">DB n=%d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-slate-700 dark:text-slate-300 whitespace-nowrap">Player n=%d</span>|html}
          db_season_count
          player_season_count
      else
        Printf.sprintf
          {html|<span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-slate-700 dark:text-slate-300 whitespace-nowrap">데이터 시즌 %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-slate-700 dark:text-slate-300 whitespace-nowrap">선수 시즌 %d</span>|html}
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
		        {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-4 sm:p-5 min-w-0"><div class="flex flex-wrap items-start justify-between gap-3 min-w-0"><div class="text-slate-600 dark:text-slate-400 font-bold uppercase tracking-widest text-[11px] flex items-center gap-2"><span class="text-base">🎥</span> 문자중계 +/-</div><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">부분</span></div>%s<div class="mt-3 text-slate-600 dark:text-slate-400 text-xs leading-relaxed space-y-1"><div><span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>: 문자중계 기반</div><div><span class="font-mono text-slate-700 dark:text-slate-300">M</span>: 문자중계가 없을 때 팀 득실마진(경기 최종 점수)</div><div><span class="font-mono text-slate-700 dark:text-slate-300">-</span>: 데이터 없음/품질 이슈</div></div></div>|html}
		        pbp_stat_line
		    in
		    let draft_card_html =
		      let ops_sync_html =
		        if show_ops then
		          {html|<details class="text-[11px] text-slate-600 dark:text-slate-400"><summary class="cursor-pointer select-none text-slate-600 dark:text-slate-400 font-bold">Sync</summary><div class="mt-2"><div class="leading-relaxed">공식 페이지 기반 추가 수집이 필요하면 아래를 실행하세요: (네트워크 필요)</div><code class="mt-2 block font-mono text-slate-700 dark:text-slate-300 bg-white dark:bg-slate-900/40 border border-slate-300 dark:border-slate-700/60 px-3 py-2 rounded overflow-x-auto whitespace-nowrap">dune exec bin/scraper_tool.exe draft</code></div></details>|html}
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
		      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-6 shadow-lg"><div class="flex items-start justify-between gap-4 mb-4"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs flex items-center gap-2"><span class="text-lg">🧾</span> 데이터 안내</h3><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">현황</span></div><div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-1 xl:grid-cols-2 gap-3 md:gap-4">%s%s<div class="md:col-span-2 lg:col-span-1 xl:col-span-2">%s</div></div></div>|html}
		      seasons_card_html
		      pbp_card_html
		      draft_card_html
  in
		  let missing_data_html =
		    if profile.season_breakdown = [] && profile.recent_games = [] && profile.all_star_games = [] && profile.draft = None && profile.official_trade_events = [] then
		      {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/50 p-4 sm:p-6 flex items-center justify-center text-slate-600 dark:text-slate-400 text-sm gap-2"><span class="text-xl">🚧</span><div><div class="font-bold">데이터 수집 중</div><div>추가 정보는 순차적으로 업데이트됩니다.</div></div></div>|html}
		    else
		      ""
		  in
	  (* Trend analysis panel - requires at least 2 recent games *)
	  let trends_panel_html =
	    if List.length profile.recent_games >= 2 then
	      Views_charts.player_trends_panel profile.recent_games
	    else
	      ""
	  in

  let display_name = normalize_name p.name in
  let og_card_url = Printf.sprintf "https://wkbl.win/card/player/png/%s" p.id in
  let canonical = Printf.sprintf "/player/%s" p.id in
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
  layout ~title:(display_name ^ " | WKBL 선수")
    ~canonical_path:canonical
    ~description:seo_desc
    ~og_image:og_card_url
    ~json_ld:json_ld_data
    ~content:(Printf.sprintf {html|<div class="space-y-6 sm:space-y-8 animate-fade-in">
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
          <div class="space-y-4">
            %s
            <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
              <table class="min-w-[680px] sm:min-w-[920px] w-full text-xs sm:text-sm font-mono table-fixed tabular-nums" aria-label="최근 경기 기록">
                <colgroup>
                  <col style="width: 110px;"> <!-- Date -->
                  <col style="width: auto;">  <!-- Opponent -->
                  <col style="width: 72px;">  <!-- MIN -->
                  <col style="width: 72px;">  <!-- PTS -->
                  <col style="width: 72px;">  <!-- +/- -->
                  <col style="width: 72px;">  <!-- REB -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- AST -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- STL -->
                  <col class="hidden sm:table-column" style="width: 72px;"> <!-- BLK -->
                </colgroup>
                <thead class="bg-slate-100/80 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase tracking-wider text-[10px] sm:text-xs whitespace-nowrap">
                  <tr>
                    <th scope="col" class="px-3 py-2 text-left font-sans">날짜</th>
                    <th scope="col" class="px-3 py-2 text-left font-sans">상대</th>
                    <th scope="col" class="px-3 py-2 text-right" title="출전시간">MIN</th>
                    <th scope="col" class="px-3 py-2 text-right text-orange-600 dark:text-orange-400" title="득점">PTS</th>
                    <th scope="col" class="px-3 py-2 text-right" title="+/-">+/-</th>
                    <th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="어시스트">AST</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="스틸">STL</th>
                    <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="블록">BLK</th>
                  </tr>
                </thead>
                <tbody>%s</tbody>
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
          "" (* Placeholder for hero footer element - line 2231 %s *)
          season_stats_component
          career_trajectory_html
          recent_games_header_html
          recent_rows
          all_star_section_html
          team_movement_html
          leaderboards_html
          (Printf.sprintf {html|<div hx-get="/player/%s/shot-chart" hx-trigger="load" hx-swap="innerHTML" class="htmx-indicator-wrapper"><div class="text-center py-4"><span class="htmx-indicator inline-flex items-center gap-2 text-slate-400"><span class="w-4 h-4 border-2 border-slate-300 border-t-orange-500 rounded-full animate-spin" aria-hidden="true"></span><span>슛 차트 로딩 중...</span></span></div></div>|html} (Uri.pct_encode p.id))
          trends_panel_html
          (advanced_stats_card profile.averages)
          (career_highs_card profile.career_highs)
          (milestone_tracker_card profile.averages)
          missing_data_html
          data_notes_html) ()
let player_game_logs_page (profile: player_profile) ~(season: string) ~(seasons: season_info list) ~(include_mismatch: bool) (games: player_game_stat list) =
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
      {html|<div class="flex flex-wrap items-center gap-2 text-[11px] font-mono text-slate-600 dark:text-slate-400"><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">✓ %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">Σ %d</span><span class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 whitespace-nowrap">! %d</span></div>|html}
      verified_cnt derived_cnt mismatch_cnt
  in
  let rows =
    if games = [] then
      {html|<tr><td colspan="5" class="sm:hidden px-4 py-10 text-center text-slate-600 dark:text-slate-400 text-sm">경기 기록이 없습니다.</td><td colspan="9" class="hidden sm:table-cell px-4 py-10 text-center text-slate-600 dark:text-slate-400 text-sm">경기 기록이 없습니다.</td></tr>|html}
    else
      games
      |> List.map (fun (g: player_game_stat) ->
          let res_color = if g.pts >= 20 then "text-orange-600 dark:text-orange-400" else "text-slate-700 dark:text-slate-300" in
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
                  else "bg-slate-50 dark:bg-[#0b0e14]0/10 text-slate-700 dark:text-slate-300 border-slate-500/30"
                in
                let label =
                  if margin > 0 then Printf.sprintf "W +%d" margin
                  else if margin < 0 then Printf.sprintf "L %d" margin
                  else "T 0"
                in
                Printf.sprintf
                  {html|<span class="inline-flex items-center px-2 py-0.5 rounded border text-[10px] font-mono %s">%s</span>|html}
                  cls label
            | _ ->
                {html|<span class="inline-flex items-center px-2 py-0.5 rounded border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-600 dark:text-slate-400">-</span>|html}
          in
          let quality_badge = score_quality_badge ~compact:true g.score_quality in
          let opponent_label = if g.is_home then "vs " ^ g.opponent else "@ " ^ g.opponent in
          Printf.sprintf
            {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-sm font-mono whitespace-nowrap w-[90px] sm:w-[110px]"><a href="/boxscore/%s" class="hover:text-orange-600 dark:text-orange-400 transition-colors">%s</a></td><td class="px-3 py-2 text-slate-900 dark:text-slate-200"><div class="flex items-center justify-between gap-3"><span class="player-name truncate">%s</span><div class="flex items-center gap-2 shrink-0">%s%s</div></div></td><td class="px-3 py-2 text-right font-mono text-slate-600 dark:text-slate-400 w-[60px] sm:w-[72px] hidden sm:table-cell">%.1f</td><td class="px-3 py-2 text-right font-bold %s w-[60px] sm:w-[72px]">%d</td><td class="px-3 py-2 text-right font-mono w-[60px] sm:w-[72px] %s">%s</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px]">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td><td class="px-3 py-2 text-right text-slate-700 dark:text-slate-300 w-[60px] sm:w-[72px] hidden sm:table-cell">%d</td></tr>|html}
            (Uri.pct_encode g.game_id)
            (escape_html g.game_date)
            (escape_html opponent_label)
            margin_badge
            quality_badge
            g.min
            res_color
            g.pts
            pm_class
            (escape_html pm_str)
            g.reb
            g.ast
            g.stl
            g.blk)
      |> String.concat "\n"
  in
  let canonical = Printf.sprintf "/player/%s/games" p.id in
  let seo_desc = Printf.sprintf "%s 경기별 기록 - %d경기 출전, WKBL 여자농구 선수 게임 로그"
    display_name (List.length games) in
  layout ~title:(display_name ^ " | 경기 로그")
    ~canonical_path:canonical
    ~description:seo_desc
    ~content:(Printf.sprintf
      {html|<div class="space-y-8 animate-fade-in"><div class="flex flex-col sm:flex-row sm:items-end sm:justify-between gap-4"><div class="flex items-center gap-4"><div class="shrink-0">%s</div><div class="min-w-0"><div class="text-sm text-slate-600 dark:text-slate-400"><a href="/player/%s" class="hover:text-slate-900 dark:hover:text-white dark:text-slate-200 transition">← 프로필</a></div><h2 class="text-3xl font-black text-slate-900 dark:text-slate-200 truncate">%s <span class="text-slate-600 dark:text-slate-400 text-lg font-mono">경기 로그</span></h2><div class="mt-1 text-slate-600 dark:text-slate-400 text-sm">총 %d경기</div></div></div><div class="flex flex-col items-start sm:items-end gap-3"><form action="/player/%s/games" method="get" class="flex flex-wrap items-center justify-end gap-2"><select name="season" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none" onchange="this.form.submit()">%s</select><label class="flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400 whitespace-nowrap"><input type="checkbox" name="include_mismatch" value="1" %s class="h-4 w-4 rounded border-slate-300 dark:border-slate-700 bg-slate-100 dark:bg-slate-800 accent-orange-500" onchange="this.form.submit()"><span>불일치 포함</span></label></form>%s</div></div><p class="text-[11px] text-slate-600 dark:text-slate-400">개인 <span class="font-mono text-slate-700 dark:text-slate-300">+/-</span>는 문자중계 기반입니다. 문자중계가 없으면 <span class="font-mono text-slate-700 dark:text-slate-300">M</span>으로 팀 득실마진(경기 최종 점수)을 대신 표시합니다. (데이터가 없거나 품질 이슈면 <span class="font-mono text-slate-700 dark:text-slate-300">-</span>)</p><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg"><table class="min-w-[680px] sm:min-w-[920px] w-full text-sm font-mono table-fixed tabular-nums" aria-label="선수별 게임 로그">
          <colgroup>
            <col style="width: 110px;"> <!-- Date -->
            <col style="width: auto;">  <!-- Opponent -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- MIN -->
            <col style="width: 72px;">  <!-- PTS -->
            <col style="width: 72px;">  <!-- +/- -->
            <col style="width: 72px;">  <!-- REB -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- AST -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- STL -->
            <col class="hidden sm:table-column" style="width: 72px;"> <!-- BLK -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-600 dark:text-slate-400 uppercase tracking-wider text-xs"><tr><th scope="col" class="px-3 py-2 text-left font-sans whitespace-nowrap">날짜</th><th scope="col" class="px-3 py-2 text-left font-sans">상대</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="출전시간">MIN</th><th scope="col" class="px-3 py-2 text-right text-orange-600 dark:text-orange-400" title="득점">PTS</th><th scope="col" class="px-3 py-2 text-right" title="+/-">+/-</th><th scope="col" class="px-3 py-2 text-right" title="리바운드">REB</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="어시스트">AST</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="스틸">STL</th><th scope="col" class="px-3 py-2 text-right hidden sm:table-cell" title="블록">BLK</th></tr></thead><tbody>%s</tbody></table></div>|html></div>|html}
      (player_img_tag ~class_name:"w-14 h-14 border border-slate-300 dark:border-slate-700 shadow-lg" p.id p.name)
      (Uri.pct_encode p.id)
      (escape_html display_name)
      (List.length games)
      (Uri.pct_encode p.id)
      season_options
      include_checked
      quality_chips
      rows) ()

(* Re-export from Views_history for convenient access *)
let player_career_page = Views_history.player_career_page
