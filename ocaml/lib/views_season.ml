(** Season summary page — Basketball Reference style season overview *)

open Domain
open Views_common

(** Find current season's historical record by matching season code *)
let find_history ~season (histories: historical_season list) =
  List.find_opt (fun (h: historical_season) -> h.hs_season_id = season) histories

(** Prev/Next season navigation helper *)
let season_nav ~current (seasons: season_info list) =
  let sorted = List.sort (fun a b -> String.compare a.code b.code) seasons in
  let rec scan = function
    | [] -> (None, None)
    | [s] -> if s.code = current then (None, None) else (None, None)
    | a :: b :: rest ->
        if b.code = current then
          let next = match rest with c :: _ -> Some c | [] -> None in
          (Some a, next)
        else scan (b :: rest)
  in
  (* Handle first element *)
  match sorted with
  | [] -> (None, None)
  | first :: _ when first.code = current ->
      (None, (match List.tl sorted with s :: _ -> Some s | [] -> None))
  | _ -> scan sorted

(** Stat leader card — shows top 3 for a category *)
let leader_card ~title ~icon (leaders: Db.leader_base list) ~extract ~format_val =
  let sorted = leaders
    |> List.filter (fun (b: Db.leader_base) -> b.lb_gp >= 10)
    |> List.sort (fun a b -> compare (extract b) (extract a))
  in
  let top3 = match sorted with
    | a :: b :: c :: _ -> [a; b; c]
    | shorter -> shorter
  in
  let rows = top3 |> List.mapi (fun i (b: Db.leader_base) ->
    let rank_badge = match i with
      | 0 -> {|<span class="w-6 h-6 flex items-center justify-center rounded-full bg-amber-100 dark:bg-amber-900/40 text-amber-700 dark:text-amber-400 text-xs font-bold">1</span>|}
      | 1 -> {|<span class="w-6 h-6 flex items-center justify-center rounded-full bg-slate-200 dark:bg-slate-700 text-slate-600 dark:text-slate-300 text-xs font-bold">2</span>|}
      | _ -> {|<span class="w-6 h-6 flex items-center justify-center rounded-full bg-orange-100 dark:bg-orange-900/30 text-orange-600 dark:text-orange-400 text-xs font-bold">3</span>|}
    in
    Printf.sprintf
      {html|<div class="flex items-center gap-3 py-2 %s">
        %s
        <a href="/player/%s" class="flex-1 min-w-0">
          <div class="font-medium text-sm text-slate-900 dark:text-slate-200 truncate">%s</div>
          <div class="text-xs text-slate-500 dark:text-slate-400">%s</div>
        </a>
        <div class="text-lg font-bold tabular-nums font-mono text-slate-900 dark:text-slate-100">%s</div>
      </div>|html}
      (if i < List.length top3 - 1 then "border-b border-slate-100 dark:border-slate-800/60" else "")
      rank_badge
      (Uri.pct_encode b.lb_player_id)
      (escape_html b.lb_player_name)
      (escape_html (normalize_name b.lb_team_name))
      (format_val (extract b))
  ) |> String.concat "\n" in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900/80 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-sm">
      <div class="flex items-center gap-2 mb-3">
        <span class="text-lg">%s</span>
        <h3 class="font-bold text-sm text-slate-700 dark:text-slate-300 uppercase tracking-wide">%s</h3>
      </div>
      %s
    </div>|html}
    icon title rows

(** Champion & awards hero section *)
let awards_hero (hist: historical_season option) =
  match hist with
  | None ->
    {|<div class="bg-slate-50 dark:bg-slate-800/40 rounded-xl p-6 text-center text-slate-500 dark:text-slate-400 text-sm">이 시즌의 수상 기록이 없습니다.</div>|}
  | Some h ->
    let champion_html = match h.hs_champion_team with
      | Some t ->
        Printf.sprintf
          {html|<div class="flex items-center gap-3">
            %s
            <div>
              <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wide">우승</div>
              <div class="font-bold text-lg text-slate-900 dark:text-slate-100">%s</div>
            </div>
          </div>|html}
          (team_logo_tag ~class_name:"w-12 h-12" t) (team_badge t)
      | None -> ""
    in
    let runner_html = match h.hs_runner_up with
      | Some t ->
        Printf.sprintf
          {html|<div class="flex items-center gap-3">
            %s
            <div>
              <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wide">준우승</div>
              <div class="font-medium text-slate-700 dark:text-slate-300">%s</div>
            </div>
          </div>|html}
          (team_logo_tag ~class_name:"w-10 h-10" t) (team_badge t)
      | None -> ""
    in
    let award_pill label value =
      match value with
      | None -> ""
      | Some v ->
        Printf.sprintf
          {html|<div class="flex items-center gap-2 px-3 py-2 rounded-lg bg-slate-50 dark:bg-slate-800/60">
            <span class="text-xs text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</span>
            <span class="font-medium text-sm text-slate-900 dark:text-slate-200">%s</span>
          </div>|html}
          label (escape_html v)
    in
    let awards = [
      award_pill "MVP" h.hs_regular_mvp;
      award_pill "파이널 MVP" h.hs_finals_mvp;
      award_pill "신인상" h.hs_rookie_of_year;
      award_pill "득점왕" h.hs_scoring_leader;
    ] |> List.filter (fun s -> s <> "") |> String.concat "\n" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900/80 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm">
        <div class="flex flex-col sm:flex-row sm:items-center gap-6 mb-4">
          %s
          %s
        </div>
        <div class="flex flex-wrap gap-2">%s</div>
      </div>|html}
      champion_html runner_html awards

(** Standings table for season summary *)
let summary_standings_table (standings: team_standing list) =
  let rows = standings |> List.mapi (fun i (s: team_standing) ->
    let row_class = if i mod 2 = 0
      then "bg-white dark:bg-slate-900/60"
      else "bg-slate-50/50 dark:bg-slate-800/20" in
    let diff_class = if s.diff > 0.0 then "text-green-600 dark:text-green-400"
      else if s.diff < 0.0 then "text-red-500 dark:text-red-400"
      else "text-slate-500" in
    let diff_str = if s.diff > 0.0 then Printf.sprintf "+%.1f" s.diff
      else Printf.sprintf "%.1f" s.diff in
    let gb_str = if s.gb = 0.0 then "-" else Printf.sprintf "%.1f" s.gb in
    Printf.sprintf
      {html|<tr class="%s border-b border-slate-100 dark:border-slate-800/40 hover:bg-slate-50 dark:hover:bg-slate-800/30">
        <td class="px-3 py-2 text-center font-bold text-slate-400 dark:text-slate-500 text-sm">%d</td>
        <td class="px-3 py-2 font-medium">%s</td>
        <td class="px-3 py-2 text-center tabular-nums font-mono">%d</td>
        <td class="px-3 py-2 text-center tabular-nums font-mono">%d</td>
        <td class="px-3 py-2 text-center tabular-nums font-mono">%d</td>
        <td class="px-3 py-2 text-center tabular-nums font-mono font-bold">.%03d</td>
        <td class="px-3 py-2 text-center tabular-nums font-mono text-slate-500 dark:text-slate-400">%s</td>
        <td class="px-3 py-2 text-right tabular-nums font-mono">%.1f</td>
        <td class="px-3 py-2 text-right tabular-nums font-mono">%.1f</td>
        <td class="px-3 py-2 text-right tabular-nums font-mono %s">%s</td>
      </tr>|html}
      row_class
      (i + 1)
      (team_badge s.team_name)
      s.games_played s.wins s.losses
      (int_of_float (s.win_pct *. 1000.0))
      gb_str
      s.avg_pts s.avg_opp_pts
      diff_class diff_str
  ) |> String.concat "\n" in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900/80 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-sm">
      <table class="w-full text-sm tabular-nums" aria-label="시즌 순위표">
        <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase">
          <tr>
            <th scope="col" class="px-3 py-2 text-center w-10">#</th>
            <th scope="col" class="px-3 py-2 text-left">팀</th>
            <th scope="col" class="px-3 py-2 text-center" title="경기수">G</th>
            <th scope="col" class="px-3 py-2 text-center" title="승">W</th>
            <th scope="col" class="px-3 py-2 text-center" title="패">L</th>
            <th scope="col" class="px-3 py-2 text-center" title="승률">PCT</th>
            <th scope="col" class="px-3 py-2 text-center" title="게임차">GB</th>
            <th scope="col" class="px-3 py-2 text-right" title="경기당 득점">PS/G</th>
            <th scope="col" class="px-3 py-2 text-right" title="경기당 실점">PA/G</th>
            <th scope="col" class="px-3 py-2 text-right" title="득실차">DIFF</th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>|html}
    rows

(** Main season summary page *)
let season_summary_page ?(lang=I18n.Ko) ~season ~seasons
    ~(standings: team_standing list)
    ~(leaders: Db.leader_base list)
    ~(histories: historical_season list) () =
  let season_name =
    match List.find_opt (fun (s: season_info) -> s.code = season) seasons with
    | Some s -> s.name
    | None -> season
  in
  let hist = find_history ~season histories in
  let (prev_season, next_season) = season_nav ~current:season seasons in

  (* Season navigation arrows *)
  let nav_html =
    let prev_link = match prev_season with
      | Some s -> Printf.sprintf {html|<a href="/season/%s" class="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-sm font-medium text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 hover:bg-slate-100 dark:hover:bg-slate-800/60 transition-colors" title="%s">← %s</a>|html} s.code (escape_html s.name) (escape_html s.name)
      | None -> {|<span></span>|}
    in
    let next_link = match next_season with
      | Some s -> Printf.sprintf {html|<a href="/season/%s" class="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-sm font-medium text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 hover:bg-slate-100 dark:hover:bg-slate-800/60 transition-colors" title="%s">%s →</a>|html} s.code (escape_html s.name) (escape_html s.name)
      | None -> {|<span></span>|}
    in
    Printf.sprintf
      {html|<div class="flex items-center justify-between">%s%s</div>|html}
      prev_link next_link
  in

  (* Stat leader cards *)
  let fmt_f1 v = Printf.sprintf "%.1f" v in
  let fmt_f2 v = Printf.sprintf "%.1f" v in
  let per_game_f field (b: Db.leader_base) =
    if b.lb_gp = 0 then 0.0 else (float_of_int (field b)) /. float_of_int b.lb_gp
  in
  let leaders_html =
    let ppg_card = leader_card ~title:"득점" ~icon:"🏀" leaders
      ~extract:(per_game_f (fun b -> b.lb_pts)) ~format_val:fmt_f1 in
    let rpg_card = leader_card ~title:"리바운드" ~icon:"📊" leaders
      ~extract:(per_game_f (fun b -> b.lb_reb)) ~format_val:fmt_f2 in
    let apg_card = leader_card ~title:"어시스트" ~icon:"🎯" leaders
      ~extract:(per_game_f (fun b -> b.lb_ast)) ~format_val:fmt_f1 in
    let spg_card = leader_card ~title:"스틸" ~icon:"🛡️" leaders
      ~extract:(per_game_f (fun b -> b.lb_stl)) ~format_val:fmt_f1 in
    Printf.sprintf
      {html|<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">%s%s%s%s</div>|html}
      ppg_card rpg_card apg_card spg_card
  in

  let page_title = Printf.sprintf "%s 시즌 요약 | WKBL" (escape_html season_name) in
  let page_desc = Printf.sprintf "%s WKBL 여자농구 시즌 요약 - 순위, 수상, 개인 리더 정보" (escape_html season_name) in

  (* JSON-LD structured data *)
  let json_ld_data =
    let champion_str = match hist with
      | Some h -> (match h.hs_champion_team with
        | Some t -> Printf.sprintf {|,"subEvent":{"@type":"SportsEvent","name":"%s 우승","winner":{"@type":"SportsTeam","name":"%s"}}|} (escape_html season_name) (escape_html t)
        | None -> "")
      | None -> ""
    in
    Printf.sprintf {|{"@context":"https://schema.org","@type":"SportsEvent","name":"%s WKBL 여자프로농구","sport":"Basketball","organizer":{"@type":"SportsOrganization","name":"WKBL"},"url":"https://wkbl.win/season/%s"%s}|}
      (escape_html season_name) (Uri.pct_encode season) champion_str
  in

  layout ~lang ~title:page_title
    ~canonical_path:(Printf.sprintf "/season/%s" season)
    ~description:page_desc
    ~json_ld:json_ld_data
    ~content:(Printf.sprintf
      {html|<div class="space-y-6">
        %s
        <div>
          <h1 class="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-slate-100">%s</h1>
          <p class="text-slate-500 dark:text-slate-400 text-sm mt-1">WKBL 여자프로농구 시즌 요약</p>
        </div>
        %s
        %s
        <div>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-3">개인 기록 리더</h2>
          %s
        </div>
        <div>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-3">순위표</h2>
          %s
        </div>
        %s
      </div>|html}
      (breadcrumb [("홈", "/"); ("역대 기록", "/history"); (escape_html season_name, "")])
      (escape_html season_name)
      nav_html
      (awards_hero hist)
      leaders_html
      (summary_standings_table standings)
      nav_html
    ) ()
