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
      | _ -> {|<span class="w-6 h-6 flex items-center justify-center rounded-full bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-400 text-xs font-bold">3</span>|}
    in
    Printf.sprintf
      {html|<div class="flex items-center gap-3 py-2 %s">
        %s
        <a href="%s" class="flex-1 min-w-0">
          <div class="font-medium text-sm text-slate-900 dark:text-slate-200 truncate">%s</div>
          <div class="text-xs text-slate-500 dark:text-slate-400">%s</div>
        </a>
        <div class="text-lg font-bold tabular-nums font-mono text-slate-900 dark:text-slate-100">%s</div>
      </div>|html}
      (if i < List.length top3 - 1 then "border-b border-slate-100 dark:border-slate-800/60" else "")
      rank_badge
      (player_href b.lb_player_id)
      (escape_html b.lb_player_name)
      (Printf.sprintf {|<a href="%s" class="hover:underline">%s</a>|} (team_href b.lb_team_name) (escape_html (normalize_name b.lb_team_name)))
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
  let cols = [
    col ~align:`Center ~w:(px 40) "#";
    col ~sticky:true ~w:(px 150) "팀";
    col ~align:`Center ~title:"경기수" "G";
    col ~align:`Center ~title:"승" "W";
    col ~align:`Center ~title:"패" "L";
    col ~align:`Center ~highlight:true ~title:"승률" "PCT";
    col ~align:`Center ~title:"게임차" "GB";
    col ~align:`Right ~title:"경기당 득점" "PS/G";
    col ~align:`Right ~title:"경기당 실점" "PA/G";
    col ~align:`Right ~title:"득실차" "DIFF";
  ] in
  let rows = standings |> List.mapi (fun i (s: team_standing) ->
    let pct_str = Printf.sprintf ".%03d" (int_of_float (s.win_pct *. 1000.0)) in
    let gb_str = if s.gb = 0.0 then "-" else Printf.sprintf "%.1f" s.gb in
    let diff_class = if s.diff > 0.0 then "text-green-600 dark:text-green-400"
      else if s.diff < 0.0 then "text-red-500 dark:text-red-400"
      else "text-slate-500" in
    let diff_str = if s.diff > 0.0 then Printf.sprintf "+%.1f" s.diff
      else Printf.sprintf "%.1f" s.diff in
    let diff_html = Printf.sprintf {html|<span class="%s">%s</span>|html} diff_class diff_str in
    [ string_of_int (i + 1);
      team_badge s.team_name;
      string_of_int s.games_played;
      string_of_int s.wins;
      string_of_int s.losses;
      pct_str;
      gb_str;
      format_float s.avg_pts;
      format_float s.avg_opp_pts;
      diff_html ]
  ) in
  render_fixed_table ~striped:true
    ~aria_label:"시즌 순위표"
    ~id:"season-standings"
    ~min_width:"min-w-[700px]" ~cols rows

(** Team stats comparison table — all teams side-by-side for a season *)
let team_stats_comparison_table ~(scope_label: string) (stats: team_stats list) =
  if stats = [] then "" else
  let f = format_float in
  let cols = [
    col ~sticky:true ~w:(px 150) "팀";
    col ~align:`Center ~title:"경기수" "G";
    col ~align:`Right ~highlight:true ~title:"득점" "PTS";
    col ~align:`Right ~title:"리바운드" "REB";
    col ~align:`Right ~title:"어시스트" "AST";
    col ~align:`Right ~title:"스틸" "STL";
    col ~align:`Right ~title:"블록" "BLK";
    col ~align:`Right ~title:"턴오버" "TOV";
    col ~align:`Right ~title:"야투 성공률" "FG%";
    col ~align:`Right ~title:"3점 성공률" "3P%";
    col ~align:`Right ~title:"자유투 성공률" "FT%";
    col ~align:`Right ~resp:`Hidden_sm ~title:"유효 야투율" "eFG%";
    col ~align:`Right ~resp:`Hidden_sm ~title:"진정 슈팅률" "TS%";
  ] in
  let rows = stats |> List.map (fun (s: team_stats) ->
    [ team_badge s.team;
      string_of_int s.gp; f s.pts; f s.reb; f s.ast;
      f s.stl; f s.blk; f s.turnovers;
      f s.fg_pct; f s.fg3_pct; f s.ft_pct; f s.efg_pct; f s.ts_pct ]
  ) in
  render_fixed_table ~striped:true
    ~aria_label:(scope_label ^ " 팀 스탯 비교")
    ~id:("team-stats-" ^ String.lowercase_ascii scope_label)
    ~min_width:"min-w-[900px]" ~cols rows

(** Main season summary page *)
let season_summary_page ?(lang=I18n.Ko) ~season ~seasons
    ~(standings: team_standing list)
    ~(leaders: Db.leader_base list)
    ~(histories: historical_season list)
    ~(team_per_game: team_stats list)
    ~(team_totals: team_stats list) () =
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
      | Some s -> Printf.sprintf {html|<a href="%s" class="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-sm font-medium text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 hover:bg-slate-100 dark:hover:bg-slate-800/60 transition-colors" title="%s">← %s</a>|html} (season_href s.code) (escape_html s.name) (escape_html s.name)
      | None -> {|<span></span>|}
    in
    let next_link = match next_season with
      | Some s -> Printf.sprintf {html|<a href="%s" class="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-lg text-sm font-medium text-slate-600 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 hover:bg-slate-100 dark:hover:bg-slate-800/60 transition-colors" title="%s">%s →</a>|html} (season_href s.code) (escape_html s.name) (escape_html s.name)
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
    let bpg_card = leader_card ~title:"블록" ~icon:"🚫" leaders
      ~extract:(per_game_f (fun b -> b.lb_blk)) ~format_val:fmt_f1 in
    Printf.sprintf
      {html|<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-5 gap-4">%s%s%s%s%s</div>|html}
      ppg_card rpg_card apg_card spg_card bpg_card
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
        <div>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-3">팀 Per Game 스탯</h2>
          %s
        </div>
        <div>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-3">팀 시즌 합계</h2>
          %s
        </div>
        %s
      </div>|html}
      (breadcrumb [("홈", "/"); ("역대 기록", "/history"); (escape_html season_name, season_href season)])
      (escape_html season_name)
      nav_html
      (awards_hero hist)
      leaders_html
      (summary_standings_table standings)
      (team_stats_comparison_table ~scope_label:"Per Game" team_per_game)
      (team_stats_comparison_table ~scope_label:"시즌 합계" team_totals)
      nav_html
    ) ()
