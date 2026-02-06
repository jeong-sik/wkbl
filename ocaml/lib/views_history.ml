(** Historical data view functions for WKBL Analytics *)
(** Contains history, legends, coaches, and career pages. *)

open Domain
open Views_common

(** Render multiple teams as a flex row of team logos *)
let multi_team_badges teams_str =
  let teams = String.split_on_char ',' teams_str
    |> List.map String.trim
    |> List.filter (fun s -> s <> "") in
  match teams with
  | [] -> "-"
  | _ ->
    let badges = teams |> List.map (fun t -> team_logo_tag ~class_name:"w-5 h-5" t) |> String.concat " " in
    Printf.sprintf {html|<div class="flex items-center gap-1 flex-wrap">%s</div>|html} badges

let history_page ?(lang=I18n.Ko) (seasons: historical_season list) =
  let rows = seasons |> List.map (fun (s: historical_season) ->
    let champion = match s.hs_champion_team with Some t -> team_badge ~max_width:"max-w-[100px]" t | None -> "-" in
    let runner_up = match s.hs_runner_up with Some t -> team_badge ~max_width:"max-w-[100px]" t | None -> "-" in
    let mvp = match s.hs_regular_mvp with Some p -> escape_html p | None -> "-" in
    let finals_mvp = match s.hs_finals_mvp with Some p -> escape_html p | None -> "-" in
    let roy = match s.hs_rookie_of_year with Some p -> escape_html p | None -> "-" in
    let scoring = match s.hs_scoring_leader with Some p -> escape_html p | None -> "-" in
    Printf.sprintf
      {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-800/30">
        <td class="px-3 py-2 font-bold text-orange-600 dark:text-orange-400">%s</td>
        <td class="px-3 py-2 font-medium">%s</td>
        <td class="px-3 py-2">%s</td>
        <td class="px-3 py-2">%s</td>
        <td class="px-3 py-2">%s</td>
        <td class="px-3 py-2">%s</td>
        <td class="px-3 py-2">%s</td>
      </tr>|html}
      (escape_html s.hs_season_name) champion runner_up mvp finals_mvp roy scoring
  ) |> String.concat "\n" in
  let mobile_cards = seasons |> List.map (fun (s: historical_season) ->
    let champion = match s.hs_champion_team with Some t -> team_badge ~max_width:"max-w-[100px]" t | None -> "-" in
    let mvp = match s.hs_regular_mvp with Some p -> escape_html p | None -> "-" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-4 space-y-2">
        <div class="font-bold text-orange-600 dark:text-orange-400 text-lg">%s</div>
        <div class="grid grid-cols-2 gap-2 text-sm">
          <div><span class="text-slate-500 dark:text-slate-400">우승:</span> <span class="font-medium">%s</span></div>
          <div><span class="text-slate-500 dark:text-slate-400">MVP:</span> <span class="font-medium">%s</span></div>
        </div>
      </div>|html}
      (escape_html s.hs_season_name) champion mvp
  ) |> String.concat "\n" in
  layout ~lang ~title:"역대 기록 | WKBL" ~content:(Printf.sprintf
    {html|<div class="space-y-6">
      <div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">WKBL 역대 기록</h2>
        <p class="text-slate-500 dark:text-slate-400 text-sm">1998년 이후 시즌 우승, MVP 등 주요 기록을 정리했습니다.</p></div>
      <div class="space-y-3 sm:hidden">%s</div>
      <div class="hidden sm:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 table-scroll-container shadow-lg">
        <table class="w-full text-sm table-fixed" aria-label="WKBL 시즌별 역대 기록">
          <colgroup>
            <col style="width: 120px;"> <!-- Season -->
            <col style="width: auto;">  <!-- Champion -->
            <col style="width: auto;">  <!-- Runner-up -->
            <col style="width: 140px;"> <!-- MVP -->
            <col style="width: 140px;"> <!-- Finals MVP -->
            <col style="width: 120px;"> <!-- ROY -->
            <col style="width: 120px;"> <!-- Scoring -->
	          </colgroup>
	          <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase">
	            <tr><th scope="col" class="px-3 py-2 text-left">시즌</th><th scope="col" class="px-3 py-2 text-left">우승</th><th scope="col" class="px-3 py-2 text-left">준우승</th><th scope="col" class="px-3 py-2 text-left" title="정규리그 MVP">MVP</th><th scope="col" class="px-3 py-2 text-left" title="파이널 MVP">파이널 MVP</th><th scope="col" class="px-3 py-2 text-left" title="신인상">신인상</th><th scope="col" class="px-3 py-2 text-left" title="득점 1위">득점왕</th></tr>
	          </thead>
	          <tbody>%s</tbody>
	        </table>
      </div>
    </div>|html}
    mobile_cards rows) ()

(** Legends page - Hall of fame and legendary players *)
let legends_page ?(lang=I18n.Ko) (legends: legend_player list) =
  (* Mobile card view *)
  let cards = legends |> List.map (fun (l: legend_player) ->
    let teams = match l.lp_teams with Some t -> multi_team_badges t | None -> "-" in
    let years = match l.lp_career_years with Some y -> escape_html y | None -> "-" in
    let achievements = match l.lp_notable_achievements with Some a -> escape_html a | None -> "" in
    let hof_badge = if l.lp_is_hall_of_fame then {html|<span class="bg-yellow-500 text-white text-[10px] px-2 py-0.5 rounded-full ml-2">명예의 전당</span>|html} else "" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900/60 border border-slate-200 dark:border-slate-800 rounded-lg p-5 shadow-md hover:shadow-lg transition-shadow">
        <div class="flex items-center justify-between mb-3">
          <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">%s%s</h3>
          <span class="text-slate-500 dark:text-slate-400 text-sm">%s</span>
        </div>
        <div class="text-sm text-slate-600 dark:text-slate-400 mb-3">%s</div>
        <div class="grid grid-cols-3 gap-3 text-center mb-3">
          <div class="bg-slate-100 dark:bg-slate-800 rounded p-2">
            <div class="text-xl font-bold text-orange-600 dark:text-orange-400">%d</div>
            <div class="text-[10px] text-slate-500 dark:text-slate-400">우승</div>
          </div>
          <div class="bg-slate-100 dark:bg-slate-800 rounded p-2">
            <div class="text-xl font-bold text-orange-600 dark:text-orange-400">%d</div>
            <div class="text-[10px] text-slate-500 dark:text-slate-400">MVP</div>
          </div>
          <div class="bg-slate-100 dark:bg-slate-800 rounded p-2">
            <div class="text-xl font-bold text-orange-600 dark:text-orange-400">%d</div>
            <div class="text-[10px] text-slate-500 dark:text-slate-400">올스타</div>
          </div>
        </div>
        <div class="flex justify-around text-xs text-slate-500 dark:text-slate-400 border-t border-slate-200 dark:border-slate-700 pt-3">
          <span>득점: <strong class="text-slate-900 dark:text-slate-200">%d</strong></span>
          <span>리바: <strong class="text-slate-900 dark:text-slate-200">%d</strong></span>
          <span>어시: <strong class="text-slate-900 dark:text-slate-200">%d</strong></span>
        </div>
        %s
      </div>|html}
      (escape_html l.lp_player_name) hof_badge years teams
      l.lp_championships l.lp_mvp_count l.lp_all_star_count
      l.lp_career_points l.lp_career_rebounds l.lp_career_assists
      (if achievements = "" then "" else Printf.sprintf {html|<div class="mt-3 text-xs text-slate-500 dark:text-slate-400 italic">%s</div>|html} achievements)
  ) |> String.concat "\n" in
  (* Desktop table view *)
  let rows = legends |> List.map (fun (l: legend_player) ->
    let teams = match l.lp_teams with Some t -> multi_team_badges t | None -> "-" in
    let years = match l.lp_career_years with Some y -> escape_html y | None -> "-" in
    let hof_badge = if l.lp_is_hall_of_fame then {html|<span class="bg-yellow-500 text-white text-[10px] px-2 py-0.5 rounded-full ml-1">명예의 전당</span>|html} else "" in
    Printf.sprintf
      {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-800/30">
        <td class="px-3 py-2 font-bold whitespace-nowrap">%s%s</td>
        <td class="px-3 py-2 text-slate-600 dark:text-slate-400">%s</td>
        <td class="px-3 py-2 text-slate-600 dark:text-slate-400 text-sm">%s</td>
        <td class="px-3 py-2 text-center font-bold text-orange-600 dark:text-orange-400">%d</td>
        <td class="px-3 py-2 text-center font-bold text-orange-600 dark:text-orange-400">%d</td>
        <td class="px-3 py-2 text-center">%d</td>
        <td class="px-3 py-2 text-center font-mono">%d</td>
        <td class="px-3 py-2 text-center font-mono">%d</td>
        <td class="px-3 py-2 text-center font-mono">%d</td>
      </tr>|html}
      (escape_html l.lp_player_name) hof_badge years teams
      l.lp_championships l.lp_mvp_count l.lp_all_star_count
      l.lp_career_points l.lp_career_rebounds l.lp_career_assists
  ) |> String.concat "\n" in
  layout ~lang ~title:"레전드 | WKBL" ~content:(Printf.sprintf
    {html|<div class="space-y-6">
      <div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">WKBL 레전드</h2>
        <p class="text-slate-500 dark:text-slate-400 text-sm">명예의 전당 및 역대 최고 선수들</p></div>
      <!-- Mobile card view -->
      <div class="grid grid-cols-1 gap-4 md:hidden">%s</div>
      <!-- Desktop table view -->
      <div class="hidden md:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 table-scroll-container shadow-lg">
        <table class="min-w-full text-sm table-fixed" aria-label="WKBL 레전드 선수 목록">
          <colgroup>
            <col style="width: 160px;"> <!-- Player -->
            <col style="width: 120px;"> <!-- Tenure -->
            <col style="width: auto;">  <!-- Teams -->
            <col style="width: 60px;">  <!-- Champ -->
            <col style="width: 60px;">  <!-- MVP -->
            <col style="width: 60px;">  <!-- AllStar -->
            <col style="width: 80px;">  <!-- PTS -->
            <col style="width: 80px;">  <!-- REB -->
            <col style="width: 80px;">  <!-- AST -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase">
            <tr>
              <th scope="col" class="px-3 py-2 text-left">선수</th>
              <th scope="col" class="px-3 py-2 text-left">활동기간</th>
              <th scope="col" class="px-3 py-2 text-left">소속팀</th>
              <th scope="col" class="px-3 py-2 text-center" title="챔피언십 우승 횟수">우승</th>
              <th scope="col" class="px-3 py-2 text-center" title="정규시즌 MVP 수상 횟수">MVP</th>
              <th scope="col" class="px-3 py-2 text-center" title="올스타 선정 횟수">올스타</th>
              <th scope="col" class="px-3 py-2 text-center" title="통산 총 득점">통산득점</th>
              <th scope="col" class="px-3 py-2 text-center" title="통산 총 리바운드">통산리바</th>
              <th scope="col" class="px-3 py-2 text-center" title="통산 총 어시스트">통산어시</th>
            </tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>
    </div>|html}
    cards rows) ()

(** Coaches page - Coaching records *)
let coaches_page ?(lang=I18n.Ko) (coaches: coach list) =
  (* Mobile card view *)
  let cards = coaches |> List.map (fun (c: coach) ->
    let team = match c.c_team with Some t -> escape_html t | None -> "-" in
    let tenure = match c.c_tenure_start, c.c_tenure_end with
      | Some s, Some e -> Printf.sprintf "%d-%d" s e
      | Some s, None -> Printf.sprintf "%d-현재" s
      | _ -> "-" in
    let player_badge = if c.c_former_player then
      match c.c_player_career_years with
      | Some y -> Printf.sprintf {html|<span class="inline-flex items-center gap-1 px-2 py-0.5 rounded-full bg-green-100 dark:bg-green-900/30 text-green-700 dark:text-green-400 text-xs">선수 출신 %s</span>|html} (escape_html y)
      | None -> {html|<span class="inline-flex items-center gap-1 px-2 py-0.5 rounded-full bg-green-100 dark:bg-green-900/30 text-green-700 dark:text-green-400 text-xs">선수 출신</span>|html}
    else "" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 shadow-sm">
        <div class="flex items-start justify-between gap-3 mb-3">
          <div>
            <h3 class="font-bold text-slate-900 dark:text-slate-200">%s</h3>
            <p class="text-sm text-slate-500 dark:text-slate-400">%s · %s</p>
          </div>
          %s
        </div>
        <div class="grid grid-cols-3 gap-3 text-center">
          <div class="bg-slate-50 dark:bg-slate-800/50 rounded-lg p-2">
            <div class="text-xl font-bold text-orange-600 dark:text-orange-400">%d</div>
            <div class="text-xs text-slate-500 dark:text-slate-400">우승</div>
          </div>
          <div class="bg-slate-50 dark:bg-slate-800/50 rounded-lg p-2">
            <div class="text-xl font-bold text-slate-700 dark:text-slate-300">%d</div>
            <div class="text-xs text-slate-500 dark:text-slate-400">정규 W</div>
          </div>
          <div class="bg-slate-50 dark:bg-slate-800/50 rounded-lg p-2">
            <div class="text-xl font-bold text-slate-700 dark:text-slate-300">%d</div>
            <div class="text-xs text-slate-500 dark:text-slate-400">플옵 W</div>
          </div>
        </div>
      </div>|html}
      (escape_html c.c_coach_name) team tenure player_badge c.c_championships c.c_regular_season_wins c.c_playoff_wins
  ) |> String.concat "\n" in
  (* Desktop table view *)
  let rows = coaches |> List.map (fun (c: coach) ->
    let team = match c.c_team with Some t -> escape_html t | None -> "-" in
    let tenure = match c.c_tenure_start, c.c_tenure_end with
      | Some s, Some e -> Printf.sprintf "%d-%d" s e
      | Some s, None -> Printf.sprintf "%d-현재" s
      | _ -> "-" in
    let player_info = if c.c_former_player then
      match c.c_player_career_years with
      | Some y -> Printf.sprintf {html|<span class="text-green-600 dark:text-green-400" title="선수 출신">⚫ %s</span>|html} (escape_html y)
      | None -> {html|<span class="text-green-600 dark:text-green-400">⚫</span>|html}
    else "" in
    Printf.sprintf
      {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-800/30">
        <td class="px-3 py-2 font-bold">%s</td>
        <td class="px-3 py-2">%s</td>
        <td class="px-3 py-2 font-mono text-sm">%s</td>
        <td class="px-3 py-2 text-center font-bold text-orange-600 dark:text-orange-400">%d</td>
        <td class="px-3 py-2 text-center">%d</td>
        <td class="px-3 py-2 text-center">%d</td>
        <td class="px-3 py-2">%s</td>
      </tr>|html}
      (escape_html c.c_coach_name) team tenure c.c_championships c.c_regular_season_wins c.c_playoff_wins player_info
  ) |> String.concat "\n" in
  layout ~lang ~title:"감독 | WKBL" ~content:(Printf.sprintf
    {html|<div class="space-y-6">
      <div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">WKBL 감독</h2>
        <p class="text-slate-500 dark:text-slate-400 text-sm">감독 기록 및 업적</p></div>
      <!-- Mobile card view -->
      <div class="grid gap-4 md:hidden">%s</div>
      <!-- Desktop table view -->
      <div class="hidden md:block bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 table-scroll-container shadow-lg">
        <table class="min-w-full text-sm table-fixed" aria-label="WKBL 감독 기록">
          <colgroup>
            <col style="width: 140px;"> <!-- Coach -->
            <col style="width: auto;">  <!-- Team -->
            <col style="width: 120px;"> <!-- Tenure -->
            <col style="width: 60px;">  <!-- Champ -->
            <col style="width: 80px;">  <!-- Reg W -->
            <col style="width: 80px;">  <!-- Playoff W -->
            <col style="width: 140px;"> <!-- Career -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase">
            <tr><th scope="col" class="px-3 py-2 text-left">감독</th><th scope="col" class="px-3 py-2 text-left">팀</th><th scope="col" class="px-3 py-2 text-left">재임기간</th><th scope="col" class="px-3 py-2 text-center" title="챔피언십 우승 횟수">우승</th><th scope="col" class="px-3 py-2 text-center" title="정규시즌 승리 횟수">정규 W</th><th scope="col" class="px-3 py-2 text-center" title="플레이오프 승리 횟수">플옵 W</th><th scope="col" class="px-3 py-2 text-left">선수경력</th></tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>
    </div>|html}
    cards rows) ()

(** Player career page - Year by year stats for a player *)
let player_career_page ?(lang=I18n.Ko) ~player_name (entries: player_career_entry list) =
  let rows = entries |> List.map (fun (e: player_career_entry) ->
    let jersey = match e.pce_jersey_number with Some n -> string_of_int n | None -> "-" in
    let gp = match e.pce_games_played with Some g -> string_of_int g | None -> "-" in
    let ppg = match e.pce_points_per_game with Some p -> Printf.sprintf "%.1f" p | None -> "-" in
    let rpg = match e.pce_rebounds_per_game with Some r -> Printf.sprintf "%.1f" r | None -> "-" in
    let apg = match e.pce_assists_per_game with Some a -> Printf.sprintf "%.1f" a | None -> "-" in
    let allstar = if e.pce_is_allstar then {html|<span class="text-yellow-500">★</span>|html} else "" in
    let awards = match e.pce_awards with Some a -> escape_html a | None -> "" in
    Printf.sprintf
      {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-800/30">
        <td class="px-3 py-2 font-mono">%s</td>
        <td class="px-3 py-2 font-medium">%s</td>
        <td class="px-3 py-2 text-center">%s</td>
        <td class="px-3 py-2 text-center">%s</td>
        <td class="px-3 py-2 text-center font-bold">%s</td>
        <td class="px-3 py-2 text-center">%s</td>
        <td class="px-3 py-2 text-center">%s</td>
        <td class="px-3 py-2 text-center">%s</td>
        <td class="px-3 py-2 text-sm text-slate-500 dark:text-slate-400">%s</td>
      </tr>|html}
      (escape_html e.pce_season_id) (escape_html e.pce_team) jersey gp ppg rpg apg allstar awards
  ) |> String.concat "\n" in
  layout ~lang ~title:(Printf.sprintf "%s 시즌별 기록 | WKBL" (escape_html player_name)) ~content:(Printf.sprintf
    {html|<div class="space-y-6">
      <div><h2 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h2>
        <p class="text-slate-500 dark:text-slate-400 text-sm">시즌별 기록입니다.</p></div>
      <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 table-scroll-container shadow-lg">
        <table class="min-w-full text-sm font-mono tabular-nums table-fixed" aria-label="선수 시즌별 기록">
          <colgroup>
            <col style="width: 100px;"> <!-- Season -->
            <col style="width: auto;">  <!-- Team -->
            <col style="width: 50px;">  <!-- Jersey -->
            <col style="width: 60px;">  <!-- GP -->
            <col style="width: 60px;">  <!-- PPG -->
            <col style="width: 60px;">  <!-- RPG -->
            <col style="width: 60px;">  <!-- APG -->
            <col style="width: 40px;">  <!-- AllStar -->
            <col style="width: auto;">  <!-- Awards -->
	          </colgroup>
	          <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase">
	            <tr><th scope="col" class="px-3 py-2 text-left">시즌</th><th scope="col" class="px-3 py-2 text-left">팀</th><th scope="col" class="px-3 py-2 text-center" title="등번호">#</th><th scope="col" class="px-3 py-2 text-center" title="출전 경기">GP</th><th scope="col" class="px-3 py-2 text-center" title="경기당 득점">PPG</th><th scope="col" class="px-3 py-2 text-center" title="경기당 리바운드">RPG</th><th scope="col" class="px-3 py-2 text-center" title="경기당 어시스트">APG</th><th scope="col" class="px-3 py-2 text-center" title="올스타 선정">★</th><th scope="col" class="px-3 py-2 text-left">수상</th></tr>
	          </thead>
	          <tbody>%s</tbody>
	        </table>
	      </div>
	      <div class="text-center"><a href="/legends" class="text-orange-500 hover:text-orange-600 dark:text-orange-400 dark:hover:text-orange-300 text-sm">← 레전드 목록</a></div>
	    </div>|html}
    (escape_html player_name) rows) ()

(** Error page *)
