(** Views for Hot Streaks Page *)

open Domain
open Views_common

(** Common helpers to reduce code duplication *)

let status_badge ~is_active =
  if is_active then
    {html|<span class="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400">진행중</span>|html}
  else
    {html|<span class="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-slate-100 text-slate-600 dark:bg-slate-800 dark:text-slate-400">종료</span>|html}

let format_date_range ~start_date ~end_date =
  match end_date with
  | None -> Printf.sprintf "%s ~" start_date
  | Some ed -> Printf.sprintf "%s ~ %s" start_date ed

let team_logo ~size team_name =
  let team_code = team_code_of_string team_name |> Option.value ~default:"" in
  team_logo_tag ~class_name:size team_code

(** Render a single player streak card *)
let streak_card (streak: player_streak) =
  let badge = status_badge ~is_active:streak.ps_is_active in
  let date_range = format_date_range ~start_date:streak.ps_start_date ~end_date:streak.ps_end_date in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 hover:border-orange-300 dark:hover:border-orange-600 transition-colors">
      <div class="flex items-start justify-between gap-2 mb-3">
        <div class="flex items-center gap-2">
          %s
          <div>
            <a href="/player/%s" class="font-medium text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:hover:text-orange-400">%s</a>
            <div class="text-xs text-slate-500 dark:text-slate-400">%s</div>
          </div>
        </div>
        %s
      </div>
      <div class="flex items-center justify-between">
        <div>
          <div class="text-sm text-slate-500 dark:text-slate-400">%s</div>
          <div class="text-3xl font-bold text-orange-600 dark:text-orange-400">%d<span class="text-lg ml-1">경기</span></div>
        </div>
        <div class="text-right text-xs text-slate-400 dark:text-slate-500">%s</div>
      </div>
    </div>|html}
    (team_logo ~size:"w-8 h-8" streak.ps_team_name)
    (Uri.pct_encode streak.ps_player_id)
    (escape_html streak.ps_player_name)
    (escape_html streak.ps_team_name)
    badge
    (escape_html (streak_type_to_label streak.ps_streak_type))
    streak.ps_current_count
    (escape_html date_range)

(** Render team streak card *)
let team_streak_card (streak: team_streak) =
  let badge = status_badge ~is_active:streak.ts_is_active in
  let date_range = format_date_range ~start_date:streak.ts_start_date ~end_date:streak.ts_end_date in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 hover:border-orange-300 dark:hover:border-orange-600 transition-colors">
      <div class="flex items-start justify-between gap-2 mb-3">
        <div class="flex items-center gap-2">
          %s
          <div>
            <a href="/team/%s" class="font-medium text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:hover:text-orange-400">%s</a>
          </div>
        </div>
        %s
      </div>
      <div class="flex items-center justify-between">
        <div>
          <div class="text-sm text-slate-500 dark:text-slate-400">%s</div>
          <div class="text-3xl font-bold text-orange-600 dark:text-orange-400">%d<span class="text-lg ml-1">연승</span></div>
        </div>
        <div class="text-right text-xs text-slate-400 dark:text-slate-500">%s</div>
      </div>
    </div>|html}
    (team_logo ~size:"w-8 h-8" streak.ts_team_name)
    (Uri.pct_encode streak.ts_team_name)
    (escape_html streak.ts_team_name)
    badge
    (escape_html (streak_type_to_label streak.ts_streak_type))
    streak.ts_current_count
    (escape_html date_range)

(** Render streak record row *)
let streak_record_row (record: streak_record) =
  let holder_cell =
    match record.sr_holder_id with
    | Some pid ->
        Printf.sprintf {html|<td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 whitespace-nowrap">
          <div class="flex items-center gap-2">
            %s
            <a href="/player/%s" class="hover:text-orange-600 dark:hover:text-orange-400">%s</a>
          </div>
        </td>|html}
          (team_logo ~size:"w-5 h-5" (Option.value ~default:"" record.sr_team_name))
          (Uri.pct_encode pid)
          (escape_html record.sr_holder_name)
    | None ->
        Printf.sprintf {html|<td class="px-3 py-2 font-medium text-slate-900 dark:text-slate-200 whitespace-nowrap">
          <div class="flex items-center gap-2">
            %s
            <a href="/team/%s" class="hover:text-orange-600 dark:hover:text-orange-400">%s</a>
          </div>
        </td>|html}
          (team_logo ~size:"w-5 h-5" record.sr_holder_name)
          (Uri.pct_encode record.sr_holder_name)
          (escape_html record.sr_holder_name)
  in
  Printf.sprintf
    {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-800/30">
      %s
      <td class="px-3 py-2 text-slate-700 dark:text-slate-300">%s</td>
      <td class="px-3 py-2 text-right text-orange-600 dark:text-orange-400 font-bold">%d</td>
      <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 hidden sm:table-cell">%s</td>
      <td class="px-3 py-2 text-right text-slate-500 dark:text-slate-400 hidden md:table-cell">%s</td>
    </tr>|html}
    holder_cell
    (escape_html (streak_type_to_label record.sr_streak_type))
    record.sr_count
    record.sr_season
    (Printf.sprintf "%s ~ %s" record.sr_start_date record.sr_end_date)

(** Render the streak records table *)
let streak_records_table (records: streak_record list) =
  let rows = records |> List.map streak_record_row |> String.concat "\n" in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto">
      <table class="w-full min-w-max text-sm">
        <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 text-xs uppercase tracking-wider">
          <tr>
            <th class="px-3 py-2 text-left">보유자</th>
            <th class="px-3 py-2 text-left">기록 종류</th>
            <th class="px-3 py-2 text-right">횟수</th>
            <th class="px-3 py-2 text-right hidden sm:table-cell">시즌</th>
            <th class="px-3 py-2 text-right hidden md:table-cell">기간</th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>|html}
    rows

(** Main streaks page *)
let streaks_page
    ~season
    ~seasons
    ~active_player_streaks
    ~active_team_streaks
    ~all_time_records
    () =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html}
      (if season = "ALL" then "selected" else "") base
  in

  (* Active player streaks section *)
  let active_player_section =
    if List.length active_player_streaks = 0 then
      {html|<div class="text-center py-8 text-slate-500 dark:text-slate-400">현재 진행 중인 선수 기록이 없습니다.</div>|html}
    else
      let cards = active_player_streaks |> List.map streak_card |> String.concat "\n" in
      Printf.sprintf {html|<div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">%s</div>|html} cards
  in

  (* Active team streaks section *)
  let active_team_section =
    if List.length active_team_streaks = 0 then
      {html|<div class="text-center py-8 text-slate-500 dark:text-slate-400">현재 진행 중인 팀 연승 기록이 없습니다.</div>|html}
    else
      let cards = active_team_streaks |> List.map team_streak_card |> String.concat "\n" in
      Printf.sprintf {html|<div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">%s</div>|html} cards
  in

  (* All-time records section *)
  let records_section =
    if List.length all_time_records = 0 then
      {html|<div class="text-center py-8 text-slate-500 dark:text-slate-400">기록이 없습니다.</div>|html}
    else
      streak_records_table all_time_records
  in

  layout ~title:"Hot Streaks | WKBL" ~canonical_path:"/streaks"
    ~description:"WKBL 여자농구 핫 스트릭 - 연속 기록 트래커. 20+ 득점, 더블더블, 팀 연승 등 진행 중인 기록과 역대 최고 기록을 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-8">
        <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
          <div>
            <h1 class="text-2xl font-bold text-slate-900 dark:text-slate-200">Hot Streaks</h1>
            <p class="text-slate-500 dark:text-slate-400 text-sm mt-1">연속 기록 트래커 - 선수 및 팀의 핫 스트릭을 확인하세요.</p>
          </div>
          <form class="flex gap-2" hx-get="/streaks" hx-target="body" hx-trigger="change" hx-push-url="true">
            <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-1.5 text-sm focus:border-orange-500 focus:outline-none">%s</select>
          </form>
        </div>

        <!-- Active Team Streaks -->
        <section>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-4 flex items-center gap-2">
            <span class="w-2 h-2 rounded-full bg-green-500"></span>
            진행 중인 팀 연승
          </h2>
          %s
        </section>

        <!-- Active Player Streaks -->
        <section>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-4 flex items-center gap-2">
            <span class="w-2 h-2 rounded-full bg-green-500"></span>
            진행 중인 개인 기록
          </h2>
          %s
        </section>

        <!-- All-Time Records -->
        <section>
          <h2 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-4">역대 최고 기록</h2>
          %s
        </section>

        <!-- Legend -->
        <section class="bg-slate-50 dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800 p-4">
          <h3 class="text-sm font-bold text-slate-700 dark:text-slate-300 mb-3">기록 종류 안내</h3>
          <div class="grid grid-cols-2 md:grid-cols-4 gap-3 text-xs text-slate-600 dark:text-slate-400">
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">20+</span>득점 20점 이상</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">DD</span>더블더블 (2개 부문 10+)</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">TD</span>트리플더블 (3개 부문 10+)</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">10R</span>리바운드 10개 이상</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">7A</span>어시스트 7개 이상</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">3B</span>블록 3개 이상</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">3S</span>스틸 3개 이상</div>
            <div><span class="font-mono bg-slate-200 dark:bg-slate-700 px-1.5 py-0.5 rounded mr-2">W</span>팀 연승</div>
          </div>
        </section>
      </div>|html}
      season_options
      active_team_section
      active_player_section
      records_section) ()
