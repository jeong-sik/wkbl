(** Data tools view functions for WKBL Analytics *)
(** Contains QA dashboard and transactions pages. *)

open Domain
open Views_common

let qa_dashboard_page (report: Db.qa_db_report) ?(markdown=None) () =
  let int_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%d</div>|html} v
  in
  let pct_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%.1f<span class="text-base text-slate-500 dark:text-slate-400">%%</span></div>|html} v
  in
  let kpi_card ~label ~value_html ~hint_html =
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><div class="text-slate-500 dark:text-slate-400 text-[11px] uppercase tracking-widest font-bold">%s</div><div class="mt-2">%s</div><div class="mt-2 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div></div>|html}
      (escape_html label)
      value_html
      hint_html
  in
  let score_or_dash = function | None -> "-" | Some v -> string_of_int v in
  let delta_or_dash a b =
    match (a, b) with
    | Some stored, Some summed -> Printf.sprintf "%+d" (summed - stored)
    | _ -> "-"
  in
  let mismatch_rows =
    report.qdr_score_mismatch_sample
    |> List.map (fun (row: Db.qa_score_mismatch) ->
      let stored = Printf.sprintf "%s - %s" (score_or_dash row.qsm_home_score) (score_or_dash row.qsm_away_score) in
      let summed = Printf.sprintf "%s - %s" (score_or_dash row.qsm_home_sum) (score_or_dash row.qsm_away_sum) in
      let home_delta = delta_or_dash row.qsm_home_score row.qsm_home_sum in
      let away_delta = delta_or_dash row.qsm_away_score row.qsm_away_sum in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs whitespace-nowrap">%s</td><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/boxscore/%s">%s</a></td><td class="px-3 py-2 text-slate-700 dark:text-slate-300 text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-500 dark:text-slate-400 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-500 dark:text-slate-400 tabular-nums">%s</td></tr>|html}
        (escape_html row.qsm_game_date)
        (Uri.pct_encode row.qsm_game_id)
        (escape_html row.qsm_game_id)
        (escape_html (row.qsm_home_team ^ " vs " ^ row.qsm_away_team))
        (escape_html stored)
        (escape_html summed)
        (escape_html home_delta)
        (escape_html away_delta))
    |> String.concat "\n"
  in
  let team_count_rows =
    report.qdr_team_count_anomaly_sample
    |> List.map (fun (row: Db.qa_team_count_anomaly) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/boxscore/%s">%s</a></td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td></tr>|html}
        (Uri.pct_encode row.qtca_game_id)
        (escape_html row.qtca_game_id)
        row.qtca_team_count)
    |> String.concat "\n"
  in
  let dup_row_rows =
    report.qdr_duplicate_player_row_sample
    |> List.map (fun (row: Db.qa_duplicate_player_row) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/boxscore/%s">%s</a></td><td class="px-3 py-2 text-xs">%s</td><td class="px-3 py-2 text-xs"><a class="hover:text-orange-600 dark:text-orange-400" href="/player/%s">%s</a></td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td></tr>|html}
        (Uri.pct_encode row.qdpr_game_id)
        (escape_html row.qdpr_game_id)
        (team_badge row.qdpr_team_name)
        (escape_html row.qdpr_player_id)
        (escape_html row.qdpr_player_name)
        row.qdpr_row_count)
    |> String.concat "\n"
  in
  let dup_name_rows =
    report.qdr_duplicate_player_name_sample
    |> List.map (fun (row: Db.qa_duplicate_player_name) ->
      let ids =
        row.qdpn_player_ids
        |> List.map (fun id -> Printf.sprintf {html|<a href="/player/%s" class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400">%s</a>|html} (escape_html id) (escape_html id))
        |> String.concat " "
      in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td><td class="px-3 py-2">%s</td></tr>|html}
        (escape_html row.qdpn_player_name)
        row.qdpn_id_count
        ids)
    |> String.concat "\n"
  in
  let markdown_block =
    match markdown with
    | None -> ""
    | Some md ->
        Printf.sprintf
          {html|<details class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><summary class="cursor-pointer select-none text-slate-700 dark:text-slate-300 font-bold">Legacy QA (Markdown)</summary><pre class="mt-4 text-[11px] text-slate-700 dark:text-slate-300 whitespace-pre-wrap break-words">%s</pre></details>|html}
          (escape_html md)
  in
  let sources_block =
    {html|<details class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 text-xs text-slate-500 dark:text-slate-400">
      <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">출처 / 검증 기준</summary>
      <div class="mt-2 space-y-1 leading-relaxed">
        <div class="text-slate-500 dark:text-slate-400 font-bold">출처 (공식)</div>
        <div>• 경기 결과/스코어: <a href="https://www.wkbl.or.kr/game/result.asp" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:underline">wkbl.or.kr/game/result.asp</a></div>
        <div>• 박스스코어: <span class="font-mono text-slate-900 dark:text-slate-200">/game/ajax/ajax_game_result_2.asp</span> (POST) → <span class="font-mono text-slate-900 dark:text-slate-200">game_stats</span></div>
        <div>• PBP +/-: <a href="https://www.wkbl.or.kr/live11/path_live_sms.asp" target="_blank" rel="noreferrer" class="text-orange-600 dark:text-orange-400 hover:underline">wkbl.or.kr/live11/path_live_sms.asp</a> → <span class="font-mono text-slate-900 dark:text-slate-200">player_plus_minus</span></div>
        <div class="pt-1 text-slate-500 dark:text-slate-400 font-bold">검증 기준 (스코어 교차검증)</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">VERIFIED (✓)</span>: <span class="font-mono text-slate-900 dark:text-slate-200">games.home/away_score</span>가 존재하고, 양 팀 <span class="font-mono text-slate-900 dark:text-slate-200">SUM(game_stats.pts)</span>와 모두 일치</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">DERIVED (Σ)</span>: 스코어/합계 누락으로 교차검증 불가. 표시 스코어는 <span class="font-mono text-slate-900 dark:text-slate-200">COALESCE(games score, sum pts)</span></div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">MISMATCH (!)</span>: 스코어와 합계가 모두 있는데 값이 다름</div>
        <div class="pt-1">※ 이 검증은 “최종 득점”만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 검증이 필요합니다.</div>
      </div>
    </details>|html}
  in
  layout ~title:"QA | WKBL"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in"><div class="flex flex-col gap-2"><h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Data Quality (QA)</h2><div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">기록 신뢰도를 위해 <span class="font-mono text-slate-900 dark:text-slate-200">스코어 불일치</span>, <span class="font-mono text-slate-900 dark:text-slate-200">팀 수 이상</span>, <span class="font-mono text-slate-900 dark:text-slate-200">중복 스탯 row</span>, <span class="font-mono text-slate-900 dark:text-slate-200">중복 선수 ID</span>를 점검합니다. (Generated: <span class="font-mono">%s</span>)</div></div>%s<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">%s%s%s%s</div><div class="grid grid-cols-1 lg:grid-cols-3 gap-4">%s%s%s</div><div class="grid grid-cols-1 gap-4"><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Score Mismatch</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed"><span class="font-mono text-slate-900 dark:text-slate-200">games.home/away_score</span> vs <span class="font-mono text-slate-900 dark:text-slate-200">SUM(game_stats.pts)</span> 비교</div><div class="mt-4 overflow-x-auto"><table class="min-w-[860px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left w-[90px]">Date</th><th class="px-3 py-2 text-left w-[120px]">Game</th><th class="px-3 py-2 text-left">Matchup</th><th class="px-3 py-2 text-right w-[120px]">Stored</th><th class="px-3 py-2 text-right w-[120px]">Summed</th><th class="px-3 py-2 text-right w-[80px]">HΔ</th><th class="px-3 py-2 text-right w-[80px]">AΔ</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Team Count Anomaly</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">한 경기에서 <span class="font-mono text-slate-900 dark:text-slate-200">game_stats.team_code</span>가 2개가 아닌 케이스</div><div class="mt-4 overflow-x-auto"><table class="min-w-[320px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left">Game</th><th class="px-3 py-2 text-right w-[90px]">Teams</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Duplicate Player Rows</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed"><span class="font-mono text-slate-900 dark:text-slate-200">(game_id, team_code, player_id)</span> 중복</div><div class="mt-4 overflow-x-auto"><table class="min-w-[720px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left w-[120px]">Game</th><th class="px-3 py-2 text-left w-[160px]">Team</th><th class="px-3 py-2 text-left">Player</th><th class="px-3 py-2 text-right w-[80px]">Rows</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Duplicate Player Names</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">count=%d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">동일 이름으로 <span class="font-mono text-slate-900 dark:text-slate-200">player_id</span>가 여러 개인 케이스</div><div class="mt-4 overflow-x-auto"><table class="min-w-[720px] w-full text-sm font-mono table-fixed"><thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th class="px-3 py-2 text-left">Name</th><th class="px-3 py-2 text-right w-[80px]">IDs</th><th class="px-3 py-2 text-left">player_id</th></tr></thead><tbody>%s</tbody></table></div></div></div>%s</div>|html}
      (escape_html report.qdr_generated_at)
      sources_block
      (kpi_card ~label:"Games" ~value_html:(int_chip report.qdr_games_total) ~hint_html:"전체 경기 수(정규/PO, 시범 제외)")
      (kpi_card ~label:"Games w/ Stats" ~value_html:(int_chip report.qdr_games_with_stats) ~hint_html:"game_stats가 존재하는 경기")
      (kpi_card ~label:"PBP +/- Coverage" ~value_html:(pct_chip report.qdr_plus_minus_coverage_pct) ~hint_html:(Printf.sprintf "PBP 기반 +/-가 있는 경기: %d" report.qdr_plus_minus_games))
      (kpi_card ~label:"Generated" ~value_html:(Printf.sprintf {html|<div class="text-sm font-mono text-slate-900 dark:text-slate-200 break-all">%s</div>|html} (escape_html report.qdr_generated_at)) ~hint_html:"UTC 기준")
      (kpi_card ~label:"Score Mismatch" ~value_html:(int_chip report.qdr_score_mismatch_count) ~hint_html:"최종 스코어 vs 합계 불일치")
      (kpi_card ~label:"Team Count != 2" ~value_html:(int_chip report.qdr_team_count_anomaly_count) ~hint_html:"한 경기 팀 수가 2가 아님")
      (kpi_card ~label:"Dup Player Rows" ~value_html:(int_chip report.qdr_duplicate_player_row_count) ~hint_html:"중복으로 라인이 2개 뜨는 원인")
      report.qdr_score_mismatch_count
      mismatch_rows
      report.qdr_team_count_anomaly_count
      team_count_rows
      report.qdr_duplicate_player_row_count
      dup_row_rows
      report.qdr_duplicate_player_name_count
      dup_name_rows
      markdown_block) ()

(** Draft / Trade (official) page *)
let transactions_page
  ~tab
  ~year
  ~q
  ~draft_years
  ~trade_years
  ~(draft_picks: draft_pick_row list)
  ~(trade_events: official_trade_event list)
  =
  let tab_value = tab |> String.trim |> String.lowercase_ascii in
  let active_tab = if tab_value = "trade" then "trade" else "draft" in
  let tab_link t label =
    let cls =
      if active_tab = t then
        "bg-slate-100 dark:bg-slate-800/80 border-slate-300 dark:border-slate-700 text-slate-900 dark:text-slate-200"
      else
        "bg-white dark:bg-slate-900/40 border-slate-200 dark:border-slate-800 text-slate-500 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white dark:text-slate-200 hover:bg-slate-100 dark:bg-slate-800/50"
    in
    Printf.sprintf
      {html|<a href="/transactions?tab=%s" class="px-3 py-2 rounded-lg border text-xs font-bold uppercase tracking-widest transition %s">%s</a>|html}
      (escape_html t)
      cls
      (escape_html label)
  in
  let year_options years =
    let all_selected = if year = 0 then "selected" else "" in
    let base =
      years
      |> List.map (fun y ->
          let selected = if y = year then "selected" else "" in
          Printf.sprintf {html|<option value="%d" %s>%d</option>|html} y selected y)
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="0" %s>All</option>%s|html} all_selected base
  in
  let active_years = if active_tab = "trade" then trade_years else draft_years in
  let filter_form =
    Printf.sprintf
      {html|<form method="get" action="/transactions" class="flex flex-col sm:flex-row sm:items-end gap-3">
  <input type="hidden" name="tab" value="%s">
  <label class="block text-xs text-slate-500 dark:text-slate-400 space-y-1">
    <div class="font-bold uppercase tracking-widest text-[10px]">Year</div>
    <select name="year" class="bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none">
      %s
    </select>
  </label>
  <label class="block text-xs text-slate-500 dark:text-slate-400 space-y-1 min-w-0 flex-1">
    <div class="font-bold uppercase tracking-widest text-[10px]">Search</div>
    <input name="q" value="%s" placeholder="player / team / keyword" class="w-full bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none">
  </label>
  <button class="shrink-0 bg-orange-600 hover:bg-orange-500 text-slate-900 dark:text-slate-200 font-bold rounded-lg px-4 py-2 text-sm transition">Apply</button>
</form>|html}
      (escape_html active_tab)
      (year_options active_years)
      (escape_html q)
  in
  let draft_table =
    let pick_label (row: draft_pick_row) =
      match row.dpr_draft_round, row.dpr_pick_in_round, row.dpr_overall_pick with
      | Some r, Some p, Some o -> Printf.sprintf "R%d #%d (O%d)" r p o
      | Some r, Some p, None -> Printf.sprintf "R%d #%d" r p
      | Some r, None, Some o -> Printf.sprintf "R%d (O%d)" r o
      | _, _, _ -> "-"
    in
    let rows =
      match draft_picks with
      | [] ->
          {html|<tr><td colspan="6" class="px-4 py-10 text-center text-slate-500 dark:text-slate-400 text-sm">No draft rows found. (Build with <span class="font-mono text-slate-700 dark:text-slate-300">WKBL_SYNC_DRAFT_TRADE=1</span> or run <span class="font-mono text-slate-700 dark:text-slate-300">scripts/wkbl_draft_trade_sync.py</span>)</td></tr>|html}
      | xs ->
          xs
          |> List.map (fun (r: draft_pick_row) ->
              let y =
                match r.dpr_draft_year with
                | Some v -> string_of_int v
                | None -> "-"
              in
              let team_html =
                match r.dpr_draft_team with
                | None -> {html|<span class="text-slate-500 dark:text-slate-400">-</span>|html}
                | Some t -> team_badge t
              in
              Printf.sprintf
                {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:bg-slate-800/30 transition-colors">
  <td class="px-4 py-3 text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap w-[72px]">%s</td>
  <td class="px-4 py-3 text-slate-700 dark:text-slate-300 font-mono whitespace-nowrap w-[140px]">%s</td>
  <td class="px-4 py-3 min-w-0"><a class="text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:text-orange-400 font-bold truncate block" href="/player/%s">%s</a><div class="mt-1 text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s</div></td>
  <td class="px-4 py-3">%s</td>
  <td class="px-4 py-3 text-[11px] text-slate-700 dark:text-slate-300 font-mono whitespace-pre-line break-words">%s</td>
  <td class="px-4 py-3 text-[11px]"><a class="text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">Source</a></td>
</tr>|html}
                (escape_html y)
                (escape_html (pick_label r))
                (Uri.pct_encode r.dpr_player_id)
                (escape_html (normalize_name r.dpr_player_name))
                (escape_html r.dpr_player_id)
                team_html
                (escape_html r.dpr_raw_text)
                (escape_html r.dpr_source_url))
          |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
  <table class="min-w-[980px] w-full text-sm table-fixed tabular-nums">
    <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] whitespace-nowrap">
      <tr>
        <th class="px-4 py-3 text-left w-[72px]">Year</th>
        <th class="px-4 py-3 text-left w-[140px]">Pick</th>
        <th class="px-4 py-3 text-left">Player</th>
        <th class="px-4 py-3 text-left w-[220px]">Team</th>
        <th class="px-4 py-3 text-left">Raw</th>
        <th class="px-4 py-3 text-left w-[90px]">Link</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
  </table>
</div>|html}
      rows
  in
  let trade_list =
    let extract_salary_krw (text : string) : int option =
      let key = "연봉" in
      match find_substring_from ~sub:key text ~from:0 with
      | None -> None
      | Some idx ->
          let start = idx + String.length key in
          let len = String.length text in
          let is_digit c = c >= '0' && c <= '9' in
          let rec scan i seen_digit acc =
            if i >= len then acc
            else
              let c = text.[i] in
              if is_digit c then scan (i + 1) true (c :: acc)
              else if c = ',' && seen_digit then scan (i + 1) seen_digit acc
              else if seen_digit then acc
              else scan (i + 1) seen_digit acc
          in
          let digits_rev = scan start false [] in
          (match digits_rev with
          | [] -> None
          | _ ->
              let digits = digits_rev |> List.rev |> List.to_seq |> String.of_seq in
              int_of_string_opt digits)
    in
    let salary_chip (text: string) =
      match extract_salary_krw text with
      | None -> ""
      | Some won when won > 0 ->
          let million = (won + 500_000) / 1_000_000 in
          Printf.sprintf
            {html|<span title="연봉 ₩%d" class="shrink-0 px-2 py-0.5 rounded bg-orange-500/10 border border-orange-500/30 text-[10px] font-mono text-orange-700 whitespace-nowrap">₩%dM</span>|html}
            won
            million
      | Some _ -> ""
    in
    let contains_team (text: string) (team: string) =
      match find_substring_from ~sub:team text ~from:0 with
      | Some _ -> true
      | None -> false
    in
    let trade_team_candidates =
      [ "BNK 썸"; "BNK"; "우리은행"; "삼성생명"; "신한은행"; "KB스타즈"; "하나원큐"; "하나은행"
      ; "KB국민은행"; "국민은행"; "신세계"; "금호생명"; "KDB생명"; "KEB하나은행"
      ]
    in
    let team_chips (text: string) =
      let found =
        trade_team_candidates
        |> List.filter (contains_team text)
        |> List.fold_left
          (fun acc team_name ->
            let key =
              match team_code_of_string team_name with
              | Some code -> "CODE:" ^ code
              | None -> "NAME:" ^ (normalize_label team_name |> String.uppercase_ascii)
            in
            if List.exists (fun (k, _) -> k = key) acc then acc else (key, team_name) :: acc)
          []
        |> List.rev
      in
      found
      |> List.map (fun (_, team_name) -> team_badge team_name)
      |> String.concat ""
    in
    let rows =
      match trade_events with
      | [] ->
          {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/50 p-5 text-slate-500 dark:text-slate-400 text-sm">No trade events found. (Build with <span class="font-mono text-slate-700 dark:text-slate-300">WKBL_SYNC_DRAFT_TRADE=1</span> or run <span class="font-mono text-slate-700 dark:text-slate-300">scripts/wkbl_draft_trade_sync.py</span>)</div>|html}
      | xs ->
          let items =
            xs
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
                let salary_chip_html = salary_chip e.ote_event_text in
                let team_chips_html = team_chips e.ote_event_text in
                Printf.sprintf
                  {html|<li class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 sm:p-5 shadow-lg space-y-2">
  <div class="flex flex-wrap items-center justify-between gap-2">
    <div class="font-mono text-[11px] text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</div>
    <div class="flex flex-wrap items-center gap-2">%s%s%s</div>
  </div>
  <div class="text-slate-900 dark:text-slate-200 text-sm leading-relaxed break-words">%s</div>
  <div class="text-[11px]"><a class="text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">Source</a></div>
</li>|html}
                  (escape_html e.ote_event_date)
                  team_chips_html
                  contract_chip_html
                  salary_chip_html
                  (escape_html e.ote_event_text)
                  (escape_html e.ote_source_url))
            |> String.concat "\n"
          in
          Printf.sprintf {html|<ol class="space-y-3">%s</ol>|html} items
    in
    rows
  in
  let content =
    let section =
      if active_tab = "trade" then trade_list else draft_table
    in
    Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
  <div class="flex flex-col gap-2">
    <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Draft / Trade</h2>
    <div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">WKBL 공식 페이지 원문 기반입니다. (Draft는 <span class="font-mono text-slate-900 dark:text-slate-200">player_id(pno)</span> 기반 / Trade는 <span class="font-mono text-slate-900 dark:text-slate-200">원문 저장 + 텍스트 검색</span>)</div>
  </div>
  <div class="flex flex-wrap items-center gap-2">%s%s</div>
  %s
  <details class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 text-xs text-slate-500 dark:text-slate-400">
    <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">Sync / Build</summary>
    <div class="mt-2 space-y-2 leading-relaxed">
      <div>Docker 빌드에서 공식 Draft/Trade를 포함하려면 <span class="font-mono text-slate-900 dark:text-slate-200">WKBL_SYNC_DRAFT_TRADE=1</span>을 켜세요.</div>
      <div class="text-slate-500 dark:text-slate-400">로컬 DB를 갱신하려면 아래를 실행하세요: (네트워크 필요)</div>
      <code class="block font-mono text-slate-700 dark:text-slate-300 bg-slate-950/30 border border-slate-300 dark:border-slate-700/60 px-3 py-2 rounded overflow-x-auto whitespace-nowrap">python3 scripts/wkbl_draft_trade_sync.py --only-missing --backup</code>
    </div>
  </details>
  %s
</div>|html}
      (tab_link "draft" "Draft")
      (tab_link "trade" "Trade")
      filter_form
      section
  in
  layout ~title:"Draft / Trade | WKBL" ~content ()

(* ===== Fantasy Calculator ===== *)

(** Fantasy Calculator Page - Calculate fantasy points with custom weights *)
let rec fantasy_calculator_page
    ~season
    ~(seasons: season_info list)
    ~(rules: fantasy_scoring_rule)
    ~(scores: fantasy_player_score list)
    =
  let season_options =
    seasons
    |> List.map (fun (s: season_info) ->
        let selected = if s.code = season then "selected" else "" in
        Printf.sprintf {html|<option value="%s" %s>%s</option>|html}
          (escape_html s.code) selected (escape_html s.name))
    |> String.concat "\n"
  in
  let slider_input ~id ~label ~value ~min_val ~max_val ~step =
    Printf.sprintf
      {html|<div class="space-y-1">
  <div class="flex items-center justify-between">
    <label for="%s" class="text-xs font-bold text-slate-500 dark:text-slate-400 uppercase tracking-widest">%s</label>
    <span id="%s-value" class="text-sm font-mono font-bold text-slate-900 dark:text-slate-200">%.1f</span>
  </div>
  <input type="range" id="%s" name="%s" value="%.1f" min="%.1f" max="%.1f" step="%.1f"
         class="w-full h-2 bg-slate-200 dark:bg-slate-700 rounded-lg appearance-none cursor-pointer accent-orange-500"
         oninput="document.getElementById('%s-value').textContent = this.value">
</div>|html}
      id label id value id id value min_val max_val step id
  in
  let rules_form =
    Printf.sprintf
      {html|<form id="fantasy-form" hx-get="/fantasy/calculate" hx-target="#fantasy-results" hx-trigger="change delay:300ms" hx-swap="innerHTML"
            class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg space-y-5">
  <div class="flex items-center justify-between">
    <h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">Scoring Rules</h3>
    <button type="button" onclick="resetRules()" class="text-[11px] text-slate-500 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400 font-mono">Reset</button>
  </div>
  <input type="hidden" name="season" value="%s">
  <div class="grid grid-cols-2 md:grid-cols-3 gap-4">
    %s
    %s
    %s
    %s
    %s
    %s
  </div>
  <div class="pt-2 text-[11px] text-slate-500 dark:text-slate-400">
    <span class="font-mono">Fantasy Score</span> = (PTS × <span class="font-mono text-slate-700 dark:text-slate-300">pts</span>) + (REB × <span class="font-mono text-slate-700 dark:text-slate-300">reb</span>) + (AST × <span class="font-mono text-slate-700 dark:text-slate-300">ast</span>) + (STL × <span class="font-mono text-slate-700 dark:text-slate-300">stl</span>) + (BLK × <span class="font-mono text-slate-700 dark:text-slate-300">blk</span>) + (TOV × <span class="font-mono text-slate-700 dark:text-slate-300">tov</span>)
  </div>
</form>
<script>
function resetRules() {
  document.getElementById('pts').value = 1.0;
  document.getElementById('reb').value = 1.2;
  document.getElementById('ast').value = 1.5;
  document.getElementById('stl').value = 2.0;
  document.getElementById('blk').value = 2.0;
  document.getElementById('tov').value = -1.0;
  ['pts', 'reb', 'ast', 'stl', 'blk', 'tov'].forEach(id => {
    document.getElementById(id + '-value').textContent = document.getElementById(id).value;
  });
  htmx.trigger('#fantasy-form', 'change');
}
</script>|html}
      (escape_html season)
      (slider_input ~id:"pts" ~label:"Points" ~value:rules.fsr_points ~min_val:0.0 ~max_val:3.0 ~step:0.1)
      (slider_input ~id:"reb" ~label:"Rebounds" ~value:rules.fsr_rebounds ~min_val:0.0 ~max_val:3.0 ~step:0.1)
      (slider_input ~id:"ast" ~label:"Assists" ~value:rules.fsr_assists ~min_val:0.0 ~max_val:3.0 ~step:0.1)
      (slider_input ~id:"stl" ~label:"Steals" ~value:rules.fsr_steals ~min_val:0.0 ~max_val:5.0 ~step:0.1)
      (slider_input ~id:"blk" ~label:"Blocks" ~value:rules.fsr_blocks ~min_val:0.0 ~max_val:5.0 ~step:0.1)
      (slider_input ~id:"tov" ~label:"Turnovers" ~value:rules.fsr_turnovers ~min_val:(-3.0) ~max_val:0.0 ~step:0.1)
  in
  let results_table = fantasy_results_table scores in
  let content =
    Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
  <div class="flex flex-col gap-2">
    <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">Fantasy Calculator</h2>
    <div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">
      슬라이더를 조정하여 나만의 Fantasy 포인트 가중치를 설정하세요. 결과는 실시간으로 업데이트됩니다.
    </div>
  </div>
  <div class="flex items-center gap-3">
    <label class="text-xs text-slate-500 dark:text-slate-400 font-bold uppercase tracking-widest">Season</label>
    <select id="season-select" class="bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none"
            onchange="window.location.href='/fantasy?season=' + this.value">
      %s
    </select>
  </div>
  %s
  <div id="fantasy-results">
    %s
  </div>
</div>|html}
      season_options
      rules_form
      results_table
  in
  layout ~title:"Fantasy Calculator | WKBL" ~content ()

(** Fantasy results table partial (for HTMX updates) *)
and fantasy_results_table (scores: fantasy_player_score list) =
  let sorted = List.sort (fun a b -> Float.compare b.fps_avg_score a.fps_avg_score) scores in
  let rows =
    sorted
    |> List.mapi (fun i (s: fantasy_player_score) ->
        let rank = i + 1 in
        let rank_class =
          if rank = 1 then "text-orange-600 dark:text-orange-400 font-black"
          else if rank <= 3 then "text-slate-700 dark:text-slate-300 font-bold"
          else "text-slate-500 dark:text-slate-400"
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/30 transition-colors">
  <td class="px-4 py-3 %s tabular-nums">%d</td>
  <td class="px-4 py-3">
    <a href="/player/%s" class="text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:hover:text-orange-400 font-bold">%s</a>
  </td>
  <td class="px-4 py-3">%s</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-slate-500 dark:text-slate-400">%d</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-slate-900 dark:text-slate-200 font-bold">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-orange-600 dark:text-orange-400 font-bold">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-4 py-3 text-right font-mono tabular-nums text-[11px] text-red-500 dark:text-red-400">%.1f</td>
</tr>|html}
          rank_class rank
          (Uri.pct_encode s.fps_player_id)
          (escape_html (normalize_name s.fps_player_name))
          (team_badge s.fps_team_name)
          s.fps_games_played
          s.fps_total_score
          s.fps_avg_score
          s.fps_pts_contrib
          s.fps_reb_contrib
          s.fps_ast_contrib
          s.fps_stl_contrib
          s.fps_blk_contrib
          s.fps_tov_contrib)
    |> String.concat "\n"
  in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
  <table class="min-w-[1100px] w-full text-sm table-fixed tabular-nums">
    <thead class="bg-slate-100 dark:bg-slate-800/80 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] whitespace-nowrap">
      <tr>
        <th class="px-4 py-3 text-left w-[60px]">#</th>
        <th class="px-4 py-3 text-left">Player</th>
        <th class="px-4 py-3 text-left w-[180px]">Team</th>
        <th class="px-4 py-3 text-right w-[60px]">GP</th>
        <th class="px-4 py-3 text-right w-[90px]">Total</th>
        <th class="px-4 py-3 text-right w-[90px]">AVG</th>
        <th class="px-4 py-3 text-right w-[70px]" title="Points contribution">PTS</th>
        <th class="px-4 py-3 text-right w-[70px]" title="Rebounds contribution">REB</th>
        <th class="px-4 py-3 text-right w-[70px]" title="Assists contribution">AST</th>
        <th class="px-4 py-3 text-right w-[70px]" title="Steals contribution">STL</th>
        <th class="px-4 py-3 text-right w-[70px]" title="Blocks contribution">BLK</th>
        <th class="px-4 py-3 text-right w-[70px]" title="Turnovers contribution">TOV</th>
      </tr>
    </thead>
    <tbody>%s</tbody>
  </table>
</div>
<div class="text-[11px] text-slate-500 dark:text-slate-400 mt-2">
  %d players · Sorted by average fantasy score (per game)
</div>|html}
    rows
    (List.length scores)

(* ===== History & Legends Pages ===== *)

(** History page - Season champions and MVPs *)

(* ===== H2H Advanced Section ===== *)

(** H2H Summary Card - Shows win/loss record between two players *)
let h2h_summary_card ~p1_name ~p2_name (summary: Domain.h2h_summary) =
  if summary.h2h_total_games = 0 then
    ""
  else
    let p1_win_pct =
      if summary.h2h_total_games > 0 then
        float_of_int summary.h2h_p1_wins /. float_of_int summary.h2h_total_games *. 100.0
      else 0.0
    in
    let p2_win_pct = 100.0 -. p1_win_pct in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
        <h4 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest mb-4">Head-to-Head Record</h4>
        <div class="flex items-center justify-center gap-6">
          <div class="text-center">
            <div class="text-3xl font-black text-orange-500">%d</div>
            <div class="text-xs text-slate-500 dark:text-slate-400 uppercase font-bold">%s Wins</div>
          </div>
          <div class="text-center">
            <div class="text-2xl font-bold text-slate-400">-</div>
          </div>
          <div class="text-center">
            <div class="text-3xl font-black text-sky-500">%d</div>
            <div class="text-xs text-slate-500 dark:text-slate-400 uppercase font-bold">%s Wins</div>
          </div>
        </div>
        <div class="mt-4 text-center text-xs text-slate-500 dark:text-slate-400">
          Total: %d games
        </div>
        <div class="mt-3 flex h-3 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
          <div class="bg-orange-500 transition-all duration-700" style="width: %.1f%%"></div>
          <div class="bg-sky-500 transition-all duration-700" style="width: %.1f%%"></div>
        </div>
      </div>|html}
      summary.h2h_p1_wins (escape_html p1_name)
      summary.h2h_p2_wins (escape_html p2_name)
      summary.h2h_total_games
      p1_win_pct p2_win_pct

(** H2H Average Stats Comparison - Shows average stats in H2H matchups *)
let h2h_avg_comparison ~p1_name ~p2_name (summary: Domain.h2h_summary) =
  if summary.h2h_total_games = 0 then
    ""
  else
    let stat_bar label v1 v2 =
      let max_val = max v1 v2 in
      let pct1 = if max_val > 0.0 then v1 /. max_val *. 100.0 else 50.0 in
      let pct2 = if max_val > 0.0 then v2 /. max_val *. 100.0 else 50.0 in
      Printf.sprintf
        {html|<div class="space-y-1">
          <div class="flex justify-between text-xs font-bold text-slate-500 dark:text-slate-400 uppercase tracking-tighter">
            <span class="text-orange-500 font-mono">%.1f</span>
            <span>%s</span>
            <span class="text-sky-500 font-mono">%.1f</span>
          </div>
          <div class="flex h-2 bg-slate-100 dark:bg-slate-800 rounded-full overflow-hidden">
            <div class="flex justify-end w-1/2 border-r border-slate-300 dark:border-slate-700">
              <div class="bg-orange-500 h-full transition-all duration-500 rounded-l-full" style="width: %.1f%%"></div>
            </div>
            <div class="flex justify-start w-1/2">
              <div class="bg-sky-500 h-full transition-all duration-500 rounded-r-full" style="width: %.1f%%"></div>
            </div>
          </div>
        </div>|html}
        v1 (escape_html label) v2 pct1 pct2
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
        <h4 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest mb-4">H2H Average Stats</h4>
        <div class="flex justify-between text-xs font-bold text-slate-500 dark:text-slate-400 mb-3">
          <span class="text-orange-500">%s</span>
          <span class="text-sky-500">%s</span>
        </div>
        <div class="space-y-4">
          %s
          %s
          %s
        </div>
        <div class="mt-3 text-center text-[10px] text-slate-400 dark:text-slate-500">
          Based on %d head-to-head games
        </div>
      </div>|html}
      (escape_html p1_name) (escape_html p2_name)
      (stat_bar "Points" summary.h2h_p1_avg_pts summary.h2h_p2_avg_pts)
      (stat_bar "Rebounds" summary.h2h_p1_avg_reb summary.h2h_p2_avg_reb)
      (stat_bar "Assists" summary.h2h_p1_avg_ast summary.h2h_p2_avg_ast)
      summary.h2h_total_games

(** H2H Score Chart - Visual bar chart comparing scores per game *)
let h2h_score_chart ~p1_name ~p2_name (games: Domain.h2h_game list) =
  if games = [] then
    ""
  else
    let max_pts =
      List.fold_left (fun acc (g: Domain.h2h_game) ->
        max acc (max g.player1_pts g.player2_pts)) 0 games
    in
    let max_pts_f = float_of_int (max max_pts 1) in
    let game_bar (g: Domain.h2h_game) =
      let p1_pct = float_of_int g.player1_pts /. max_pts_f *. 100.0 in
      let p2_pct = float_of_int g.player2_pts /. max_pts_f *. 100.0 in
      let p1_winner = g.player1_team = g.winner_team in
      let p1_border = if p1_winner then "ring-2 ring-orange-400" else "" in
      let p2_border = if not p1_winner then "ring-2 ring-sky-400" else "" in
      Printf.sprintf
        {html|<div class="flex items-center gap-2 group">
          <div class="w-20 text-right text-[10px] text-slate-500 dark:text-slate-400 font-mono truncate">%s</div>
          <div class="flex-1 flex items-center gap-1">
            <div class="flex-1 flex justify-end">
              <div class="bg-orange-500 h-6 rounded-l transition-all duration-500 flex items-center justify-end pr-1 %s" style="width: %.1f%%">
                <span class="text-[10px] font-bold text-white font-mono">%d</span>
              </div>
            </div>
            <div class="w-px h-6 bg-slate-300 dark:bg-slate-600"></div>
            <div class="flex-1 flex justify-start">
              <div class="bg-sky-500 h-6 rounded-r transition-all duration-500 flex items-center justify-start pl-1 %s" style="width: %.1f%%">
                <span class="text-[10px] font-bold text-white font-mono">%d</span>
              </div>
            </div>
          </div>
        </div>|html}
        (escape_html g.game_date)
        p1_border p1_pct g.player1_pts
        p2_border p2_pct g.player2_pts
    in
    let bars = games |> List.map game_bar |> String.concat "\n" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
        <h4 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest mb-4">Points per Game</h4>
        <div class="flex justify-between text-xs font-bold text-slate-500 dark:text-slate-400 mb-3 px-[5.5rem]">
          <span class="text-orange-500">%s</span>
          <span class="text-sky-500">%s</span>
        </div>
        <div class="space-y-2">
          %s
        </div>
        <div class="mt-3 text-center text-[10px] text-slate-400 dark:text-slate-500">
          Ring indicates winning team's player
        </div>
      </div>|html}
      (escape_html p1_name) (escape_html p2_name) bars

(** Complete H2H Advanced Section - Combines all H2H components *)
let h2h_advanced_section ~p1_name ~p2_name (games: Domain.h2h_game list) =
  if games = [] then
    ""
  else
    let summary = Domain.calculate_h2h_summary ~p1_team:"" games in
    Printf.sprintf
      {html|<div class="space-y-6 mt-8">
        <h3 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest">Head-to-Head Advanced</h3>
        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
          %s
          %s
        </div>
        %s
      </div>|html}
      (h2h_summary_card ~p1_name ~p2_name summary)
      (h2h_avg_comparison ~p1_name ~p2_name summary)
      (h2h_score_chart ~p1_name ~p2_name games)

(* ============================================= *)
(* Game Flow Chart                               *)
(* ============================================= *)

(** Render game flow chart as SVG showing score difference over time.
    X-axis: Time (Q1 -> Q4 + OT), Y-axis: Score difference (home - away) *)
let game_flow_chart ~home_team ~away_team (flow_points: Domain.score_flow_point list) =
  if flow_points = [] then
    {html|<div class="text-center text-slate-500 dark:text-slate-400 py-8">No score flow data available</div>|html}
  else
    (* Chart dimensions *)
    let width = 800 in
    let height = 300 in
    let padding_x = 50 in
    let padding_y = 40 in
    let chart_width = width - (2 * padding_x) in
    let chart_height = height - (2 * padding_y) in

    (* Calculate time range - full game is 2400 seconds (4 quarters x 10 min) *)
    let max_time =
      flow_points
      |> List.fold_left (fun acc p -> max acc p.Domain.sfp_elapsed_seconds) 0
      |> max 2400
    in

    (* Calculate score diff range with padding *)
    let (min_diff, max_diff) =
      flow_points
      |> List.fold_left (fun (mn, mx) p -> (min mn p.Domain.sfp_diff, max mx p.Domain.sfp_diff)) (0, 0)
    in
    let diff_range = max (abs min_diff) (abs max_diff) |> max 10 in
    let y_min = -diff_range - 5 in
    let y_max = diff_range + 5 in
    let y_range = y_max - y_min in

    (* Scale functions *)
    let x_scale t = padding_x + (t * chart_width / max_time) in
    let y_scale d = padding_y + chart_height - ((d - y_min) * chart_height / y_range) in

    (* Build path string *)
    let path_d =
      flow_points
      |> List.mapi (fun i p ->
          let x = x_scale p.Domain.sfp_elapsed_seconds in
          let y = y_scale p.Domain.sfp_diff in
          if i = 0 then Printf.sprintf "M %d %d" x y
          else Printf.sprintf "L %d %d" x y)
      |> String.concat " "
    in

    (* Area fill path (from line to zero baseline) *)
    let area_path =
      let baseline_y = y_scale 0 in
      let first_pt = List.hd flow_points in
      let last_pt = List.hd (List.rev flow_points) in
      let first_x = x_scale first_pt.Domain.sfp_elapsed_seconds in
      let last_x = x_scale last_pt.Domain.sfp_elapsed_seconds in
      Printf.sprintf "%s L %d %d L %d %d Z" path_d last_x baseline_y first_x baseline_y
    in

    (* Quarter markers *)
    let quarter_lines =
      [600; 1200; 1800; 2400]  (* End of Q1, Q2, Q3, Q4 *)
      |> List.filter (fun t -> t <= max_time)
      |> List.map (fun t ->
          let x = x_scale t in
          Printf.sprintf {|<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#94a3b8" stroke-width="1" stroke-dasharray="4 4"/>|} x padding_y x (height - padding_y))
      |> String.concat "\n"
    in

    (* Quarter labels *)
    let quarter_labels =
      [("Q1", 300); ("Q2", 900); ("Q3", 1500); ("Q4", 2100)]
      |> List.filter (fun (_, t) -> t <= max_time)
      |> List.map (fun (label, t) ->
          let x = x_scale t in
          Printf.sprintf {|<text x="%d" y="%d" class="fill-slate-500 dark:fill-slate-400" font-size="11" text-anchor="middle">%s</text>|} x (height - 10) label)
      |> String.concat "\n"
    in

    (* OT labels if game went to overtime *)
    let ot_labels =
      if max_time > 2400 then
        let ot_count = (max_time - 2400 + 299) / 300 in
        List.init ot_count (fun i ->
          let t = 2400 + (i * 300) + 150 in
          let x = x_scale t in
          Printf.sprintf {|<text x="%d" y="%d" class="fill-slate-500 dark:fill-slate-400" font-size="11" text-anchor="middle">OT%d</text>|} x (height - 10) (i + 1))
        |> String.concat "\n"
      else ""
    in

    (* Y-axis labels *)
    let y_labels =
      let step = if diff_range > 20 then 10 else 5 in
      let values = List.init ((2 * diff_range / step) + 1) (fun i -> y_min + 5 + (i * step)) in
      values
      |> List.filter (fun v -> v >= y_min && v <= y_max)
      |> List.map (fun v ->
          let y = y_scale v in
          let label = if v > 0 then Printf.sprintf "+%d" v else string_of_int v in
          Printf.sprintf {|<text x="%d" y="%d" class="fill-slate-500 dark:fill-slate-400" font-size="10" text-anchor="end" dominant-baseline="middle">%s</text>|} (padding_x - 5) y label)
      |> String.concat "\n"
    in

    (* Zero line *)
    let zero_line =
      let y = y_scale 0 in
      Printf.sprintf {|<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#475569" stroke-width="2"/>|} padding_x y (width - padding_x) y
    in

    (* Data points (circles) *)
    let data_points =
      flow_points
      |> List.map (fun p ->
          let x = x_scale p.Domain.sfp_elapsed_seconds in
          let y = y_scale p.Domain.sfp_diff in
          let color = if p.Domain.sfp_diff > 0 then "#0ea5e9" else if p.Domain.sfp_diff < 0 then "#f97316" else "#64748b" in
          Printf.sprintf {|<circle cx="%d" cy="%d" r="3" fill="%s" class="hover:r-5 transition-all"><title>%s %s: %d-%d (%s%d)</title></circle>|}
            x y color
            p.Domain.sfp_period p.Domain.sfp_clock
            p.Domain.sfp_home_score p.Domain.sfp_away_score
            (if p.Domain.sfp_diff > 0 then "+" else "") p.Domain.sfp_diff)
      |> String.concat "\n"
    in

    (* Determine fill color based on final score *)
    let final_diff = (List.hd (List.rev flow_points)).Domain.sfp_diff in
    let gradient_id = "flowGradient" in
    let (gradient_color, line_color) =
      if final_diff > 0 then ("#0ea5e920", "#0ea5e9")  (* Sky blue for home lead *)
      else if final_diff < 0 then ("#f9731620", "#f97316")  (* Orange for away lead *)
      else ("#64748b20", "#64748b")  (* Gray for tie *)
    in

    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
        <div class="flex items-center justify-between mb-4">
          <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 flex items-center gap-2">
            <span class="w-1 h-6 bg-orange-500 rounded-full"></span>
            Game Flow
          </h3>
          <div class="flex items-center gap-4 text-xs">
            <div class="flex items-center gap-1">
              <span class="w-3 h-3 rounded-full bg-sky-500"></span>
              <span class="text-slate-600 dark:text-slate-400">%s leads</span>
            </div>
            <div class="flex items-center gap-1">
              <span class="w-3 h-3 rounded-full bg-orange-500"></span>
              <span class="text-slate-600 dark:text-slate-400">%s leads</span>
            </div>
          </div>
        </div>
        <div class="overflow-x-auto">
          <svg viewBox="0 0 %d %d" class="w-full min-w-[600px]" preserveAspectRatio="xMidYMid meet">
            <defs>
              <linearGradient id="%s" x1="0%%" y1="0%%" x2="0%%" y2="100%%">
                <stop offset="0%%" stop-color="%s"/>
                <stop offset="100%%" stop-color="transparent"/>
              </linearGradient>
            </defs>
            <!-- Grid lines -->
            %s
            <!-- Zero baseline -->
            %s
            <!-- Area fill -->
            <path d="%s" fill="url(#%s)" opacity="0.5"/>
            <!-- Line -->
            <path d="%s" fill="none" stroke="%s" stroke-width="2" stroke-linejoin="round"/>
            <!-- Data points -->
            %s
            <!-- X-axis labels -->
            %s
            %s
            <!-- Y-axis labels -->
            %s
          </svg>
        </div>
      </div>|html}
      (escape_html home_team)
      (escape_html away_team)
      width height
      gradient_id
      gradient_color
      quarter_lines
      zero_line
      area_path gradient_id
      path_d line_color
      data_points
      quarter_labels
      ot_labels
      y_labels

(** Game flow page with chart and summary statistics *)
let game_flow_page ~(game: Domain.game_info) (flow_points: Domain.score_flow_point list) =
  let chart = game_flow_chart ~home_team:game.gi_home_team_name ~away_team:game.gi_away_team_name flow_points in

  (* Calculate lead changes and biggest leads *)
  let (lead_changes, biggest_home_lead, biggest_away_lead) =
    let rec count_changes prev_leader changes home_max away_max = function
      | [] -> (changes, home_max, away_max)
      | p :: rest ->
          let current_leader =
            if p.Domain.sfp_diff > 0 then 1
            else if p.Domain.sfp_diff < 0 then -1
            else prev_leader
          in
          let new_changes = if current_leader <> prev_leader && prev_leader <> 0 then changes + 1 else changes in
          let new_home_max = max home_max p.Domain.sfp_diff in
          let new_away_max = max away_max (-p.Domain.sfp_diff) in
          count_changes current_leader new_changes new_home_max new_away_max rest
    in
    count_changes 0 0 0 0 flow_points
  in

  (* Calculate time with lead *)
  let (home_lead_time, away_lead_time, _tied_time) =
    let rec calc_times _prev_time h_time a_time t_time = function
      | [] -> (h_time, a_time, t_time)
      | [_] -> (h_time, a_time, t_time)
      | p1 :: (p2 :: _ as rest) ->
          let duration = p2.Domain.sfp_elapsed_seconds - p1.Domain.sfp_elapsed_seconds in
          let (h, a, t) =
            if p1.Domain.sfp_diff > 0 then (h_time + duration, a_time, t_time)
            else if p1.Domain.sfp_diff < 0 then (h_time, a_time + duration, t_time)
            else (h_time, a_time, t_time + duration)
          in
          calc_times p2.Domain.sfp_elapsed_seconds h a t rest
    in
    match flow_points with
    | [] -> (0, 0, 0)
    | _ -> calc_times 0 0 0 0 flow_points
  in

  let format_time secs =
    let mins = secs / 60 in
    let secs_rem = secs mod 60 in
    Printf.sprintf "%d:%02d" mins secs_rem
  in

  let stats_section =
    Printf.sprintf
      {html|<div class="grid grid-cols-2 md:grid-cols-4 gap-4 mt-6">
        <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
          <div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono">%d</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">Lead Changes</div>
        </div>
        <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
          <div class="text-2xl font-black text-sky-600 dark:text-sky-400 font-mono">+%d</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">%s Max Lead</div>
        </div>
        <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
          <div class="text-2xl font-black text-orange-600 dark:text-orange-400 font-mono">+%d</div>
          <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">%s Max Lead</div>
        </div>
        <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
          <div class="text-lg font-bold text-slate-900 dark:text-slate-200 font-mono">
            <span class="text-sky-600 dark:text-sky-400">%s</span> /
            <span class="text-orange-600 dark:text-orange-400">%s</span>
          </div>
          <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">Time with Lead</div>
        </div>
      </div>|html}
      lead_changes
      biggest_home_lead (escape_html game.gi_home_team_name)
      biggest_away_lead (escape_html game.gi_away_team_name)
      (format_time home_lead_time) (format_time away_lead_time)
  in

  layout ~title:(Printf.sprintf "Game Flow: %s vs %s" game.gi_home_team_name game.gi_away_team_name)
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
        <div class="text-center">
          <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s vs %s</h1>
          <div class="text-slate-500 dark:text-slate-400 text-sm mt-1">%s</div>
          <div class="text-3xl font-black text-slate-900 dark:text-slate-200 mt-2">%d - %d</div>
        </div>
        %s
        %s
        <div class="flex justify-center gap-4">
          <a href="/boxscore/%s" class="px-4 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">← Boxscore</a>
          <a href="/boxscore/%s/pbp" class="px-4 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">Play-by-Play →</a>
        </div>
      </div>|html}
      (escape_html game.gi_home_team_name) (escape_html game.gi_away_team_name)
      (escape_html game.gi_game_date)
      game.gi_home_score game.gi_away_score
      chart
      stats_section
      (escape_html game.gi_game_id)
      (escape_html game.gi_game_id)) ()
