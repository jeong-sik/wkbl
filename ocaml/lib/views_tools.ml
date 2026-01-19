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

(* ===== History & Legends Pages ===== *)

(** History page - Season champions and MVPs *)
