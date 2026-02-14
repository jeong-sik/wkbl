(** Data tools view functions for WKBL Analytics *)
(** Contains QA dashboard and transactions pages. *)

open Domain
open Views_common

let pretty_timestamp (s : string) : string =
  (* Make ISO-ish timestamps easier to read in UI.
     Examples:
     - 2026-02-07T08:17:50Z -> 2026-02-07 08:17
     - 2026-02-07 08:17:50 -> 2026-02-07 08:17 *)
  let s = String.trim s in
  if s = "" then s
  else
    let s =
      if String.contains s 'T' then
        String.map (fun c -> if c = 'T' then ' ' else c) s
      else
        s
    in
    let s =
      match String.index_opt s '.' with
      | None -> s
      | Some i -> String.sub s 0 i
    in
    let s =
      if String.ends_with ~suffix:"Z" s then
        String.sub s 0 (String.length s - 1)
      else
        s
    in
    if String.length s >= 16 then String.sub s 0 16 else s

let qa_dashboard_page ?(lang=I18n.Ko) (report: Db.qa_db_report) ?(markdown=None) () =
  let tr = I18n.t lang in
  let page_title = tr { ko = "데이터 점검 | WKBL"; en = "Data Check | WKBL" } in
  let heading = tr { ko = "데이터 점검"; en = "Data Check" } in
  let label_last_checked = tr { ko = "마지막 확인"; en = "Last checked" } in
  let label_count = tr { ko = "건수"; en = "Count" } in
  let matchup_sep = tr { ko = " 대 "; en = " vs " } in
  let desc =
    tr
      { ko = "경기 점수/선수 기록이 서로 맞는지, 빠진 부분이 없는지 확인합니다."
      ; en = "Check whether game scores and player stats match, and whether anything is missing."
      }
  in
  let flag_no_games = tr { ko = "경기 없음"; en = "No games" } in
  let flag_missing_team = tr { ko = "팀 정보 누락"; en = "Missing team info" } in

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
  let score_or_dash = Option.fold ~none:"-" ~some:string_of_int in
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
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs whitespace-nowrap">%s</td><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300" href="%s">%s</a></td><td class="px-3 py-2 text-slate-700 dark:text-slate-300 text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-500 dark:text-slate-400 tabular-nums">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-500 dark:text-slate-400 tabular-nums">%s</td></tr>|html}
        (escape_html row.qsm_game_date)
        (boxscore_href row.qsm_game_id)
        (escape_html row.qsm_game_id)
        (escape_html (row.qsm_home_team ^ matchup_sep ^ row.qsm_away_team))
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
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300" href="%s">%s</a></td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td></tr>|html}
        (boxscore_href row.qtca_game_id)
        (escape_html row.qtca_game_id)
        row.qtca_team_count)
    |> String.concat "\n"
  in
  let dup_row_rows =
    report.qdr_duplicate_player_row_sample
    |> List.map (fun (row: Db.qa_duplicate_player_row) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-700 dark:text-orange-400" href="%s">%s</a></td><td class="px-3 py-2 text-xs">%s</td><td class="px-3 py-2 text-xs"><a class="hover:text-orange-700 dark:text-orange-400" href="%s">%s</a></td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td></tr>|html}
        (boxscore_href row.qdpr_game_id)
        (escape_html row.qdpr_game_id)
        (team_badge row.qdpr_team_name)
        (player_href row.qdpr_player_id)
        (escape_html row.qdpr_player_name)
        row.qdpr_row_count)
    |> String.concat "\n"
  in
  let dup_name_rows =
    report.qdr_duplicate_player_name_sample
    |> List.map (fun (row: Db.qa_duplicate_player_name) ->
      let ids =
        row.qdpn_player_ids
        |> List.map (fun id -> Printf.sprintf {html|<a href="%s" class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-900 dark:text-slate-200 hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300">%s</a>|html} (player_href id) (escape_html id))
        |> String.concat " "
      in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td><td class="px-3 py-2">%s</td></tr>|html}
        (escape_html row.qdpn_player_name)
        row.qdpn_id_count
        ids)
    |> String.concat "\n"
  in
  let dup_identity_rows =
    report.qdr_duplicate_player_identity_sample
    |> List.map (fun (row: Db.qa_duplicate_player_identity) ->
      let ids =
        row.qdpi_player_ids
        |> List.map (fun id -> Printf.sprintf {html|<a href="%s" class="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 text-[10px] font-mono text-slate-900 dark:text-slate-200 hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300">%s</a>|html} (player_href id) (escape_html id))
        |> String.concat " "
      in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 text-xs">%s</td><td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td><td class="px-3 py-2">%s</td></tr>|html}
        (escape_html row.qdpi_player_name)
        (escape_html row.qdpi_birth_date)
        row.qdpi_id_count
        ids)
    |> String.concat "\n"
  in
  let schedule_missing_game_rows =
    report.qdr_schedule_missing_game_sample
    |> List.map (fun (row: Db.qa_schedule_missing_game) ->
      let matchup = row.qsmg_home_team ^ matchup_sep ^ row.qsmg_away_team in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs whitespace-nowrap">%s</td><td class="px-3 py-2 text-xs font-mono">%s</td><td class="px-3 py-2 text-xs">%s</td></tr>|html}
        (escape_html row.qsmg_game_date)
        (escape_html row.qsmg_season_code)
        (escape_html matchup))
    |> String.concat "\n"
  in
  let schedule_missing_stats_rows =
    report.qdr_schedule_missing_stats_sample
    |> List.map (fun (row: Db.qa_schedule_missing_stats) ->
      let matchup = row.qsms_home_team ^ matchup_sep ^ row.qsms_away_team in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs whitespace-nowrap">%s</td><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs"><a class="hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300" href="%s">%s</a></td><td class="px-3 py-2 text-xs">%s</td></tr>|html}
        (escape_html row.qsms_game_date)
        (boxscore_href row.qsms_game_id)
        (escape_html row.qsms_game_id)
        (escape_html matchup))
    |> String.concat "\n"
  in
  let schedule_coverage_rows =
    let badge label cls =
      Printf.sprintf
        {html|<span class="px-2 py-0.5 rounded border text-[10px] font-mono %s">%s</span>|html}
        cls
        (escape_html label)
    in
    report.qdr_schedule_coverage
    |> List.map (fun (row: Db.qa_schedule_coverage) ->
      let coverage_html =
        Printf.sprintf
          {html|<span class="font-mono tabular-nums text-xs">%0.1f%%</span>|html}
          row.qsc_coverage_pct
      in
      let flags =
        []
        |> fun acc -> if row.qsc_season_uningested then badge flag_no_games "bg-amber-50 text-amber-700 border-amber-200 dark:bg-amber-900/20 dark:text-amber-300 dark:border-amber-700/60" :: acc else acc
        |> fun acc -> if row.qsc_games_missing_team then badge flag_missing_team "bg-rose-50 text-rose-700 border-rose-200 dark:bg-rose-900/20 dark:text-rose-300 dark:border-rose-700/60" :: acc else acc
      in
      let flags_html =
        match List.rev flags with
        | [] -> {html|<span class="text-xs text-slate-400">-</span>|html}
        | items -> String.concat " " items
      in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors"><td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs">%s</td><td class="px-3 py-2 text-right font-mono text-xs tabular-nums">%d</td><td class="px-3 py-2 text-right font-mono text-xs tabular-nums">%d</td><td class="px-3 py-2 text-right font-mono text-xs tabular-nums">%d</td><td class="px-3 py-2 text-right font-mono text-xs tabular-nums">%d</td><td class="px-3 py-2 text-right">%s</td><td class="px-3 py-2">%s</td></tr>|html}
        (escape_html row.qsc_season_code)
        row.qsc_schedule_completed
        row.qsc_games_total
        row.qsc_matched
        row.qsc_missing
        coverage_html
        flags_html)
    |> String.concat "\n"
  in
  let schedule_coverage_block =
    let label_heading =
      tr { ko = "시즌별 일정-경기 매칭"; en = "Schedule coverage (by season)" }
    in
    let label_items = tr { ko = "항목"; en = "Items" } in
    let label_desc =
      tr
        { ko = "종료된 일정 대비 경기 매칭 비율 (팀 정보 누락/미수집 시즌 포함)"
        ; en = "Match rate for completed schedule rows (including missing-team / no-game seasons)."
        }
    in
    let th_season = tr { ko = "시즌"; en = "Season" } in
    let th_completed = tr { ko = "종료 일정"; en = "Completed" } in
    let th_games = tr { ko = "경기"; en = "Games" } in
    let th_matched = tr { ko = "매칭"; en = "Matched" } in
    let th_missing = tr { ko = "누락"; en = "Missing" } in
    let th_rate = tr { ko = "비율"; en = "Rate" } in
    let th_flags = tr { ko = "표시"; en = "Flags" } in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[880px] w-full text-sm font-mono table-fixed" aria-label="시즌별 일정-경기 매칭 현황">
        <colgroup>
          <col style="width: 70px;"> <!-- Season -->
          <col style="width: 90px;"> <!-- Schedule -->
          <col style="width: 80px;"> <!-- Games -->
          <col style="width: 80px;"> <!-- Matched -->
          <col style="width: 80px;"> <!-- Missing -->
          <col style="width: 90px;"> <!-- Coverage -->
          <col style="width: auto;"> <!-- Flags -->
        </colgroup>
        <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-right">%s</th><th scope="col" class="px-3 py-2 text-right">%s</th><th scope="col" class="px-3 py-2 text-right">%s</th><th scope="col" class="px-3 py-2 text-right">%s</th><th scope="col" class="px-3 py-2 text-right">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (escape_html label_heading)
      (escape_html label_items)
      (List.length report.qdr_schedule_coverage)
      (escape_html label_desc)
      (escape_html th_season)
      (escape_html th_completed)
      (escape_html th_games)
      (escape_html th_matched)
      (escape_html th_missing)
      (escape_html th_rate)
      (escape_html th_flags)
      schedule_coverage_rows
  in
  let markdown_block =
    match markdown with
    | None -> ""
	    | Some md ->
	        Printf.sprintf
	          {html|<details class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><summary class="cursor-pointer select-none text-slate-700 dark:text-slate-300 font-bold">%s</summary><pre class="mt-4 text-[11px] text-slate-700 dark:text-slate-300 whitespace-pre-wrap break-words">%s</pre></details>|html}
	          (escape_html (tr { ko = "이전 점검 결과"; en = "Previous report" }))
	          (escape_html md)
  in
  let sources_block =
    let label_summary =
      tr { ko = "출처 / 확인 방식"; en = "Sources / How it is checked" }
    in
  let label_sources = tr { ko = "출처 (공식)"; en = "Sources" } in
  let label_checks = tr { ko = "확인 방식"; en = "How it's checked" } in
  let label_scores_src = tr { ko = "경기 결과/스코어"; en = "Scores" } in
  let label_boxscore_src = tr { ko = "박스스코어"; en = "Boxscores" } in
  let label_pm_src = tr { ko = "개인 +/-"; en = "Plus/minus" } in
  let label_boxscore_desc =
    tr { ko = "WKBL 공식 박스스코어(선수 기록)"; en = "Official WKBL boxscore (player stats)." }
  in
  let label_pm_desc =
    tr
      { ko = "문자중계, 일부 경기만 제공"
      ; en = "Live text feed; available for some games only."
      }
  in
  let label_match = tr { ko = "일치"; en = "Match" } in
  let label_estimate = tr { ko = "추정"; en = "Estimated" } in
  let label_mismatch = tr { ko = "불일치"; en = "Mismatch" } in
  let desc_match =
    tr
      { ko = "최종 스코어와 양 팀 선수 득점 합계가 모두 일치"
      ; en = "Final score matches summed player points for both teams."
      }
  in
  let desc_estimate =
    tr
      { ko = "최종 스코어 또는 득점 합계가 일부 비어 있어 교차 확인이 어려움"
      ; en = "Some values are missing, so cross-check is limited."
      }
  in
  let desc_mismatch =
    tr
      { ko = "최종 스코어와 선수 득점 합계가 모두 있는데 값이 다름"
      ; en = "Both values exist, but they differ."
      }
  in
  let note_checks =
    tr
      { ko = "※ 이 점검은 “최종 득점”만 대상으로, 다른 스탯(리바운드/어시스트 등)은 별도 확인이 필요합니다."
      ; en = "Note: this only checks final points. Other stats (rebounds/assists, etc.) need separate validation."
      }
  in
  Printf.sprintf
    {html|<details class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 text-xs text-slate-500 dark:text-slate-400">
      <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">%s</summary>
      <div class="mt-2 space-y-1 leading-relaxed">
        <div class="text-slate-500 dark:text-slate-400 font-bold">%s</div>
        <div>• %s: <a href="https://www.wkbl.or.kr/game/result.asp" target="_blank" rel="noreferrer" class="text-orange-700 dark:text-orange-400 hover:underline">wkbl.or.kr/game/result.asp</a></div>
        <div>• %s: %s</div>
        <div>• %s: <a href="https://www.wkbl.or.kr/live11/path_live_sms.asp" target="_blank" rel="noreferrer" class="text-orange-700 dark:text-orange-400 hover:underline">wkbl.or.kr/live11/path_live_sms.asp</a> (%s)</div>
        <div class="pt-1 text-slate-500 dark:text-slate-400 font-bold">%s</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">%s</span>: %s</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">%s</span>: %s</div>
        <div>• <span class="font-mono text-slate-900 dark:text-slate-200">%s</span>: %s</div>
        <div class="pt-1">%s</div>
      </div>
    </details>|html}
    (escape_html label_summary)
    (escape_html label_sources)
    (escape_html label_scores_src)
    (escape_html label_boxscore_src)
    (escape_html label_boxscore_desc)
    (escape_html label_pm_src)
    (escape_html label_pm_desc)
    (escape_html label_checks)
    (escape_html label_match)
    (escape_html desc_match)
    (escape_html label_estimate)
    (escape_html desc_estimate)
    (escape_html label_mismatch)
    (escape_html desc_mismatch)
    (escape_html note_checks)
  in
  let schedule_missing_blocks =
    let h_missing_game =
      tr { ko = "일정-경기 누락"; en = "Schedule not matched" }
    in
	    let d_missing_game =
	      tr
	        { ko = "종료된 일정이지만 경기 매칭이 되지 않음"
	        ; en = "Completed schedule rows that do not match any game."
	        }
	    in
	    let h_missing_stats =
	      tr { ko = "선수 기록 누락"; en = "Missing player stats" }
	    in
	    let d_missing_stats =
	      tr
	        { ko = "경기 정보는 있으나 선수 기록이 없음"
	        ; en = "Game exists, but player stats are missing."
	        }
	    in
	    let th_date = tr { ko = "날짜"; en = "Date" } in
	    let th_season = tr { ko = "시즌"; en = "Season" } in
	    let th_matchup = tr { ko = "대진"; en = "Matchup" } in
	    let th_game = tr { ko = "경기"; en = "Game" } in
	    Printf.sprintf
			      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[520px] w-full text-sm font-mono table-fixed" aria-label="일정-경기 누락 목록">
		          <colgroup>
		            <col style="width: 90px;"> <!-- Date -->
		            <col style="width: 80px;"> <!-- Season -->
		            <col style="width: auto;"> <!-- Matchup -->
		          </colgroup>
			          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th></tr></thead><tbody>%s</tbody></table></div></div><div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[620px] w-full text-sm font-mono table-fixed" aria-label="일정-선수 기록 누락 목록">
		          <colgroup>
		            <col style="width: 90px;"> <!-- Date -->
		            <col style="width: 120px;"> <!-- Game -->
		            <col style="width: auto;"> <!-- Matchup -->
		          </colgroup>
		          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
	      (escape_html h_missing_game)
	      (escape_html label_count)
	      report.qdr_schedule_missing_game_count
	      (escape_html d_missing_game)
	      (escape_html th_date)
	      (escape_html th_season)
	      (escape_html th_matchup)
	      schedule_missing_game_rows
	      (escape_html h_missing_stats)
	      (escape_html label_count)
	      report.qdr_schedule_missing_stats_count
	      (escape_html d_missing_stats)
	      (escape_html th_date)
      (escape_html th_game)
      (escape_html th_matchup)
      schedule_missing_stats_rows
  in
  let score_mismatch_block =
    let h = tr { ko = "스코어 불일치"; en = "Score mismatch" } in
    let d =
      tr
        { ko = "최종 스코어와 선수 득점 합계를 비교합니다."
        ; en = "Compare final score with summed player points."
        }
    in
    let th_date = tr { ko = "날짜"; en = "Date" } in
    let th_game = tr { ko = "경기"; en = "Game" } in
    let th_matchup = tr { ko = "대진"; en = "Matchup" } in
    let th_stored = tr { ko = "저장"; en = "Stored" } in
    let th_summed = tr { ko = "합산"; en = "Summed" } in
    let th_home_delta = tr { ko = "홈Δ"; en = "Home Δ" } in
    let th_away_delta = tr { ko = "원정Δ"; en = "Away Δ" } in
    let title_stored = tr { ko = "저장된 스코어"; en = "Stored score" } in
    let title_summed = tr { ko = "합산된 스코어"; en = "Summed points" } in
    let title_home_delta = tr { ko = "홈팀 차이"; en = "Home delta" } in
    let title_away_delta = tr { ko = "원정팀 차이"; en = "Away delta" } in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[860px] w-full text-sm font-mono table-fixed" aria-label="스코어 불일치 목록">
          <colgroup>
            <col style="width: 90px;"> <!-- Date -->
            <col style="width: 120px;"> <!-- Game -->
            <col style="width: auto;"> <!-- Matchup -->
            <col style="width: 120px;"> <!-- Stored -->
            <col style="width: 120px;"> <!-- Summed -->
            <col style="width: 80px;"> <!-- HΔ -->
            <col style="width: 80px;"> <!-- AΔ -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (escape_html h)
      (escape_html label_count)
      report.qdr_score_mismatch_count
      (escape_html d)
      (escape_html th_date)
      (escape_html th_game)
      (escape_html th_matchup)
      (escape_html title_stored)
      (escape_html th_stored)
      (escape_html title_summed)
      (escape_html th_summed)
      (escape_html title_home_delta)
      (escape_html th_home_delta)
      (escape_html title_away_delta)
      (escape_html th_away_delta)
      mismatch_rows
  in
  let team_count_anomaly_block =
    let h = tr { ko = "팀 수 이상"; en = "Team count anomaly" } in
    let d =
      tr
        { ko = "한 경기에서 팀 정보가 2개가 아닌 경우입니다."
        ; en = "Games where team entries are not exactly 2."
        }
    in
    let th_game = tr { ko = "경기"; en = "Game" } in
    let th_teams = tr { ko = "팀 수"; en = "Teams" } in
    let title_teams = tr { ko = "팀 수"; en = "Team count" } in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[320px] w-full text-sm font-mono table-fixed" aria-label="팀 수 이상 목록">
          <colgroup>
            <col style="width: auto;"> <!-- Game -->
            <col style="width: 90px;"> <!-- Teams -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (escape_html h)
      (escape_html label_count)
      report.qdr_team_count_anomaly_count
      (escape_html d)
      (escape_html th_game)
      (escape_html title_teams)
      (escape_html th_teams)
      team_count_rows
  in
  let duplicate_player_rows_block =
    let h = tr { ko = "중복 선수 기록"; en = "Duplicate player rows" } in
    let d =
      tr
        { ko = "한 경기에서 같은 선수가 여러 줄로 저장된 경우입니다."
        ; en = "Same player appears multiple times in a single game."
        }
    in
    let th_game = tr { ko = "경기"; en = "Game" } in
    let th_team = tr { ko = "팀"; en = "Team" } in
    let th_player = tr { ko = "선수"; en = "Player" } in
    let th_rows = tr { ko = "행 수"; en = "Rows" } in
    let title_rows = tr { ko = "중복 행 수"; en = "Duplicate row count" } in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[720px] w-full text-sm font-mono table-fixed" aria-label="중복 선수 기록 목록">
          <colgroup>
            <col style="width: 120px;"> <!-- Game -->
            <col style="width: 160px;"> <!-- Team -->
            <col style="width: auto;">  <!-- Player -->
            <col style="width: 80px;">  <!-- Rows -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (escape_html h)
      (escape_html label_count)
      report.qdr_duplicate_player_row_count
      (escape_html d)
      (escape_html th_game)
      (escape_html th_team)
      (escape_html th_player)
      (escape_html title_rows)
      (escape_html th_rows)
      dup_row_rows
  in
  let duplicate_player_name_block =
    let h = tr { ko = "동명이인(고유번호 여러 개)"; en = "Same name (multiple IDs)" } in
    let d =
      tr
        { ko = "동일 이름으로 선수 고유번호가 여러 개인 경우입니다."
        ; en = "Multiple player IDs share the same name."
        }
    in
    let th_name = tr { ko = "이름"; en = "Name" } in
    let th_ids = tr { ko = "고유번호"; en = "IDs" } in
    let th_id_count = tr { ko = "고유번호 수"; en = "ID count" } in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[720px] w-full text-sm font-mono table-fixed" aria-label="동명이인 목록">
          <colgroup>
            <col style="width: 120px;"> <!-- Name -->
            <col style="width: 80px;">  <!-- IDs -->
            <col style="width: auto;">  <!-- player_id -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (escape_html h)
      (escape_html label_count)
      report.qdr_duplicate_player_name_count
      (escape_html d)
      (escape_html th_name)
      (escape_html th_id_count)
      (escape_html th_id_count)
      (escape_html th_ids)
      dup_name_rows
  in
  let duplicate_player_identity_block =
    let h = tr { ko = "동일인 추정(이름+생년월일)"; en = "Possible same person (name + birth date)" } in
	    let d =
	      tr
	        { ko = "이름과 생년월일이 같은데 고유번호가 여러 개인 경우입니다. 대부분의 화면에서는 한 사람으로 묶어서 보여줍니다."
	        ; en = "Multiple player IDs share the same name and birth date. Most pages group them as one person."
	        }
	    in
    let th_name = tr { ko = "이름"; en = "Name" } in
    let th_birth = tr { ko = "생년월일"; en = "Birth date" } in
    let th_ids = tr { ko = "고유번호"; en = "IDs" } in
    let th_id_count = tr { ko = "고유번호 수"; en = "ID count" } in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="flex items-center justify-between gap-3"><h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3><span class="text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></div><div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div><div class="mt-4 overflow-x-auto"><table class="min-w-[760px] w-full text-sm font-mono table-fixed" aria-label="동일인 추정 목록">
          <colgroup>
            <col style="width: 140px;"> <!-- Name -->
            <col style="width: 120px;"> <!-- Birth -->
            <col style="width: 80px;">  <!-- IDs -->
            <col style="width: auto;">  <!-- player_id -->
          </colgroup>
          <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]"><tr><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th><th scope="col" class="px-3 py-2 text-right" title="%s">%s</th><th scope="col" class="px-3 py-2 text-left">%s</th></tr></thead><tbody>%s</tbody></table></div></div>|html}
      (escape_html h)
      (escape_html label_count)
      report.qdr_duplicate_player_identity_count
      (escape_html d)
      (escape_html th_name)
      (escape_html th_birth)
      (escape_html th_id_count)
      (escape_html th_id_count)
      (escape_html th_ids)
      dup_identity_rows
  in
	  layout ~lang ~title:page_title
	    ~canonical_path:"/tools/qa"
	    ~description:"WKBL 데이터 품질 대시보드 - 선수, 경기 데이터 무결성 점검"
	    ~content:(Printf.sprintf
	      {html|<div class="space-y-6 animate-fade-in">%s%s<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">%s%s%s%s%s%s%s%s%s</div><div class="grid grid-cols-1 lg:grid-cols-4 gap-4">%s%s%s%s</div><div class="grid grid-cols-1 gap-4">%s%s%s%s%s%s%s</div>%s</div>|html}
	      (Printf.sprintf
         {html|<div class="flex flex-col gap-2"><h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s</h2><div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">%s <span class="text-slate-400">(%s: <span class="font-mono">%s</span>)</span></div></div>|html}
         (escape_html heading)
	         (escape_html desc)
	         (escape_html label_last_checked)
	         (escape_html (pretty_timestamp report.qdr_generated_at)))
	      sources_block
	      (kpi_card ~label:(tr { ko = "경기"; en = "Games" }) ~value_html:(int_chip report.qdr_games_total) ~hint_html:(tr { ko = "전체 경기 수(정규/PO, 시범 제외)"; en = "Total games (regular/playoffs, excluding exhibition)." }))
	      (kpi_card ~label:(tr { ko = "문자중계 수집"; en = "Live text coverage" })
	        ~value_html:(pct_chip report.qdr_pbp_coverage_pct)
	        ~hint_html:(Printf.sprintf
	          {html|%s: %d · %s: %d · <a href="/qa/pbp-missing" class="underline decoration-slate-300 hover:decoration-slate-600">%s</a>|html}
	          (escape_html (tr { ko = "종료 경기"; en = "Finished" }))
	          report.qdr_finished_games_total
	          (escape_html (tr { ko = "문자중계 있음"; en = "With live text" }))
	          report.qdr_pbp_games
	          (escape_html (tr { ko = "누락 경기 보기"; en = "View missing games" }))))
	      (kpi_card ~label:(tr { ko = "기록 수집 경기"; en = "With boxscores" }) ~value_html:(int_chip report.qdr_games_with_stats) ~hint_html:(tr { ko = "선수 기록(박스스코어)이 수집된 경기"; en = "Games where player boxscores were collected." }))
	      (kpi_card ~label:(tr { ko = "스케줄 전체"; en = "Schedule rows" }) ~value_html:(int_chip report.qdr_schedule_total) ~hint_html:(tr { ko = "전체 스케줄 행 수"; en = "Total schedule rows." }))
	      (kpi_card ~label:(tr { ko = "종료 경기"; en = "Completed" }) ~value_html:(int_chip report.qdr_schedule_completed) ~hint_html:(tr { ko = "종료된 일정"; en = "Completed schedule rows." }))
	      (kpi_card ~label:(tr { ko = "+/- 가능"; en = "+/- available" }) ~value_html:(pct_chip report.qdr_plus_minus_coverage_pct) ~hint_html:(Printf.sprintf "%s: %d" (tr { ko = "+/-가 계산된 경기"; en = "Games with +/-" }) report.qdr_plus_minus_games))
	      (kpi_card ~label:(tr { ko = "스케줄-경기 누락"; en = "Schedule not matched" }) ~value_html:(int_chip report.qdr_schedule_missing_game_count) ~hint_html:(Printf.sprintf "%s: %.1f%%" (tr { ko = "종료 일정 중 경기 매칭 누락"; en = "Missing match (completed)" }) report.qdr_schedule_missing_game_pct))
	      (kpi_card ~label:(tr { ko = "선수 기록 누락"; en = "Missing player stats" }) ~value_html:(int_chip report.qdr_schedule_missing_stats_count) ~hint_html:(Printf.sprintf "%s: %.1f%%" (tr { ko = "종료 일정 중 선수 기록 없음"; en = "No player stats (completed)" }) report.qdr_schedule_missing_stats_pct))
	      (kpi_card ~label:label_last_checked ~value_html:(Printf.sprintf {html|<div class="text-sm font-mono text-slate-900 dark:text-slate-200">%s</div>|html} (escape_html (pretty_timestamp report.qdr_generated_at))) ~hint_html:(tr { ko = "자동으로 갱신됩니다."; en = "Auto-updated." }))
      (kpi_card ~label:(tr { ko = "스코어 불일치"; en = "Score mismatch" }) ~value_html:(int_chip report.qdr_score_mismatch_count) ~hint_html:(tr { ko = "최종 스코어와 득점 합계가 다름"; en = "Final score and summed points do not match." }))
      (kpi_card ~label:(tr { ko = "팀 수 이상"; en = "Team count anomaly" }) ~value_html:(int_chip report.qdr_team_count_anomaly_count) ~hint_html:(tr { ko = "한 경기에서 팀이 2개가 아닌 케이스"; en = "Games where team count is not 2." }))
      (kpi_card ~label:(tr { ko = "중복 선수 기록"; en = "Duplicate player rows" }) ~value_html:(int_chip report.qdr_duplicate_player_row_count) ~hint_html:(tr { ko = "한 경기에서 같은 선수가 여러 줄로 잡힘"; en = "Same player saved multiple times in a game." }))
      (kpi_card ~label:(tr { ko = "중복 선수 신원"; en = "Possible duplicate identity" }) ~value_html:(int_chip report.qdr_duplicate_player_identity_count) ~hint_html:(tr { ko = "이름+생년월일 기준 중복 ID"; en = "Multiple IDs with same name and birth date." }))
      schedule_coverage_block
      schedule_missing_blocks
      score_mismatch_block
      team_count_anomaly_block
      duplicate_player_rows_block
      duplicate_player_name_block
      duplicate_player_identity_block
	      markdown_block) ()

(** Live text missing QA:
    Shows finished games where live text (play-by-play) was not saved. *)
let qa_pbp_missing_page
    ?(lang=I18n.Ko)
    ~season
    ~(seasons: Domain.season_info list)
    (report: Db.qa_pbp_missing_report)
    () =
  let tr = I18n.t lang in
  let page_title =
    tr { ko = "데이터 점검 | 문자중계 누락 | WKBL"; en = "Data Check | Missing live text | WKBL" }
  in
  let heading = tr { ko = "문자중계 누락(종료 경기)"; en = "Missing live text (finished games)" } in
  let desc =
    tr
      { ko = "종료 경기를 기준으로, 문자중계가 저장되지 않은 경기를 보여줍니다."
      ; en = "Shows finished games where live text was not saved."
      }
  in

  let season_options =
    seasons
    |> List.map (fun (s: Domain.season_info) ->
        Printf.sprintf {html|<option value="%s"%s>%s</option>|html}
          (escape_html s.code)
          (if s.code = season then " selected" else "")
          (escape_html s.name))
    |> String.concat "\n"
  in

  let int_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%d</div>|html} v
  in
  let pct_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%.1f%%</div>|html} v
  in
  let kpi_card ~label ~value_html ~hint_html =
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><div class="text-slate-500 dark:text-slate-400 text-[11px] uppercase tracking-widest font-bold">%s</div><div class="mt-2">%s</div><div class="mt-2 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div></div>|html}
      (escape_html label)
      value_html
      hint_html
  in

  let missing_rows =
    report.qpmr_missing_sample
    |> List.map (fun (g: Db.qa_pbp_missing_game) ->
        let game_id_url = g.qpmg_game_id in
        let matchup = Printf.sprintf "%s vs %s" g.qpmg_home_team g.qpmg_away_team in
        Printf.sprintf
          {html|<tr class="border-t border-slate-200 dark:border-slate-800">
  <td class="px-3 py-2 text-xs font-mono text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-sm text-slate-800 dark:text-slate-200">%s</td>
  <td class="px-3 py-2 text-sm font-mono text-slate-700 dark:text-slate-300 whitespace-nowrap">%d - %d</td>
  <td class="px-3 py-2 text-right whitespace-nowrap">
    <a href="%s" class="px-3 py-1.5 rounded-lg text-xs font-bold bg-slate-700 hover:bg-slate-800 text-white">기록</a>
    <a href="%s" class="ml-2 px-3 py-1.5 rounded-lg text-xs font-bold bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">문자중계</a>
  </td>
</tr>|html}
          (escape_html g.qpmg_game_date)
          (escape_html matchup)
          g.qpmg_home_score
          g.qpmg_away_score
          (boxscore_href game_id_url)
          (boxscore_pbp_href game_id_url))
    |> String.concat "\n"
  in

  let sample_note =
    let shown = List.length report.qpmr_missing_sample in
    if report.qpmr_missing_games > shown then
      Printf.sprintf
        {html|<div class="text-xs text-slate-500 dark:text-slate-400">표시: 최근 %d경기 (전체 누락 %d경기)</div>|html}
        shown
        report.qpmr_missing_games
    else
      ""
  in

  let table_or_empty =
    if report.qpmr_missing_games = 0 then
      Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><div class="text-slate-600 dark:text-slate-400">%s</div></div>|html}
        (escape_html (tr { ko = "이 시즌에는 문자중계 누락 경기가 없습니다."; en = "No missing live text games for this season." }))
    else
      Printf.sprintf
        {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg">
  <div class="flex items-center justify-between gap-3">
    <h2 class="text-lg font-black text-slate-900 dark:text-slate-200">%s</h2>
    %s
  </div>
  <div class="mt-4 overflow-x-auto">
    <table class="min-w-[900px] w-full text-sm table-fixed" aria-label="문자중계 누락 경기 목록">
      <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]">
        <tr>
          <th scope="col" class="px-3 py-2 text-left">%s</th>
          <th scope="col" class="px-3 py-2 text-left">%s</th>
          <th scope="col" class="px-3 py-2 text-left">%s</th>
          <th scope="col" class="px-3 py-2 text-right">%s</th>
        </tr>
      </thead>
      <tbody>%s</tbody>
    </table>
  </div>
</div>|html}
        (escape_html (tr { ko = "누락 경기"; en = "Missing games" }))
        sample_note
        (escape_html (tr { ko = "날짜"; en = "Date" }))
        (escape_html (tr { ko = "매치업"; en = "Matchup" }))
        (escape_html (tr { ko = "스코어"; en = "Score" }))
        (escape_html (tr { ko = "보기"; en = "View" }))
        missing_rows
  in

  layout ~lang ~title:page_title
    ~canonical_path:"/tools/pbp-missing"
    ~description:"WKBL 플레이바이플레이 데이터 누락 경기 목록"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
  <div class="flex items-start justify-between gap-4">
    <div>
      <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s</h1>
      <div class="mt-1 text-sm text-slate-600 dark:text-slate-400">%s</div>
    </div>
    <form action="/qa/pbp-missing" method="get" class="flex items-center gap-2">
      <select name="season" onchange="this.form.submit()" class="bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-1.5 text-sm font-medium focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40 transition-colors">
        %s
      </select>
    </form>
  </div>

  <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
    %s
    %s
    %s
    %s
  </div>

  %s
</div>|html}
      (escape_html heading)
      (escape_html desc)
      season_options
      (kpi_card ~label:(tr { ko = "종료 경기"; en = "Finished games" }) ~value_html:(int_chip report.qpmr_finished_games_total) ~hint_html:(tr { ko = "스코어가 확인된 경기"; en = "Games with known final scores." }))
      (kpi_card ~label:(tr { ko = "문자중계 있음"; en = "With live text" }) ~value_html:(int_chip report.qpmr_pbp_games) ~hint_html:(tr { ko = "문자중계 이벤트가 저장된 경기"; en = "Games with saved live text events." }))
      (kpi_card ~label:(tr { ko = "누락"; en = "Missing" }) ~value_html:(int_chip report.qpmr_missing_games) ~hint_html:(tr { ko = "종료 경기 중 문자중계 없음"; en = "Finished games without live text." }))
      (kpi_card ~label:(tr { ko = "수집 비율"; en = "Coverage" }) ~value_html:(pct_chip report.qpmr_coverage_pct) ~hint_html:(Printf.sprintf "%s: %s" (tr { ko = "마지막 확인"; en = "Last checked" }) (escape_html (pretty_timestamp report.qpmr_generated_at))))
      table_or_empty) ()

(** Schedule Missing QA:
    Focuses on "completed schedule rows that do not match games" for seasons that have games ingested.
    This is primarily used to detect schedule sync issues and team-code mapping issues. *)
let qa_schedule_missing_page ?(lang=I18n.Ko) (report: Db.qa_schedule_missing_report) () =
  let tr = I18n.t lang in
  let page_title =
    tr { ko = "데이터 점검 | 일정 누락 | WKBL"; en = "Data Check | Schedule gaps | WKBL" }
  in
  let heading =
    tr { ko = "일정 누락(수집된 시즌)"; en = "Schedule gaps (ingested seasons)" }
  in
  let label_last_checked = tr { ko = "마지막 확인"; en = "Last checked" } in
  let desc =
    tr
      { ko = "종료된 일정 중에서 수집된 경기와 매칭되지 않는 항목입니다."
      ; en = "Completed schedule rows that do not match any ingested game."
      }
  in

  let int_chip v =
    Printf.sprintf {html|<div class="text-2xl font-black text-slate-900 dark:text-slate-200 font-mono tabular-nums">%d</div>|html} v
  in
  let kpi_card ~label ~value_html ~hint_html =
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg"><div class="text-slate-500 dark:text-slate-400 text-[11px] uppercase tracking-widest font-bold">%s</div><div class="mt-2">%s</div><div class="mt-2 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div></div>|html}
      (escape_html label)
      value_html
      hint_html
  in
  let reason_label reason =
    match reason with
    | "games_UNKNOWN_team_code" -> tr { ko = "팀 정보 누락"; en = "Missing team info" }
    | "team_code_mismatch" -> tr { ko = "팀 정보 불일치(같은 날짜)"; en = "Team mismatch (same date)" }
    | "schedule_alpha_code" -> tr { ko = "일정 팀 코드 형식 이상"; en = "Unexpected team code format" }
    | "schedule_allstar_AS" -> tr { ko = "올스타(AS) 임시 표기"; en = "All-star (AS) marker" }
    | "no_game_on_date" -> tr { ko = "해당 날짜 경기 없음"; en = "No game on that date" }
    | "home_away_swapped" -> tr { ko = "홈/원정 뒤바뀜"; en = "Home/away swapped" }
    | other -> other |> String.map (fun c -> if c = '_' then ' ' else c)
  in
  let reason_hint reason =
    match reason with
    | "games_UNKNOWN_team_code" ->
        tr
          { ko = "해당 날짜에 경기는 있으나 팀 정보가 비어 있어 매칭에 실패합니다."
          ; en = "Games exist on that date, but team info is missing."
          }
    | "team_code_mismatch" ->
        tr
          { ko = "해당 날짜에 경기는 있으나 홈/원정 팀 조합이 다릅니다."
          ; en = "Games exist on that date, but matchup differs (home/away)."
          }
    | "schedule_alpha_code" ->
        tr
          { ko = "일정의 팀 코드 형식이 예상과 다릅니다."
          ; en = "Unexpected team code format in schedule."
          }
    | "schedule_allstar_AS" ->
        tr
          { ko = "올스타 일정이 AS로 저장되어 일반 경기와 매칭되지 않습니다."
          ; en = "All-star items are marked as AS and won't match regular games."
          }
    | "no_game_on_date" ->
        tr
          { ko = "종료된 일정인데 해당 날짜의 경기가 없습니다."
          ; en = "Schedule is completed, but no game exists on that date."
          }
    | "home_away_swapped" ->
        tr
          { ko = "홈/원정이 뒤바뀐 경우입니다."
          ; en = "Home/away appears to be swapped."
          }
    | _ -> ""
  in
  let reason_rows =
    report.qsmr_reason_counts
    |> List.map (fun (reason, n) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">
  <td class="px-3 py-2 text-slate-900 dark:text-slate-200 text-xs font-mono">%s</td>
  <td class="px-3 py-2 text-slate-700 dark:text-slate-300 text-xs">%s</td>
  <td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td>
</tr>|html}
        (escape_html (reason_label reason))
        (escape_html (reason_hint reason))
        n)
    |> String.concat "\n"
  in
  let samples_by_reason =
    report.qsmr_samples
    |> List.fold_left (fun acc (row: Db.qa_schedule_missing_sample) ->
      let existing = match List.assoc_opt row.qsmp_reason acc with Some xs -> xs | None -> [] in
      (row.qsmp_reason, row :: existing) :: (List.remove_assoc row.qsmp_reason acc)
    ) []
    |> List.map (fun (reason, rows) -> (reason, List.rev rows))
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let label_samples = tr { ko = "표본"; en = "Samples" } in
  let sample_blocks =
    samples_by_reason
    |> List.map (fun (reason, rows) ->
      let sample_rows =
        rows
        |> List.map (fun (r: Db.qa_schedule_missing_sample) ->
          let matchup = Printf.sprintf "%s 대 %s" r.qsmp_home_team_code r.qsmp_away_team_code in
          let games_info = r.qsmp_games_info |> Option.value ~default:"" in
          Printf.sprintf
            {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">
  <td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono text-xs whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-slate-900 dark:text-slate-200 font-mono text-xs">%s</td>
  <td class="px-3 py-2 text-right font-mono text-xs text-slate-900 dark:text-slate-200 tabular-nums">%d</td>
  <td class="px-3 py-2 text-slate-700 dark:text-slate-300 font-mono text-[11px] break-all">%s</td>
</tr>|html}
            (escape_html r.qsmp_season_code)
            (escape_html r.qsmp_game_date)
            (escape_html matchup)
            r.qsmp_games_on_date
            (escape_html games_info))
        |> String.concat "\n"
      in
      Printf.sprintf
        {html|<details class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg"><summary class="cursor-pointer select-none text-slate-700 dark:text-slate-300 font-bold">%s <span class="ml-2 text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s %d</span></summary>
  <div class="mt-3 text-[11px] text-slate-500 dark:text-slate-400 leading-relaxed">%s</div>
  <div class="mt-4 overflow-x-auto">
	    <table class="min-w-[980px] w-full text-sm font-mono table-fixed" aria-label="일정 누락 샘플">
      <colgroup>
        <col style="width: 80px;">
        <col style="width: 110px;">
        <col style="width: 180px;">
        <col style="width: 90px;">
        <col style="width: auto;">
      </colgroup>
      <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]">
        <tr>
	          <th scope="col" class="px-3 py-2 text-left">시즌</th>
	          <th scope="col" class="px-3 py-2 text-left">날짜</th>
	          <th scope="col" class="px-3 py-2 text-left">대진</th>
	          <th scope="col" class="px-3 py-2 text-right">당일 경기 수</th>
	          <th scope="col" class="px-3 py-2 text-left">경기 목록</th>
	        </tr>
	      </thead>
      <tbody>%s</tbody>
    </table>
  </div>
</details>|html}
        (escape_html (reason_label reason))
        (escape_html label_samples)
        (List.length rows)
        (escape_html (reason_hint reason))
        sample_rows)
    |> String.concat "\n"
  in
  let s = report.qsmr_summary in
  layout ~lang ~title:page_title
    ~canonical_path:"/tools/score-mismatch"
    ~description:"WKBL 스코어 불일치 분석 - 박스스코어와 합산 점수 비교"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
  <div class="flex flex-col gap-2">
    <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s</h2>
    <div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">
      %s
      <span class="ml-2">%s: <span class="font-mono">%s</span></span>
    </div>
    <div class="text-slate-500 dark:text-slate-400 text-xs leading-relaxed">
      %s
    </div>
  </div>

  <div class="grid grid-cols-1 sm:grid-cols-3 gap-4">
    %s%s%s
  </div>

		  <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg">
		    <div class="flex items-center justify-between gap-3">
		      <h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">%s</h3>
		    </div>
		    <div class="mt-4 overflow-x-auto">
		      <table class="min-w-[860px] w-full text-sm table-fixed" aria-label="일정 누락 원인">
		        <colgroup>
		          <col style="width: 220px;">
		          <col style="width: auto;">
	          <col style="width: 90px;">
	        </colgroup>
		        <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]">
		          <tr>
		            <th scope="col" class="px-3 py-2 text-left">%s</th>
		            <th scope="col" class="px-3 py-2 text-left">%s</th>
		            <th scope="col" class="px-3 py-2 text-right">%s</th>
		          </tr>
		        </thead>
	        <tbody>%s</tbody>
	      </table>
	    </div>
	  </div>

	  <div class="space-y-4">%s</div>
</div>|html}
	      (escape_html heading)
	      (escape_html desc)
	      (escape_html label_last_checked)
	      (escape_html (pretty_timestamp report.qsmr_generated_at))
	      (escape_html (tr { ko = "참고: 경기가 아예 없는 시즌(미수집 시즌)은 별도로 집계합니다."; en = "Note: seasons with no games are counted separately." }))
      (kpi_card
         ~label:(tr { ko = "누락(수집된 시즌)"; en = "Missing (ingested)" })
         ~value_html:(int_chip s.qsms_missing_ingested)
         ~hint_html:(tr { ko = "경기가 수집된 시즌에서만 계산"; en = "Only seasons with games ingested." }))
      (kpi_card
         ~label:(tr { ko = "누락(미수집 시즌)"; en = "Missing (no games)" })
         ~value_html:(int_chip s.qsms_missing_uningested)
         ~hint_html:(tr { ko = "경기가 없는 시즌(미수집)에서 일정 누락"; en = "Seasons with no games." }))
      (kpi_card
         ~label:(tr { ko = "누락(전체)"; en = "Missing (total)" })
         ~value_html:(int_chip s.qsms_missing_total)
         ~hint_html:(tr { ko = "수집된 시즌 + 미수집 시즌"; en = "Ingested + no-game seasons." }))
	      (escape_html (tr { ko = "원인별 분류(수집된 시즌)"; en = "Breakdown (ingested seasons)" }))
	      (escape_html (tr { ko = "원인"; en = "Reason" }))
	      (escape_html (tr { ko = "설명"; en = "Description" }))
	      (escape_html (tr { ko = "건수"; en = "Count" }))
      reason_rows
      sample_blocks) ()

(** Draft / Trade (official) page *)
let transactions_page
  ?(lang=I18n.Ko)
  ~show_ops
  ~tab
  ~year
  ~q
  ~draft_years
  ~trade_years
  ~(draft_picks: draft_pick_row list)
  ~(trade_events: official_trade_event list)
  ()
  =
  let tab_value = tab |> String.trim |> String.lowercase_ascii in
  let active_tab = if tab_value = "trade" then "trade" else "draft" in
  let tab_link t label =
    let cls =
      if active_tab = t then
        "bg-slate-100 dark:bg-slate-800/80 border-slate-300 dark:border-slate-700 text-slate-900 dark:text-slate-200"
      else
        "bg-white dark:bg-slate-900/40 border-slate-200 dark:border-slate-800 text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white hover:bg-slate-100 dark:hover:bg-slate-800/50"
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
    Printf.sprintf {html|<option value="0" %s>전체</option>%s|html} all_selected base
  in
  let active_years = if active_tab = "trade" then trade_years else draft_years in
  let filter_form =
    Printf.sprintf
      {html|<form method="get" action="/transactions" class="flex flex-col sm:flex-row sm:items-end gap-3">
  <input type="hidden" name="tab" value="%s">
  <div class="block text-xs text-slate-500 dark:text-slate-400 space-y-1">
    <label for="filter-year" class="font-bold uppercase tracking-widest text-[10px]">연도</label>
    <select id="filter-year" name="year" class="bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none">
      %s
    </select>
  </div>
  <div class="block text-xs text-slate-500 dark:text-slate-400 space-y-1 min-w-0 flex-1">
    <label for="filter-search" class="font-bold uppercase tracking-widest text-[10px]">검색</label>
    <input id="filter-search" name="q" value="%s" placeholder="선수 / 팀 / 키워드" class="w-full bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none">
  </div>
  <button class="shrink-0 bg-orange-600 hover:bg-orange-500 text-slate-900 dark:text-slate-200 font-bold rounded-lg px-3 py-2 text-sm transition">적용</button>
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
          if show_ops then
            {html|<tr><td colspan="6" class="px-4 py-10 text-center text-slate-500 dark:text-slate-400 text-sm">드래프트 데이터가 아직 없습니다. (운영자: 수집 설정을 확인해주세요.)</td></tr>|html}
          else
            {html|<tr><td colspan="6" class="px-4 py-10 text-center text-slate-500 dark:text-slate-400 text-sm">드래프트 데이터가 아직 없습니다.</td></tr>|html}
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
                {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">
  <td class="px-3 py-2 text-slate-500 dark:text-slate-400 font-mono whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-slate-700 dark:text-slate-300 font-mono whitespace-nowrap">%s</td>
  <td class="px-3 py-2 min-w-0"><a class="text-slate-900 dark:text-slate-200 hover:text-orange-700 dark:text-orange-400 dark:hover:text-orange-300 font-bold truncate block" href="%s">%s</a><div class="mt-1 text-[11px] text-slate-500 dark:text-slate-400 font-mono">%s</div></td>
  <td class="px-3 py-2">%s</td>
  <td class="px-3 py-2 text-[11px] text-slate-700 dark:text-slate-300 font-mono whitespace-pre-line break-words">%s</td>
  <td class="px-3 py-2 text-[11px]"><a class="text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">출처</a></td>
</tr>|html}
                (escape_html y)
                (escape_html (pick_label r))
                (player_href r.dpr_player_id)
                (escape_html (normalize_name r.dpr_player_name))
                (escape_html r.dpr_player_id)
                team_html
                (escape_html r.dpr_raw_text)
                (escape_html r.dpr_source_url))
          |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-x-auto shadow-lg">
  <table class="min-w-[980px] w-full text-sm table-fixed tabular-nums" aria-label="드래프트 현황">
    <colgroup>
      <col style="width: 72px;"> <!-- Year -->
      <col style="width: 140px;"> <!-- Pick -->
      <col style="width: auto;"> <!-- Player -->
      <col style="width: 220px;"> <!-- Team -->
      <col style="width: auto;"> <!-- Raw -->
      <col style="width: 90px;"> <!-- Link -->
    </colgroup>
    <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] whitespace-nowrap">
      <tr>
	        <th scope="col" class="px-3 py-2 text-left">연도</th>
	        <th scope="col" class="px-3 py-2 text-left" title="지명 순번">순번</th>
	        <th scope="col" class="px-3 py-2 text-left">선수</th>
	        <th scope="col" class="px-3 py-2 text-left">팀</th>
	        <th scope="col" class="px-3 py-2 text-left" title="WKBL 원문">원문</th>
	        <th scope="col" class="px-3 py-2 text-left">링크</th>
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
          if show_ops then
            {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/50 p-5 text-slate-500 dark:text-slate-400 text-sm">이적 데이터가 아직 없습니다. (운영자: 수집 설정을 확인해주세요.)</div>|html}
          else
            {html|<div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800/50 p-5 text-slate-500 dark:text-slate-400 text-sm">이적 데이터가 아직 없습니다.</div>|html}
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
  <div class="text-[11px]"><a class="text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:text-slate-300 underline font-mono" href="%s" target="_blank" rel="noreferrer">출처</a></div>
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
    let intro_html =
      if show_ops then
        {html|WKBL 공식 페이지 원문 기반입니다. 드래프트는 공식 페이지의 선수 고유번호를 사용하고, 이적은 원문을 저장해 검색합니다.|html}
      else
        {html|WKBL 공식 페이지를 기준으로 정리했습니다.|html}
    in
    let sync_build_block =
      if show_ops then
        {html|<details class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-5 text-xs text-slate-500 dark:text-slate-400">
  <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">운영자 안내</summary>
  <div class="mt-2 space-y-2 leading-relaxed">
    <div>드래프트/이적 데이터는 별도의 수집 과정이 필요합니다.</div>
  </div>
</details>|html}
      else
        ""
    in
    Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">%s
  <div class="flex flex-col gap-2">
	    <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">드래프트 / 이적</h2>
    <div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">%s</div>
  </div>
  <div class="flex flex-wrap items-center gap-2">%s%s</div>
  %s
  %s
  %s
</div>|html}
      (breadcrumb [("홈", "/"); ("드래프트 / 이적", "")])
      intro_html
	      (tab_link "draft" "드래프트")
	      (tab_link "trade" "이적")
      filter_form
      section
      sync_build_block
  in
	  layout ~lang ~title:"드래프트 / 이적 | WKBL"
	    ~canonical_path:"/tools/draft"
	    ~description:"WKBL 드래프트 및 이적 기록"
	    ~content ()

(* ===== Fantasy Calculator ===== *)

(** Fantasy Calculator Page - Calculate fantasy points with custom weights *)
let rec fantasy_calculator_page
    ?(lang=I18n.Ko)
    ~season
    ~(seasons: season_info list)
    ~(rules: fantasy_scoring_rule)
    ~(scores: fantasy_player_score list)
    ()
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
    <h3 class="text-slate-700 dark:text-slate-300 font-bold uppercase tracking-wider text-xs">점수 설정</h3>
    <button type="button" onclick="resetRules()" class="text-[11px] text-slate-500 dark:text-slate-400 hover:text-orange-600 dark:hover:text-orange-400">기본값</button>
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
    점수는 득점, 리바운드, 어시스트, 스틸, 블록, 턴오버에 가중치를 곱해 합산합니다.
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
      (slider_input ~id:"pts" ~label:"득점" ~value:rules.fsr_points ~min_val:0.0 ~max_val:3.0 ~step:0.1)
      (slider_input ~id:"reb" ~label:"리바운드" ~value:rules.fsr_rebounds ~min_val:0.0 ~max_val:3.0 ~step:0.1)
      (slider_input ~id:"ast" ~label:"어시스트" ~value:rules.fsr_assists ~min_val:0.0 ~max_val:3.0 ~step:0.1)
      (slider_input ~id:"stl" ~label:"스틸" ~value:rules.fsr_steals ~min_val:0.0 ~max_val:5.0 ~step:0.1)
      (slider_input ~id:"blk" ~label:"블록" ~value:rules.fsr_blocks ~min_val:0.0 ~max_val:5.0 ~step:0.1)
      (slider_input ~id:"tov" ~label:"턴오버" ~value:rules.fsr_turnovers ~min_val:(-3.0) ~max_val:0.0 ~step:0.1)
  in
  let results_table = fantasy_results_table scores in
	  let content =
	    Printf.sprintf
	      {html|<div class="space-y-6 animate-fade-in">%s
	  <div class="flex flex-col gap-2">
	    <h2 class="text-2xl font-black text-slate-900 dark:text-slate-200">판타지 계산기</h2>
	    <div class="text-slate-500 dark:text-slate-400 text-sm leading-relaxed">
	      슬라이더를 조정해 나만의 판타지 포인트 가중치를 설정하세요. 결과는 실시간으로 업데이트됩니다.
	    </div>
	  </div>
	  <div class="flex items-center gap-3">
	    <label for="season-select" class="text-xs text-slate-500 dark:text-slate-400 font-bold uppercase tracking-widest">시즌</label>
	    <select id="season-select" class="bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700/60 rounded-lg px-3 py-2 text-sm text-slate-900 dark:text-slate-200 focus:border-orange-500 focus:outline-none"
	            onchange="window.location.href='/fantasy?season=' + this.value">
	      %s
	    </select>
	  </div>
  %s
  <div id="fantasy-results" data-skeleton="stats" data-skeleton-count="4">
    %s
  </div>
</div>|html}
      (breadcrumb [("홈", "/"); ("판타지 계산기", "")])
      season_options
      rules_form
      results_table
  in
  layout ~lang ~title:"판타지 계산기 | WKBL"
    ~canonical_path:"/tools/fantasy"
    ~description:"WKBL 판타지 포인트 계산기 - 커스텀 가중치로 선수별 판타지 점수 산출"
    ~content ()

(** Fantasy results table partial (for HTMX updates) *)
and fantasy_results_table (scores: fantasy_player_score list) =
  let sorted = List.sort (fun a b -> Float.compare b.fps_avg_score a.fps_avg_score) scores in
  let rows =
    sorted
    |> List.mapi (fun i (s: fantasy_player_score) ->
        let rank = i + 1 in
        let rank_class =
          if rank = 1 then "text-orange-700 dark:text-orange-400 font-black"
          else if rank <= 3 then "text-slate-700 dark:text-slate-300 font-bold"
          else "text-slate-500 dark:text-slate-400"
        in
        Printf.sprintf
          {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/30 transition-colors">
  <td class="px-3 py-2 %s tabular-nums">%d</td>
  <td class="px-3 py-2">
    <a href="%s" class="text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:hover:text-orange-400 font-bold">%s</a>
  </td>
  <td class="px-3 py-2">%s</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-slate-500 dark:text-slate-400">%d</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-slate-900 dark:text-slate-200 font-bold">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-orange-700 dark:text-orange-400 font-bold">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-[11px] text-slate-500 dark:text-slate-400">%.1f</td>
  <td class="px-3 py-2 text-right font-mono tabular-nums text-[11px] text-red-500 dark:text-red-400">%.1f</td>
</tr>|html}
          rank_class rank
          (player_href s.fps_player_id)
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
  <table class="min-w-[1100px] w-full text-sm table-fixed tabular-nums" aria-label="판타지 포인트 순위">
    <colgroup>
      <col style="width: 60px;"> <!-- # -->
      <col style="width: auto;"> <!-- Player -->
      <col style="width: 180px;"> <!-- Team -->
      <col style="width: 60px;">  <!-- GP -->
      <col style="width: 90px;">  <!-- Total -->
      <col style="width: 90px;">  <!-- AVG -->
      <col style="width: 70px;">  <!-- PTS -->
      <col style="width: 70px;">  <!-- REB -->
      <col style="width: 70px;">  <!-- AST -->
      <col style="width: 70px;">  <!-- STL -->
      <col style="width: 70px;">  <!-- BLK -->
      <col style="width: 70px;">  <!-- TOV -->
    </colgroup>
	    <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px] whitespace-nowrap">
	      <tr>
	        <th scope="col" class="px-3 py-2 text-left">#</th>
	        <th scope="col" class="px-3 py-2 text-left">선수</th>
	        <th scope="col" class="px-3 py-2 text-left">팀</th>
	        <th scope="col" class="px-3 py-2 text-right" title="출전 경기">GP</th>
	        <th scope="col" class="px-3 py-2 text-right" title="총점">총점</th>
	        <th scope="col" class="px-3 py-2 text-right" title="경기당 평균">평균</th>
	        <th scope="col" class="px-3 py-2 text-right" title="득점 기여">PTS</th>
	        <th scope="col" class="px-3 py-2 text-right" title="리바운드 기여">REB</th>
	        <th scope="col" class="px-3 py-2 text-right" title="어시스트 기여">AST</th>
	        <th scope="col" class="px-3 py-2 text-right" title="스틸 기여">STL</th>
	        <th scope="col" class="px-3 py-2 text-right" title="블록 기여">BLK</th>
	        <th scope="col" class="px-3 py-2 text-right" title="턴오버(감점)">TOV</th>
	      </tr>
	    </thead>
	    <tbody>%s</tbody>
	  </table>
	</div>
	<div class="text-[11px] text-slate-500 dark:text-slate-400 mt-2">
	  %d명 · 경기당 평균 점수 순
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
	        <h4 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest mb-4">맞대결 평균 기록</h4>
	        <div class="flex justify-between text-xs font-bold text-slate-500 dark:text-slate-400 mb-3">
	          <span class="text-orange-500">%s</span>
	          <span class="text-sky-500">%s</span>
	        </div>
        <div class="space-y-4">
          %s
          %s
          %s
          %s
          %s
	        </div>
	        <div class="mt-3 text-center text-[10px] text-slate-400 dark:text-slate-500">
	          총 %d경기 기준
	        </div>
	      </div>|html}
	      (escape_html p1_name) (escape_html p2_name)
	      (stat_bar "득점" summary.h2h_p1_avg_pts summary.h2h_p2_avg_pts)
	      (stat_bar "리바운드" summary.h2h_p1_avg_reb summary.h2h_p2_avg_reb)
	      (stat_bar "어시스트" summary.h2h_p1_avg_ast summary.h2h_p2_avg_ast)
	      (stat_bar "스틸" summary.h2h_p1_avg_stl summary.h2h_p2_avg_stl)
	      (stat_bar "블록" summary.h2h_p1_avg_blk summary.h2h_p2_avg_blk)
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
        (escape_html g.hg_game_date)
        p1_border p1_pct g.player1_pts
        p2_border p2_pct g.player2_pts
    in
    let bars = games |> List.map game_bar |> String.concat "\n" in
	    Printf.sprintf
	      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-5 shadow-lg">
	        <h4 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest mb-4">경기당 득점</h4>
	        <div class="flex justify-between text-xs font-bold text-slate-500 dark:text-slate-400 mb-3 px-[5.5rem]">
	          <span class="text-orange-500">%s</span>
	          <span class="text-sky-500">%s</span>
	        </div>
        <div class="space-y-2">
          %s
	        </div>
	        <div class="mt-3 text-center text-[10px] text-slate-400 dark:text-slate-500">
	          테두리는 이긴 쪽 선수를 표시합니다.
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
	        <h3 class="text-center text-slate-500 dark:text-slate-400 text-sm font-bold uppercase tracking-widest">맞대결 상세</h3>
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
  match flow_points with
  | [] ->
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-center text-slate-600 dark:text-slate-400 text-sm">득점흐름을 만들 기록이 없어요</div>|html}
  | first_pt :: rest ->
    (* Chart dimensions *)
    let width = 800 in
    let height = 300 in
    let padding_x = 50 in
    let padding_y = 40 in
    let chart_width = width - (2 * padding_x) in
    let chart_height = height - (2 * padding_y) in
    let last_pt = List.fold_left (fun _ p -> p) first_pt rest in

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

		    Printf.sprintf
		      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
		        <div class="flex items-center justify-between mb-4">
		          <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 flex items-center gap-2">
		            <span class="w-1 h-6 bg-orange-500 rounded-full"></span>
		            경기 흐름
		          </h3>
		          <div class="flex items-center gap-4 text-xs">
		            <div class="flex items-center gap-1">
		              <span class="w-3 h-3 rounded-full bg-sky-500"></span>
		              <span class="text-slate-600 dark:text-slate-400">%s 우세</span>
		            </div>
		            <div class="flex items-center gap-1">
		              <span class="w-3 h-3 rounded-full bg-orange-500"></span>
		              <span class="text-slate-600 dark:text-slate-400">%s 우세</span>
		            </div>
		          </div>
		        </div>
	        <div class="overflow-x-auto">
	          <svg viewBox="0 0 %d %d" class="w-full min-w-[600px]" preserveAspectRatio="xMidYMid meet">
	            <!-- Grid lines -->
	            %s
	            <!-- Zero baseline -->
	            %s
	            <!-- Area fill -->
	            <path d="%s" fill="currentColor" class="text-slate-400/15 dark:text-slate-700/20"/>
	            <!-- Line -->
	            <path d="%s" fill="none" stroke="currentColor" class="text-slate-700 dark:text-slate-200" stroke-width="2" stroke-linejoin="round"/>
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
	      quarter_lines
	      zero_line
	      area_path
	      path_d
	      data_points
	      quarter_labels
	      ot_labels
	      y_labels

(** Game flow page with chart and summary statistics *)
let game_flow_page ?(lang=I18n.Ko) ~(game: Domain.game_info) (flow_points: Domain.score_flow_point list) =
  let title = Printf.sprintf "경기 흐름: %s 대 %s" game.gi_home_team_name game.gi_away_team_name in
  let has_scoring =
    flow_points
    |> List.exists (fun (p: Domain.score_flow_point) -> (p.sfp_home_score + p.sfp_away_score) > 0)
  in

  match flow_points with
  | [] ->
      layout
        ~lang
        ~title
        ~canonical_path:(Printf.sprintf "/games/%s/flow" game.gi_game_id)
        ~description:(Printf.sprintf "%s 대 %s 경기 흐름 - %s" game.gi_home_team_name game.gi_away_team_name game.gi_game_date)
        ~content:(Printf.sprintf
          {html|<div class="space-y-6 animate-fade-in">%s
            <div class="text-center">
              <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s 대 %s</h1>
              <div class="text-slate-500 dark:text-slate-400 text-sm mt-1">%s</div>
              <div class="text-3xl font-black text-slate-900 dark:text-slate-200 mt-2">%d - %d</div>
            </div>
            <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-slate-600 dark:text-slate-400 text-sm">
              문자중계가 없는 경기라 득점흐름을 만들 수 없어요.
            </div>
            <div class="flex justify-center gap-4">
              <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">← 박스스코어</a>
              <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">문자중계 →</a>
            </div>
          </div>|html}
          (breadcrumb [("홈", "/"); ("경기 흐름", "")])
          (escape_html game.gi_home_team_name)
          (escape_html game.gi_away_team_name)
          (escape_html game.gi_game_date)
          game.gi_home_score
          game.gi_away_score
          (boxscore_href game.gi_game_id)
          (boxscore_pbp_href game.gi_game_id))
        ()
  | _ when not has_scoring ->
      layout
        ~lang
        ~title
        ~canonical_path:(Printf.sprintf "/games/%s/flow" game.gi_game_id)
        ~description:(Printf.sprintf "%s 대 %s 경기 흐름 - %s" game.gi_home_team_name game.gi_away_team_name game.gi_game_date)
        ~content:(Printf.sprintf
          {html|<div class="space-y-6 animate-fade-in">%s
            <div class="text-center">
              <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s 대 %s</h1>
              <div class="text-slate-500 dark:text-slate-400 text-sm mt-1">%s</div>
              <div class="text-3xl font-black text-slate-900 dark:text-slate-200 mt-2">%d - %d</div>
            </div>
            <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 text-slate-600 dark:text-slate-400 text-sm">
              득점흐름을 만들 기록이 없어요.
            </div>
            <div class="flex justify-center gap-4">
              <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">← 박스스코어</a>
              <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">문자중계 →</a>
            </div>
          </div>|html}
          (breadcrumb [("홈", "/"); ("경기 흐름", "")])
          (escape_html game.gi_home_team_name)
          (escape_html game.gi_away_team_name)
          (escape_html game.gi_game_date)
          game.gi_home_score
          game.gi_away_score
          (boxscore_href game.gi_game_id)
          (boxscore_pbp_href game.gi_game_id))
        ()
  | _ ->
      let chart =
        game_flow_chart
          ~home_team:game.gi_home_team_name
          ~away_team:game.gi_away_team_name
          flow_points
      in
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
              let new_changes =
                if current_leader <> prev_leader && prev_leader <> 0 then changes + 1 else changes
              in
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
        calc_times 0 0 0 0 flow_points
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
              <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">리드 교체</div>
            </div>
            <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
              <div class="text-2xl font-black text-sky-600 dark:text-sky-400 font-mono">+%d</div>
              <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">%s 최대 리드</div>
            </div>
            <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
              <div class="text-2xl font-black text-orange-700 dark:text-orange-400 font-mono">+%d</div>
              <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">%s 최대 리드</div>
            </div>
            <div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 p-4 text-center">
              <div class="text-lg font-bold text-slate-900 dark:text-slate-200 font-mono">
                <span class="text-sky-600 dark:text-sky-400">%s</span> /
                <span class="text-orange-700 dark:text-orange-400">%s</span>
              </div>
              <div class="text-xs text-slate-500 dark:text-slate-400 uppercase tracking-wider mt-1">리드 시간</div>
            </div>
          </div>|html}
          lead_changes
          biggest_home_lead
          (escape_html game.gi_home_team_name)
          biggest_away_lead
          (escape_html game.gi_away_team_name)
          (format_time home_lead_time)
          (format_time away_lead_time)
      in

      layout
        ~lang
        ~title
        ~canonical_path:(Printf.sprintf "/games/%s/flow" game.gi_game_id)
        ~description:(Printf.sprintf "%s 대 %s 경기 흐름 - %s" game.gi_home_team_name game.gi_away_team_name game.gi_game_date)
        ~content:(Printf.sprintf
          {html|<div class="space-y-6 animate-fade-in">%s
            <div class="text-center">
              <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s 대 %s</h1>
              <div class="text-slate-500 dark:text-slate-400 text-sm mt-1">%s</div>
              <div class="text-3xl font-black text-slate-900 dark:text-slate-200 mt-2">%d - %d</div>
            </div>
            %s
            %s
            <div class="flex justify-center gap-4">
              <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">← 박스스코어</a>
              <a href="%s" class="px-3 py-2 rounded-lg bg-slate-100 dark:bg-slate-800/60 border border-slate-300 dark:border-slate-700 text-sm text-slate-900 dark:text-slate-200 hover:border-slate-500 transition">문자중계 →</a>
            </div>
          </div>|html}
          (breadcrumb [("홈", "/"); ("경기 흐름", "")])
          (escape_html game.gi_home_team_name)
          (escape_html game.gi_away_team_name)
          (escape_html game.gi_game_date)
          game.gi_home_score
          game.gi_away_score
          chart
          stats_section
          (boxscore_href game.gi_game_id)
          (boxscore_pbp_href game.gi_game_id))
        ()

(* ===== Lineup Chemistry Views ===== *)

(** Render a single lineup row *)
let render_lineup_row (lineup: Domain.lineup_stats) (rank: int) : string =
  let players_html = lineup.ls_players
    |> List.map (fun (p: Domain.lineup_player) ->
        Printf.sprintf {html|<a href="%s" class="inline-block px-2 py-1 text-xs bg-slate-100 dark:bg-slate-800 rounded mr-1 mb-1 hover:bg-sky-100 dark:hover:bg-sky-900/40 hover:text-sky-700 dark:hover:text-sky-400 transition-colors">%s</a>|html}
          (player_href p.lp_player_id)
          (escape_html p.lp_player_name))
    |> String.concat "" in
  let pm_class = if lineup.ls_plus_minus >= 0
    then "text-green-600 dark:text-green-400"
    else "text-red-600 dark:text-red-400" in
  let pm_sign = if lineup.ls_plus_minus >= 0 then "+" else "" in
  Printf.sprintf
    {html|<tr class="border-b border-slate-200 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-900/50">
      <td class="px-3 py-2 text-center font-mono text-sm">%d</td>
      <td class="px-3 py-2">
        <div class="flex flex-wrap">%s</div>
      </td>
      <td class="px-3 py-2 text-center font-mono text-sm">%d</td>
      <td class="px-3 py-2 text-center font-mono text-sm">%.1f</td>
      <td class="px-3 py-2 text-center font-mono text-sm">%d</td>
      <td class="px-3 py-2 text-center font-mono text-sm %s">%s%d</td>
      <td class="px-3 py-2 text-center font-mono text-sm">%.2f</td>
    </tr>|html}
    rank
    players_html
    lineup.ls_games_together
    lineup.ls_total_minutes
    lineup.ls_total_pts
    pm_class pm_sign lineup.ls_plus_minus
    lineup.ls_avg_margin_per_min

(** Render a synergy row *)
let render_synergy_row (syn: Domain.lineup_synergy) (rank: int) : string =
  let score_class = if syn.syn_synergy_score >= 0.0
    then "text-green-600 dark:text-green-400"
    else "text-red-600 dark:text-red-400" in
  let pm_class = if syn.syn_avg_plus_minus >= 0.0
    then "text-green-600 dark:text-green-400"
    else "text-red-600 dark:text-red-400" in
  Printf.sprintf
    {html|<tr class="border-b border-slate-200 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-900/50">
      <td class="px-3 py-2 text-center font-mono text-sm">%d</td>
      <td class="px-3 py-2 text-sm"><a href="%s" class="text-sky-600 dark:text-sky-400 hover:underline">%s</a></td>
      <td class="px-3 py-2 text-sm"><a href="%s" class="text-sky-600 dark:text-sky-400 hover:underline">%s</a></td>
      <td class="px-3 py-2 text-center font-mono text-sm">%d</td>
      <td class="px-3 py-2 text-center font-mono text-sm">%.1f</td>
      <td class="px-3 py-2 text-center font-mono text-sm %s">%+.2f</td>
      <td class="px-3 py-2 text-center font-mono text-sm font-bold %s">%.2f</td>
    </tr>|html}
    rank
    (player_href syn.syn_player1_id)
    (escape_html syn.syn_player1_name)
    (player_href syn.syn_player2_id)
    (escape_html syn.syn_player2_name)
    syn.syn_games_together
    syn.syn_total_minutes
    pm_class syn.syn_avg_plus_minus
    score_class syn.syn_synergy_score

(** Render lineup table *)
let render_lineup_table ~title (lineups: Domain.lineup_stats list) : string =
  if lineups = [] then
    Printf.sprintf {html|<div class="text-center text-slate-500 dark:text-slate-400 py-8">%s: No data available</div>|html} title
  else
    let rows = lineups |> List.mapi (fun i l -> render_lineup_row l (i + 1)) |> String.concat "\n" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden">
        <div class="px-3 py-2 bg-slate-50 dark:bg-slate-800/50 border-b border-slate-200 dark:border-slate-700">
          <h2 class="font-bold text-slate-900 dark:text-slate-200">%s</h2>
        </div>
        <div class="overflow-x-auto">
	          <table class="w-full text-sm table-fixed font-mono tabular-nums" aria-label="라인업 통계">
	            <colgroup>
	              <col style="width: 50px;">  <!-- # -->
	              <col style="width: auto;">  <!-- Players -->
	              <col style="width: 70px;">  <!-- Games -->
	              <col style="width: 70px;">  <!-- Min -->
	              <col style="width: 60px;">  <!-- Pts -->
              <col style="width: 60px;">  <!-- +/- -->
              <col style="width: 80px;">  <!-- +/-/min -->
            </colgroup>
	            <thead class="bg-slate-50 dark:bg-slate-800/30">
	              <tr class="text-left text-xs uppercase tracking-wider text-slate-500 dark:text-slate-400">
	                <th scope="col" class="px-3 py-2 text-center font-sans">#</th>
	                <th scope="col" class="px-3 py-2 font-sans">선수</th>
	                <th scope="col" class="px-3 py-2 text-center font-sans" title="출전 경기">경기</th>
	                <th scope="col" class="px-3 py-2 text-center font-sans" title="출전 시간(분)">분</th>
	                <th scope="col" class="px-3 py-2 text-center font-sans" title="득점">득점</th>
	                <th scope="col" class="px-3 py-2 text-center font-sans" title="득실(+/-)">+/-</th>
	                <th scope="col" class="px-3 py-2 text-center font-sans" title="분당 득실">+/-/분</th>
	              </tr>
	            </thead>
            <tbody class="text-slate-700 dark:text-slate-300">
              %s
            </tbody>
          </table>
        </div>
      </div>|html}
      title rows

(** Render synergy table *)
let render_synergy_table (synergies: Domain.lineup_synergy list) : string =
  if synergies = [] then
    {html|<div class="text-center text-slate-500 dark:text-slate-400 py-8">시너지 데이터가 없습니다</div>|html}
  else
    let rows = synergies |> List.mapi (fun i s -> render_synergy_row s (i + 1)) |> String.concat "\n" in
    Printf.sprintf
      {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden">
        <div class="px-3 py-2 bg-slate-50 dark:bg-slate-800/50 border-b border-slate-200 dark:border-slate-700">
          <h2 class="font-bold text-slate-900 dark:text-slate-200">선수 시너지 (상위 조합)</h2>
        </div>
        <div class="overflow-x-auto">
          <table class="w-full text-sm table-fixed font-mono tabular-nums" aria-label="선수 시너지 순위">
            <colgroup>
              <col style="width: 50px;">  <!-- # -->
              <col style="width: auto;">  <!-- P1 -->
              <col style="width: auto;">  <!-- P2 -->
              <col style="width: 70px;">  <!-- Games -->
              <col style="width: 70px;">  <!-- Min -->
              <col style="width: 80px;">  <!-- +/-/min -->
              <col style="width: 80px;">  <!-- Synergy -->
            </colgroup>
            <thead class="bg-slate-50 dark:bg-slate-800/30">
              <tr class="text-left text-xs uppercase tracking-wider text-slate-500 dark:text-slate-400">
                <th scope="col" class="px-3 py-2 text-center font-sans">#</th>
                <th scope="col" class="px-3 py-2 font-sans">선수 1</th>
                <th scope="col" class="px-3 py-2 font-sans">선수 2</th>
                <th scope="col" class="px-3 py-2 text-center font-sans" title="함께 뛴 경기">경기</th>
                <th scope="col" class="px-3 py-2 text-center font-sans" title="함께 뛴 시간(분)">분</th>
                <th scope="col" class="px-3 py-2 text-center font-sans" title="분당 +/-">+/-/분</th>
                <th scope="col" class="px-3 py-2 text-center font-sans" title="궁합 점수">궁합</th>
              </tr>
            </thead>
            <tbody class="text-slate-700 dark:text-slate-300">
              %s
            </tbody>
          </table>
        </div>
      </div>|html}
      rows

(** Full lineup chemistry page *)
let lineup_chemistry_page
    ?(lang=I18n.Ko)
    ~teams ~seasons
    ~selected_team ~selected_season
    (chemistry: Domain.lineup_chemistry) : string =

  let team_options = teams
    |> List.map (fun t ->
        let selected = if t.Domain.team_code = selected_team then " selected" else "" in
        Printf.sprintf {|<option value="%s"%s>%s</option>|}
          (escape_html t.team_code) selected (escape_html t.team_name))
    |> String.concat "\n" in

  let season_options = seasons
    |> List.map (fun s ->
        let selected = if s.Domain.code = selected_season then " selected" else "" in
        Printf.sprintf {|<option value="%s"%s>%s</option>|}
          (escape_html s.code) selected (escape_html s.name))
    |> String.concat "\n" in

	  let filter_form = Printf.sprintf
	    {html|<form class="flex flex-wrap gap-4 items-end mb-6" hx-get="/lineups/table" hx-target="#lineup-content" hx-swap="innerHTML">
	      <div>
	        <label class="block text-xs text-slate-500 dark:text-slate-400 mb-1">팀</label>
	        <select name="team" class="px-3 py-2 rounded-lg bg-white dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-sm">
	          <option value="ALL"%s>전체 팀</option>
	          %s
	        </select>
	      </div>
	      <div>
	        <label class="block text-xs text-slate-500 dark:text-slate-400 mb-1">시즌</label>
	        <select name="season" class="px-3 py-2 rounded-lg bg-white dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-sm">
	          <option value="ALL"%s>전체 시즌</option>
	          %s
	        </select>
	      </div>
	      <button type="submit" class="px-3 py-2 rounded-lg bg-sky-600 hover:bg-sky-700 text-white text-sm font-medium transition">
	        적용
	      </button>
	    </form>|html}
    (if selected_team = "ALL" then " selected" else "") team_options
    (if selected_season = "ALL" then " selected" else "") season_options
  in

	  let frequent_table = render_lineup_table
	    ~title:"자주 쓰는 라인업 (출전 시간 TOP 10)"
	    chemistry.lc_frequent_lineups in

	  let top_table = render_lineup_table
	    ~title:"성과 좋은 라인업 (+/- TOP 10)"
	    chemistry.lc_top_lineups in

  let synergy_table = render_synergy_table chemistry.lc_synergies in

	  layout ~lang ~title:"라인업 궁합 | WKBL"
      ~canonical_path:"/lineups"
      ~description:"WKBL 여자프로농구 라인업 궁합 분석 - 5인 조합별 +/- 및 시너지"
	    ~content:(Printf.sprintf
	      {html|<div class="space-y-6 animate-fade-in">%s
	        <div class="flex items-center justify-between flex-wrap gap-3">
	          <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">라인업 궁합</h1>
	          <div class="flex items-center gap-3">
	            <a href="/on-off?season=%s" class="text-sm text-sky-600 dark:text-sky-400 hover:underline">온/오프 영향력 →</a>
	          </div>
	        </div>
	        %s
	        <div id="lineup-content" class="space-y-6" data-skeleton="cards" data-skeleton-count="3">
          %s
          %s
          %s
        </div>
      </div>|html}
      (breadcrumb [("홈", "/"); ("라인업 궁합", "")])
      (escape_html selected_season)
      filter_form
      frequent_table
      top_table
      synergy_table) ()

(** Lineup chemistry table content (for HTMX partial update) *)
let lineup_chemistry_table_content (chemistry: Domain.lineup_chemistry) : string =
  let frequent_table = render_lineup_table
    ~title:"자주 쓰는 라인업 (출전 시간 TOP 10)"
    chemistry.lc_frequent_lineups in

  let top_table = render_lineup_table
    ~title:"성과 좋은 라인업 (+/- TOP 10)"
    chemistry.lc_top_lineups in

  let synergy_table = render_synergy_table chemistry.lc_synergies in

  Printf.sprintf {html|%s
%s
%s|html}
    frequent_table top_table synergy_table

(* ===== On/Off Impact Page ===== *)

(** Format plus/minus with color *)
let format_plus_minus (pm: float) : string =
  if pm > 0.0 then
    Printf.sprintf {html|<span class="text-emerald-600 dark:text-emerald-400 font-semibold">+%.1f</span>|html} pm
  else if pm < 0.0 then
    Printf.sprintf {html|<span class="text-rose-600 dark:text-rose-400 font-semibold">%.1f</span>|html} pm
  else
    {html|<span class="text-slate-500 dark:text-slate-400">0.0</span>|html}

let format_plus_minus_int (pm: int) : string =
  if pm > 0 then
    Printf.sprintf {html|<span class="text-emerald-600 dark:text-emerald-400 font-semibold">+%d</span>|html} pm
  else if pm < 0 then
    Printf.sprintf {html|<span class="text-rose-600 dark:text-rose-400 font-semibold">%d</span>|html} pm
  else
    {html|<span class="text-slate-500 dark:text-slate-400">0</span>|html}

(** On/Off Impact table *)
let on_off_impact_table (impacts: Domain.on_off_impact list) : string =
  let rows = impacts |> List.mapi (fun i (impact: Domain.on_off_impact) ->
    let rank = i + 1 in
    let rank_badge =
      if rank <= 3 then
        Printf.sprintf {html|<span class="inline-flex items-center justify-center w-6 h-6 rounded-full bg-amber-100 dark:bg-amber-900/30 text-amber-700 dark:text-amber-400 text-xs font-bold">%d</span>|html} rank
      else
        Printf.sprintf {html|<span class="text-slate-500 dark:text-slate-400">%d</span>|html} rank
    in
    Printf.sprintf
      {html|<tr class="border-b border-slate-200/50 dark:border-slate-800/50 hover:bg-slate-50/50 dark:hover:bg-slate-800/30 transition-colors">
        <td class="py-2 px-4 text-center">%s</td>
        <td class="py-2 px-4">
          <a href="%s" class="font-semibold text-sky-600 dark:text-sky-400 hover:underline">%s</a>
          <div class="text-xs text-slate-500 dark:text-slate-400">%s</div>
        </td>
        <td class="py-2 px-4 text-center font-mono tabular-nums">%d</td>
        <td class="py-2 px-4 text-center font-mono tabular-nums">%.1f</td>
        <td class="py-2 px-4 text-center font-mono tabular-nums">%s</td>
        <td class="py-2 px-4 text-center font-mono tabular-nums">%s</td>
        <td class="py-2 px-4 text-center font-mono tabular-nums">%d</td>
      </tr>|html}
      rank_badge
      (player_href impact.ooi_player_id)
      (escape_html impact.ooi_player_name)
      (team_badge ~max_width:"max-w-[100px]" impact.ooi_team_name)
      impact.ooi_games_played
      impact.ooi_total_minutes
      (format_plus_minus impact.ooi_plus_minus_avg)
      (format_plus_minus_int impact.ooi_plus_minus_total)
      impact.ooi_on_court.ocs_games
  ) |> String.concat "\n" in

  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 shadow-lg overflow-hidden">
      <div class="overflow-x-auto">
        <table class="w-full text-sm table-fixed" aria-label="온/오프 영향력">
          <colgroup>
            <col style="width: 64px;"> <!-- # -->
            <col style="width: auto;"> <!-- Player -->
            <col style="width: 60px;">  <!-- GP -->
            <col style="width: 80px;">  <!-- MIN -->
            <col style="width: 100px;"> <!-- +/- AVG -->
            <col style="width: 100px;"> <!-- +/- Total -->
            <col style="width: 100px;"> <!-- Games w/ +/- -->
          </colgroup>
          <thead class="bg-slate-50 dark:bg-slate-800/50 text-slate-700 dark:text-slate-300">
	            <tr>
	              <th scope="col" class="py-2 px-4 text-center font-semibold">#</th>
	              <th scope="col" class="py-2 px-4 text-left font-semibold">선수</th>
	              <th scope="col" class="py-2 px-4 text-center font-semibold" title="출전 경기">GP</th>
	              <th scope="col" class="py-2 px-4 text-center font-semibold" title="출전 시간(분)">MIN</th>
	              <th scope="col" class="py-2 px-4 text-center font-semibold" title="경기당 평균 +/-">+/- AVG</th>
	              <th scope="col" class="py-2 px-4 text-center font-semibold" title="누적 +/-">+/- Total</th>
	              <th scope="col" class="py-2 px-4 text-center font-semibold" title="+/-가 있는 경기">+/- 경기</th>
	            </tr>
	          </thead>
          <tbody class="text-slate-700 dark:text-slate-300">
            %s
          </tbody>
        </table>
      </div>
    </div>|html}
    rows

(** On/Off Impact page *)
let on_off_impact_page
    ?(lang=I18n.Ko)
    ~season
    ~(seasons: Domain.season_info list)
    (impacts: Domain.on_off_impact list)
    : string =
  let season_options = seasons |> List.map (fun (s: Domain.season_info) ->
    Printf.sprintf {html|<option value="%s"%s>%s</option>|html}
      s.code
      (if s.code = season then " selected" else "")
      (escape_html s.name)
  ) |> String.concat "\n" in

	  let filter_form = Printf.sprintf
	    {html|<form method="get" action="/on-off" class="flex flex-wrap items-end gap-4 p-4 bg-slate-100 dark:bg-slate-800/50 rounded-xl mb-6">
	      <div>
	        <label class="block text-xs text-slate-500 dark:text-slate-400 mb-1">시즌</label>
	        <select name="season" class="px-3 py-2 rounded-lg bg-white dark:bg-slate-800 border border-slate-300 dark:border-slate-700 text-sm">
	          <option value="ALL"%s>전체 시즌</option>
	          %s
	        </select>
	      </div>
	      <button type="submit" class="px-3 py-2 rounded-lg bg-sky-600 hover:bg-sky-700 text-white text-sm font-medium transition">
	        적용
	      </button>
	    </form>|html}
    (if season = "ALL" then " selected" else "") season_options
  in

	  let content =
	    if List.length impacts = 0 then
	      {html|<div class="text-center py-12 text-slate-500 dark:text-slate-400">
	        <div class="text-4xl mb-4">📊</div>
	        <div>이 시즌의 온/오프 영향력 데이터가 없습니다.</div>
	        <div class="text-sm mt-2">최소 5경기, 50분 이상 출전한 선수만 계산됩니다.</div>
	      </div>|html}
	    else
	      on_off_impact_table impacts
  in

  (* Summary stats *)
  let total_players = List.length impacts in
  let avg_pm =
    if total_players > 0 then
      let sum = List.fold_left (fun acc (i: Domain.on_off_impact) -> acc +. i.ooi_plus_minus_avg) 0.0 impacts in
      sum /. float_of_int total_players
    else 0.0
  in
  let best_player = match impacts with
    | [] -> None
    | h :: _ -> Some h
  in

	  let summary_cards = Printf.sprintf
	    {html|<div class="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
	      <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
	        <div class="text-slate-500 dark:text-slate-400 text-xs uppercase tracking-widest font-bold">집계된 선수</div>
	        <div class="text-2xl font-black text-slate-900 dark:text-slate-200 mt-1 font-mono tabular-nums">%d</div>
	      </div>
	      <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
	        <div class="text-slate-500 dark:text-slate-400 text-xs uppercase tracking-widest font-bold">경기당 평균 +/-</div>
	        <div class="text-2xl font-black mt-1 font-mono tabular-nums">%s</div>
	      </div>
	      <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 shadow-lg">
	        <div class="text-slate-500 dark:text-slate-400 text-xs uppercase tracking-widest font-bold">최고 영향력 선수</div>
	        <div class="text-lg font-black text-slate-900 dark:text-slate-200 mt-1">%s</div>
	      </div>
	    </div>|html}
    total_players
    (format_plus_minus avg_pm)
    (match best_player with
     | Some p -> Printf.sprintf {html|<a href="%s" class="text-sky-600 dark:text-sky-400 hover:underline">%s</a>|html}
         (player_href p.ooi_player_id) (escape_html p.ooi_player_name)
     | None -> "-")
  in

  layout ~lang ~title:"온/오프 영향력 | WKBL"
    ~canonical_path:"/on-off-impact"
    ~description:"WKBL 여자프로농구 온/오프 영향력 - 선수별 +/- 분석"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">%s
        <div class="flex items-center justify-between flex-wrap gap-3">
          <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">온/오프 영향력</h1>
          <div class="flex items-center gap-3">
            <a href="/lineups?season=%s" class="text-sm text-sky-600 dark:text-sky-400 hover:underline">← 라인업 궁합</a>
          </div>
        </div>
        %s
        %s
        %s
        <div class="text-xs text-slate-500 dark:text-slate-400 mt-4">
          <p><strong>+/-:</strong> 코트에 있을 때의 득실 차이입니다. +면 팀 득점이 더 많고, -면 실점이 더 많습니다.</p>
          <p class="mt-1"><strong>참고:</strong> +/- 데이터는 공식 기록 제공 범위에 따라 일부 경기에서만 제공됩니다.</p>
        </div>
      </div>|html}
      (breadcrumb [("홈", "/"); ("온/오프 영향력", "")])
      (escape_html season)
      filter_form summary_cards content) ()

(* ============================================= *)
(* QA: Data Overrides (Exclude / Restore)        *)
(* ============================================= *)

let format_mmss (seconds : int) : string =
  let s = if seconds < 0 then 0 else seconds in
  let mm = s / 60 in
  let ss = s mod 60 in
  Printf.sprintf "%d:%02d" mm ss

let qa_anomalies_page
    ?(lang=I18n.Ko)
    ~(season:string)
    ~(seasons: season_info list)
    ~(candidates: Db.qa_stat_anomaly list)
    ~(exclusions: Db.qa_stat_exclusion list)
    ()
  =
  let tr = I18n.t lang in
  let page_title = tr { ko = "기록 제외/복구 | WKBL"; en = "Exclude/Restore Stats | WKBL" } in
  let heading = tr { ko = "기록 제외/복구"; en = "Exclude / Restore" } in
  let desc =
    tr
      { ko = "공식 기록의 명백한 오류로 보이는 행을 제외하거나, 되돌릴 수 있습니다."
      ; en = "Exclude obviously-wrong rows from public aggregates, or restore them."
      }
  in
  let label_all_seasons = tr { ko = "전체 시즌"; en = "All seasons" } in
  let label_candidates = tr { ko = "추천 후보"; en = "Suggested" } in
  let label_excluded = tr { ko = "제외된 기록"; en = "Excluded" } in
  let label_manual = tr { ko = "직접 제외"; en = "Manual exclude" } in
  let label_exclude = tr { ko = "제외"; en = "Exclude" } in
  let label_restore = tr { ko = "복구"; en = "Restore" } in
  let label_reason = tr { ko = "사유"; en = "Reason" } in
  let label_when = tr { ko = "시각"; en = "When" } in
  let label_game_id = tr { ko = "경기 ID"; en = "Game ID" } in
  let label_player_id = tr { ko = "선수 ID"; en = "Player ID" } in
  let ph_game_id = tr { ko = "게임 ID (예: 046-01-50)"; en = "Game ID (e.g., 046-01-50)" } in
  let ph_player_id = tr { ko = "선수 ID (예: 095533)"; en = "Player ID (e.g., 095533)" } in
  let ph_reason = tr { ko = "사유 (선택)"; en = "Reason (optional)" } in

  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
        Printf.sprintf
          {|<option value="%s" %s>%s</option>|}
          (escape_html s.code)
          (if s.code = season then "selected" else "")
          (escape_html s.name)
      )
      |> String.concat ""
    in
    Printf.sprintf
      {html|<option value="ALL" %s>%s</option>%s|html}
      (if season = "ALL" then "selected" else "")
      (escape_html label_all_seasons)
      base
  in

  let candidates_rows =
    candidates
    |> List.map (fun (r: Db.qa_stat_anomaly) ->
      let reason =
        tr
          { ko = "한 경기만 기록(짧은 출전)이라 오기록 가능성이 있어 보입니다."
          ; en = "One-game, very-low-minutes row looks suspicious."
          }
      in
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">
  <td class="px-3 py-2 text-xs font-mono text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-xs font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap"><a href="%s" class="hover:underline">%s</a></td>
  <td class="px-3 py-2 text-xs text-slate-900 dark:text-slate-200 whitespace-nowrap"><a href="%s" class="hover:underline">%s</a></td>
  <td class="px-3 py-2 text-xs text-slate-900 dark:text-slate-200 whitespace-nowrap"><a href="%s" class="hover:underline">%s</a><span class="ml-2 text-[10px] font-mono text-slate-400">#%s</span></td>
  <td class="px-3 py-2 text-xs font-mono text-right tabular-nums">%s</td>
  <td class="px-3 py-2 text-xs font-mono text-right tabular-nums">%d</td>
  <td class="px-3 py-2 text-xs text-slate-700 dark:text-slate-300 whitespace-nowrap">%s <span class="ml-2 text-[10px] font-mono text-slate-400">(GP %d)</span></td>
  <td class="px-3 py-2 text-right">
    <form action="/qa/anomalies/exclude" method="post" class="inline-flex items-center gap-2">
      <input type="hidden" name="season" value="%s">
      <input type="hidden" name="game_id" value="%s">
      <input type="hidden" name="player_id" value="%s">
      <input type="hidden" name="reason" value="%s">
      <button type="submit" class="px-3 py-1.5 rounded-lg text-xs font-bold bg-rose-600 hover:bg-rose-700 text-white">%s</button>
    </form>
  </td>
</tr>|html}
        (escape_html r.qsa_game_date)
        (boxscore_href r.qsa_game_id)
        (escape_html r.qsa_game_id)
        (team_href r.qsa_team_name)
        (escape_html r.qsa_team_name)
        (player_href r.qsa_player_id)
        (escape_html r.qsa_player_name)
        (escape_html r.qsa_player_id)
        (escape_html (format_mmss r.qsa_min_seconds))
        r.qsa_pts
        (escape_html r.qsa_primary_team_name)
        r.qsa_primary_gp
        (escape_html season)
        (escape_html r.qsa_game_id)
        (escape_html r.qsa_player_id)
        (escape_html reason)
        (escape_html label_exclude))
    |> String.concat "\n"
  in

  let exclusions_rows =
    exclusions
    |> List.map (fun (r: Db.qa_stat_exclusion) ->
      Printf.sprintf
        {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-100 dark:hover:bg-slate-800/50 transition-colors">
  <td class="px-3 py-2 text-xs font-mono text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-xs font-mono text-slate-900 dark:text-slate-200 whitespace-nowrap"><a href="%s" class="hover:underline">%s</a></td>
  <td class="px-3 py-2 text-xs text-slate-900 dark:text-slate-200 whitespace-nowrap"><a href="%s" class="hover:underline">%s</a></td>
  <td class="px-3 py-2 text-xs text-slate-900 dark:text-slate-200 whitespace-nowrap"><a href="%s" class="hover:underline">%s</a><span class="ml-2 text-[10px] font-mono text-slate-400">#%s</span></td>
  <td class="px-3 py-2 text-xs font-mono text-right tabular-nums">%s</td>
  <td class="px-3 py-2 text-xs font-mono text-right tabular-nums">%d</td>
  <td class="px-3 py-2 text-xs text-slate-700 dark:text-slate-300">%s</td>
  <td class="px-3 py-2 text-xs font-mono text-slate-500 dark:text-slate-400 whitespace-nowrap">%s</td>
  <td class="px-3 py-2 text-right">
    <form action="/qa/anomalies/restore" method="post" class="inline-flex items-center gap-2">
      <input type="hidden" name="season" value="%s">
      <input type="hidden" name="game_id" value="%s">
      <input type="hidden" name="player_id" value="%s">
      <button type="submit" class="px-3 py-1.5 rounded-lg text-xs font-bold bg-slate-700 hover:bg-slate-800 text-white">%s</button>
    </form>
  </td>
</tr>|html}
        (escape_html r.qse_game_date)
        (boxscore_href r.qse_game_id)
        (escape_html r.qse_game_id)
        (team_href r.qse_team_name)
        (escape_html r.qse_team_name)
        (player_href r.qse_player_id)
        (escape_html r.qse_player_name)
        (escape_html r.qse_player_id)
        (escape_html (format_mmss r.qse_min_seconds))
        r.qse_pts
        (escape_html r.qse_reason)
        (escape_html (pretty_timestamp r.qse_created_at))
        (escape_html season)
        (escape_html r.qse_game_id)
        (escape_html r.qse_player_id)
        (escape_html label_restore))
    |> String.concat "\n"
  in

  layout ~lang ~title:page_title
    ~canonical_path:"/tools/qa-anomalies"
    ~description:"WKBL 데이터 이상치 탐지 - 통계적으로 비정상적인 경기 기록 분석"
    ~content:(Printf.sprintf
      {html|<div class="space-y-6 animate-fade-in">
  <div class="flex items-start justify-between gap-4">
    <div>
      <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200">%s</h1>
      <div class="mt-1 text-sm text-slate-600 dark:text-slate-400">%s</div>
    </div>
    <form action="/qa/anomalies" method="get" class="flex items-center gap-2">
      <select name="season" onchange="this.form.submit()" class="bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-1.5 text-sm font-medium focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40 transition-colors">
        %s
      </select>
    </form>
  </div>

  <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg">
    <h2 class="text-lg font-black text-slate-900 dark:text-slate-200">%s</h2>
    <form action="/qa/anomalies/exclude" method="post" class="mt-4 grid grid-cols-1 md:grid-cols-4 gap-3 items-end">
      <input type="hidden" name="season" value="%s">
      <label class="block">
        <span class="text-[11px] text-slate-500 dark:text-slate-400 font-bold">%s</span>
        <input name="game_id" placeholder="%s" class="mt-1 w-full bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-2 text-sm font-mono focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40">
      </label>
      <label class="block">
        <span class="text-[11px] text-slate-500 dark:text-slate-400 font-bold">%s</span>
        <input name="player_id" placeholder="%s" class="mt-1 w-full bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-2 text-sm font-mono focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40">
      </label>
      <label class="block md:col-span-1">
        <span class="text-[11px] text-slate-500 dark:text-slate-400 font-bold">%s</span>
        <input name="reason" placeholder="%s" class="mt-1 w-full bg-white dark:bg-slate-800 border border-slate-200 dark:border-slate-700 rounded-lg px-3 py-2 text-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-orange-500/40">
      </label>
      <button type="submit" class="h-10 px-4 rounded-lg text-sm font-black bg-rose-600 hover:bg-rose-700 text-white">%s</button>
    </form>
  </div>

  <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg">
    <div class="flex items-center justify-between">
      <h2 class="text-lg font-black text-slate-900 dark:text-slate-200">%s</h2>
      <span class="text-xs font-mono text-slate-400">%d</span>
    </div>
    <div class="mt-4 overflow-x-auto">
      <table class="min-w-[1100px] w-full text-sm table-fixed" aria-label="추천 후보">
        <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]">
          <tr>
            <th scope="col" class="px-3 py-2 text-left">%s</th>
            <th scope="col" class="px-3 py-2 text-left">ID</th>
            <th scope="col" class="px-3 py-2 text-left">팀</th>
            <th scope="col" class="px-3 py-2 text-left">선수</th>
            <th scope="col" class="px-3 py-2 text-right">MIN</th>
            <th scope="col" class="px-3 py-2 text-right">PTS</th>
            <th scope="col" class="px-3 py-2 text-left">주 소속</th>
            <th scope="col" class="px-3 py-2 text-right">%s</th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>
  </div>

  <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-lg">
    <div class="flex items-center justify-between">
      <h2 class="text-lg font-black text-slate-900 dark:text-slate-200">%s</h2>
      <span class="text-xs font-mono text-slate-400">%d</span>
    </div>
    <div class="mt-4 overflow-x-auto">
      <table class="min-w-[1200px] w-full text-sm table-fixed" aria-label="제외된 기록">
        <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 uppercase tracking-wider text-[10px]">
          <tr>
            <th scope="col" class="px-3 py-2 text-left">%s</th>
            <th scope="col" class="px-3 py-2 text-left">ID</th>
            <th scope="col" class="px-3 py-2 text-left">팀</th>
            <th scope="col" class="px-3 py-2 text-left">선수</th>
            <th scope="col" class="px-3 py-2 text-right">MIN</th>
            <th scope="col" class="px-3 py-2 text-right">PTS</th>
            <th scope="col" class="px-3 py-2 text-left">%s</th>
            <th scope="col" class="px-3 py-2 text-left">%s</th>
            <th scope="col" class="px-3 py-2 text-right"> </th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>
  </div>
</div>|html}
      (escape_html heading)
      (escape_html desc)
      season_options
      (escape_html label_manual)
      (escape_html season)
      (escape_html label_game_id)
      (escape_html ph_game_id)
      (escape_html label_player_id)
      (escape_html ph_player_id)
      (escape_html label_reason)
      (escape_html ph_reason)
      (escape_html label_exclude)
      (escape_html label_candidates)
      (List.length candidates)
      (escape_html (tr { ko = "날짜"; en = "Date" }))
      (escape_html (tr { ko = "동작"; en = "Action" }))
      candidates_rows
      (escape_html label_excluded)
      (List.length exclusions)
      (escape_html (tr { ko = "날짜"; en = "Date" }))
      (escape_html label_reason)
      (escape_html label_when)
      exclusions_rows)
    ()
