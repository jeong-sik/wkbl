open Domain
open Views_common

let prediction_result_card ~(home: string) ~(away: string) (output: prediction_output) =
 let pct value = value *. 100.0 in
 let result = output.result in
 let breakdown = output.breakdown in
 let home_pct = pct result.prob_a in
 let away_pct = pct result.prob_b in
 let elo_home_pct = pct breakdown.pb_elo_prob in
 let elo_away_pct = pct (1.0 -. breakdown.pb_elo_prob) in
 let pyth_home_pct = pct breakdown.pb_pyth_prob in
 let pyth_away_pct = pct (1.0 -. breakdown.pb_pyth_prob) in
 let stats_home_pct = pct breakdown.pb_stats_prob in
 let stats_away_pct = pct (1.0 -. breakdown.pb_stats_prob) in
 
 (* Margin Logic *)
 let margin = abs_float result.predicted_margin in
 let spread_str = Printf.sprintf "%.1f" margin in
 let spread_badge =
   if margin < 1.5 then "초접전"
   else if margin < 5.5 then "접전"
   else if margin < 10.5 then "우세"
   else "압승"
 in
 let spread_class = 
   if margin < 1.5 then "bg-rose-100 text-rose-700 dark:bg-rose-900/30 dark:text-rose-300"
   else if margin < 5.5 then "bg-amber-100 text-amber-700 dark:bg-amber-900/30 dark:text-amber-300"
   else "bg-emerald-100 text-emerald-700 dark:bg-emerald-900/30 dark:text-emerald-300"
 in

 (* Generate AI explanation *)
 let ai_explanation = Ai.get_explanation ~home ~away output in
 let context_card_html, context_note_html =
  match breakdown.pb_context with
	  | None ->
	    ("",
	     {html|<div>기본 모델(전력/득실 기대/기록)만 사용합니다.</div>
		     <div class="text-slate-600 dark:text-slate-400">옵션을 켜면 최근 5경기 흐름/주요 선수 출전/휴식일을 조금 반영합니다. (부상/전술/실시간 상황은 반영하지 못합니다.)</div>|html})
  | Some ctx ->
    let delta_pp = ctx.pcb_delta *. 100.0 in
    let delta_cls =
     if delta_pp > 0.0 then "text-sky-600 dark:text-sky-400"
     else if delta_pp < 0.0 then "text-rose-600 dark:text-rose-400"
     else "text-slate-700 dark:text-slate-300"
    in
    let delta_str =
     if delta_pp > 0.0 then Printf.sprintf "+%.1f%%" delta_pp else Printf.sprintf "%.1f%%" delta_pp
    in
    let form_home_pct = pct ctx.pcb_form_home in
    let form_away_pct = pct ctx.pcb_form_away in
    let roster_text =
     match ctx.pcb_roster_home, ctx.pcb_roster_away with
     | Some h, Some a ->
       Printf.sprintf "%d/%d vs %d/%d" h.rcs_present h.rcs_total a.rcs_present a.rcs_total
     | _ -> "-"
    in
    let rest_text =
     match ctx.pcb_rest_home_days, ctx.pcb_rest_away_days with
     | Some h, Some a -> Printf.sprintf "%d일 vs %d일" h a
     | _ -> "-"
    in
    ( Printf.sprintf
      {html|<div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3">
	       <div class="flex items-center justify-between">
	        <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest">컨텍스트</div>
	        <div class="text-[10px] font-mono %s">%s</div>
	       </div>
	       <div class="mt-2 space-y-1 text-[10px] text-slate-600 dark:text-slate-400 font-mono">
	        <div class="flex justify-between"><span>최근 5경기 승률</span><span class="text-slate-700 dark:text-slate-300">%.0f%% vs %.0f%%</span></div>
	        <div class="flex justify-between"><span>코어 로스터</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
	        <div class="flex justify-between"><span>휴식</span><span class="text-slate-700 dark:text-slate-300">%s</span></div>
	       </div>
	      </div>|html}
      delta_cls
      (escape_html delta_str)
      form_home_pct
      form_away_pct
      (escape_html roster_text)
      (escape_html rest_text),
	     {html|<div>최근 5경기 흐름/주요 선수 출전/휴식일을 조금 반영합니다. (반영 폭은 작게 제한합니다.)</div><div class="text-slate-600 dark:text-slate-400">주요 선수는 “최근 경기 출전” 기준으로 추정합니다. 부상/전술/실시간 상황은 반영하지 못합니다.</div>|html}
	    )
 in
 let winner_class =
  if normalize_label result.winner = normalize_label home then "text-orange-700 dark:text-orange-400"
  else "text-sky-600 dark:text-sky-400"
 in
 Printf.sprintf
  {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl space-y-4">
   <div class="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-4">
    <div class="min-w-0">
     <div class="text-sm text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest mb-1">예측</div>
     <div class="text-3xl font-black text-slate-900 dark:text-slate-200 tracking-tight">%s</div>
     <div class="mt-1 text-xs text-slate-500 dark:text-slate-400 font-medium truncate flex items-center gap-2">
	       <span>%s 대 %s</span>
	       <span class="px-1.5 py-0.5 rounded text-[10px] font-bold %s">%s</span>
	     </div>
	    </div>
	    <div class="flex gap-4 text-right shrink-0">
	     <div>
	       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase font-bold">총점</div>
	       <div class="text-xl font-black font-mono text-slate-800 dark:text-slate-300">%.1f</div>
	     </div>
	     <div>
	       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase font-bold">점수차</div>
	       <div class="text-xl font-black font-mono text-slate-800 dark:text-slate-300">+%s</div>
	     </div>
	     <div>
	       <div class="text-[10px] text-slate-500 dark:text-slate-400 uppercase font-bold">승리 팀</div>
	       <div class="text-xl font-black %s">%s</div>
	     </div>
	    </div>
	   </div>
   <div class="space-y-3 pt-2">
    <div class="flex justify-between text-[10px] font-black uppercase tracking-widest text-slate-500">
     <span class="flex items-center gap-1.5"><span class="w-2 h-2 rounded-full bg-orange-500"></span>%s</span>
     <span class="flex items-center gap-1.5">%s<span class="w-2 h-2 rounded-full bg-sky-500"></span></span>
    </div>
    %s
   </div>
   <!-- AI Analysis -->
   <div class="bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 border border-indigo-200 dark:border-indigo-800/50 rounded-lg p-4">
    <div class="flex items-center gap-2 mb-2">
     <span class="text-lg">🤖</span>
     <span class="text-xs font-bold text-indigo-700 dark:text-indigo-300 uppercase tracking-wider">AI 분석</span>
    </div>
    <p class="text-sm text-slate-700 dark:text-slate-300 leading-relaxed">%s</p>
   </div>
   <div class="grid grid-cols-1 lg:grid-cols-4 gap-3 text-xs">
    <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 border-t-2 border-t-violet-500">
	     <div class="flex items-center justify-between">
	      <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest flex items-center gap-1.5"><span class="w-1.5 h-1.5 rounded-full bg-violet-500"></span>전력</div>
	      <div class="text-[10px] text-slate-600 dark:text-slate-400 font-mono">%d경기</div>
	     </div>
     <div class="mt-2 flex items-center justify-between font-mono">
      <div class="text-orange-700 dark:text-orange-400">%.0f</div>
      <div class="text-sky-700 dark:text-sky-400">%.0f</div>
     </div>
     <div class="mt-1 flex items-center justify-between font-mono font-bold">
      <div class="text-orange-700 dark:text-orange-400">%.1f%%</div>
      <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
     </div>
    </div>
	    <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 border-t-2 border-t-emerald-500">
	     <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest flex items-center gap-1.5"><span class="w-1.5 h-1.5 rounded-full bg-emerald-500"></span>득실 기대</div>
     <div class="mt-2 flex items-center justify-between font-mono">
      <div class="text-orange-700 dark:text-orange-400">%.3f</div>
      <div class="text-sky-700 dark:text-sky-400">%.3f</div>
     </div>
     <div class="mt-1 flex items-center justify-between font-mono font-bold">
      <div class="text-orange-700 dark:text-orange-400">%.1f%%</div>
      <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
     </div>
    </div>
	    <div class="bg-slate-100 dark:bg-slate-800/40 border border-slate-300 dark:border-slate-700/50 rounded-lg p-3 border-t-2 border-t-amber-500">
	     <div class="text-slate-600 dark:text-slate-400 font-mono uppercase tracking-widest flex items-center gap-1.5"><span class="w-1.5 h-1.5 rounded-full bg-amber-500"></span>기록</div>
     <div class="mt-2 flex items-center justify-between font-mono font-bold">
      <div class="text-orange-700 dark:text-orange-400">%.1f%%</div>
      <div class="text-sky-600 dark:text-sky-400">%.1f%%</div>
     </div>
	     <div class="mt-1 text-[10px] text-slate-600 dark:text-slate-400 font-mono">승률 + 효율 결합</div>
	    </div>
    %s
   </div>
   <details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-xs text-slate-600 dark:text-slate-400">
	    <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">예측 안내</summary>
	    <div class="mt-2 space-y-1 leading-relaxed">
	     <div>종합 확률은 <span class="font-mono text-slate-900 dark:text-slate-200">전력(60%%)</span> + <span class="font-mono text-slate-900 dark:text-slate-200">득실 기대(25%%)</span> + <span class="font-mono text-slate-900 dark:text-slate-200">기록(15%%)</span>을 섞어 계산합니다.</div>
	     <div><span class="font-mono text-slate-900 dark:text-slate-200">전력</span>은 최근 경기 결과(점수차 반영)를 바탕으로 업데이트합니다.</div>
	     <div><span class="font-mono text-slate-900 dark:text-slate-200">득실 기대</span>는 득점/실점으로 기대 승률을 계산해 매치업 확률로 바꿉니다.</div>
	     <div><span class="font-mono text-slate-900 dark:text-slate-200">기록</span>은 승률과 효율을 단순 결합합니다.</div>
	     <div>중립 경기면 홈 이점을 예측에 반영하지 않습니다.</div>
	     %s
	    </div>
	   </details>
  </div>|html}
  (escape_html breakdown.pb_season)
  (escape_html home)
  (escape_html away)
  spread_class
  spread_badge
  result.predicted_total_score
  spread_str
  winner_class
  (escape_html result.winner)
  (escape_html home)
  (escape_html away)
  (Prediction_gauge.gauge_chart ~home_pct ~away_pct)
  (escape_html ai_explanation)
  breakdown.pb_games_used
  breakdown.pb_elo_home
  breakdown.pb_elo_away
  elo_home_pct
  elo_away_pct
  breakdown.pb_pyth_home
  breakdown.pb_pyth_away
  pyth_home_pct
  pyth_away_pct
  stats_home_pct
  stats_away_pct
  context_card_html
  context_note_html

let upcoming_games_section (upcoming: Domain.schedule_entry list) =
 if upcoming = [] then "" 
 else
  let game_card (g: Domain.schedule_entry) =
   let home_name = Option.value g.sch_home_team_name ~default:g.sch_home_team_code in
   let away_name = Option.value g.sch_away_team_name ~default:g.sch_away_team_code in
   let home_color = Domain.team_code_of_string home_name |> Option.map Domain.team_code_to_color |> Option.value ~default:"#666" in
   let away_color = Domain.team_code_of_string away_name |> Option.map Domain.team_code_to_color |> Option.value ~default:"#666" in
   let time_str = match g.sch_game_time with
    | Some t -> Printf.sprintf " %s" t
    | None -> ""
   in
   Printf.sprintf
    {html|<a href="/predict?home=%s&away=%s" class="block bg-white dark:bg-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-700/60 border border-slate-200 dark:border-slate-700 hover:border-orange-500/50 rounded-lg p-3 transition-all group overflow-hidden relative">
     <div class="absolute inset-y-0 left-0 w-1 rounded-l-lg" style="background: linear-gradient(%s, %s)"></div>
     <div class="text-[10px] text-slate-500 dark:text-slate-400 font-mono mb-2 pl-2">%s%s</div>
     <div class="flex items-center gap-2 pl-2">
      <div class="flex items-center gap-1.5 flex-1 min-w-0">
       %s
       <span class="text-sm font-semibold text-slate-800 dark:text-slate-200 group-hover:text-orange-500 truncate">%s</span>
      </div>
      <span class="text-[10px] text-slate-400 font-bold uppercase tracking-wider shrink-0 px-1">vs</span>
      <div class="flex items-center gap-1.5 flex-1 min-w-0 justify-end">
       <span class="text-sm font-semibold text-slate-800 dark:text-slate-200 group-hover:text-orange-500 truncate">%s</span>
       %s
      </div>
     </div>
    </a>|html}
    (escape_html home_name) (escape_html away_name)
    home_color away_color
    (escape_html g.sch_game_date) time_str
    (team_logo_tag ~class_name:"w-6 h-6 shrink-0" home_name)
    (escape_html home_name)
    (escape_html away_name)
    (team_logo_tag ~class_name:"w-6 h-6 shrink-0" away_name)
  in
  let cards = upcoming |> List.map game_card |> String.concat "\n" in
	  Printf.sprintf
	   {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4 mb-6">
	    <h3 class="text-sm font-bold text-slate-600 dark:text-slate-400 uppercase tracking-wider mb-3">📅 예정 경기</h3>
	    <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-2">%s</div>
	   </div>|html}
	   cards

let predict_page ?(lang=I18n.Ko) ~season ~seasons ~teams ~home ~away ~what_if ~is_neutral ~context_enabled ~include_mismatch ~upcoming ?(games=[]) (result: prediction_output option) (error: string option) =
 let season_options =
  let base =
   seasons
   |> List.map (fun (s : season_info) ->
     let selected = if s.code = season then "selected" else "" in
     Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
   |> String.concat "\n"
  in
  Printf.sprintf {html|<option value="ALL" %s>전체 시즌</option>%s|html} (if season = "ALL" then "selected" else "") base
 in

 let result_html =
  match result, error with
  | _, Some e -> Views_common.error_with_retry ~message:e ()
	  | None, None ->
	    Printf.sprintf
	    {html|<div class="text-slate-600 dark:text-slate-400 text-sm">팀을 선택해 예측을 확인하세요.</div>|html}
	  | Some r, _ ->
      let insight_html =
        if games <> [] then
          Printf.sprintf
            {html|
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mt-6">
                <!-- Home Recent Form -->
                <div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
	                  <h3 class="text-xs font-bold text-slate-500 dark:text-slate-400 uppercase tracking-wider mb-3 flex items-center gap-2">
	                    %s 최근 흐름
	                  </h3>
                  %s
                </div>
                <!-- Away Recent Form -->
                <div class="bg-white dark:bg-slate-900/50 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
	                  <h3 class="text-xs font-bold text-slate-500 dark:text-slate-400 uppercase tracking-wider mb-3 flex items-center gap-2">
	                    %s 최근 흐름
	                  </h3>
                  %s
                </div>
              </div>
              %s
            |html}
            (Views_common.team_badge home)
            (Views_common.render_recent_games_for_predict home games 5)
            (Views_common.team_badge away)
            (Views_common.render_recent_games_for_predict away games 5)
            (Views_common.render_h2h_summary home away games season)
        else ""
      in
      let what_if_html =
        let is_home_ace_missing = match what_if with Some "home" -> true | _ -> false in
        let is_away_ace_missing = match what_if with Some "away" -> true | _ -> false in
        let base_url = Printf.sprintf "/predict?season=%s&home=%s&away=%s%s%s"
          (Uri.pct_encode season) (Uri.pct_encode home) (Uri.pct_encode away)
          (if is_neutral then "&neutral=1" else "")
          (if context_enabled then "&context=1" else "")
        in
        Printf.sprintf
          {html|
            <div class="mt-8 bg-slate-900 rounded-xl border border-slate-700/80 p-5 shadow-2xl relative overflow-hidden group">
              <div class="absolute inset-0 bg-gradient-to-br from-indigo-500/10 to-purple-600/10 pointer-events-none"></div>
              <div class="relative z-10 flex flex-col md:flex-row items-center justify-between gap-4">
                <div class="flex items-center gap-3">
                  <div class="w-10 h-10 rounded-full bg-slate-800 flex items-center justify-center border border-slate-700 shadow-inner">
                    <span class="text-xl">🚑</span>
                  </div>
                  <div>
                    <h3 class="text-sm font-black text-white tracking-wider flex items-center gap-2">
                      WHAT-IF 시뮬레이터 <span class="px-1.5 py-0.5 rounded bg-indigo-500/20 text-indigo-300 text-[9px] uppercase">Beta</span>
                    </h3>
                    <p class="text-xs text-slate-400 mt-0.5">만약 각 팀의 에이스(최고 효율 선수)가 갑자기 결장한다면?</p>
                  </div>
                </div>
                <div class="flex flex-wrap gap-2 w-full md:w-auto">
                  <a href="%s" class="flex-1 md:flex-none text-center px-4 py-2 rounded-lg text-xs font-bold transition-all duration-300 %s">
                    원래대로
                  </a>
                  <a href="%s&what_if=home" class="flex-1 md:flex-none text-center px-4 py-2 rounded-lg text-xs font-bold transition-all duration-300 %s">
                    홈 에이스 결장
                  </a>
                  <a href="%s&what_if=away" class="flex-1 md:flex-none text-center px-4 py-2 rounded-lg text-xs font-bold transition-all duration-300 %s">
                    원정 에이스 결장
                  </a>
                </div>
              </div>
            </div>
          |html}
          base_url
          (if not is_home_ace_missing && not is_away_ace_missing then "bg-slate-700 text-white shadow-md border border-slate-600" else "bg-slate-800 text-slate-400 hover:bg-slate-700 hover:text-white border border-transparent")
          base_url
          (if is_home_ace_missing then "bg-orange-600 text-white shadow-md border border-orange-500 shadow-orange-500/20" else "bg-slate-800 text-slate-400 hover:bg-orange-600/80 hover:text-white border border-transparent")
          base_url
          (if is_away_ace_missing then "bg-sky-600 text-white shadow-md border border-sky-500 shadow-sky-500/20" else "bg-slate-800 text-slate-400 hover:bg-sky-600/80 hover:text-white border border-transparent")
      in
      (prediction_result_card ~home ~away r) ^ what_if_html ^ insight_html
 in

 let team_option current name =
  let selected = if normalize_label current = normalize_label name then "selected" else "" in
  Printf.sprintf {html|<option value="%s" %s>%s</option>|html} (escape_html name) selected (escape_html name)
 in
	 let team_options current =
	  let base = teams |> List.map (team_option current) |> String.concat "\n" in
	  Printf.sprintf {html|<option value="" %s>팀 선택…</option>%s|html} (if String.trim current = "" then "selected" else "") base
	 in

 let upcoming_html = upcoming_games_section upcoming in
 let og_image =
   if home <> "" && away <> "" then
     Some (Printf.sprintf "https://wkbl.win/api/og/predict?home=%s&away=%s&season=%s" (Uri.pct_encode home) (Uri.pct_encode away) (Uri.pct_encode season))
   else None
 in
 layout ~lang ~title:"WKBL 예측" ~canonical_path:"/predict"
  ~description:"WKBL 여자농구 경기 예측 - AI 기반 승률 예측과 분석을 확인하세요."
  ?og_image
	  ~content:(Printf.sprintf
	   {html|<div class="space-y-6 animate-fade-in">%s
	    <div class="flex flex-col md:flex-row md:items-end md:justify-between gap-3">
	     <div>
	      <h2 class="text-3xl font-black text-slate-900 dark:text-slate-200">경기 예측</h2>
	      <p class="text-slate-600 dark:text-slate-400 text-sm">예측 결과와 함께 근거 요약을 보여줍니다.</p>
	     </div>
	    </div>
    %s
    <form action="/predict" method="get" class="grid grid-cols-1 md:grid-cols-3 gap-3 bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-4">
     <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
     <select name="home" aria-label="홈팀 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
     <select name="away" aria-label="원정팀 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">%s</select>
     <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
      <input type="checkbox" name="context" value="1" class="accent-orange-500" %s>
      컨텍스트 반영 (폼/로스터/휴식)
     </label>
	     <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
	      <input type="checkbox" name="neutral" value="1" class="accent-orange-500" %s>
	      중립 경기 (홈 이점 없음)
	     </label>
	     <label class="md:col-span-3 flex items-center gap-2 text-xs text-slate-600 dark:text-slate-400">
	      <input type="checkbox" name="include_mismatch" value="1" class="accent-orange-500" %s>
	      불일치 포함 (최종 스코어와 득점 합계가 다른 경기 포함)
	     </label>
	     <div class="md:col-span-3 flex justify-end">
	      <button type="submit" class="bg-orange-500 hover:bg-orange-400 text-black font-bold px-4 py-2 rounded text-sm transition">예측하기</button>
	     </div>
	    </form>
    %s
   </div>|html}
   (breadcrumb [("홈", "/"); ("경기 예측", "")])
   upcoming_html
   season_options
   (team_options home)
   (team_options away)
   (if context_enabled then "checked" else "")
   (if is_neutral then "checked" else "")
   (if include_mismatch then "checked" else "")
   result_html) ()

(** Podium-style leader card (Gemini UX feedback: 1-2-3위 시각적 강조) *)
