(** MVP Race Views - MVP Race feature page rendering *)

open Views_common
open Domain

(** Format float with specified digits *)
let format_float ?(digits=1) value = Printf.sprintf "%.*f" digits value

(** Format win-loss record *)
let format_record wins losses =
  Printf.sprintf "%d-%d" wins losses

(** MVP score breakdown tooltip *)
let score_breakdown_tooltip ~base_score ~win_bonus =
  Printf.sprintf "기본: %.1f + 승리 보너스: %.1f" base_score win_bonus

(** Rank badge based on position *)
let rank_badge rank =
  let (bg_class, text_class) = match rank with
    | 1 -> ("bg-yellow-400", "text-yellow-900")
    | 2 -> ("bg-slate-300", "text-slate-800")
    | 3 -> ("bg-amber-600", "text-amber-50")
    | _ -> ("bg-slate-200 dark:bg-slate-700", "text-slate-700 dark:text-slate-300")
  in
  Printf.sprintf
    {html|<span class="flex items-center justify-center w-8 h-8 rounded-full %s %s font-bold text-sm">%d</span>|html}
    bg_class text_class rank

(** Two-line stat cell inner HTML for MVP table (returns content, not <td>) *)
let mvp_stat ?(color="") label value =
  Printf.sprintf {html|<div class="flex flex-col items-end leading-tight"><span class="%s">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px]">%s</span></div>|html}
    color value label

(** Convert MVP candidate to row data for render_fixed_table *)
let mvp_candidate_data (candidate: mvp_candidate) =
  let phref = player_href candidate.mvp_player_id in
  let team_code = match candidate.mvp_team_code with Some c -> c | None -> "" in
  let thref = team_href team_code in
  let tooltip = score_breakdown_tooltip ~base_score:candidate.mvp_base_score ~win_bonus:candidate.mvp_win_bonus in
  let player_cell = Printf.sprintf
    {html|<div class="flex items-center gap-3 font-sans">%s<div><a href="%s" class="font-semibold text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:hover:text-orange-400 transition-colors block truncate max-w-[140px]">%s</a><a href="%s" class="block text-xs text-slate-500 dark:text-slate-400 hover:text-orange-500 truncate">%s</a></div></div>|html}
    (player_img_tag ~class_name:"w-10 h-10 rounded-full" candidate.mvp_player_id candidate.mvp_player_name)
    (escape_html phref)
    (escape_html candidate.mvp_player_name)
    (escape_html thref)
    (escape_html candidate.mvp_team_name)
  in
  let gp_cell = Printf.sprintf
    {html|<div class="flex flex-col items-center leading-tight"><span>%d</span><span class="text-slate-500 dark:text-slate-400 text-[10px]">GP</span></div>|html}
    candidate.mvp_games_played
  in
  let record_cell = Printf.sprintf
    {html|<div class="flex flex-col items-center leading-tight"><span class="text-xs text-slate-500 dark:text-slate-400">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px]">전적</span></div>|html}
    (format_record candidate.mvp_team_wins candidate.mvp_team_losses)
  in
  let score_cell = Printf.sprintf
    {html|<div class="flex flex-col items-end leading-tight" title="%s"><span class="font-bold text-sky-600 dark:text-sky-400">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px]">점수</span></div>|html}
    (escape_html tooltip) (format_float candidate.mvp_final_score)
  in
  [ player_cell; gp_cell;
    mvp_stat "PPG" (format_float candidate.mvp_ppg);
    mvp_stat "RPG" (format_float candidate.mvp_rpg);
    mvp_stat "APG" (format_float candidate.mvp_apg);
    mvp_stat "SPG" (format_float candidate.mvp_spg);
    mvp_stat "BPG" (format_float candidate.mvp_bpg);
    mvp_stat ~color:"text-orange-600 dark:text-orange-400 font-bold" "EFF" (format_float candidate.mvp_efficiency);
    record_cell; score_cell ]

(** MVP candidates table *)
let mvp_race_table (candidates: mvp_candidate list) =
  if candidates = [] then
    empty_state ~icon:BasketballIcon
      "MVP 데이터가 없습니다"
      "아직 시즌 데이터가 충분하지 않거나, 선택한 시즌에 경기가 없습니다. 다른 시즌을 선택해 보세요."
  else
    let mvp_cols = [
      col ~sticky:true ~w:(px 200) "선수";
      col ~align:`Center ~w:(px 60) ~title:"출전 경기" "GP";
      col ~align:`Right ~w:(px 60) ~title:"경기당 득점" "PPG";
      col ~align:`Right ~resp:`Hidden_md ~w:(px 60) ~title:"경기당 리바운드" "RPG";
      col ~align:`Right ~resp:`Hidden_md ~w:(px 60) ~title:"경기당 어시스트" "APG";
      col ~align:`Right ~resp:`Hidden_lg ~w:(px 60) ~title:"경기당 스틸" "SPG";
      col ~align:`Right ~resp:`Hidden_lg ~w:(px 60) ~title:"경기당 블록" "BPG";
      col ~align:`Right ~resp:`Hidden_sm ~highlight:true ~w:(px 80) ~title:"효율" "EFF";
      col ~align:`Center ~resp:`Hidden_sm ~w:(px 80) ~title:"팀 전적" "팀 전적";
      col ~align:`Right ~w:(px 80) ~title:"MVP 점수 = 기본 + 승리 보너스" "점수";
    ] in
    let mvp_data = candidates |> List.map mvp_candidate_data in
    render_fixed_table ~striped:true ~aria_label:"MVP 레이스 순위"
      ~id:"mvp-race" ~min_width:"min-w-[820px]" ~cols:mvp_cols mvp_data

(** MVP Race page *)
let mvp_race_page ?(lang=I18n.Ko) ~season ~seasons (candidates: mvp_candidate list) =
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
  let table = mvp_race_table candidates in
  let formula_section =
    {html|<details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-sm text-slate-600 dark:text-slate-400">
      <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">MVP 점수 계산</summary>
      <div class="mt-3 space-y-2 leading-relaxed font-mono text-xs">
        <div class="bg-slate-50 dark:bg-slate-800/50 p-3 rounded">
          <div class="text-orange-600 dark:text-orange-400 font-bold mb-2">기본 점수</div>
          <code>= (PPG x 2.0) + (RPG x 1.2) + (APG x 1.5) + (SPG x 2.0) + (BPG x 2.0) + (EFF x 0.5)</code>
        </div>
        <div class="bg-slate-50 dark:bg-slate-800/50 p-3 rounded">
          <div class="text-sky-600 dark:text-sky-400 font-bold mb-2">승리 보너스</div>
          <code>= 팀 승률 x 20</code>
        </div>
        <div class="bg-sky-50 dark:bg-sky-900/30 p-3 rounded border border-sky-200 dark:border-sky-800">
          <div class="text-sky-700 dark:text-sky-300 font-bold mb-2">최종 MVP 점수</div>
          <code>= 기본 점수 + 승리 보너스</code>
        </div>
      </div>
    </details>|html}
  in
  layout ~lang ~title:"MVP 레이스 | WKBL" ~canonical_path:"/mvp-race"
    ~description:"WKBL 여자농구 MVP 레이스 - 시즌 MVP 후보 순위와 점수를 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6">%s
        <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
          <div>
            <h1 class="text-2xl font-bold text-slate-900 dark:text-slate-200">MVP 레이스</h1>
            <p class="text-slate-500 dark:text-slate-400 text-sm mt-1">시즌 MVP 후보를 종합 점수로 정렬했습니다.</p>
          </div>
          <form class="flex gap-3" hx-get="/mvp-race/table" hx-target="#mvp-table" hx-trigger="change">
            <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm focus:border-orange-500 focus:outline-none">
              %s
            </select>
          </form>
        </div>
        %s
        <div id="mvp-table" data-skeleton="table" data-skeleton-count="10" data-skeleton-cols="6">%s</div>
      </div>|html}
      (breadcrumb [("홈", "/"); ("MVP 레이스", "")])
      season_options
      formula_section
      table) ()
