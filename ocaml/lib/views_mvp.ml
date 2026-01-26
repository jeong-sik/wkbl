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
  Printf.sprintf "Base: %.1f + Win Bonus: %.1f" base_score win_bonus

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

(** MVP candidate row for the table *)
(** Two-line stat cell for MVP table *)
let mvp_cell ?(hide="") ?(color="text-slate-700 dark:text-slate-300") label value =
  Printf.sprintf {html|<td class="px-3 py-2 text-right %s"><div class="flex flex-col items-end leading-tight"><span class="%s font-mono">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">%s</span></div></td>|html}
    hide color value label

let mvp_candidate_row (candidate: mvp_candidate) =
  let player_href = Printf.sprintf "/player/%s" (Uri.pct_encode candidate.mvp_player_id) in
  let team_code = match candidate.mvp_team_code with Some c -> c | None -> "" in
  let team_href = Printf.sprintf "/team/%s" (Uri.pct_encode team_code) in
  let tooltip = score_breakdown_tooltip ~base_score:candidate.mvp_base_score ~win_bonus:candidate.mvp_win_bonus in
  Printf.sprintf
    {html|<tr class="border-b border-slate-200 dark:border-slate-800/60 hover:bg-slate-50 dark:hover:bg-slate-800/30 transition-colors">
      <td class="px-3 py-2 text-center">%s</td>
      <td class="px-3 py-2">
        <div class="flex items-center gap-3">
          %s
          <div>
            <a href="%s" class="font-semibold text-slate-900 dark:text-slate-200 hover:text-orange-600 dark:hover:text-orange-400 transition-colors">%s</a>
            <a href="%s" class="block text-xs text-slate-500 dark:text-slate-400 hover:text-orange-500">%s</a>
          </div>
        </div>
      </td>
      <td class="px-3 py-2 text-center"><div class="flex flex-col items-center leading-tight"><span class="text-slate-600 dark:text-slate-400 font-mono">%d</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">GP</span></div></td>
      %s%s%s%s%s%s
      <td class="px-3 py-2 text-center hidden sm:table-cell"><div class="flex flex-col items-center leading-tight"><span class="text-xs text-slate-500 dark:text-slate-400 font-mono">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">REC</span></div></td>
      <td class="px-3 py-2 text-right" title="%s"><div class="flex flex-col items-end leading-tight"><span class="font-mono font-bold text-sky-600 dark:text-sky-400">%s</span><span class="text-slate-500 dark:text-slate-400 text-[10px] font-mono">SCORE</span></div></td>
    </tr>|html}
    (rank_badge candidate.mvp_rank)
    (player_img_tag ~class_name:"w-10 h-10 rounded-full" candidate.mvp_player_id candidate.mvp_player_name)
    (escape_html player_href)
    (escape_html candidate.mvp_player_name)
    (escape_html team_href)
    (escape_html candidate.mvp_team_name)
    candidate.mvp_games_played
    (mvp_cell "PPG" (format_float candidate.mvp_ppg))
    (mvp_cell ~hide:"hidden md:table-cell" "RPG" (format_float candidate.mvp_rpg))
    (mvp_cell ~hide:"hidden md:table-cell" "APG" (format_float candidate.mvp_apg))
    (mvp_cell ~hide:"hidden lg:table-cell" "SPG" (format_float candidate.mvp_spg))
    (mvp_cell ~hide:"hidden lg:table-cell" "BPG" (format_float candidate.mvp_bpg))
    (mvp_cell ~hide:"hidden sm:table-cell" ~color:"text-orange-600 dark:text-orange-400 font-bold" "EFF" (format_float candidate.mvp_efficiency))
    (format_record candidate.mvp_team_wins candidate.mvp_team_losses)
    (escape_html tooltip)
    (format_float candidate.mvp_final_score)

(** MVP candidates table *)
let mvp_race_table (candidates: mvp_candidate list) =
  let rows = candidates |> List.map mvp_candidate_row |> String.concat "\n" in
  Printf.sprintf
    {html|<div class="bg-white dark:bg-slate-900 rounded-lg border border-slate-200 dark:border-slate-800 overflow-x-auto overflow-y-hidden">
      <table class="w-full min-w-[720px] text-sm table-fixed" aria-label="MVP 레이스 순위">
        <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 text-xs uppercase tracking-wider">
          <tr>
            <th scope="col" class="px-3 py-2 text-center w-[6%%]">Rank</th>
            <th scope="col" class="px-3 py-2 text-left w-[22%%]">Player</th>
            <th scope="col" class="px-3 py-2 text-center w-[6%%]" title="Games Played">GP</th>
            <th scope="col" class="px-3 py-2 text-right w-[7%%]" title="Points Per Game">PPG</th>
            <th scope="col" class="px-3 py-2 text-right hidden md:table-cell w-[7%%]" title="Rebounds Per Game">RPG</th>
            <th scope="col" class="px-3 py-2 text-right hidden md:table-cell w-[7%%]" title="Assists Per Game">APG</th>
            <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell w-[7%%]" title="Steals Per Game">SPG</th>
            <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell w-[7%%]" title="Blocks Per Game">BPG</th>
            <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell text-orange-600 dark:text-orange-400 w-[8%%]" title="Efficiency">EFF</th>
            <th scope="col" class="px-3 py-2 text-center hidden sm:table-cell w-[12%%]" title="Team Win-Loss Record">Team W-L</th>
            <th scope="col" class="px-3 py-2 text-right text-sky-600 dark:text-sky-400 w-[8%%]" title="MVP Score = Base + Win Bonus">Score</th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>|html}
    rows

(** MVP Race page *)
let mvp_race_page ~season ~seasons (candidates: mvp_candidate list) =
  let season_options =
    let base =
      seasons
      |> List.map (fun (s: season_info) ->
          let selected = if s.code = season then "selected" else "" in
          Printf.sprintf {html|<option value="%s" %s>%s</option>|html} s.code selected (escape_html s.name))
      |> String.concat "\n"
    in
    Printf.sprintf {html|<option value="ALL" %s>All Seasons</option>%s|html} (if season = "ALL" then "selected" else "") base
  in
  let table = mvp_race_table candidates in
  let formula_section =
    {html|<details class="bg-white dark:bg-slate-900/50 rounded-lg border border-slate-200 dark:border-slate-800/50 p-4 text-sm text-slate-600 dark:text-slate-400">
      <summary class="cursor-pointer font-bold text-slate-700 dark:text-slate-300 select-none">MVP Score Formula</summary>
      <div class="mt-3 space-y-2 leading-relaxed font-mono text-xs">
        <div class="bg-slate-50 dark:bg-slate-800/50 p-3 rounded">
          <div class="text-orange-600 dark:text-orange-400 font-bold mb-2">Base Score</div>
          <code>= (PPG x 2.0) + (RPG x 1.2) + (APG x 1.5) + (SPG x 2.0) + (BPG x 2.0) + (EFF x 0.5)</code>
        </div>
        <div class="bg-slate-50 dark:bg-slate-800/50 p-3 rounded">
          <div class="text-sky-600 dark:text-sky-400 font-bold mb-2">Win Bonus</div>
          <code>= Team Win% x 20</code>
        </div>
        <div class="bg-sky-50 dark:bg-sky-900/30 p-3 rounded border border-sky-200 dark:border-sky-800">
          <div class="text-sky-700 dark:text-sky-300 font-bold mb-2">Final MVP Score</div>
          <code>= Base Score + Win Bonus</code>
        </div>
      </div>
    </details>|html}
  in
  layout ~title:"MVP Race - WKBL" ~canonical_path:"/mvp-race"
    ~description:"WKBL 여자농구 MVP 레이스 - 시즌 MVP 후보 순위와 점수를 확인하세요."
    ~content:(Printf.sprintf
      {html|<div class="space-y-6">
        <div class="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
          <div>
            <h1 class="text-2xl font-bold text-slate-900 dark:text-slate-200">MVP Race</h1>
            <p class="text-slate-500 dark:text-slate-400 text-sm mt-1">Season MVP candidates ranked by composite score</p>
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
      season_options
      formula_section
      table) ()
