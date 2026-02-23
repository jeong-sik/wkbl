(** Live scores and position leaders view functions *)

open Domain
open Views_common

(** Live scores widget for homepage *)
let live_scores_widget ?(lang=I18n.Ko) (games: Domain.live_game list) =
  let tr = I18n.t lang in
  let label_no_games_title = tr { ko = "오늘 경기가 없습니다"; en = "No games today" } in
  let label_no_games_sub = tr { ko = "경기 일정이 있는 날 다시 확인해주세요."; en = "Check back on game days." } in
  let label_live = tr { ko = "진행중"; en = "LIVE" } in
  let label_scheduled = tr { ko = "예정"; en = "Scheduled" } in
  let label_final = tr { ko = "경기종료"; en = "Final" } in
  if List.length games = 0 then
    empty_state ~icon:BasketballIcon label_no_games_title label_no_games_sub
  else
    let game_cards =
      games
      |> List.map (fun (g : Domain.live_game) ->
          let q = String.trim g.lg_quarter in
          let is_pre_game =
            (not g.lg_is_live)
            && (q = "경기전" || q = "경기 전" || q = "예정"
                || (q = "" && g.lg_home_score = 0 && g.lg_away_score = 0))
          in
          let show_badge_text =
            if g.lg_is_live then
              label_live
            else if is_pre_game then
              label_scheduled
            else if q = "" then
              label_scheduled
            else
              match lang with
              | I18n.En -> (
                  match q with
                  | "경기전" | "경기 전" | "예정" -> label_scheduled
                  | "경기종료" -> label_final
                  | _ -> q)
              | _ -> q
          in
          let time_html =
            let t = String.trim g.lg_time_remaining in
            if t = "" then ""
            else
              Printf.sprintf
                {html|<span class="text-xs font-mono text-slate-500">%s</span>|html}
                (escape_html t)
          in
          let status_badge =
            if g.lg_is_live then
              Printf.sprintf
                {html|<span class="live-badge"><span class="live-dot"></span>%s</span>|html}
                (escape_html label_live)
            else
              Printf.sprintf
                {html|<span class="px-2 py-0.5 rounded-full bg-slate-200 dark:bg-slate-700 text-slate-600 dark:text-slate-300 text-[10px]">%s</span>|html}
                (escape_html show_badge_text)
          in
          let score_center =
            if is_pre_game then
              {html|<span class="text-xl font-black text-slate-500 dark:text-slate-400">VS</span>|html}
            else
              Printf.sprintf
                {html|<span class="text-xl font-black font-mono text-slate-900 dark:text-white">%d : %d</span>|html}
                g.lg_home_score
                g.lg_away_score
          in
          Printf.sprintf
            {html|
     <div class="bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 rounded-lg p-4 shadow-sm">
      <div class="flex items-center justify-between mb-2">
       %s
       %s
      </div>
      <div class="flex items-center justify-between">
       <div class="flex items-center gap-2">
        %s
        <span class="font-bold text-slate-900 dark:text-white truncate min-w-0">%s</span>
       </div>
       %s
       <div class="flex items-center gap-2">
        <span class="font-bold text-slate-900 dark:text-white text-right truncate min-w-0">%s</span>
        %s
       </div>
      </div>
     </div>
    |html}
            status_badge
            time_html
            (team_logo_tag ~class_name:"w-6 h-6" g.lg_home_team)
            (escape_html g.lg_home_team)
            score_center
            (escape_html g.lg_away_team)
            (team_logo_tag ~class_name:"w-6 h-6" g.lg_away_team))
      |> String.concat "\n"
    in
    Printf.sprintf
      {html|<div class="grid grid-cols-1 md:grid-cols-2 gap-4">%s</div>|html}
      game_cards

(** HTMX endpoint for live scores widget *)
let live_scores_htmx () =
 let games = Live.get_current_games () in
 live_scores_widget games

let live_page ?(lang=I18n.Ko) () =
 let tr = I18n.t lang in
 let title = tr { ko = "실시간 점수 | WKBL"; en = "Live Scores | WKBL" } in
 let h1 = tr { ko = "실시간 점수"; en = "Live Scores" } in
 let desc = tr { ko = "실시간 경기 점수"; en = "Live game scores" } in
 let connecting = tr { ko = "업데이트 중..."; en = "Updating..." } in
 let connected = tr { ko = "업데이트 됨"; en = "Updated" } in
 let disconnected = tr { ko = "업데이트 지연"; en = "Update delayed" } in
 let status_scheduled = tr { ko = "예정"; en = "Scheduled" } in
 let status_live = tr { ko = "진행중"; en = "Live" } in
 let status_final = tr { ko = "종료"; en = "Final" } in
 let empty_today = tr { ko = "오늘 예정된 경기가 없습니다."; en = "No games scheduled today." } in
 let view_all_games = tr { ko = "전체 경기 보기"; en = "View all games" } in
 let auto_updates = tr { ko = "자동으로 업데이트됩니다."; en = "Updates automatically." } in
 let page_html =
  Printf.sprintf
   {html|
	  <div class="space-y-6">%s
	   <div class="flex flex-col sm:flex-row sm:items-center justify-between gap-4">
	    <div>
	     <h1 class="text-2xl font-bold text-slate-900 dark:text-slate-200">%s</h1>
   <p class="text-slate-600 dark:text-slate-400">%s</p>
    </div>
    <div id="connection-status" class="flex items-center gap-2">
     <span class="w-2 h-2 bg-yellow-500 rounded-full animate-pulse"></span>
     <span class="text-sm text-slate-600 dark:text-slate-400">%s</span>
    </div>
   </div>

   <div id="live-games" class="grid gap-4 md:grid-cols-2">
    <div class="bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-6 text-center">
     <div class="animate-pulse space-y-3">
      <div class="h-4 bg-slate-200 dark:bg-slate-700 rounded w-3/4 mx-auto"></div>
      <div class="h-8 bg-slate-200 dark:bg-slate-700 rounded w-1/2 mx-auto"></div>
      <div class="h-4 bg-slate-200 dark:bg-slate-700 rounded w-2/3 mx-auto"></div>
     </div>
    </div>
   </div>

   <div class="text-xs text-slate-500 dark:text-slate-400 text-center">
    %s
   </div>
  </div>

  <script>
  (function() {
   const gamesContainer = document.getElementById('live-games');
   const statusEl = document.getElementById('connection-status');

   function updateStatus(ok) {
    statusEl.innerHTML = ok
     ? '<span class="w-2 h-2 bg-green-500 rounded-full"></span><span class="text-sm text-slate-600 dark:text-slate-400">%s</span>'
     : '<span class="w-2 h-2 bg-red-500 rounded-full"></span><span class="text-sm text-slate-600 dark:text-slate-400">%s</span>';
   }

   function renderGame(game) {
    const q = (game.quarter || '').trim();
    const isLive = game.is_live === true;
    const isScheduled = !isLive && (q === '경기전' || q === '경기 전' || q === '예정' || (game.home_score === 0 && game.away_score === 0) || game.home_score == null || game.away_score == null);
    const isFinal = !isLive && !isScheduled;

    const badgeText = isLive ? (q || '%s') : (isScheduled ? '%s' : '%s');
    const badgeClass = isLive
     ? 'bg-orange-100 dark:bg-orange-900/30 text-orange-700 dark:text-orange-300'
     : (isFinal
        ? 'bg-emerald-100 dark:bg-emerald-900/30 text-emerald-700 dark:text-emerald-300'
        : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400');

    const timeText = (game.time || '').trim();
    const subText = isLive
     ? (timeText ? `${q} · ${timeText}` : q)
     : '';

    const scoreHtml = isScheduled
     ? '<div class="text-2xl font-black text-slate-500 dark:text-slate-400">VS</div>'
     : `<div class="flex items-center justify-center gap-4 text-3xl font-black">
        <span class="${game.home_score > game.away_score ? 'text-orange-500' : 'text-slate-900 dark:text-slate-200'}">${game.home_score}</span>
        <span class="text-slate-400 font-light">-</span>
        <span class="${game.away_score > game.home_score ? 'text-orange-500' : 'text-slate-900 dark:text-slate-200'}">${game.away_score}</span>
       </div>`;

	    return `
	     <a href="/boxscore/${game.game_id}" class="block bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-6 hover:border-orange-400 transition-colors">
	      <div class="flex justify-between items-center mb-4">
	       <span class="text-lg font-semibold text-slate-900 dark:text-slate-200">${game.home_team}</span>
	       <span class="text-xs px-2 py-1 rounded ${badgeClass}">${badgeText}</span>
	       <span class="text-lg font-semibold text-slate-900 dark:text-slate-200">${game.away_team}</span>
      </div>
      ${scoreHtml}
      ${subText ? `<div class="mt-3 text-xs text-slate-500 dark:text-slate-400 text-center">${subText}</div>` : ''}
     </a>
    `;
   }

   function renderGames(data) {
	    if (!data || !data.games || data.games.length === 0) {
	     gamesContainer.innerHTML = `
	      <div class="col-span-2 bg-white dark:bg-slate-900 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700 p-12 text-center">
		       <p class="text-slate-600 dark:text-slate-400">%s (${data && data.today ? data.today : ''})</p>
		       <a href="/games" class="text-orange-500 hover:underline mt-2 inline-block">%s →</a>
	      </div>
	     `;
	    } else {
     gamesContainer.innerHTML = data.games.map(renderGame).join('');
    }
   }

   async function fetchStatus() {
    try {
     const res = await fetch('/api/live/status', { cache: 'no-store' });
     if (!res.ok) throw new Error('bad status');
     const data = await res.json();
     updateStatus(true);
     renderGames(data);
    } catch (_) {
     updateStatus(false);
    }
   }

   fetchStatus();
   setInterval(fetchStatus, 20000);
	  })();
	  </script>
	 |html}
   (breadcrumb [("홈", "/"); ("실시간 점수", "")])
   (escape_html h1)
   (escape_html desc)
   (escape_html connecting)
   (escape_html auto_updates)
   (escape_js_string connected)
   (escape_js_string disconnected)
   (escape_js_string status_live)
   (escape_js_string status_scheduled)
   (escape_js_string status_final)
   (escape_js_string empty_today)
   (escape_js_string view_all_games)
 in
 layout
	  ~lang
	  ~title
	  ~canonical_path:"/live"
  ~description:
    (tr
       { ko = "WKBL 여자농구 실시간 스코어 - 오늘 경기 점수를 확인하세요."
	       ; en = "Live scores for today's games."
	       })
	  ~content:page_html
	    ()

(** Position-based leaderboard page *)
let position_leaders_page ?(lang=I18n.Ko) ?(player_info_map=None) ~season ~seasons ~position (leaders: (string * leader_entry list) list) =
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
  let position_btn pos label emoji =
    let active = if pos = position then "bg-orange-600 text-white" else "bg-slate-100 dark:bg-slate-800 text-slate-700 dark:text-slate-300 hover:bg-slate-200 dark:hover:bg-slate-700" in
    Printf.sprintf {html|<a href="/leaders/by-position?position=%s&season=%s" class="px-4 py-2 rounded-lg font-bold transition-colors %s">%s %s</a>|html} pos season active emoji (escape_html label)
  in
  let lookup stat =
    leaders |> List.find_opt (fun (k, _) -> k = stat) |> Option.map snd |> Option.value ~default:[]
  in
  let leader_table title stat entries =
    if List.length entries = 0 then
      Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6">
        <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200 mb-4">%s</h3>
        <p class="text-slate-500 dark:text-slate-400 text-center py-4">데이터가 없습니다</p>
      </div>|html} (escape_html title)
    else
      let name_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
      entries |> List.iter (fun (e: leader_entry) ->
        let key = normalize_name e.le_player_name in
        Hashtbl.replace name_counts key ((Hashtbl.find_opt name_counts key |> Option.value ~default:0) + 1));
      let show_id (e: leader_entry) =
        Hashtbl.find_opt name_counts (normalize_name e.le_player_name) |> Option.map (fun c -> c > 1) |> Option.value ~default:false
      in
      let info_opt player_id =
        match player_info_map with
        | Some map -> Hashtbl.find_opt map player_id
        | None -> None
      in
      let rows = entries |> List.mapi (fun i (e: leader_entry) ->
        let rank_badge = match i with
          | 0 -> {html|<span class="text-amber-500">🥇</span>|html}
          | 1 -> {html|<span class="text-slate-400">🥈</span>|html}
          | 2 -> {html|<span class="text-amber-700">🥉</span>|html}
          | _ -> Printf.sprintf {html|<span class="text-slate-500">%d</span>|html} (i + 1)
        in
        let disambiguation =
          if show_id e then
            player_disambiguation_line ~team_name:e.le_team_name ~player_id:e.le_player_id (info_opt e.le_player_id)
          else ""
        in
        Printf.sprintf {html|<tr class="border-b border-slate-200 dark:border-slate-800 hover:bg-slate-50 dark:hover:bg-slate-800/50">
          <td class="px-3 py-2 text-center font-bold">%s</td>
          <td class="px-3 py-2">
            <div class="flex flex-col">
              <a href="%s" class="text-slate-900 dark:text-slate-200 hover:text-orange-600 font-medium">%s</a>
              %s
              <span class="text-slate-500 text-xs">%s</span>
            </div>
          </td>
          <td class="px-3 py-2 text-right font-bold font-mono text-orange-700 dark:text-orange-400">%.1f</td>
        </tr>|html}
        rank_badge (player_href e.le_player_id) (escape_html e.le_player_name) disambiguation (escape_html e.le_team_name) e.le_stat_value
      ) |> String.concat "\n" in
      Printf.sprintf {html|<div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 overflow-hidden shadow-lg">
        <div class="px-4 py-3 bg-slate-50 dark:bg-slate-800/50 border-b border-slate-200 dark:border-slate-700">
          <h3 class="text-lg font-bold text-slate-900 dark:text-slate-200">%s</h3>
        </div>
        <table class="w-full text-sm">
          <thead class="bg-slate-100 dark:bg-slate-800 text-xs uppercase text-slate-500 dark:text-slate-400">
            <tr><th scope="col" class="px-3 py-2 text-center w-12">순위</th><th scope="col" class="px-3 py-2 text-left">선수</th><th scope="col" class="px-3 py-2 text-right w-20">%s</th></tr>
          </thead>
          <tbody>%s</tbody>
        </table>
      </div>|html} (escape_html title) (escape_html stat) rows
  in
  let position_label = match position with
    | "G" -> "가드 (Guard)"
    | "F" -> "포워드 (Forward)"
    | "C" -> "센터 (Center)"
    | _ -> "전체 포지션"
  in
  layout ~lang ~title:(position_label ^ " 리더보드 | WKBL")
    ~canonical_path:"/leaders/positions"
    ~description:(Printf.sprintf "%s 포지션별 득점, 리바운드, 어시스트 리더보드" position_label)
    ~content:(Printf.sprintf {html|<div class="space-y-6 animate-fade-in">%s
      <!-- Header -->
      <div class="bg-white dark:bg-slate-900 rounded-xl border border-slate-200 dark:border-slate-800 p-6 shadow-xl">
        <h1 class="text-2xl font-black text-slate-900 dark:text-slate-200 text-center mb-4">포지션별 리더보드</h1>
        <div class="flex flex-col md:flex-row items-center justify-center gap-4">
          <!-- Position Tabs -->
          <div class="flex flex-wrap justify-center gap-2">
            %s
            %s
            %s
            %s
          </div>
          <!-- Season Filter -->
          <form action="/leaders/by-position" method="get" class="flex items-center gap-2">
            <input type="hidden" name="position" value="%s" />
            <select name="season" aria-label="시즌 선택" class="bg-slate-100 dark:bg-slate-800 border border-slate-300 dark:border-slate-700 rounded px-3 py-2 text-sm" data-auto-submit="change">
              %s
            </select>
          </form>
        </div>
        <p class="text-center text-sm text-slate-500 dark:text-slate-400 mt-4">%s</p>
      </div>

      <!-- Stats Grid -->
      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        %s
        %s
        %s
        %s
      </div>
    </div>|html}
    (breadcrumb [("홈", "/"); ("리더", "/leaders"); ("포지션별", "")])
    (position_btn "ALL" "전체" "👥")
    (position_btn "G" "가드" "🏃")
    (position_btn "F" "포워드" "💪")
    (position_btn "C" "센터" "🏀")
    position
    season_options
    (escape_html position_label)
    (leader_table "득점 (PTS)" "PTS" (lookup "pts"))
    (leader_table "리바운드 (REB)" "REB" (lookup "reb"))
    (leader_table "어시스트 (AST)" "AST" (lookup "ast"))
    (leader_table "효율 (EFF)" "EFF" (lookup "eff"))
  ) ()
