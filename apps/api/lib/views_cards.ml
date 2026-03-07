open Domain
open Views_common

let player_cards ?(lang=I18n.Ko) ?(player_info_map=None) (players: player_aggregate list) =
  let _ = lang in
  let _ = player_info_map in
  
  let get_team_color name =
    let hex = Domain.team_code_of_string name
      |> Option.map Domain.team_code_to_color
      |> Option.value ~default:"#F97316"
    in
    hex
  in

  let cards = players |> List.map (fun (p: player_aggregate) ->
    let team_color = get_team_color p.team_name in
    let photo_url = Printf.sprintf "/api/images/player/%s.png" p.player_id in
    
    Printf.sprintf {|
    <a href="/players/%s" class="block group relative overflow-hidden rounded-2xl bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 shadow-sm hover:shadow-md transition-all duration-200 hover:-translate-y-1">
      <!-- Team Color Header Bar -->
      <div class="h-16 w-full" style="background-color: %s; opacity: 0.8;"></div>
      
      <!-- Player Info Section -->
      <div class="px-5 pb-5 pt-0 relative">
        <!-- Photo overlapping header -->
        <div class="absolute -top-10 left-5">
          <div class="w-20 h-20 rounded-full border-4 border-white dark:border-slate-900 bg-slate-100 dark:bg-slate-800 overflow-hidden">
            <img src="%s" alt="%s" class="w-full h-full object-cover" onerror="this.src='/static/images/placeholder.svg'" />
          </div>
        </div>
        
        <!-- Team Badge (Top Right) -->
        <div class="absolute top-3 right-5">
          <span class="text-xs font-bold px-2 py-1 rounded-full bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-300 border border-slate-200 dark:border-slate-700">
            %s
          </span>
        </div>
        
        <div class="mt-12">
          <h3 class="text-lg font-bold text-slate-900 dark:text-white group-hover:text-emerald-600 dark:group-hover:text-emerald-400 transition-colors">
            %s
          </h3>
          <p class="text-sm text-slate-500 dark:text-slate-400">WKBL %s</p>
        </div>
        
        <!-- Key Stats Grid -->
        <div class="grid grid-cols-4 gap-2 mt-5 py-3 border-t border-b border-slate-100 dark:border-slate-800">
          <div class="text-center">
            <div class="text-[10px] text-slate-500 uppercase font-semibold">EFF</div>
            <div class="text-sm font-bold text-slate-900 dark:text-white">%.1f</div>
          </div>
          <div class="text-center">
            <div class="text-[10px] text-slate-500 uppercase font-semibold">PTS</div>
            <div class="text-sm font-bold text-slate-900 dark:text-white">%.1f</div>
          </div>
          <div class="text-center">
            <div class="text-[10px] text-slate-500 uppercase font-semibold">REB</div>
            <div class="text-sm font-bold text-slate-900 dark:text-white">%.1f</div>
          </div>
          <div class="text-center">
            <div class="text-[10px] text-slate-500 uppercase font-semibold">AST</div>
            <div class="text-sm font-bold text-slate-900 dark:text-white">%.1f</div>
          </div>
        </div>
      </div>
    </a>
    |} p.player_id team_color photo_url (escape_html p.name) (escape_html p.team_name) (escape_html p.name) p.player_id p.efficiency p.avg_points p.avg_rebounds p.avg_assists
  ) |> String.concat "
" in
  
  Printf.sprintf {|
  <div class="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 xl:grid-cols-4 gap-6">
    %s
  </div>
  |} cards