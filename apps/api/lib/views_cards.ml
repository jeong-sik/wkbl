open Domain
open Views_common

let player_cards ?(lang=I18n.Ko) ?(player_info_map=None) (players: player_aggregate list) =
  let _ = lang in
  
  let get_team_color name =
    let hex = Domain.team_code_of_string name
      |> Option.map Domain.team_code_to_color
      |> Option.value ~default:"#F97316"
    in
    hex
  in

  let cards = players |> List.mapi (fun idx (p: player_aggregate) ->
    let team_color = get_team_color p.team_name in
    let photo_url = Printf.sprintf "https://wkbl.win/api/images/player/%s.png" p.player_id in
    
    let position, height = 
      match player_info_map with
      | Some map -> 
          (match Hashtbl.find_opt map p.player_id with
           | Some info -> 
               let pos = Option.value info.position ~default:"-" in
               let h = match info.height with Some h -> Printf.sprintf "%dcm" h | None -> "-" in
               (pos, h)
           | None -> ("-", "-"))
      | None -> ("-", "-")
    in
    
    let meta_info = if position <> "-" then Printf.sprintf "%s · %s" position height else "WKBL Player" in
    let rank = idx + 1 in
    
    Printf.sprintf {|
    <a href="/players/%s" class="block group relative overflow-hidden rounded-2xl bg-white dark:bg-slate-900 border border-slate-200 dark:border-slate-800 shadow-sm hover:shadow-lg transition-all duration-300 hover:-translate-y-1">
      <!-- Team Color Gradient Header Bar -->
      <div class="h-20 w-full" style="background: linear-gradient(135deg, %scc 0%%, %s 100%%);"></div>
      
      <!-- Ranking Badge -->
      <div class="absolute top-2 left-2 z-10 w-7 h-7 flex items-center justify-center rounded-full bg-black/40 backdrop-blur-sm text-white font-bold text-xs border border-white/20 shadow-sm">
        %d
      </div>
      
      <!-- Player Info Section -->
      <div class="px-5 pb-5 pt-0 relative">
        <!-- Photo overlapping header -->
        <div class="absolute -top-12 left-1/2 -translate-x-1/2 transition-transform duration-300 group-hover:scale-105">
          <div class="w-24 h-24 rounded-full border-4 border-white dark:border-slate-900 bg-slate-100 dark:bg-slate-800 overflow-hidden shadow-sm">
            <img src="%s" alt="%s" class="w-full h-full object-cover" onerror="this.src='/static/images/placeholder.svg'" />
          </div>
        </div>
        
        <!-- Team Badge (Top Right of Body) -->
        <div class="absolute top-3 right-5">
          <span class="text-[10px] font-bold px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-300 border border-slate-200 dark:border-slate-700">
            %s
          </span>
        </div>
        
        <div class="mt-14 text-center">
          <h3 class="text-xl font-extrabold text-slate-900 dark:text-white group-hover:text-[%s] transition-colors">
            %s
          </h3>
          <p class="text-xs font-medium text-slate-500 dark:text-slate-400 mt-1">%s</p>
        </div>
        
        <!-- Key Stats Grid -->
        <div class="grid grid-cols-4 gap-1 mt-5 pt-4 border-t border-slate-100 dark:border-slate-800/50">
          <div class="flex flex-col items-center p-1 rounded-lg hover:bg-slate-50 dark:hover:bg-slate-800/50 transition-colors">
            <div class="text-[10px] text-slate-400 dark:text-slate-500 font-bold tracking-wider">EFF</div>
            <div class="text-base font-black text-emerald-600 dark:text-emerald-400 leading-tight">%.1f</div>
          </div>
          <div class="flex flex-col items-center p-1 rounded-lg hover:bg-slate-50 dark:hover:bg-slate-800/50 transition-colors">
            <div class="text-[10px] text-slate-400 dark:text-slate-500 font-bold tracking-wider">PTS</div>
            <div class="text-base font-bold text-slate-800 dark:text-slate-200 leading-tight">%.1f</div>
          </div>
          <div class="flex flex-col items-center p-1 rounded-lg hover:bg-slate-50 dark:hover:bg-slate-800/50 transition-colors">
            <div class="text-[10px] text-slate-400 dark:text-slate-500 font-bold tracking-wider">REB</div>
            <div class="text-base font-bold text-slate-800 dark:text-slate-200 leading-tight">%.1f</div>
          </div>
          <div class="flex flex-col items-center p-1 rounded-lg hover:bg-slate-50 dark:hover:bg-slate-800/50 transition-colors">
            <div class="text-[10px] text-slate-400 dark:text-slate-500 font-bold tracking-wider">AST</div>
            <div class="text-base font-bold text-slate-800 dark:text-slate-200 leading-tight">%.1f</div>
          </div>
        </div>
      </div>
    </a>
    |} p.player_id team_color team_color rank photo_url (escape_html p.name) (escape_html p.team_name) team_color (escape_html p.name) meta_info p.efficiency p.avg_points p.avg_rebounds p.avg_assists
  ) |> String.concat "\n" in
  
  Printf.sprintf {|
  <div class="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 xl:grid-cols-4 gap-6">
    %s
  </div>
  |} cards
