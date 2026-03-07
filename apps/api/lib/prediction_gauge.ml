let gauge_chart ~home_pct ~away_pct =
  let radius = 100.0 in
  let stroke_width = 24.0 in
  
  let total_len = Float.pi *. radius in
  let home_len = total_len *. (home_pct /. 100.0) in
  let away_len = total_len *. (away_pct /. 100.0) in
  
  Printf.sprintf {|
  <div class="relative w-full max-w-[280px] mx-auto overflow-hidden group py-4">
    <svg viewBox="0 0 240 130" class="w-full h-auto drop-shadow-lg filter" aria-label="Win Probability Gauge">
      <!-- Background Track -->
      <path d="M 20 110 A 100 100 0 0 1 220 110" fill="none" stroke="currentColor" stroke-width="%f" stroke-linecap="round" class="text-slate-100 dark:text-slate-800/80" />
      
      <!-- Away Team Arc (Right side) -->
      <path d="M 220 110 A 100 100 0 0 0 20 110" fill="none" stroke="url(#awayGradient)" stroke-width="%f" stroke-linecap="round" stroke-dasharray="%f %f" stroke-dashoffset="0" class="transition-all duration-1000 ease-out origin-center" style="transform: rotate(180deg);" />
      
      <!-- Home Team Arc (Left side) -->
      <path d="M 20 110 A 100 100 0 0 1 220 110" fill="none" stroke="url(#homeGradient)" stroke-width="%f" stroke-linecap="round" stroke-dasharray="%f %f" stroke-dashoffset="0" class="transition-all duration-1000 ease-out" />
      
      <!-- Center Needle / Indicator -->
      <g class="transition-transform duration-1000 ease-out origin-[120px_110px]" style="transform: rotate(%.1fdeg)">
        <polygon points="116,110 124,110 120,20" fill="currentColor" class="text-slate-800 dark:text-slate-200 drop-shadow-md" />
        <circle cx="120" cy="110" r="8" fill="currentColor" class="text-slate-900 dark:text-slate-100 shadow-sm" />
        <circle cx="120" cy="110" r="3" fill="currentColor" class="text-slate-300 dark:text-slate-600" />
      </g>
      
      <defs>
        <linearGradient id="homeGradient" x1="0%%" y1="0%%" x2="100%%" y2="0%%">
          <stop offset="0%%" stop-color="#f97316" />
          <stop offset="100%%" stop-color="#ea580c" />
        </linearGradient>
        <linearGradient id="awayGradient" x1="0%%" y1="0%%" x2="100%%" y2="0%%">
          <stop offset="0%%" stop-color="#38bdf8" />
          <stop offset="100%%" stop-color="#0284c7" />
        </linearGradient>
      </defs>
    </svg>
    
    <div class="absolute bottom-2 left-0 right-0 flex justify-between px-4 text-center">
      <div class="flex flex-col animate-fade-in text-left">
        <span class="text-xs font-bold text-orange-600 dark:text-orange-500 uppercase tracking-widest mb-1">홈 승리</span>
        <span class="text-3xl font-black text-transparent bg-clip-text bg-gradient-to-r from-orange-500 to-orange-700 drop-shadow-sm tabular-nums">%.1f<span class="text-lg">%%</span></span>
      </div>
      <div class="flex flex-col animate-fade-in text-right">
        <span class="text-xs font-bold text-sky-600 dark:text-sky-500 uppercase tracking-widest mb-1">원정 승리</span>
        <span class="text-3xl font-black text-transparent bg-clip-text bg-gradient-to-l from-sky-400 to-sky-600 drop-shadow-sm tabular-nums">%.1f<span class="text-lg">%%</span></span>
      </div>
    </div>
  </div>
  |} stroke_width stroke_width away_len total_len stroke_width home_len total_len (180.0 *. (home_pct /. 100.0) -. 90.0) home_pct away_pct