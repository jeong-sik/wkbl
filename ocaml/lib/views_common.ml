<table class="min-w-[680px] sm:min-w-[860px] lg:min-w-[980px] w-full text-xs sm:text-sm font-mono tabular-nums table-fixed" aria-label="선수 스탯 순위">
      <colgroup>
        <col class="w-12" />
        <col class="w-auto" />
        <col class="w-[140px]" />
        <col class="w-16 hidden sm:table-column" />
        <col class="w-20" />
        <col class="w-20 hidden md:table-column" />
        <col class="w-20" />
        <col class="w-20 hidden md:table-column" />
        <col class="w-20 hidden lg:table-column" />
        <col class="w-20 hidden lg:table-column" />
        <col class="w-20 hidden lg:table-column" />
        <col class="w-20" />
        <col class="w-20 hidden sm:table-column" />
      </colgroup>
      <thead class="bg-slate-100 dark:bg-slate-800/80 sticky top-0 z-10 text-slate-500 dark:text-slate-400 text-[10px] sm:text-xs uppercase tracking-wider whitespace-nowrap font-mono">
        <tr>
          <th scope="col" class="px-2 py-2 text-center">#</th>
          <th scope="col" class="px-3 py-2 text-left font-sans">Player</th>
          <th scope="col" class="px-3 py-2 text-left font-sans">Team</th>
          <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell">GP</th>
          <th scope="col" class="px-3 py-2 text-right cursor-pointer hover:text-orange-600 dark:text-orange-400" hx-get="/players/table?sort=pts" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">PTS</th>
          <th scope="col" class="px-3 py-2 text-right cursor-pointer hover:text-orange-600 dark:text-orange-400 hidden md:table-cell" hx-get="/players/table?sort=mg" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">MG</th>
          <th scope="col" class="px-3 py-2 text-right cursor-pointer hover:text-orange-600 dark:text-orange-400" hx-get="/players/table?sort=reb" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">REB</th>
          <th scope="col" class="px-3 py-2 text-right cursor-pointer hover:text-orange-600 dark:text-orange-400 hidden md:table-cell" hx-get="/players/table?sort=ast" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">AST</th>
          <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell">STL</th>
          <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell">BLK</th>
          <th scope="col" class="px-3 py-2 text-right hidden lg:table-cell">TO</th>
          <th scope="col" class="px-3 py-2 text-right cursor-pointer text-orange-600 dark:text-orange-400 border-b-2 border-orange-500/50 bg-orange-500/5" hx-get="/players/table?sort=eff" hx-target="#players-table" hx-swap="innerHTML" hx-include="#players-filter">EFF ↓</th>
          <th scope="col" class="px-3 py-2 text-right hidden sm:table-cell">PER</th>
        </tr>
      </thead>
      <tbody id="players-body">%s</tbody>
    </table></div>|html}
    rows