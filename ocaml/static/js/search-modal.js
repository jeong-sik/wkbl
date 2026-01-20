/**
 * Command Palette / Quick Search (Cmd+K / Ctrl+K)
 * - Keyboard-driven navigation
 * - Fuzzy search for pages and players
 * - Accessible modal with focus trap
 */
(function() {
  'use strict';

  let modal = null;
  let input = null;
  let resultsList = null;
  let selectedIndex = -1;
  let lastFocusedElement = null;

  // Static navigation items (Korean + English for search)
  const NAV_ITEMS = [
    { type: 'page', name: '홈', alias: 'Home', path: '/', icon: '🏠' },
    { type: 'page', name: '시상식', alias: 'Awards', path: '/awards', icon: '🏆' },
    { type: 'page', name: '리더스', alias: 'Leaders Stats', path: '/leaders', icon: '📊' },
    { type: 'page', name: '박스스코어', alias: 'Boxscores', path: '/boxscores', icon: '📋' },
    { type: 'page', name: '경기일정', alias: 'Games Schedule', path: '/games', icon: '📅' },
    { type: 'page', name: '순위', alias: 'Standings', path: '/standings', icon: '🏅' },
    { type: 'page', name: '팀', alias: 'Teams', path: '/teams', icon: '👥' },
    { type: 'page', name: '선수', alias: 'Players', path: '/players', icon: '👤' },
    { type: 'page', name: '승부예측', alias: 'Predict', path: '/predict', icon: '🔮' },
    { type: 'page', name: '선수비교', alias: 'Compare Players', path: '/compare', icon: '⚖️' },
    { type: 'page', name: '드래프트/트레이드', alias: 'Draft Trade Transactions', path: '/transactions', icon: '📜' },
    { type: 'page', name: 'QA 대시보드', alias: 'QA Dashboard', path: '/qa', icon: '✅' }
  ];

  function init() {
    createModal();
    bindKeyboardShortcut();
  }

  function createModal() {
    // Create modal HTML
    const modalHtml = `
      <div id="search-modal" class="fixed inset-0 z-[200] hidden" role="dialog" aria-modal="true" aria-label="빠른 검색">
        <div class="absolute inset-0 bg-black/50 backdrop-blur-sm" data-search-backdrop></div>
        <div class="absolute top-[10%] left-1/2 -translate-x-1/2 w-full max-w-lg px-4">
          <div class="bg-white dark:bg-slate-800 rounded-xl shadow-2xl border border-slate-200 dark:border-slate-700 overflow-hidden">
            <!-- Search Input -->
            <div class="flex items-center gap-3 px-4 py-3 border-b border-slate-200 dark:border-slate-700">
              <svg class="w-5 h-5 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path>
              </svg>
              <input
                type="text"
                id="search-input"
                class="flex-1 bg-transparent outline-none text-slate-900 dark:text-white placeholder-slate-400"
                placeholder="페이지 검색... (Esc로 닫기)"
                autocomplete="off"
                spellcheck="false"
              >
              <kbd class="hidden sm:inline-flex items-center gap-1 px-2 py-1 text-xs font-mono text-slate-400 bg-slate-100 dark:bg-slate-700 rounded">
                Esc
              </kbd>
            </div>
            <!-- Status announcer for screen readers -->
            <div id="search-status" class="sr-only" aria-live="polite" aria-atomic="true"></div>
            <!-- Results List -->
            <ul id="search-results" class="max-h-80 overflow-y-auto py-2" role="listbox" aria-label="검색 결과">
              <!-- Results populated by JS -->
            </ul>
            <!-- Footer Hint -->
            <div class="px-4 py-2 text-xs text-slate-400 border-t border-slate-200 dark:border-slate-700 flex items-center gap-4">
              <span class="flex items-center gap-1">
                <kbd class="px-1.5 py-0.5 bg-slate-100 dark:bg-slate-700 rounded">↑</kbd>
                <kbd class="px-1.5 py-0.5 bg-slate-100 dark:bg-slate-700 rounded">↓</kbd>
                이동
              </span>
              <span class="flex items-center gap-1">
                <kbd class="px-1.5 py-0.5 bg-slate-100 dark:bg-slate-700 rounded">Enter</kbd>
                선택
              </span>
            </div>
          </div>
        </div>
      </div>
    `;

    document.body.insertAdjacentHTML('beforeend', modalHtml);
    modal = document.getElementById('search-modal');
    input = document.getElementById('search-input');
    resultsList = document.getElementById('search-results');

    // Event listeners
    input.addEventListener('input', handleInput);
    input.addEventListener('keydown', handleKeydown);
    modal.querySelector('[data-search-backdrop]').addEventListener('click', close);
    // Event delegation for result items (accessibility)
    resultsList.addEventListener('click', handleResultClick);
    resultsList.addEventListener('keydown', handleResultKeydown);

    // Show initial results
    renderResults(NAV_ITEMS);
  }

  function bindKeyboardShortcut() {
    document.addEventListener('keydown', function(e) {
      // Cmd+K or Ctrl+K
      if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
        e.preventDefault();
        toggle();
      }
      // Escape to close
      if (e.key === 'Escape' && isOpen()) {
        close();
      }
    });
  }

  function isOpen() {
    return modal && !modal.classList.contains('hidden');
  }

  function open() {
    lastFocusedElement = document.activeElement;
    modal.classList.remove('hidden');
    input.value = '';
    input.focus();
    selectedIndex = -1;
    renderResults(NAV_ITEMS);
    document.body.style.overflow = 'hidden';
  }

  function close() {
    modal.classList.add('hidden');
    document.body.style.overflow = '';
    if (lastFocusedElement) {
      lastFocusedElement.focus();
    }
  }

  function toggle() {
    if (isOpen()) {
      close();
    } else {
      open();
    }
  }

  let searchTimeout = null;
  let lastQuery = '';

  function handleInput() {
    const query = input.value.toLowerCase().trim();
    selectedIndex = -1;

    if (query === '') {
      renderResults(NAV_ITEMS);
      lastQuery = '';
      return;
    }

    // Filter nav items (search name, alias, and path)
    const filtered = NAV_ITEMS.filter(function(item) {
      return item.name.toLowerCase().includes(query) ||
             (item.alias && item.alias.toLowerCase().includes(query)) ||
             item.path.toLowerCase().includes(query);
    });

    // Immediately show nav results
    renderResults(filtered);

    // Debounced player search (only if 2+ chars)
    if (query.length >= 2 && query !== lastQuery) {
      lastQuery = query;
      if (searchTimeout) clearTimeout(searchTimeout);
      searchTimeout = setTimeout(function() {
        searchPlayers(query, filtered);
      }, 200);
    }
  }

  function searchPlayers(query, navResults) {
    fetch('/api/search/players?q=' + encodeURIComponent(query))
      .then(function(res) { return res.json(); })
      .then(function(players) {
        if (players.length > 0) {
          var playerItems = players.map(function(p) {
            return {
              type: 'player',
              name: p.name,
              alias: p.team + ' · ' + p.pts + ' PPG',
              path: '/player/' + p.id,
              icon: '🏀'
            };
          });
          // Combine: nav results first, then players
          renderResults(navResults.concat(playerItems));
        }
      })
      .catch(function() { /* silent fail */ });
  }

  function handleKeydown(e) {
    const items = resultsList.querySelectorAll('[role="option"]');
    const itemCount = items.length;

    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        selectedIndex = (selectedIndex + 1) % itemCount;
        updateSelection(items);
        break;

      case 'ArrowUp':
        e.preventDefault();
        selectedIndex = selectedIndex <= 0 ? itemCount - 1 : selectedIndex - 1;
        updateSelection(items);
        break;

      case 'Enter':
        e.preventDefault();
        if (selectedIndex >= 0 && items[selectedIndex]) {
          const path = items[selectedIndex].dataset.path;
          if (path) {
            window.location.href = path;
          }
        } else if (items.length > 0) {
          // Select first item if none selected
          const path = items[0].dataset.path;
          if (path) {
            window.location.href = path;
          }
        }
        break;

      case 'Tab':
        // Trap focus in modal - keep focus on input
        e.preventDefault();
        break;
    }
  }

  function updateSelection(items) {
    items.forEach(function(item, idx) {
      if (idx === selectedIndex) {
        item.classList.add('bg-orange-50', 'dark:bg-orange-900/20');
        item.setAttribute('aria-selected', 'true');
        item.scrollIntoView({ block: 'nearest' });
      } else {
        item.classList.remove('bg-orange-50', 'dark:bg-orange-900/20');
        item.setAttribute('aria-selected', 'false');
      }
    });
  }

  function announceStatus(message) {
    var status = document.getElementById('search-status');
    if (status) status.textContent = message;
  }

  function renderResults(items) {
    if (items.length === 0) {
      resultsList.innerHTML = `
        <li class="px-4 py-8 text-center text-slate-400" role="status">
          <svg class="w-12 h-12 mx-auto mb-2 text-slate-300 dark:text-slate-600" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M9.172 16.172a4 4 0 015.656 0M9 10h.01M15 10h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
          </svg>
          검색 결과가 없습니다
        </li>
      `;
      announceStatus('검색 결과가 없습니다');
      return;
    }
    announceStatus(items.length + '개의 결과가 있습니다');

    resultsList.innerHTML = items.map(function(item) {
      return `
        <li
          role="option"
          aria-selected="false"
          tabindex="0"
          data-path="${item.path}"
          class="flex items-center gap-3 px-4 py-2 cursor-pointer hover:bg-slate-50 dark:hover:bg-slate-700/50 transition focus:outline-none focus:ring-2 focus:ring-orange-500"
        >
          <span class="text-lg" aria-hidden="true">${item.icon}</span>
          <span class="flex-1 text-slate-900 dark:text-white">${item.name}</span>
          <span class="text-xs text-slate-400">${item.path}</span>
        </li>
      `;
    }).join('');
  }

  function navigateToPath(path) {
    if (path) window.location.href = path;
  }

  function handleResultClick(e) {
    var li = e.target.closest('[role="option"]');
    if (li) navigateToPath(li.dataset.path);
  }

  function handleResultKeydown(e) {
    if (e.key === 'Enter' || e.key === ' ') {
      var li = e.target.closest('[role="option"]');
      if (li) {
        e.preventDefault();
        navigateToPath(li.dataset.path);
      }
    }
  }

  // Expose for external use
  window.SearchModal = {
    open: open,
    close: close,
    toggle: toggle
  };

  // Initialize when DOM ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
