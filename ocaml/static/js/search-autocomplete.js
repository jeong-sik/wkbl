/**
 * Search Autocomplete
 * Fuzzy search with keyboard navigation for player/team names
 */
(() => {
  const DEBOUNCE_MS = 150;
  const MIN_CHARS = 1;
  const MAX_RESULTS = 8;

  // Simple fuzzy match scoring
  const fuzzyScore = (query, target) => {
    const q = query.toLowerCase();
    const t = target.toLowerCase();

    // Exact match at start = highest score
    if (t.startsWith(q)) return 100 + (q.length / t.length) * 50;

    // Contains = medium score
    if (t.includes(q)) return 50 + (q.length / t.length) * 25;

    // Character-by-character fuzzy
    let score = 0;
    let lastIndex = -1;
    for (const char of q) {
      const index = t.indexOf(char, lastIndex + 1);
      if (index === -1) return 0;
      score += 10 - Math.min(index - lastIndex - 1, 10);
      lastIndex = index;
    }
    return score;
  };

  const createAutocomplete = (input) => {
    if (input.dataset.autocompleteInit) return;
    input.dataset.autocompleteInit = 'true';

    const wrapper = document.createElement('div');
    wrapper.className = 'relative';
    input.parentNode.insertBefore(wrapper, input);
    wrapper.appendChild(input);

    const dropdown = document.createElement('div');
    dropdown.className = [
      'absolute top-full left-0 right-0 z-50 mt-1',
      'bg-white dark:bg-slate-800 rounded-lg shadow-xl',
      'border border-slate-200 dark:border-slate-700',
      'max-h-80 overflow-y-auto hidden'
    ].join(' ');
    dropdown.setAttribute('role', 'listbox');
    dropdown.id = `autocomplete-${Date.now()}`;
    wrapper.appendChild(dropdown);

    input.setAttribute('role', 'combobox');
    input.setAttribute('aria-autocomplete', 'list');
    input.setAttribute('aria-controls', dropdown.id);
    input.setAttribute('aria-expanded', 'false');

    let items = [];
    let selectedIndex = -1;
    let debounceTimer = null;

    // Load search data from existing page elements or fetch
    const loadSearchData = () => {
      const data = [];

      // Collect from data attributes or page content
      document.querySelectorAll('[data-search-item]').forEach(el => {
        data.push({
          name: el.dataset.searchItem,
          type: el.dataset.searchType || 'item',
          url: el.dataset.searchUrl || el.href || '#',
          extra: el.dataset.searchExtra || ''
        });
      });

      // If no explicit search items, try collecting from links
      if (data.length === 0) {
        // Player links
        document.querySelectorAll('a[href^="/player/"]').forEach(a => {
          const name = a.textContent?.trim();
          if (name && name.length > 1) {
            data.push({ name, type: 'player', url: a.href, extra: '선수' });
          }
        });

        // Team links
        document.querySelectorAll('a[href^="/team/"]').forEach(a => {
          const name = a.textContent?.trim();
          if (name && name.length > 1) {
            data.push({ name, type: 'team', url: a.href, extra: '팀' });
          }
        });
      }

      // Deduplicate
      const seen = new Set();
      return data.filter(item => {
        const key = `${item.type}:${item.name}`;
        if (seen.has(key)) return false;
        seen.add(key);
        return true;
      });
    };

    const search = (query) => {
      if (!query || query.length < MIN_CHARS) {
        hideDropdown();
        return;
      }

      const searchData = loadSearchData();
      const results = searchData
        .map(item => ({ ...item, score: fuzzyScore(query, item.name) }))
        .filter(item => item.score > 0)
        .sort((a, b) => b.score - a.score)
        .slice(0, MAX_RESULTS);

      if (results.length === 0) {
        hideDropdown();
        return;
      }

      items = results;
      renderDropdown(results, query);
    };

    const highlightMatch = (text, query) => {
      const q = query.toLowerCase();
      const t = text.toLowerCase();
      const idx = t.indexOf(q);
      if (idx === -1) return text;
      return text.slice(0, idx) +
        `<mark class="bg-orange-200 dark:bg-orange-500/30 text-inherit rounded px-0.5">${text.slice(idx, idx + q.length)}</mark>` +
        text.slice(idx + q.length);
    };

    const renderDropdown = (results, query) => {
      const typeIcons = {
        player: '👤',
        team: '🏀',
        game: '📅',
        item: '🔍'
      };

      dropdown.innerHTML = results.map((item, i) => `
        <a href="${item.url}"
           class="flex items-center gap-3 px-4 py-3 hover:bg-slate-100 dark:hover:bg-slate-700 cursor-pointer transition-colors ${i === selectedIndex ? 'bg-slate-100 dark:bg-slate-700' : ''}"
           role="option"
           data-index="${i}"
           aria-selected="${i === selectedIndex}">
          <span class="text-lg">${typeIcons[item.type] || '🔍'}</span>
          <div class="flex-1 min-w-0">
            <div class="font-medium text-slate-900 dark:text-white truncate">${highlightMatch(item.name, query)}</div>
            ${item.extra ? `<div class="text-xs text-slate-500 dark:text-slate-400">${item.extra}</div>` : ''}
          </div>
        </a>
      `).join('');

      showDropdown();
    };

    const showDropdown = () => {
      dropdown.classList.remove('hidden');
      input.setAttribute('aria-expanded', 'true');
    };

    const hideDropdown = () => {
      dropdown.classList.add('hidden');
      input.setAttribute('aria-expanded', 'false');
      selectedIndex = -1;
      items = [];
    };

    const selectItem = (index) => {
      if (index < 0 || index >= items.length) return;
      window.location.href = items[index].url;
    };

    const updateSelection = (newIndex) => {
      selectedIndex = Math.max(-1, Math.min(newIndex, items.length - 1));
      dropdown.querySelectorAll('[role="option"]').forEach((el, i) => {
        const isSelected = i === selectedIndex;
        el.classList.toggle('bg-slate-100', isSelected);
        el.classList.toggle('dark:bg-slate-700', isSelected);
        el.setAttribute('aria-selected', isSelected);
        if (isSelected) el.scrollIntoView({ block: 'nearest' });
      });
    };

    // Event listeners
    input.addEventListener('input', (e) => {
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(() => search(e.target.value), DEBOUNCE_MS);
    });

    input.addEventListener('keydown', (e) => {
      if (!items.length) return;

      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          updateSelection(selectedIndex + 1);
          break;
        case 'ArrowUp':
          e.preventDefault();
          updateSelection(selectedIndex - 1);
          break;
        case 'Enter':
          if (selectedIndex >= 0) {
            e.preventDefault();
            selectItem(selectedIndex);
          }
          break;
        case 'Escape':
          hideDropdown();
          break;
      }
    });

    input.addEventListener('focus', () => {
      if (input.value.length >= MIN_CHARS) {
        search(input.value);
      }
    });

    input.addEventListener('blur', () => {
      // Delay to allow click on dropdown items
      setTimeout(hideDropdown, 200);
    });

    dropdown.addEventListener('click', (e) => {
      const item = e.target.closest('[data-index]');
      if (item) {
        e.preventDefault();
        selectItem(parseInt(item.dataset.index));
      }
    });
  };

  const init = () => {
    // Initialize all search inputs
    document.querySelectorAll('input[data-search-autocomplete], input[type="search"]').forEach(createAutocomplete);

    // Handle HTMX content swaps
    document.body.addEventListener('htmx:afterSwap', () => {
      document.querySelectorAll('input[data-search-autocomplete], input[type="search"]').forEach(createAutocomplete);
    });
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
