/**
 * Client-side table sorting for WKBL data tables
 *
 * Design:
 * - No HTMX round-trip, instant sort on header click
 * - Supports numeric, text, and percentage values
 * - Maintains stable sort for equal values
 * - Updates URL query params for shareable state
 * - Visual sort direction indicators
 */

(function() {
  'use strict';

  // Sort state per table
  const sortState = new Map();

  /**
   * Parse cell value for sorting
   * Handles: numbers, percentages, text, dashes (treat as 0)
   */
  function parseValue(text, type) {
    const trimmed = text.trim();

    // Dash or empty = treat as minimum
    if (trimmed === '-' || trimmed === '' || trimmed === 'N/A') {
      return type === 'text' ? '' : Number.NEGATIVE_INFINITY;
    }

    // Percentage (e.g., "45.2%")
    if (trimmed.endsWith('%')) {
      return parseFloat(trimmed) || 0;
    }

    // Number (including negatives and decimals)
    const num = parseFloat(trimmed.replace(/,/g, ''));
    if (!isNaN(num)) {
      return num;
    }

    // Text fallback
    return trimmed.toLowerCase();
  }

  /**
   * Detect column data type from first non-empty cell
   */
  function detectType(table, colIndex) {
    const rows = table.querySelectorAll('tbody tr');
    for (const row of rows) {
      const cell = row.cells[colIndex];
      if (!cell) continue;
      const text = cell.textContent.trim();
      if (text === '-' || text === '') continue;

      if (text.endsWith('%')) return 'percent';
      if (!isNaN(parseFloat(text.replace(/,/g, '')))) return 'number';
      return 'text';
    }
    return 'text';
  }

  /**
   * Sort table by column
   */
  function sortTable(table, colIndex, direction) {
    const tbody = table.querySelector('tbody');
    if (!tbody) return;

    const rows = Array.from(tbody.querySelectorAll('tr'));
    const type = detectType(table, colIndex);

    // Stable sort with original index
    const indexed = rows.map((row, i) => ({ row, index: i }));

    indexed.sort((a, b) => {
      const aCell = a.row.cells[colIndex];
      const bCell = b.row.cells[colIndex];
      if (!aCell || !bCell) return 0;

      const aVal = parseValue(aCell.textContent, type);
      const bVal = parseValue(bCell.textContent, type);

      let cmp = 0;
      if (typeof aVal === 'string' && typeof bVal === 'string') {
        cmp = aVal.localeCompare(bVal, 'ko');
      } else {
        cmp = aVal < bVal ? -1 : aVal > bVal ? 1 : 0;
      }

      // Stable sort: use original index if equal
      if (cmp === 0) {
        return a.index - b.index;
      }

      return direction === 'desc' ? -cmp : cmp;
    });

    // Re-append rows in sorted order and reapply stripe classes
    indexed.forEach(({ row }, i) => {
      tbody.appendChild(row);
      // Toggle stripe: odd rows get the subtle background
      row.classList.remove('bg-slate-50/50', 'dark:bg-slate-800/20');
      if (i % 2 === 1) {
        row.classList.add('bg-slate-50/50', 'dark:bg-slate-800/20');
      }
    });
  }

  /**
   * Update sort indicator icons
   */
  function updateIndicators(table, activeColIndex, direction) {
    const headers = table.querySelectorAll('thead th');
    headers.forEach((th, i) => {
      const indicator = th.querySelector('.sort-indicator');
      if (!indicator) return;

      if (i === activeColIndex) {
        indicator.setAttribute('data-sort', direction);
        indicator.innerHTML = direction === 'asc'
          ? '<svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7"></path></svg>'
          : '<svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7"></path></svg>';
      } else {
        indicator.removeAttribute('data-sort');
        indicator.innerHTML = '<svg class="w-3 h-3 opacity-30" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4"></path></svg>';
      }
    });
  }

  /**
   * Update URL with sort params (for shareable state)
   */
  function updateUrl(tableId, colKey, direction) {
    const url = new URL(window.location);
    if (colKey && direction) {
      url.searchParams.set('sort', colKey);
      url.searchParams.set('order', direction);
    } else {
      url.searchParams.delete('sort');
      url.searchParams.delete('order');
    }
    window.history.replaceState({}, '', url);
  }

  /**
   * Initialize sortable table
   */
  function initSortableTable(table) {
    const headers = table.querySelectorAll('thead th[data-sortable]');
    if (!headers.length) return;

    headers.forEach((th, colIndex) => {
      // Add sort indicator if not present
      if (!th.querySelector('.sort-indicator')) {
        const indicator = document.createElement('span');
        indicator.className = 'sort-indicator ml-1 inline-block';
        indicator.innerHTML = '<svg class="w-3 h-3 opacity-30" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4"></path></svg>';
        th.appendChild(indicator);
      }

      // Make header clickable
      th.style.cursor = 'pointer';
      th.setAttribute('role', 'button');
      th.setAttribute('aria-label', `Sort by ${th.textContent.trim()}`);

      th.addEventListener('click', () => {
        const tableId = table.id || 'default';
        const colKey = th.dataset.sortKey || colIndex.toString();
        const currentState = sortState.get(tableId) || {};

        // Toggle direction
        let direction = 'desc';
        if (currentState.col === colIndex) {
          direction = currentState.dir === 'desc' ? 'asc' : 'desc';
        }

        // Update state
        sortState.set(tableId, { col: colIndex, dir: direction });

        // Sort and update UI
        sortTable(table, colIndex, direction);
        updateIndicators(table, colIndex, direction);
        updateUrl(tableId, colKey, direction);
      });
    });

    // Apply initial sort from URL if present
    const url = new URL(window.location);
    const sortKey = url.searchParams.get('sort');
    const sortOrder = url.searchParams.get('order');

    if (sortKey && sortOrder) {
      const th = table.querySelector(`thead th[data-sort-key="${sortKey}"]`);
      if (th) {
        const colIndex = Array.from(th.parentElement.children).indexOf(th);
        sortState.set(table.id || 'default', { col: colIndex, dir: sortOrder });
        sortTable(table, colIndex, sortOrder);
        updateIndicators(table, colIndex, sortOrder);
      }
    }
  }

  /**
   * Initialize all sortable tables
   */
  function initAll() {
    document.querySelectorAll('table[data-sortable]').forEach(initSortableTable);
  }

  // Run on DOMContentLoaded and after HTMX swaps
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initAll);
  } else {
    initAll();
  }

  // Re-init after HTMX content swap
  document.body.addEventListener('htmx:afterSwap', function(e) {
    const tables = e.detail.target.querySelectorAll('table[data-sortable]');
    tables.forEach(initSortableTable);
  });

  // Expose for manual init
  window.TableSort = { init: initSortableTable, initAll };
})();
