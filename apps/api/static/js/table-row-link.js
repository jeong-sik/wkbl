/**
 * Make certain tables feel "clickable" by allowing row clicks to follow the
 * primary link inside the row.
 *
 * Currently used for team tables (standings/teams) via `data-row-link="team"`.
 */
(function() {
  'use strict';

  function isInteractiveTarget(target) {
    return !!target.closest('a, button, input, select, textarea, [role="button"], [role="link"]');
  }

  function followPrimaryLink(row) {
    const link = row.querySelector('a.team-link[href]');
    if (!link) return;
    window.location.href = link.href;
  }

  function initRowCursor(root) {
    const scope = root || document;
    // HTMX often swaps only a <tbody>, so the swap target may not contain the <table>.
    // If we're inside a matching table, scope to that table; otherwise search within scope.
    if (scope && typeof scope.closest === 'function') {
      const table = scope.closest('table[data-row-link="team"]');
      if (table) {
        table.querySelectorAll('tbody tr').forEach((row) => {
          row.style.cursor = 'pointer';
        });
        return;
      }
    }

    scope.querySelectorAll('table[data-row-link="team"] tbody tr').forEach((row) => {
      row.style.cursor = 'pointer';
    });
  }

  // Click anywhere on the row to follow the team link, unless the click was on an interactive element.
  document.addEventListener('click', (e) => {
    const row = e.target.closest('table[data-row-link="team"] tbody tr');
    if (!row) return;
    if (isInteractiveTarget(e.target)) return;
    followPrimaryLink(row);
  });

  // Run on load and after HTMX swaps.
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => initRowCursor(document));
  } else {
    initRowCursor(document);
  }

  document.body.addEventListener('htmx:afterSwap', (e) => {
    initRowCursor(e.detail.target);
  });
})();
