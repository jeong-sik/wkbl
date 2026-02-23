/**
 * CSV export for WKBL data tables
 *
 * Extracts visible table data (thead + tbody) and triggers a CSV download.
 * Uses BOM prefix for correct Korean text in Excel.
 */

(function() {
  'use strict';

  document.addEventListener('click', function(e) {
    var btn = e.target.closest('.csv-export-btn');
    if (!btn) return;

    var tableId = btn.dataset.tableId;
    var table = document.getElementById(tableId);
    if (!table) return;

    // Extract visible headers
    var headers = Array.from(table.querySelectorAll('thead th'))
      .filter(function(th) { return th.offsetParent !== null; })
      .map(function(th) {
        // Strip sort indicator text
        var clone = th.cloneNode(true);
        var indicator = clone.querySelector('.sort-indicator');
        if (indicator) indicator.remove();
        return clone.textContent.trim();
      });

    // Extract visible body rows
    var rows = Array.from(table.querySelectorAll('tbody tr'))
      .map(function(tr) {
        return Array.from(tr.querySelectorAll('td'))
          .filter(function(td) { return td.offsetParent !== null; })
          .map(function(td) { return td.textContent.trim(); });
      });

    // Include tfoot rows if present
    var footRows = Array.from(table.querySelectorAll('tfoot tr'))
      .map(function(tr) {
        return Array.from(tr.querySelectorAll('td'))
          .filter(function(td) { return td.offsetParent !== null; })
          .map(function(td) { return td.textContent.trim(); });
      });

    // Build CSV with proper escaping
    var allRows = [headers].concat(rows).concat(footRows);
    var csv = allRows
      .map(function(row) {
        return row.map(function(cell) {
          return '"' + cell.replace(/"/g, '""') + '"';
        }).join(',');
      })
      .join('\n');

    // Download with BOM for Korean text in Excel
    var blob = new Blob(['\ufeff' + csv], { type: 'text/csv;charset=utf-8' });
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = tableId + '.csv';
    a.click();
    URL.revokeObjectURL(url);
  });
})();
