/**
 * Team roster tab switcher
 */
(function() {
  'use strict';

  var ACTIVE_CLASS = 'px-4 py-1.5 rounded-md text-sm font-bold transition-colors bg-white dark:bg-slate-700 text-slate-900 dark:text-slate-200 shadow-sm';
  var INACTIVE_CLASS = 'px-4 py-1.5 rounded-md text-sm font-bold transition-colors text-slate-500 dark:text-slate-400 hover:text-slate-700 dark:hover:text-slate-300';

  function applyTab(container, tabName) {
    var buttons = container.querySelectorAll('[data-roster-tab]');
    var panels = container.querySelectorAll('[data-roster-panel]');

    buttons.forEach(function(button) {
      var isActive = button.getAttribute('data-roster-tab') === tabName;
      button.className = isActive ? ACTIVE_CLASS : INACTIVE_CLASS;
      button.setAttribute('aria-selected', isActive ? 'true' : 'false');
    });

    panels.forEach(function(panel) {
      var isActive = panel.getAttribute('data-roster-panel') === tabName;
      panel.classList.toggle('hidden', !isActive);
    });
  }

  function init(container) {
    if (!container || container.dataset.rosterTabsBound === '1') return;
    container.dataset.rosterTabsBound = '1';

    var defaultTab = container.getAttribute('data-roster-default') || 'pergame';
    applyTab(container, defaultTab);

    container.addEventListener('click', function(event) {
      var button = event.target && event.target.closest ? event.target.closest('[data-roster-tab]') : null;
      if (!button || !container.contains(button)) return;
      event.preventDefault();
      applyTab(container, button.getAttribute('data-roster-tab'));
    });
  }

  function initAll(scope) {
    var root = scope || document;
    root.querySelectorAll('[data-roster-tabs]').forEach(init);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() { initAll(document); });
  } else {
    initAll(document);
  }

  document.body.addEventListener('htmx:afterSwap', function(e) {
    if (e && e.detail && e.detail.target) initAll(e.detail.target);
  });
})();
