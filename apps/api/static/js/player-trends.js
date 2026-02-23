/**
 * Player trend chart overlay interactions
 */
(function() {
  'use strict';

  function getOverlay() {
    return document.querySelector('[data-trend-overlay]');
  }

  function closeOverlay() {
    var overlay = getOverlay();
    if (!overlay) return;
    overlay.classList.add('hidden');
    document.body.style.overflow = '';
  }

  function openOverlayFromCell(cell) {
    var overlay = getOverlay();
    var content = document.getElementById('trend-overlay-content');
    if (!overlay || !content || !cell) return;

    var svg = cell.querySelector('svg');
    if (!svg) return;

    var clone = svg.cloneNode(true);
    clone.setAttribute('width', '100%');
    clone.setAttribute('height', '460');
    clone.style.maxWidth = '100%';
    clone.style.minWidth = '640px';

    content.innerHTML = '';
    content.appendChild(clone);
    overlay.classList.remove('hidden');
    document.body.style.overflow = 'hidden';
  }

  function bindOnce(root) {
    if (!root || root.dataset.playerTrendsBound === '1') return;
    root.dataset.playerTrendsBound = '1';

    root.addEventListener('click', function(event) {
      var openTrigger = event.target && event.target.closest ? event.target.closest('[data-trend-expand]') : null;
      if (openTrigger) {
        event.preventDefault();
        openOverlayFromCell(openTrigger);
        return;
      }

      var closeTrigger = event.target && event.target.closest ? event.target.closest('[data-trend-close]') : null;
      if (closeTrigger) {
        event.preventDefault();
        closeOverlay();
        return;
      }

      var overlay = event.target && event.target.closest ? event.target.closest('[data-trend-overlay]') : null;
      var panel = event.target && event.target.closest ? event.target.closest('[data-trend-overlay-panel]') : null;
      if (overlay && !panel) {
        event.preventDefault();
        closeOverlay();
      }
    });
  }

  document.addEventListener('keydown', function(event) {
    if (event.key === 'Escape') closeOverlay();
  });

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() { bindOnce(document.body); });
  } else {
    bindOnce(document.body);
  }
})();
