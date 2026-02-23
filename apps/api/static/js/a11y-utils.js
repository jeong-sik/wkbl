/**
 * Accessibility utilities for WKBL
 *
 * Features:
 * - Screen reader announcements for dynamic content
 * - Keyboard navigation helpers
 * - Focus management
 */

(function() {
  'use strict';

  let announceRegion = null;

  /**
   * Initialize accessibility utilities
   */
  function init() {
    createAnnouncementRegion();
    setupKeyboardNavigation();
  }

  /**
   * Create ARIA live region for announcements
   */
  function createAnnouncementRegion() {
    if (announceRegion) return;

    announceRegion = document.createElement('div');
    announceRegion.id = 'sr-announcements';
    announceRegion.className = 'sr-announce';
    announceRegion.setAttribute('role', 'status');
    announceRegion.setAttribute('aria-live', 'polite');
    announceRegion.setAttribute('aria-atomic', 'true');
    document.body.appendChild(announceRegion);
  }

  /**
   * Announce message to screen readers
   * @param {string} message - The message to announce
   * @param {string} priority - 'polite' (default) or 'assertive'
   */
  function announce(message, priority = 'polite') {
    if (!announceRegion) createAnnouncementRegion();

    announceRegion.setAttribute('aria-live', priority);

    // Clear then set to trigger announcement
    announceRegion.textContent = '';
    requestAnimationFrame(() => {
      announceRegion.textContent = message;
    });
  }

  /**
   * Setup keyboard navigation helpers
   */
  function setupKeyboardNavigation() {
    // Announce page transitions for HTMX
    document.body.addEventListener('htmx:afterSwap', function(e) {
      const target = e.detail.target;

      // Find page title or main heading
      const heading = target.querySelector('h1, h2, [role="heading"]');
      if (heading) {
        announce(`${heading.textContent} 페이지 로드됨`);
      }
    });

    // Announce sort changes
    document.body.addEventListener('click', function(e) {
      const th = e.target.closest('th[data-sortable]');
      if (th) {
        const colName = th.textContent.trim().replace(/[\u2191\u2193]/g, '');
        const indicator = th.querySelector('.sort-indicator');
        const direction = indicator?.dataset.sort;
        if (direction) {
          const dirText = direction === 'asc' ? '오름차순' : '내림차순';
          announce(`${colName} 기준 ${dirText}으로 정렬됨`);
        }
      }
    });

    // Announce table loading
    document.body.addEventListener('htmx:beforeRequest', function(e) {
      const target = e.detail.target;
      if (target.matches('table, tbody, [data-table-content]')) {
        announce('테이블 데이터 로딩 중...');
      }
    });
  }

  /**
   * Trap focus within an element (for modals)
   * @param {HTMLElement} container - The container to trap focus within
   * @param {KeyboardEvent} event - The keyboard event
   */
  function trapFocus(container, event) {
    const focusable = container.querySelectorAll(
      'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])'
    );

    if (focusable.length === 0) return;

    const first = focusable[0];
    const last = focusable[focusable.length - 1];

    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  }

  /**
   * Move focus to element and scroll into view
   * @param {HTMLElement|string} target - Element or selector
   */
  function moveFocus(target) {
    const element = typeof target === 'string'
      ? document.querySelector(target)
      : target;

    if (!element) return;

    // Make focusable if not already
    if (!element.hasAttribute('tabindex')) {
      element.setAttribute('tabindex', '-1');
    }

    element.focus({ preventScroll: false });
    element.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }

  // Initialize on DOM ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  // Expose for external use
  window.A11y = {
    announce,
    trapFocus,
    moveFocus
  };
})();
