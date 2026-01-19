/**
 * Skeleton Loader for HTMX Content
 * - Automatic skeleton injection during htmx:beforeRequest
 * - Graceful fade-out when content arrives
 * - Reusable skeleton templates for common patterns
 */
(function() {
  'use strict';

  // Skeleton templates for different content types
  const SKELETONS = {
    // Table row skeleton (for stats tables)
    tableRow: function(cols) {
      cols = cols || 8;
      // Deterministic width pattern to avoid layout jitter
      var widths = ['80%', '60%', '45%', '55%', '50%', '65%', '40%', '70%'];
      let cells = '';
      for (let i = 0; i < cols; i++) {
        const width = widths[i % widths.length];
        cells += '<td class="px-3 py-3"><div class="skeleton-box h-4" style="width: ' + width + '"></div></td>';
      }
      return '<tr class="skeleton-row animate-pulse">' + cells + '</tr>';
    },

    // Card skeleton (for player cards, team cards)
    card: function() {
      return '<div class="skeleton-card animate-pulse bg-white dark:bg-slate-800 rounded-xl p-4 border border-slate-200 dark:border-slate-700">' +
        '<div class="flex items-center gap-4 mb-4">' +
          '<div class="skeleton-box w-12 h-12 rounded-full"></div>' +
          '<div class="flex-1 space-y-2">' +
            '<div class="skeleton-box h-4 w-3/4"></div>' +
            '<div class="skeleton-box h-3 w-1/2"></div>' +
          '</div>' +
        '</div>' +
        '<div class="space-y-2">' +
          '<div class="skeleton-box h-3 w-full"></div>' +
          '<div class="skeleton-box h-3 w-5/6"></div>' +
          '<div class="skeleton-box h-3 w-4/6"></div>' +
        '</div>' +
      '</div>';
    },

    // Stats grid skeleton (for stat boxes)
    statsGrid: function(count) {
      count = count || 4;
      let boxes = '';
      for (let i = 0; i < count; i++) {
        boxes += '<div class="skeleton-stat animate-pulse bg-white dark:bg-slate-800 rounded-lg p-4 border border-slate-200 dark:border-slate-700">' +
          '<div class="skeleton-box h-3 w-1/2 mb-2"></div>' +
          '<div class="skeleton-box h-8 w-3/4"></div>' +
        '</div>';
      }
      return '<div class="grid grid-cols-2 md:grid-cols-4 gap-4">' + boxes + '</div>';
    },

    // List item skeleton
    listItem: function() {
      return '<div class="skeleton-item animate-pulse flex items-center gap-3 p-3 border-b border-slate-100 dark:border-slate-700">' +
        '<div class="skeleton-box w-10 h-10 rounded-lg"></div>' +
        '<div class="flex-1 space-y-2">' +
          '<div class="skeleton-box h-4 w-2/3"></div>' +
          '<div class="skeleton-box h-3 w-1/3"></div>' +
        '</div>' +
        '<div class="skeleton-box h-6 w-16 rounded-full"></div>' +
      '</div>';
    },

    // Chart placeholder skeleton
    chart: function() {
      return '<div class="skeleton-chart animate-pulse bg-white dark:bg-slate-800 rounded-xl p-6 border border-slate-200 dark:border-slate-700">' +
        '<div class="skeleton-box h-4 w-1/4 mb-4"></div>' +
        '<div class="flex items-end gap-2 h-48">' +
          '<div class="skeleton-box flex-1 h-1/3"></div>' +
          '<div class="skeleton-box flex-1 h-2/3"></div>' +
          '<div class="skeleton-box flex-1 h-1/2"></div>' +
          '<div class="skeleton-box flex-1 h-4/5"></div>' +
          '<div class="skeleton-box flex-1 h-3/5"></div>' +
          '<div class="skeleton-box flex-1 h-full"></div>' +
          '<div class="skeleton-box flex-1 h-2/5"></div>' +
          '<div class="skeleton-box flex-1 h-3/4"></div>' +
        '</div>' +
      '</div>';
    },

    // Full page content skeleton
    page: function() {
      return '<div class="skeleton-page animate-pulse space-y-6">' +
        '<div class="skeleton-box h-8 w-1/3 mb-6"></div>' +
        '<div class="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">' +
          '<div class="skeleton-box h-24 rounded-xl"></div>' +
          '<div class="skeleton-box h-24 rounded-xl"></div>' +
          '<div class="skeleton-box h-24 rounded-xl"></div>' +
        '</div>' +
        '<div class="skeleton-box h-64 rounded-xl"></div>' +
      '</div>';
    }
  };

  // Create skeleton HTML based on target element attributes
  function createSkeleton(target) {
    const type = target.getAttribute('data-skeleton') || 'page';
    const count = parseInt(target.getAttribute('data-skeleton-count') || '3', 10);

    switch (type) {
      case 'table':
        const cols = parseInt(target.getAttribute('data-skeleton-cols') || '8', 10);
        let rows = '';
        for (let i = 0; i < count; i++) {
          rows += SKELETONS.tableRow(cols);
        }
        return '<tbody class="skeleton-container">' + rows + '</tbody>';

      case 'cards':
        let cards = '';
        for (let i = 0; i < count; i++) {
          cards += SKELETONS.card();
        }
        return '<div class="skeleton-container grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">' + cards + '</div>';

      case 'stats':
        return '<div class="skeleton-container">' + SKELETONS.statsGrid(count) + '</div>';

      case 'list':
        let items = '';
        for (let i = 0; i < count; i++) {
          items += SKELETONS.listItem();
        }
        return '<div class="skeleton-container">' + items + '</div>';

      case 'chart':
        return '<div class="skeleton-container">' + SKELETONS.chart() + '</div>';

      default:
        return '<div class="skeleton-container">' + SKELETONS.page() + '</div>';
    }
  }

  // Show skeleton before HTMX request
  function showSkeleton(event) {
    const target = event.detail.target;
    if (!target || !target.hasAttribute('data-skeleton')) return;

    // Store original content
    target.setAttribute('data-original-content', target.innerHTML);

    // Inject skeleton
    target.innerHTML = createSkeleton(target);
    target.classList.add('is-loading');
  }

  // Remove skeleton after HTMX content swap
  function hideSkeleton(event) {
    const target = event.detail.target;
    if (!target) return;

    target.classList.remove('is-loading');

    // Add fade-in animation to new content
    const content = target.querySelector(':scope > *:not(.skeleton-container)');
    if (content) {
      content.classList.add('skeleton-fade-in');
      setTimeout(function() {
        content.classList.remove('skeleton-fade-in');
      }, 300);
    }
  }

  // Bind HTMX events
  document.addEventListener('htmx:beforeRequest', showSkeleton);
  document.addEventListener('htmx:afterSwap', hideSkeleton);

  // Expose for manual use
  window.SkeletonLoader = {
    templates: SKELETONS,
    show: function(element, type, options) {
      options = options || {};
      element.setAttribute('data-skeleton', type);
      if (options.count) element.setAttribute('data-skeleton-count', options.count);
      if (options.cols) element.setAttribute('data-skeleton-cols', options.cols);
      element.innerHTML = createSkeleton(element);
      element.classList.add('is-loading');
    },
    hide: function(element) {
      element.classList.remove('is-loading');
      element.removeAttribute('data-skeleton');
      element.removeAttribute('data-skeleton-count');
      element.removeAttribute('data-skeleton-cols');
    }
  };

})();
