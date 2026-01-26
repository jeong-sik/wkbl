/**
 * Skeleton Loader for HTMX Content
 * - Automatic skeleton injection during htmx:beforeRequest
 * - Graceful fade-out when content arrives
 * - Reusable skeleton templates for common patterns
 *
 * @accessibility aria-busy, aria-live, role="status" for screen readers
 * @performance Fixed dimensions prevent layout shift (CLS optimization)
 */
(function() {
  'use strict';

  // Minimum heights for layout shift prevention (CLS)
  const MIN_HEIGHTS = {
    tableRow: 48,    // px per row
    card: 180,
    statsGrid: 100,
    listItem: 64,
    chart: 280,
    page: 400,
    playerProfile: 200,
    gameScore: 80,
    podium: 240,
    inlineStat: 24
  };

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
        cells += '<td class="px-3 py-3"><div class="skeleton-box h-4 bg-slate-200/50 dark:bg-slate-800/50 rounded" style="width: ' + width + '"></div></td>';
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
    },

    // Player profile header skeleton
    playerProfile: function() {
      return '<div class="skeleton-profile animate-pulse flex flex-col md:flex-row gap-6 p-6">' +
        '<div class="skeleton-box w-32 h-32 rounded-full mx-auto md:mx-0"></div>' +
        '<div class="flex-1 space-y-4 text-center md:text-left">' +
          '<div class="skeleton-box h-8 w-48 mx-auto md:mx-0"></div>' +
          '<div class="skeleton-box h-4 w-32 mx-auto md:mx-0"></div>' +
          '<div class="flex gap-4 justify-center md:justify-start">' +
            '<div class="skeleton-box h-6 w-20 rounded-full"></div>' +
            '<div class="skeleton-box h-6 w-24 rounded-full"></div>' +
          '</div>' +
        '</div>' +
      '</div>';
    },

    // Game score card skeleton
    gameScore: function() {
      return '<div class="skeleton-game animate-pulse bg-white dark:bg-slate-800 rounded-xl p-4 border border-slate-200 dark:border-slate-700">' +
        '<div class="flex items-center justify-between">' +
          '<div class="flex items-center gap-3">' +
            '<div class="skeleton-box w-12 h-12 rounded-lg"></div>' +
            '<div class="skeleton-box h-5 w-24"></div>' +
          '</div>' +
          '<div class="skeleton-box h-8 w-12 rounded-lg"></div>' +
          '<div class="skeleton-box h-4 w-8"></div>' +
          '<div class="skeleton-box h-8 w-12 rounded-lg"></div>' +
          '<div class="flex items-center gap-3">' +
            '<div class="skeleton-box h-5 w-24"></div>' +
            '<div class="skeleton-box w-12 h-12 rounded-lg"></div>' +
          '</div>' +
        '</div>' +
      '</div>';
    },

    // Leader podium skeleton
    podium: function() {
      return '<div class="skeleton-podium animate-pulse flex items-end justify-center gap-4 py-6">' +
        '<div class="flex flex-col items-center">' +
          '<div class="skeleton-box w-16 h-16 rounded-full mb-2"></div>' +
          '<div class="skeleton-box h-4 w-20 mb-1"></div>' +
          '<div class="skeleton-box h-6 w-12"></div>' +
          '<div class="skeleton-box w-20 h-24 rounded-t-lg mt-2"></div>' +
        '</div>' +
        '<div class="flex flex-col items-center">' +
          '<div class="skeleton-box w-20 h-20 rounded-full mb-2"></div>' +
          '<div class="skeleton-box h-5 w-24 mb-1"></div>' +
          '<div class="skeleton-box h-7 w-14"></div>' +
          '<div class="skeleton-box w-24 h-32 rounded-t-lg mt-2"></div>' +
        '</div>' +
        '<div class="flex flex-col items-center">' +
          '<div class="skeleton-box w-14 h-14 rounded-full mb-2"></div>' +
          '<div class="skeleton-box h-4 w-18 mb-1"></div>' +
          '<div class="skeleton-box h-5 w-10"></div>' +
          '<div class="skeleton-box w-18 h-20 rounded-t-lg mt-2"></div>' +
        '</div>' +
      '</div>';
    },

    // Inline stat value skeleton (for partial updates)
    inlineStat: function() {
      return '<span class="skeleton-inline animate-pulse inline-block">' +
        '<span class="skeleton-box inline-block h-5 w-12 align-middle rounded"></span>' +
      '</span>';
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

      case 'player-profile':
        return '<div class="skeleton-container">' + SKELETONS.playerProfile() + '</div>';

      case 'game-score':
        let games = '';
        for (let i = 0; i < count; i++) {
          games += SKELETONS.gameScore();
        }
        return '<div class="skeleton-container space-y-4">' + games + '</div>';

      case 'podium':
        return '<div class="skeleton-container">' + SKELETONS.podium() + '</div>';

      case 'inline':
        return SKELETONS.inlineStat();

      default:
        return '<div class="skeleton-container">' + SKELETONS.page() + '</div>';
    }
  }

  // Calculate min-height for layout shift prevention
  function getMinHeight(type, count) {
    count = count || 1;
    const base = MIN_HEIGHTS[type] || MIN_HEIGHTS.page;
    return type === 'table' || type === 'list' ? base * count : base;
  }

  // Show skeleton before HTMX request
  function showSkeleton(event) {
    const target = event.detail.target;
    if (!target || !target.hasAttribute('data-skeleton')) return;

    const type = target.getAttribute('data-skeleton') || 'page';
    const count = parseInt(target.getAttribute('data-skeleton-count') || '3', 10);

    // Store original content and dimensions
    target.setAttribute('data-original-content', target.innerHTML);
    target.setAttribute('data-original-height', target.style.minHeight || '');

    // Set minimum height to prevent layout shift
    target.style.minHeight = getMinHeight(type, count) + 'px';

    // Accessibility: indicate loading state
    target.setAttribute('aria-busy', 'true');
    target.setAttribute('aria-live', 'polite');

    // Inject skeleton with accessible label
    const skeleton = createSkeleton(target);
    target.innerHTML = '<div role="status" aria-label="콘텐츠 로딩 중">' +
      '<span class="sr-only">로딩 중...</span>' +
      skeleton +
    '</div>';
    target.classList.add('is-loading');
  }

  // Remove skeleton after HTMX content swap
  function hideSkeleton(event) {
    const target = event.detail.target;
    if (!target) return;

    target.classList.remove('is-loading');

    // Restore original min-height
    const originalHeight = target.getAttribute('data-original-height');
    if (originalHeight !== null) {
      target.style.minHeight = originalHeight;
      target.removeAttribute('data-original-height');
    }

    // Accessibility: indicate loading complete
    target.setAttribute('aria-busy', 'false');

    // Add fade-in animation to new content
    const content = target.querySelector(':scope > *:not(.skeleton-container):not([role="status"])');
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
