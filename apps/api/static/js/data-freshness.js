/**
 * Data Freshness Indicator
 * Shows relative time since data was last updated
 * Automatically refreshes display every minute
 */
(() => {
  const REFRESH_INTERVAL = 60000; // 1 minute

  const formatRelativeTime = (dateStr) => {
    const date = new Date(dateStr);
    if (isNaN(date.getTime())) return null;

    const now = new Date();
    const diffMs = now - date;
    const diffSec = Math.floor(diffMs / 1000);
    const diffMin = Math.floor(diffSec / 60);
    const diffHour = Math.floor(diffMin / 60);
    const diffDay = Math.floor(diffHour / 24);

    if (diffSec < 60) return '방금 전';
    if (diffMin < 60) return `${diffMin}분 전`;
    if (diffHour < 24) return `${diffHour}시간 전`;
    if (diffDay < 7) return `${diffDay}일 전`;

    // Format as date for older data
    return date.toLocaleDateString('ko-KR', {
      month: 'short',
      day: 'numeric'
    });
  };

  const getFreshnessLevel = (dateStr) => {
    const date = new Date(dateStr);
    if (isNaN(date.getTime())) return 'unknown';

    const now = new Date();
    const diffMs = now - date;
    const diffMin = Math.floor(diffMs / 60000);

    if (diffMin < 5) return 'live';      // < 5min = live
    if (diffMin < 60) return 'fresh';    // < 1hr = fresh
    if (diffMin < 360) return 'recent';  // < 6hr = recent
    return 'stale';                      // > 6hr = stale
  };

  const levelStyles = {
    live: {
      bg: 'bg-emerald-100 dark:bg-emerald-900/30',
      text: 'text-emerald-700 dark:text-emerald-400',
      dot: 'bg-emerald-500',
      pulse: true
    },
    fresh: {
      bg: 'bg-blue-100 dark:bg-blue-900/30',
      text: 'text-blue-700 dark:text-blue-400',
      dot: 'bg-blue-500',
      pulse: false
    },
    recent: {
      bg: 'bg-amber-100 dark:bg-amber-900/30',
      text: 'text-amber-700 dark:text-amber-400',
      dot: 'bg-amber-500',
      pulse: false
    },
    stale: {
      bg: 'bg-slate-100 dark:bg-slate-800',
      text: 'text-slate-500 dark:text-slate-400',
      dot: 'bg-slate-400',
      pulse: false
    },
    unknown: {
      bg: 'bg-slate-100 dark:bg-slate-800',
      text: 'text-slate-500 dark:text-slate-400',
      dot: 'bg-slate-400',
      pulse: false
    }
  };

  const updateElement = (el) => {
    const timestamp = el.dataset.freshness;
    if (!timestamp) return;

    const relativeTime = formatRelativeTime(timestamp);
    const level = getFreshnessLevel(timestamp);
    const style = levelStyles[level];

    // Find or create the display span
    let display = el.querySelector('.freshness-display');
    if (!display) {
      display = document.createElement('span');
      display.className = 'freshness-display inline-flex items-center gap-1.5 px-2 py-0.5 rounded-full text-xs font-medium transition-colors';
      el.appendChild(display);
    }

    // Update styles
    display.className = `freshness-display inline-flex items-center gap-1.5 px-2 py-0.5 rounded-full text-xs font-medium transition-colors ${style.bg} ${style.text}`;

    // Update content
    display.innerHTML = `
      <span class="w-1.5 h-1.5 rounded-full ${style.dot} ${style.pulse ? 'animate-pulse' : ''}"></span>
      <span>${relativeTime || '업데이트 정보 없음'}</span>
    `;

    // Set tooltip with exact time
    const date = new Date(timestamp);
    if (!isNaN(date.getTime())) {
      el.title = `마지막 업데이트: ${date.toLocaleString('ko-KR')}`;
    }
  };

  const updateAll = () => {
    document.querySelectorAll('[data-freshness]').forEach(updateElement);
  };

  const init = () => {
    updateAll();

    // Refresh every minute
    setInterval(updateAll, REFRESH_INTERVAL);

    // Handle HTMX content swaps
    document.body.addEventListener('htmx:afterSwap', updateAll);
  };

  // Export for manual use
  window.updateDataFreshness = updateAll;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
