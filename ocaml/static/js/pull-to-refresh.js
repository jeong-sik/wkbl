/**
 * Pull to Refresh
 * Native-feeling pull-to-refresh for mobile browsers
 * Only activates when scrolled to top
 */
(() => {
  const THRESHOLD = 80;     // px to pull before triggering
  const MAX_PULL = 120;     // max pull distance
  const RESISTANCE = 2.5;   // pull resistance factor

  // Only run on touch devices
  const isTouchDevice = () =>
    'ontouchstart' in window || navigator.maxTouchPoints > 0;

  const createIndicator = () => {
    const indicator = document.createElement('div');
    indicator.id = 'ptr-indicator';
    indicator.className = [
      'fixed top-0 left-1/2 -translate-x-1/2 z-[150]',
      'flex items-center justify-center',
      'w-12 h-12 rounded-full',
      'bg-white dark:bg-slate-800',
      'shadow-lg border border-slate-200 dark:border-slate-700',
      'transition-all duration-200',
      'opacity-0 -translate-y-full'
    ].join(' ');
    indicator.innerHTML = `
      <svg class="ptr-arrow w-5 h-5 text-slate-600 dark:text-slate-400 transition-transform" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 14l-7 7m0 0l-7-7m7 7V3"></path>
      </svg>
      <svg class="ptr-spinner hidden w-5 h-5 text-orange-500 animate-spin" fill="none" viewBox="0 0 24 24">
        <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
        <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
      </svg>
    `;
    document.body.appendChild(indicator);
    return indicator;
  };

  const init = () => {
    if (!isTouchDevice()) return;

    const indicator = createIndicator();
    const arrow = indicator.querySelector('.ptr-arrow');
    const spinner = indicator.querySelector('.ptr-spinner');

    let startY = 0;
    let currentY = 0;
    let pulling = false;
    let refreshing = false;

    const isAtTop = () => window.scrollY <= 0;

    const handleTouchStart = (e) => {
      if (!isAtTop() || refreshing) return;
      startY = e.touches[0].clientY;
      pulling = true;
    };

    const handleTouchMove = (e) => {
      if (!pulling || refreshing) return;

      currentY = e.touches[0].clientY;
      const diff = currentY - startY;

      if (diff < 0) {
        pulling = false;
        return;
      }

      // Prevent default scroll when pulling
      if (diff > 10 && isAtTop()) {
        e.preventDefault();
      }

      // Calculate pull distance with resistance
      const pullDistance = Math.min(diff / RESISTANCE, MAX_PULL);

      // Update indicator position and opacity
      const progress = Math.min(pullDistance / THRESHOLD, 1);
      indicator.style.transform = `translateX(-50%) translateY(${pullDistance - 48}px)`;
      indicator.style.opacity = progress;

      // Rotate arrow based on progress
      const rotation = progress >= 1 ? 180 : progress * 180;
      arrow.style.transform = `rotate(${rotation}deg)`;

      // Add haptic-like visual feedback at threshold
      if (progress >= 1) {
        indicator.classList.add('scale-110');
      } else {
        indicator.classList.remove('scale-110');
      }
    };

    const handleTouchEnd = () => {
      if (!pulling) return;
      pulling = false;

      const diff = currentY - startY;
      const pullDistance = diff / RESISTANCE;

      if (pullDistance >= THRESHOLD && !refreshing) {
        // Trigger refresh
        triggerRefresh();
      } else {
        // Reset indicator
        resetIndicator();
      }
    };

    const triggerRefresh = () => {
      refreshing = true;

      // Show spinner
      arrow.classList.add('hidden');
      spinner.classList.remove('hidden');
      indicator.style.transform = 'translateX(-50%) translateY(8px)';
      indicator.classList.remove('scale-110');

      // Perform refresh
      setTimeout(() => {
        window.location.reload();
      }, 300);
    };

    const resetIndicator = () => {
      indicator.style.opacity = '0';
      indicator.style.transform = 'translateX(-50%) translateY(-100%)';
      indicator.classList.remove('scale-110');
      arrow.style.transform = 'rotate(0deg)';

      setTimeout(() => {
        arrow.classList.remove('hidden');
        spinner.classList.add('hidden');
      }, 200);
    };

    // Add CSS for touch-action
    const style = document.createElement('style');
    style.textContent = `
      body.ptr-pulling {
        touch-action: none;
        overflow: hidden;
      }
      #ptr-indicator {
        will-change: transform, opacity;
      }
    `;
    document.head.appendChild(style);

    // Bind events
    document.addEventListener('touchstart', handleTouchStart, { passive: true });
    document.addEventListener('touchmove', handleTouchMove, { passive: false });
    document.addEventListener('touchend', handleTouchEnd, { passive: true });
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
