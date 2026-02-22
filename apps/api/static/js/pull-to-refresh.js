/**
 * Pull to Refresh
 * Native-feeling pull-to-refresh for mobile browsers
 * Only activates when scrolled to top
 *
 * @accessibility Provides alternative refresh button for non-touch users
 * @performance Uses requestAnimationFrame for smooth animations
 */
(() => {
  'use strict';

  const THRESHOLD = 80;     // px to pull before triggering
  const MAX_PULL = 120;     // max pull distance
  const RESISTANCE = 2.5;   // pull resistance factor

  // Only run on touch devices
  const isTouchDevice = () =>
    'ontouchstart' in window || navigator.maxTouchPoints > 0;

  // Check for reduced motion
  const prefersReducedMotion = () =>
    window.matchMedia('(prefers-reduced-motion: reduce)').matches;

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

    // Accessibility attributes
    indicator.setAttribute('role', 'status');
    indicator.setAttribute('aria-live', 'polite');
    indicator.setAttribute('aria-label', '새로고침');

    indicator.innerHTML = `
      <svg class="ptr-arrow w-5 h-5 text-slate-600 dark:text-slate-400 transition-transform" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 14l-7 7m0 0l-7-7m7 7V3"></path>
      </svg>
      <svg class="ptr-spinner hidden w-5 h-5 text-orange-500 animate-spin" fill="none" viewBox="0 0 24 24" aria-hidden="true">
        <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
        <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
      </svg>
      <span class="sr-only ptr-status">아래로 당겨서 새로고침</span>
    `;
    document.body.appendChild(indicator);
    return indicator;
  };

  // Create alternative refresh button for keyboard/non-touch users
  const createRefreshButton = () => {
    const button = document.createElement('button');
    button.id = 'ptr-refresh-button';
    button.className = [
      'fixed top-2 right-2 z-[150]',
      'p-2 rounded-full',
      'bg-white dark:bg-slate-800',
      'shadow border border-slate-200 dark:border-slate-700',
      'hover:bg-slate-100 dark:hover:bg-slate-700',
      'focus:outline-none focus:ring-2 focus:ring-orange-500',
      'transition-colors',
      'sm:hidden'  // Only show on mobile-sized screens
    ].join(' ');
    button.setAttribute('aria-label', '페이지 새로고침');
    button.innerHTML = `
      <svg class="w-5 h-5 text-slate-600 dark:text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"></path>
      </svg>
    `;
    button.addEventListener('click', () => {
      window.location.reload();
    });

    // Only add if touch device (for accessibility on mobile)
    if (isTouchDevice()) {
      document.body.appendChild(button);
    }
    return button;
  };

  const init = () => {
    if (!isTouchDevice()) return;

    const indicator = createIndicator();
    const arrow = indicator.querySelector('.ptr-arrow');
    const spinner = indicator.querySelector('.ptr-spinner');
    const statusText = indicator.querySelector('.ptr-status');

    // Create alternative refresh button
    createRefreshButton();

    let startY = 0;
    let currentY = 0;
    let pulling = false;
    let refreshing = false;
    let rafId = null;

    const isAtTop = () => window.scrollY <= 0;

    // Detect if user is scrolling inside a scrollable element
    const isInsideScrollable = (target) => {
      let el = target;
      while (el && el !== document.body) {
        const style = window.getComputedStyle(el);
        const overflowY = style.overflowY;
        if ((overflowY === 'auto' || overflowY === 'scroll') && el.scrollTop > 0) {
          return true;
        }
        el = el.parentElement;
      }
      return false;
    };

    const handleTouchStart = (e) => {
      if (!isAtTop() || refreshing) return;
      if (isInsideScrollable(e.target)) return;  // Prevent conflict with scrollable elements

      startY = e.touches[0].clientY;
      pulling = true;
    };

    const updateIndicator = (pullDistance, progress) => {
      if (prefersReducedMotion()) {
        // Simplified feedback for reduced motion
        indicator.style.opacity = progress > 0.5 ? 1 : 0;
        return;
      }

      indicator.style.transform = `translateX(-50%) translateY(${pullDistance - 48}px)`;
      indicator.style.opacity = progress;

      // Rotate arrow based on progress
      const rotation = progress >= 1 ? 180 : progress * 180;
      arrow.style.transform = `rotate(${rotation}deg)`;

      // Update status for screen readers
      if (progress >= 1) {
        statusText.textContent = '놓으면 새로고침';
        indicator.classList.add('scale-110');
      } else {
        statusText.textContent = '아래로 당겨서 새로고침';
        indicator.classList.remove('scale-110');
      }
    };

    const handleTouchMove = (e) => {
      if (!pulling || refreshing) return;

      currentY = e.touches[0].clientY;
      const diff = currentY - startY;

      if (diff < 0) {
        pulling = false;
        return;
      }

      // Prevent default scroll when pulling (only at top)
      if (diff > 10 && isAtTop()) {
        e.preventDefault();
      }

      // Use requestAnimationFrame for smooth updates
      if (rafId) {
        cancelAnimationFrame(rafId);
      }

      rafId = requestAnimationFrame(() => {
        // Calculate pull distance with resistance
        const pullDistance = Math.min(diff / RESISTANCE, MAX_PULL);
        const progress = Math.min(pullDistance / THRESHOLD, 1);
        updateIndicator(pullDistance, progress);
        rafId = null;
      });
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

      // Cancel any pending animation frame
      if (rafId) {
        cancelAnimationFrame(rafId);
        rafId = null;
      }

      // Show spinner
      arrow.classList.add('hidden');
      spinner.classList.remove('hidden');
      indicator.style.transform = 'translateX(-50%) translateY(8px)';
      indicator.classList.remove('scale-110');

      // Update status for screen readers
      statusText.textContent = '새로고침 중...';

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
