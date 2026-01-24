/**
 * WKBL Analytics - Theme Toggle with System Preference Sync
 *
 * Features:
 * - localStorage persistence
 * - System preference detection (prefers-color-scheme)
 * - Auto-sync when system preference changes
 * - Smooth CSS transitions
 */

(function() {
  'use strict';

  const STORAGE_KEY = 'wkbl-theme';
  const DARK_CLASS = 'dark';
  const TRANSITION_CLASS = 'theme-transitioning';

  // Get stored preference or detect system preference
  function getPreferredTheme() {
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored === 'dark' || stored === 'light') {
      return stored;
    }
    return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
  }

  // Apply theme with optional transition
  function applyTheme(theme, animate = false) {
    const html = document.documentElement;

    if (animate) {
      html.classList.add(TRANSITION_CLASS);
      // Remove transition class after animation completes
      setTimeout(() => html.classList.remove(TRANSITION_CLASS), 350);
    }

    html.classList.toggle(DARK_CLASS, theme === 'dark');

    // Update theme-color meta tag for mobile browsers
    const themeColorMeta = document.querySelector('meta[name="theme-color"]');
    if (themeColorMeta) {
      themeColorMeta.content = theme === 'dark' ? '#0b0e14' : '#f97316';
    }
  }

  // Initialize theme immediately (no flash)
  applyTheme(getPreferredTheme(), false);

  // Listen for system preference changes
  const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
  mediaQuery.addEventListener('change', (e) => {
    // Only auto-switch if user hasn't manually set a preference
    const stored = localStorage.getItem(STORAGE_KEY);
    if (!stored) {
      applyTheme(e.matches ? 'dark' : 'light', true);
    }
  });

  // Global toggle function
  window.toggleTheme = function() {
    const html = document.documentElement;
    const isDark = html.classList.contains(DARK_CLASS);
    const newTheme = isDark ? 'light' : 'dark';

    localStorage.setItem(STORAGE_KEY, newTheme);
    applyTheme(newTheme, true);

    // Announce change for screen readers
    const announcement = document.createElement('div');
    announcement.setAttribute('role', 'status');
    announcement.setAttribute('aria-live', 'polite');
    announcement.className = 'sr-only';
    announcement.textContent = newTheme === 'dark' ? '다크 모드 활성화' : '라이트 모드 활성화';
    document.body.appendChild(announcement);
    setTimeout(() => announcement.remove(), 1000);
  };

  // Reset to system preference
  window.resetTheme = function() {
    localStorage.removeItem(STORAGE_KEY);
    applyTheme(window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light', true);
  };
})();
