/**
 * WKBL App Init - Core UI behaviors
 *
 * Consolidates: toast notifications, screen reader announce,
 * dark mode toggle, mobile menu toggle.
 *
 * I18n strings are read from data-* attributes on the relevant
 * DOM elements (set server-side via OCaml sprintf).
 */
(function() {
  'use strict';

  // --- Toast Notification System ---
  window.showToast = function(message, type) {
    type = type || 'info';
    var container = document.getElementById('toast-container');
    if (!container) return;
    var colors = {
      success: 'bg-emerald-500',
      error: 'bg-rose-500',
      warning: 'bg-amber-500',
      info: 'bg-sky-500'
    };
    var toast = document.createElement('div');
    toast.className = colors[type] + ' text-white px-4 py-3 rounded-lg shadow-lg transform translate-x-full opacity-0 transition-transform transition-opacity duration-300 flex items-center gap-2 max-w-sm';
    toast.innerHTML = '<span class="break-words">' + message + '</span><button type="button" aria-label="\ub2eb\uae30" onclick="this.parentElement.remove()" class="ml-2 hover:opacity-75 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-white/40 rounded">\u00d7</button>';
    container.appendChild(toast);
    requestAnimationFrame(function() {
      toast.classList.remove('translate-x-full', 'opacity-0');
    });
    setTimeout(function() {
      toast.classList.add('translate-x-full', 'opacity-0');
      setTimeout(function() { toast.remove(); }, 300);
    }, 4000);
  };
  window.hideAllToasts = function() {
    var container = document.getElementById('toast-container');
    if (container) container.innerHTML = '';
  };

  // --- Screen Reader Announce ---
  window.announceToScreenReader = function(message) {
    var liveRegion = document.getElementById('aria-live');
    if (liveRegion) {
      liveRegion.textContent = message;
      setTimeout(function() { liveRegion.textContent = ''; }, 1000);
    }
  };

  // --- Dark Mode Toggle ---
  (function() {
    var toggle = document.getElementById('theme-toggle');
    var html = document.documentElement;

    // Initialize theme from localStorage or system preference
    var stored = localStorage.getItem('wkbl-theme');
    if (stored === 'dark' || (!stored && window.matchMedia('(prefers-color-scheme: dark)').matches)) {
      html.classList.add('dark');
    }

    if (toggle) {
      toggle.setAttribute('aria-pressed', html.classList.contains('dark') ? 'true' : 'false');
      toggle.addEventListener('click', function() {
        html.classList.toggle('dark');
        var isDark = html.classList.contains('dark');
        toggle.setAttribute('aria-pressed', isDark ? 'true' : 'false');
        localStorage.setItem('wkbl-theme', isDark ? 'dark' : 'light');
        // Read i18n strings from data attributes
        var srOn = toggle.getAttribute('data-sr-on') || '';
        var srOff = toggle.getAttribute('data-sr-off') || '';
        window.announceToScreenReader(isDark ? srOn : srOff);
      });
    }
  })();

  // --- Mobile Menu Toggle ---
  (function() {
    var menuToggle = document.getElementById('mobile-menu-toggle');
    var mobileMenu = document.getElementById('mobile-menu');
    if (menuToggle && mobileMenu) {
      menuToggle.addEventListener('click', function() {
        mobileMenu.classList.toggle('hidden');
        var isOpen = !mobileMenu.classList.contains('hidden');
        menuToggle.setAttribute('aria-expanded', isOpen);
        // Read i18n strings from data attributes
        var srOpen = menuToggle.getAttribute('data-sr-open') || '';
        var srClose = menuToggle.getAttribute('data-sr-close') || '';
        window.announceToScreenReader(isOpen ? srOpen : srClose);
      });
    }
  })();
})();
