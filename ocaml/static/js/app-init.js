/**
 * WKBL App Init - Core UI behaviors
 *
 * Consolidates: toast notifications, screen reader announce,
 * dark mode toggle, and shared app behaviors.
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
    toast.innerHTML = '<span class="break-words">' + message + '</span>' +
      '<button type="button" aria-label="\ud56d\ubaa9 \ub2eb\uae30" class="wkbl-toast-close ml-2 hover:opacity-75 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-white/40 rounded">\u00d7</button>';
    container.appendChild(toast);
    requestAnimationFrame(function() {
      toast.classList.remove('translate-x-full', 'opacity-0');
    });
    setTimeout(function() {
      container.addEventListener('click', function(e) {
        if (e.target && e.target.classList && e.target.classList.contains('wkbl-toast-close')) {
          e.preventDefault();
          toast.remove();
        }
      }, { once: true });
    }, 0);
    setTimeout(function() {
      toast.classList.add('translate-x-full', 'opacity-0');
      setTimeout(function() { toast.remove(); }, 300);
    }, 4000);
  };
  window.hideAllToasts = function() {
    var container = document.getElementById('toast-container');
    if (container) container.innerHTML = '';
  };

  // --- Search Modal Trigger ---
  (function() {
    document.addEventListener('click', function(event) {
      var btn = event.target && event.target.closest ? event.target.closest('[data-search-trigger]') : null;
      if (!btn) return;

      event.preventDefault();
      if (window.SearchModal && window.SearchModal.open) {
        window.SearchModal.open();
      }

      if (window.MobileNav && window.MobileNav.close) {
        window.MobileNav.close();
      }
    });
  })();

  // --- Declarative Form Behaviors ---
  (function() {
    document.addEventListener('change', function(event) {
      var autoSubmit = event.target && event.target.closest ? event.target.closest('[data-auto-submit]') : null;
      if (autoSubmit && autoSubmit.form) {
        if (typeof autoSubmit.form.requestSubmit === 'function') {
          autoSubmit.form.requestSubmit();
        } else {
          autoSubmit.form.submit();
        }
        return;
      }

      var redirectSelect = event.target && event.target.closest ? event.target.closest('[data-season-redirect]') : null;
      if (redirectSelect) {
        var prefix = redirectSelect.getAttribute('data-season-redirect') || '';
        if (prefix) window.location.href = prefix + encodeURIComponent(redirectSelect.value || '');
      }
    });

    document.addEventListener('input', function(event) {
      var clearTarget = event.target && event.target.closest ? event.target.closest('[data-clear-target]') : null;
      if (clearTarget) {
        var targetId = clearTarget.getAttribute('data-clear-target');
        var hiddenInput = targetId ? document.getElementById(targetId) : null;
        if (hiddenInput) hiddenInput.value = '';
      }

      var rangeInput = event.target && event.target.closest ? event.target.closest('[data-fantasy-slider]') : null;
      if (rangeInput) {
        var syncTargetId = rangeInput.getAttribute('data-sync-target');
        var syncTarget = syncTargetId ? document.getElementById(syncTargetId) : null;
        if (syncTarget) syncTarget.textContent = rangeInput.value;
      }
    });

    document.addEventListener('click', function(event) {
      var cardLink = event.target && event.target.closest ? event.target.closest('[data-card-link]') : null;
      if (cardLink) {
        var href = cardLink.getAttribute('data-card-link');
        if (href) {
          event.preventDefault();
          window.location.href = href;
        }
        return;
      }

      var shareBtn = event.target && event.target.closest ? event.target.closest('[data-share-compare]') : null;
      if (shareBtn && typeof window.shareCompareUrl === 'function') {
        event.preventDefault();
        window.shareCompareUrl();
        return;
      }

      var retryBtn = event.target && event.target.closest ? event.target.closest('[data-retry-action], [data-retry-url]') : null;
      if (retryBtn) {
        event.preventDefault();
        var retryUrl = retryBtn.getAttribute('data-retry-url');
        if (retryUrl) {
          window.location.href = retryUrl;
          return;
        }
        if (retryBtn.getAttribute('data-retry-action') === 'reload') {
          window.location.reload();
          return;
        }
      }

      var resetBtn = event.target && event.target.closest ? event.target.closest('[data-fantasy-reset]') : null;
      if (resetBtn) {
        event.preventDefault();
        var defaults = {
          pts: 1.0,
          reb: 1.2,
          ast: 1.5,
          stl: 2.0,
          blk: 2.0,
          tov: -1.0
        };

        Object.keys(defaults).forEach(function(id) {
          var input = document.getElementById(id);
          var output = document.getElementById(id + '-value');
          if (input) input.value = defaults[id];
          if (output) output.textContent = String(defaults[id]);
        });

        if (window.htmx && typeof window.htmx.trigger === 'function') {
          window.htmx.trigger('#fantasy-form', 'change');
        }
      }
    });

    document.addEventListener('toggle', function(event) {
      var details = event.target && event.target.closest ? event.target.closest('details[data-toggle-target]') : null;
      if (!details) return;
      var targetId = details.getAttribute('data-toggle-target');
      var target = targetId ? document.getElementById(targetId) : null;
      if (target) target.hidden = details.open;
    }, true);

    document.addEventListener('keydown', function(event) {
      var cardLink = event.target && event.target.closest ? event.target.closest('[data-card-link]') : null;
      if (!cardLink) return;
      if (event.key === 'Enter' || event.key === ' ') {
        event.preventDefault();
        var href = cardLink.getAttribute('data-card-link');
        if (href) window.location.href = href;
      }
    });
  })();

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

})();
