/**
 * Service Worker registration
 */
(function() {
  'use strict';

  if (!('serviceWorker' in navigator)) return;

  function register() {
    var html = document.documentElement;
    var version = (html && html.getAttribute('data-static-version')) || '20260220';
    var swUrl = '/sw.js?v=' + encodeURIComponent(version);

    navigator.serviceWorker.register(swUrl, { scope: '/' }).catch(function(err) {
      console.warn('[SW] registration failed:', err);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', register, { once: true });
  } else {
    register();
  }
})();
