/**
 * WKBL Analytics Service Worker
 * Strategy: Cache-first for static, Network-first for API
 */

const FALLBACK_SW_VERSION = '20260220';
const SW_VERSION = (() => {
  try {
    return new URL(self.location.href).searchParams.get('v') || FALLBACK_SW_VERSION;
  } catch (_) {
    return FALLBACK_SW_VERSION;
  }
})();
const CACHE_VERSION = `wkbl-${SW_VERSION}`;
const STATIC_CACHE = `${CACHE_VERSION}-static`;
const DYNAMIC_CACHE = `${CACHE_VERSION}-dynamic`;

const VERSIONED_ASSET_PATHS = [
  '/static/css/styles.css',
  '/static/css/tailwind.css',
  '/static/js/app-init.js',
  '/static/js/htmx-1.9.10.min.js',
  '/static/js/page-transitions.js',
  '/static/js/scroll-shadow.js',
  '/static/js/table-sort.js',
  '/static/js/table-export.js',
  '/static/js/table-row-link.js',
  '/static/js/number-format.js',
  '/static/js/a11y-utils.js',
  '/static/js/skeleton-loader.js',
  '/static/js/data-freshness.js',
  '/static/js/search-modal.js',
  '/static/js/back-to-top.js',
  '/static/js/chart-tooltip.js',
  '/static/js/chart.min.js',
  '/static/js/island-loader.js',
  '/static/js/keyboard-nav.js',
  '/static/js/notifications.js',
  '/static/js/player-photo-fallback.js',
  '/static/js/pull-to-refresh.js',
  '/static/js/search-autocomplete.js',
  '/static/js/theme-toggle.js',
  '/static/js/share-utils.js',
  '/static/js/mobile-nav.js',
  '/static/js/touch-ripple.js',
  '/sw.js',
  '/static/images/favicon-32.png',
  '/static/images/favicon-512.png',
  '/static/images/apple-touch-icon.png',
  '/static/images/og-image.jpeg',
];

const isStaticRequest = (url) =>
  url.pathname === '/manifest.json' || url.pathname === '/sw.js' || url.pathname.startsWith('/static/');

const STATIC_ASSETS = [
  '/',
  '/manifest.json',
  ...VERSIONED_ASSET_PATHS.map((path) => `${path}?v=${SW_VERSION}`),
];

const withVersion = (requestUrl) => {
  if (requestUrl.pathname.startsWith('/static/') && !requestUrl.searchParams.has('v')) {
    const withV = new URL(requestUrl.href);
    withV.searchParams.set('v', SW_VERSION);
    return withV;
  }
  return requestUrl;
};

// Routes that should use network-first (data/API endpoints)
const NETWORK_FIRST_PATHS = [
  '/players/table',
  '/teams/table',
  '/standings/table',
  '/boxscores/table',
  '/games/table',
  '/leaders/table',
  '/api/',
  '/predict'
];

// Install: Cache static assets
self.addEventListener('install', (event) => {
  event.waitUntil(
    caches.open(STATIC_CACHE)
      .then((cache) => cache.addAll(STATIC_ASSETS))
      .then(() => self.skipWaiting())
      .catch((err) => console.error('[SW] Install failed:', err))
  );
});

// Activate: Clean old caches
self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches.keys()
      .then((keys) => Promise.all(
        keys
          .filter((key) => key.startsWith('wkbl-') && key !== STATIC_CACHE && key !== DYNAMIC_CACHE)
          .map((key) => caches.delete(key))
      ))
      .then(() => self.clients.claim())
  );
});

// Fetch: Strategy based on request type
self.addEventListener('fetch', (event) => {
  const { request } = event;
  const url = new URL(request.url);

  // Skip non-GET requests
  if (request.method !== 'GET') return;

  // Skip external requests
  if (url.origin !== location.origin) return;

  // Network-first for API/dynamic content
  if (NETWORK_FIRST_PATHS.some((path) => url.pathname.includes(path))) {
    event.respondWith(networkFirst(request));
    return;
  }

  // Cache-first for static assets
  if (isStaticRequest(url)) {
    event.respondWith(cacheFirst(request));
    return;
  }

  // Network-first with cache fallback for HTML pages
  event.respondWith(networkFirstWithFallback(request));
});

// Cache-first strategy
async function cacheFirst(request) {
  const requestUrl = new URL(request.url);
  const versionedRequestUrl = withVersion(requestUrl);
  const versionedRequest =
    versionedRequestUrl.toString() === request.url
      ? request
      : new Request(versionedRequestUrl.toString(), request);

  const cached = await caches.match(request);
  if (cached) return cached;
  if (versionedRequest !== request) {
    const versionedCached = await caches.match(versionedRequest);
    if (versionedCached) return versionedCached;
  }

  try {
    const response = await fetch(request);
    if (response.ok) {
      const cache = await caches.open(STATIC_CACHE);
      await cache.put(versionedRequest, response.clone());
    }
    return response;
  } catch (err) {
    console.error('[SW] Cache-first fetch failed:', err);
    return new Response('Offline', { status: 503 });
  }
}

// Network-first strategy
async function networkFirst(request) {
  try {
    const response = await fetch(request);
    if (response.ok) {
      const cache = await caches.open(DYNAMIC_CACHE);
      cache.put(request, response.clone());
    }
    return response;
  } catch (err) {
    const cached = await caches.match(request);
    if (cached) return cached;
    return new Response('Offline', { status: 503 });
  }
}

// Network-first with offline fallback for HTML
async function networkFirstWithFallback(request) {
  try {
    const response = await fetch(request);
    if (response.ok) {
      const cache = await caches.open(DYNAMIC_CACHE);
      cache.put(request, response.clone());
    }
    return response;
  } catch (err) {
    const cached = await caches.match(request);
    if (cached) return cached;

    // Fallback to cached home page
    const homeCached = await caches.match('/');
    if (homeCached) return homeCached;

    return new Response(offlineHTML(), {
      status: 503,
      headers: { 'Content-Type': 'text/html; charset=utf-8' }
    });
  }
}

// ========== Web Push Notifications ==========

// Handle incoming push messages
self.addEventListener('push', (event) => {
  let data = { title: 'WKBL 알림', body: '새로운 소식이 있습니다.' };

  if (event.data) {
    try {
      data = event.data.json();
    } catch (e) {
      data.body = event.data.text();
    }
  }

  const options = {
    body: data.body,
    icon: '/static/images/favicon-512.png',
    badge: '/static/images/favicon-32.png',
    vibrate: [100, 50, 100],
    tag: data.tag || 'wkbl-notification',
    renotify: true,
    data: {
      url: data.url || '/',
      gameId: data.gameId
    },
    actions: data.actions || []
  };

  event.waitUntil(
    self.registration.showNotification(data.title, options)
  );
});

// Handle notification click
self.addEventListener('notificationclick', (event) => {
  event.notification.close();

  const url = event.notification.data?.url || '/';

  // Handle action buttons
  if (event.action === 'view-boxscore' && event.notification.data?.gameId) {
    const boxscoreUrl = `/boxscore/${event.notification.data.gameId}`;
    event.waitUntil(clients.openWindow(boxscoreUrl));
    return;
  }

  // Default: open or focus the app
  event.waitUntil(
    clients.matchAll({ type: 'window', includeUncontrolled: true })
      .then((windowClients) => {
        // Focus existing window if available
        for (const client of windowClients) {
          if (client.url.includes(self.location.origin) && 'focus' in client) {
            client.navigate(url);
            return client.focus();
          }
        }
        // Open new window
        return clients.openWindow(url);
      })
  );
});

// Handle notification close (optional analytics)
self.addEventListener('notificationclose', (event) => {
  // analytics hook placeholder
});

// ========== Offline Support ==========

// Offline fallback HTML
function offlineHTML() {
  return `<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>오프라인 - WKBL Analytics</title>
  <style>
    body { font-family: system-ui, sans-serif; background: #0b0e14; color: #e2e8f0; display: flex; align-items: center; justify-content: center; min-height: 100vh; margin: 0; text-align: center; }
    .container { padding: 2rem; }
    h1 { font-size: 3rem; margin-bottom: 1rem; }
    p { color: #94a3b8; margin-bottom: 2rem; }
    button { background: #f97316; color: white; border: none; padding: 0.75rem 1.5rem; border-radius: 0.5rem; font-size: 1rem; cursor: pointer; }
    button:hover { background: #ea580c; }
  </style>
</head>
<body>
  <div class="container">
    <h1>🏀</h1>
    <h2>오프라인 상태입니다</h2>
    <p>인터넷 연결을 확인해 주세요.</p>
    <button id="offline-retry-btn" type="button">다시 시도</button>
    <script>
      document.getElementById('offline-retry-btn')?.addEventListener('click', function() {
        location.reload();
      });
    </script>
  </div>
</body>
</html>`;
}
