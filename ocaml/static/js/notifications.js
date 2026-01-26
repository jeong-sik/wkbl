/**
 * WKBL Notification Manager
 * Web Push notification subscription and management
 */

const WKBLNotifications = {
  // State
  isSupported: false,
  permission: 'default',
  subscription: null,

  // Default notification preferences
  defaultPrefs: {
    gameStart: true,
    gameEnd: true,
    liveScore: false,
    favoriteTeams: []
  },

  // Get preferences from localStorage
  getPrefs() {
    try {
      const stored = localStorage.getItem('wkbl-notify-prefs');
      return stored ? { ...this.defaultPrefs, ...JSON.parse(stored) } : this.defaultPrefs;
    } catch {
      return this.defaultPrefs;
    }
  },

  // Save preferences
  savePrefs(prefs) {
    localStorage.setItem('wkbl-notify-prefs', JSON.stringify(prefs));
  },

  // Set a single preference
  setPref(key, value) {
    const prefs = this.getPrefs();
    prefs[key] = value;
    this.savePrefs(prefs);
  },

  // Initialize notification system
  async init() {
    // Check browser support
    this.isSupported = 'Notification' in window && 'serviceWorker' in navigator;

    if (!this.isSupported) {
      console.log('[Notify] Notifications not supported');
      return false;
    }

    this.permission = Notification.permission;
    console.log('[Notify] Permission:', this.permission);

    // Update UI based on permission
    this.updateUI();

    // Load saved preference
    const enabled = localStorage.getItem('wkbl-notifications') === 'true';
    if (enabled && this.permission === 'granted') {
      await this.subscribe();
    }

    return true;
  },

  // Request permission and subscribe
  async enable() {
    if (!this.isSupported) {
      this.showToast('이 브라우저는 알림을 지원하지 않습니다.');
      return false;
    }

    try {
      // Request permission
      const permission = await Notification.requestPermission();
      this.permission = permission;
      console.log('[Notify] Permission result:', permission);

      if (permission === 'granted') {
        await this.subscribe();
        localStorage.setItem('wkbl-notifications', 'true');
        this.showToast('알림이 활성화되었습니다! 🏀');
        this.updateUI();
        return true;
      } else if (permission === 'denied') {
        this.showToast('알림 권한이 거부되었습니다. 브라우저 설정에서 변경할 수 있습니다.');
      }
    } catch (err) {
      console.error('[Notify] Enable failed:', err);
      this.showToast('알림 활성화 중 오류가 발생했습니다.');
    }

    return false;
  },

  // Disable notifications
  async disable() {
    try {
      await this.unsubscribe();
      localStorage.setItem('wkbl-notifications', 'false');
      this.showToast('알림이 비활성화되었습니다.');
      this.updateUI();
    } catch (err) {
      console.error('[Notify] Disable failed:', err);
    }
  },

  // Toggle notification state
  async toggle() {
    const enabled = localStorage.getItem('wkbl-notifications') === 'true';
    if (enabled) {
      await this.disable();
    } else {
      await this.enable();
    }
  },

  // Subscribe to push notifications
  async subscribe() {
    try {
      const registration = await navigator.serviceWorker.ready;

      // For demo purposes, just use local notifications
      // Real Web Push requires VAPID keys and server-side push
      this.subscription = { type: 'local' };

      // Notify server about subscription
      await fetch('/api/push/subscribe', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          type: 'local',
          userAgent: navigator.userAgent,
          timestamp: Date.now()
        })
      }).catch(() => {});  // Ignore errors for MVP

      console.log('[Notify] Subscribed');
      return true;
    } catch (err) {
      console.error('[Notify] Subscribe failed:', err);
      return false;
    }
  },

  // Unsubscribe from push notifications
  async unsubscribe() {
    try {
      this.subscription = null;

      // Notify server about unsubscription
      await fetch('/api/push/unsubscribe', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' }
      }).catch(() => {});  // Ignore errors for MVP

      console.log('[Notify] Unsubscribed');
      return true;
    } catch (err) {
      console.error('[Notify] Unsubscribe failed:', err);
      return false;
    }
  },

  // Send a local test notification
  async testNotification() {
    if (this.permission !== 'granted') {
      this.showToast('먼저 알림을 활성화해주세요.');
      return;
    }

    const registration = await navigator.serviceWorker.ready;
    registration.showNotification('WKBL 테스트 알림 🏀', {
      body: '알림이 정상적으로 작동합니다!',
      icon: '/static/images/favicon-512.png',
      badge: '/static/images/favicon-32.png',
      tag: 'test-notification',
      data: { url: '/' }
    });
  },

  // Update UI elements
  updateUI() {
    const enabled = localStorage.getItem('wkbl-notifications') === 'true';
    const granted = this.permission === 'granted';

    // Update toggle buttons
    document.querySelectorAll('[data-notify-toggle]').forEach(btn => {
      btn.classList.toggle('active', enabled && granted);
      btn.setAttribute('aria-pressed', enabled && granted);

      const icon = btn.querySelector('.notify-icon');
      if (icon) {
        icon.textContent = enabled && granted ? '🔔' : '🔕';
      }
    });

    // Update status text
    document.querySelectorAll('[data-notify-status]').forEach(el => {
      if (!this.isSupported) {
        el.textContent = '지원되지 않음';
      } else if (this.permission === 'denied') {
        el.textContent = '차단됨';
      } else if (enabled && granted) {
        el.textContent = '활성화됨';
      } else {
        el.textContent = '비활성화됨';
      }
    });
  },

  // Show toast message
  showToast(message) {
    // Use existing toast system if available, otherwise console
    if (window.showToast) {
      window.showToast(message);
    } else {
      console.log('[Notify]', message);

      // Simple toast fallback
      const toast = document.createElement('div');
      toast.className = 'fixed bottom-4 left-1/2 -translate-x-1/2 bg-slate-800 text-white px-4 py-2 rounded-lg shadow-lg text-sm z-50 animate-fade-in';
      toast.textContent = message;
      document.body.appendChild(toast);

      setTimeout(() => {
        toast.style.opacity = '0';
        toast.style.transition = 'opacity 0.3s';
        setTimeout(() => toast.remove(), 300);
      }, 3000);
    }
  },

  // === Game-specific notifications ===

  // Notify game start
  async notifyGameStart(homeTeam, awayTeam, gameUrl) {
    const prefs = this.getPrefs();
    if (!prefs.gameStart) return;
    if (this.permission !== 'granted') return;

    try {
      const registration = await navigator.serviceWorker.ready;
      registration.showNotification('🏀 경기 시작!', {
        body: `${homeTeam} vs ${awayTeam}`,
        icon: '/static/images/favicon-512.png',
        badge: '/static/images/favicon-32.png',
        tag: `game-start-${Date.now()}`,
        data: { url: gameUrl, type: 'gameStart' },
        vibrate: [200, 100, 200],
        requireInteraction: false
      });
    } catch (err) {
      console.error('[Notify] Game start notification failed:', err);
    }
  },

  // Notify game end
  async notifyGameEnd(homeTeam, homeScore, awayTeam, awayScore, gameUrl) {
    const prefs = this.getPrefs();
    if (!prefs.gameEnd) return;
    if (this.permission !== 'granted') return;

    const winner = homeScore > awayScore ? homeTeam : awayTeam;

    try {
      const registration = await navigator.serviceWorker.ready;
      registration.showNotification('🎉 경기 종료!', {
        body: `${homeTeam} ${homeScore} - ${awayScore} ${awayTeam}\n${winner} 승리!`,
        icon: '/static/images/favicon-512.png',
        badge: '/static/images/favicon-32.png',
        tag: `game-end-${Date.now()}`,
        data: { url: gameUrl, type: 'gameEnd' },
        vibrate: [200, 100, 200, 100, 200],
        requireInteraction: false
      });
    } catch (err) {
      console.error('[Notify] Game end notification failed:', err);
    }
  },

  // Notify live score update
  async notifyLiveScore(homeTeam, homeScore, awayTeam, awayScore, quarter, gameUrl) {
    const prefs = this.getPrefs();
    if (!prefs.liveScore) return;
    if (this.permission !== 'granted') return;

    try {
      const registration = await navigator.serviceWorker.ready;
      registration.showNotification(`📊 ${quarter}쿼터 스코어`, {
        body: `${homeTeam} ${homeScore} - ${awayScore} ${awayTeam}`,
        icon: '/static/images/favicon-512.png',
        badge: '/static/images/favicon-32.png',
        tag: `live-score`,  // Same tag to replace previous
        data: { url: gameUrl, type: 'liveScore' },
        silent: true,
        requireInteraction: false
      });
    } catch (err) {
      console.error('[Notify] Live score notification failed:', err);
    }
  },

  // Get settings panel HTML
  getSettingsPanel() {
    const prefs = this.getPrefs();
    const enabled = localStorage.getItem('wkbl-notifications') === 'true';
    const granted = this.permission === 'granted';

    return `
      <div class="space-y-3">
        <label class="flex items-center gap-3 cursor-pointer ${!enabled || !granted ? 'opacity-50' : ''}">
          <input type="checkbox" ${prefs.gameStart ? 'checked' : ''} ${!enabled || !granted ? 'disabled' : ''}
            onchange="WKBLNotifications.setPref('gameStart', this.checked)"
            class="w-4 h-4 rounded border-slate-300 text-orange-500 focus:ring-orange-500">
          <div>
            <div class="font-medium text-sm">경기 시작 알림</div>
            <div class="text-xs text-slate-500">경기 시작 시 알림을 받습니다</div>
          </div>
        </label>
        <label class="flex items-center gap-3 cursor-pointer ${!enabled || !granted ? 'opacity-50' : ''}">
          <input type="checkbox" ${prefs.gameEnd ? 'checked' : ''} ${!enabled || !granted ? 'disabled' : ''}
            onchange="WKBLNotifications.setPref('gameEnd', this.checked)"
            class="w-4 h-4 rounded border-slate-300 text-orange-500 focus:ring-orange-500">
          <div>
            <div class="font-medium text-sm">경기 종료 알림</div>
            <div class="text-xs text-slate-500">경기 결과를 알려드립니다</div>
          </div>
        </label>
        <label class="flex items-center gap-3 cursor-pointer ${!enabled || !granted ? 'opacity-50' : ''}">
          <input type="checkbox" ${prefs.liveScore ? 'checked' : ''} ${!enabled || !granted ? 'disabled' : ''}
            onchange="WKBLNotifications.setPref('liveScore', this.checked)"
            class="w-4 h-4 rounded border-slate-300 text-orange-500 focus:ring-orange-500">
          <div>
            <div class="font-medium text-sm">실시간 스코어</div>
            <div class="text-xs text-slate-500">쿼터별 점수 업데이트</div>
          </div>
        </label>
      </div>
    `;
  }
};

// Initialize on DOM ready
document.addEventListener('DOMContentLoaded', () => {
  WKBLNotifications.init();
});

// Expose globally
window.WKBLNotifications = WKBLNotifications;
