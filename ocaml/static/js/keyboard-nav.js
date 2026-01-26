/**
 * Keyboard Navigation Enhancement
 * Provides keyboard shortcuts and navigation hints for power users
 *
 * @accessibility Skip links, focus management, aria attributes
 */
(() => {
  'use strict';

  const shortcuts = {
    '/': { action: () => document.querySelector('[data-search-trigger]')?.click(), desc: '검색' },
    'g h': { action: () => window.location.href = '/', desc: '홈' },
    'g l': { action: () => window.location.href = '/leaders', desc: '리더' },
    'g p': { action: () => window.location.href = '/players', desc: '선수' },
    'g t': { action: () => window.location.href = '/teams', desc: '팀' },
    'g g': { action: () => window.location.href = '/games', desc: '경기' },
    '?': { action: showHelp, desc: '도움말' },
  };

  let keySequence = '';
  let keyTimer = null;
  let previousFocusEl = null;

  // Create skip link for keyboard users
  function createSkipLink() {
    if (document.getElementById('skip-to-content')) return;

    const skipLink = document.createElement('a');
    skipLink.id = 'skip-to-content';
    skipLink.href = '#main-content';
    skipLink.className = [
      'sr-only focus:not-sr-only',
      'fixed top-2 left-2 z-[300]',
      'bg-orange-500 text-white',
      'px-4 py-2 rounded-lg',
      'focus:outline-none focus:ring-2 focus:ring-white'
    ].join(' ');
    skipLink.textContent = '본문으로 건너뛰기';

    // Insert at the very beginning of body
    document.body.insertBefore(skipLink, document.body.firstChild);
  }

  function showHelp() {
    // Check if help modal already exists
    let modal = document.getElementById('keyboard-help-modal');
    if (modal) {
      const isHidden = modal.classList.contains('hidden');
      modal.classList.toggle('hidden');
      modal.setAttribute('aria-hidden', isHidden ? 'false' : 'true');

      if (isHidden) {
        // Save focus and move to modal
        previousFocusEl = document.activeElement;
        modal.querySelector('[data-close-modal]')?.focus();
      } else {
        // Restore focus
        previousFocusEl?.focus();
      }
      return;
    }

    // Save current focus
    previousFocusEl = document.activeElement;

    // Create help modal
    modal = document.createElement('div');
    modal.id = 'keyboard-help-modal';
    modal.className = 'fixed inset-0 z-[200] bg-black/50 backdrop-blur-sm flex items-center justify-center p-4';

    // Accessibility attributes
    modal.setAttribute('role', 'dialog');
    modal.setAttribute('aria-modal', 'true');
    modal.setAttribute('aria-labelledby', 'keyboard-help-title');
    modal.setAttribute('aria-hidden', 'false');

    modal.innerHTML = `
      <div class="bg-white dark:bg-slate-900 rounded-xl shadow-2xl max-w-md w-full p-6 border border-slate-200 dark:border-slate-800" role="document">
        <div class="flex items-center justify-between mb-4">
          <h2 id="keyboard-help-title" class="text-lg font-bold text-slate-900 dark:text-white">⌨️ 키보드 단축키</h2>
          <button data-close-modal aria-label="닫기" class="p-1 text-slate-500 hover:text-slate-900 dark:hover:text-white focus:outline-none focus:ring-2 focus:ring-orange-500 rounded">
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg>
          </button>
        </div>
        <div class="space-y-3 text-sm">
          <div class="text-slate-500 dark:text-slate-400 font-medium uppercase text-xs tracking-wider" role="heading" aria-level="3">네비게이션</div>
          <div class="grid grid-cols-2 gap-2" role="list">
            <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="g 다음 h">g h</kbd><span class="text-slate-600 dark:text-slate-400">홈</span></div>
            <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="g 다음 l">g l</kbd><span class="text-slate-600 dark:text-slate-400">리더</span></div>
            <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="g 다음 p">g p</kbd><span class="text-slate-600 dark:text-slate-400">선수</span></div>
            <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="g 다음 t">g t</kbd><span class="text-slate-600 dark:text-slate-400">팀</span></div>
            <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="g 다음 g">g g</kbd><span class="text-slate-600 dark:text-slate-400">경기</span></div>
          </div>
          <div class="border-t border-slate-200 dark:border-slate-700 pt-3 mt-3">
            <div class="text-slate-500 dark:text-slate-400 font-medium uppercase text-xs tracking-wider mb-2" role="heading" aria-level="3">일반</div>
            <div class="grid grid-cols-2 gap-2" role="list">
              <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="슬래시">/</kbd><span class="text-slate-600 dark:text-slate-400">검색</span></div>
              <div class="flex items-center gap-2" role="listitem"><kbd class="kbd" aria-label="물음표">?</kbd><span class="text-slate-600 dark:text-slate-400">도움말</span></div>
              <div class="flex items-center gap-2" role="listitem"><kbd class="kbd">Esc</kbd><span class="text-slate-600 dark:text-slate-400">닫기</span></div>
            </div>
          </div>
        </div>
      </div>
    `;

    // Close button handler with focus restore
    const closeBtn = modal.querySelector('[data-close-modal]');
    closeBtn.addEventListener('click', () => {
      modal.classList.add('hidden');
      modal.setAttribute('aria-hidden', 'true');
      previousFocusEl?.focus();
    });

    // Click outside to close
    modal.addEventListener('click', (e) => {
      if (e.target === modal) {
        modal.classList.add('hidden');
        modal.setAttribute('aria-hidden', 'true');
        previousFocusEl?.focus();
      }
    });

    // Focus trap
    modal.addEventListener('keydown', (e) => {
      if (e.key === 'Tab') {
        const focusable = modal.querySelectorAll('button, [href], [tabindex]:not([tabindex="-1"])');
        const first = focusable[0];
        const last = focusable[focusable.length - 1];

        if (e.shiftKey && document.activeElement === first) {
          e.preventDefault();
          last.focus();
        } else if (!e.shiftKey && document.activeElement === last) {
          e.preventDefault();
          first.focus();
        }
      }
    });

    document.body.appendChild(modal);
    closeBtn.focus();
  }

  function handleKeyDown(e) {
    // Ignore if user is typing in input/textarea
    if (['INPUT', 'TEXTAREA', 'SELECT'].includes(document.activeElement?.tagName)) {
      return;
    }

    // Escape closes modal
    if (e.key === 'Escape') {
      const modal = document.getElementById('keyboard-help-modal');
      if (modal && !modal.classList.contains('hidden')) {
        modal.classList.add('hidden');
        modal.setAttribute('aria-hidden', 'true');
        previousFocusEl?.focus();
      }
      return;
    }

    // Build key sequence
    const key = e.key.toLowerCase();

    // Clear previous sequence after delay
    if (keyTimer) clearTimeout(keyTimer);
    keyTimer = setTimeout(() => { keySequence = ''; }, 1000);

    // Handle single key shortcuts
    if (keySequence === '' && shortcuts[key]) {
      e.preventDefault();
      shortcuts[key].action();
      keySequence = '';
      return;
    }

    // Build multi-key sequence
    keySequence = keySequence ? keySequence + ' ' + key : key;

    // Check for matching shortcut
    if (shortcuts[keySequence]) {
      e.preventDefault();
      shortcuts[keySequence].action();
      keySequence = '';
    }
  }

  function init() {
    // Create skip link for accessibility
    createSkipLink();

    document.addEventListener('keydown', handleKeyDown);

    // Add CSS for kbd elements and accessibility
    const style = document.createElement('style');
    style.textContent = `
      .kbd {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 1.5rem;
        padding: 2px 6px;
        font-family: ui-monospace, monospace;
        font-size: 11px;
        font-weight: 500;
        background: #f1f5f9;
        border: 1px solid #e2e8f0;
        border-radius: 4px;
        box-shadow: 0 1px 0 #cbd5e1;
      }
      .dark .kbd {
        background: #334155;
        border-color: #475569;
        box-shadow: 0 1px 0 #1e293b;
        color: #e2e8f0;
      }
    `;
    document.head.appendChild(style);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
