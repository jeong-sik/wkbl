/**
 * Mobile Navigation Controller
 * - Hamburger menu toggle with slide-in overlay
 * - Bottom navigation active state
 * - Keyboard accessibility (Escape to close)
 * - Focus trap for accessibility (Tab cycles within modal)
 */
(function() {
  'use strict';

  let menuOpen = false;
  let menuOverlay = null;
  let menuPanel = null;
  let menuBtn = null;
  let lastFocusedElement = null;

  // Focusable elements selector
  const FOCUSABLE_SELECTOR = 'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])';

  function init() {
    menuOverlay = document.getElementById('mobile-menu-overlay');
    menuPanel = document.getElementById('mobile-menu-panel');
    menuBtn = document.getElementById('mobile-menu-btn');

    if (!menuOverlay || !menuPanel || !menuBtn) return;

    // Click handlers
    menuBtn.addEventListener('click', toggleMenu);
    menuOverlay.addEventListener('click', function(e) {
      if (e.target === menuOverlay) closeMenu();
    });

    // Keyboard accessibility
    document.addEventListener('keydown', function(e) {
      if (!menuOpen) return;

      if (e.key === 'Escape') {
        closeMenu();
        return;
      }

      // Focus trap: Tab cycles within modal
      if (e.key === 'Tab') {
        handleFocusTrap(e);
      }
    });

    // Close button inside menu
    const closeBtn = document.getElementById('mobile-menu-close');
    if (closeBtn) {
      closeBtn.addEventListener('click', closeMenu);
    }

    // Set active state for bottom nav
    setActiveBottomNav();
  }

  function toggleMenu() {
    if (menuOpen) {
      closeMenu();
    } else {
      openMenu();
    }
  }

  function openMenu() {
    lastFocusedElement = document.activeElement;
    menuOpen = true;
    menuOverlay.classList.remove('hidden');
    menuOverlay.classList.add('flex');

    // Animate in
    requestAnimationFrame(function() {
      menuOverlay.classList.add('opacity-100');
      menuPanel.classList.remove('translate-x-full');
    });

    // Trap focus
    document.body.style.overflow = 'hidden';
    menuBtn.setAttribute('aria-expanded', 'true');

    // Focus first menu item
    const firstLink = menuPanel.querySelector('a');
    if (firstLink) firstLink.focus();
  }

  function closeMenu() {
    menuOpen = false;
    menuOverlay.classList.remove('opacity-100');
    menuPanel.classList.add('translate-x-full');

    // Wait for animation
    setTimeout(function() {
      menuOverlay.classList.remove('flex');
      menuOverlay.classList.add('hidden');
    }, 300);

    document.body.style.overflow = '';
    menuBtn.setAttribute('aria-expanded', 'false');

    // Restore focus
    if (lastFocusedElement) lastFocusedElement.focus();
  }

  function handleFocusTrap(e) {
    const focusableElements = menuPanel.querySelectorAll(FOCUSABLE_SELECTOR);
    if (focusableElements.length === 0) return;

    const firstElement = focusableElements[0];
    const lastElement = focusableElements[focusableElements.length - 1];

    if (e.shiftKey) {
      // Shift + Tab: if on first element, wrap to last
      if (document.activeElement === firstElement) {
        e.preventDefault();
        lastElement.focus();
      }
    } else {
      // Tab: if on last element, wrap to first
      if (document.activeElement === lastElement) {
        e.preventDefault();
        firstElement.focus();
      }
    }
  }

  function setActiveBottomNav() {
    const path = window.location.pathname;
    const bottomNavLinks = document.querySelectorAll('[data-bottom-nav] a');

    bottomNavLinks.forEach(function(link) {
      const href = link.getAttribute('href');
      const isActive = path === href || (href !== '/' && path.startsWith(href));

      if (isActive) {
        link.classList.add('text-orange-500', 'dark:text-orange-400');
        link.classList.remove('text-slate-500', 'dark:text-slate-400');
      } else {
        link.classList.remove('text-orange-500', 'dark:text-orange-400');
        link.classList.add('text-slate-500', 'dark:text-slate-400');
      }
    });

    // Also set active in slide menu
    const menuLinks = document.querySelectorAll('#mobile-menu-panel a');
    menuLinks.forEach(function(link) {
      const href = link.getAttribute('href');
      const isActive = path === href || (href !== '/' && path.startsWith(href));

      if (isActive) {
        link.classList.add('bg-orange-50', 'dark:bg-orange-900/20', 'text-orange-600', 'dark:text-orange-400');
        link.classList.remove('text-slate-700', 'dark:text-slate-300');
      }
    });
  }

  // Expose for external use
  window.MobileNav = {
    open: openMenu,
    close: closeMenu,
    toggle: toggleMenu
  };

  // Initialize when DOM ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
