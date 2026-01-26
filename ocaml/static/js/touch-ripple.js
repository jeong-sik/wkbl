/**
 * Touch Ripple Effect
 * Material Design-style ripple feedback for touch interactions
 * Only activates on touch devices to avoid mouse interference
 */
(() => {
  // Only run on touch devices
  const isTouchDevice = () =>
    'ontouchstart' in window || navigator.maxTouchPoints > 0;

  const createRipple = (event, element) => {
    const rect = element.getBoundingClientRect();
    const ripple = document.createElement("span");
    const size = Math.max(rect.width, rect.height);
    const x = event.touches ? event.touches[0].clientX - rect.left - size / 2 : event.clientX - rect.left - size / 2;
    const y = event.touches ? event.touches[0].clientY - rect.top - size / 2 : event.clientY - rect.top - size / 2;

    ripple.style.cssText = `
      position: absolute;
      width: ${size}px;
      height: ${size}px;
      left: ${x}px;
      top: ${y}px;
      background: currentColor;
      opacity: 0.15;
      border-radius: 50%;
      transform: scale(0);
      animation: ripple-expand 400ms ease-out forwards;
      pointer-events: none;
    `;

    // Ensure parent has relative positioning
    const computedStyle = getComputedStyle(element);
    if (computedStyle.position === 'static') {
      element.style.position = 'relative';
    }
    element.style.overflow = 'hidden';

    element.appendChild(ripple);

    // Remove ripple after animation
    setTimeout(() => ripple.remove(), 400);
  };

  const init = () => {
    if (!isTouchDevice()) return;

    // Add ripple keyframe to document
    const style = document.createElement('style');
    style.textContent = `
      @keyframes ripple-expand {
        to {
          transform: scale(2.5);
          opacity: 0;
        }
      }
    `;
    document.head.appendChild(style);

    // Add ripple to interactive elements with data-ripple attribute
    document.addEventListener('touchstart', (e) => {
      const target = e.target.closest('[data-ripple], button, .btn, a.card-enter');
      if (target && !target.disabled) {
        createRipple(e, target);
      }
    }, { passive: true });
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
