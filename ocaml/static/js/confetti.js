/**
 * Confetti Celebration Effect
 * Lightweight confetti animation for game results
 * Uses CSS transforms + requestAnimationFrame for performance
 *
 * @accessibility prefers-reduced-motion respected
 * @performance Uses animationend events instead of setTimeout
 */
const ConfettiModule = (() => {
  'use strict';

  const PARTICLE_COUNT = 80;
  const COLORS = Object.freeze([
    '#f97316', // orange-500
    '#eab308', // yellow-500
    '#22c55e', // green-500
    '#3b82f6', // blue-500
    '#a855f7', // purple-500
    '#ec4899', // pink-500
    '#ef4444', // red-500
  ]);
  const SHAPES = Object.freeze(['square', 'circle', 'ribbon']);

  // Check for reduced motion preference
  const prefersReducedMotion = () =>
    window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  const random = (min, max) => Math.random() * (max - min) + min;
  const randomFrom = (arr) => arr[Math.floor(Math.random() * arr.length)];

  // Particle pool for memory efficiency
  const particlePool = [];
  const MAX_POOL_SIZE = 100;

  const getParticle = () => {
    return particlePool.pop() || document.createElement('div');
  };

  const releaseParticle = (particle) => {
    if (particlePool.length < MAX_POOL_SIZE) {
      particle.className = '';
      particle.style.cssText = '';
      particle.removeAttribute('style');
      particlePool.push(particle);
    }
  };

  const createParticle = (container, index, colors = COLORS) => {
    if (prefersReducedMotion()) return null;

    const particle = getParticle();
    const shape = randomFrom(SHAPES);
    const color = randomFrom(colors);
    const size = random(8, 14);
    const startX = random(0, 100);
    const startY = random(-20, -10);
    const rotateStart = random(0, 360);
    const rotateEnd = rotateStart + random(-720, 720);
    const duration = random(2.5, 4);
    const delay = random(0, 0.5);

    // Shape-specific styling
    let shapeStyle = '';
    if (shape === 'circle') {
      shapeStyle = 'border-radius: 50%;';
    } else if (shape === 'ribbon') {
      shapeStyle = `width: ${size / 3}px; height: ${size}px; border-radius: 2px;`;
    }

    particle.className = 'confetti-particle';
    particle.setAttribute('aria-hidden', 'true');
    particle.style.cssText = `
      position: absolute;
      width: ${size}px;
      height: ${size}px;
      background: ${color};
      left: ${startX}%;
      top: ${startY}%;
      opacity: 1;
      pointer-events: none;
      transform: rotate(${rotateStart}deg);
      animation: confetti-fall ${duration}s cubic-bezier(0.25, 0.46, 0.45, 0.94) ${delay}s forwards;
      --rotate-end: ${rotateEnd}deg;
      --drift-x: ${random(-30, 30)}vw;
      --drift-y: ${random(100, 120)}vh;
      ${shapeStyle}
    `;

    // Use animationend event instead of setTimeout for accurate cleanup
    const handleAnimationEnd = () => {
      particle.removeEventListener('animationend', handleAnimationEnd);
      if (particle.parentNode) {
        particle.parentNode.removeChild(particle);
      }
      releaseParticle(particle);
    };
    particle.addEventListener('animationend', handleAnimationEnd, { once: true });

    container.appendChild(particle);
    return particle;
  };

  const createContainer = () => {
    let container = document.getElementById('confetti-container');
    if (!container) {
      container = document.createElement('div');
      container.id = 'confetti-container';
      container.style.cssText = `
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        pointer-events: none;
        overflow: hidden;
        z-index: 9999;
      `;
      document.body.appendChild(container);
    }
    return container;
  };

  const addKeyframes = () => {
    if (document.getElementById('confetti-keyframes')) return;

    const style = document.createElement('style');
    style.id = 'confetti-keyframes';
    style.textContent = `
      @keyframes confetti-fall {
        0% {
          opacity: 1;
          transform: translateX(0) translateY(0) rotate(0deg);
        }
        100% {
          opacity: 0;
          transform: translateX(var(--drift-x)) translateY(var(--drift-y)) rotate(var(--rotate-end));
        }
      }

      @media (prefers-reduced-motion: reduce) {
        .confetti-particle {
          animation: none !important;
          opacity: 0 !important;
        }
      }
    `;
    document.head.appendChild(style);
  };

  // Main confetti burst function
  const burst = (options = {}) => {
    if (prefersReducedMotion()) {
      // Show a simple visual feedback for reduced motion users
      return;
    }

    const {
      particleCount = PARTICLE_COUNT,
      duration = 3000,
      colors = COLORS
    } = options;

    addKeyframes();
    const container = createContainer();

    // Batch particle creation using requestAnimationFrame for smoother performance
    let created = 0;
    const batchSize = 20;

    const createBatch = () => {
      const end = Math.min(created + batchSize, particleCount);
      for (let i = created; i < end; i++) {
        createParticle(container, i, colors);
      }
      created = end;

      if (created < particleCount) {
        requestAnimationFrame(createBatch);
      }
    };

    requestAnimationFrame(createBatch);

    // Cleanup container after all animations (fallback)
    setTimeout(() => {
      if (container && container.children.length === 0) {
        container.remove();
      }
    }, duration + 2000);
  };

  // Team-colored confetti (WKBL teams)
  const teamColors = Object.freeze({
    'KB스타즈': ['#002868', '#FFFFFF', '#f97316'],
    '우리은행': ['#005EB8', '#FFFFFF', '#fbbf24'],
    '삼성생명': ['#1428A0', '#FFFFFF', '#22c55e'],
    '신한은행': ['#0046FF', '#FFFFFF', '#f97316'],
    'BNK': ['#E31937', '#FFFFFF', '#fbbf24'],
    '하나원큐': ['#00A651', '#FFFFFF', '#22c55e'],
  });

  const burstTeamColors = (teamName, options = {}) => {
    // Get team colors or fall back to default
    const colors = teamColors[teamName]
      ? [...teamColors[teamName], '#f97316', '#fbbf24']
      : [...COLORS];

    burst({ ...options, colors });
  };

  // Sad effect for losses (fewer, slower particles falling down)
  const sadEffect = (options = {}) => {
    if (prefersReducedMotion()) {
      return;
    }

    addKeyframes();
    const container = createContainer();
    const count = options.particleCount || 20;

    const sadStyle = document.createElement('style');
    sadStyle.id = 'confetti-sad-keyframes';
    sadStyle.textContent = `
      @keyframes confetti-sad-fall {
        0% {
          opacity: 0.6;
          transform: translateY(0) rotate(0deg);
        }
        100% {
          opacity: 0;
          transform: translateY(100vh) rotate(180deg);
        }
      }
    `;
    if (!document.getElementById('confetti-sad-keyframes')) {
      document.head.appendChild(sadStyle);
    }

    for (let i = 0; i < count; i++) {
      const particle = getParticle();
      const size = random(6, 10);
      const startX = random(0, 100);
      const duration = random(3, 5);
      const delay = random(0, 1);

      particle.className = 'confetti-particle';
      particle.setAttribute('aria-hidden', 'true');
      particle.style.cssText = `
        position: absolute;
        width: ${size}px;
        height: ${size}px;
        background: #94a3b8;
        left: ${startX}%;
        top: -5%;
        opacity: 0.6;
        pointer-events: none;
        border-radius: 50%;
        animation: confetti-sad-fall ${duration}s ease-in ${delay}s forwards;
      `;

      // Use animationend for cleanup
      particle.addEventListener('animationend', () => {
        if (particle.parentNode) {
          particle.parentNode.removeChild(particle);
        }
        releaseParticle(particle);
      }, { once: true });

      container.appendChild(particle);
    }
  };

  // Public API
  const api = Object.freeze({
    burst,
    burstTeamColors,
    sadEffect,

    // Convenience methods
    win(teamName) {
      if (teamName) {
        burstTeamColors(teamName, { particleCount: 100 });
      } else {
        burst({ particleCount: 100 });
      }
    },

    lose() {
      sadEffect({ particleCount: 15 });
    },

    // For testing
    test() {
      burst({ particleCount: 50 });
    },

    // Check if animations are enabled
    isEnabled() {
      return !prefersReducedMotion();
    }
  });

  // Auto-trigger on game result pages
  const autoTrigger = () => {
    const resultEl = document.querySelector('[data-game-result]');
    if (!resultEl) return;

    const result = resultEl.dataset.gameResult;
    const teamName = resultEl.dataset.teamName;

    if (result === 'win') {
      // Slight delay for dramatic effect
      setTimeout(() => api.win(teamName), 500);
    } else if (result === 'lose') {
      setTimeout(() => api.lose(), 500);
    }
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', autoTrigger, { once: true });
  } else {
    autoTrigger();
  }

  return api;
})();

// Expose to global scope for backward compatibility
// Use ConfettiModule directly for module-style access
window.Confetti = ConfettiModule;
