/**
 * Confetti Celebration Effect
 * Lightweight confetti animation for game results
 * Uses CSS transforms for performance
 */
(() => {
  const PARTICLE_COUNT = 80;
  const COLORS = [
    '#f97316', // orange-500
    '#eab308', // yellow-500
    '#22c55e', // green-500
    '#3b82f6', // blue-500
    '#a855f7', // purple-500
    '#ec4899', // pink-500
    '#ef4444', // red-500
  ];
  const SHAPES = ['square', 'circle', 'ribbon'];

  const random = (min, max) => Math.random() * (max - min) + min;
  const randomFrom = (arr) => arr[Math.floor(Math.random() * arr.length)];

  const createParticle = (container, index) => {
    const particle = document.createElement('div');
    const shape = randomFrom(SHAPES);
    const color = randomFrom(COLORS);
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

    container.appendChild(particle);

    // Remove after animation
    setTimeout(() => particle.remove(), (duration + delay) * 1000 + 100);
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
    const {
      particleCount = PARTICLE_COUNT,
      duration = 3000
    } = options;

    addKeyframes();
    const container = createContainer();

    // Create particles
    for (let i = 0; i < particleCount; i++) {
      createParticle(container, i);
    }

    // Cleanup container after all animations
    setTimeout(() => {
      if (container.children.length === 0) {
        container.remove();
      }
    }, duration + 1000);
  };

  // Team-colored confetti
  const teamColors = {
    'KB스타즈': ['#002868', '#FFFFFF'],
    '우리은행': ['#005EB8', '#FFFFFF'],
    '삼성생명': ['#1428A0', '#FFFFFF'],
    '신한은행': ['#0046FF', '#FFFFFF'],
    'BNK': ['#E31937', '#FFFFFF'],
    '하나원큐': ['#00A651', '#FFFFFF'],
  };

  const burstTeamColors = (teamName, options = {}) => {
    const colors = teamColors[teamName] || COLORS;
    const originalColors = [...COLORS];

    // Temporarily replace colors
    COLORS.length = 0;
    colors.forEach(c => COLORS.push(c));
    // Add some complementary colors
    COLORS.push('#f97316', '#fbbf24', '#22c55e');

    burst(options);

    // Restore original colors
    setTimeout(() => {
      COLORS.length = 0;
      originalColors.forEach(c => COLORS.push(c));
    }, 100);
  };

  // Sad effect for losses (fewer, slower particles falling down)
  const sadEffect = (options = {}) => {
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
      const particle = document.createElement('div');
      const size = random(6, 10);
      const startX = random(0, 100);
      const duration = random(3, 5);
      const delay = random(0, 1);

      particle.className = 'confetti-particle';
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

      container.appendChild(particle);
      setTimeout(() => particle.remove(), (duration + delay) * 1000 + 100);
    }
  };

  // Expose API
  window.Confetti = {
    burst,
    burstTeamColors,
    sadEffect,

    // Convenience methods
    win(teamName) {
      if (teamName) {
        this.burstTeamColors(teamName, { particleCount: 100 });
      } else {
        this.burst({ particleCount: 100 });
      }
    },

    lose() {
      this.sadEffect({ particleCount: 15 });
    },

    // For testing
    test() {
      this.burst({ particleCount: 50 });
    }
  };

  // Auto-trigger on game result pages
  const autoTrigger = () => {
    const resultEl = document.querySelector('[data-game-result]');
    if (!resultEl) return;

    const result = resultEl.dataset.gameResult;
    const teamName = resultEl.dataset.teamName;

    if (result === 'win') {
      // Slight delay for dramatic effect
      setTimeout(() => Confetti.win(teamName), 500);
    } else if (result === 'lose') {
      setTimeout(() => Confetti.lose(), 500);
    }
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', autoTrigger, { once: true });
  } else {
    autoTrigger();
  }
})();
