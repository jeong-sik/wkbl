/**
 * Chart Tooltip Enhancement
 * Rich tooltips for SVG charts with follow cursor and auto-positioning
 */
(() => {
  const OFFSET = 12;
  const TOOLTIP_ID = 'chart-tooltip';

  let tooltip = null;
  let currentTarget = null;

  const createTooltip = () => {
    if (tooltip) return tooltip;

    tooltip = document.createElement('div');
    tooltip.id = TOOLTIP_ID;
    tooltip.className = [
      'fixed z-[200] pointer-events-none',
      'bg-slate-900 dark:bg-slate-100',
      'text-white dark:text-slate-900',
      'text-sm rounded-lg shadow-xl',
      'px-3 py-2 max-w-xs',
      'opacity-0 transition-opacity duration-150',
      'border border-slate-700 dark:border-slate-300'
    ].join(' ');
    tooltip.style.willChange = 'transform, opacity';
    document.body.appendChild(tooltip);
    return tooltip;
  };

  const positionTooltip = (x, y) => {
    if (!tooltip) return;

    const rect = tooltip.getBoundingClientRect();
    const vw = window.innerWidth;
    const vh = window.innerHeight;

    // Default: below and to the right
    let left = x + OFFSET;
    let top = y + OFFSET;

    // Flip horizontal if too close to right edge
    if (left + rect.width > vw - 20) {
      left = x - rect.width - OFFSET;
    }

    // Flip vertical if too close to bottom
    if (top + rect.height > vh - 20) {
      top = y - rect.height - OFFSET;
    }

    // Ensure minimum margins
    left = Math.max(10, Math.min(left, vw - rect.width - 10));
    top = Math.max(10, Math.min(top, vh - rect.height - 10));

    tooltip.style.transform = `translate(${left}px, ${top}px)`;
  };

  const showTooltip = (target, x, y) => {
    createTooltip();

    // Get tooltip content from data attributes
    const title = target.dataset.tooltipTitle || '';
    const value = target.dataset.tooltipValue || '';
    const subtitle = target.dataset.tooltipSubtitle || '';
    const extra = target.dataset.tooltipExtra || '';
    const color = target.dataset.tooltipColor || '#f97316';

    // Build tooltip HTML
    let html = '';

    if (title) {
      html += `<div class="font-semibold flex items-center gap-2">`;
      if (color) {
        html += `<span class="w-2.5 h-2.5 rounded-full" style="background:${color}"></span>`;
      }
      html += `${title}</div>`;
    }

    if (value) {
      html += `<div class="text-lg font-bold tabular-nums">${value}</div>`;
    }

    if (subtitle) {
      html += `<div class="text-xs opacity-75">${subtitle}</div>`;
    }

    if (extra) {
      html += `<div class="text-xs mt-1 pt-1 border-t border-slate-600 dark:border-slate-400 opacity-75">${extra}</div>`;
    }

    // Handle structured data from data-tooltip-data (JSON)
    const dataJson = target.dataset.tooltipData;
    if (dataJson) {
      try {
        const data = JSON.parse(dataJson);
        if (Array.isArray(data)) {
          html += '<div class="mt-1 space-y-0.5">';
          data.forEach(item => {
            html += `<div class="flex justify-between gap-4 text-xs">
              <span class="opacity-75">${item.label}</span>
              <span class="font-medium tabular-nums">${item.value}</span>
            </div>`;
          });
          html += '</div>';
        }
      } catch (e) {
        // Invalid JSON, ignore
      }
    }

    if (!html) {
      // Fallback to title attribute
      html = target.getAttribute('title') || target.textContent?.trim() || '';
      if (html) {
        // Clear native title to prevent double tooltip
        target.dataset.originalTitle = html;
        target.removeAttribute('title');
      }
    }

    if (!html) return;

    tooltip.innerHTML = html;
    tooltip.style.opacity = '1';
    positionTooltip(x, y);
    currentTarget = target;
  };

  const hideTooltip = () => {
    if (!tooltip) return;
    tooltip.style.opacity = '0';

    // Restore original title
    if (currentTarget?.dataset.originalTitle) {
      currentTarget.setAttribute('title', currentTarget.dataset.originalTitle);
      delete currentTarget.dataset.originalTitle;
    }
    currentTarget = null;
  };

  const handleMouseMove = (e) => {
    if (currentTarget) {
      positionTooltip(e.clientX, e.clientY);
    }
  };

  const init = () => {
    // Delegate event handling for chart elements
    document.addEventListener('mouseenter', (e) => {
      const target = e.target.closest('[data-tooltip], [data-tooltip-value], svg [title]');
      if (target) {
        showTooltip(target, e.clientX, e.clientY);
      }
    }, true);

    document.addEventListener('mouseleave', (e) => {
      const target = e.target.closest('[data-tooltip], [data-tooltip-value], svg [title]');
      if (target) {
        hideTooltip();
      }
    }, true);

    document.addEventListener('mousemove', handleMouseMove, { passive: true });

    // Hide on scroll
    window.addEventListener('scroll', hideTooltip, { passive: true });

    // Handle touch devices - show on touch, hide on touchend
    document.addEventListener('touchstart', (e) => {
      const target = e.target.closest('[data-tooltip], [data-tooltip-value]');
      if (target) {
        const touch = e.touches[0];
        showTooltip(target, touch.clientX, touch.clientY);
      } else {
        hideTooltip();
      }
    }, { passive: true });

    document.addEventListener('touchend', () => {
      setTimeout(hideTooltip, 1500); // Keep visible briefly on touch
    }, { passive: true });
  };

  // Export for programmatic use
  window.ChartTooltip = {
    show: showTooltip,
    hide: hideTooltip,
    position: positionTooltip
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
