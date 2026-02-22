/**
 * Chart Tooltip Enhancement
 * Rich tooltips for SVG charts with follow cursor and auto-positioning
 *
 * @accessibility Keyboard navigation, aria-describedby, focus management
 * @performance Optimized event delegation on specific containers
 */
(() => {
  'use strict';

  const OFFSET = 12;
  const TOOLTIP_ID = 'chart-tooltip';

  let tooltip = null;
  let currentTarget = null;
  let tooltipIdCounter = 0;

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

    // Accessibility attributes
    tooltip.setAttribute('role', 'tooltip');
    tooltip.setAttribute('aria-hidden', 'true');

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
    tooltip.setAttribute('aria-hidden', 'false');

    // Link target to tooltip for screen readers
    const tooltipInstanceId = `${TOOLTIP_ID}-${++tooltipIdCounter}`;
    tooltip.id = tooltipInstanceId;
    target.setAttribute('aria-describedby', tooltipInstanceId);

    positionTooltip(x, y);
    currentTarget = target;
  };

  const hideTooltip = () => {
    if (!tooltip) return;
    tooltip.style.opacity = '0';
    tooltip.setAttribute('aria-hidden', 'true');

    // Remove aria link and restore original title
    if (currentTarget) {
      currentTarget.removeAttribute('aria-describedby');

      if (currentTarget.dataset.originalTitle) {
        currentTarget.setAttribute('title', currentTarget.dataset.originalTitle);
        delete currentTarget.dataset.originalTitle;
      }
    }
    currentTarget = null;
  };

  const handleMouseMove = (e) => {
    if (currentTarget) {
      positionTooltip(e.clientX, e.clientY);
    }
  };

  const TOOLTIP_SELECTOR = '[data-tooltip], [data-tooltip-value], svg [title]';

  const init = () => {
    // Find chart containers for scoped event handling (performance optimization)
    const setupContainerEvents = (container) => {
      container.addEventListener('mouseenter', (e) => {
        if (!e.target || typeof e.target.closest !== 'function') return;
        const target = e.target.closest(TOOLTIP_SELECTOR);
        if (target) {
          showTooltip(target, e.clientX, e.clientY);
        }
      }, true);

      container.addEventListener('mouseleave', (e) => {
        if (!e.target || typeof e.target.closest !== 'function') return;
        const target = e.target.closest(TOOLTIP_SELECTOR);
        if (target) {
          hideTooltip();
        }
      }, true);

      // Keyboard accessibility: show on focus, hide on blur
      container.addEventListener('focusin', (e) => {
        if (!e.target || typeof e.target.closest !== 'function') return;
        const target = e.target.closest(TOOLTIP_SELECTOR);
        if (target) {
          const rect = target.getBoundingClientRect();
          showTooltip(target, rect.left + rect.width / 2, rect.bottom);
        }
      });

      container.addEventListener('focusout', (e) => {
        if (!e.target || typeof e.target.closest !== 'function') return;
        const target = e.target.closest(TOOLTIP_SELECTOR);
        if (target) {
          hideTooltip();
        }
      });
    };

    // Setup on existing chart containers
    const chartContainers = document.querySelectorAll('.chart-container, [data-chart], svg, .stat-card, [data-tooltip-container]');
    chartContainers.forEach(setupContainerEvents);

    // Fallback: document-level for elements outside containers
    document.addEventListener('mouseenter', (e) => {
      if (!e.target || typeof e.target.closest !== 'function') return;
      // Skip if already handled by container
      if (e.target.closest('.chart-container, [data-chart], svg, .stat-card, [data-tooltip-container]')) return;

      const target = e.target.closest(TOOLTIP_SELECTOR);
      if (target) {
        showTooltip(target, e.clientX, e.clientY);
      }
    }, true);

    document.addEventListener('mouseleave', (e) => {
      if (!e.target || typeof e.target.closest !== 'function') return;
      if (e.target.closest('.chart-container, [data-chart], svg, .stat-card, [data-tooltip-container]')) return;

      const target = e.target.closest(TOOLTIP_SELECTOR);
      if (target) {
        hideTooltip();
      }
    }, true);

    // Keyboard accessibility at document level
    document.addEventListener('focusin', (e) => {
      if (!e.target || typeof e.target.closest !== 'function') return;
      const target = e.target.closest(TOOLTIP_SELECTOR);
      if (target) {
        const rect = target.getBoundingClientRect();
        showTooltip(target, rect.left + rect.width / 2, rect.bottom);
      }
    });

    document.addEventListener('focusout', (e) => {
      if (!e.target || typeof e.target.closest !== 'function') return;
      const target = e.target.closest(TOOLTIP_SELECTOR);
      if (target) {
        hideTooltip();
      }
    });

    // Escape key to dismiss tooltip
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && currentTarget) {
        hideTooltip();
      }
    });

    document.addEventListener('mousemove', handleMouseMove, { passive: true });

    // Hide on scroll
    window.addEventListener('scroll', hideTooltip, { passive: true });

    // Handle touch devices - show on touch, hide on touchend
    document.addEventListener('touchstart', (e) => {
      if (!e.target || typeof e.target.closest !== 'function') return;
      const target = e.target.closest(TOOLTIP_SELECTOR);
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

    // MutationObserver for dynamically added containers
    const observer = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        mutation.addedNodes.forEach((node) => {
          if (node.nodeType === 1) {
            if (node.matches?.('.chart-container, [data-chart], [data-tooltip-container]')) {
              setupContainerEvents(node);
            }
            node.querySelectorAll?.('.chart-container, [data-chart], [data-tooltip-container]')
              .forEach(setupContainerEvents);
          }
        });
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });
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
