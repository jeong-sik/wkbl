/**
 * Number Formatting
 * Automatically formats large numbers into human-readable format (1.2K, 3.5M, etc.)
 * Uses data-format-number attribute with optional thresholds
 */
(() => {
  const formatNumber = (num, options = {}) => {
    const { threshold = 1000, decimals = 1, locale = 'ko-KR' } = options;

    if (num < threshold) {
      return num.toLocaleString(locale);
    }

    const units = [
      { value: 1e9, symbol: 'B' },
      { value: 1e6, symbol: 'M' },
      { value: 1e4, symbol: '만' },  // Korean 10,000
      { value: 1e3, symbol: 'K' },
    ];

    for (const unit of units) {
      if (num >= unit.value) {
        const formatted = (num / unit.value).toFixed(decimals);
        // Remove trailing zeros after decimal
        const clean = parseFloat(formatted).toString();
        return clean + unit.symbol;
      }
    }

    return num.toLocaleString(locale);
  };

  const processElement = (el) => {
    const rawValue = el.dataset.formatNumber;
    if (!rawValue) return;

    const num = parseFloat(rawValue.replace(/,/g, ''));
    if (isNaN(num)) return;

    const threshold = parseInt(el.dataset.formatThreshold) || 10000;
    const decimals = parseInt(el.dataset.formatDecimals) || 1;

    const formatted = formatNumber(num, { threshold, decimals });

    // Store original for tooltip
    if (!el.dataset.originalValue) {
      el.dataset.originalValue = num.toLocaleString('ko-KR');
      el.title = el.dataset.originalValue;
    }

    el.textContent = formatted;
  };

  const processAll = () => {
    document.querySelectorAll('[data-format-number]').forEach(processElement);
  };

  const init = () => {
    processAll();

    // Handle HTMX content swaps
    document.body.addEventListener('htmx:afterSwap', processAll);

    // Observe for dynamically added elements
    const observer = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        if (mutation.type === 'childList') {
          mutation.addedNodes.forEach(node => {
            if (node.nodeType === Node.ELEMENT_NODE) {
              if (node.dataset?.formatNumber) processElement(node);
              node.querySelectorAll?.('[data-format-number]').forEach(processElement);
            }
          });
        }
      }
    });

    observer.observe(document.body, { childList: true, subtree: true });
  };

  // Export for manual use
  window.formatNumber = formatNumber;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else {
    init();
  }
})();
