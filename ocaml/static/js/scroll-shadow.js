/**
 * Scroll Shadow Indicators
 * Shows gradient shadows on scrollable containers to indicate more content
 *
 * Usage: Add class="scroll-shadow" to any overflow-x-auto container
 */
(() => {
  const updateShadows = (el) => {
    const { scrollLeft, scrollWidth, clientWidth } = el;
    const canScrollLeft = scrollLeft > 1;
    const canScrollRight = scrollLeft < scrollWidth - clientWidth - 1;

    el.classList.toggle("can-scroll-left", canScrollLeft);
    el.classList.toggle("can-scroll-right", canScrollRight);
  };

  const initScrollShadow = (el) => {
    // Initial state
    updateShadows(el);

    // Throttled scroll handler
    let ticking = false;
    el.addEventListener("scroll", () => {
      if (!ticking) {
        window.requestAnimationFrame(() => {
          updateShadows(el);
          ticking = false;
        });
        ticking = true;
      }
    }, { passive: true });

    // Handle resize
    const resizeObserver = new ResizeObserver(() => {
      updateShadows(el);
    });
    resizeObserver.observe(el);
  };

  const initAll = () => {
    document.querySelectorAll(".scroll-shadow").forEach(initScrollShadow);
  };

  // Init on DOM ready
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initAll, { once: true });
  } else {
    initAll();
  }

  // Re-init after HTMX swaps (new content may have scroll-shadow elements)
  document.addEventListener("htmx:afterSwap", (e) => {
    e.target.querySelectorAll(".scroll-shadow").forEach(initScrollShadow);
    // Also check if target itself is scroll-shadow
    if (e.target.classList && e.target.classList.contains("scroll-shadow")) {
      initScrollShadow(e.target);
    }
  });
})();
