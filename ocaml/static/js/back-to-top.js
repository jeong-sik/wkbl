/**
 * Back to Top Button
 * Shows a floating button when user scrolls down, smooth-scrolls to top on click
 */
(() => {
  const THRESHOLD = 400; // px to scroll before showing button
  const BTN_ID = "back-to-top";

  const createButton = () => {
    const btn = document.createElement("button");
    btn.id = BTN_ID;
    btn.className = [
      "fixed bottom-24 md:bottom-8 right-4 z-40",
      "w-12 h-12 rounded-full",
      "bg-slate-200 dark:bg-slate-800",
      "text-slate-600 dark:text-slate-400",
      "hover:bg-orange-500 hover:text-white",
      "dark:hover:bg-orange-500 dark:hover:text-white",
      "shadow-lg",
      "flex items-center justify-center",
      "opacity-0 pointer-events-none",
      "transition-all duration-300",
      "transform translate-y-4"
    ].join(" ");
    btn.setAttribute("aria-label", "맨 위로 스크롤");
    btn.innerHTML = `<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 10l7-7m0 0l7 7m-7-7v18"></path></svg>`;

    btn.addEventListener("click", () => {
      window.scrollTo({ top: 0, behavior: "smooth" });
    });

    document.body.appendChild(btn);
    return btn;
  };

  const init = () => {
    let btn = document.getElementById(BTN_ID);
    if (!btn) {
      btn = createButton();
    }

    let visible = false;
    let ticking = false;

    const updateVisibility = () => {
      const shouldShow = window.scrollY > THRESHOLD;
      if (shouldShow !== visible) {
        visible = shouldShow;
        if (visible) {
          btn.classList.remove("opacity-0", "pointer-events-none", "translate-y-4");
          btn.classList.add("opacity-100", "translate-y-0");
        } else {
          btn.classList.remove("opacity-100", "translate-y-0");
          btn.classList.add("opacity-0", "pointer-events-none", "translate-y-4");
        }
      }
    };

    window.addEventListener("scroll", () => {
      if (!ticking) {
        window.requestAnimationFrame(() => {
          updateVisibility();
          ticking = false;
        });
        ticking = true;
      }
    }, { passive: true });

    // Initial check
    updateVisibility();
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init, { once: true });
  } else {
    init();
  }
})();
