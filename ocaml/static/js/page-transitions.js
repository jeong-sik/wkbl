(() => {
  const body = document.body;
  if (!body) return;

  let showTimer = null;

  const show = () => {
    if (showTimer) {
      window.clearTimeout(showTimer);
    }
    showTimer = window.setTimeout(() => {
      body.classList.add("is-loading");
    }, 120);
  };

  const hide = () => {
    if (showTimer) {
      window.clearTimeout(showTimer);
      showTimer = null;
    }
    body.classList.remove("is-loading");
  };

  const initialHide = () => {
    window.setTimeout(hide, 150);
  };

  const isSameOrigin = (href) => {
    try {
      const url = new URL(href, window.location.href);
      return url.origin === window.location.origin;
    } catch {
      return false;
    }
  };

  const shouldShowForLink = (anchor) => {
    if (!anchor) return false;
    if (anchor.hasAttribute("data-no-loader")) return false;
    if (anchor.target && anchor.target !== "_self") return false;
    if (anchor.hasAttribute("download")) return false;
    const href = anchor.getAttribute("href");
    if (!href || href.startsWith("#")) return false;
    return isSameOrigin(anchor.href);
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialHide, { once: true });
  } else {
    initialHide();
  }

  window.addEventListener("pageshow", hide, { once: true });
  window.addEventListener("beforeunload", show);

  document.addEventListener("click", (event) => {
    const anchor = event.target.closest("a");
    if (shouldShowForLink(anchor)) {
      show();
    }
  });

  document.addEventListener("htmx:beforeRequest", show);
  document.addEventListener("htmx:afterRequest", hide);
  document.addEventListener("htmx:responseError", hide);
  document.addEventListener("htmx:sendError", hide);
  document.addEventListener("htmx:timeout", hide);
})();
