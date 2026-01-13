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

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialHide, { once: true });
  } else {
    initialHide();
  }

  window.addEventListener("pageshow", hide, { once: true });

  document.addEventListener("htmx:beforeRequest", show);
  document.addEventListener("htmx:afterRequest", hide);
  document.addEventListener("htmx:responseError", hide);
  document.addEventListener("htmx:sendError", hide);
  document.addEventListener("htmx:timeout", hide);
})();
