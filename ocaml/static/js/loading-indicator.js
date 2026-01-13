(() => {
  const bar = document.getElementById("global-loading");
  if (!bar) return;

  let pending = 0;
  let navTimer = null;

  const show = () => {
    if (navTimer) {
      clearTimeout(navTimer);
      navTimer = null;
    }
    bar.classList.add("active");
  };

  const hide = () => {
    if (navTimer) {
      clearTimeout(navTimer);
      navTimer = null;
    }
    bar.classList.remove("active");
  };

  const bump = () => {
    if (bar.classList.contains("active")) return;
    navTimer = setTimeout(show, 120);
  };

  document.addEventListener(
    "click",
    (event) => {
      const link = event.target.closest("a");
      if (!link || event.defaultPrevented) return;
      if (link.target === "_blank" || link.hasAttribute("download")) return;
      const href = link.getAttribute("href");
      if (!href || href.startsWith("#")) return;
      try {
        const url = new URL(href, window.location.href);
        if (url.origin !== window.location.origin) return;
      } catch {
        return;
      }
      bump();
    },
    true
  );

  document.body.addEventListener("htmx:beforeRequest", () => {
    pending += 1;
    show();
  });

  const done = () => {
    pending = Math.max(0, pending - 1);
    if (pending === 0) hide();
  };

  document.body.addEventListener("htmx:afterRequest", done);
  document.body.addEventListener("htmx:responseError", done);
  document.body.addEventListener("htmx:sendError", done);
  document.body.addEventListener("htmx:timeout", done);

  window.addEventListener("pageshow", () => {
    pending = 0;
    hide();
  });
})();
