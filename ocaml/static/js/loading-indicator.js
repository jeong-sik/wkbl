(function () {
  var root = document.documentElement;

  function setLoading(on) {
    if (on) {
      root.classList.add("htmx-loading");
    } else {
      root.classList.remove("htmx-loading");
    }
  }

  function clearBoot() {
    root.classList.remove("is-loading");
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", clearBoot, { once: true });
  } else {
    clearBoot();
  }

  document.addEventListener("htmx:beforeRequest", function () {
    setLoading(true);
  });
  document.addEventListener("htmx:afterRequest", function () {
    setLoading(false);
  });
  document.addEventListener("htmx:requestError", function () {
    setLoading(false);
  });
  document.addEventListener("htmx:responseError", function () {
    setLoading(false);
  });
})();
