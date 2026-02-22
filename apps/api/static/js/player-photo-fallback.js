(function () {
  function onImageError(event) {
    var target = event.target;
    if (!(target instanceof HTMLImageElement)) return;
    var placeholder = target.dataset.placeholder;
    if (!placeholder) return;
    if (target.dataset.placeholderApplied) return;
    target.dataset.placeholderApplied = "1";
    target.src = placeholder;
  }

  document.addEventListener("error", onImageError, true);
})();
