/**
 * Share utilities for WKBL
 * - URL copying with fallback for older browsers
 * - Toast notification for feedback
 */

/**
 * Share the current compare URL by copying to clipboard
 */
function shareCompareUrl() {
  const url = window.location.href;
  const btn = document.getElementById('share-compare-btn');

  // Try modern clipboard API first
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(url)
      .then(() => showShareFeedback(btn, true))
      .catch(() => fallbackCopyToClipboard(url, btn));
  } else {
    fallbackCopyToClipboard(url, btn);
  }
}

/**
 * Fallback for browsers without Clipboard API
 */
function fallbackCopyToClipboard(text, btn) {
  const textArea = document.createElement('textarea');
  textArea.value = text;
  textArea.style.position = 'fixed';
  textArea.style.left = '-9999px';
  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();

  try {
    const successful = document.execCommand('copy');
    showShareFeedback(btn, successful);
  } catch (err) {
    showShareFeedback(btn, false);
  }

  document.body.removeChild(textArea);
}

/**
 * Show visual feedback after copy attempt
 */
function showShareFeedback(btn, success) {
  if (!btn) return;

  const originalHtml = btn.innerHTML;
  const feedbackHtml = success
    ? '<svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path></svg><span>복사됨</span>'
    : '<svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path></svg><span>복사 실패</span>';

  btn.innerHTML = feedbackHtml;
  btn.classList.add(success ? 'text-green-600' : 'text-red-500');
  btn.classList.remove('text-slate-600', 'dark:text-slate-400');

  setTimeout(() => {
    btn.innerHTML = originalHtml;
    btn.classList.remove('text-green-600', 'text-red-500');
    btn.classList.add('text-slate-600', 'dark:text-slate-400');
  }, 2000);
}
