/* C41 UI-only theme control. Works with localStorage disabled; no network required. */
(function () {
  'use strict';
  var storeKey = '0010-investment-research-theme';
  function savedMode() {
    try { return window.localStorage.getItem(storeKey) === 'light' ? 'light' : 'dark'; }
    catch (e) { return 'dark'; }
  }
  var mode = savedMode();
  function syncCss() {
    document.documentElement.setAttribute('data-c41-theme', mode);
    var dark = document.getElementById('c41-dark-css');
    var light = document.getElementById('c41-light-css');
    if (dark) dark.media = mode === 'dark' ? 'all' : 'not all';
    if (light) light.media = mode === 'light' ? 'all' : 'not all';
    ['dark','light'].forEach(function (name) {
      var btn = document.getElementById('c41-theme-' + name);
      if (btn) btn.setAttribute('aria-pressed', String(mode === name));
    });
  }
  function notifyShiny() {
    if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
      window.Shiny.setInputValue('c41_theme_mode', mode, {priority:'event'});
    }
  }
  function choose(next) {
    if (next !== 'dark' && next !== 'light') return;
    if (mode === next) return;
    mode = next;
    try { window.localStorage.setItem(storeKey, mode); } catch (e) {}
    syncCss();
    notifyShiny();
  }
  // CSS selection takes effect as soon as stylesheets appear, before first Shiny redraw.
  document.documentElement.setAttribute('data-c41-theme', mode);
  function init() {
    syncCss();
    ['dark','light'].forEach(function (name) {
      var btn = document.getElementById('c41-theme-' + name);
      if (btn) btn.addEventListener('click', function () { choose(name); });
    });
    // Shiny may connect before/after DOMContentLoaded; publish current value both ways.
    if (window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.$socket) notifyShiny();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',init);
  else init();
  // Shiny emits jQuery custom events; bind both for different versions.
  document.addEventListener('shiny:connected',notifyShiny);
  if (window.jQuery) window.jQuery(document).on('shiny:connected',notifyShiny);
})();
