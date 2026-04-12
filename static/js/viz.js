/* viz.js — Vega-Lite renderer with monochrome theming and dark-mode support.
 *
 * Finds every <script type="application/json" class="vega-spec"> embedded by
 * Filters.Viz, renders it into the parent .vega-container via Vega-Embed,
 * and re-renders whenever the site's light/dark theme changes.
 *
 * The site-level monochrome config is always applied; scripts should not
 * set colour-related Vega-Lite config keys — use encoding shape, strokeDash,
 * and opacity to distinguish series instead of hue.
 */
(function () {
  'use strict';

  // -------------------------------------------------------------------------
  // Monochrome Vega-Lite configs (matched to base.css custom properties)
  // -------------------------------------------------------------------------

  var LIGHT = {
    background: null,
    font: 'Spectral, Georgia, "Times New Roman", serif',
    mark: { color: '#1a1a1a' },
    axis: {
      gridColor:   '#cccccc',
      gridOpacity: 0.6,
      domainColor: '#1a1a1a',
      tickColor:   '#1a1a1a',
      labelColor:  '#1a1a1a',
      titleColor:  '#555555',
      labelFont:   'Spectral, Georgia, serif',
      titleFont:   '"Fira Sans", "Helvetica Neue", Arial, sans-serif',
      titleFontWeight: 'normal',
    },
    legend: {
      labelColor: '#1a1a1a',
      titleColor: '#555555',
      labelFont:  'Spectral, Georgia, serif',
      titleFont:  '"Fira Sans", "Helvetica Neue", Arial, sans-serif',
      titleFontWeight: 'normal',
    },
    title: {
      color:      '#1a1a1a',
      subtitleColor: '#555555',
      font:       '"Fira Sans", "Helvetica Neue", Arial, sans-serif',
      fontWeight: 'normal',
    },
    range: {
      category: ['#1a1a1a', '#555555', '#888888', '#aaaaaa', '#cccccc'],
      ordinal:  { scheme: 'greys' },
      ramp:     { scheme: 'greys' },
    },
    view: { stroke: null },
  };

  var DARK = {
    background: null,
    font: 'Spectral, Georgia, "Times New Roman", serif',
    mark: { color: '#d4d0c8' },
    axis: {
      gridColor:   '#333333',
      gridOpacity: 0.8,
      domainColor: '#d4d0c8',
      tickColor:   '#d4d0c8',
      labelColor:  '#d4d0c8',
      titleColor:  '#8c8881',
      labelFont:   'Spectral, Georgia, serif',
      titleFont:   '"Fira Sans", "Helvetica Neue", Arial, sans-serif',
      titleFontWeight: 'normal',
    },
    legend: {
      labelColor: '#d4d0c8',
      titleColor: '#8c8881',
      labelFont:  'Spectral, Georgia, serif',
      titleFont:  '"Fira Sans", "Helvetica Neue", Arial, sans-serif',
      titleFontWeight: 'normal',
    },
    title: {
      color:      '#d4d0c8',
      subtitleColor: '#8c8881',
      font:       '"Fira Sans", "Helvetica Neue", Arial, sans-serif',
      fontWeight: 'normal',
    },
    range: {
      category: ['#d4d0c8', '#8c8881', '#6a6660', '#444444', '#333333'],
      ordinal:  { scheme: 'greys' },
      ramp:     { scheme: 'greys' },
    },
    view: { stroke: null },
  };

  // -------------------------------------------------------------------------
  // Theme detection (matches theme.js logic)
  // -------------------------------------------------------------------------

  function isDark() {
    var t = document.documentElement.dataset.theme;
    if (t === 'dark')  return true;
    if (t === 'light') return false;
    return window.matchMedia('(prefers-color-scheme: dark)').matches;
  }

  function themeConfig() {
    return isDark() ? DARK : LIGHT;
  }

  // -------------------------------------------------------------------------
  // Rendering
  // -------------------------------------------------------------------------

  function renderOne(container) {
    var spec = container._vegaSpec;
    if (!spec) return;
    // Always apply site theme; ignore any config baked into the spec.
    var mergedSpec = Object.assign({}, spec, { config: themeConfig() });
    vegaEmbed(container, mergedSpec, { actions: false, renderer: 'svg' })
      .catch(function (err) { console.error('[viz]', err); });
  }

  function renderAll() {
    var scripts = document.querySelectorAll('script.vega-spec');
    for (var i = 0; i < scripts.length; i++) {
      var scriptEl  = scripts[i];
      var container = scriptEl.closest('.vega-container');
      if (!container) continue;
      try {
        // Store the parsed spec on the container before vegaEmbed replaces
        // the container's innerHTML (which removes the <script> element).
        container._vegaSpec = JSON.parse(scriptEl.textContent);
      } catch (e) {
        console.error('[viz] Failed to parse Vega-Lite spec:', e);
        continue;
      }
      renderOne(container);
    }
  }

  function reRenderAll() {
    var containers = document.querySelectorAll('.vega-container');
    for (var i = 0; i < containers.length; i++) {
      renderOne(containers[i]);
    }
  }

  // -------------------------------------------------------------------------
  // Initialisation and theme-change listener
  // -------------------------------------------------------------------------

  document.addEventListener('DOMContentLoaded', renderAll);

  new MutationObserver(function (mutations) {
    for (var i = 0; i < mutations.length; i++) {
      if (mutations[i].attributeName === 'data-theme') {
        reRenderAll();
        return;
      }
    }
  }).observe(document.documentElement, { attributes: true });

}());
