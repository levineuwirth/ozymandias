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
  // Monochrome Vega-Lite configs (matched to base.css custom properties).
  // '{serif}' and '{sans}' are the site's faces, its --font-serif and
  // --font-sans (tokens.css, a site's theme.css), put in when a chart is
  // drawn (themeConfig): a chart is set in what the page is.
  // -------------------------------------------------------------------------

  var LIGHT = {
    background: null,
    font: '{serif}',
    mark: { color: '#1a1a1a' },
    axis: {
      gridColor:   '#cccccc',
      gridOpacity: 0.6,
      domainColor: '#1a1a1a',
      tickColor:   '#1a1a1a',
      labelColor:  '#1a1a1a',
      titleColor:  '#555555',
      labelFont:   '{serif}',
      titleFont:   '{sans}',
      titleFontWeight: 'normal',
    },
    legend: {
      labelColor: '#1a1a1a',
      titleColor: '#555555',
      labelFont:  '{serif}',
      titleFont:  '{sans}',
      titleFontWeight: 'normal',
    },
    title: {
      color:      '#1a1a1a',
      subtitleColor: '#555555',
      font:       '{sans}',
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
    font: '{serif}',
    mark: { color: '#d4d0c8' },
    axis: {
      gridColor:   '#333333',
      gridOpacity: 0.8,
      domainColor: '#d4d0c8',
      tickColor:   '#d4d0c8',
      labelColor:  '#d4d0c8',
      titleColor:  '#8c8881',
      labelFont:   '{serif}',
      titleFont:   '{sans}',
      titleFontWeight: 'normal',
    },
    legend: {
      labelColor: '#d4d0c8',
      titleColor: '#8c8881',
      labelFont:  '{serif}',
      titleFont:  '{sans}',
      titleFontWeight: 'normal',
    },
    title: {
      color:      '#d4d0c8',
      subtitleColor: '#8c8881',
      font:       '{sans}',
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
  // Theme detection: the color-scheme the theme in force declares
  // (tokens.css, a site's theme.css), so a site's own themes need no list
  // here; the system's preference only if it declares neither or both.
  // -------------------------------------------------------------------------

  function isDark() {
    var scheme = getComputedStyle(document.documentElement).getPropertyValue('color-scheme');
    var dark = /(^|\s)dark(\s|$)/.test(scheme), light = /(^|\s)light(\s|$)/.test(scheme);
    if (dark !== light) return dark;
    return window.matchMedia('(prefers-color-scheme: dark)').matches;
  }

  function themeConfig() {
    var css = getComputedStyle(document.documentElement);
    var faces = {
      '{serif}': css.getPropertyValue('--font-serif').trim() || 'serif',
      '{sans}':  css.getPropertyValue('--font-sans').trim() || 'sans-serif'
    };
    return JSON.parse(JSON.stringify(isDark() ? DARK : LIGHT), function (key, value) {
      return typeof value === 'string' && faces[value] ? faces[value] : value;
    });
  }

  // -------------------------------------------------------------------------
  // Rendering
  // -------------------------------------------------------------------------

  function renderOne(container) {
    var spec = container._vegaSpec;
    if (!spec) return;
    // vegaEmbed is asynchronous and mutates the container. Serialize
    // renders, coalescing theme changes while one is still in flight.
    if (container._vegaRendering) {
      container._vegaRedraw = true;
      return;
    }
    container._vegaRendering = true;
    // Always apply site theme; ignore any config baked into the spec.
    var mergedSpec = Object.assign({}, spec, { config: themeConfig() });
    // Release the previous view (its timers and listeners) before the
    // re-render replaces it.
    if (container._vegaResult) container._vegaResult.finalize();
    container._vegaResult = null;
    Promise.resolve().then(function () {
      return vegaEmbed(container, mergedSpec, { actions: false, renderer: 'svg' });
    })
      .then(function (result) { container._vegaResult = result; })
      .catch(function (err) { console.error('[viz]', err); })
      .then(function () {
        container._vegaRendering = false;
        if (container._vegaRedraw) {
          container._vegaRedraw = false;
          renderOne(container);
        }
      });
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

  // With no theme chosen, isDark() follows the system; follow its changes too.
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function () {
    if (!document.documentElement.dataset.theme) reRenderAll();
  });

}());
