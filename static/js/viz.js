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
  // The monochrome Vega-Lite config, in the theme's own ink: each '{…}' is
  // put in when a chart is drawn (themeConfig) from the theme in force
  // (tokens.css, a site's theme.css), so a chart is set in what the page
  // is. Faces: --font-serif and --font-sans. Colors: --text for marks,
  // axes and labels, --text-muted for titles, --border for the grid, and
  // the series in turn --text, --text-muted, the midpoint of --text-muted
  // and --border-muted, --border-muted and --border.
  // -------------------------------------------------------------------------

  var CONFIG = {
    background: null,
    font: '{serif}',
    mark: { color: '{text}' },
    axis: {
      gridColor:   '{border}',
      gridOpacity: '{grid-opacity}',
      domainColor: '{text}',
      tickColor:   '{text}',
      labelColor:  '{text}',
      titleColor:  '{muted}',
      labelFont:   '{serif}',
      titleFont:   '{sans}',
      titleFontWeight: 'normal',
    },
    legend: {
      labelColor: '{text}',
      titleColor: '{muted}',
      labelFont:  '{serif}',
      titleFont:  '{sans}',
      titleFontWeight: 'normal',
    },
    title: {
      color:      '{text}',
      subtitleColor: '{muted}',
      font:       '{sans}',
      fontWeight: 'normal',
    },
    range: {
      category: ['{text}', '{muted}', '{between}', '{border-muted}', '{border}'],
      ordinal:  { scheme: 'greys' },
      ramp:     { scheme: 'greys' },
    },
    view: { stroke: null },
  };

  /* What a theme without the tokens gets: the engine's light and dark
     palettes, by the scheme the theme declares. */
  var LIGHT_INK = { text: '#1a1a1a', muted: '#555555', border: '#cccccc', borderMuted: '#aaaaaa', grid: 0.6 };
  var DARK_INK  = { text: '#d4d0c8', muted: '#8c8881', border: '#333333', borderMuted: '#444444', grid: 0.8 };

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

  /* A token's color as sRGB channels and alpha, whatever syntax
     the theme wrote it in (oklch(), color-mix(), …): Vega's own color
     handling reads only a few. A pixel of it is painted and read back,
     in sRGB; a color the canvas cannot read leaves the fallback. */
  var canvas = document.createElement('canvas');
  canvas.width = canvas.height = 1;
  var ink = canvas.getContext('2d', { willReadFrequently: true });
  function hex2(v) { return (v < 16 ? '0' : '') + v.toString(16); }
  function color(css, token, fallback, probe) {
    // Resolve currentColor, light-dark(), and mixes in the root's actual
    // context before asking the detached canvas to convert color spaces.
    probe.style.backgroundColor = fallback;
    probe.style.backgroundColor = css.getPropertyValue(token).trim() || fallback;
    ink.clearRect(0, 0, 1, 1);
    ink.fillStyle = fallback;
    ink.fillStyle = getComputedStyle(probe).getPropertyValue('background-color');
    ink.fillRect(0, 0, 1, 1);
    return Array.from(ink.getImageData(0, 0, 1, 1).data);
  }

  function colorString(px) {
    return px[3] === 255 ? '#' + hex2(px[0]) + hex2(px[1]) + hex2(px[2])
      : 'rgba(' + px[0] + ', ' + px[1] + ', ' + px[2] + ', ' + (px[3] / 255).toFixed(3) + ')';
  }

  function between(a, b) {
    // Interpolate premultiplied sRGB and alpha, then unpremultiply. A
    // transparent endpoint contributes no hue; opaque colors retain the
    // original arithmetic midpoint.
    var alpha = a[3] + b[3];
    return [0, 1, 2].map(function (i) {
      return alpha ? Math.round((a[i] * a[3] + b[i] * b[3]) / alpha) : 0;
    }).concat(alpha / 2);
  }

  function themeConfig() {
    var css = getComputedStyle(document.documentElement);
    var f = isDark() ? DARK_INK : LIGHT_INK;
    var probe = document.createElement('span');
    probe.hidden = true;
    probe.style.color = 'inherit';
    probe.style.colorScheme = 'inherit';
    document.documentElement.appendChild(probe);
    var values;
    try {
      var muted = color(css, '--text-muted', f.muted, probe);
      var borderMuted = color(css, '--border-muted', f.borderMuted, probe);
      values = {
        '{serif}':        css.getPropertyValue('--font-serif').trim() || 'serif',
        '{sans}':         css.getPropertyValue('--font-sans').trim() || 'sans-serif',
        '{text}':         colorString(color(css, '--text', f.text, probe)),
        '{muted}':        colorString(muted),
        '{border}':       colorString(color(css, '--border', f.border, probe)),
        '{border-muted}': colorString(borderMuted),
        '{between}':      colorString(between(muted, borderMuted)),
        '{grid-opacity}': f.grid
      };
    } finally {
      probe.remove();
    }
    return JSON.parse(JSON.stringify(CONFIG), function (key, value) {
      return typeof value === 'string' && Object.prototype.hasOwnProperty.call(values, value)
        ? values[value] : value;
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
