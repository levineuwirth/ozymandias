/* Photography section — Leaflet map.
 *
 * Loaded only on /photography/map/ via the photography-map context flag
 * gating in templates/partials/head.html and templates/default.html.
 *
 * Pin source: /photography/map.json — emitted by the Hakyll
 * photographyMapDataRule, with city-precision (or per-photo override)
 * coordinate rounding applied at build time. Full-precision coords
 * never reach the client.
 *
 * Tile source: CartoDB Positron — free for all volumes; required
 * attribution is wired in below. Subdomains a-d are load-balanced.
 *
 * Marker behavior:
 *   * Click: navigate to the photo entry page.
 *   * Hover: tooltip with thumbnail + title + captured date.
 *   * Dense areas: leaflet.markercluster groups overlapping pins,
 *     expanding on click.
 *
 * The page chrome (header, toggle, attribution paragraph) renders
 * pre-JS so search engines and no-JS readers see the orientation
 * copy. Only the map viewport itself depends on Leaflet loading.
 */
(function () {
    'use strict';

    var MAP_DATA_URL  = '/photography/map.json';
    var MAP_ELEMENT   = 'photography-map';
    var TILE_URL      = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png';
    var TILE_ATTRIB   = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> '
                      + 'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>';
    var TILE_SUBDOMS  = 'abcd';
    var FALLBACK_VIEW = [20, 0];   // [lat, lon] when there are zero pins
    var FALLBACK_ZOOM = 2;

    // Override the default Leaflet marker icon paths so they resolve
    // to the vendored copy under /leaflet/images/. Leaflet's default
    // resolution uses the URL of leaflet.js, which fails for vendored
    // setups since the script lives in /js/, not /leaflet/.
    function configureMarkerIconPaths() {
        if (typeof L === 'undefined' || !L.Icon || !L.Icon.Default) return;
        L.Icon.Default.mergeOptions({
            iconRetinaUrl: '/leaflet/images/marker-icon-2x.png',
            iconUrl:       '/leaflet/images/marker-icon.png',
            shadowUrl:     '/leaflet/images/marker-shadow.png'
        });
    }

    function escapeHtml(s) {
        return String(s)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#39;');
    }

    // ------------------------------------------------------------------
    // Place-level helpers
    // ------------------------------------------------------------------

    // "Munich, Germany" -> "Munich". The country repeats across every pin in
    // a country-sized view and earns no space in a marker label.
    //
    // A rounded coordinate can hold more than one place — Bagsværd and
    // Kongens Lyngby are four kilometres apart and land on the same
    // city-precision point — so the label lists every distinct one rather
    // than naming whichever frame happened to come first and quietly
    // mislabelling the rest.
    function placeLabel(group) {
        var seen = [];
        group.pins.forEach(function (p) {
            if (!p.location) return;
            var name = p.location.split(',')[0].trim();
            if (name && seen.indexOf(name) === -1) seen.push(name);
        });
        if (seen.length) return seen.join(' · ');
        return group.pins[0].title || 'Untitled';
    }

    // A click should land somewhere that shows the place. If every frame here
    // belongs to one series, that series is the answer; a lone frame goes to
    // itself; a mixed pile has no single right destination, so it gets none
    // and stays a hover-only marker.
    function groupTarget(group) {
        if (group.pins.length === 1) return group.pins[0].url || null;
        var series = group.pins[0].series;
        if (!series) return null;
        var shared = group.pins.every(function (p) { return p.series === series; });
        return shared ? '/photography/' + series + '/' : null;
    }

    function groupTooltipHtml(group) {
        var count = group.pins.length;
        if (count === 1) return tooltipHtml(group.pins[0]);

        var strip = group.pins.slice(0, 4).map(function (p) {
            /* P01: `thumb` is the .w480 rung where the build found one
               (see buildPin in build/Photography.hs), so the strip's 46px
               squares no longer pull the 2400px delivery source. The
               intrinsic size is declared so a slow thumbnail does not
               reflow the tooltip it is inside. */
            return p.thumb
                ? '<img class="photography-map-strip-img" src="'
                  + escapeHtml(p.thumb) + '" alt="" loading="lazy"'
                  + ' decoding="async" width="46" height="46" sizes="46px">'
                : '';
        }).join('');

        var dates = group.pins
            .map(function (p) { return p.captured; })
            .filter(Boolean)
            .sort();
        var when = dates.length
            ? (dates[0] === dates[dates.length - 1]
                ? dates[0]
                : dates[0] + ' – ' + dates[dates.length - 1])
            : '';

        return '<div class="photography-map-tooltip photography-map-tooltip--group">'
             + '<div class="photography-map-strip">' + strip + '</div>'
             + '<div class="photography-map-tooltip-title">' + escapeHtml(placeLabel(group)) + '</div>'
             + '<div class="photography-map-tooltip-meta">' + count + ' photographs'
             + (when ? ' · ' + escapeHtml(when) : '') + '</div>'
             + '</div>';
    }

    function tooltipHtml(pin) {
        /* The tooltip image is 14rem wide (280px) and 8rem tall, cropped
           with object-fit; the .w480 rung the build hands us covers that
           at 2x. Width/height are the CSS box, not the file's intrinsic
           size — object-fit makes them the same claim to the layout. */
        var thumb = pin.thumb
            ? '<img class="photography-map-tooltip-img" src="' + escapeHtml(pin.thumb)
              + '" alt="" loading="lazy" decoding="async"'
              + ' width="280" height="160" sizes="280px">'
            : '';
        var date = pin.captured
            ? '<div class="photography-map-tooltip-date">' + escapeHtml(pin.captured) + '</div>'
            : '';
        return ''
            + '<div class="photography-map-tooltip">'
            + thumb
            + '<div class="photography-map-tooltip-title">' + escapeHtml(pin.title || '(untitled)') + '</div>'
            + date
            + '</div>';
    }

    function renderEmptyState(container) {
        container.classList.add('photography-map--empty');
        container.innerHTML =
              '<p class="photography-map-empty">'
            + 'No geo-tagged photographs yet. Photos with a '
            + '<code>geo:</code> frontmatter field will appear here.'
            + '</p>';
    }

    function renderErrorState(container, message) {
        container.classList.add('photography-map--error');
        container.innerHTML =
              '<p class="photography-map-error">'
            + escapeHtml(message || 'Could not load the map.')
            + '</p>';
    }

    document.addEventListener('DOMContentLoaded', function () {
        var container = document.getElementById(MAP_ELEMENT);
        if (!container) return;

        // Leaflet must be present; the conditional script load in
        // default.html should guarantee this on /photography/map/, but
        // a defensive fallback is cheap.
        if (typeof L === 'undefined') {
            renderErrorState(container, 'Map library failed to load.');
            return;
        }

        configureMarkerIconPaths();

        /* Under the normal cache rules: `force-cache` used any stored copy
           however old, so a returning visitor need not see a photograph
           geotagged since (audit J11). */
        fetch(MAP_DATA_URL)
            .then(function (r) {
                if (!r.ok) throw new Error('HTTP ' + r.status);
                return r.json();
            })
            .then(function (pins) {
                if (!Array.isArray(pins) || pins.length === 0) {
                    renderEmptyState(container);
                    return;
                }

                var map = L.map(container, {
                    scrollWheelZoom: false,        // require explicit interaction
                    zoomControl: true,
                    attributionControl: true
                }).setView(FALLBACK_VIEW, FALLBACK_ZOOM);

                L.tileLayer(TILE_URL, {
                    attribution: TILE_ATTRIB,
                    subdomains:  TILE_SUBDOMS,
                    maxZoom:     19
                }).addTo(map);

                // One marker per PLACE, not per photograph.
                //
                // geo-precision rounds coordinates before they reach this
                // file, so every frame from a city shares one position
                // exactly. Adding them as individual markers gave
                // markercluster nothing to separate: clicking a cluster
                // spiderfied 28 identical points into a starburst of legs
                // radiating from a single spot — a drawing of the clustering
                // algorithm rather than of anywhere. Aggregating first says
                // what the data actually says: two places, with counts.
                var groups = {};
                pins.forEach(function (pin) {
                    if (typeof pin.lat !== 'number' || typeof pin.lon !== 'number') return;
                    var key = pin.lat + ',' + pin.lon;
                    if (!groups[key]) {
                        groups[key] = { lat: pin.lat, lon: pin.lon, pins: [] };
                    }
                    groups[key].pins.push(pin);
                });

                var hasCluster = typeof L.markerClusterGroup === 'function';
                var layer = hasCluster ? L.markerClusterGroup() : L.featureGroup();
                var placed = 0;

                Object.keys(groups).forEach(function (key) {
                    var g     = groups[key];
                    var count = g.pins.length;

                    // A div icon, not the default PNG: it scales with the
                    // count, carries the number, and removes the map's only
                    // dependency on a bitmap that has to resolve.
                    var size = count > 1 ? 34 : 22;
                    var marker = L.marker([g.lat, g.lon], {
                        icon: L.divIcon({
                            className:  'photography-map-pin',
                            html:       count > 1 ? '<span>' + count + '</span>' : '<span></span>',
                            iconSize:   [size, size],
                            iconAnchor: [size / 2, size / 2]
                        }),
                        title: placeLabel(g) + (count > 1 ? ' — ' + count + ' photographs' : '')
                    });

                    marker.bindTooltip(groupTooltipHtml(g), {
                        direction: 'top',
                        offset:    [0, -18],
                        className: 'photography-map-tooltip-wrap',
                        opacity:   1
                    });

                    // Send a click somewhere that shows the whole place: the
                    // series when they share one, otherwise the single frame.
                    var target = groupTarget(g);
                    if (target) {
                        marker.on('click', function () { window.location.href = target; });
                    }

                    layer.addLayer(marker);
                    placed += 1;
                });

                map.addLayer(layer);

                // Frame the visible pins with a small padding. Single-
                // pin portfolios get a moderate zoom rather than the
                // hard-coded zoom level so the marker doesn't feel
                // marooned in negative space.
                if (placed === 1) {
                    var only = groups[Object.keys(groups)[0]];
                    map.setView([only.lat, only.lon], 9);
                } else {
                    var bounds = layer.getBounds();
                    if (bounds.isValid()) {
                        map.fitBounds(bounds.pad(0.15));
                    }
                }

                // Allow scroll-wheel zoom only while the map has focus —
                // prevents the page from "trapping" the scroll on someone
                // passing through. Every focus, not the first only: after
                // one blur, wheel zoom never came back (audit J11).
                map.on('focus', function () { map.scrollWheelZoom.enable(); });
                map.on('blur',  function () { map.scrollWheelZoom.disable(); });
            })
            .catch(function (err) {
                renderErrorState(container, 'Could not load map data: ' + err.message);
            });
    });

}());
