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

    function tooltipHtml(pin) {
        var thumb = pin.thumb
            ? '<img class="photography-map-tooltip-img" src="' + escapeHtml(pin.thumb) + '" alt="" loading="lazy">'
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

        fetch(MAP_DATA_URL, { cache: 'force-cache' })
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

                // markercluster — groups overlapping pins; falls back
                // to plain L.featureGroup if the plugin failed to load.
                var hasCluster = typeof L.markerClusterGroup === 'function';
                var layer = hasCluster ? L.markerClusterGroup() : L.featureGroup();

                pins.forEach(function (pin) {
                    if (typeof pin.lat !== 'number' || typeof pin.lon !== 'number') return;
                    var marker = L.marker([pin.lat, pin.lon]);
                    marker.bindTooltip(tooltipHtml(pin), {
                        direction: 'top',
                        offset:    [0, -36],
                        className: 'photography-map-tooltip-wrap',
                        opacity:   1
                    });
                    if (pin.url) {
                        marker.on('click', function () {
                            window.location.href = pin.url;
                        });
                    }
                    layer.addLayer(marker);
                });

                map.addLayer(layer);

                // Frame the visible pins with a small padding. Single-
                // pin portfolios get a moderate zoom rather than the
                // hard-coded zoom level so the marker doesn't feel
                // marooned in negative space.
                if (pins.length === 1) {
                    map.setView([pins[0].lat, pins[0].lon], 8);
                } else {
                    var bounds = layer.getBounds();
                    if (bounds.isValid()) {
                        map.fitBounds(bounds.pad(0.15));
                    }
                }

                // Allow scroll-wheel zoom only after the user clicks
                // into the map — prevents the page from "trapping" the
                // scroll on someone passing through.
                map.once('focus', function () { map.scrollWheelZoom.enable(); });
                map.on('blur',    function () { map.scrollWheelZoom.disable(); });
            })
            .catch(function (err) {
                renderErrorState(container, 'Could not load map data: ' + err.message);
            });
    });

}());
