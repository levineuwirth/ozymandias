/* The fixture site's own script (site.yaml's scripts), on every page after
   the engine's: it counts its runs, so a test can see it ran once, and
   registers a photography layout where the registry is, as a site's
   portfolio would. */
(function () {
    'use strict';
    window.fixtureSiteScript = (window.fixtureSiteScript || 0) + 1;
    if (window.ozyPhotography) {
        window.ozyPhotography.registerMode('fixture-strip', {});
    }
}());
