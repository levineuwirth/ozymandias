/* popups/photography.js — with the photography switch: photography cards
   are their own preview. A card already shows the photograph, and in grid
   mode reveals the title over it on hover — so the generic internal-link
   popup fires at the same moment, in the same gesture, saying the same
   thing in a floating panel that covers neighbouring frames. The
   in-picture caption supersedes it here. This is scoped to the grid, so a
   photograph linked from prose still gets the normal popup. Served after
   popups.js, in the same file (EngineFiles.jsModules). */
(function () {
    'use strict';
    if (window.ozyPopups) window.ozyPopups.exclude('.photography-grid');
}());
