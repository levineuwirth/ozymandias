/* popups/music.js — with the music switch: the music shelf is its own
   preview. Pointing at a spine turns that score face-out beside it, and
   a popup would cover the page it has just shown. Served after popups.js,
   in the same file (EngineFiles.jsModules). */
(function () {
    'use strict';
    if (window.ozyPopups) window.ozyPopups.exclude('.shelf');
}());
