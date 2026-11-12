/* popups/source-refs.js — a source file the page links (build/Filters/SourceRefs.hs).
   With the source-refs switch: served after popups.js, in the same file
   (EngineFiles.jsModules), and bound through window.ozyPopups. */
(function () {
    'use strict';

    var P = window.ozyPopups;
    if (!P) return;   /* popups.js did not run (a touch-only device returns before binding) */

    var sourceOrProse = P.source.sourceOrProse, sourceOpts = P.source.sourceOpts;

    /* Source-file preview — fetches /source/<path> (a same-origin copy
       emitted by the source-preview Hakyll rule) and renders it with
       renderSourcePopup; a Markdown file instead gets its sectioned
       prose rendering (/source/<path>.sections.json, MarkdownSections.hs)
       unless the link asks for a line range.

       Raw responses are cached (cachedFetch); rendering is repeated per
       hover so a cached entry never gets re-parented (a Node can only
       live in one place at a time). */
    function sourceContent(target) {
        var path = target.dataset.sourcePath;
        if (!path) return Promise.resolve(null);
        var opts = sourceOpts(target, 'view full file');
        var raw  = '/source/' + path;
        return sourceOrProse(path, raw, raw + '.sections.json', null, opts);
    }

    /* Source-file references — wrapped at build time by
       build/Filters/SourceRefs.hs around inline `path` and Forgejo
       links. These specific kinds are bound before the generic
       internal- and external-link binders (popups.js). Other
       links to a forge get no popup: Forgejo sends no CORS headers, and
       an Anubis in front of one refuses anonymous API requests. */
    P.bind(40, 'a.source-ref[data-source-path]', sourceContent);
}());
