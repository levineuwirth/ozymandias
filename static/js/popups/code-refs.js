/* popups/code-refs.js — GitHub blob, tree and commit snapshots.
   With the code-refs switch: served after popups.js, in the same file
   (EngineFiles.jsModules), and bound through window.ozyPopups. */
(function () {
    'use strict';

    var P = window.ozyPopups;
    if (!P) return;   /* popups.js did not run (a touch-only device returns before binding) */

    function esc(s) {
        return window.ozyUtils.escapeHtml(s);
    }

    var truncate = P.truncate;
    var sourceOrProse = P.source.sourceOrProse, sourceOpts = P.source.sourceOpts,
        cachedFetch = P.source.cachedFetch;

    /* ------------------------------------------------------------------
       Code references — GitHub blob / tree / commit snapshots.

       tools/code-refs.py stores each linked object under /code-refs/
       at build time; build/Filters/CodeRefs.hs puts its location and
       revision on the link as data-code-* attributes. Everything here
       is same-origin: no GitHub API call, no rate limit, and the popup
       shows exactly the revision the link is pinned to.
    ------------------------------------------------------------------ */

    function codeRefContent(target) {
        var d    = target.dataset;
        var kind = d.codeRef;
        var src  = d.codeSrc;
        if (!src || !/^\/code-refs\//.test(src)) return Promise.resolve(null);

        var info = {
            repo:   d.codeRepo || '',
            sha:    d.codeSha || '',
            path:   d.codePath || '',
            date:   d.codeDate || '',
            branch: d.codeBranch || ''
        };

        if (kind === 'blob') {
            return sourceOrProse(info.path, src,
                src.replace(/\.txt$/, '.sections.json'),
                revisionLine(info), sourceOpts(target, 'view on GitHub'));
        }
        if (kind === 'tree') {
            return cachedFetch(src, 'json').then(function (tree) {
                return tree ? renderTreePopup(info, tree) : null;
            });
        }
        if (kind === 'commit') {
            return cachedFetch(src, 'json').then(function (c) {
                return c ? renderCommitPopup(info, c) : null;
            });
        }
        return Promise.resolve(null);
    }

    /* "JamesPetrie/VerInf @ 3508ef1 · 20 Sep 2026" — or, for a link to
       a branch, which snapshot of that branch the popup is showing. */
    function revisionLine(info) {
        var short = info.sha.slice(0, 7);
        var date  = formatDay(info.date);
        if (info.branch) {
            return info.repo + ' · ' + info.branch + ' as of ' + short
                 + (date ? ', ' + date : '');
        }
        return info.repo + ' @ ' + short + (date ? ' · ' + date : '');
    }

    function formatDay(iso) {
        var d = iso ? new Date(iso) : null;
        if (!d || isNaN(d.getTime())) return '';
        return d.toLocaleDateString('en-GB',
            { day: 'numeric', month: 'short', year: 'numeric', timeZone: 'UTC' });
    }

    /* First paragraph of a README that is prose rather than a heading,
       badge row, or HTML block — markup stripped to plain text. */
    function readmeLead(text) {
        var paras = String(text).split(/\n\s*\n/);
        for (var i = 0; i < paras.length; i++) {
            var p = paras[i].trim();
            if (!p || /^(#|!\[|\[!\[|<|```|---|\|)/.test(p)) continue;
            return p.replace(/!\[[^\]]*\]\([^)]*\)/g, '')
                    .replace(/\[([^\]]*)\]\([^)]*\)/g, '$1')
                    .replace(/[`*_]/g, '')
                    .replace(/\s+/g, ' ');
        }
        return '';
    }

    /* The snapshot commit's own message is deliberately not shown: it
       describes whatever change happened to land last, which is rarely
       about the directory linked. The revision line is what matters. */
    function renderTreePopup(info, tree) {
        var MAX_ENTRIES = 24;
        var entries = tree.entries || [];
        var html = '<div class="popup-source-code popup-code-ref">';
        var path = info.path ? info.path.replace(/\/?$/, '/') : info.repo + '/';
        var rev  = revisionLine(info);
        html += '<div class="popup-source-path">' + esc(path)
              + '<div class="popup-source-rev">' + esc(rev) + '</div></div>';
        html += '<div class="popup-code-body">';
        var lead = tree.readme ? readmeLead(tree.readme.text) : '';
        if (lead) html += '<div class="popup-abstract">' + esc(truncate(lead, 320)) + '</div>';
        html += '<ul class="popup-code-list">';
        entries.slice(0, MAX_ENTRIES).forEach(function (e) {
            var dir = e.type === 'dir';
            html += '<li class="' + (dir ? 'is-dir' : 'is-file') + '">'
                  + esc(e.name) + (dir ? '/' : '') + '</li>';
        });
        html += '</ul></div>';
        if (entries.length > MAX_ENTRIES) {
            var n = entries.length - MAX_ENTRIES;
            html += '<div class="popup-source-truncated">' + n + ' more entr'
                  + (n === 1 ? 'y' : 'ies') + '</div>';
        }
        return html + '</div>';
    }

    function renderCommitPopup(info, c) {
        var MAX_FILES = 12;
        var lines   = String(c.message || '').split('\n');
        var subject = lines[0] || '';
        var body    = lines.slice(1).join('\n').trim();
        var files   = c.files || [];
        var total   = +(c.files_total != null ? c.files_total : files.length) || 0;
        var st      = c.stats || {};

        var html = '<div class="popup-source-code popup-code-ref">';
        html += '<div class="popup-source-path">' + esc(info.repo) + ' @ '
              + esc(String(c.sha || info.sha).slice(0, 7))
              + '<div class="popup-source-rev">'
              + esc([c.author, formatDay(c.date)].filter(Boolean).join(' · '))
              + '</div></div>';
        html += '<div class="popup-code-body">';
        html += '<div class="popup-code-subject">' + esc(subject) + '</div>';
        if (body) html += '<div class="popup-abstract">' + esc(truncate(body, 420)) + '</div>';
        html += '<div class="popup-code-stat">' + total + ' file' + (total === 1 ? '' : 's')
              + (st.additions != null ? ' · <ins>+' + (+st.additions) + '</ins>' : '')
              + (st.deletions != null ? ' <del>−' + (+st.deletions) + '</del>' : '')
              + '</div>';
        html += '<ul class="popup-code-list popup-code-files">';
        files.slice(0, MAX_FILES).forEach(function (f) {
            html += '<li><span class="popup-code-file">' + esc(f.filename) + '</span>'
                  + '<span class="popup-code-delta"><ins>+' + (+f.additions || 0) + '</ins> '
                  + '<del>−' + (+f.deletions || 0) + '</del></span></li>';
        });
        html += '</ul></div>';
        if (total > MAX_FILES) {
            var n = total - MAX_FILES;
            html += '<div class="popup-source-truncated">' + n + ' more file'
                  + (n === 1 ? '' : 's') + '</div>';
        }
        return html + '</div>';
    }

    /* GitHub code references — blob / tree / commit links that
       tools/code-refs.py snapshotted at build time and
       build/Filters/CodeRefs.hs tagged. Bound before the external
       dispatcher so the repository-card provider does not claim
       them; untagged GitHub links still fall through to it. */
    P.bind(50, 'a[data-code-ref][data-code-src]', codeRefContent);
}());
