/* copy.js — Copy-to-clipboard control for <pre> code blocks.
 *
 * Each eligible <pre> is wrapped in a `.code-block` positioning context and
 * given a sibling <button class="copy-btn">. The button is a *sibling*, never
 * a child, which is the whole point of the design:
 *
 *   - It is outside the <pre>'s scroll box, so it stays pinned to the block's
 *     corner instead of sliding away when the code is scrolled sideways.
 *   - It is out of the code's flow, so it cannot reflow, indent, or shorten
 *     the first line (the old float + negative-margin arrangement did all
 *     three, which is what made short inline snippets look broken).
 *   - It is not inside the copied subtree, so the copied text needs no
 *     cloning or scrubbing, and a screen reader reading the block no longer
 *     hits a stray "copy" before the code.
 *
 * It is placed inside the <pre>'s top padding band, so on any viewport —
 * phones included — it overlays empty gutter rather than code. It is an icon,
 * not a word, and stays invisible until the block is hovered or the button is
 * focused; on touch, where there is no hover, it sits faint but always
 * reachable with a finger-sized hit area (see .copy-btn::after in CSS).
 *
 * Dynamic content: attaches over an arbitrary root and listens for
 * `ln:content-added`, the event transclude.js dispatches after injecting
 * content. Idempotent per <pre> (a data flag), so a container enhanced twice
 * gets one button.
 */

(function () {
    'use strict';

    var RESET_DELAY = 1800; /* ms before the button reverts to its idle state */

    var SVG_NS = 'http://www.w3.org/2000/svg';

    /* Blocks that are chrome rather than content: link/source popups, the
       signature panel, the score reader's cloned gutter. A copy button in
       any of those is noise. */
    var EXCLUDED = '.link-popup, .popup-sig, .selection-popup, .score-reader,' +
                   ' .ann-tooltip, [data-no-copy]';

    function svg(paths, cls) {
        var el = document.createElementNS(SVG_NS, 'svg');
        el.setAttribute('viewBox', '0 0 24 24');
        el.setAttribute('fill', 'none');
        el.setAttribute('stroke', 'currentColor');
        el.setAttribute('stroke-width', '2');
        el.setAttribute('stroke-linecap', 'round');
        el.setAttribute('stroke-linejoin', 'round');
        el.setAttribute('aria-hidden', 'true');
        el.setAttribute('focusable', 'false');
        el.setAttribute('class', cls);
        paths.forEach(function (d) {
            var p = document.createElementNS(SVG_NS, 'path');
            p.setAttribute('d', d);
            el.appendChild(p);
        });
        return el;
    }

    function copyIcon() {
        return svg([
            'M10 8h9a2 2 0 0 1 2 2v9a2 2 0 0 1-2 2h-9a2 2 0 0 1-2-2v-9a2 2 0 0 1 2-2z',
            'M4 16a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2'
        ], 'copy-btn-glyph copy-btn-glyph--idle');
    }

    function doneIcon() {
        return svg(['M20 6 9 17l-5-5'], 'copy-btn-glyph copy-btn-glyph--done');
    }

    /* The block's text, exactly as authored: the button is not part of the
       subtree, so textContent is already clean. Only the trailing newline
       Pandoc leaves on every fenced block is dropped. */
    function blockText(pre) {
        var code = pre.querySelector('code');
        return (code || pre).textContent.replace(/\n$/, '');
    }

    /* Clipboard API needs a secure context; fall back to a throwaway textarea
       so file:// previews and plain-http mirrors still copy. */
    function writeClipboard(text) {
        if (navigator.clipboard && window.isSecureContext) {
            return navigator.clipboard.writeText(text);
        }
        return new Promise(function (resolve, reject) {
            var ta = document.createElement('textarea');
            ta.value = text;
            ta.setAttribute('readonly', '');
            ta.style.position = 'fixed';
            ta.style.top = '-9999px';
            document.body.appendChild(ta);
            ta.select();
            var ok = false;
            try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
            ta.remove();
            ok ? resolve() : reject(new Error('copy unsupported'));
        });
    }

    function attachButton(pre) {
        if (pre.dataset.copyButton === '1') return;
        if (pre.closest(EXCLUDED)) return;
        pre.dataset.copyButton = '1';

        var wrap = document.createElement('div');
        wrap.className = 'code-block';
        pre.parentNode.insertBefore(wrap, pre);
        wrap.appendChild(pre);

        var btn = document.createElement('button');
        btn.type = 'button';
        btn.className = 'copy-btn';
        btn.setAttribute('aria-label', 'Copy code to clipboard');
        btn.title = 'Copy';
        btn.appendChild(copyIcon());
        btn.appendChild(doneIcon());

        /* Announced, not drawn: the icon swap is the sighted feedback. */
        var status = document.createElement('span');
        status.className = 'visually-hidden';
        status.setAttribute('role', 'status');
        status.setAttribute('aria-live', 'polite');

        var timer = null;

        function settle(state, announced) {
            btn.setAttribute('data-state', state);
            btn.title = announced;
            status.textContent = announced;
            if (timer) clearTimeout(timer);
            timer = setTimeout(function () {
                btn.removeAttribute('data-state');
                btn.title = 'Copy';
                status.textContent = '';
            }, RESET_DELAY);
        }

        btn.addEventListener('click', function () {
            writeClipboard(blockText(pre)).then(function () {
                settle('copied', 'Copied');
            }).catch(function () {
                settle('error', 'Copy failed');
            });
        });

        wrap.appendChild(btn);
        wrap.appendChild(status);
    }

    /* Attach to every <pre> at or below `root` (a document, an element, or
       nothing at all — in which case the whole document). */
    function initCopyButtons(root) {
        var scope = root && root.querySelectorAll ? root : document;
        if (scope.nodeType === 1 && scope.tagName === 'PRE') attachButton(scope);
        scope.querySelectorAll('pre').forEach(attachButton);
    }

    window.initCopyButtons = initCopyButtons;

    document.addEventListener('DOMContentLoaded', function () {
        initCopyButtons(document);
    });

    /* Content injected after load (transclusions, and anything else that
       calls window.lnEnhance). */
    document.addEventListener('ln:content-added', function (e) {
        initCopyButtons(e.detail && e.detail.container);
    });
}());
