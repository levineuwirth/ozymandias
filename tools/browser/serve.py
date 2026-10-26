#!/usr/bin/env python3
"""Read-only emulation of the production nginx vhost for _site/.

- try_files $uri $uri/index.html $uri.html =404 ; index index.html
- error_page 404 /404.html (status preserved); /404.html itself is internal
- gzip_static / brotli_static sidecars (optional, --compress)
- security-headers.conf + security-framing.conf, with the CSP taken from
  nginx/security-headers.conf (enforcing candidate) or a variant
- /pdfjs/ and /archive/ use frame-ancestors 'self' + XFO SAMEORIGIN
- /proxy/* -> 404 (logged), /csp-report POST -> 204 + JSONL log
- Range support (nginx serves ranges for static files)
- --fixtures DIR: /__fixture/<name> serves DIR/<name>, with the same
  headers, for pages written to exercise one behavior (tools/browser/fixture/,
  or a test's own)
"""
import argparse, json, os, re, sys, threading, time, urllib.parse
from http.server import ThreadingHTTPServer, BaseHTTPRequestHandler

ap = argparse.ArgumentParser()
ap.add_argument('--root', required=True)
ap.add_argument('--port', type=int, required=True, help='0 for any free port')
ap.add_argument('--csp', required=True, help='policy string')
ap.add_argument('--mode', choices=['enforce', 'report-only', 'none'], default='enforce')
ap.add_argument('--compress', action='store_true')
ap.add_argument('--log', required=True)
ap.add_argument('--fixtures', help='directory served under /__fixture/')
args = ap.parse_args()
ROOT = os.path.realpath(args.root)
os.makedirs(os.path.dirname(os.path.abspath(args.log)), exist_ok=True)
LOG = open(args.log, 'a', buffering=1)
LOCK = threading.Lock()

MIME = {
    '.html': 'text/html', '.htm': 'text/html', '.css': 'text/css',
    '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json',
    '.svg': 'image/svg+xml', '.png': 'image/png', '.jpg': 'image/jpeg', '.jpeg': 'image/jpeg',
    '.gif': 'image/gif', '.webp': 'image/webp', '.ico': 'image/x-icon', '.avif': 'image/avif',
    '.woff': 'font/woff', '.woff2': 'font/woff2', '.ttf': 'font/ttf', '.otf': 'font/otf',
    '.pdf': 'application/pdf', '.xml': 'text/xml', '.txt': 'text/plain', '.wasm': 'application/wasm',
    '.mp3': 'audio/mpeg', '.ogg': 'audio/ogg', '.mp4': 'video/mp4', '.webmanifest': 'application/manifest+json',
    '.asc': 'application/octet-stream', '.sig': 'application/octet-stream', '.bcmap': 'application/octet-stream',
    '.pfb': 'application/octet-stream', '.ftl': 'application/octet-stream', '.icc': 'application/octet-stream',
}

def log(kind, **kw):
    kw['kind'] = kind; kw['t'] = time.time()
    with LOCK:
        LOG.write(json.dumps(kw) + '\n')

def resolve(upath):
    """nginx try_files emulation. Returns filesystem path or None."""
    p = os.path.normpath(upath).lstrip('/')
    if '..' in p.split('/'):
        return None
    base = os.path.join(ROOT, p) if p not in ('', '.') else ROOT
    cands = []
    if upath.endswith('/'):
        cands = [os.path.join(base, 'index.html')]
    else:
        cands = [base, os.path.join(base, 'index.html'), base + '.html']
    for c in cands:
        if os.path.isfile(c):
            rc = os.path.realpath(c)
            if rc.startswith(ROOT):
                return rc
    return None

def fixture(name):
    """A file directly or below --fixtures, or None."""
    top = os.path.realpath(args.fixtures)
    fs = os.path.realpath(os.path.join(top, name))
    return fs if fs.startswith(top + os.sep) and os.path.isfile(fs) else None

class H(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'
    def log_message(self, fmt, *a):
        pass

    def sec_headers(self, path):
        h = [('X-Content-Type-Options', 'nosniff'),
             ('Referrer-Policy', 'strict-origin-when-cross-origin'),
             ('Permissions-Policy', 'camera=(), microphone=(), geolocation=(), interest-cohort=()')]
        if args.mode == 'enforce':
            h.append(('Content-Security-Policy', args.csp))
        elif args.mode == 'report-only':
            h.append(('Content-Security-Policy-Report-Only', args.csp))
        if path.startswith('/pdfjs/') or path.startswith('/archive/'):
            h += [('X-Frame-Options', 'SAMEORIGIN'), ('Content-Security-Policy', "frame-ancestors 'self'")]
        else:
            h += [('X-Frame-Options', 'DENY'), ('Content-Security-Policy', "frame-ancestors 'none'")]
        return h

    def send_body(self, status, headers, body, head=False):
        self.send_response(status)
        for k, v in headers:
            self.send_header(k, v)
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        if not head:
            try:
                self.wfile.write(body)
            except (BrokenPipeError, ConnectionResetError):
                pass

    def do_POST(self):
        path = urllib.parse.urlsplit(self.path).path
        n = int(self.headers.get('Content-Length') or 0)
        body = self.rfile.read(n) if n else b''
        if path == '/csp-report':
            try:
                rep = json.loads(body.decode('utf-8', 'replace'))
            except Exception:
                rep = body.decode('utf-8', 'replace')
            log('csp-report', report=rep, referer=self.headers.get('Referer'), ua=self.headers.get('User-Agent'))
            self.send_body(204, [('Cache-Control', 'no-store')], b'')
            return
        self.send_body(405, [], b'')

    def do_HEAD(self):
        self.do_GET(head=True)

    def do_GET(self, head=False):
        sp = urllib.parse.urlsplit(self.path)
        path = urllib.parse.unquote(sp.path)
        if path.startswith('/proxy/'):
            log('proxy', path=self.path)
            self.serve_404(head)
            return
        if path == '/404.html':
            self.serve_404(head)
            return
        if args.fixtures and path.startswith('/__fixture/'):
            fs = fixture(path[len('/__fixture/'):])
            if fs is None:
                self.serve_404(head)
            else:
                self.serve_file(fs, path, head)
            return
        fs = resolve(path)
        if fs is None:
            log('404', path=self.path, referer=self.headers.get('Referer'))
            self.serve_404(head)
            return
        self.serve_file(fs, path, head)

    def serve_404(self, head):
        fs = os.path.join(ROOT, '404.html')
        with open(fs, 'rb') as f:
            body = f.read()
        hs = self.sec_headers('/404.html') + [('Content-Type', 'text/html')]
        self.send_body(404, hs, body, head)

    def serve_file(self, fs, path, head):
        ext = os.path.splitext(fs)[1].lower()
        ctype = MIME.get(ext, 'application/octet-stream')
        hs = self.sec_headers(path) + [('Content-Type', ctype)]
        enc = None
        ae = self.headers.get('Accept-Encoding', '')
        rng = self.headers.get('Range')
        src = fs
        if args.compress and not rng:
            if 'br' in ae and os.path.isfile(fs + '.br'):
                src, enc = fs + '.br', 'br'
            elif 'gzip' in ae and os.path.isfile(fs + '.gz'):
                src, enc = fs + '.gz', 'gzip'
            if os.path.isfile(fs + '.br') or os.path.isfile(fs + '.gz'):
                hs.append(('Vary', 'Accept-Encoding'))
        with open(src, 'rb') as f:
            body = f.read()
        if enc:
            hs.append(('Content-Encoding', enc))
        hs.append(('Accept-Ranges', 'bytes'))
        if rng and not enc:
            m = re.match(r'bytes=(\d*)-(\d*)$', rng.strip())
            if m:
                total = len(body)
                a, b = m.group(1), m.group(2)
                if a == '':
                    start = max(0, total - int(b)); end = total - 1
                else:
                    start = int(a); end = int(b) if b else total - 1
                end = min(end, total - 1)
                if start <= end:
                    hs.append(('Content-Range', f'bytes {start}-{end}/{total}'))
                    self.send_body(206, hs, body[start:end + 1], head)
                    return
        self.send_body(200, hs, body, head)

class Server(ThreadingHTTPServer):
    daemon_threads = True
    # nginx listens with a backlog of 511; socketserver's default is 5. With
    # several browsers opening connections at once that queue overflowed, the
    # kernel dropped the SYNs (TcpExtListenOverflows), and the browsers retried
    # after 1, 2, 4, 8… seconds: page loads stalled for up to tens of seconds
    # when the machine was busy, and the browser tests timed out.
    request_queue_size = 511

srv = Server(('127.0.0.1', args.port), H)
# --port 0 takes a free port; this line names it (the tests read it).
print(f'serving {ROOT} on 127.0.0.1:{srv.server_address[1]} mode={args.mode}', flush=True)
srv.serve_forever()
