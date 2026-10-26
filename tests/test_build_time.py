"""The footer build time comes from one file, and an unchanged page keeps
its signature (tools/stamp-build-time.py, tools/sign-site.sh).

Until 2026-10-06 the time was stamped into every page after every build,
so every build rewrote and recompressed ~500 unchanged pages and every
deploy re-signed and re-sent them. Now the build writes
_site/build/time.txt, the pages stay byte-identical, and sign-site.sh
signs a page again only when its content changed.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests._helpers import SITE_ROOT, engine_path, script_env, stub_bin

STAMP = engine_path("tools/stamp-build-time.py")
SIGN = engine_path("tools/sign-site.sh")
PRESET = engine_path("tools/preset-signing-passphrase.sh")
GPG_PRESET = Path("/usr/lib/gnupg/gpg-preset-passphrase")
FOOTER = '<footer><span class="footer-build-time" data-build-time></span></footer>'


class StampTests(unittest.TestCase):
    def test_writes_the_time_once_and_leaves_pages_alone(self):
        with tempfile.TemporaryDirectory() as directory:
            site = Path(directory)
            page = site / "essays" / "a.html"
            page.parent.mkdir(parents=True)
            page.write_text(f"<html><body><p>Prose.</p>{FOOTER}</body></html>\n")
            before = page.read_bytes()
            done = subprocess.run(["python3", str(STAMP), str(site)], env=script_env(),
                                  capture_output=True, text=True, timeout=30)
            self.assertEqual(done.returncode, 0, done.stderr)
            self.assertEqual(page.read_bytes(), before)
            # The footers' format: "Monday, October 6th, 2026 13:03:33".
            self.assertRegex((site / "build" / "time.txt").read_text(),
                             r"^[A-Z][a-z]+day, [A-Z][a-z]+ \d{1,2}(st|nd|rd|th), "
                             r"\d{4} \d{2}:\d{2}:\d{2}\n$")

    def test_the_footer_ships_an_empty_span(self):
        footer = (SITE_ROOT / "templates" / "partials" / "footer.html").read_text()
        self.assertIn('<span class="footer-build-time" data-build-time></span>', footer)


@unittest.skipUnless(shutil.which("gpg") and shutil.which("gpgconf"), "needs gpg")
class SignTests(unittest.TestCase):
    """Signed with a throwaway Ed25519 key in a scratch keyring."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.home = self.root / "gnupg"
        self.home.mkdir(mode=0o700)
        self.addCleanup(subprocess.run, ["gpgconf", "--homedir", str(self.home), "--kill", "all"],
                        capture_output=True)
        subprocess.run(["gpg", "--homedir", str(self.home), "--batch", "--passphrase", "",
                        "--quick-gen-key", "sign-site test", "ed25519", "sign", "never"],
                       check=True, capture_output=True, timeout=60)
        listing = subprocess.run(["gpg", "--homedir", str(self.home), "--list-keys", "--with-colons"],
                                 check=True, capture_output=True, text=True).stdout
        self.key = next(line.split(":")[9] for line in listing.splitlines() if line.startswith("fpr:"))
        self.site = self.root / "site"
        self.manifest = self.root / "sign-manifest.txt"
        for rel in ("index.html", "essays/a.html", "essays/b.html"):
            self.write(rel, f"<p>{rel}</p>")

    def write(self, rel, body):
        path = self.site / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(f"<html><body>{body}{FOOTER}</body></html>\n")
        return path

    def run_sign(self, **extra):
        env = script_env(GNUPGHOME=str(self.home), SIGNING_KEY=self.key,
                         SIGN_MANIFEST=str(self.manifest), **extra)
        return subprocess.run(["bash", str(SIGN), str(self.site)], env=env,
                              capture_output=True, text=True, timeout=120)

    def sign(self, **extra):
        done = self.run_sign(**extra)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return int(re.search(r"Signed (\d+) HTML", done.stdout).group(1))

    def verifies(self, rel):
        page = self.site / rel
        return subprocess.run(["gpg", "--homedir", str(self.home), "--verify",
                               f"{page}.sig", str(page)], capture_output=True).returncode == 0

    def test_only_changed_or_unsigned_pages_are_signed_again(self):
        self.assertEqual(self.sign(), 3)
        sig_a = (self.site / "essays/a.html.sig").read_bytes()
        self.assertEqual(self.sign(), 0)
        self.assertEqual((self.site / "essays/a.html.sig").read_bytes(), sig_a)

        self.write("essays/a.html", "<p>revised</p>")
        (self.site / "essays/b.html.sig").unlink()
        self.assertEqual(self.sign(), 2)
        for rel in ("index.html", "essays/a.html", "essays/b.html"):
            self.assertTrue(self.verifies(rel), rel)

    def test_a_new_mtime_alone_is_not_a_change(self):
        self.sign()
        (self.site / "essays/a.html").touch()
        self.assertEqual(self.sign(), 0)

    def test_sign_all_and_another_key_sign_everything(self):
        self.sign()
        self.assertEqual(self.sign(SIGN_ALL="1"), 3)
        self.manifest.write_text(self.manifest.read_text().replace(self.key, "OTHER", 1))
        self.assertEqual(self.sign(), 3)

    def test_a_swapped_or_edited_signature_is_signed_again(self):
        # Reuse vouches that the .sig is the file made for this content: one
        # copied from another page, or altered, is replaced.
        self.sign()
        shutil.copy(self.site / "essays/b.html.sig", self.site / "essays/a.html.sig")
        self.assertFalse(self.verifies("essays/a.html"))
        self.assertEqual(self.sign(), 1)
        self.assertTrue(self.verifies("essays/a.html"))
        with open(self.site / "index.html.sig", "a") as sig:
            sig.write("\n")
        self.assertEqual(self.sign(), 1)
        self.assertTrue(self.verifies("index.html"))

    @unittest.skipIf(os.geteuid() == 0, "root reads an unreadable file")
    def test_a_page_that_cannot_be_hashed_stops_signing(self):
        # A failed hash used to vanish inside a process substitution: the
        # run succeeded, left the page out of the manifest and kept its old
        # signature.
        self.sign()
        before = self.manifest.read_bytes()
        page = self.write("essays/a.html", "<p>revised</p>")
        page.chmod(0)
        self.addCleanup(page.chmod, 0o644)
        done = self.run_sign()
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("could not hash every page", done.stderr)
        self.assertEqual(self.manifest.read_bytes(), before)

    def site_yaml_names_a_subkey(self, **extra) -> tuple[dict[str, str], str, str]:
        # As in production, site.yaml names a signing subkey, whose keygrip
        # is not the primary key's. `site config` (stubbed) gives its
        # fingerprint, spaced as the footer shows it. Returns the
        # environment, the subkey's fingerprint and its keygrip.
        subprocess.run(["gpg", "--homedir", str(self.home), "--batch", "--passphrase", "",
                        "--quick-add-key", self.key, "ed25519", "sign", "never"],
                       check=True, capture_output=True, timeout=60)
        listing = subprocess.run(["gpg", "--homedir", str(self.home), "--with-colons", "--with-keygrip",
                                  "--list-keys", self.key], check=True, capture_output=True, text=True).stdout
        subkey = listing[listing.index("\nsub:"):].splitlines()
        fingerprint = next(line.split(":")[9] for line in subkey if line.startswith("fpr:"))
        keygrip = next(line.split(":")[9] for line in subkey if line.startswith("grp:"))
        spaced = " ".join(fingerprint[i:i + 8] for i in range(0, len(fingerprint), 8))
        stub = stub_bin(self.root / "bin", ["site"],
                        f"#!/bin/sh\necho '{{\"signing-fingerprint\": \"{spaced}\"}}'\n")
        env = script_env(GNUPGHOME=str(self.home), SITE_BINARY=str(stub / "site"), **extra)
        return env, fingerprint, keygrip

    def test_without_a_key_named_the_site_s_signs(self):
        env, fingerprint, _ = self.site_yaml_names_a_subkey(SIGN_MANIFEST=str(self.manifest))
        done = subprocess.run(["bash", str(SIGN), str(self.site)], capture_output=True, text=True,
                              env=env, timeout=120)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertTrue(self.manifest.read_text().startswith(f"v2 key {fingerprint} site "))
        self.assertTrue(self.verifies("index.html"))

    @unittest.skipUnless(GPG_PRESET.is_file(), "needs gpg-preset-passphrase")
    def test_the_passphrase_is_cached_for_the_site_s_key(self):
        # By the subkey's keygrip, which the script finds from its
        # fingerprint.
        (self.home / "gpg-agent.conf").write_text("allow-preset-passphrase\n")
        subprocess.run(["gpgconf", "--homedir", str(self.home), "--kill", "gpg-agent"],
                       check=True, capture_output=True)
        env, _, keygrip = self.site_yaml_names_a_subkey()
        done = subprocess.run(["bash", str(PRESET)], input="passphrase\n", capture_output=True,
                              text=True, env=env, timeout=60)
        self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        self.assertIn(f"Passphrase cached for keygrip {keygrip} ", done.stdout)


if __name__ == "__main__":
    unittest.main()
