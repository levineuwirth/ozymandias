"""Calendar days in static/js/utils.js (lnUtils.isoDay, lnUtils.localDay).

now.js and popups.js's date popups count "how long ago" with these: the
reader's local calendar date against a YYYY-MM-DD date, in whole days
(audit J09). The popups used to measure from the date's UTC midnight to
the present instant, so the same date read "yesterday" on a Los Angeles
evening and had no popup at all, as a future date, on a Tokyo morning.
utils.js runs in node here, under each time zone the cases name.
"""

from __future__ import annotations

import json
import shutil
import subprocess
import unittest

from tests._helpers import engine_path, script_env

UTILS = engine_path("static/js/utils.js")

RUN = """
const vm = require('vm'), fs = require('fs');
const sandbox = {window: {}};
vm.createContext(sandbox);
vm.runInContext(fs.readFileSync(process.argv[1], 'utf8'), sandbox);
const u = sandbox.window.lnUtils;
const out = {};
for (const [iso, instant] of JSON.parse(process.argv[2])) {
  const then = u.isoDay(iso);
  out[iso + ' @ ' + instant] = then === null ? null : u.localDay(new Date(instant)) - then;
}
console.log(JSON.stringify(out));
"""

# time zone -> [(date, instant, calendar days from the date to the instant)]
CASES = {
    "America/Los_Angeles": [
        # The audit's case: 20:00 on 1 October, a date of that day.
        ("2026-10-01", "2026-10-02T03:00:00Z", 0),
        ("2026-09-30", "2026-10-02T03:00:00Z", 1),
        ("2026-10-02", "2026-10-02T03:00:00Z", -1),
    ],
    "Asia/Tokyo": [
        # 08:00 on 2 October, a date of that day: today, not the future.
        ("2026-10-02", "2026-10-01T23:00:00Z", 0),
        ("2026-10-01", "2026-10-01T23:00:00Z", 1),
    ],
    "America/New_York": [
        # Across both 2026 DST changes; a local day is 23 or 25 hours.
        ("2026-03-07", "2026-03-08T23:30:00-04:00", 1),
        ("2026-03-08", "2026-03-08T00:30:00-05:00", 0),
        ("2026-10-31", "2026-11-01T23:30:00-05:00", 1),
        ("2026-11-01", "2026-11-01T00:30:00-04:00", 0),
    ],
    "Europe/Berlin": [
        ("2026-10-06", "2026-10-05T22:30:00Z", 0),
        ("2025-10-21", "2026-10-06T10:00:00Z", 350),
        ("2024-02-29", "2026-02-28T12:00:00Z", 730),
    ],
    "UTC": [
        ("2026-10-01", "2026-10-01T00:00:00Z", 0),
        ("2026-10-01", "2026-10-01T23:59:59Z", 0),
        ("2026-10-01T18:00:00Z", "2026-10-02T01:00:00Z", 1),
    ],
}

NOT_DATES = ["", "2026-02-31", "2025-02-29", "2026-13-01", "2026-00-10", "0099-01-01",
             "10/01/2026", "2026-1-01"]


@unittest.skipUnless(shutil.which("node"), "node not on PATH")
class CalendarDayTests(unittest.TestCase):
    def days(self, tz: str, pairs: list[tuple[str, str]]) -> dict:
        done = subprocess.run(["node", "-e", RUN, str(UTILS), json.dumps(pairs)],
                              env=script_env(TZ=tz), capture_output=True, text=True,
                              check=True, timeout=30)
        return json.loads(done.stdout)

    def test_days_in_each_time_zone(self) -> None:
        for tz, cases in CASES.items():
            got = self.days(tz, [(iso, instant) for iso, instant, _ in cases])
            for iso, instant, want in cases:
                with self.subTest(tz=tz, date=iso, at=instant):
                    self.assertEqual(got[f"{iso} @ {instant}"], want)

    def test_what_is_not_a_date(self) -> None:
        got = self.days("UTC", [(s, "2026-10-06T12:00:00Z") for s in NOT_DATES])
        for s in NOT_DATES:
            with self.subTest(text=s):
                self.assertIsNone(got[f"{s} @ 2026-10-06T12:00:00Z"])


if __name__ == "__main__":
    unittest.main()
