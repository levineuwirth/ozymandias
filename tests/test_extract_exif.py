"""tools/extract-exif.py: how raw EXIF becomes a photo's sidecar — camera
and lens names, focal length, exposure, date and GPS (audit T14). The
exiftool reader is fed through its per-run cache, so no exiftool is needed."""

import unittest
from pathlib import Path
from tests._helpers import load_tool

exif = load_tool("extract-exif.py")

IMAGE = Path("/nonexistent/photo.jpg")


def read(**raw):
    exif._RAW_CACHE[str(IMAGE.resolve())] = raw
    try:
        return exif._read_exif_via_exiftool(IMAGE)
    finally:
        exif._RAW_CACHE.clear()


class FormattingTests(unittest.TestCase):
    def test_shutter(self):
        self.assertEqual(exif._format_shutter(1 / 125), "1/125")
        self.assertEqual(exif._format_shutter(0.5), "1/2")
        self.assertEqual(exif._format_shutter(2.0), "2s")
        self.assertEqual(exif._format_shutter(0), "")

    def test_aperture_and_focal(self):
        self.assertEqual(exif._format_aperture(8.0), "f/8")
        self.assertEqual(exif._format_aperture(5.6), "f/5.6")
        self.assertEqual(exif._format_aperture(1.78), "f/1.8")
        self.assertEqual(exif._format_focal(34.6), "35mm")

    def test_exposure_string(self):
        self.assertEqual(exif._build_exposure_string("1/125", "f/8", 400), "1/125 f/8 ISO 400")
        self.assertEqual(exif._build_exposure_string(None, "f/2", None), "f/2")
        self.assertIsNone(exif._build_exposure_string(None, None, None))


class CameraAndLensTests(unittest.TestCase):
    def test_brand_repeated_in_model_is_collapsed(self):
        self.assertEqual(read(Make="NIKON CORPORATION", Model="NIKON D3100")["camera"], "NIKON D3100")
        self.assertEqual(read(Make="Canon", Model="Canon EOS R6")["camera"], "Canon EOS R6")

    def test_distinct_make_and_model_are_joined(self):
        self.assertEqual(read(Make="FUJIFILM", Model="X-T4")["camera"], "FUJIFILM X-T4")
        self.assertEqual(read(Model="X100V")["camera"], "X100V")

    def test_lens_preference_is_most_readable_first(self):
        out = read(Model="D3100", LensID="AF-S DX Nikkor 18-55mm", LensInfo="18 55 3.5 5.6")
        self.assertEqual(out["lens"], "AF-S DX Nikkor 18-55mm")

    def test_a_phone_lens_that_restates_the_body_is_dropped(self):
        out = read(Make="Apple", Model="iPhone 15 Pro",
                   LensModel="iPhone 15 Pro back triple camera 6.765mm f/1.78",
                   FocalLength=6.765, FocalLengthIn35mmFormat=24)
        self.assertNotIn("lens", out)
        self.assertEqual(out["focal-length"], "24mm", "no real lens: the 35mm equivalent")

    def test_a_named_lens_keeps_its_printed_focal_length(self):
        out = read(Model="D3100", LensModel="18-55mm", FocalLength=18, FocalLengthIn35mmFormat=27)
        self.assertEqual(out["focal-length"], "18mm")


class ExposureDateAndPlaceTests(unittest.TestCase):
    def test_exposure_fields(self):
        out = read(ExposureTime=0.008, FNumber=8, ISO=400)
        self.assertEqual((out["shutter"], out["aperture"], out["iso"], out["exposure"]),
                         ("1/125", "f/8", 400, "1/125 f/8 ISO 400"))

    def test_capture_date(self):
        self.assertEqual(read(DateTimeOriginal="2024:07:11 18:03:22")["captured"], "2024-07-11")
        self.assertEqual(read(CreateDate="2023:01:02 00:00:00")["captured"], "2023-01-02")
        self.assertNotIn("captured", read(DateTimeOriginal="garbage"))

    def test_exiftool_gps_is_already_signed(self):
        self.assertEqual(read(GPSLatitude=55.6786123, GPSLongitude=-12.5901239)["geo"], [55.678612, -12.590124])
        self.assertNotIn("geo", read(GPSLatitude=55.6))

    def test_pillow_gps_takes_its_sign_from_the_reference(self):
        coord = (55.0, 40.0, 43.2)
        self.assertAlmostEqual(exif._gps_to_decimal(coord, "N"), 55.678667, places=5)
        self.assertAlmostEqual(exif._gps_to_decimal(coord, "S"), -55.678667, places=5)
        self.assertAlmostEqual(exif._gps_to_decimal((12.0, 35.0, 0.0), "W"), -12.583333, places=5)
        self.assertIsNone(exif._gps_to_decimal(None, "N"))

    def test_pillow_rationals(self):
        self.assertEqual(exif._pillow_rational((1, 250)), 0.004)
        self.assertIsNone(exif._pillow_rational((1, 0)))
        self.assertEqual(exif._pillow_rational(2.8), 2.8)

    def test_empty_metadata_gives_an_empty_sidecar(self):
        self.assertEqual(read(), {})


if __name__ == "__main__":
    unittest.main()
