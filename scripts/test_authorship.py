"""Check the public author identity inherited from Chulapa."""

from pathlib import Path
import unittest

import mf2py

SITE = Path(__file__).resolve().parents[1] / "_site"
HOME = "https://dieghernan.github.io/"
PROJECT = HOME + "spain-munic-bot/"


class AuthorshipTests(unittest.TestCase):
    def test_landing_page_uses_the_public_name_and_canonical_home(self):
        parsed = mf2py.parse(doc=(SITE / "index.html").read_text(encoding="utf-8"), url=PROJECT)
        entry, = [item for item in parsed["items"] if "h-entry" in item["type"]]
        self.assertEqual(entry["properties"]["url"], [PROJECT])
        author, = entry["properties"]["author"]
        self.assertIn("h-card", author["type"])
        self.assertEqual(author["properties"]["name"], ["Diego HH"])
        self.assertEqual(author["properties"]["url"][0], HOME)


if __name__ == "__main__":
    unittest.main()
