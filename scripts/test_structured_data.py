"""Validate project JSON-LD after building the Jekyll site."""

import json
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[1]


def schemas():
    html = (ROOT / "_site/index.html").read_text(encoding="utf-8")
    return [json.loads(raw) for raw in re.findall(
        r'<script[^>]*type="application/ld\+json"[^>]*>(.*?)</script>', html, re.S)]


class StructuredDataTests(unittest.TestCase):
    def test_software_is_archived_r_code_with_its_own_identity(self):
        blocks = schemas()
        code, = [item for item in blocks if item.get("@type") == "SoftwareSourceCode"]
        self.assertEqual(code["programmingLanguage"], "R")
        self.assertEqual(code["license"], "https://opensource.org/licenses/MIT")
        self.assertEqual(code["codeRepository"], "https://github.com/dieghernan/" + ROOT.name)
        self.assertEqual(code["@id"], "https://dieghernan.github.io/" + ROOT.name + "/#source-code")
        self.assertEqual(code["author"]["url"], "https://dieghernan.github.io/")
        self.assertEqual(code["author"]["sameAs"], "https://orcid.org/0000-0001-8457-4658")
        self.assertEqual([item for item in blocks if item.get("@type") == "WebApplication"], [])
        self.assertNotIn("version", code)

    def test_bot_is_described_as_inactive(self):
        blocks = schemas()
        code, = [item for item in blocks if item.get("@type") == "SoftwareSourceCode"]
        self.assertIn("Automated posting is disabled", code["description"])
        self.assertEqual([item for item in blocks if item.get("@type") == "Dataset"], [])
        self.assertTrue((ROOT / "R/trigger.R").is_file())
        self.assertTrue((ROOT / "R/01_create_map.R").is_file())


if __name__ == "__main__":
    unittest.main()
