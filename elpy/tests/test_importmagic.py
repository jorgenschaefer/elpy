# coding: utf-8

"""Tests for the elpy.impmagic module"""

import re
import unittest

from elpy import impmagic
from elpy.tests.support import BackendTestCase

TEST_SOURCE = '''# test file

import sys

os.getcwd()
'''


class ImportMagicTestCase(BackendTestCase):

    def setUp(self):
        if not impmagic.importmagic:
            raise unittest.SkipTest
        # HACK: importmagic would ignore our test module because it is in
        # a directory called elpy-testXXX -- the correct solution would be
        # for the blacklist to be changeable in importmagic
        impmagic.importmagic.index.BLACKLIST_RE = \
            re.compile(r'\btests?(_|\b)', re.I)
        self.importmagic = impmagic.ImportMagic()
        super(ImportMagicTestCase, self).setUp()

    def build_index(self):
        self.project_file('mymod.py', 'class AnUncommonName:\n pass\n')
        self.importmagic.build_index(self.project_root,
                                     custom_path=[self.project_root])
        self.importmagic._thread.join()

    def test_get_symbols(self):
        self.build_index()
        candidates = self.importmagic.get_import_symbols('AnUncommonName')
        self.assertEqual(candidates, ['mymod'])
        candidates = self.importmagic.get_import_symbols('mymod')
        self.assertEqual(candidates, ['<standalone module>'])

    def test_add_import(self):
        self.build_index()
        start, end, newblock = self.importmagic.add_import(
            TEST_SOURCE, 'AnUncommonName', 'mymod')
        self.assertEqual(start, 2)
        self.assertEqual(end, 4)
        self.assertEqual(newblock.strip(),
                         'import sys\n\nfrom mymod import AnUncommonName')

        start, end, newblock = self.importmagic.add_import(
            TEST_SOURCE, 'mymod', '<standalone module>')
        self.assertEqual(start, 2)
        self.assertEqual(end, 4)
        self.assertEqual(newblock.strip(), 'import sys\n\nimport mymod')

    def test_fixup(self):
        self.build_index()
        start, end, newblock = self.importmagic.fixup_imports(TEST_SOURCE)
        self.assertEqual(start, 2)
        self.assertEqual(end, 4)
        self.assertEqual(newblock.strip(), 'import os')
