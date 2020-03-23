# coding: utf-8
"""Tests for the elpy.black module"""

import unittest
import os

from elpy import blackutil
from elpy.rpc import Fault
from elpy.tests.support import BackendTestCase


@unittest.skipIf(blackutil.BLACK_NOT_SUPPORTED,
                 'black not supported for current python version')
class BLACKTestCase(BackendTestCase):
    def setUp(self):
        if blackutil.BLACK_NOT_SUPPORTED:
            raise unittest.SkipTest

    def test_fix_code_should_throw_error_for_invalid_code(self):
        src = 'x = '
        self.assertRaises(Fault, blackutil.fix_code, src, os.getcwd())

    def test_fix_code(self):
        testdata = [
            ('x=       123\n', 'x = 123\n'),
            ('x=1; \ny=2 \n', 'x = 1\ny = 2\n'),
        ]
        for src, expected in testdata:
            self._assert_format(src, expected)

    def test_perfect_code(self):
        testdata = [
            ('x = 123\n', 'x = 123\n'),
            ('x = 1\ny = 2\n', 'x = 1\ny = 2\n'),
        ]
        for src, expected in testdata:
            self._assert_format(src, expected)

    def _assert_format(self, src, expected):
        new_block = blackutil.fix_code(src, os.getcwd())
        self.assertEqual(new_block, expected)

    def test_should_read_options_from_pyproject_toml(self):
        with open('pyproject.toml', 'w') as f:
            f.write('[tool.black]\nline-length = 10')

        self.addCleanup(os.remove, 'pyproject.toml')

        testdata = [('x=       123\n', 'x = 123\n'),
                    ('x=1; \ny=2 \n', 'x = 1\ny = 2\n'),
                    ('x, y, z, a, b, c = 123, 124, 125, 126, 127, 128',
                     '(\n    x,\n    y,\n    z,\n    a,\n    b,\n    c,\n)'
                     ' = (\n    123,\n    124,\n    125,'
                     '\n    126,\n    127,\n    128,\n)\n')]
        for src, expected in testdata:
            self._assert_format(src, expected)
