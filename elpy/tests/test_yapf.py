# coding: utf-8
"""Tests for the elpy.yapf module"""

import sys
import unittest

from elpy import yapfutil
from elpy.rpc import Fault
from elpy.tests.support import BackendTestCase


@unittest.skipIf(yapfutil.YAPF_NOT_SUPPORTED,
                 'yapf not supported for current python version')
class YAPFTestCase(BackendTestCase):
    def setUp(self):
        if not yapfutil.YAPF_NOT_SUPPORTED:
            raise unittest.SkipTest

    def test_fix_code_should_throw_error_for_invalid_code(self):
        src = 'x = '
        self.assertRaises(Fault, yapfutil.fix_code, src)

    def test_fix_code(self):
        testdata = [
            ('x=       123\n', 'x = 123\n'),
            ('x=1; \ny=2 \n', 'x = 1\ny = 2\n'),
        ]
        for src, expected in testdata:
            self._assert_format(src, expected)

    def _assert_format(self, src, expected):
        new_block = yapfutil.fix_code(src)
        self.assertEqual(new_block, expected)
