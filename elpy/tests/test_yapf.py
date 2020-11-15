# coding: utf-8
"""Tests for the elpy.yapf module"""

import unittest
import os

from elpy import yapfutil
from elpy.rpc import Fault
from elpy.tests.support import BackendTestCase


@unittest.skipIf(yapfutil.YAPF_NOT_SUPPORTED,
                 'yapf not supported for current python version')
class YAPFTestCase(BackendTestCase):
    def setUp(self):
        if yapfutil.YAPF_NOT_SUPPORTED:
            raise unittest.SkipTest

    def test_fix_code_should_throw_error_for_invalid_code(self):
        src = 'x = '
        self.assertRaises(Fault, yapfutil.fix_code, src, os.getcwd())

    def test_fix_code_should_throw_error_without_yapf_installed(self):
        yapf = yapfutil.yapf_api
        yapfutil.yapf_api = None
        src = 'x=       123\n', 'x = 123\n'
        with self.assertRaises(Fault):
            yapfutil.fix_code(src, os.getcwd())
        yapfutil.yapf_api = yapf

    def test_fix_code(self):
        testdata = [
            ('x=       123\n', 'x = 123\n'),
            ('x=1; \ny=2 \n', 'x = 1\ny = 2\n'),
        ]
        for src, expected in testdata:
            self._assert_format(src, expected)

    def _assert_format(self, src, expected):
        new_block = yapfutil.fix_code(src, os.getcwd())
        self.assertEqual(new_block, expected)
