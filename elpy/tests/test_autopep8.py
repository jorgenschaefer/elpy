# coding: utf-8

"""Tests for the elpy.autopep module"""

import unittest

from elpy import autopep
from elpy.tests.support import BackendTestCase


class Autopep8TestCase(BackendTestCase):

    def setUp(self):
        if not autopep.autopep8:
            raise unittest.SkipTest

    def test_fix_code(self):
        code_block = 'x=       123\n'
        new_block = autopep.fix_code(code_block)
        self.assertEqual(new_block, 'x = 123\n')
