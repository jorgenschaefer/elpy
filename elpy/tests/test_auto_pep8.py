# coding: utf-8

"""Tests for the elpy.autopep8 module"""

import unittest
import os

from elpy import auto_pep8
from elpy.tests.support import BackendTestCase


class Autopep8TestCase(BackendTestCase):

    def setUp(self):
        if not auto_pep8.autopep8:
            raise unittest.SkipTest

    def test_fix_code(self):
        code_block = 'x=       123\n'
        new_block = auto_pep8.fix_code(code_block, os.getcwd())
        self.assertEqual(new_block, 'x = 123\n')
