"""Tests for the elpy.backends.jedibackend module."""

import unittest

import mock

from elpy.tests import compat
from elpy.backends import jedibackend
from elpy.tests.support import BackendTestCase, source_and_offset


class JediBackendTestCase(BackendTestCase):
    def setUp(self):
        super(JediBackendTestCase, self).setUp()
        self.backend = jedibackend.JediBackend()


class TestInit(JediBackendTestCase):
    def test_should_have_jedi_as_name(self):
        self.assertEqual(self.backend.name, "jedi")

    def test_should_return_object_if_jedi_available(self):
        self.assertIsNotNone(jedibackend.JediBackend())

    @mock.patch.object(compat.builtins, '__import__')
    def test_should_return_none_if_no_rope(self, import_):
        import_.side_effect = ImportError
        self.assertIsNone(jedibackend.JediBackend())


class TestGetCompletions(JediBackendTestCase):
    def test_should_complete_builtin(self):
        source, offset = source_and_offset("o_|_")
        self.assertEqual(sorted([name for (name, doc) in
                                 self.backend.rpc_get_completions(
                                     None, "test.py", source, offset)]),
                         sorted(['SError', 'bject', 'ct', 'pen', 'r',
                                 'rd', 'verflowError']))

    def test_should_find_with_trailing_text(self):
        source, offset = source_and_offset(
            "import threading\nthreading.T_|_mumble mumble")
        if compat.PYTHON3:
            expected = ["hread", "hreadError", "IMEOUT_MAX", "imer"]
        else:
            expected = ["hread", "hread", "hreadError", "imer"]
        self.assertEqual(sorted([name for (name, doc) in
                                 self.backend.rpc_get_completions(
                                     None, "test.py", source, offset)]),
                         sorted(expected))

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_completions(self.project_root,
                                         "doesnotexist.py",
                                         "", 0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_completions(self.project_root,
                                         None,
                                         "open", 0)

    def test_should_find_completion_different_package(self):
        # See issue #74
        self.project_file("project/__init__.py", "")
        source1 = ("class Add:\n"
                   "    def add(self, a, b):\n"
                   "        return a + b\n")
        self.project_file("project/add.py", source1)
        source2, offset = source_and_offset(
            "from project.add import Add\n"
            "class Calculator:\n"
            "    def add(self, a, b):\n"
            "        c = Add()\n"
            "        c.ad_|_\n")
        file2 = self.project_file("project/calculator.py", source2)
        proposals = self.backend.rpc_get_completions(self.project_root,
                                                     file2,
                                                     source2,
                                                     offset)
        self.assertEqual(proposals, [['d', 'add(self, a, b)\n\n']])


class TestGetDefinition(JediBackendTestCase):
    def test_should_return_definition_location_same_file(self):
        source, offset = source_and_offset("import threading\n"
                                           "def test_function(a, b):\n"
                                           "    return a + b\n"
                                           "\n"
                                           "test_func_|_tion(\n")
        filename = self.project_file("test.py", source)
        self.assertEqual(self.backend.rpc_get_definition(self.project_root,
                                                         filename,
                                                         source,
                                                         offset),
                         (filename, 17))

    def test_should_return_none_if_file_does_not_exist(self):
        source, offset = source_and_offset(
            "def foo():\n"
            "    pass\n"
            "\n"
            "fo_|_o()\n")
        self.assertIsNone(
            self.backend.rpc_get_definition(self.project_root,
                                            self.project_root +
                                            "/doesnotexist.py",
                                            source,
                                            offset))

    def test_should_return_none_if_not_found(self):
        source, offset = source_and_offset(
            "fo_|_o()\n")
        filename = self.project_file("test.py", source)
        self.assertIsNone(
            self.backend.rpc_get_definition(self.project_root,
                                            filename,
                                            source,
                                            offset))

    def test_should_return_definition_location_different_file(self):
        source1 = ("def test_function(a, b):\n"
                   "    return a + b\n")
        file1 = self.project_file("test1.py", source1)
        source2, offset = source_and_offset("from test1 import test_function\n"
                                            "test_function_|_(1, 2)\n")
        file2 = self.project_file("test2.py", source2)
        location = self.backend.rpc_get_definition(self.project_root,
                                                   file2,
                                                   source2,
                                                   offset)
        self.assertEqual(location, (file1, 0))

    def test_should_return_definition_location_different_package(self):
        # See issue #74
        self.project_file("project/__init__.py", "")
        source1 = ("class Add:\n"
                   "    def add(self, a, b):\n"
                   "        return a + b\n")
        file1 = self.project_file("project/add.py", source1)
        source2, offset = source_and_offset(
            "from project.add import Add\n"
            "class Calculator:\n"
            "    def add(self, a, b):\n"
            "        return Add_|_().add(a, b)\n")
        file2 = self.project_file("project/calculator.py", source2)
        location = self.backend.rpc_get_definition(self.project_root,
                                                   file2,
                                                   source2,
                                                   offset)
        self.assertEqual(location, (file1, 0))

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_definition(self.project_root,
                                        "doesnotexist.py",
                                        "open", 0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_definition(self.project_root,
                                        None,
                                        "open", 0)


class TestGetCalltip(JediBackendTestCase):
    def test_should_return_calltip(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset("import threading\n"
                                           "threading.Thread(_|_")
        calltip = self.backend.rpc_get_calltip(self.project_root,
                                               filename,
                                               source, offset)
        if compat.PYTHON3:
            self.assertEqual(calltip.replace(" = ", "="),
                             "Thread(group=None, target=None, name=None, "
                             "args=(), kwargs=None, daemon=None)")
        else:
            self.assertEqual(calltip.replace(" = ", "="),
                             "Thread(group=None, target=None, name=None, "
                             "args=(), kwargs=None, verbose=None)")

    def test_should_return_none_outside_of_all(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset("import thr_|_eading\n")
        calltip = self.backend.rpc_get_calltip(self.project_root,
                                               filename,
                                               source, offset)
        self.assertIsNone(calltip)

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_calltip(self.project_root,
                                     "doesnotexist.py",
                                     "open(", 5)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_calltip(self.project_root,
                                     None,
                                     "open", 0)

    def test_should_find_calltip_different_package(self):
        # See issue #74
        self.project_file("project/__init__.py", "")
        source1 = ("class Add:\n"
                   "    def add(self, a, b):\n"
                   "        return a + b\n")
        self.project_file("project/add.py", source1)
        source2, offset = source_and_offset(
            "from project.add import Add\n"
            "class Calculator:\n"
            "    def add(self, a, b):\n"
            "        c = Add()\n"
            "        c.add(_|_\n")
        file2 = self.project_file("project/calculator.py", source2)
        calltip = self.backend.rpc_get_calltip(self.project_root,
                                                 file2,
                                                 source2,
                                                 offset)
        self.assertEqual(calltip, 'add(a, b)')


class TestGetDocstring(JediBackendTestCase):
    def test_should_get_docstring(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset(
            "import threading\nthreading.Thread.join_|_(")
        docstring = self.backend.rpc_get_docstring(self.project_root,
                                                   filename,
                                                   source,
                                                   offset)

        import pydoc
        wanted = pydoc.render_doc("threading.Thread.join",
                                  "Elpy Pydoc Documentation for %s",
                                  False)
        self.assertEqual(docstring, wanted)

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_docstring(self.project_root,
                                       "doesnotexist.py",
                                       "open", 0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_docstring(self.project_root,
                                       None,
                                       "open", 0)


class TestPosToLinecol(unittest.TestCase):
    def test_should_handle_beginning_of_string(self):
        self.assertEqual(jedibackend.pos_to_linecol("foo", 0),
                         (1, 0))

    def test_should_handle_end_of_line(self):
        self.assertEqual(jedibackend.pos_to_linecol("foo\nbar\nbaz\nqux", 9),
                         (3, 1))

    def test_should_handle_end_of_string(self):
        self.assertEqual(jedibackend.pos_to_linecol("foo\nbar\nbaz\nqux", 14),
                         (4, 2))


class TestLinecolToPos(unittest.TestCase):
    def test_should_handle_beginning_of_string(self):
        self.assertEqual(jedibackend.linecol_to_pos("foo", 1, 0),
                         0)

    def test_should_handle_end_of_string(self):
        self.assertEqual(jedibackend.linecol_to_pos("foo\nbar\nbaz\nqux",
                                                    3, 1),
                         9)

    def test_should_return_offset(self):
        self.assertEqual(jedibackend.linecol_to_pos("foo\nbar\nbaz\nqux",
                                                    4, 2),
                         14)

    def test_should_fail_for_line_past_text(self):
        self.assertRaises(ValueError,
                          jedibackend.linecol_to_pos, "foo\n", 3, 1)

    def test_should_fail_for_column_past_text(self):
        self.assertRaises(ValueError,
                          jedibackend.linecol_to_pos, "foo\n", 1, 10)
