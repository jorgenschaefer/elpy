"""Tests for the elpy.jedibackend module."""

import unittest

import jedi
import mock

from elpy import jedibackend
from elpy import rpc
from elpy.tests import compat
from elpy.tests.support import BackendTestCase, source_and_offset


class JediBackendTestCase(BackendTestCase):
    def setUp(self):
        super(JediBackendTestCase, self).setUp()
        self.backend = jedibackend.JediBackend(self.project_root)


class TestInit(JediBackendTestCase):
    def test_should_have_jedi_as_name(self):
        self.assertEqual(self.backend.name, "jedi")


class TestGetCompletions(JediBackendTestCase):
    def test_should_complete_builtin(self):
        source, offset = source_and_offset("o_|_")
        self.assertEqual(
            sorted([cand['suffix'] for cand in
                    self.backend.rpc_get_completions("test.py",
                                                     source, offset)]),
            sorted(['SError', 'bject', 'ct', 'pen', 'r', 'rd',
                    'verflowError']))

    def test_should_find_with_trailing_text(self):
        source, offset = source_and_offset(
            "import threading\nthreading.T_|_mumble mumble")
        if compat.PYTHON3:
            expected = ["hread", "hreadError", "IMEOUT_MAX", "imer"]
        else:
            expected = ["hread", "hread", "hreadError", "imer"]

        got = [cand['suffix'] for cand in
               self.backend.rpc_get_completions("test.py", source, offset)]

        self.assertEqual(sorted(got), sorted(expected))

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_completions("doesnotexist.py",
                                         "", 0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_completions(None,
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
        proposals = self.backend.rpc_get_completions(file2,
                                                     source2,
                                                     offset)
        self.assertEqual(proposals,
                         [{'name': 'add',
                           'suffix': 'd',
                           'annotation': 'function',
                           'meta': 'function: add.Add.add'}])


class TestRPCGetCompletionDocstring(JediBackendTestCase):
    def test_should_return_docstring(self):
        source, offset = source_and_offset("import json\n"
                                           "json.J_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        completions.sort(key=lambda p: p["name"])
        prop = completions[0]
        self.assertEqual(prop["name"], "JSONDecoder")

        docs = self.backend.rpc_get_completion_docstring("JSONDecoder")

        self.assertIn("Simple JSON", docs)

    def test_should_return_none_if_unknown(self):
        docs = self.backend.rpc_get_completion_docstring("Foo")

        self.assertIsNone(docs)


class TestRPCGetCompletionLocation(JediBackendTestCase):
    def test_should_return_location(self):
        source, offset = source_and_offset("donaudampfschiff = 1\n"
                                           "donau_|_")
        filename = self.project_file("test.py", source)
        completions = self.backend.rpc_get_completions(filename,
                                                       source,
                                                       offset)
        prop = completions[0]
        self.assertEqual(prop["name"], "donaudampfschiff")

        loc = self.backend.rpc_get_completion_location("donaudampfschiff")

        self.assertEqual((filename, 1), loc)

    def test_should_return_none_if_unknown(self):
        docs = self.backend.rpc_get_completion_location("Foo")

        self.assertIsNone(docs)


class TestGetDefinition(JediBackendTestCase):
    def test_should_return_definition_location_same_file(self):
        source, offset = source_and_offset("import threading\n"
                                           "def test_function(a, b):\n"
                                           "    return a + b\n"
                                           "\n"
                                           "test_func_|_tion(\n")
        filename = self.project_file("test.py", source)
        self.assertEqual(self.backend.rpc_get_definition(filename,
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
            self.backend.rpc_get_definition(self.project_root +
                                            "/doesnotexist.py",
                                            source,
                                            offset))

    def test_should_return_none_if_not_found(self):
        source, offset = source_and_offset(
            "fo_|_o()\n")
        filename = self.project_file("test.py", source)
        self.assertIsNone(
            self.backend.rpc_get_definition(filename,
                                            source,
                                            offset))

    def test_should_return_definition_location_different_file(self):
        source1 = ("def test_function(a, b):\n"
                   "    return a + b\n")
        file1 = self.project_file("test1.py", source1)
        source2, offset = source_and_offset("from test1 import test_function\n"
                                            "test_function_|_(1, 2)\n")
        file2 = self.project_file("test2.py", source2)
        location = self.backend.rpc_get_definition(file2,
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
        location = self.backend.rpc_get_definition(file2,
                                                   source2,
                                                   offset)
        self.assertEqual(location, (file1, 0))

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_definition("doesnotexist.py",
                                        "open", 0)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_definition(None,
                                        "open", 0)

    def test_should_find_variable_definition(self):
        source, offset = source_and_offset("SOME_VALUE = 1\n"
                                           "\n"
                                           "variable = _|_SOME_VALUE\n")
        filename = self.project_file("test.py", source)
        self.assertEqual(self.backend.rpc_get_definition(filename,
                                                         source,
                                                         offset),
                         (filename, 0))


class TestGetCalltip(JediBackendTestCase):
    def test_should_return_calltip(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset("import threading\n"
                                           "threading.Thread(_|_")
        calltip = self.backend.rpc_get_calltip(filename,
                                               source, offset)
        if compat.PYTHON3:
            self.assertEqual(calltip,
                             {"name": "Thread",
                              "params": ["group = None",
                                         "target = None",
                                         "name = None",
                                         "args = ()",
                                         "kwargs = None",
                                         "daemon = None"],
                              "index": 0})
        else:
            self.assertEqual(calltip,
                             {"name": "Thread",
                              "params": ["group = None",
                                         "target = None",
                                         "name = None",
                                         "args = ()",
                                         "kwargs = None",
                                         "verbose = None"],
                              "index": 0})

    def test_should_return_none_outside_of_all(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset("import thr_|_eading\n")
        calltip = self.backend.rpc_get_calltip(filename,
                                               source, offset)
        self.assertIsNone(calltip)

    def test_should_not_fail_on_inexisting_file(self):
        self.backend.rpc_get_calltip("doesnotexist.py",
                                     "open(", 5)

    def test_should_not_fail_if_file_is_none(self):
        self.backend.rpc_get_calltip(None,
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
        calltip = self.backend.rpc_get_calltip(file2,
                                               source2,
                                               offset)
        self.assertEqual(calltip,
                         {'name': u'add',
                          'index': 0,
                          'params': [u'a', u'b']})


class TestGetUsages(JediBackendTestCase):
    def test_should_return_uses_in_same_file(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset(
            "def foo(x):\n"
            "    return _|_x + x\n")

        usages = self.backend.rpc_get_usages(filename,
                                             source,
                                             offset)

        self.assertEqual(usages,
                         [{'name': 'x',
                           'offset': 8,
                           'filename': filename},
                          {'name': 'x',
                           'filename': filename,
                           'offset': 23},
                          {'name': u'x',
                           'filename': filename,
                           'offset': 27}])

    def test_should_return_uses_in_other_file(self):
        file1 = self.project_file("file1.py", "")
        file2 = self.project_file("file2.py", "\n\n\n\n\nx = 5")
        source, offset = source_and_offset(
            "import file2\n"
            "file2._|_x\n")

        usages = self.backend.rpc_get_usages(file1,
                                             source,
                                             offset)

        self.assertEqual(usages,
                         [{'name': 'x',
                           'filename': file1,
                           'offset': 19},
                          {'name': 'x',
                           'filename': file2,
                           'offset': 5}])

    def test_should_not_fail_without_symbol(self):
        filename = self.project_file("file.py", "")

        usages = self.backend.rpc_get_usages(filename,
                                             "",
                                             0)

        self.assertEqual(usages, [])


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


class TestRunWithDebug(unittest.TestCase):
    @mock.patch('jedi.Script')
    def test_should_call_method(self, Script):
        Script.return_value.test_method.return_value = "test-result"

        result = jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3)

        Script.assert_called_with(1, 2, arg=3)
        self.assertEqual(result, 'test-result')

    @mock.patch('jedi.Script')
    def test_should_re_raise(self, Script):
        Script.side_effect = RuntimeError

        with self.assertRaises(RuntimeError):
            jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3,
                                       re_raise=(RuntimeError,))

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_keep_debug_info(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3)
        except rpc.Fault as e:
            self.assertGreaterEqual(e.code, 400)
            self.assertIsNotNone(e.data)
            self.assertIn("traceback", e.data)
            jedi_debug_info = e.data["jedi_debug_info"]
            self.assertIsNotNone(jedi_debug_info)
            self.assertEqual(jedi_debug_info["script_args"],
                             "1, 2, arg=3")
            self.assertEqual(jedi_debug_info["source"], None)
            self.assertEqual(jedi_debug_info["method"], "test_method")
            self.assertEqual(jedi_debug_info["debug_info"], [])

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_keep_error_text(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3)
        except rpc.Fault as e:
            self.assertEqual(str(e), str(RuntimeError()))
            self.assertEqual(e.message, str(RuntimeError()))

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_handle_source_special(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', source="foo")
        except rpc.Fault as e:
            self.assertEqual(e.data["jedi_debug_info"]["script_args"],
                             "source=source")
            self.assertEqual(e.data["jedi_debug_info"]["source"], "foo")

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_set_debug_info(self, set_debug_function, Script):
        the_debug_function = [None]

        def my_set_debug_function(debug_function, **kwargs):
            the_debug_function[0] = debug_function

        def my_script(*args, **kwargs):
            the_debug_function[0](jedi.debug.NOTICE, "Notice")
            the_debug_function[0](jedi.debug.WARNING, "Warning")
            the_debug_function[0]("other", "Other")
            raise RuntimeError

        set_debug_function.side_effect = my_set_debug_function
        Script.return_value.test_method = my_script

        try:
            jedibackend.run_with_debug(jedi, 'test_method', source="foo")
        except rpc.Fault as e:
            self.assertEqual(e.data["jedi_debug_info"]["debug_info"],
                             ["[N] Notice",
                              "[W] Warning",
                              "[?] Other"])
