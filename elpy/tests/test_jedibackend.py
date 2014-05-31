"""Tests for the elpy.backends.jedibackend module."""

import unittest

import jedi
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
        self.assertEqual(
            sorted([cand['suffix'] for cand in
                    self.backend.rpc_get_completions(None, "test.py",
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
               self.backend.rpc_get_completions(None, "test.py",
                                                source, offset)]

        self.assertEqual(sorted(got), sorted(expected))

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
        self.assertEqual(proposals,
                         [{'suffix': 'd',
                           'docstring': 'add(self, a, b)\n\n',
                           'annotation': 'function',
                           'meta': 'function: add.Add.add'}])

    @mock.patch('elpy.backends.jedibackend.get_source')
    def test_should_call_get_source(self, get_source):
        get_source.return_value = "test-source"

        self.backend.rpc_get_completions(self.project_root, None,
                                         "test-source", 0)

        get_source.assert_called_with("test-source")


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

    def test_should_find_variable_definition(self):
        source, offset = source_and_offset("SOME_VALUE = 1\n"
                                           "\n"
                                           "variable = _|_SOME_VALUE\n")
        filename = self.project_file("test.py", source)
        self.assertEqual(self.backend.rpc_get_definition(self.project_root,
                                                         filename,
                                                         source,
                                                         offset),
                         (filename, 0))

    @mock.patch('elpy.backends.jedibackend.get_source')
    def test_should_call_get_source(self, get_source):
        get_source.return_value = "test-source"

        self.backend.rpc_get_definition(self.project_root, None,
                                        "test-source", 0)

        get_source.assert_called_with("test-source")


class TestGetCalltip(JediBackendTestCase):
    def test_should_return_calltip(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset("import threading\n"
                                           "threading.Thread(_|_")
        calltip = self.backend.rpc_get_calltip(self.project_root,
                                               filename,
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
        self.assertEqual(calltip,
                         {'name': u'add',
                          'index': 0,
                          'params': [u'a', u'b']})

    @mock.patch('elpy.backends.jedibackend.get_source')
    def test_should_call_get_source(self, get_source):
        get_source.return_value = "test-source"

        self.backend.rpc_get_calltip(self.project_root, None,
                                     "test-source", 0)

        get_source.assert_called_with("test-source")


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

    @mock.patch('elpy.backends.jedibackend.get_source')
    def test_should_call_get_source(self, get_source):
        get_source.return_value = "test-source"

        self.backend.rpc_get_docstring(self.project_root, None,
                                       "test-source", 0)

        get_source.assert_called_with("test-source")


class TestGetUsages(JediBackendTestCase):
    def test_should_return_uses_in_same_file(self):
        filename = self.project_file("test.py", "")
        source, offset = source_and_offset(
            "def foo(x):\n"
            "    return _|_x + x\n")

        usages = self.backend.rpc_get_usages(self.project_root,
                                             filename,
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
        file2 = self.project_file("file2.py", "x = 5")
        source, offset = source_and_offset(
            "import file2\n"
            "file2._|_x\n")

        usages = self.backend.rpc_get_usages(self.project_root,
                                             file1,
                                             source,
                                             offset)

        self.assertEqual(usages,
                         [{'name': 'x',
                           'filename': file1,
                           'offset': 19},
                          {'name': 'x',
                           'filename': file2,
                           'offset': 0}])


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
    @mock.patch('jedi.set_debug_function')
    def test_should_keep_debug_info(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3)
        except RuntimeError as e:
            self.assertIsNotNone(e.jedi_debug_info)
            self.assertEqual(e.jedi_debug_info["script_args"],
                             "1, 2, arg=3")
            self.assertEqual(e.jedi_debug_info["source"], None)
            self.assertEqual(e.jedi_debug_info["method"], "test_method")
            self.assertEqual(e.jedi_debug_info["debug_info"], [])

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_handle_source_special(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', source="foo")
        except RuntimeError as e:
            self.assertEqual(e.jedi_debug_info["script_args"],
                             "source=source")
            self.assertEqual(e.jedi_debug_info["source"], "foo")

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
        except RuntimeError as e:
            self.assertEqual(e.jedi_debug_info["debug_info"],
                             ["[N] Notice",
                              "[W] Warning",
                              "[?] Other"])
