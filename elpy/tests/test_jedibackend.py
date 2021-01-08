"""Tests for the elpy.jedibackend module."""

import sys
import unittest
from pathlib import Path

import jedi
import mock
import re

from elpy import jedibackend
from elpy.jedibackend import SourceCode
from elpy import rpc
from elpy.tests import compat
from elpy.tests.support import BackendTestCase
from elpy.tests.support import RPCGetCompletionsTests
from elpy.tests.support import RPCGetCompletionDocstringTests
from elpy.tests.support import RPCGetCompletionLocationTests
from elpy.tests.support import RPCGetDocstringTests
from elpy.tests.support import RPCGetOnelineDocstringTests
from elpy.tests.support import RPCGetDefinitionTests
from elpy.tests.support import RPCGetCalltipTests
from elpy.tests.support import RPCGetUsagesTests
from elpy.tests.support import RPCGetNamesTests
from elpy.tests.support import RPCGetRenameDiffTests
from elpy.tests.support import RPCGetExtractFunctionDiffTests
from elpy.tests.support import RPCGetExtractVariableDiffTests
from elpy.tests.support import RPCGetInlineDiffTests
from elpy.tests.support import RPCGetAssignmentTests
from elpy.tests.support import source_and_offset

class JediBackendTestCase(BackendTestCase):
    def setUp(self):
        super(JediBackendTestCase, self).setUp()
        env = jedi.get_default_environment().path
        self.backend = jedibackend.JediBackend(self.project_root, env)

class TestSourceCode(BackendTestCase):
    def setUp(self):
        super().setUp()
        self.src = SourceCode(path="/notreal/path", source="\n1234\n6789")

    def test_should_build_offset_index(self):
        self.assertEqual(self.src._get_line_offsets(), [0, 1, 6, 10])

    def test_should_return_offset(self):
        self.assertEqual(self.src.get_offset(1, 0), 0)
        self.assertEqual(self.src.get_offset(2, 0), 1)

    def test_should_return_pos(self):
        self.assertEqual(self.src.get_pos(0), (1, 0))
        self.assertEqual(self.src.get_pos(1), (2, 0))
        self.assertEqual(self.src.get_pos(5), (2, 4))
        self.assertEqual(self.src.get_pos(9), (3, 3))

    def test_should_return_source_string(self):
        self.assertEqual(str(self.src),  "\n1234\n6789")

    def test_should_return_source_path(self):
        self.assertEqual(self.src.path, Path("/notreal/path"))

    def test_should_load_source_if_not_source_arg(self):
        file_name = self.project_file("test.py", "my\nsource\ncode")
        src = SourceCode(file_name)
        self.assertEqual("my\nsource\ncode", str(src))

    def test_should_not_load_source_if_source_in_args(self):
        file_name = self.project_file("test.py", "my\nsource\nin\file")
        not_saved_source_string = "edited\nbut\not\nsaved\source"
        src = SourceCode(file_name, not_saved_source_string)
        self.assertEqual(not_saved_source_string, str(src))
        
    def test_should_fail_on_wrong_pos(self):
        with self.assertRaises(ValueError) as cm:
            self.src.get_offset(100, 1)
        self.assertEqual("Text does not have 100 lines.", str(cm.exception))
        with self.assertRaises(ValueError) as cm:
            self.src.get_offset(1, 1)
        self.assertEqual( "Line 1 column 1 is not within the text",
                         str(cm.exception))


class TestInit(JediBackendTestCase):
    def test_should_have_jedi_as_name(self):
        self.assertEqual(self.backend.name, "jedi")


class TestRPCGetCompletions(RPCGetCompletionsTests,
                            JediBackendTestCase):
    BUILTINS = ['object', 'oct', 'open', 'ord', 'OSError', 'OverflowError']


class TestRPCGetAssignment(RPCGetAssignmentTests,
                           JediBackendTestCase):
    pass


class TestRPCGetCompletionDocstring(RPCGetCompletionDocstringTests,
                                    JediBackendTestCase):
    pass


class TestRPCGetCompletionLocation(RPCGetCompletionLocationTests,
                                   JediBackendTestCase):
    pass


class TestRPCGetDocstring(RPCGetDocstringTests,
                          JediBackendTestCase):

    def __init__(self, *args, **kwargs):
        super(TestRPCGetDocstring, self).__init__(*args, **kwargs)
        self.JSON_LOADS_REGEX = (
            r'loads\(s.*, encoding.*, cls.*, object_hook.*, parse_float.*, '
            r'parse_int.*, .*\)'
            )

    def check_docstring(self, docstring):
        lines = docstring.splitlines()
        self.assertEqual(lines[0], 'Documentation for json.loads:')
        match = re.match(self.JSON_LOADS_REGEX, lines[2])
        self.assertIsNotNone(match)

    @mock.patch("elpy.jedibackend.run_with_debug")
    def test_should_not_return_empty_docstring(self, run_with_debug):
        location = mock.MagicMock()
        location.full_name = "testthing"
        location.docstring.return_value = ""
        run_with_debug.return_value = [location]
        filename = self.project_file("test.py", "print")
        docstring = self.backend.rpc_get_docstring(filename, "print", 0)
        self.assertIsNone(docstring)


class TestRPCGetOnelineDocstring(RPCGetOnelineDocstringTests,
                                 JediBackendTestCase):

    def __init__(self, *args, **kwargs):
        super(TestRPCGetOnelineDocstring, self).__init__(*args, **kwargs)
        if sys.version_info >= (3, 6):
            self.JSON_LOADS_DOCSTRING = (
                'Deserialize ``s`` (a ``str``, ``bytes`` or'
                ' ``bytearray`` instance containing a JSON'
                ' document) to a Python object.'
            )
            self.JSON_DOCSTRING = (
                "JSON (JavaScript Object Notation) <http://json.org>"
                " is a subset of JavaScript syntax (ECMA-262"
                " 3rd edition) used as a lightweight data interchange format.")
        elif sys.version_info >= (3, 0):
            self.JSON_LOADS_DOCSTRING = (
                'Deserialize ``s`` (a ``str`` instance '
                'containing a JSON document) to a Python object.'
            )
            self.JSON_DOCSTRING = (
                "JSON (JavaScript Object Notation) <http://json.org>"
                " is a subset of JavaScript syntax (ECMA-262"
                " 3rd edition) used as a lightweight data interchange format.")
        else:
            self.JSON_LOADS_DOCSTRING = (
                'Deserialize ``s`` (a ``str`` or ``unicode`` '
                'instance containing a JSON document) to a Python object.'
            )
            self.JSON_DOCSTRING = (
                "JSON (JavaScript Object Notation) <http://json.org>"
                " is a subset of JavaScript syntax (ECMA-262"
                " 3rd edition) used as a lightweight data interchange format.")

    @mock.patch("elpy.jedibackend.run_with_debug")
    def test_should_not_return_empty_docstring(self, run_with_debug):
        location = mock.MagicMock()
        location.full_name = "testthing"
        location.docstring.return_value = ""
        run_with_debug.return_value = [location]
        filename = self.project_file("test.py", "print")
        docstring = self.backend.rpc_get_oneline_docstring(filename, "print", 0)
        self.assertIsNone(docstring)


class TestRPCGetDefinition(RPCGetDefinitionTests,
                           JediBackendTestCase):
    @mock.patch("jedi.Script")
    def test_should_not_fail_if_module_path_is_none(self, Script):
        """Do not fail if loc.module_path is None.

        This can happen under some circumstances I am unsure about.
        See #537 for the issue that reported this.

        """
        locations = [
            mock.Mock(module_path=None)
        ]
        script = Script.return_value
        script.goto_definitions.return_value = locations
        script.goto_assignments.return_value = locations

        location = self.rpc("", "", 0)

        self.assertIsNone(location)


class TestRPCGetRenameDiff(RPCGetRenameDiffTests,
                           JediBackendTestCase):
    pass


class TestRPCGetExtractFunctionDiff(RPCGetExtractFunctionDiffTests,
                                    JediBackendTestCase):
    pass


class TestRPCGetExtractVariableDiff(RPCGetExtractVariableDiffTests,
                                    JediBackendTestCase):
    pass


class TestRPCGetInlineDiff(RPCGetInlineDiffTests,
                           JediBackendTestCase):
    pass


class TestRPCGetCalltip(RPCGetCalltipTests,
                        JediBackendTestCase):
    KEYS_CALLTIP = {'index': None,
                    'params': [],
                    'name': u'keys'}
    RADIX_CALLTIP = {'index': None,
                     'params': [],
                     'name': u'radix'}
    ADD_CALLTIP = {'index': 0,
                   'params': [u'a', u'b'],
                   'name': u'add'}
    if compat.PYTHON3:
        THREAD_CALLTIP = {'name': 'Thread',
                          'index': 0,
                          'params': ['group: None=...',
                                     'target: Optional[Callable[..., Any]]=...',
                                     'name: Optional[str]=...',
                                     'args: Iterable[Any]=...',
                                     'kwargs: Mapping[str, Any]=...',
                                     'daemon: Optional[bool]=...']}

    else:
        THREAD_CALLTIP = {'index': 0,
                          'name': u'Thread',
                          'params': [u'group: None=...',
                                     u'target: Optional[Callable[..., Any]]=...',
                                     u'name: Optional[str]=...',
                                     u'args: Iterable[Any]=...',
                                     u'kwargs: Mapping[str, Any]=...']}

    def test_should_not_fail_with_get_subscope_by_name(self):
        # Bug #677 / jedi#628
        source = (
            u"my_lambda = lambda x: x+1\n"
            u"my_lambda(1)"
        )
        filename = self.project_file("project.py", source)
        offset = 37

        sigs = self.backend.rpc_get_calltip(filename, source, offset)
        sigs["index"]


class TestRPCGetUsages(RPCGetUsagesTests,
                       JediBackendTestCase):
    def test_should_not_fail_for_missing_module(self):
        # This causes use.module_path to be None
        source = "import sys\n\nsys.path.\n"  # insert()"
        offset = 21
        filename = self.project_file("project.py", source)

        self.rpc(filename, source, offset)


class TestRPCGetNames(RPCGetNamesTests,
                      JediBackendTestCase):
    pass


class TestRunWithDebug(unittest.TestCase):
    @mock.patch('jedi.Script')
    def test_should_call_method(self, Script):
        Script.return_value.test_method.return_value = "test-result"

        result = jedibackend.run_with_debug(jedi, 'test_method', {}, 1, 2,
                                            arg=3)

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
            jedibackend.run_with_debug(jedi, 'test_method', {}, 1, 2, arg=3)
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
        else:
            self.fail("Fault not thrown")

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_keep_error_text(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', {}, 1, 2, arg=3)
        except rpc.Fault as e:
            self.assertEqual(str(e), str(RuntimeError()))
            self.assertEqual(e.message, str(RuntimeError()))
        else:
            self.fail("Fault not thrown")

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
        else:
            self.fail("Fault not thrown")

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
        else:
            self.fail("Fault not thrown")

    @mock.patch('jedi.set_debug_function')
    @mock.patch('jedi.Script')
    def test_should_not_fail_with_bad_data(self, Script, set_debug_function):
        import jedi.debug

        def set_debug(function, speed=True):
            if function is not None:
                function(jedi.debug.NOTICE, u"\xab")

        set_debug_function.side_effect = set_debug
        Script.return_value.test_method.side_effect = Exception

        with self.assertRaises(rpc.Fault):
            jedibackend.run_with_debug(jedi, 'test_method', {}, 1, 2, arg=3)
