"""Tests for the elpy.jedibackend module."""

import sys
import unittest

import jedi
import mock

from elpy import jedibackend
from elpy import rpc
from elpy.tests import compat
from elpy.tests.support import BackendTestCase
from elpy.tests.support import RPCGetCompletionsTests
from elpy.tests.support import RPCGetCompletionDocstringTests
from elpy.tests.support import RPCGetCompletionLocationTests
from elpy.tests.support import RPCGetDocstringTests
from elpy.tests.support import RPCGetOnelineDocstringTests
from elpy.tests.support import RPCGetDefinitionTests
from elpy.tests.support import RPCGetAssignmentTests
from elpy.tests.support import RPCGetCalltipTests
from elpy.tests.support import RPCGetUsagesTests
from elpy.tests.support import RPCGetNamesTests


class JediBackendTestCase(BackendTestCase):
    def setUp(self):
        super(JediBackendTestCase, self).setUp()
        env = jedi.get_default_environment().path
        self.backend = jedibackend.JediBackend(self.project_root, env)


class TestInit(JediBackendTestCase):
    def test_should_have_jedi_as_name(self):
        self.assertEqual(self.backend.name, "jedi")


class TestRPCGetCompletions(RPCGetCompletionsTests,
                            JediBackendTestCase):
    BUILTINS = ['object', 'oct', 'open', 'ord', 'OSError', 'OverflowError']


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
        if sys.version_info >= (3, 6):
            self.JSON_LOADS_DOCSTRING = (
                'loads(s, *, encoding=None, cls=None, object_hook=None, '
                'parse_float=None, parse_int=None, parse_constant=None, '
                'object_pairs_hook=None, object_hook: '
                'Optional[Callable[[Dict[str, Any]], Any]]=..., '
                'parse_float: Optional[Callable[[str], Any]]=..., '
                'parse_int: Optional[Callable[[str], Any]]=..., '
                'parse_constant: Optional[Callable[[str], Any]]=..., '
                'strict: bool=..., '
                'object_pairs_hook: Optional[Callable[[List[Tuple[str, Any]]], '
                'Any]]=...)'
            )
        elif sys.version_info >= (3, 5):
            self.JSON_LOADS_DOCSTRING = (
                'loads(s, encoding=None, cls=None, object_hook=None, '
                'parse_float=None, parse_int=None, parse_constant=None, '
                'object_pairs_hook=None, *, object_hook: '
                'Optional[Callable[[Dict[str, Any]], Any]]=..., '
                'parse_float: Optional[Callable[[str], Any]]=..., '
                'parse_int: Optional[Callable[[str], Any]]=..., '
                'parse_constant: Optional[Callable[[str], Any]]=..., '
                'strict: bool=..., '
                'object_pairs_hook: Optional[Callable[[List[Tuple[str, Any]]], '
                'Any]]=...)'
            )
        else:
            self.JSON_LOADS_DOCSTRING = (
                'loads(s, encoding=None, cls=None, object_hook=None, '
                'parse_float=None, parse_int=None, parse_constant=None, '
                'object_pairs_hook=None, **kw)'
            )

    def check_docstring(self, docstring):
        lines = docstring.splitlines()
        self.assertEqual(lines[0], 'Documentation for json.loads:')
        self.assertEqual(lines[2], self.JSON_LOADS_DOCSTRING)

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


class TestRPCGetAssignment(RPCGetAssignmentTests,
                           JediBackendTestCase):
    @mock.patch("jedi.Script")
    def test_should_not_fail_if_module_path_is_none(self, Script):
        """Do not fail if loc.module_path is None.

        """
        locations = [
            mock.Mock(module_path=None)
        ]
        script = Script.return_value
        script.goto_assignments.return_value = locations
        script.goto_assignments.return_value = locations

        location = self.rpc("", "", 0)

        self.assertIsNone(location)


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
                                     'args: Iterable=...',
                                     'kwargs: Mapping[str, Any]=...',
                                     'daemon: Optional[bool]=...']}

    else:
        THREAD_CALLTIP = {'index': 0,
                          'name': u'Thread',
                          'params': [u'group: None=...',
                                     u'target: Optional[Callable[..., Any]]=...',
                                     u'name: Optional[str]=...',
                                     u'args: Iterable=...',
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
        else:
            self.fail("Fault not thrown")

    @mock.patch('jedi.Script')
    @mock.patch('jedi.set_debug_function')
    def test_should_keep_error_text(self, set_debug_function, Script):
        Script.side_effect = RuntimeError

        try:
            jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3)
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
            jedibackend.run_with_debug(jedi, 'test_method', 1, 2, arg=3)
