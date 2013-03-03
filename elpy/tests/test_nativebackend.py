"""Tests for the elpy.backends.nativebackend backend."""

import pydoc

from elpy.backends import nativebackend
from elpy.tests.support import BackendTestCase, source_and_offset


class NativeBackendTestCase(BackendTestCase):
    def setUp(self):
        super(NativeBackendTestCase, self).setUp()
        self.backend = nativebackend.NativeBackend()


class TestInit(NativeBackendTestCase):
    def test_should_have_native_as_name(self):
        self.assertEqual(self.backend.name, "native")


class TestNoOpMethods(NativeBackendTestCase):
    def test_should_have_rpc_before_save(self):
        self.backend.rpc_before_save(None, None)

    def test_should_have_rpc_after_save(self):
        self.backend.rpc_after_save(None, None)

    def test_should_have_rpc_get_definition(self):
        self.assertIsNone(self.backend.rpc_get_definition(None, None,
                                                          None, None))

    def test_should_have_rpc_get_calltip(self):
        self.assertIsNone(self.backend.rpc_get_calltip(None, None,
                                                       None, None))


class TestGetCompletions(NativeBackendTestCase):
    def test_should_complete_simple_calls(self):
        source, offset = source_and_offset("o_|_")
        self.assertEqual(sorted([name for (name, doc) in
                                 self.backend.rpc_get_completions(
                                     None, None, source, offset)]),
                         sorted(["bject", "ct", "pen", "r", "rd"]))


class TestGetDocstring(NativeBackendTestCase):
    def test_should_find_documentation(self):
        source = "foo(open("
        offset = 6  # foo(op_|_en(
        docstring = pydoc.render_doc("open",
                                     "Elpy Pydoc Documentation for %s",
                                     False)
        self.assertEqual(self.backend.rpc_get_docstring(None, None,
                                                        source, offset),
                         docstring)


class TestGetPydocDocumentation(NativeBackendTestCase):
    def test_should_find_documentation(self):
        docstring = pydoc.render_doc("open",
                                     "Elpy Pydoc Documentation for %s",
                                     False)
        self.assertEqual(self.backend.rpc_get_pydoc_documentation("open"),
                         docstring)


class TestFindSymbol(NativeBackendTestCase):
    def test_should_find_symbol(self):
        source, offset = source_and_offset("threading.current_th_|_read")
        result = nativebackend.find_symbol_backward(source, offset)
        self.assertEqual(result[0], "current_th")
        self.assertEqual(source[result[1]:result[2]],
                         "current_th")
        result = nativebackend.find_symbol(source, offset)
        self.assertEqual(result[0], "current_thread")
        self.assertEqual(source[result[1]:result[2]],
                         "current_thread")

    def test_should_find_empty_string_at_start_of_source(self):
        source, offset = source_and_offset("_|_threading")
        result = nativebackend.find_symbol_backward(source, offset)
        self.assertEqual(result[0], "")
        self.assertEqual(source[result[1]:result[2]],
                         "")

    def test_should_find_empty_string_at_start_of_symbol(self):
        source, offset = source_and_offset("threading._|_current_thread()")
        result = nativebackend.find_symbol_backward(source, offset)
        self.assertEqual(result[0], "")
        self.assertEqual(source[result[1]:result[2]],
                         "")

    def test_should_find_symbol_at_start_of_source(self):
        source, offset = source_and_offset("thr_|_eading")
        result = nativebackend.find_symbol_backward(source, offset)
        self.assertEqual(result[0], "thr")
        self.assertEqual(source[result[1]:result[2]],
                         "thr")


class TestFindDottedSymbol(NativeBackendTestCase):
    def test_should_find_symbol(self):
        source, offset = source_and_offset(
            "foo(threading.current_th_|_read())")
        result = nativebackend.find_dotted_symbol_backward(source, offset)
        self.assertEqual(result[0], "threading.current_th")
        self.assertEqual(source[result[1]:result[2]],
                         "threading.current_th")
        result = nativebackend.find_dotted_symbol(source, offset)
        self.assertEqual(result[0], "threading.current_thread")
        self.assertEqual(source[result[1]:result[2]],
                         "threading.current_thread")

    def test_should_find_empty_string_at_start_of_source(self):
        source = "threading.current_thread"
        offset = 0
        result = nativebackend.find_dotted_symbol_backward(source, offset)
        self.assertEqual(result[0], "")
        self.assertEqual(source[result[1]:result[2]],
                         "")

    def test_should_find_empty_string_at_start_of_symbol(self):
        source = "foo(threading.current_thread)"
        offset = 4  # foo(_|_thr
        result = nativebackend.find_dotted_symbol_backward(source, offset)
        self.assertEqual(result[0], "")
        self.assertEqual(source[result[1]:result[2]],
                         "")

    def test_should_find_symbol_at_start_of_source(self):
        source = "threading.current_thread"
        offset = 13  # threading.cur_|_rent
        result = nativebackend.find_dotted_symbol_backward(source, offset)
        self.assertEqual(result[0], "threading.cur")
        self.assertEqual(source[result[1]:result[2]],
                         "threading.cur")
