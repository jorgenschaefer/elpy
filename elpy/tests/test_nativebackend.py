# coding: utf-8
"""Tests for the elpy.backends.nativebackend backend."""

import mock
import os
import pydoc
import tempfile

from elpy.backends import nativebackend
from elpy.tests.support import BackendTestCase, source_and_offset


class NativeBackendTestCase(BackendTestCase):
    def setUp(self):
        super(NativeBackendTestCase, self).setUp()
        self.backend = nativebackend.NativeBackend()


class TestInit(NativeBackendTestCase):
    def test_should_have_native_as_name(self):
        self.assertEqual(self.backend.name, "native")


class TestRPCGetDefinition(NativeBackendTestCase):
    def test_should_have_rpc_get_definition(self):
        self.assertIsNone(self.backend.rpc_get_definition(None, None,
                                                          None, None))

    @mock.patch('elpy.backends.nativebackend.get_source')
    def test_should_call_get_source(self, get_source):
        self.backend.rpc_get_definition(None, None, "test-source", None)

        get_source.assert_called_with("test-source")


class TestRPCGetCalltip(NativeBackendTestCase):
    def test_should_have_rpc_get_calltip(self):
        self.assertIsNone(self.backend.rpc_get_calltip(None, None,
                                                       None, None))

    @mock.patch('elpy.backends.nativebackend.get_source')
    def test_should_call_get_source(self, get_source):
        self.backend.rpc_get_calltip(None, None, "test-source", None)

        get_source.assert_called_with("test-source")


class TestGetCompletions(NativeBackendTestCase):
    def test_should_complete_simple_calls(self):
        source, offset = source_and_offset("o_|_")
        self.assertEqual(sorted([name for (name, doc) in
                                 self.backend.rpc_get_completions(
                                     None, None, source, offset)]),
                         sorted(["bject", "ct", "pen", "r", "rd"]))

    @mock.patch('elpy.backends.nativebackend.get_source')
    def test_should_call_get_source(self, get_source):
        self.backend.rpc_get_completions(None, None, "test-source", 0)

        get_source.assert_called_with("test-source")


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

    @mock.patch('elpy.backends.nativebackend.get_source')
    def test_should_call_get_source(self, get_source):
        get_source.return_value = "test-source"

        self.backend.rpc_get_docstring(None, None, "test-source", 0)

        get_source.assert_called_with("test-source")


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


class TestGetSource(NativeBackendTestCase):
    def test_should_return_string_by_default(self):
        self.assertEqual(nativebackend.get_source("foo"),
                         "foo")

    def test_should_return_file_contents(self):
        fd, filename = tempfile.mkstemp(prefix="elpy-test-")
        self.addCleanup(os.remove, filename)
        with open(filename, "w") as f:
            f.write("file contents")

        fileobj = {'filename': filename}

        self.assertEqual(nativebackend.get_source(fileobj),
                         "file contents")

    def test_should_clean_up_tempfile(self):
        fd, filename = tempfile.mkstemp(prefix="elpy-test-")
        with open(filename, "w") as f:
            f.write("file contents")

        fileobj = {'filename': filename,
                   'delete_after_use': True}

        self.assertEqual(nativebackend.get_source(fileobj),
                         "file contents")
        self.assertFalse(os.path.exists(filename))

    def test_should_support_utf8(self):
        fd, filename = tempfile.mkstemp(prefix="elpy-test-")
        self.addCleanup(os.remove, filename)
        with open(filename, "wb") as f:
            f.write(u"möp".encode("utf-8"))

        source = nativebackend.get_source({'filename': filename})

        self.assertEqual(source, u"möp")
