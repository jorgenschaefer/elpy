"""Tests for the elpy.server module"""

import sys
import unittest

import mock

import elpy
from elpy import server


class TestServer(unittest.TestCase):
    def setUp(self):
        self.patches = [mock.patch.object(server, 'NativeBackend'),
                        mock.patch.object(server, 'RopeBackend'),
                        mock.patch.object(server, 'JediBackend')]
        (self.NativeBackend,
         self.RopeBackend,
         self.JediBackend) = [patch.__enter__() for patch in self.patches]
        (server.BACKEND_MAP["native"],
         server.BACKEND_MAP["rope"],
         server.BACKEND_MAP["jedi"]) = (self.NativeBackend,
                                        self.RopeBackend,
                                        self.JediBackend)
        self.NativeBackend.return_value.name = "native"
        self.RopeBackend.return_value.name = "rope"
        self.JediBackend.return_value.name = "jedi"

    def tearDown(self):
        for patch in self.patches:
            patch.__exit__(None, None)


class TestInit(TestServer):
    def test_should_select_rope_if_available(self):
        srv = server.ElpyRPCServer()
        self.assertEqual(srv.rpc_get_backend(), "rope")

    def test_should_select_jedi_if_rope_is_not_available(self):
        self.RopeBackend.return_value = None
        srv = server.ElpyRPCServer()
        self.assertEqual(srv.rpc_get_backend(), "jedi")

    def test_should_select_native_if_nothing_else_is_available(self):
        self.RopeBackend.return_value = None
        self.JediBackend.return_value = None
        srv = server.ElpyRPCServer()
        self.assertEqual(srv.rpc_get_backend(), "native")


class TestHandle(TestServer):
    def test_should_fail_for_missing_method(self):
        srv = server.ElpyRPCServer()
        srv.backend = object()
        self.assertRaises(server.Fault,
                          srv.handle, "does_not_exist", ())

    def test_should_call_method(self):
        srv = server.ElpyRPCServer()
        srv.backend = mock.MagicMock()
        srv.backend.rpc_does_exist.return_value = "It works"
        self.assertEqual(srv.handle("does_exist", (1, 2, 3)),
                         "It works")
        srv.backend.rpc_does_exist.assert_called_with(1, 2, 3)


class TestRPCVersion(TestServer):
    def test_should_return_current_version(self):
        srv = server.ElpyRPCServer()
        self.assertEqual(srv.rpc_version(),
                         elpy.__version__)


class TestRPCEcho(TestServer):
    def test_should_return_arguments(self):
        srv = server.ElpyRPCServer()
        self.assertEqual(srv.rpc_echo("hello", "world"),
                         ("hello", "world"))


class TestRPCGetSetBackend(TestServer):
    def test_should_fail_on_inexisting_backend(self):
        srv = server.ElpyRPCServer()
        self.assertRaises(ValueError,
                          srv.rpc_set_backend, "doesnotexist")

    def test_should_fail_if_backend_is_inactive(self):
        self.JediBackend.return_value = None
        srv = server.ElpyRPCServer()
        self.assertRaises(ValueError,
                          srv.rpc_set_backend, "jedi")

    def test_should_get_new_backend(self):
        srv = server.ElpyRPCServer()
        srv.rpc_set_backend("jedi")
        self.assertEqual(srv.rpc_get_backend(),
                         "jedi")


class TestRPCGetAvailableBackends(TestServer):
    def test_should_return_available_backends(self):
        srv = server.ElpyRPCServer()
        self.JediBackend.return_value = None
        self.assertEqual(sorted(srv.rpc_get_available_backends()),
                         sorted(["native", "rope"]))


class TestRPCGetPydocCompletions(TestServer):
    @mock.patch.object(server, 'get_pydoc_completions')
    def test_should_call_pydoc_completions(self, get_pydoc_completions):
        srv = server.ElpyRPCServer()
        srv.rpc_get_pydoc_completions()
        get_pydoc_completions.assert_called_with(None)
        srv.rpc_get_pydoc_completions("foo")
        get_pydoc_completions.assert_called_with("foo")


from elpy.tests import compat
from elpy.tests.support import BackendTestCase

import elpy.refactor


class RopeTestCase(BackendTestCase):
    def setUp(self):
        super(RopeTestCase, self).setUp()


class TestRPCGetRefactorOptions(RopeTestCase):
    @mock.patch.object(compat.builtins, '__import__')
    def test_should_fail_if_rope_is_not_available(self, import_):
        import_.side_effect = ImportError
        filename = self.project_file("foo.py", "")
        srv = server.ElpyRPCServer()
        self.assertRaises(ImportError, srv.rpc_get_refactor_options,
                          self.project_root, filename, 0)

    @mock.patch.object(elpy.refactor, 'Refactor')
    def test_should_initialize_and_call_refactor_object(self, Refactor):
        filename = self.project_file("foo.py", "import foo")
        srv = server.ElpyRPCServer()
        srv.rpc_get_refactor_options(self.project_root, filename, 5)
        Refactor.assert_called_with(self.project_root, filename)
        Refactor.return_value.get_refactor_options.assert_called_with(5, None)


class TestRPCRefactor(RopeTestCase):
    @mock.patch.object(compat.builtins, '__import__')
    def test_should_fail_if_rope_is_not_available(self, import_):
        import_.side_effect = ImportError
        filename = self.project_file("foo.py", "")
        srv = server.ElpyRPCServer()
        self.assertRaises(ImportError, srv.rpc_refactor,
                          self.project_root, filename, 'foo', ())

    @mock.patch.object(elpy.refactor, 'Refactor')
    def test_should_initialize_and_call_refactor_object_with_args(
            self, Refactor):
        filename = self.project_file("foo.py", "import foo")
        srv = server.ElpyRPCServer()
        srv.rpc_refactor(self.project_root, filename, 'foo', (1, 2, 3))
        Refactor.assert_called_with(self.project_root, filename)
        Refactor.return_value.get_changes.assert_called_with('foo', 1, 2, 3)

    @mock.patch.object(elpy.refactor, 'Refactor')
    def test_should_initialize_and_call_refactor_object_without_args(
            self, Refactor):
        filename = self.project_file("foo.py", "import foo")
        srv = server.ElpyRPCServer()
        srv.rpc_refactor(self.project_root, filename, 'foo', None)
        Refactor.assert_called_with(self.project_root, filename)
        Refactor.return_value.get_changes.assert_called_with('foo')
