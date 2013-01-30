"""Tests for the elpy.server module"""

import unittest
import mock

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


class TestRPCEcho(TestServer):
    def test_should_return_arguments(self):
        srv = server.ElpyRPCServer()
        self.assertEqual(srv.rpc_echo("hello", "world"),
                         ("hello", "world"))


class TestRPCGetTraceback(TestServer):
    def test_should_return_traceback(self):
        srv = server.ElpyRPCServer()
        srv.last_traceback = "foo"
        self.assertEqual(srv.rpc_get_traceback(),
                         "foo")


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


class TestGetAvailableBackends(TestServer):
    def test_should_return_available_backends(self):
        srv = server.ElpyRPCServer()
        self.JediBackend.return_value = None
        self.assertEqual(sorted(srv.rpc_get_available_backends()),
                         sorted(["native", "rope"]))


class TestPassthroughRPCCalls(TestServer):
    rpc_calls2 = ["rpc_before_save", "rpc_after_save"]
    rpc_calls4 = ["rpc_get_completions", "rpc_get_definition",
                  "rpc_get_calltip", "rpc_get_docstring"]

    def test_should_pass_methods_to_backend(self):
        backend = mock.MagicMock()
        self.RopeBackend.return_value = backend
        srv = server.ElpyRPCServer()
        for method in self.rpc_calls2:
            getattr(srv, method)("foo", "bar")
            getattr(backend, method).assert_called_with("foo", "bar")
        for method in self.rpc_calls4:
            getattr(srv, method)("foo", "bar", 2, 3)
            getattr(backend, method).assert_called_with("foo", "bar",
                                                        2, 3)
