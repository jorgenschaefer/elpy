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


class TestRPCGetAvailableBackends(TestServer):
    def test_should_return_available_backends(self):
        srv = server.ElpyRPCServer()
        self.JediBackend.return_value = None
        self.assertEqual(sorted(srv.rpc_get_available_backends()),
                         sorted(["native", "rope"]))
