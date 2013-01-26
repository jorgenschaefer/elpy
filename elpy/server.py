"""Method implementations for the Elpy JSON-RPC server.

This file implements the methods exported by the JSON-RPC server. It
handles backend selection and passes methods on to the selected
backend.

"""

from elpy.rpc import JSONRPCServer

from elpy.backends.nativebackend import NativeBackend
from elpy.backends.ropebackend import RopeBackend
from elpy.backends.jedibackend import JediBackend

BACKEND_MAP = {
    'native': NativeBackend,
    'rope': RopeBackend,
    'jedi': JediBackend,
}


class ElpyRPCServer(JSONRPCServer):
    """The RPC server for elpy.

    See the rpc_* methods for exported method documentation.

    """

    def __init__(self):
        """Return a new RPC server object.

        As the default backend, we choose the first available from
        rope, jedi, or native.

        """
        super(ElpyRPCServer, self).__init__()
        for cls in [RopeBackend, JediBackend, NativeBackend]:
            backend = cls()
            if backend is not None:
                self.backend = backend
                break

    def rpc_echo(self, *args):
        """Return the arguments.

        This is a simple test method to see if the protocol is
        working.

        """
        return args

    def rpc_set_backend(self, backend_name):
        """Set the current backend to backend_name.

        This will change the current backend. If the backend is not
        found or can not find its library, it will raise a ValueError.

        """

        backend_cls = BACKEND_MAP.get(backend_name)
        if backend_cls is None:
            raise ValueError("Unknown backend {}"
                             .format(backend_name))
        backend = backend_cls()
        if backend is None:
            raise ValueError("Backend {} could not find the "
                             "required Python library"
                             .format(backend_name))
        self.backend = backend

    def rpc_get_backend(self):
        """Return the name of the current backend."""
        return self.backend.name

    def rpc_get_available_backends(self):
        """Return a list of names of the  available backends.

        A backend is "available" if the libraries it uses can be
        loaded.

        """
        result = []
        for cls in BACKEND_MAP.values():
            backend = cls()
            if backend is not None:
                result.append(backend.name)
        return result

    def rpc_before_save(self, project_root, filename):
        """Bookkeeping method.

        Needs to be called before a file is saved.

        """
        return self.backend.rpc_before_save(
            project_root, filename)

    def rpc_after_save(self, project_root, filename):
        """Bookkeeping method.

        Needs to be called after a file is saved.

        """
        return self.backend.rpc_after_save(
            project_root, filename)

    def rpc_get_completions(self, project_root, filename, source, offset):
        """Complete symbol at offset in source.

        Returns a list of tuples of the full symbol including
        completion and a possible docstring, or None.

        """
        return self.backend.rpc_get_completions(
            project_root, filename, source, offset)

    def rpc_get_definition(self, project_root, filename, source, offset):
        """Return the location where the symbol at offset is defined.

        The location is either a tuple of (filename, offset), or None
        if no location could be found.

        """
        return self.backend.rpc_get_definition(
            project_root, filename, source, offset)

    def rpc_get_calltip(self, project_root, filename, source, offset):
        """Return the calltip for the symbol at offset.

        This is a string. If no calltip is found, return None.

        """
        return self.backend.rpc_get_calltip(
            project_root, filename, source, offset)

    def rpc_get_docstring(self, project_root, filename, source, offset):
        """Return the calltip for the symbol at offset.

        This is a string. If no calltip is found, return None.

        """
        return self.backend.rpc_get_docstring(
            project_root, filename, source, offset)
