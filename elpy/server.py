"""Method implementations for the Elpy JSON-RPC server.

This file implements the methods exported by the JSON-RPC server. It
handles backend selection and passes methods on to the selected
backend.

"""
import sys

import elpy

from elpy.utils.pydocutils import get_pydoc_completions
from elpy.rpc import JSONRPCServer, Fault

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

    def handle(self, method_name, args):
        """Call the RPC method method_name with the specified args.

        """
        method = getattr(self.backend, "rpc_" + method_name, None)
        if method is None:
            raise Fault("Unknown method {0}".format(method_name))
        return method(*args)

    def rpc_version(self):
        """Return the version of the elpy RPC backend."""
        return elpy.__version__

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
            raise ValueError("Unknown backend {0}"
                             .format(backend_name))
        backend = backend_cls()
        if backend is None:
            raise ValueError("Backend {0} could not find the "
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

    def rpc_get_pydoc_completions(self, name=None):
        """Return a list of possible strings to pass to pydoc.

        If name is given, the strings are under name. If not, top
        level modules are returned.

        """
        return get_pydoc_completions(name)

    def rpc_get_refactor_options(self, project_root, filename,
                                 start, end=None):
        """Return a list of possible refactoring options.

        This list will be filtered depending on whether it's
        applicable at the point START and possibly the region between
        START and END.

        """
        try:
            from elpy import refactor
        except:
            raise ImportError("Rope not installed, refactorings unavailable")
        ref = refactor.Refactor(project_root, filename)
        return ref.get_refactor_options(start, end)

    def rpc_refactor(self, project_root, filename, method, args):
        """Return a list of changes from the refactoring action.

        A change is a dictionary describing the change. See
        elpy.refactor.translate_changes for a description.

        """
        try:
            from elpy import refactor
        except:
            raise ImportError("Rope not installed, refactorings unavailable")
        if args is None:
            args = ()
        ref = refactor.Refactor(project_root, filename)
        return ref.get_changes(method, *args)
