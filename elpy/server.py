"""Method implementations for the Elpy JSON-RPC server.

This file implements the methods exported by the JSON-RPC server. It
handles backend selection and passes methods on to the selected
backend.

"""
import io
import os
import pydoc

from elpy.pydocutils import get_pydoc_completions
from elpy.rpc import JSONRPCServer, Fault
from elpy.auto_pep8 import fix_code
from elpy.yapfutil import fix_code as fix_code_with_yapf
from elpy.blackutil import fix_code as fix_code_with_black


try:
    from elpy import jedibackend
except ImportError:  # pragma: no cover
    jedibackend = None


class ElpyRPCServer(JSONRPCServer):
    """The RPC server for elpy.

    See the rpc_* methods for exported method documentation.

    """
    def __init__(self, *args, **kwargs):
        super(ElpyRPCServer, self).__init__(*args, **kwargs)
        self.backend = None
        self.project_root = None

    def _call_backend(self, method, default, *args, **kwargs):
        """Call the backend method with args.

        If there is currently no backend, return default."""
        meth = getattr(self.backend, method, None)
        if meth is None:
            return default
        else:
            return meth(*args, **kwargs)

    def rpc_echo(self, *args):
        """Return the arguments.

        This is a simple test method to see if the protocol is
        working.

        """
        return args

    def rpc_init(self, options):
        self.project_root = options["project_root"]
        self.env = options["environment"]

        if jedibackend:
            self.backend = jedibackend.JediBackend(self.project_root, self.env)
        else:
            self.backend = None

        return {
            'jedi_available': (self.backend is not None)
        }

    def rpc_get_calltip(self, filename, source, offset):
        """Get the calltip for the function at the offset.

        """
        return self._call_backend("rpc_get_calltip", None, filename,
                                  get_source(source), offset)

    def rpc_get_oneline_docstring(self, filename, source, offset):
        """Get a oneline docstring for the symbol at the offset.

        """
        return self._call_backend("rpc_get_oneline_docstring", None, filename,
                                  get_source(source), offset)

    def rpc_get_calltip_or_oneline_docstring(self, filename, source, offset):
        """Get a calltip or a oneline docstring for the symbol at the offset.

        """
        return self._call_backend("rpc_get_calltip_or_oneline_docstring",
                                  None, filename,
                                  get_source(source), offset)

    def rpc_get_completions(self, filename, source, offset):
        """Get a list of completion candidates for the symbol at offset.

        """
        results = self._call_backend("rpc_get_completions", [], filename,
                                     get_source(source), offset)
        # Uniquify by name
        results = list(dict((res['name'], res) for res in results)
                       .values())
        results.sort(key=lambda cand: _pysymbol_key(cand["name"]))
        return results

    def rpc_get_completion_docstring(self, completion):
        """Return documentation for a previously returned completion.

        """
        return self._call_backend("rpc_get_completion_docstring",
                                  None, completion)

    def rpc_get_completion_location(self, completion):
        """Return the location for a previously returned completion.

        This returns a list of [file name, line number].

        """
        return self._call_backend("rpc_get_completion_location", None,
                                  completion)

    def rpc_get_definition(self, filename, source, offset):
        """Get the location of the definition for the symbol at the offset.

        """
        return self._call_backend("rpc_get_definition", None, filename,
                                  get_source(source), offset)

    def rpc_get_assignment(self, filename, source, offset):
        """Get the location of the assignment for the symbol at the offset.

        """
        return self._call_backend("rpc_get_assignment", None, filename,
                                  get_source(source), offset)

    def rpc_get_docstring(self, filename, source, offset):
        """Get the docstring for the symbol at the offset.

        """
        return self._call_backend("rpc_get_docstring", None, filename,
                                  get_source(source), offset)

    def rpc_get_pydoc_completions(self, name=None):
        """Return a list of possible strings to pass to pydoc.

        If name is given, the strings are under name. If not, top
        level modules are returned.

        """
        return get_pydoc_completions(name)

    def rpc_get_pydoc_documentation(self, symbol):
        """Get the Pydoc documentation for the given symbol.

        Uses pydoc and can return a string with backspace characters
        for bold highlighting.

        """
        try:
            docstring = pydoc.render_doc(str(symbol),
                                         "Elpy Pydoc Documentation for %s",
                                         False)
        except (ImportError, pydoc.ErrorDuringImport):
            return None
        else:
            if isinstance(docstring, bytes):
                docstring = docstring.decode("utf-8", "replace")
            return docstring

    def rpc_get_usages(self, filename, source, offset):
        """Get usages for the symbol at point.

        """
        source = get_source(source)

        return self._call_backend("rpc_get_usages",
                                  None, filename, source, offset)

    def rpc_get_names(self, filename, source, offset):
        """Get all possible names

        """
        source = get_source(source)
        return self._call_backend("rpc_get_names",
                                  None, filename, source, offset)

    def rpc_get_rename_diff(self, filename, source, offset, new_name):
        """Get the diff resulting from renaming the thing at point

        """
        source = get_source(source)

        return self._call_backend("rpc_get_rename_diff",
                                  None, filename, source, offset, new_name)

    def rpc_get_extract_variable_diff(self, filename, source, offset, new_name,
                                      line_beg, line_end, col_beg, col_end):
        """Get the diff resulting from extracting the selected code

        """
        source = get_source(source)
        return self._call_backend("rpc_get_extract_variable_diff",
                                  None, filename, source, offset,
                                  new_name, line_beg, line_end, col_beg,
                                  col_end)

    def rpc_get_extract_function_diff(self, filename, source, offset, new_name,
                                      line_beg, line_end, col_beg, col_end):
        """Get the diff resulting from extracting the selected code

        """
        source = get_source(source)
        return self._call_backend("rpc_get_extract_function_diff",
                                  None, filename, source, offset, new_name,
                                  line_beg, line_end, col_beg, col_end)

    def rpc_get_inline_diff(self, filename, source, offset):
        """Get the diff resulting from inlining the thing at point.

        """
        source = get_source(source)
        return self._call_backend("rpc_get_inline_diff",
                                  None, filename, source, offset)

    def rpc_fix_code(self, source, directory):
        """Formats Python code to conform to the PEP 8 style guide.

        """
        source = get_source(source)
        return fix_code(source, directory)

    def rpc_fix_code_with_yapf(self, source, directory):
        """Formats Python code to conform to the PEP 8 style guide.

        """
        source = get_source(source)
        return fix_code_with_yapf(source, directory)

    def rpc_fix_code_with_black(self, source, directory):
        """Formats Python code to conform to the PEP 8 style guide.

        """
        source = get_source(source)
        return fix_code_with_black(source, directory)


def get_source(fileobj):
    """Translate fileobj into file contents.

    fileobj is either a string or a dict. If it's a string, that's the
    file contents. If it's a string, then the filename key contains
    the name of the file whose contents we are to use.

    If the dict contains a true value for the key delete_after_use,
    the file should be deleted once read.

    """
    if not isinstance(fileobj, dict):
        return fileobj
    else:
        try:
            with io.open(fileobj["filename"], encoding="utf-8",
                         errors="ignore") as f:
                return f.read()
        finally:
            if fileobj.get('delete_after_use'):
                try:
                    os.remove(fileobj["filename"])
                except:  # pragma: no cover
                    pass


def _pysymbol_key(name):
    """Return a sortable key index for name.

    Sorting is case-insensitive, with the first underscore counting as
    worse than any character, but subsequent underscores do not. This
    means that dunder symbols (like __init__) are sorted after symbols
    that start with an alphabetic character, but before those that
    start with only a single underscore.

    """
    if name.startswith("_"):
        name = "~" + name[1:]
    return name.lower()
