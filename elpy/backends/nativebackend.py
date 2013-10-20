"""Elpy backend using native Python methods.

This backend does not use any external packages, so should work even
if only the core Python library is available. This does make it
somewhat limited compared to the other backends.

On the other hand, this backend also serves as a root class for the
other backends, so they can fall back to the native method if the
specific solutions do not work.

"""


import pydoc
import re
import rlcompleter


class NativeBackend(object):
    """Elpy backend that uses native Python implementations.

    Works as a stand-alone backend or as a fallback for other
    backends.

    """

    def __init__(self):
        self.name = "native"

    def rpc_before_save(self, project_root, filename):
        """Method called from before-save-hook"""
        pass

    def rpc_after_save(self, project_root, filename):
        """Method called from after-save-hook"""
        pass

    def rpc_get_pydoc_documentation(self, symbol):
        """Get the Pydoc documentation for the given symbol.

        Uses pydoc and can return a string with backspace characters
        for bold highlighting.

        """
        try:
            return pydoc.render_doc(str(symbol),
                                    "Elpy Pydoc Documentation for %s",
                                    False)
        except (ImportError, pydoc.ErrorDuringImport):
            return None

    def rpc_get_completions(self, project_root, filename, source, offset):
        """Get completions for symbol at the offset.

        Wrapper around rlcompleter.

        """
        completer = rlcompleter.Completer()
        symbol, start, end = find_dotted_symbol_backward(source, offset)
        completions = []
        i = 0
        while True:
            res = completer.complete(symbol, i)
            if res is None:
                break
            completion = res[len(symbol):].rstrip("(")
            completions.append((completion, None))
            i += 1
        return completions

    def rpc_get_definition(self, project_root, filename, source, offset):
        """Get the location of the definition for the symbol at the offset.

        Not implemented in the native backend.

        """
        return None

    def rpc_get_calltip(self, project_root, filename, source, offset):
        """Get the calltip for the function at the offset.

        Not implemented in the native backend.

        """
        return None

    def rpc_get_docstring(self, project_root, filename, source, offset):
        """Get the docstring for the symbol at the offset.

        Uses pydoc and can return a string with backspace characters
        for bold highlighting.

        """
        symbol, start, end = find_dotted_symbol(source, offset)
        return self.rpc_get_pydoc_documentation(symbol)


# Helper functions

_SYMBOL_RX = re.compile("[A-Za-z0-9_]")
_DOTTED_SYMBOL_RX = re.compile("[A-Za-z0-9_.]")


def find_symbol_backward(source, offset, regexp=_SYMBOL_RX):
    """Find the Python symbol at offset in source.

    This will move backwards from offset until a non-symbol
    constituing character is found. It will NOT move forwards.

    """
    end = offset
    start = offset
    while (start > 0 and
           regexp.match(source[start - 1])):
        start -= 1
    return (source[start:end], start, end)


def find_dotted_symbol_backward(source, offset):
    """Find the Python symbol with dots at offset in source.

    This will move backwards from offset until a non-symbol
    constituing character is found. It will NOT move forwards.

    """
    return find_symbol_backward(source, offset,
                                _DOTTED_SYMBOL_RX)


def find_symbol(source, offset, regexp=_SYMBOL_RX):
    """Find the Python symbol at offset.

    This will move forward and backward from offset.

    """
    symbol, start, end = find_symbol_backward(source, offset,
                                              regexp)
    while (end < len(source) and
           regexp.match(source[end])):
        end += 1
    return (source[start:end], start, end)


def find_dotted_symbol(source, offset):
    """Find the dotted Python symbol at offset.

    This will move forward and backward from offset.

    """
    return find_symbol(source, offset, _DOTTED_SYMBOL_RX)
