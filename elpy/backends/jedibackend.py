"""Elpy backend using the Jedi library.

This backend uses the Jedi library:

https://github.com/davidhalter/jedi

"""

import os
import sys


from elpy.backends.nativebackend import NativeBackend


class JediBackend(NativeBackend):
    """The Jedi backend class.

    Implements the RPC calls we can pass on to Jedi. Also subclasses
    the native backend to provide methods Jedi does not provide, if
    any.

    """
    def __new__(cls):
        try:
            import jedi
        except:
            return None
        obj = super(JediBackend, cls).__new__(cls)
        obj.jedi = jedi
        return obj

    def __init__(self):
        super(JediBackend, self).__init__()
        self.name = "jedi"

    def rpc_get_completions(self, project_root, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        sys.path.append(project_root)
        try:
            script = self.jedi.Script(source, line, column, filename,
                                      source_encoding='utf-8')
            proposals = script.complete()
        finally:
            sys.path.pop()
        return [[proposal.complete, proposal.doc]
                for proposal in proposals]

    def rpc_get_definition(self, project_root, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        sys.path.append(project_root)
        try:
            script = self.jedi.Script(source, line, column, filename,
                                      source_encoding='utf-8')
            locations = script.get_definition()
            # get_definition() can return silly stuff like __builtin__
            # for int variables, so we fall back on goto() in those
            # cases. See issue #76.
            if (
                    locations and
                    locations[0].module_path is None
            ):
                locations = script.goto()
        finally:
            sys.path.pop()
        if not locations:
            return None
        else:
            loc = locations[-1]
            try:
                if loc.module_path:
                    with open(loc.module_path) as f:
                        offset = linecol_to_pos(f.read(),
                                                *loc.start_pos)
            except IOError:
                return None
            return (loc.module_path, offset)

    def rpc_get_calltip(self, project_root, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        sys.path.append(project_root)
        try:
            script = self.jedi.Script(source, line, column, filename,
                                      source_encoding='utf-8')
            call = script.get_in_function_call()
        finally:
            sys.path.pop()
        if call is None:
            return None
        return "{0}({1})".format(call.call_name,
                                 ", ".join(param.get_code().strip()
                                           for param in call.params))

    def rpc_get_docstring(self, project_root, filename, source, offset):
        """Return a docstring for the symbol at offset.

        This uses the nativebackend, as apparently, Jedi does not know
        how to do this. It can do a completion and find docstrings for
        that, but not for the symbol at a location. Huh.

        """
        return super(JediBackend, self).rpc_get_docstring(project_root,
                                                          filename,
                                                          source,
                                                          offset)


# From the Jedi documentation:
#
#   line is the current line you want to perform actions on (starting
#   with line #1 as the first line). column represents the current
#   column/indent of the cursor (starting with zero). source_path
#   should be the path of your file in the file system.
#
# Now, why you'd offset a program to a piece of code in a string using
# line/column indeces is a bit beyond me. And even moreso, why you'd
# make lines one-based and columns zero-based is a complete mystery.
# But well, that's what it says.

def pos_to_linecol(text, pos):
    """Return a tuple of line and column for offset pos in text.

    Lines are one-based, columns zero-based.

    This is how Jedi wants it. Don't ask me why.

    """
    line_start = text.rfind("\n", 0, pos) + 1
    line = text.count("\n", 0, line_start) + 1
    col = pos - line_start
    return line, col


def linecol_to_pos(text, line, col):
    """Return the offset of this line and column in text.

    Lines are one-based, columns zero-based.

    This is how Jedi wants it. Don't ask me why.

    """
    nth_newline_offset = 0
    for i in range(line - 1):
        new_offset = text.find("\n", nth_newline_offset)
        if new_offset < 0:
            raise ValueError("Text does not have {0} lines."
                             .format(line))
        nth_newline_offset = new_offset + 1
    offset = nth_newline_offset + col
    if offset > len(text):
        raise ValueError("Line {0} column {1} is not within the text"
                         .format(line, col))
    return offset
