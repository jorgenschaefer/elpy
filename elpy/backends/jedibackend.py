"""Elpy backend using the Jedi library.

This backend uses the Jedi library:

https://github.com/davidhalter/jedi

"""

import sys


from elpy.backends.nativebackend import get_source
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
        source = get_source(source)
        line, column = pos_to_linecol(source, offset)
        sys.path.append(project_root)
        try:
            proposals = run_with_debug(self.jedi, 'completions',
                                       source=source, line=line, column=column,
                                       path=filename, encoding='utf-8')
        finally:
            sys.path.pop()
        return [[proposal.complete, proposal.docstring()]
                for proposal in proposals]

    def rpc_get_definition(self, project_root, filename, source, offset):
        source = get_source(source)
        line, column = pos_to_linecol(source, offset)
        sys.path.append(project_root)
        try:
            locations = run_with_debug(self.jedi, 'goto_definitions',
                                       source=source, line=line, column=column,
                                       path=filename, encoding='utf-8')
            # goto_definitions() can return silly stuff like __builtin__
            # for int variables, so we fall back on goto() in those
            # cases. See issue #76.
            if (
                    locations and
                    locations[0].module_path is None
            ):
                locations = run_with_debug(self.jedi, 'goto_assignments',
                                           source=source, line=line,
                                           column=column,
                                           path=filename, encoding='utf-8')
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
                                                loc.line,
                                                loc.column)
            except IOError:
                return None
            return (loc.module_path, offset)

    def rpc_get_calltip(self, project_root, filename, source, offset):
        source = get_source(source)
        line, column = pos_to_linecol(source, offset)
        sys.path.append(project_root)
        try:
            call = run_with_debug(self.jedi, 'get_in_function_call',
                                  source=source, line=line, column=column,
                                  path=filename, encoding='utf-8')
        finally:
            sys.path.pop()
        if not call:
            return None
        return {"name": call.name,
                "index": call.index,
                "params": [param.description for param in call.params]}

    def rpc_get_docstring(self, project_root, filename, source, offset):
        """Return a docstring for the symbol at offset.

        This uses the nativebackend, as apparently, Jedi does not know
        how to do this. It can do a completion and find docstrings for
        that, but not for the symbol at a location. Huh.

        """
        source = get_source(source)
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


def run_with_debug(jedi, name, *args, **kwargs):
    try:
        script = jedi.Script(*args, **kwargs)
        return getattr(script, name)()
    except:
        from jedi import debug

        debug_info = []

        def _debug(level, str_out):
            if level == debug.NOTICE:
                prefix = "[N]"
            elif level == debug.WARNING:
                prefix = "[W]"
            else:
                prefix = "[?]"
            debug_info.append("{0} {1}".format(prefix, str_out))

        jedi.set_debug_function(_debug, speed=False)
        try:
            script = jedi.Script(*args, **kwargs)
            return getattr(script, name)()
        except Exception as e:
            source = kwargs.get('source')
            sc_args = []
            sc_args.extend(repr(arg) for arg in args)
            sc_args.extend("{0}={1}".format(k, "source" if k == "source"
                                            else repr(v))
                           for (k, v) in kwargs.items())
            e.jedi_debug_info = {'script_args': ", ".join(sc_args),
                                 'source': source,
                                 'method': name,
                                 'debug_info': debug_info}
            raise
        finally:
            jedi.set_debug_function(None)
