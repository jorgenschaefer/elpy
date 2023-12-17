"""Elpy backend using the Jedi library.

This backend uses the Jedi library:

https://github.com/davidhalter/jedi

"""

import sys
import traceback
import re

import jedi

from elpy import rpc
from elpy.rpc import Fault

# in case pkg_resources is not properly installed
# (see https://github.com/jorgenschaefer/elpy/issues/1674).
try:
    from packaging.version import Version as parse_version
except ImportError:  # pragma: no cover
    def parse_version(*arg, **kwargs):
        raise Fault("`pkg_resources` could not be imported, "
                    "please reinstall Elpy RPC virtualenv with"
                    " `M-x elpy-rpc-reinstall-virtualenv`", code=400)
JEDISUP17 = parse_version(jedi.__version__) >= parse_version("0.17.0")
JEDISUP18 = parse_version(jedi.__version__) >= parse_version("0.18.0")


class JediBackend(object):
    """The Jedi backend class.

    Implements the RPC calls we can pass on to Jedi.

    Documentation: http://jedi.jedidjah.ch/en/latest/docs/plugin-api.html

    """
    name = "jedi"

    def __init__(self, project_root, environment_binaries_path):
        self.project_root = project_root
        self.environment = None
        if environment_binaries_path is not None:
            self.environment = jedi.create_environment(environment_binaries_path,
                                                       safe=False)
        self.completions = {}
        sys.path.append(project_root)
        # Backward compatibility to jedi<17
        if not JEDISUP17:  # pragma: no cover
            self.rpc_get_completions = self.rpc_get_completions_jedi16
            self.rpc_get_docstring = self.rpc_get_docstring_jedi16
            self.rpc_get_definition = self.rpc_get_definition_jedi16
            self.rpc_get_assignment = self.rpc_get_assignment_jedi16
            self.rpc_get_calltip = self.rpc_get_calltip_jedi16
            self.rpc_get_oneline_docstring = self.rpc_get_oneline_docstring_jedi16
            self.rpc_get_usages = self.rpc_get_usages_jedi16
            self.rpc_get_names = self.rpc_get_names_jedi16

    def rpc_get_completions(self, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        proposals = run_with_debug(jedi, 'complete', code=source,
                                   path=filename,
                                   environment=self.environment,
                                   fun_kwargs={'line': line, 'column': column})
        self.completions = dict((proposal.name, proposal)
                                for proposal in proposals)
        return [{'name': proposal.name.rstrip("="),
                 'suffix': proposal.complete.rstrip("="),
                 'annotation': proposal.type,
                 'meta': proposal.description}
                for proposal in proposals]

    def rpc_get_completions_jedi16(self, filename, source, offset):
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        proposals = run_with_debug(jedi, 'completions',
                                   source=source, line=line, column=column,
                                   path=filename, encoding='utf-8',
                                   environment=self.environment)
        if proposals is None:
            return []
        self.completions = dict((proposal.name, proposal)
                                for proposal in proposals)
        return [{'name': proposal.name.rstrip("="),
                 'suffix': proposal.complete.rstrip("="),
                 'annotation': proposal.type,
                 'meta': proposal.description}
                for proposal in proposals]

    def rpc_get_completion_docstring(self, completion):
        proposal = self.completions.get(completion)
        if proposal is None:
            return None
        else:
            return proposal.docstring(fast=False)

    def rpc_get_completion_location(self, completion):
        proposal = self.completions.get(completion)
        if proposal is None:
            return None
        else:
            return (proposal.module_path, proposal.line)

    def rpc_get_docstring(self, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        locations = run_with_debug(jedi, 'goto',
                                   code=source,
                                   path=filename,
                                   environment=self.environment,
                                   fun_kwargs={'line': line,
                                               'column': column,
                                               'follow_imports': True,
                                               'follow_builtin_imports': True})
        if not locations:
            return None
        # Filter uninteresting things
        if locations[-1].name in ["str", "int", "float", "bool", "tuple",
                                  "list", "dict"]:
            return None
        if locations[-1].docstring():
            return ('Documentation for {0}:\n\n'.format(
                locations[-1].full_name) + locations[-1].docstring())
        else:
            return None

    def rpc_get_docstring_jedi16(self, filename, source, offset):
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        locations = run_with_debug(jedi, 'goto_definitions',
                                   source=source, line=line, column=column,
                                   path=filename, encoding='utf-8',
                                   environment=self.environment)
        if not locations:
            return None
        # Filter uninteresting things
        if locations[-1].name in ["str", "int", "float", "bool", "tuple",
                                  "list", "dict"]:
            return None
        if locations[-1].docstring():
            return ('Documentation for {0}:\n\n'.format(
                locations[-1].full_name) + locations[-1].docstring())
        else:
            return None

    def rpc_get_definition(self, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        locations = run_with_debug(jedi, 'goto',
                                   code=source,
                                   path=filename,
                                   environment=self.environment,
                                   fun_kwargs={'line': line,
                                               'column': column,
                                               'follow_imports': True,
                                               'follow_builtin_imports': True})
        if not locations:
            return None
        # goto_definitions() can return silly stuff like __builtin__
        # for int variables, so we remove them. See issue #76.
        locations = [
            loc for loc in locations
            if (loc.module_path is not None
                and loc.module_name != 'builtins'
                and loc.module_name != '__builtin__')]
        if len(locations) == 0:
            return None
        loc = locations[-1]
        try:
            if loc.module_path == filename:
                offset = linecol_to_pos(source,
                                        loc.line,
                                        loc.column)
            else:
                with open(loc.module_path) as f:
                    offset = linecol_to_pos(f.read(),
                                            loc.line,
                                            loc.column)
        except IOError:  # pragma: no cover
            return None
        return (loc.module_path, offset)

    def rpc_get_definition_jedi16(self, filename, source, offset): # pragma: no cover
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        locations = run_with_debug(jedi, 'goto_definitions',
                                   source=source, line=line, column=column,
                                   path=filename, encoding='utf-8',
                                   environment=self.environment)
        # goto_definitions() can return silly stuff like __builtin__
        # for int variables, so we fall back on goto() in those
        # cases. See issue #76.
        if (
                locations and
                (locations[0].module_path is None
                 or locations[0].module_name == 'builtins'
                 or locations[0].module_name == '__builtin__')
        ):
            locations = run_with_debug(jedi, 'goto_assignments',
                                       source=source, line=line,
                                       column=column,
                                       path=filename,
                                       encoding='utf-8',
                                       environment=self.environment)
        if not locations:
            return None
        else:
            loc = locations[-1]
            try:
                if loc.module_path:
                    if loc.module_path == filename:
                        offset = linecol_to_pos(source,
                                                loc.line,
                                                loc.column)
                    else:
                        with open(loc.module_path) as f:
                            offset = linecol_to_pos(f.read(),
                                                    loc.line,
                                                    loc.column)
                else:
                    return None
            except IOError:
                return None
            return (loc.module_path, offset)

    def rpc_get_assignment(self, filename, source, offset):
        raise Fault("Obsolete since jedi 17.0. Please use 'get_definition'.")

    def rpc_get_assignment_jedi16(self, filename, source, offset): # pragma: no cover
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        locations = run_with_debug(jedi, 'goto_assignments',
                                   source=source, line=line, column=column,
                                   path=filename, encoding='utf-8',
                                   environment=self.environment)

        if not locations:
            return None
        else:
            loc = locations[-1]
            try:
                if loc.module_path:
                    if loc.module_path == filename:
                        offset = linecol_to_pos(source,
                                                loc.line,
                                                loc.column)
                    else:
                        with open(loc.module_path) as f:
                            offset = linecol_to_pos(f.read(),
                                                    loc.line,
                                                    loc.column)
                else:
                    return None
            except IOError:
                return None
            return (loc.module_path, offset)

    def rpc_get_calltip(self, filename, source, offset):
        line, column = pos_to_linecol(source, offset)
        calls = run_with_debug(jedi, 'get_signatures',
                               code=source,
                               path=filename,
                               environment=self.environment,
                               fun_kwargs={'line': line,
                                           'column': column})
        if not calls:
            return None
        params = [re.sub("^param ", '', param.description)
                  for param in calls[0].params]
        return {"name": calls[0].name,
                "index": calls[0].index,
                "params": params}

    def rpc_get_calltip_jedi16(self, filename, source, offset): # pragma: no cover
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        calls = run_with_debug(jedi, 'call_signatures',
                               source=source, line=line, column=column,
                               path=filename, encoding='utf-8',
                               environment=self.environment)
        if calls:
            call = calls[0]
        else:
            call = None
        if not call:
            return None
        # Strip 'param' added by jedi at the beginning of
        # parameter names. Should be unecessary for jedi > 0.13.0
        params = [re.sub("^param ", '', param.description)
                  for param in call.params]
        return {"name": call.name,
                "index": call.index,
                "params": params}

    def rpc_get_calltip_or_oneline_docstring(self, filename, source, offset):
        """
        Return the current function calltip or its oneline docstring.

        Meant to be used with eldoc.
        """
        # Try to get a oneline docstring then
        docs = self.rpc_get_oneline_docstring(filename=filename,
                                              source=source,
                                              offset=offset)
        if docs is not None:
            if docs['doc'] != "No documentation":
                docs['kind'] = 'oneline_doc'
                return docs
        # Try to get a calltip
        calltip = self.rpc_get_calltip(filename=filename, source=source,
                                       offset=offset)
        if calltip is not None:
            calltip['kind'] = 'calltip'
            return calltip
        # Ok, no calltip, just display the function name
        if docs is not None:
            docs['kind'] = 'oneline_doc'
            return docs
        # Giving up...
        return None

    def rpc_get_oneline_docstring(self, filename, source, offset):
        """Return a oneline docstring for the symbol at offset"""
        line, column = pos_to_linecol(source, offset)
        definitions = run_with_debug(jedi, 'goto',
                                     code=source,
                                     path=filename,
                                     environment=self.environment,
                                     fun_kwargs={'line': line,
                                                 'column': column})
        if not definitions:
            return None
        # avoid unintersting stuff
        definitions = [defi for defi in definitions
                       if defi.name not in
                       ["str", "int", "float", "bool", "tuple",
                        "list", "dict"]]
        if len(definitions) == 0:
            return None
        definition = definitions[0]
        # Get name
        if definition.type in ['function', 'class']:
            raw_name = definition.name
            name = '{}()'.format(raw_name)
            doc = definition.docstring().split('\n')
        elif definition.type in ['module']:
            raw_name = definition.name
            name = '{} {}'.format(raw_name, definition.type)
            doc = definition.docstring().split('\n')
        elif (definition.type in ['instance']
              and hasattr(definition, "name")):
            raw_name = definition.name
            name = raw_name
            doc = definition.docstring().split('\n')
        else:
            return None
        # Keep only the first paragraph that is not a function declaration
        lines = []
        call = "{}(".format(raw_name)
        # last line
        doc.append('')
        for i in range(len(doc)):
            if doc[i] == '' and len(lines) != 0:
                paragraph = " ".join(lines)
                lines = []
                if call != paragraph[0:len(call)]:
                    break
                paragraph = ""
                continue
            lines.append(doc[i])
        # Keep only the first sentence
        onelinedoc = paragraph.split('. ', 1)
        if len(onelinedoc) == 2:
            onelinedoc = onelinedoc[0] + '.'
        else:
            onelinedoc = onelinedoc[0]
        if onelinedoc == '':
            onelinedoc = "No documentation"
        return {"name": name,
                "doc": onelinedoc}

    def rpc_get_oneline_docstring_jedi16(self, filename, source, offset): # pragma: no cover
        """Return a oneline docstring for the symbol at offset"""
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        definitions = run_with_debug(jedi, 'goto_definitions',
                                     source=source, line=line, column=column,
                                     path=filename, encoding='utf-8',
                                     environment=self.environment)
        # avoid unintersting stuff
        try:
            if definitions[0].name in ["str", "int", "float", "bool", "tuple",
                                       "list", "dict"]:
                return None
        except:
            pass
        assignments = run_with_debug(jedi, 'goto_assignments',
                                     source=source, line=line, column=column,
                                     path=filename, encoding='utf-8',
                                     environment=self.environment)

        if definitions:
            definition = definitions[0]
        else:
            definition = None
        if assignments:
            assignment = assignments[0]
        else:
            assignment = None
        if definition:
            # Get name
            if definition.type in ['function', 'class']:
                raw_name = definition.name
                name = '{}()'.format(raw_name)
                doc = definition.docstring().split('\n')
            elif definition.type in ['module']:
                raw_name = definition.name
                name = '{} {}'.format(raw_name, definition.type)
                doc = definition.docstring().split('\n')
            elif (definition.type in ['instance']
                  and hasattr(assignment, "name")):
                raw_name = assignment.name
                name = raw_name
                doc = assignment.docstring().split('\n')
            else:
                return None
            # Keep only the first paragraph that is not a function declaration
            lines = []
            call = "{}(".format(raw_name)
            # last line
            doc.append('')
            for i in range(len(doc)):
                if doc[i] == '' and len(lines) != 0:
                    paragraph = " ".join(lines)
                    lines = []
                    if call != paragraph[0:len(call)]:
                        break
                    paragraph = ""
                    continue
                lines.append(doc[i])
            # Keep only the first sentence
            onelinedoc = paragraph.split('. ', 1)
            if len(onelinedoc) == 2:
                onelinedoc = onelinedoc[0] + '.'
            else:
                onelinedoc = onelinedoc[0]
            if onelinedoc == '':
                onelinedoc = "No documentation"
            return {"name": name,
                    "doc": onelinedoc}
        return None

    def rpc_get_usages(self, filename, source, offset):
        """Return the uses of the symbol at offset.

        Returns a list of occurrences of the symbol, as dicts with the
        fields name, filename, and offset.

        """
        line, column = pos_to_linecol(source, offset)
        uses = run_with_debug(jedi, 'get_references',
                              code=source,
                              path=filename,
                              environment=self.environment,
                              fun_kwargs={'line': line,
                                          'column': column})
        if uses is None:
            return None
        result = []
        for use in uses:
            if use.module_path == filename:
                offset = linecol_to_pos(source, use.line, use.column)
            elif use.module_path is not None:
                with open(use.module_path) as f:
                    text = f.read()
                offset = linecol_to_pos(text, use.line, use.column)
            result.append({"name": use.name,
                           "filename": use.module_path,
                           "offset": offset})
        return result

    def rpc_get_usages_jedi16(self, filename, source, offset): # pragma: no cover
        """Return the uses of the symbol at offset.

        Returns a list of occurrences of the symbol, as dicts with the
        fields name, filename, and offset.

        """
        # Backward compatibility to jedi<17
        line, column = pos_to_linecol(source, offset)
        uses = run_with_debug(jedi, 'usages',
                              source=source, line=line, column=column,
                              path=filename, encoding='utf-8',
                              environment=self.environment)
        if uses is None:
            return None
        result = []
        for use in uses:
            if use.module_path == filename:
                offset = linecol_to_pos(source, use.line, use.column)
            elif use.module_path is not None:
                with open(use.module_path) as f:
                    text = f.read()
                offset = linecol_to_pos(text, use.line, use.column)

            result.append({"name": use.name,
                           "filename": use.module_path,
                           "offset": offset})

        return result

    def rpc_get_names(self, filename, source, offset):
        """Return the list of possible names"""
        names = run_with_debug(jedi, 'get_names',
                               code=source,
                               path=filename,
                               environment=self.environment,
                               fun_kwargs={'all_scopes': True,
                                           'definitions': True,
                                           'references': True})
        result = []
        for name in names:
            if name.module_path == filename:
                offset = linecol_to_pos(source, name.line, name.column)
            elif name.module_path is not None:
                with open(name.module_path) as f:
                    text = f.read()
                offset = linecol_to_pos(text, name.line, name.column)
            result.append({"name": name.name,
                           "filename": name.module_path,
                           "offset": offset})
        return result

    def rpc_get_names_jedi16(self, filename, source, offset): # pragma: no cover
        """Return the list of possible names"""
        # Backward compatibility to jedi<17
        names = jedi.api.names(source=source,
                               path=filename, encoding='utf-8',
                               all_scopes=True,
                               definitions=True,
                               references=True)

        result = []
        for name in names:
            if name.module_path == filename:
                offset = linecol_to_pos(source, name.line, name.column)
            elif name.module_path is not None:
                with open(name.module_path) as f:
                    text = f.read()
                offset = linecol_to_pos(text, name.line, name.column)
            result.append({"name": name.name,
                           "filename": name.module_path,
                           "offset": offset})
        return result

    def rpc_get_rename_diff(self, filename, source, offset, new_name):
        """Get the diff resulting from renaming the thing at point"""
        if not hasattr(jedi.Script, "rename"):  # pragma: no cover
            return {'success': "Not available"}
        line, column = pos_to_linecol(source, offset)
        ren = run_with_debug(jedi, 'rename', code=source,
                             path=filename,
                             environment=self.environment,
                             fun_kwargs={'line': line,
                                         'column': column,
                                         'new_name': new_name})
        if ren is None:
            return {'success': False}
        else:
            return {'success': True,
                    'project_path': ren._inference_state.project._path,
                    'diff': ren.get_diff(),
                    'changed_files': list(ren.get_changed_files().keys())}

    def rpc_get_extract_variable_diff(self, filename, source, offset, new_name,
                                      line_beg, line_end, col_beg, col_end):
        """Get the diff resulting from extracting the selected code"""
        if not hasattr(jedi.Script, "extract_variable"):  # pragma: no cover
            return {'success': "Not available"}
        ren = run_with_debug(jedi, 'extract_variable', code=source,
                             path=filename,
                             environment=self.environment,
                             fun_kwargs={'line': line_beg,
                                         'until_line': line_end,
                                         'column': col_beg,
                                         'until_column': col_end,
                                         'new_name': new_name})
        if ren is None:
            return {'success': False}
        else:
            return {'success': True,
                    'project_path': ren._inference_state.project._path,
                    'diff': ren.get_diff(),
                    'changed_files': list(ren.get_changed_files().keys())}

    def rpc_get_extract_function_diff(self, filename, source, offset, new_name,
                                      line_beg, line_end, col_beg, col_end):
        """Get the diff resulting from extracting the selected code"""
        if not hasattr(jedi.Script, "extract_function"):  # pragma: no cover
            return {'success': "Not available"}
        ren = run_with_debug(jedi, 'extract_function', code=source,
                             path=filename,
                             environment=self.environment,
                             fun_kwargs={'line': line_beg,
                                         'until_line': line_end,
                                         'column': col_beg,
                                         'until_column': col_end,
                                         'new_name': new_name})
        if ren is None:
            return {'success': False}
        else:
            return {'success': True,
                    'project_path': ren._inference_state.project._path,
                    'diff': ren.get_diff(),
                    'changed_files': list(ren.get_changed_files().keys())}

    def rpc_get_inline_diff(self, filename, source, offset):
        """Get the diff resulting from inlining the selected variable"""
        if not hasattr(jedi.Script, "inline"):  # pragma: no cover
            return {'success': "Not available"}
        line, column = pos_to_linecol(source, offset)
        ren = run_with_debug(jedi, 'inline', code=source,
                             path=filename,
                             environment=self.environment,
                             fun_kwargs={'line': line,
                                         'column': column})
        if ren is None:
            return {'success': False}
        else:
            return {'success': True,
                    'project_path': ren._inference_state.project._path,
                    'diff': ren.get_diff(),
                    'changed_files': list(ren.get_changed_files().keys())}


# From the Jedi documentation:
#
#   line is the current line you want to perform actions on (starting
#   with line #1 as the first line). column represents the current
#   column/indent of the cursor (starting with zero). source_path
#   should be the path of your file in the file system.

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


def run_with_debug(jedi, name, fun_kwargs={}, *args, **kwargs):
    re_raise = kwargs.pop('re_raise', ())
    try:
        script = jedi.Script(*args, **kwargs)
        return getattr(script, name)(**fun_kwargs)
    except Exception as e:
        if isinstance(e, re_raise):
            raise
        if JEDISUP17:
            if isinstance(e, jedi.RefactoringError):
                return None
        # Bug jedi#485
        if (
                isinstance(e, ValueError) and
                "invalid \\x escape" in str(e)
        ):
            return None
        # Bug jedi#485 in Python 3
        if (
                isinstance(e, SyntaxError) and
                "truncated \\xXX escape" in str(e)
        ):
            return None

        from jedi import debug

        debug_info = []

        def _debug(level, str_out):
            if level == debug.NOTICE:
                prefix = "[N]"
            elif level == debug.WARNING:
                prefix = "[W]"
            else:
                prefix = "[?]"
            debug_info.append(u"{0} {1}".format(prefix, str_out))

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

            data = {
                "traceback": traceback.format_exc(),
                "jedi_debug_info": {'script_args': ", ".join(sc_args),
                                    'source': source,
                                    'method': name,
                                    'debug_info': debug_info}
            }
            raise rpc.Fault(message=str(e),
                            code=500,
                            data=data)
        finally:
            jedi.set_debug_function(None)
