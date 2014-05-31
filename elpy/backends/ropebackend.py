"""Elpy backend using the Rope library.

This backend uses the Rope library:

http://rope.sourceforge.net/

"""
import os
import time
from functools import wraps

from elpy.backends.nativebackend import get_source
from elpy.backends.nativebackend import NativeBackend
import elpy.utils.pydocutils

VALIDATE_EVERY_SECONDS = 5
MAXFIXES = 5


class RopeBackend(NativeBackend):
    """The Rope backend class.

    Implements the RPC calls we can pass on to Rope. Also subclasses
    the native backend to provide methods Rope does not provide, if
    any.

    """

    def __init__(self):
        super(RopeBackend, self).__init__()
        self.name = "rope"
        self.projects = {}
        self.last_validation = {}

    def __new__(cls):
        values = cls.initialize()
        if values is None:
            return None
        obj = super(RopeBackend, cls).__new__(cls)
        obj.__dict__.update(values)
        return obj

    @classmethod
    def initialize(cls):
        try:
            from rope.contrib import codeassist
            from rope.base import project
            from rope.base import libutils
            from rope.base.exceptions import BadIdentifierError
            from rope.base.exceptions import ModuleSyntaxError
            from rope.contrib import findit
            patch_codeassist(codeassist)
            return {'codeassist': codeassist,
                    'projectlib': project,
                    'libutils': libutils,
                    'BadIdentifierError': BadIdentifierError,
                    'ModuleSyntaxError': ModuleSyntaxError,
                    'findit': findit
                    }
        except:
            return None

    def get_project(self, project_root):
        """Return a project object for the given path.

        This caches previously used project objects so they do not
        have to be re-created.

        """
        if project_root is None:
            raise ValueError("No project root is specified, "
                             "but required for Rope")
        if not os.path.isdir(project_root):
            return None
        project = self.projects.get(project_root)
        if project is None:
            prefs = dict(ignored_resources=['*.pyc', '*~', '.ropeproject',
                                            '.hg', '.svn', '_svn', '.git'],
                         python_files=['*.py'],
                         save_objectdb=False,
                         compress_objectdb=False,
                         automatic_soa=True,
                         soa_followed_calls=0,
                         perform_doa=True,
                         validate_objectdb=True,
                         max_history_items=32,
                         save_history=False,
                         compress_history=False,
                         indent_size=4,
                         extension_modules=[],
                         import_dynload_stdmods=True,
                         ignore_syntax_errors=False,
                         ignore_bad_imports=False)
            project = self.projectlib.Project(project_root,
                                              ropefolder=None,
                                              **prefs)

            self.projects[project_root] = project
        last_validation = self.last_validation.get(project_root, 0.0)
        now = time.time()
        if (now - last_validation) > VALIDATE_EVERY_SECONDS:
            project.validate()
            self.last_validation[project_root] = now
        return project

    def get_resource(self, project, filename):
        if filename is not None and os.path.exists(filename):
            return self.libutils.path_to_resource(project,
                                                  filename,
                                                  'file')
        else:
            return None

    def rpc_get_completions(self, project_root, filename, source, offset):
        source = get_source(source)
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        try:
            proposals = self.codeassist.code_assist(project, source, offset,
                                                    resource,
                                                    maxfixes=MAXFIXES)
            starting_offset = self.codeassist.starting_offset(source, offset)
        except self.ModuleSyntaxError:
            # Rope can't parse this file
            return []
        except IndentationError:
            # Rope can't parse this file
            return []
        except IndexError:
            # Bug in Rope, see #186
            return []
        prefixlen = offset - starting_offset
        return [{'suffix': proposal.name[prefixlen:],
                 'docstring': proposal.get_doc()}
                for proposal in proposals]

    def rpc_get_definition(self, project_root, filename, source, offset):
        source = get_source(source)
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        # The find_definition call fails on an empty strings
        if source == '':
            return None

        try:
            location = self.findit.find_definition(project, source, offset,
                                                   resource, MAXFIXES)
        except (self.ModuleSyntaxError, IndentationError):
            # Rope can't parse this file
            return None

        if location is None:
            return None
        else:
            return (location.resource.real_path, location.offset)

    def rpc_get_calltip(self, project_root, filename, source, offset):
        source = get_source(source)
        offset = find_called_name_offset(source, offset)
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        if 0 < offset < len(source) and source[offset] == ')':
            offset -= 1
        try:
            calltip = self.codeassist.get_calltip(project, source, offset,
                                                  resource, MAXFIXES,
                                                  remove_self=True)
            if calltip:
                calltip = calltip.replace(".__init__(", "(")
                calltip = calltip.replace("(self)", "()")
                calltip = calltip.replace("(self, ", "(")
                # "elpy.tests.support.source_and_offset(source)"
                # =>
                # "support.source_and_offset(source)"
                try:
                    openpos = calltip.index("(")
                    period2 = calltip.rindex(".", 0, openpos)
                    period1 = calltip.rindex(".", 0, period2)
                    calltip = calltip[period1 + 1:]
                except ValueError:
                    pass
            return calltip
        except (self.ModuleSyntaxError, IndentationError):
            # Rope can't parse this file
            return None
        except (self.BadIdentifierError, IndexError):
            # IndexError seems to be a bug in Rope. I don't know what
            # it causing it, exactly.
            return None

    def rpc_get_docstring(self, project_root, filename, source, offset):
        source = get_source(source)
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        try:
            docstring = self.codeassist.get_doc(project, source, offset,
                                                resource, MAXFIXES)
        except (self.ModuleSyntaxError, IndentationError):
            # Rope can't parse this file
            docstring = None
        except (self.BadIdentifierError, IndexError):
            docstring = None
        if docstring is None:
            super(RopeBackend, self).rpc_get_docstring(project_root, filename,
                                                       source, offset)
        else:
            return docstring


def find_called_name_offset(source, orig_offset):
    """Return the offset of a calling function.

    This only approximates movement.

    """
    offset = min(orig_offset, len(source) - 1)
    paren_count = 0
    while True:
        if offset <= 1:
            return orig_offset
        elif source[offset] == '(':
            if paren_count == 0:
                return offset - 1
            else:
                paren_count -= 1
        elif source[offset] == ')':
            paren_count += 1
        offset -= 1


##################################################################
# Monkey patching a method in rope because it doesn't complete import
# statements.

def patch_codeassist(codeassist):
    if getattr(codeassist._PythonCodeAssist._code_completions,
               'patched_by_elpy', False):
        return

    def wrapper(fun):
        @wraps(fun)
        def inner(self):
            proposals = get_import_completions(self)
            if proposals:
                return proposals
            else:
                return fun(self)
        inner.patched_by_elpy = True
        return inner

    codeassist._PythonCodeAssist._code_completions = \
        wrapper(codeassist._PythonCodeAssist._code_completions)


def get_import_completions(self):
    if not self.word_finder.is_import_statement(self.offset):
        return []
    modulename = self.word_finder.get_primary_at(self.offset)
    # Rope can handle modules in packages
    if "." in modulename:
        return []
    return dict((name, FakeProposal(name))
                for name in elpy.utils.pydocutils.get_modules()
                if name.startswith(modulename))


class FakeProposal(object):
    def __init__(self, name):
        self.name = name

    def get_doc(self):
        return None
