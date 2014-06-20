"""Elpy backend using the Rope library.

This backend uses the Rope library:

http://rope.sourceforge.net/

"""
import os
import time

import rope.contrib.codeassist
import rope.base.project
import rope.base.libutils
import rope.base.exceptions
import rope.contrib.findit

import elpy.pydocutils

VALIDATE_EVERY_SECONDS = 5
MAXFIXES = 5


class RopeBackend(object):
    """The Rope backend class.

    Implements the RPC calls we can pass on to Rope. Also subclasses
    the native backend to provide methods Rope does not provide, if
    any.

    """
    name = "rope"

    def __init__(self, project_root):
        super(RopeBackend, self).__init__()
        self.last_validation = 0
        self.project_root = project_root
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
        self.project = rope.base.project.Project(self.project_root,
                                                 ropefolder=None,
                                                 **prefs)

    def get_resource(self, filename):
        if filename is not None and os.path.exists(filename):
            return rope.base.libutils.path_to_resource(self.project,
                                                       filename,
                                                       'file')
        else:
            return None

    def validate(self):
        """Validate the stored project.

        This should be called before every use of Rope. It will
        revalidate the project, but do some call throttling.

        """
        now = time.time()
        if now > self.last_validation + VALIDATE_EVERY_SECONDS:
            self.project.validate()
            self.last_validation = now

    def rpc_get_completions(self, filename, source, offset):
        self.validate()
        resource = self.get_resource(filename)
        try:
            proposals = rope.contrib.codeassist.code_assist(self.project,
                                                            source, offset,
                                                            resource,
                                                            maxfixes=MAXFIXES)
            starting_offset = rope.contrib.codeassist.starting_offset(source,
                                                                      offset)
        except (rope.base.exceptions.ModuleSyntaxError,
                IndentationError,
                IndexError):
            # Rope can't parse this file
            return []
        prefixlen = offset - starting_offset

        result = []
        for proposal in proposals:
            doc = proposal.get_doc()
            if doc:
                meta = doc.strip().split("\n", 1)[0]
            else:
                meta = None
            result.append({'suffix': proposal.name[prefixlen:],
                           'docstring': doc,
                           'annotation': proposal.type,
                           'meta': meta})

        return result

    def rpc_get_definition(self, filename, source, offset):
        self.validate()

        # The find_definition call fails on an empty strings
        if source == '':
            return None

        resource = self.get_resource(filename)
        try:
            location = rope.contrib.findit.find_definition(self.project,
                                                           source, offset,
                                                           resource, MAXFIXES)
        except (rope.base.exceptions.BadIdentifierError,
                rope.base.exceptions.ModuleSyntaxError,
                IndentationError):
            # Rope can't parse this file
            return None

        if location is None:
            return None
        else:
            return (location.resource.real_path, location.offset)

    def rpc_get_calltip(self, filename, source, offset):
        self.validate()
        offset = find_called_name_offset(source, offset)
        resource = self.get_resource(filename)
        if 0 < offset < len(source) and source[offset] == ')':
            offset -= 1
        try:
            calltip = rope.contrib.codeassist.get_calltip(
                self.project, source, offset, resource, MAXFIXES,
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
        except (rope.base.exceptions.BadIdentifierError,
                rope.base.exceptions.ModuleSyntaxError,
                IndentationError,
                IndexError):
            # Rope can't parse this file
            return None

    def rpc_get_docstring(self, filename, source, offset):
        self.validate()
        resource = self.get_resource(filename)
        try:
            docstring = rope.contrib.codeassist.get_doc(self.project,
                                                        source, offset,
                                                        resource, MAXFIXES)
        except (rope.base.exceptions.BadIdentifierError,
                rope.base.exceptions.ModuleSyntaxError,
                IndentationError,
                IndexError):
            # Rope can't parse this file
            docstring = None
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
from functools import wraps


def patch_codeassist(codeassist):
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
                for name in elpy.pydocutils.get_modules()
                if name.startswith(modulename))


class FakeProposal(object):
    def __init__(self, name):
        self.name = name
        self.type = "mock"

    def get_doc(self):
        return None


patch_codeassist(rope.contrib.codeassist)
