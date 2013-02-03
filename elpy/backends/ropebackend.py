"""Elpy backend using the Rope library.

This backend uses the Rope library:

http://rope.sourceforge.net/

"""

import os
import re
import time

from elpy import rpc
from elpy.backends.nativebackend import NativeBackend

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
        self.before_save_data = {}
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
            project = self.projectlib.Project(project_root)
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

    def rpc_before_save(self, project_root, filename):
        if filename is None:
            return
        try:
            self.before_save_data[project_root] = (
                filename, open(filename).read())
        except IOError:
            pass

    def rpc_after_save(self, project_root, filename):
        project = self.get_project(project_root)
        old_filename, old_contents = self.before_save_data.get(
            project_root, (None, None))
        if old_filename is not None:
            if old_filename == filename:
                self.libutils.report_change(project,
                                            filename,
                                            old_contents)
            del self.before_save_data[project_root]

    def rpc_get_completions(self, project_root, filename, source, offset):
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        try:
            proposals = self.codeassist.code_assist(project, source, offset,
                                                    resource,
                                                    maxfixes=MAXFIXES)
        except self.ModuleSyntaxError as e:
            linenos = re.findall("^  \\* line ([0-9]*):", e.message,
                                 re.MULTILINE)
            linedesc = ", ".join(str(x) for x in linenos)
            raise rpc.Fault(code=101,
                            message=("Too many syntax errors in file {} "
                                     "(lines {})"
                                     .format(e.filename, linedesc)))
        starting_offset = self.codeassist.starting_offset(source, offset)
        prefixlen = offset - starting_offset
        return [[proposal.name[prefixlen:], proposal.get_doc()]
                for proposal in proposals]

    def rpc_get_definition(self, project_root, filename, source, offset):
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        # The find_definition call fails on an empty strings
        if source == '':
            return None
        location = self.findit.find_definition(project, source, offset,
                                               resource, MAXFIXES)
        if location is None:
            return None
        else:
            return (location.resource.real_path, location.offset)

    def rpc_get_calltip(self, project_root, filename, source, offset):
        # Rewind offset to the last ( before offset
        open_paren = source.rfind("(", 0, offset)
        if open_paren > -1:
            offset = open_paren
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        try:
            return self.codeassist.get_calltip(project, source, offset,
                                               resource, MAXFIXES,
                                               remove_self=True)
        except self.ModuleSyntaxError:
            return None
        except (self.BadIdentifierError, IndexError):
            # IndexError seems to be a bug in Rope. I don't know what
            # it causing it, exactly.
            return None

    def rpc_get_docstring(self, project_root, filename, source, offset):
        project = self.get_project(project_root)
        resource = self.get_resource(project, filename)
        try:
            docstring = self.codeassist.get_doc(project, source, offset,
                                                resource, MAXFIXES)
        except (self.BadIdentifierError, IndexError):
            docstring = None
        if docstring is None:
            super(RopeBackend, self).rpc_get_docstring(project_root, filename,
                                                       source, offset)
        else:
            return docstring
