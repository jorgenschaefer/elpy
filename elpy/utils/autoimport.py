"""Refactoring module to automatically add missing autoimports.

This will currently only find module names, nothing else.

I.e.

conf = ConfigParser.SafeConfigParser()

will get "import ConfigParser" added; but

conf = SafeConfigParser()

won't get "from ConfigParser import SafeConfigParser" added.

"""

import difflib
import pkgutil

from elpy.utils import findundefined


def get_changes(filename):
    """Return a list of changes for this filename.

    See elpy.refactor.translate_changes for a definition of those
    change objects.

    """
    orig = open(filename).read()
    newsource = _autoimport_add_imports(orig)
    if newsource is None:
        return []
    diff = "".join(line + "\n"
                   for line in
                   difflib.unified_diff(orig.split("\n"),
                                        newsource.split("\n"),
                                        fromfile=filename,
                                        tofile=filename,
                                        lineterm=''))
    return [{'action': 'change',
             'file': filename,
             'contents': newsource,
             'diff': diff}]


def _autoimport_add_imports(source):
    modules = []
    for name in findundefined.find_undefined(source):
        module = find_module_for(name)
        if module:
            modules.append(module)
    if modules:
        return add_imports(source, sorted(set(modules)))
    else:
        return None


def find_module_for(name):
    """Return a module so that NAME will work."""
    def _find_loader(name):
        try:
            return pkgutil.find_loader(name)
        except ImportError:
            return None

    if _find_loader(name):
        return name
    while "." in name:
        name = name[:name.rindex(".")]
        if _find_loader(name):
            return name
    return None


def add_imports(source, module_list):
    """Return a modified SOURCE that includes imports for MODULE_LIST."""
    imports = "".join(["import {0}\n".format(module)
                       for module in module_list])
    if source.startswith("import ") or "\nimport " in source:
        last_import_offset = source.rfind("\nimport ") + 1
        offset = source.find("\n", last_import_offset) + 1
        return source[:offset] + imports + source[offset:]
    else:
        return imports + source
