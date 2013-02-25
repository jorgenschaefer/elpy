import sys
import types

from pydoc import safeimport, resolve, ErrorDuringImport
from pkgutil import iter_modules

from elpy import compat

# Types we want to recurse into (nodes).
CONTAINER_TYPES = (type, types.ModuleType)
# Types of attributes we can get documentation for (leaves).
PYDOC_TYPES = (type,
               types.FunctionType,
               types.BuiltinFunctionType,
               types.BuiltinMethodType,
               types.MethodType,
               types.ModuleType)
if not compat.PYTHON3:  # Python 2 old style classes
    CONTAINER_TYPES = tuple(list(CONTAINER_TYPES) + [types.ClassType])
    PYDOC_TYPES = tuple(list(PYDOC_TYPES) + [types.ClassType])


def get_pydoc_completions(modulename=None):
    """Get possible completions for modulename for pydoc.

    Returns a list of possible values to be passed to pydoc.

    """
    modulename = compat.ensure_not_unicode(modulename)
    modules = get_modules(modulename)
    if modulename is None:
        return modules
    if modules is None:
        modules = []
    try:
        module, name = resolve(modulename)
    except:
        return None

    if isinstance(module, CONTAINER_TYPES):
        modules.extend(name for name in dir(module)
                       if not name.startswith("_") and
                       isinstance(getattr(module, name),
                                  PYDOC_TYPES))
    if modules:
        return modules
    else:
        return None


def get_modules(modulename=None):
    """Return a list of modules and packages under modulename.

    If modulename is not given, return a list of all top level modules
    and packages.

    """
    modulename = compat.ensure_not_unicode(modulename)
    if not modulename:
        return ([modname for (importer, modname, ispkg)
                 in iter_modules()
                 if not modname.startswith("_")] +
                list(sys.builtin_module_names))
    try:
        module = safeimport(modulename)
    except ErrorDuringImport:
        return None
    if module is None:
        return None
    if hasattr(module, "__path__"):
        return [modname for (importer, modname, ispkg)
                in iter_modules(module.__path__)
                if not modname.startswith("_")]
    return None
