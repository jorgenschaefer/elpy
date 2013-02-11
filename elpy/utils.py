import sys
import types

from pydoc import safeimport, resolve, ErrorDuringImport
from pkgutil import iter_modules


def get_pydoc_completions(modulename=None):
    """Get possible completions for modulename for pydoc.

    Returns a list of possible values to be passed to pydoc.

    """
    modules = get_modules(modulename)
    if modulename is None:
        return modules
    if modules is None:
        modules = []
    try:
        module, name = resolve(modulename)
    except:
        return None

    if isinstance(module, (type, types.ClassType,
                           types.ModuleType)):
        modules.extend(name for name in dir(module)
                       if not name.startswith("_") and
                       isinstance(getattr(module, name),
                                  (type,
                                   types.FunctionType,
                                   types.BuiltinFunctionType,
                                   types.BuiltinMethodType,
                                   types.ClassType,
                                   types.MethodType,
                                   types.ModuleType)))
    if modules:
        return modules
    else:
        return None


def get_modules(modulename=None):
    """Return a list of modules and packages under modulename.

    If modulename is not given, return a list of all top level modules
    and packages.

    """
    if not modulename:
        return ([modulename for (importer, modulename, ispkg)
                 in iter_modules()
                 if not modulename.startswith("_")] +
                list(sys.builtin_module_names))
    try:
        module = safeimport(modulename)
    except ErrorDuringImport:
        return None
    if module is None:
        return None
    if hasattr(module, "__path__"):
        return [modulename for (importer, modulename, ispkg)
                in iter_modules(module.__path__)
                if not modulename.startswith("_")]
    return None
