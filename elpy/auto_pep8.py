"""Glue for the "autopep8" library.

"""

from elpy.rpc import Fault
import os

try:
    import autopep8
except ImportError:  # pragma: no cover
    autopep8 = None


def fix_code(code, directory):
    """Formats Python code to conform to the PEP 8 style guide.

    """
    if not autopep8:
        raise Fault('autopep8 not installed, cannot fix code.',
                    code=400)
    old_dir = os.getcwd()
    try:
        os.chdir(directory)
        return autopep8.fix_code(code, apply_config=True)
    finally:
        os.chdir(old_dir)
