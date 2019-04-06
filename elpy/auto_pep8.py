"""Glue for the "autopep8" library.

"""

from elpy.rpc import Fault
import os


try:
    import autopep8
except ImportError:  # pragma: no cover
    autopep8 = None


def fix_code(code, directory, max_line_length=None):
    """Formats Python code to conform to the PEP 8 style guide.

    """
    if not autopep8:
        raise Fault('autopep8 not installed, cannot fix code.',
                    code=400)
    old_dir = os.getcwd()
    options = {}
    if max_line_length is not None:
        options = {"max-line-length": max_line_length}

    try:
        os.chdir(directory)
        return autopep8.fix_code(code, apply_config=True, options=options)
    finally:
        os.chdir(old_dir)
