"""Glue for the "black" library.

"""

import os
import sys
from pkg_resources import parse_version

from elpy.rpc import Fault

BLACK_NOT_SUPPORTED = sys.version_info < (3, 6)

try:
    if BLACK_NOT_SUPPORTED:
        black = None
    else:
        import black
except ImportError:  # pragma: no cover
    black = None


def fix_code(code, directory):
    """Formats Python code to conform to the PEP 8 style guide.

    """
    if not black:
        raise Fault("black not installed", code=400)

    try:
        if parse_version(black.__version__) < parse_version("19.0"):
            reformatted_source = black.format_file_contents(
                src_contents=code, line_length=black.DEFAULT_LINE_LENGTH, fast=False
            )
        else:
            fm = black.FileMode()
            reformatted_source = black.format_file_contents(
                src_contents=code, fast=False, mode=fm
            )
        return reformatted_source
    except black.NothingChanged:
        return code
    except Exception as e:
        raise Fault("Error during formatting: {}".format(e), code=400)
