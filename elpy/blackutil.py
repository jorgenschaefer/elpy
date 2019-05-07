"""Glue for the "black" library.

"""

import sys
from pkg_resources import parse_version
try:
    # python 3
    import configparser
except ImportError:
    # python 2
    import ConfigParser as configparser
import os

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
    # Get black config from pyproject.toml
    line_length = black.DEFAULT_LINE_LENGTH
    string_normalization = True
    parser = configparser.ConfigParser()
    pyproject_path = os.path.join(directory, "pyproject.toml")
    if parser.read(pyproject_path):
        if parser.has_option("tool.black", "line-length"):
            line_length = parser.getint("tool.black", "line-length")
        if parser.has_option("tool.black", "skip-string-normalization"):
            string_normalization = not parser.getboolean(
                "tool.black", "skip-string-normalization"
            )
    try:
        if parse_version(black.__version__) < parse_version("19.0"):
            reformatted_source = black.format_file_contents(
                src_contents=code, line_length=line_length, fast=False
            )
        else:
            fm = black.FileMode(
                line_length=line_length, string_normalization=string_normalization
            )
            reformatted_source = black.format_file_contents(
                src_contents=code, fast=False, mode=fm
            )
        return reformatted_source
    except black.NothingChanged:
        return code
    except Exception as e:
        raise Fault("Error during formatting: {}".format(e), code=400)
