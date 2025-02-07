"""Glue for the "black" library.

"""

import sys

from elpy.rpc import Fault
# in case pkg_resources is not properly installed
# (see https://github.com/jorgenschaefer/elpy/issues/1674).
# in case pkg_resources is not properly installed
# (see https://github.com/jorgenschaefer/elpy/issues/1674).
try:
    from packaging.version import Version as parse_version
except ImportError:  # pragma: no cover
    try:
        from pkg_resources import parse_version
    except ImportError:  # pragma: no cover
        def parse_version(*arg, **kwargs):
            raise Fault("`pkg_resources` could not be imported, "
                        "please reinstall Elpy RPC virtualenv with"
                        " `M-x elpy-rpc-reinstall-virtualenv`", code=400)

import os

try:
    import tomli
except ImportError:
    tomli = None

try:
    import toml
except ImportError:
    toml = None


BLACK_NOT_SUPPORTED = sys.version_info < (3, 6)

try:
    if BLACK_NOT_SUPPORTED:  # pragma: no cover
        black = None
    else:
        import black
        current_version = parse_version(black.__version__)
        if current_version >= parse_version("21.5b1"):
            from black.files import find_pyproject_toml
        elif current_version >= parse_version("20.8b0"):
            from black import find_pyproject_toml
        else:
            find_pyproject_toml = None

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
    if find_pyproject_toml:
        pyproject_path = find_pyproject_toml((directory,))
    else:
        pyproject_path = os.path.join(directory, "pyproject.toml")
    if pyproject_path and os.path.exists(pyproject_path):
        if tomli:
            with open(pyproject_path, "rb") as pyproject_file:
                pyproject_config = tomli.load(pyproject_file)
        elif toml:
            pyproject_config = toml.load(pyproject_path)
        else:
            err("pyproject.toml file found, but tomli was not installed")

        black_config = pyproject_config.get("tool", {}).get("black", {})
        if "line-length" in black_config:
            line_length = black_config["line-length"]
        if "skip-string-normalization" in black_config:
            string_normalization = not black_config["skip-string-normalization"]
    try:
        if parse_version(black.__version__) < parse_version("19.0"):
            reformatted_source = black.format_file_contents(
                src_contents=code, line_length=line_length, fast=False)
        else:
            fm = black.FileMode(
                line_length=line_length,
                string_normalization=string_normalization)
            reformatted_source = black.format_file_contents(
                src_contents=code, fast=False, mode=fm)
        return reformatted_source
    except black.NothingChanged:
        return code
    except Exception as e:
        raise Fault("Error during formatting: {}".format(e), code=400)
