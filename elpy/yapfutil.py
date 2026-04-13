"""Glue for the "yapf" library.

"""

import os
import sys

from elpy.rpc import Fault

YAPF_NOT_SUPPORTED = sys.version_info < (2, 7) or (
    sys.version_info >= (3, 0) and sys.version_info < (3, 4))

# This came from elpy/blackutil.py it should probably be in a library
try:
    from pkg_resources import parse_version
except ImportError:  # pragma: no cover
    try:
        from packaging.version import parse as parse_version
    except ImportError:  # pragma: no cover
        raise Fault("Neither `packaging` or `pkg_rsources` could be imported., "
                    "please reinstall Elpy RPC virtualenv with"
                    " `M-x elpy-rpc-reinstall-virtualenv`", code=400)
try:
    if YAPF_NOT_SUPPORTED:
        yapf_api = None
    else:
        from yapf.yapflib import yapf_api
        from yapf.yapflib import file_resources
        from yapf import __version__
        yapf_version = parse_version(__version__)
except ImportError:  # pragma: no cover
    yapf_api = None


def fix_code(code, directory):
    """Formats Python code to conform to the PEP 8 style guide.

    """
    if not yapf_api:
        raise Fault('yapf not installed', code=400)
    style_config = file_resources.GetDefaultStyleForDir(directory or os.getcwd())
    try:
        extra = {}
        if yapf_version < parse_version("0.40.1"):
            extra = {"verify": False}

        reformatted_source, _ = yapf_api.FormatCode(code,
                                                    filename='<stdin>',
                                                    style_config=style_config,
                                                    **extra)
        return reformatted_source
    except Exception as e:
            raise Fault("Error during formatting: {}".format(e),
                        code=400)
