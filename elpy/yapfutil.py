"""Glue for the "yapf" library.

"""

import os
import sys

from elpy.rpc import Fault

YAPF_NOT_SUPPORTED = sys.version_info < (2, 7) or (
    sys.version_info >= (3, 0) and sys.version_info < (3, 4))

try:
    if YAPF_NOT_SUPPORTED:
        yapf_api = None
    else:
        from yapf.yapflib import yapf_api
        from yapf.yapflib import file_resources
except ImportError:  # pragma: no cover
    yapf_api = None


def fix_code(code, directory):
    """Formats Python code to conform to the PEP 8 style guide.

    """
    if not yapf_api:
        raise Fault('yapf not installed', code=400)
    style_config = file_resources.GetDefaultStyleForDir(directory or os.getcwd())
    try:
        reformatted_source, _ = yapf_api.FormatCode(code,
                                                    filename='<stdin>',
                                                    style_config=style_config,
                                                    verify=False)
        return reformatted_source
    except Exception as e:
            raise Fault("Error during formatting: {}".format(e),
                        code=400)
