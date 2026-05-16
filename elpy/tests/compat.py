"""Compatibility definitions for test code.

This module previously held Python 2/3 shims.  With Python 3.11+ as the
minimum supported version, they are no longer needed.  The remaining
public names are kept for import compatibility.
"""

import builtins  # noqa: F401
from io import StringIO  # noqa: F401
