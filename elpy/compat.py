"""Compatibility definitions.

This module previously held Python 2/3 shims.  With Python 3.11+ as the
minimum supported version, most of them are gone.  The remaining public
names are kept so that in-tree (and possibly out-of-tree) code that
already imports them keeps working.
"""

from io import StringIO  # noqa: F401


def ensure_not_unicode(obj):
    """Identity function -- kept for call-site compatibility."""
    return obj
