"""Support classes and functions for the elpy test code."""

import os
import shutil
import tempfile
import unittest


class BackendTestCase(unittest.TestCase):
    """Base class for backend tests.

    This class sets up a project root directory and provides an easy
    way to create files within the project root.

    """

    def setUp(self):
        """Create the project root and make sure it gets cleaned up."""
        self.project_root = tempfile.mkdtemp(prefix="elpy-test")
        self.addCleanup(shutil.rmtree, self.project_root, True)

    def project_file(self, relname, contents):
        """Create a file named relname within the project root.

        Write contents into that file.

        """
        full_name = os.path.join(self.project_root, relname)
        try:
            os.makedirs(os.path.dirname(full_name))
        except OSError:
            pass
        with open(full_name, "w") as f:
            f.write(contents)
        return full_name


def source_and_offset(source):
    """Return a source and offset from a source description.

    >>> source_and_offset("hello, _|_world")
    ("hello, world", 7)
    >>> source_and_offset("_|_hello, world")
    ("hello, world", 0)
    >>> source_and_offset("hello, world_|_")
    ("hello, world", 12)
    """
    offset = source.index("_|_")
    return source[:offset] + source[offset + 3:], offset
