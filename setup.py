#!/usr/bin/env python

from distutils.core import setup

import os
import elpy

if os.path.exists("README.txt"):
    exit("README.txt exists, will be clobbered.")

with open("README.txt", "w") as f:
    f.write(elpy.__doc__)

try:
    setup(name="elpy",
          version=elpy.__version__,
          description="Backend for the elpy Emacs mode",
          author="Jorgen Schaefer",
          author_email="forcer@forcix.cx",
          url="https://github.com/jorgenschaefer/elpy",
          license="GPL",
          packages=["elpy"],
          data_files=[('elpy', ["LICENSE"])],
          classifiers=[
              "Development Status :: 5 - Production/Stable",
              ("License :: OSI Approved :: "
               "GNU General Public License v3 or later (GPLv3+)"),
              "Topic :: Text Editors :: Emacs",
          ])
finally:
    os.unlink("README.txt")
    try:
        os.unlink("MANIFEST")
    except OSError:
        pass
