#!/usr/bin/env python

from setuptools import setup, find_packages

import elpy

setup(
    name="elpy",
    version=elpy.__version__,
    description="Backend for the elpy Emacs mode",
    long_description=elpy.__doc__,
    url="https://github.com/jorgenschaefer/elpy",
    license="GPL",
    author="Jorgen Schaefer",
    author_email="forcer@forcix.cx",
    packages=find_packages(),
    include_package_data=True,
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Developers",
        "Natural Language :: English",
        ("License :: OSI Approved :: "
         "GNU General Public License v3 or later (GPLv3+)"),
        "Topic :: Text Editors :: Emacs",
    ],
    install_requires=["flake8>=2.0"],
)
