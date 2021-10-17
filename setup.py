#!/usr/bin/env python

from setuptools import setup, find_packages

import elpy

setup(
    name="elpy",
    version=elpy.__version__,
    description="Backend for the elpy Emacs mode",
    long_description=elpy.__doc__,
    url="https://github.com/jorgenschaefer/elpy",
    author="Jorgen Schaefer",
    author_email="contact@jorgenschaefer.de",
    license="GPL",
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Developers",
        ("License :: OSI Approved :: "
         "GNU General Public License v3 or later (GPLv3+)"),
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        "Natural Language :: English",
        "Topic :: Text Editors :: Emacs",
    ],
    packages=find_packages(),
    include_package_data=True,
    install_requires=["flake8>=2.0"],
    test_suite="elpy"
)
