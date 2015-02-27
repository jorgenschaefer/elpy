"""Main interface to the RPC server.

You should be able to just run the following to use this module:

python -m elpy

The first line should be "elpy-rpc ready". If it isn't, something
broke.

"""

import os
import sys

import elpy
from elpy.server import ElpyRPCServer

if __name__ == '__main__':
    stdin = sys.stdin
    stdout = sys.stdout
    sys.stdout = sys.stderr = open(os.devnull, "w")
    stdout.write('elpy-rpc ready ({0})\n'
                 .format(elpy.__version__))
    stdout.flush()
    ElpyRPCServer(stdin, stdout).serve_forever()
