"""Main interface to the RPC server.

You should be able to just run the following to use this module:

python -m elpy

The first line should be "elpy-rpc ready". If it isn't, something
broke.

"""

import sys

from elpy.server import ElpyRPCServer

if __name__ == '__main__':
    sys.stdout.write('elpy-rpc ready\n')
    sys.stdout.flush()
    ElpyRPCServer().serve_forever()
