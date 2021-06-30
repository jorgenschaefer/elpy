import json

from elpy.jedibackend import JEDISUP18
if JEDISUP18:
    import pathlib


class JSONEncoder(json.JSONEncoder):
    def default(self, o):
        if JEDISUP18:
            if isinstance(o, pathlib.Path):
                return str(o)
        return super().default(o)
