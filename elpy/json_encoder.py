import json
import pathlib


class JSONEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, pathlib.Path):
            return str(o)
        else:
            return super().default(o)
