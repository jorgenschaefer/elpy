"""Glue for the "importmagic" library.

"""

import os
import sys
import threading


try:
    if sys.version_info[0] > 2:  # currently not Python 3 compatible
        raise ImportError
    import importmagic.index
    import importmagic.symbols
    import importmagic.importer
except ImportError:  # pragma: no cover
    importmagic = None


class ImportMagic(object):

    def __init__(self):
        self.is_enabled = bool(importmagic)
        self.project_root = None
        self.symbol_index = None
        self._thread = None

    def _build_symbol_index(self, project_root, custom_path):
        index = importmagic.index.SymbolIndex()
        if os.environ.get('ELPY_TEST'):
            # test suite support: do not index the whole PYTHONPATH, it
            # takes much too long
            index.build_index([])
        elif custom_path:
            index.build_index(custom_path)
        else:
            index.build_index([project_root] + sys.path)
        self.symbol_index = index

    def build_index(self, project_root, custom_path=None):
        self.project_root = None
        self._thread = threading.Thread(target=self._build_symbol_index,
                                        args=(project_root, custom_path))
        self._thread.setDaemon(True)
        self._thread.start()

    def get_import_symbols(self, symbol):
        scores = self.symbol_index.symbol_scores(symbol)
        return [e[2] and e[1] or "<standalone module>" for e in scores]

    def add_import(self, source, symbol, module):
        imports = importmagic.importer.Imports(self.symbol_index, source)
        if not module or module == "<standalone module>":
            imports.add_import(symbol)
        else:
            imports.add_import_from(module, symbol)
        start_line, end_line, import_block = imports.get_update()
        return start_line, end_line, import_block

    def fixup_imports(self, source):
        scope = importmagic.symbols.Scope.from_source(source)
        unref, unres = scope.find_unresolved_and_unreferenced_symbols()
        start_line, end_line, import_block = importmagic.importer.get_update(
            source, self.symbol_index, unref, unres)
        return start_line, end_line, import_block
