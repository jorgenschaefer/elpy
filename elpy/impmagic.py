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

    def _build_symbol_index(self, project_root, custom_path, blacklist_re):
        index = importmagic.index.SymbolIndex(blacklist_re=blacklist_re)
        if os.environ.get('ELPY_TEST'):
            # test suite support: do not index the whole PYTHONPATH, it
            # takes much too long
            index.build_index([])
        elif custom_path:
            index.build_index(custom_path)
        else:
            index.build_index([project_root] + sys.path)
        self.symbol_index = index

    def build_index(self, project_root, custom_path=None, blacklist_re=None):
        self.project_root = None
        self._thread = threading.Thread(target=self._build_symbol_index,
                                        args=(project_root, custom_path,
                                              blacklist_re))
        self._thread.setDaemon(True)
        self._thread.start()

    def get_import_symbols(self, symbol):
        scores = self.symbol_index.symbol_scores(symbol)
        if not scores:
            # offer some default if there is nothing matching
            return ["import %s" % symbol]
        return ["from %s import %s" % (mod, var) if var else "import %s" % mod
                for (_, mod, var) in scores]

    def add_import(self, source, statement):
        imports = importmagic.importer.Imports(self.symbol_index, source)
        if statement.startswith('import '):
            imports.add_import(statement[7:])
        else:
            sep = statement.find(' import ')
            if sep > -1:
                imports.add_import_from(statement[5:sep], statement[sep+8:])
        start_line, end_line, import_block = imports.get_update()
        return start_line, end_line, import_block

    def fixup_imports(self, source):
        scope = importmagic.symbols.Scope.from_source(source)
        unref, unres = scope.find_unresolved_and_unreferenced_symbols()
        start_line, end_line, import_block = importmagic.importer.get_update(
            source, self.symbol_index, unref, unres)
        return start_line, end_line, import_block
