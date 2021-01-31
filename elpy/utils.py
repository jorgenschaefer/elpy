from typing import Optional, Union, NamedTuple
from pathlib import Path
from array import array
from bisect import bisect_right
from io import StringIO


# From the Jedi documentation:
#
#   line is the current line you want to perform actions on (starting
#   with line #1 as the first line). column represents the current
#   column/indent of the cursor (starting with zero). source_path
#   should be the path of your file in the file system.
class Pos(NamedTuple):
    line: int
    col: int


class SourceCode:
    _source: Optional[str]
    _path: Optional[Path]
    _line_offsets: Optional[array]

    def __init__(
            self, path: Union[None, str, Path], source: Optional[str] = None,
            line_start: int = 1, col_start: int = 0):
        if path is not None:
            self._path = Path(path)
        self._source = source
        self._line_offsets = None
        self._line_start = line_start
        self._col_start = col_start

    def get_source(self):
        if self._source is None:
            with open(self._path, 'r') as fh:
                self._source = fh.read()
        return self._source

    def __str__(self):
        return self.get_source()

    @property
    def path(self) -> Optional[Path]:
        return self._path

    def get_pos(self, offset: int) -> Pos:
        """Return a tuple of line and column for offset pos in text.

        Lines are one-based, columns zero-based.

        This is how Jedi wants it. Don't ask me why.
        """
        idx = self._get_line_offsets()
        line_num_from_zero = bisect_right(idx, offset) - self._line_start
        line_offset = idx[line_num_from_zero]
        return Pos(line_num_from_zero + self._line_start,
                   offset - line_offset + self._col_start)

    def get_offset(self, line: int, col: int):
        """Return the offset of this line and column in text.
        
        Lines are one-based, columns zero-based.
        
        This is how Jedi wants it. Don't ask me why.
        """
        line_offsets = self._get_line_offsets()
        line_from_zero = line - self._line_start
        col_from_zero = col - self._col_start
        try:
            cur_line_offset = line_offsets[line_from_zero]
        except IndexError:
            raise ValueError(f"Text does not have {line} lines.")
        offset = cur_line_offset + col_from_zero
        next_line_offset = line_offsets[line_from_zero + 1]
        if not offset < next_line_offset:
            raise ValueError(
                f"Line {line} column {col} is not within the text")
        return offset

    def _get_line_offsets(self) -> array:
        if self._line_offsets is None:
            self._line_offsets = array('I')
            self._line_offsets.append(0)
            curr_line_offset = 0
            for line in StringIO(self.get_source()):
                curr_line_offset += len(line)
                self._line_offsets.append(curr_line_offset)
        return self._line_offsets
