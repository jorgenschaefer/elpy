from pathlib import Path
from typing import Any, List, NamedTuple, NoReturn, Optional, Tuple, Union

from pydantic import BaseModel


class Result(BaseModel):
    pass


class ServerMsg(BaseModel):
    pass


class NameResult(Result):
    name: str
    offset: int
    filename: str


class RefactoringResult(Result):
    success: bool
    project_path: Path
    diff: str
    changed_files: List[Path]
    error_msg: Optional[str]


class ErrorMsg(ServerMsg):
    id: int
    error: Union[dict, Result]


class ResponceMsg(ServerMsg):
    id: int
    result: Union[dict, List, str, Result]
