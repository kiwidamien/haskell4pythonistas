from __future__ import annotations
from functools import lru_cache


@lru_cache
def pascal_element(row: int, col: int) -> int:
    if row == 0:
        return 1 if col == 0 else 0
    if col == 0 or col == row:
        return 1
    return pascal_element(row - 1, col - 1) + pascal_element(row - 1, col)


def pascal_row(row: int) -> list[int]:
    return [pascal_element(row, col) for col in range(row + 1)]


def pascal_triangle(row: int) -> list[list[int]]:
    return [pascal_row(r) for r in range(row)]
