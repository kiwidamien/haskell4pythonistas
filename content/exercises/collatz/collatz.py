"""Find the length of a collatz sequence starting at n"""
from __future__ import annotations


def collatz_next(n: int) -> int:
    return n // 2 if (n % 2 == 0) else 3 * n + 1


def collatz_sequence(n: int) -> list[int]:
    accumulator = [n]
    while accumulator[-1] > 1:
        current = accumulator[-1]
        accumulator.append(collatz_next(current))
    return accumulator


def collatz_length(n: int) -> list[int]:
    return len(collatz_sequence(n))
