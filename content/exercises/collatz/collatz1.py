"""Find the length of a collatz sequence starting at n"""
from __future__ import annotations


def collatz_next(n: int) -> int:
    return n // 2 if (n % 2 == 0) else 3 * n + 1


def collatz_sequence(n: int) -> list[int]:
    def gen(n):
        while True:
            yield n
            if n == 1:
                return
            n = collatz_next(n)

    return list(gen(n))


def collatz_length(n: int) -> list[int]:
    return len(collatz_sequence(n))
