from __future__ import annotations
from enum import Enum, auto


class Classify(Enum):
    DEFICIENT = auto()
    ABUNDENT = auto()
    PERFECT = auto()


def factors(n) -> list[int]:
    return set([f for f in range(1, n + 1) if n % f == 0])


def classify(n) -> Classify:
    sum_proper_factors = sum(factors(n)) - n
    if n == sum_proper_factors:
        return Classify.PERFECT
    if n > sum_proper_factors:
        return Classify.DEFICIENT
    return Classify.ABUNDENT
