from typing import List


def multiple(base: int, upper: int) -> List[int]:
    if base == 0: return [0]
    if base < 0: 
        raise ValueError("Should only accept positive values for base")
    accumulator = [0]
    while True:
        candidate = len(accumulator)*base
        if candidate >= upper:
            return accumulator
        accumulator.append(candidate)


def sumOfMultiples(items: List[int], level: int) -> int:
    multiples = sum([multiple(base, upper=level) for base in items], [])
    distinct_multiples = set(multiples)
    return sum(distinct_multiples)
