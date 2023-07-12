from __future__ import annotations
from functools import lru_cache


@lru_cache()
def _make_change(amount: int, allowed: tuple[int]) -> (list[int], bool):
    if amount == 0:
        return [], True

    if amount in allowed:
        return [amount], True

    smaller_than = tuple([coin for coin in allowed if coin < amount])
    if not smaller_than:
        return [], False
    possible_routes = []
    for coin in smaller_than:
        other_coins, can_solve = _make_change(amount - coin, smaller_than)
        if can_solve:
            possible_routes.append([coin] + other_coins)
    if not possible_routes:
        return [], False
    smallest = sorted(possible_routes, key=lambda x: len(x))[0]
    return smallest, True


def make_change(amount: int, allowed: list[int]) -> list[int]:
    change, can_be_done = _make_change(amount, tuple(allowed))
    if not can_be_done:
        raise ValueError("Cannot be solved!")
    return sorted(change)
