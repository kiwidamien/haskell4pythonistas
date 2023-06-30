from __future__ import annotations
from enum import Enum


class Ordering(str, Enum):
    LT = "LT"
    GT = "GT"
    EQ = "EQ"
    NONE = "NONE"


def list_starts_with(starting_list: list, containing_list: list) -> bool:
    if len(starting_list) > len(containing_list):
        return False
    for a, b in zip(starting_list, containing_list):
        if a != b:
            return False
    return True


def is_sublist(candidate_sublist: list, containing_list: list) -> bool:
    """Returns True if x == y[i:j] for some i and j"""
    if len(candidate_sublist) > len(containing_list):
        return False
    if len(candidate_sublist) == 0:
        return True
    for index, element in enumerate(containing_list):
        if element == candidate_sublist[0]:
            if list_starts_with(candidate_sublist, containing_list[index:]):
                return True
    return False


def compare_lists(list_one: list, list_two: list) -> Ordering:
    if list_one == list_two:
        return Ordering.EQ
    if len(list_one) < len(list_two) and is_sublist(list_one, list_two):
        return Ordering.LT
    if len(list_one) > len(list_two) and is_sublist(list_two, list_one):
        return Ordering.GT
    return Ordering.NONE
