import pytest
from alpha import solve, validate


def test_example():
    eq = ["SEND", "MORE"]
    result = "MONEY"
    expected = dict(M=1, O=0, N=6, E=5, Y=2, R=8, D=7, S=9)
    print(expected)
    assert solve(eq, result) == expected


def test_four_letters():
    eq = ["AS", "A"]
    result = "MOM"
    expected = {"A": 9, "S": 2, "M": 1, "O": 0}
    assert solve(eq, result) == expected


def test_validation():
    eq = ["SEND", "MORE"]
    result = "MONEY"
    expected = dict(M=1, O=0, N=6, E=5, Y=2, R=8, D=7, S=9)
    assert validate(eq, result, subs=expected)


def test_validation_from_function():
    eq = ["SEND", "MORE"]
    result = "MONEY"
    ans = solve(eq, result)
    assert validate(eq, result, ans)
