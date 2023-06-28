import perfectNumbers as perf
import pytest


def test_6_is_perfect():
    assert perf.classify(6) == perf.Classify.PERFECT


def test_28_is_perfect():
    assert perf.classify(28) == perf.Classify.PERFECT


def test_1_is_deficient():
    """List of factors, not including the number itself"""
    assert perf.classify(1) == perf.Classify.DEFICIENT


def test_8_is_deficient():
    assert perf.factors(8) == {1, 2, 4, 8}
    assert perf.classify(8) == perf.Classify.DEFICIENT


@pytest.mark.parametrize("prime,", [3, 5, 7, 11])
def test_primes_are_deficient(prime):
    assert perf.factors(prime) == {1, prime}
    assert perf.classify(prime) == perf.Classify.DEFICIENT


def test_12_is_abundant():
    assert perf.factors(12) == {1, 2, 3, 4, 6, 12}
    assert perf.classify(12) == perf.Classify.ABUNDENT
