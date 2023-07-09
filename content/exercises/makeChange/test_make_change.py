from make_change import make_change
import pytest


def test_example_where_greedy_doesnt_work():
    coins = [1, 4, 15, 20, 50]
    expected = [4, 4, 15]
    assert make_change(amount=23, allowed=coins)==expected

def test_example_with_ones_only():
    coins = [1]
    expected = [1,1,1,1,1]
    assert make_change(amount=5, allowed=coins)==expected

def test_impossible_example():
    coins = [2]
    with pytest.raises(ValueError):
        make_change(amount=5, allowed=coins)
