import pytest
import bowling
import random


def test_score_of_10_zeros_scores_0():
    zeros = [0 for _ in range(20)]
    game = bowling.from_list(zeros)
    assert bowling.score(game) == 0


def test_score_with_no_strikes_or_spares_is_the_sum():
    rolls = [
        2,
        3,
        9,
        0,
        5,
        3,
        1,
        1,
        5,
        0,
        2,
        2,
        0,
        1,
        8,
        1,
        5,
        4,
        0,
        0,
    ]
    game = bowling.from_list(rolls)
    assert bowling.score(game) == sum(rolls)


def test_spare_followed_by_zeros_is_10_pts():
    rolls = [6, 4] + [0 for _ in range(18)]
    game = bowling.from_list(rolls)
    assert bowling.score(game) == 10


def test_points_scored_in_roll_after_spare_are_counted_twice():
    rolls = [6, 4, 3] + [0 for _ in range(17)]
    game = bowling.from_list(rolls)
    assert bowling.score(game) == 16


def test_consecutive_spares_are_scored_correctly():
    rolls = [6, 4, 3, 7, 4] + [0 for _ in range(15)]
    game = bowling.from_list(rolls)
    score1 = 10 + 3
    score2 = 10 + 4
    total_score = score1 + score2 + 4
    assert bowling.score(game) == total_score


def test_spare_in_last_frame_gets_a_one_roll_bonus_counted_once():
    rolls = [0 for _ in range(18)] + [5, 5, 3]
    game = bowling.from_list(rolls)
    total_score = 10 + 3
    assert bowling.score(game) == total_score


def test_spare_in_last_frame_and_only_20_rolls_is_invalid():
    rolls = [0 for _ in range(18)] + [5, 5]
    with pytest.raises(bowling.BowlingError):
        game = bowling.from_list(rolls)
