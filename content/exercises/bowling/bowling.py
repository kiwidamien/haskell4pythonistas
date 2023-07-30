from __future__ import annotations

class BowlingError(Exception):
    pass

class Frame:
    def __init__(self):
        self.rolls = []

    def add_roll(self, roll: int):
        if roll < 0 or roll > 10:
            raise BowlingError(f"Rolls must be between 0 and 10: found {roll}")
        self.rolls.append(roll)
        if sum(self.rolls) > 10:
            raise BowlingError(f"Sum of rolls must be 10 or less")

    @classmethod
    def from_iterable(cls, rolls: list[int]):
        new_frame = cls()
        rolls_copy = rolls[:]
        new_frame.add_roll(rolls_copy[0])
        if new_frame.total() == 10:
            return new_frame, rolls_copy[1:]
        new_frame.add_roll(rolls_copy[1])
        return new_frame, rolls_copy[2:]

    def total(self):
        return sum(self.rolls)

    def __repr__(self):
        return f"{self.__class__.__name__}: {self.rolls} ({self.total()})"

    def is_strike(self) -> bool:
        return self.rolls and (self.rolls[0]==10)

    def is_spare(self) -> bool:
        return self.rolls and (~self.is_strike()) and (self.total()==10)

    def score(self, next):
        total = self.total()
        if next is None:  # final frame rules
            return total
        if self.is_strike():
            return total + next.total()
        if self.is_spare():
            return total + next.rolls[0]
        return total

Game = list[Frame]


def from_list(rolls: list[int]) -> Game:
    rolls = rolls[:]
    game = []
    while rolls:
        frame, rolls = Frame.from_iterable(rolls)
        game.append(frame)
    return game
    


def score(game: Game) -> int:
    score=0
    for frame, next_frame in zip(game, game[1:]):
        score += frame.score(next_frame)
    score += game[-1].score(next=None)
    return score
