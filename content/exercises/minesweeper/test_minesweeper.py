import minesweeper


def test_no_rows():
    input_arr = []
    assert minesweeper.annotate(input_arr)==[]

def test_no_columns():
    input_arr = [""]
    assert minesweeper.annotate(input_arr) == [""]

def test_no_mines():
    input_arr = ["   ", "   ", "   "]
    assert minesweeper.annotate(input_arr) == input_arr


    
