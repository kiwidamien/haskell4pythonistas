def annotate(grid: list[str]) -> list[str]:
    def num_to_char(row, col, num):
        if grid[row][col] == '*':
            return '*'
        if num==0:
            return ' '
        return str(num)
    
    def count_stars(row, col):
        count = 0
        for y in [row-1, row, row+1]:
            for x in [col-1, col, col+1]:
                try:
                    count += 1 if grid[y][x] == '*' else 0
                except IndexError:
                    pass
        return count
    
    counter = [
        [count_stars(row, col) for col in range(len(grid[0]))]
        for row in range(len(grid))
    ]
    output = [
        "".join([num_to_char(row, col, num) for col, num in enumerate(row_contents)])
        for row, row_contents in enumerate(counter)
    ]
    return output