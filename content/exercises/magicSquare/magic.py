from typing import Optional, List
from copy import deepcopy


def target(n):
    return n*(n*n+1) // 2

def isValid(square):
    constant = target(len(square))
    if any([sum(row) != constant for row in square]):
        return False
    if any([sum(col) != constant for col in zip(*square)]):
        return False
    if sum([row[index] for index, row in enumerate(square)]) != constant:
        return False
    if sum([row[index] for index, row in enumerate(square[::-1])]) != constant:
        return False
    return True


def maybeSum(arr: List[int]) -> Optional[int]:
    if any(entry is None for entry in arr):
        return None
    return sum(arr)

def range_of_col_sums(arr, possible, col):
    min_sum = sum(arr[r][col] if arr[r][col] else min(possible)
                  for r in range(len(arr)))
    max_sum = sum(arr[r][col] if arr[r][col] else max(possible)
                  for r in range(len(arr)))
    return min_sum, max_sum


def buildSquare(arr, possible, row=0, col=0):
    arr = deepcopy(arr)
    t = target(len(arr))
    n = len(arr)
    element = n*row + col
    results = []
    for num in possible:
        arr[row][col] = num
        row_sum = sum([elem for elem in arr[row] if elem != None])
        if (col == len(arr)-1) and (row_sum != t):
            continue
        elif row_sum > t:
            break

        if sum([row[index] for index, row in enumerate(arr) if row[index] is not None]) > t:
            break
        if sum([row[index] for index, row in enumerate(arr[::-1]) if row[index] is not None]) > t:
            break

        if sum(arr[r][col] for r in range(row)) > t:
            break

        min_col_sum, max_col_sum =range_of_col_sums(arr,possible,col)
        if min_col_sum > t:
            break
        if max_col_sum < t:
            continue 
        if row == len(arr) - 1:
            if sum([arr[r][col] for r in range(len(arr))]) != t:
                arr[row][col] = None
                continue
            if col == len(arr) - 1:
                if isValid(arr):
                    yield [arr]
                else:
                    yield []
        pruned = [p for p in possible if p != num]
        next_element = element + 1
        next_row = next_element // n
        next_col = next_element % n
        results.extend([magic for magic in buildSquare(arr, pruned, next_row, next_col) if magic != []])
    print(arr)
    for result in results:
        yield result



        

def makeMagic(n):
    arr = [[None for _ in range(n)] for _ in range(n)]
    possible = list(range(1, n*n+1))
    return buildSquare(arr, possible)
