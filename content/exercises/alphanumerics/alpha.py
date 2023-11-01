from typing import Optional


def by_columns(eq: list[str], result: str):
    max_len = len(result)
    lhs = list(zip(*[word.zfill(max_len)[::-1] for word in eq]))
    rhs = result[::-1]
    return list(zip(lhs, rhs))


def get_ordered_free_variables(eq: list[str], result: str) -> list[str]:
    free = []
    computed = set("0")
    bc = by_columns(eq=eq, result=result)
    for lhs, rhs in bc:
        free = free + [
            letter for letter in set(lhs) if letter not in computed.union(free)
        ]
        computed = computed.union({rhs})
    return free


def fill_in(fixed: dict[str, int], non_zero: list[str], columns):
    new_fix = {**fixed, "0": 0}
    carry = 0

    for lhs, rhs in columns:
        if any([(t not in fixed) and (t != "0") for t in lhs]):
            return {k: v for k, v in new_fix.items() if k != "0"}
        sum_terms = sum(new_fix[t] for t in lhs) + carry
        new_digit = sum_terms % 10
        if rhs in new_fix:
            if new_fix[rhs] != new_digit:
                return None
        if rhs in non_zero and new_digit == 0:
            return None
        new_fix[rhs] = new_digit
        carry = sum_terms // 10
    return {k: v for k, v in new_fix.items() if k != "0"}


def dfs(fixed: dict[str, int], current: str, order: list[str], non_zero: list, columns):
    used = set(fixed.values())
    min_value = 1 if current in non_zero else 0
    possible = [num for num in range(min_value, 10) if num not in used]

    # is this the end?
    if set(order).issubset(set(fixed.keys())):
        if fill_in(fixed, non_zero, columns):
            return fixed
        return None

    for possible_val in possible:
        updated_set = fill_in({**fixed, current: possible_val}, non_zero, columns)
        if not updated_set:
            continue
        if current != order[-1]:
            next_val = order[order.index(current) + 1]
        else:
            next_val = None
        result = dfs(
            {**updated_set, current: possible_val},
            next_val,
            order=order,
            non_zero=non_zero,
            columns=columns,
        )
        if result:
            return result
    return None


def validate(eq: list[str], result: str, subs: dict[str, int]) -> bool:
    for letter, value in subs.items():
        eq = [t.replace(letter, str(value)) for t in eq]
        result = result.replace(letter, str(value))
    return sum([int(t) for t in eq]) == int(result)


def solve(eq: list[str], result: str) -> Optional[dict]:
    non_zero = set([t[0] for t in eq] + [result[0]])
    order = get_ordered_free_variables(eq=eq, result=result)
    columns = by_columns(eq=eq, result=result)
    return dfs(
        fixed={}, current=order[0], order=order, non_zero=non_zero, columns=columns
    )


if __name__ == "__main__":
    eq = ["AS", "A"]
    result = "MOM"
    expected = {"A": 9, "S": 2, "M": 1, "O": 0}
    # solve(["SEND","MORE"],"MONEY")
