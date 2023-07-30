class Solution:
    def coinChange(self, coins, amount):
        # BFS, graph search. Keep records of visited nodes
        visited = [True] + [False] * amount

        par = [0]  # parents
        child = []  # children
        step = 0
        while par:
            step += 1
            for p in par:
                for c in coins:
                    tmp = p + c
                    if tmp == amount:
                        return step
                    elif tmp < amount and not visited[tmp]:
                        visited[tmp] = True
                        child.append(tmp)
            par, child = child, []

        return -1 if amount else 0
