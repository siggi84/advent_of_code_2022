N = (-1, 0)
S = (1, 0)
W = (0, -1)
E = (0, 1)
WAIT = (0, 0)


def parse(path):
    with open(path) as f:
        data = [list(l.strip()[1:-1]) for l in f.readlines()][1:-1]

    n = len(data)
    m = len(data[0])
    grid = [[[] for _ in range(m)] for _ in range(n)]

    for r, l in enumerate(data):
        for c, s in enumerate(l):
            if s == "^":
                grid[r][c].append(N)
            elif s == "v":
                grid[r][c].append(S)
            elif s == ">":
                grid[r][c].append(E)
            elif s == "<":
                grid[r][c].append(W)

    return grid


def print_blizzards(blizzards):
    res = ""
    smap = {N: "^", S: "v", E: ">", W: "<"}
    for _, l in enumerate(blizzards):
        for _, b in enumerate(l):
            blen = len(b)
            if blen == 0:
                res += "."
            elif blen > 1:
                res += str(blen) if blen < 10 else "!"
            else:
                res += smap[list(b)[0]]

        res += "\n"
    print(res)


def evolve(blizzards):
    n = len(blizzards)
    m = len(blizzards[0])
    next_blizzard = [[[] for _ in range(m)] for _ in range(n)]
    for i in range(0, n):
        for j in range(0, m):
            for b in blizzards[i][j]:
                i_n, j_n = (i + b[0]) % n, (j + b[1]) % m
                next_blizzard[i_n][j_n].append(b)
    return next_blizzard


def part1(blizzards):
    n = len(blizzards)
    m = len(blizzards[0])
    start = (-1, 0)
    stop = (n, m - 1)

    return bfs_helper(blizzards, start, stop)


def part2(blizzards):
    n = len(blizzards)
    m = len(blizzards[0])
    start = (-1, 0)
    stop = (n, m - 1)
    t0 = bfs_helper(blizzards, start, stop, 0)
    t1 = bfs_helper(blizzards, stop, start, t0)
    t2 = bfs_helper(blizzards, start, stop, t1)

    return t2


def to_tuple(x):
    if isinstance(x, list):
        return tuple([to_tuple(v) for v in x])
    else:
        return x


blizzard_cache = {}
def precalc_blizzard_states(blizzards):
    key = to_tuple(blizzards)
    if key in blizzard_cache:
        return blizzard_cache[key]

    blizzards_state = [blizzards]
    while len(blizzards_state) <= 2 or blizzards_state[0] != blizzards_state[-1]:
        blizzards_state.append(evolve(blizzards_state[-1]))
    blizzard_cache[key] = (blizzards_state, len(blizzards_state) - 1)

    return blizzard_cache[key]


def bfs_helper(blizzards, start, stop, t0=0):
    n = len(blizzards)
    m = len(blizzards[0])

    blizzards_state, bn = precalc_blizzard_states(blizzards)

    directions = [S, E, WAIT, W, N]

    q = [(t0, start)]
    explored = set((t0, start))

    while q:
        t, loc = q.pop(0)
        row, col = loc

        if (
            0 <= row < n
            and 0 <= col
            and blizzards_state[t % bn][loc[0]][loc[1]]
        ):
            continue

        if loc == stop:
            return t

        for d in directions:
            next_row, next_col = row + d[0], col + d[1]
            if not (0 <= next_row < n and 0 <= next_col < m or (next_row, next_col) in (start, stop)):
                continue
            state = (t + 1, (next_row, next_col))
            if state not in explored:
                explored.add(state)
                q.append(state)


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 18

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 54
    print(part2(input_data))


if __name__ == "__main__":
    main()
