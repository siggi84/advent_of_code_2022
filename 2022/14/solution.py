def parse(path: str) -> dict[tuple[int,int], str]:
    with open(path) as f:
        lines = [l.strip().split("->") for l in f.readlines()]
        lines = [[[int(j) for j in k.split(",")] for k in l] for l in lines]
    grid = {}
    for line in lines:
        for node in range(0, len(line) - 1):
            [from_x, from_y], [to_x, to_y] = line[node], line[node + 1]
            delta_x = to_x - from_x
            delta_y = to_y - from_y
            n = max(abs(delta_x), abs(delta_y))
            dx = delta_x // n
            dy = delta_y // n
            for k in range(0, n + 1):
                x = from_x + k * dx
                y = from_y + k * dy
                coord = (x, y)
                grid[coord] = "#"
    return grid


def find_landing(grid, coord, max_y, floor=None):
    x, y = coord
    while True:
        if y >= max_y and not floor:
            return None
        for dx in [0, -1, 1]:
            x_cand, y_cand = x + dx, y + 1
            if (x_cand, y_cand) in grid or y_cand == floor:
                continue
            x, y = x_cand, y_cand
            break
        else:
            return (x, y)


def part1(grid):
    grid = dict(grid)
    max_y = max([y for _, y in grid])
    for i in range(100_000):
        coord = find_landing(grid, (500, 0), max_y)
        if coord:
            grid[coord] = "o"
        else:
            return i
    return None


def part2(grid):
    grid = dict(grid)
    max_y = max([y for _, y in grid])
    for i in range(1_000_000):
        coord = find_landing(grid, (500, 0), max_y, max_y + 2)
        if coord:
            if coord == (500, 0):
                return i + 1
            grid[coord] = "o"

    return None


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 24
    input_file = "input.dat"
    input = parse(input_file)
    print(part1(input))

    assert part2(test_input) == 93
    print(part2(input))


if __name__ == "__main__":
    main()
