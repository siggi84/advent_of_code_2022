def parse(path):
    with open(path) as f:
        coords = [tuple(int(v) for v in e.split(",")) for e in f.readlines()]
    return coords


DIRECTIONS = [(0, 0, -1), (0, 0, 1), (0, -1, 0), (0, 1, 0), (-1, 0, 0), (1, 0, 0)]


def part1(coords):
    grid = set(coords)
    count = 0

    for c in coords:
        x, y, z = c
        for dx, dy, dz in DIRECTIONS:
            p = (x + dx, y + dy, z + dz)
            if p not in grid:
                count += 1
    return count


def part2(coords):
    min_x = min(x for x, _, _ in coords) - 1
    min_y = min(y for _, y, _ in coords) - 1
    min_z = min(z for _, _, z in coords) - 1
    max_x = max(x for x, _, _ in coords) + 1
    max_y = max(y for _, y, _ in coords) + 1
    max_z = max(z for _, _, z in coords) + 1

    root = (min_x, min_y, min_z)
    air_surface = set()
    q = [root]
    while q:
        v = q.pop(0)
        if v in air_surface:
            continue
        air_surface.add(v)
        (x, y, z) = v
        for dx, dy, dz in DIRECTIONS:
            p = (x + dx, y + dy, z + dz)
            if p in air_surface:
                continue
            if p in coords:
                continue
            if not min_x <= p[0] <= max_x:
                continue
            if not min_y <= p[1] <= max_y:
                continue
            if not min_z <= p[2] <= max_z:
                continue
            q.append(p)

    count = 0
    for c in coords:
        x, y, z = c
        for dx, dy, dz in DIRECTIONS:
            p = (x + dx, y + dy, z + dz)
            if p in air_surface:
                count += 1
    return count


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)

    assert part1(test_input) == 64
    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 58
    print(part2(input_data))


if __name__ == "__main__":
    main()
