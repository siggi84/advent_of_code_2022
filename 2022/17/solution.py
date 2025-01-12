import itertools


def parse(path):
    with open(path) as f:
        return f.readline().strip()


class Shape:
    def __init__(self, base_shape, grid):
        left_adjust = 2
        start_height = grid.height() + 3

        self.loc = {(left_adjust + x, y + start_height) for (x, y) in base_shape}
        self.grid = grid

    def right(self):
        cand = {(x + 1, y) for (x, y) in self.loc}
        return self._check_collission(cand)

    def left(self):
        cand = {(x - 1, y) for (x, y) in self.loc}
        return self._check_collission(cand)

    def down(self):
        cand = {(x, y - 1) for (x, y) in self.loc}
        return self._check_collission(cand)

    def _check_collission(self, cand):
        if not any(c in self.grid for c in cand):
            self.loc = cand
            return True
        return False

    def signature(self):
        h = self.grid.height()
        return (frozenset((x, y - h) for (x, y) in self.loc),)


class Grid:
    def __init__(self):
        self.loc = set()
        self.width = 7
        self._top_points = [-1 for _ in range(self.width)]

    def __contains__(self, p):
        (x, y) = p

        if not (0 <= x < self.width):
            return True
        if y < 0:
            return True
        if p in self.loc:
            return True
        return False

    def __str__(self):
        h = self.height()
        out = []
        for l in range(0, h + 2):
            out.append(
                "".join(["@" if (x, l) in self.loc else "." for x in range(self.width)])
            )
        return "\n".join(reversed(out))

    def add_points(self, shape):
        for p in shape.loc:
            x, y = p
            self._top_points[x] = max(self._top_points[x], y)
        self.loc = self.loc | shape.loc

    def height(self):
        return max(self._top_points) + 1

    def signature(self):
        h = self.height()
        return tuple(v - h for v in self._top_points)


def part1(steps, m=2022):
    return part2(steps, m)


def part2(steps, m=1000000000000):
    shapes = [
        {(0, 0), (1, 0), (2, 0), (3, 0)},
        {(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)},
        {(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)},
        {(0, 0), (0, 1), (0, 2), (0, 3)},
        {(0, 0), (0, 1), (1, 0), (1, 1)},
    ]

    grid = Grid()
    num_shapes = len(shapes)
    num_landed_shapes = 0
    repetition_height = 0

    cache = {}
    shape = Shape(shapes[0], grid)
    for _ in itertools.count():
        for s in steps:
            if s == "<":
                shape.left()
            else:
                shape.right()

            moved = shape.down()
            if not moved:
                num_landed_shapes += 1
                grid.add_points(shape)
                shape = Shape(shapes[num_landed_shapes % num_shapes], grid)
                if num_landed_shapes == m:
                    return grid.height() + repetition_height

        h = grid.height()
        key = (num_landed_shapes % num_shapes, grid.signature(), shape.signature())
        if key in cache:
            last_h, last_sc = cache[key]

            if not repetition_height:
                delta_h = h - last_h
                delta_sc = num_landed_shapes - last_sc
                num_rep = (m - num_landed_shapes) // (num_landed_shapes - last_sc)
                repetition_height = num_rep * delta_h
                num_landed_shapes += num_rep * delta_sc
        else:
            cache[key] = (h, num_landed_shapes)

    return None


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 3068

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 1514285714288
    print(part2(input_data))


if __name__ == "__main__":
    main()
