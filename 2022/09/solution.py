def parse(path):
    with open(path) as f:
        lines = [e.split() for e in f.readlines()]
        instructions = [(e1, int(e2)) for (e1, e2) in lines]
    return instructions


def sign(x):
    if x < 0:
        return -1
    if x > 0:
        return 1
    return 0


def part1(input):
    return part2(input, 2)


def part2(input, n=10):
    locations: list[tuple[int, int]] = [(0, 0) for _ in range(n)]
    tail_locations: set[tuple[int, int]] = {(0, 0)}
    for direction, steps in input:
        for _ in range(steps):
            head_x, head_y = locations[0]
            if direction == "D":
                head_y -= 1
            elif direction == "U":
                head_y += 1
            elif direction == "L":
                head_x -= 1
            elif direction == "R":
                head_x += 1

            locations[0] = (head_x, head_y)

            for node_index in range(1, n):
                head_x, head_y = locations[node_index - 1]
                tail_x, tail_y = locations[node_index]
                delta_x = head_x - tail_x
                delta_y = head_y - tail_y
                if abs(delta_x) > 1 and head_y == tail_y:
                    tail_x += sign(delta_x)
                elif abs(delta_y) > 1 and head_x == tail_x:
                    tail_y += sign(delta_y)
                elif abs(delta_x) + abs(delta_y) > 2:
                    tail_x += sign(delta_x)
                    tail_y += sign(delta_y)
                locations[node_index] = (tail_x, tail_y)

            tail_locations.add(locations[-1])

    return len(tail_locations)


def main():
    input_file = parse("input.dat")
    test_input_file = parse("test_input.dat")

    assert part1(test_input_file) == 13
    print(part1(input_file))

    test_input_file2 = parse("test_input2.dat")
    assert part2(test_input_file2) == 36
    print(part2(input_file))


if __name__ == "__main__":
    main()
