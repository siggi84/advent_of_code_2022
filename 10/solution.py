def parse(path):
    data = []
    with open(path) as f:
        for line in f.readlines():
            entry = line.split()
            if len(entry) == 1:
                data.append((entry[0], 0))
            else:
                data.append((entry[0], int(entry[1])))
    return data


def locations(instructions):
    locs = [1, 1]
    x, t = 1, 1
    for (instruction, value) in instructions:
        if instruction == "noop":
            locs.append(x)
            t += 1
        elif instruction == "addx":
            t += 2
            locs.append(x)
            x += value
            locs.append(x)

    return locs


def part1(instructions):
    X = locations(instructions)
    return sum([idx * X[idx] for idx in range(20, 221, 40)])


def part2(instructions):
    screen_width = 40
    screen_height = 6
    X = locations(instructions)[1:]
    string_res = "".join(
        [
            "#" if abs(i % screen_width - X[i]) < 2 else "."
            for i in range(0, screen_width * screen_height)
        ]
    )
    res_lines = ""
    for i in range(screen_height):
        res_lines += string_res[screen_width * i : screen_width * (i + 1)] + "\n"
    return res_lines


def main():
    test_input_data = parse("test_input.dat")
    input_data = parse("input.dat")

    assert part1(test_input_data) == 13140
    print(part1(input_data))

    print(part2(input_data))


if __name__ == "__main__":
    main()
