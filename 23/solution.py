def parse(path):
    with open(path) as f:
        data = [list(l.strip()) for l in f.readlines()]

    elve_locations = set()
    for r, l in enumerate(data):
        for c, v in enumerate(l):
            if v == "#":
                elve_locations.add((r, c))

    return elve_locations


def evolve(elve_locations, num_rounds=10):
    elve_locations = set(elve_locations)
    N = (-1, 0)
    NE = (-1, 1)
    NW = (-1, -1)
    S = (1, 0)
    SE = (1, 1)
    SW = (1, -1)
    W = (0, -1)
    E = (0, 1)
    directions_checks = [(N, NE, NW), (S, SE, SW), (W, NW, SW), (E, NE, SE)]
    all_directions = [N, NE, NW, S, SE, SW, W, E]

    round = 0
    for round in range(num_rounds):
        proposals = {}
        for (r, c) in elve_locations:
            nn_check = [
                (r + dr, c + dc) in elve_locations for (dr, dc) in all_directions
            ]
            if not any(nn_check):
                continue
            for ds in directions_checks:
                nn_check = [(r + dr, c + dc) in elve_locations for (dr, dc) in ds]
                if not any(nn_check):
                    prop = (r + ds[0][0], c + ds[0][1])
                    if prop not in proposals:
                        proposals[prop] = []
                    proposals[prop].append((r, c))
                    break

        if not proposals:
            break
        for prop, cands in proposals.items():
            if len(cands) == 1:
                elve_locations.discard(cands[0])
                elve_locations.add(prop)
        directions_checks = directions_checks[1:] + [directions_checks[0]]

    min_r = min(e[0] for e in elve_locations)
    max_r = max(e[0] for e in elve_locations)
    min_c = min(e[1] for e in elve_locations)
    max_c = max(e[1] for e in elve_locations)

    return (max_r - min_r + 1) * (max_c - min_c + 1) - len(elve_locations), round + 1


def part1(elve_locations):
    return evolve(elve_locations, 10)[0]


def part2(elve_locations):
    return evolve(elve_locations, 10**10)[1]


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 110

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 20
    print(part2(input_data))


if __name__ == "__main__":
    main()
