def parse(path):
    templates = []
    with open(path) as f:
        all_txt = f.readlines()
        for template in all_txt:
            tpl_split = template.split()
            ore = int(tpl_split[6]), 0, 0, 0
            clay = int(tpl_split[12]), 0, 0, 0
            obsidian = int(tpl_split[18]), int(tpl_split[21]), 0, 0
            geode = int(tpl_split[27]), 0, int(tpl_split[30]), 0
            templates.append(tuple((ore, clay, obsidian, geode)))
    return templates


def helper_generator(template):
    GEODE = 3
    NUM_ROBOT_TYPES = 4

    best_so_far = 0
    max_needed = [
        max(template[i][j] for i in range(NUM_ROBOT_TYPES))
        for j in range(NUM_ROBOT_TYPES)
    ]
    max_needed[-1] = 1e100

    def helper(time_left, num_robots, num_resources, build_next=None):
        nonlocal best_so_far
        if time_left <= 0:
            ret = num_resources[GEODE]

            if ret >= best_so_far:
                best_so_far = ret
            return ret

        max_possible = (
            num_resources[GEODE]
            + time_left * num_robots[GEODE]
            + (time_left - 1) * time_left / 2
        )

        if max_possible <= best_so_far:
            return 0

        if build_next is None:
            best_res = 0
            for idx in reversed(range(NUM_ROBOT_TYPES)):
                if num_robots[idx] >= max_needed[idx]:
                    continue

                res = helper(time_left, num_robots, num_resources, idx)
                best_res = max(best_res, res)
            return best_res
        else:
            num_resourcesl = list(num_resources)
            while not all(
                num_resourcesl[i] >= template[build_next][i]
                for i in range(NUM_ROBOT_TYPES)
            ):
                time_left -= 1
                if time_left <= 0:
                    return num_resources[GEODE]

                for i in range(NUM_ROBOT_TYPES):
                    num_resourcesl[i] += num_robots[i]

            buy_num_robots = list(num_robots)
            buy_num_robots[build_next] += 1
            for i in range(NUM_ROBOT_TYPES):
                num_resourcesl[i] += num_robots[i] - template[build_next][i]

            return helper(time_left - 1, tuple(buy_num_robots), tuple(num_resourcesl))

    return helper


def part1(templates):
    sum = 0
    for template_id, template in enumerate(templates, 1):
        sum += template_id * helper_generator(template)(24, (1, 0, 0, 0), (0, 0, 0, 0))
    return sum


def part2(templates):
    total = 1
    for _, template in enumerate(templates[:3], 1):
        res = helper_generator(template)(32, (1, 0, 0, 0), (0, 0, 0, 0))
        total *= res
    return total


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 33
    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 56 * 62
    print(part2(input_data))


if __name__ == "__main__":
    main()
