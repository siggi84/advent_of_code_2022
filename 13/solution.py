import functools
import itertools
import json


def parse(path):
    with open(path) as f:
        lines = [l.strip() for l in f.readlines()]
        parsed_lines = [json.loads(l) for l in lines if l]
        pairs = [
            (parsed_lines[i], parsed_lines[i + 1])
            for i in range(0, len(parsed_lines), 2)
        ]
    return pairs


def compare(entry1, entry2):
    match (entry1, entry2):
        case ([*le1], [*le2]):
            for v1, v2 in itertools.zip_longest(le1, le2):
                if v1 is None:
                    return -1
                if v2 is None:
                    return 1

                sub_cmp = compare(v1, v2)
                if sub_cmp != 0:
                    return sub_cmp
            return 0
        case ([*le1], e2):
            return compare(le1, [e2])
        case (e1, [*le2]):
            return compare([e1], le2)
        case (e1, e2):
            if e1 == e2:
                return 0
            return -1 if e1 < e2 else 1


def part1(pairs):
    comp_res = [compare(e1, e2) for e1, e2 in pairs]
    return sum(idx for idx, v in enumerate(comp_res, 1) if v == -1)


def part2(pairs):
    div1, div2 = [[2]], [[6]]
    entries = [div1, div2]
    for e1, e2 in pairs:
        entries += [e1, e2]

    sorted_entries = sorted(entries, key=functools.cmp_to_key(compare))
    return (sorted_entries.index(div1) + 1) * (sorted_entries.index(div2) + 1)


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 13

    input_file = "input.dat"
    input_parsed = parse(input_file)
    print(part1(input_parsed))

    assert part2(test_input) == 140
    print(part2(input_parsed))


if __name__ == "__main__":
    main()
