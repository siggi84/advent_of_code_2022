import pathlib


def priority(l):
    if l.islower():
        return ord(l) - 96
    return ord(l) - 65 + 27


def part1(entries):
    sum = 0
    for l in entries:
        n = len(l)
        compartment1 = set(l[: n // 2])
        compartment2 = set(l[n // 2 :])
        common = compartment1 & compartment2
        assert len(common) == 1
        sum += priority(common.pop())

    return sum


def part2(entries):
    sum = 0
    for idx in range(0, len(entries), 3):
        r1, r2, r3 = entries[idx : idx + 3]
        common = set(r1) & set(r2) & set(r3)
        assert len(common) == 1
        sum += priority(common.pop())

    return sum


def main():
    input = pathlib.Path("input.dat")

    entries = list(l for l in input.read_text().split("\n") if l)
    print(part1(entries))
    print(part2(entries))


if __name__ == "__main__":
    main()
