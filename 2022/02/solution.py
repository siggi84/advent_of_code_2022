import pathlib


def part1(entries):
    type_score = {"X": 1, "Y": 2, "Z": 3}
    win_score = {
        "A X": 3,
        "A Y": 6,
        "A Z": 0,
        "B X": 0,
        "B Y": 3,
        "B Z": 6,
        "C X": 6,
        "C Y": 0,
        "C Z": 3,
    }

    total = 0
    for line in entries:
        total += type_score[line[-1]] + win_score[line]
    return total


def part2(entries):
    win_score = {"X": 0, "Y": 3, "Z": 6}
    type_score = {
        "A X": 3,
        "A Y": 1,
        "A Z": 2,
        "B X": 1,
        "B Y": 2,
        "B Z": 3,
        "C X": 2,
        "C Y": 3,
        "C Z": 1,
    }

    total = 0
    for line in entries:
        delta = win_score[line[-1]] + type_score[line]
        total += delta
    return total


def main():
    input = pathlib.Path("input.dat")

    entries = [s for s in input.read_text().split("\n") if s]
    entries_test = ["A Y", "B X", "C Z"]

    assert part1(entries_test) == 15
    print(part1(entries))

    assert part2(entries_test) == 12
    print(part2(entries))


if __name__ == "__main__":
    main()
