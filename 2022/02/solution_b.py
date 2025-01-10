import pathlib

ROCK, PAPER, SCISSOR = 1, 2, 3

translation = {
    "X": ROCK,  # LOSS
    "Y": PAPER,  # DRAW
    "Z": SCISSOR,  # WIN
    "A": ROCK,
    "B": PAPER,
    "C": SCISSOR,
}


def part1(entries):
    games = [(translation[g[0]], translation[g[2]]) for g in entries]
    return sum(((p2 - p1 + 1) % 3) * 3 + p2 for p1, p2 in games)


def part2(entries):
    games = [(translation[g[0]], translation[g[2]]) for g in entries]
    return sum((res - 1) * 3 + (p1 + res) % 3 + 1 for p1, res in games)


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
