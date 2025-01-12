import pathlib
from dataclasses import dataclass


def part1(entries):
    return sum(i1.contains(i2) or i2.contains(i1) for i1, i2 in entries)


def part2(entries):
    return sum(i1.overlaps(i2) for i1, i2 in entries)


@dataclass
class Interval:
    low: int
    high: int

    def overlaps(self, other):
        return not (other.high < self.low or other.low > self.high)

    def contains(self, other):
        return self.low <= other.low and other.high <= self.high


def main():
    input = pathlib.Path("input.dat")

    entries = []
    for l in input.read_text().split("\n"):
        if not l:
            continue
        pair = l.split(",")
        iv1 = pair[0].split("-")
        iv2 = pair[1].split("-")

        interval1 = Interval(int(iv1[0]), int(iv1[1]))
        interval2 = Interval(int(iv2[0]), int(iv2[1]))
        entries.append((interval1, interval2))
    print(part1(entries))
    print(part2(entries))


if __name__ == "__main__":
    main()
