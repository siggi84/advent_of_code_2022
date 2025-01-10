def parse(path):
    with open(path) as f:
        txt = [
            l.strip().replace(",", " ").replace(":", " ").split() for l in f.readlines()
        ]
    sensors = []
    beacons = []
    for l in txt:
        sx = int(l[2][2:])
        sy = int(l[3][2:])
        bx = int(l[8][2:])
        by = int(l[9][2:])
        sensors.append((sx, sy))
        beacons.append((bx, by))

    return sensors, beacons


class Interval:
    def __init__(self, start, stop):
        self.start = start
        self.stop = stop

    def overlaps(self, other):
        return not (self.stop < other.start or self.start > other.stop)

    def __repr__(self):
        return f"Interval({self.start} {self.stop})"


class IntervalAggregator:
    def __init__(self):
        self.intervals = {}

    def add(self, y, interval):
        if y not in self.intervals:
            self.intervals[y] = set()

        overlapping = {i for i in self.intervals[y] if i.overlaps(interval)}
        start = min([o.start for o in overlapping] + [interval.start])
        stop = max([o.stop for o in overlapping] + [interval.stop])
        self.intervals[y] = self.intervals[y] - overlapping
        self.intervals[y].add(Interval(start, stop))

    def find_empty(self, width):
        for y in range(0, width):
            sorted_intervals = sorted(self.intervals[y], key=lambda i: i.start)
            if len(sorted_intervals) == 0:
                return (0, y)

            if 0 <= sorted_intervals[0].start - 1 < width:
                return (sorted_intervals[0].start - 1, y)

            if sorted_intervals[-1].stop < width:
                return (sorted_intervals[-1].stop, y)

            for i in range(1, len(sorted_intervals)):
                if (
                    sorted_intervals[i - 1].stop < sorted_intervals[i].start
                    and 0 <= sorted_intervals[i - 1].stop < width
                ):
                    return (sorted_intervals[i - 1].stop, y)

        return None


def part1(sensors, beacons, y=10):
    imp = set()
    for (sx, sy), (bx, by) in zip(sensors, beacons):
        smallest_dist = abs(bx - sx) + abs(by - sy)
        if abs(sy - y) < smallest_dist:
            w = smallest_dist - (abs(sy - y))
            for i in range(sx - w, sx + w + 1):
                imp.add((i, y))
    return len(imp - set(beacons))


def part2(sensors, beacons, max_width=21):
    iva = IntervalAggregator()
    for (sx, sy), (bx, by) in zip(sensors, beacons):
        smallest_dist = abs(bx - sx) + abs(by - sy)
        for y in range(sy - smallest_dist, sy + smallest_dist + 1):
            w = smallest_dist - (abs(sy - y))
            iva.add(y, Interval(sx - w, sx + w + 1))

    (x, y) = iva.find_empty(max_width)
    return x * 4_000_000 + y


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(*test_input) == 26

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(*input_data, y=2_000_000))

    assert part2(*test_input) == 56000011
    print(part2(*input_data, max_width=4_000_001))


if __name__ == "__main__":
    main()
