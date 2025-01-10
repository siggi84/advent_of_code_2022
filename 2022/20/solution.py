def parse(path):
    with open(path) as f:
        return [int(v) for v in f.read().splitlines()]


class MasterDS:
    def __init__(self, elements):
        self.elements = list(elements)
        self.n = len(elements)
        self.origin_to_current = dict(zip(range(self.n), range(self.n)))
        self.current_to_origin = dict(zip(range(self.n), range(self.n)))

    def __len__(self):
        return self.n

    def __iter__(self):
        for i in range(self.n):
            yield self.elements[self.current_to_origin[i]]

    def __str__(self):
        return str(list(self))

    def move(self, origin_index):
        current_location = self.origin_to_current[origin_index]
        original_value = self.elements[origin_index] % (self.n - 1)

        if original_value == 0:
            return
        order = (
            range(original_value)
            if original_value >= 0
            else range(-1, original_value - 1, -1)
        )
        for i in order:
            key1 = (current_location + i) % self.n
            key2 = (current_location + i + 1) % self.n
            (
                self.current_to_origin[key1],
                self.current_to_origin[key2],
            ) = (
                self.current_to_origin[key2],
                self.current_to_origin[key1],
            )

            (
                self.origin_to_current[self.current_to_origin[key1]],
                self.origin_to_current[self.current_to_origin[key2]],
            ) = (
                self.origin_to_current[self.current_to_origin[key2]],
                self.origin_to_current[self.current_to_origin[key1]],
            )


def part1(data):
    ds = MasterDS(data)

    for i, _ in enumerate(data):
        ds.move(i)

    mixed = list(ds)
    zero_index = mixed.index(0)

    return sum(mixed[(zero_index + i * 1000) % len(mixed)] for i in range(1,4))


def part2(data):
    encryption_key = 811589153
    ds = MasterDS([e * encryption_key for e in data])

    for i in range(10):
        for i, _ in enumerate(list(ds)):
            ds.move(i)

    mixed = list(ds)
    zero_index = mixed.index(0)

    return sum(mixed[(zero_index + i * 1000) % len(mixed)] for i in range(1,4))


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 3

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 1623178306
    print(part2(input_data))


if __name__ == "__main__":
    main()
