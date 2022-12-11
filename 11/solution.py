import operator
import pathlib


class Monkey:
    def __init__(self, start_items: list[int], updater, div_by, m_true, m_false):
        self.items = list(start_items)
        self.updater = updater
        self.div_by = div_by
        self.m_true = m_true
        self.m_false = m_false
        self.inspect_count = 0
        self.modulo = None

    def inspects(self, monkeys, divider=3):
        if not self.modulo:
            self.modulo = 1
            for m in set(m.div_by for m in monkeys):
                self.modulo *= m

        updated_worries = [
            (self.update(i) // divider) % self.modulo for i in self.items
        ]
        for w in updated_worries:
            self.inspect_count += 1
            if w % self.div_by == 0:
                monkeys[self.m_true].get_item(w)
            else:
                monkeys[self.m_false].get_item(w)
        self.items = []

    def update(self, x):
        arg1 = x if self.updater[0] == "old" else int(self.updater[0])
        arg2 = x if self.updater[2] == "old" else int(self.updater[2])
        if self.updater[1] == "+":
            return arg1 + arg2
        return arg1 * arg2

    def get_item(self, i):
        self.items.append(i)


def parse(path):
    with open(path) as f:
        txt = f.read().split("\n\n")

    monkeys = []
    for e in txt:
        line_split = e.split("\n")
        start_items = [
            int(v)
            for v in line_split[1].replace(":", " ").replace(",", " ").split()[2:]
        ]

        operation = line_split[2].split()
        operation = (operation[3], operation[4], operation[5])

        div_by = int(line_split[3].split()[-1])
        if_true_idx = int(line_split[4].split()[-1])
        if_false_idx = int(line_split[5].split()[-1])

        monkeys.append(
            Monkey(start_items, operation, div_by, if_true_idx, if_false_idx)
        )

    return monkeys


def part1(input_path, num_rounds=20, divider=3):
    monkeys = parse(input_path)
    for _ in range(num_rounds):
        for m in monkeys:
            m.inspects(monkeys, divider=divider)
    most_active = sorted([m.inspect_count for m in monkeys])[-2:]

    return most_active[0] * most_active[1]


def part2(input_path):
    return part1(input_path, 10_000, 1)


def main():
    test_file = "test_input.dat"
    input_file = "input.dat"

    assert part1(test_file) == 10605
    print(part1(input_file))

    assert part2(test_file) == 2713310158
    print(part2(input_file))


if __name__ == "__main__":
    main()
