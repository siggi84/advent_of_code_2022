import copy
import operator


class Monkey:
    def __init__(self, name, number=None, operation=None, arg1=None, arg2=None):
        self.name = name
        self.number = number
        self.operation = operation
        self.arg1 = arg1
        self.arg2 = arg2
        self.arg1_number = None
        self.arg2_number = None

        self.subscribers = []

    def subscribe(self, m):
        if m not in self.subscribers:
            self.subscribers.append(m)

    def register(self, monkey_dict):
        if self.number is not None:
            return
        monkey_dict[self.arg1].subscribe(self)
        monkey_dict[self.arg2].subscribe(self)

    def yell(self):
        if self.number is None:
            return
        for s in self.subscribers:
            s.listen(self.name, self.number)

    def listen(self, caller_name, caller_number):
        if caller_name == self.arg1:
            self.arg1_number = caller_number
        elif caller_name == self.arg2:
            self.arg2_number = caller_number
        if self.arg1_number and self.arg2_number:
            self.number = self.operation(self.arg1_number, self.arg2_number)
            self.yell()


def parse(path):
    with open(path) as f:
        monkeys = [v.replace(":", "").split() for v in f.read().splitlines()]

    monkey_dict = {}
    for m in monkeys:
        if len(m) == 2:
            monkey_dict[m[0]] = Monkey(m[0], int(m[1]))
        elif len(m) == 4:
            op_string = m[2]
            op = None
            if op_string == "+":
                op = operator.add
            elif op_string == "-":
                op = operator.sub
            elif op_string == "*":
                op = operator.mul
            elif op_string == "/":
                op = operator.floordiv
            else:
                assert False

            monkey_dict[m[0]] = Monkey(m[0], None, op, m[1], m[3])
        else:
            assert False

    return monkey_dict


def part1(monkey_dict):
    monkey_dict = copy.deepcopy(monkey_dict)
    for m in monkey_dict.values():
        m.register(monkey_dict)
    for m in monkey_dict.values():
        m.yell()
    return monkey_dict["root"].number


def part2(monkey_dict):
    human_name = "humn"
    monkey_dict = copy.deepcopy(monkey_dict)
    monkey_dict["root"].operation = operator.eq
    for m in monkey_dict.values():
        m.register(monkey_dict)
    for m in monkey_dict.values():
        if m.name != human_name:
            m.yell()

    # Lets backtrack from the root to 'humn'
    node_name = "root"
    value = True
    while node_name != human_name:
        node = monkey_dict[node_name]

        assert (node.arg1_number is None) != (node.arg2_number is None)
        if node.arg1_number is None:
            if node.operation == operator.eq:
                value = node.arg2_number
                node_name = node.arg1
            elif node.operation == operator.floordiv:
                value = node.arg2_number * value
                node_name = node.arg1
            elif node.operation == operator.add:
                value = value - node.arg2_number
                node_name = node.arg1
            elif node.operation == operator.mul:
                value = value // node.arg2_number
                node_name = node.arg1
            elif node.operation == operator.sub:
                value = value + node.arg2_number
                node_name = node.arg1
        else:
            if node.operation == operator.eq:
                value = node.arg1_number
                node_name = node.arg2
            elif node.operation == operator.floordiv:
                value = node.arg1_number // value
                node_name = node.arg2
            elif node.operation == operator.add:
                value = value - node.arg1_number
                node_name = node.arg2
            elif node.operation == operator.mul:
                value = value // node.arg1_number
                node_name = node.arg2
            elif node.operation == operator.sub:
                value = node.arg1_number - value
                node_name = node.arg2
            else:
                print(node.name, node.operation)
                assert False

    assert node_name == human_name
    return value


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == 152

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))

    assert part2(test_input) == 301
    print(part2(input_data))


if __name__ == "__main__":
    main()
