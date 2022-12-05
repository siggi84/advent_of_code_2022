import pathlib
import copy
from dataclasses import dataclass


@dataclass
class Instruction:
    num: int
    source: int
    destination: int


def part1(stack, instructions):
    stack = copy.deepcopy(stack)
    for i in instructions:
        for _ in range(i.num):
            stack[i.destination].append(stack[i.source].pop())

    result = "".join(s[-1] for s in stack)
    return result


def part2(stack, instructions):
    stack = copy.deepcopy(stack)
    for i in instructions:
        stack[i.destination] += stack[i.source][-i.num :]
        stack[i.source] = stack[i.source][: -i.num]

    result = "".join(s[-1] for s in stack)
    return result


def main():
    input = pathlib.Path("input.dat")
    text = input.read_text().split("\n")
    split_line = text.index("")
    num_crates = 9
    stacks = [[] for _ in range(9)]
    for l in text[: split_line - 1]:
        for c in range(0, num_crates):
            entry = l[1 + c * 4]
            if entry.strip():
                stacks[c].append(entry)
    stacks = [s[::-1] for s in stacks]

    instructions = []
    for l in text[split_line + 1 :]:
        if not l:
            continue
        splitted = l.split(" ")
        instructions.append(
            Instruction(int(splitted[1]), int(splitted[3]) - 1, int(splitted[5]) - 1)
        )

    print(part1(stacks, instructions))
    print(part2(stacks, instructions))


if __name__ == "__main__":
    main()
