import pathlib

import numpy as np


def left_only(input):
    n, _ = input.shape
    left = np.zeros(input.shape) - 1
    for i in range(1, n):
        left[:, i] = np.maximum(input[:, i - 1], left[:, i - 1])
    return left


def part1(input):
    left = left_only(input)
    right = np.fliplr(left_only(np.fliplr(input)))
    top = left_only(input.T).T
    bottom = np.fliplr(left_only(np.fliplr(input.T))).T

    minv = np.min([left, right, top, bottom], axis=0)
    return np.sum(minv < input)


def part2(input):
    def helper(inp):
        n, m = inp.shape
        view = np.zeros(inp.shape, dtype=int)
        for i in range(n - 1):
            for j in range(m):
                base = inp[i, j]
                s = 1
                while s + i < n - 1 and inp[s + i, j] < base:
                    s += 1
                view[i, j] = s
        return view

    down = helper(input)
    up = np.flipud(helper(np.flipud(input)))
    right = helper(input.T).T
    left = np.flipud(helper(np.flipud(input.T))).T

    return np.max(np.prod([down, up, left, right], axis=0))


def parse(path):
    data = []
    with open(path) as f:
        for line in f.readlines():
            data.append([int(v) for v in line.strip()])
    return np.array(data)


def main():
    input_file = parse("input.dat")
    test_input_file = parse("test_input.dat")

    assert part1(test_input_file) == 21
    print(part1(input_file))

    assert part2(test_input_file) == 8
    print(part2(input_file))


if __name__ == "__main__":
    main()
