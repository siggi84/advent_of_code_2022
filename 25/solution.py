def parse(path):
    with open(path) as f:
        data = [l.strip() for l in f.readlines()]
    return data


def snafu_to_int(snafu):
    numbers = {"1": 1, "2": 2, "0": 0, "-": -1, "=": -2}
    sum = 0
    for i, c in enumerate(reversed(snafu)):
        sum += numbers[c] * 5**i
    return sum


def int_to_snafu(v):
    q = v
    sn = ""
    while True:
        m = q % 5
        q = q // 5
        if m == 0:
            sn += "0"
        elif m == 1:
            sn += "1"
        elif m == 2:
            sn += "2"
        elif m == 3:
            sn += "="
            q += 1
        elif m == 4:
            sn += "-"
            q += 1
        if q == 0:
            break
    return sn[::-1]


def part1(ns):
    sum = 0
    for n in ns:
        sum += snafu_to_int(n)
    return int_to_snafu(sum)


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(test_input) == "2=-1=0"

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(input_data))


if __name__ == "__main__":
    main()
