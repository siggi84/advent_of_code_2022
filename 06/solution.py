import pathlib


def solution(buffer, message_length=4):
    n = len(buffer)
    for i in range(0, n - message_length):
        m = len(set(buffer[i : i + message_length]))
        if m == message_length:
            return i + message_length


def main():
    input = pathlib.Path("input.dat").read_text()

    assert solution("bvwbjplbgvbhsrlpgdmjqwftvncz") == 5
    assert solution("nppdvjthqldpwncqszvftbrmjlhg") == 6
    assert solution("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 10
    assert solution("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 11
    print(solution(input))

    assert solution("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) == 19
    assert solution("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) == 23
    assert solution("nppdvjthqldpwncqszvftbrmjlhg", 14) == 23
    assert solution("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) == 29
    assert solution("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) == 26
    print(solution(input, 14))


if __name__ == "__main__":
    main()
