import networkx as nx


def get_graph(path):
    with open(path) as f:
        data = [l.strip() for l in f.readlines()]
    n, m = len(data), len(data[0])
    G = nx.DiGraph()
    for i in range(n):
        for j in range(m):
            G.add_node(i * m + j, value=data[i][j])

    start, end = None, None
    a_nodes = []
    for i in range(n):
        for j in range(m):
            idx = i * m + j
            c = data[i][j]
            co = ord(data[i][j])
            if c == "S":
                start = idx
                c = "a"
                co = ord("a")
            elif c == "E":
                end = idx
                c = "z"
                co = ord("z")
            elif c == "a":
                a_nodes.append(idx)

            steps = [(-1, 0), (1, 0), (0, -1), (0, 1)]
            for di, dj in steps:
                ik, jk = i + di, j + dj
                kidx = ik * m + jk
                if not (0 <= ik < n):
                    continue
                if not (0 <= jk < m):
                    continue

                ck = data[ik][jk]
                cok = ord(data[ik][jk])
                if ck == "S":
                    ck = "a"
                    cok = ord("a")
                elif ck == "E":
                    ck = "z"
                    cok = ord("z")

                if cok - co < 2:
                    G.add_edge(idx, kidx)

    assert start is not None
    assert end is not None

    return G, start, end, a_nodes


def part1(path):
    G, start, end, _ = get_graph(path)
    res = nx.shortest_path(G, source=start, target=end)

    return len(res) - 1


def part2(path):
    G, start, end, start_nodes = get_graph(path)
    path_lengths = []
    for start in start_nodes:
        try:
            res = nx.shortest_path(G, source=start, target=end)
            path_lengths.append(len(res) - 1)
        except nx.exception.NetworkXNoPath:
            pass

    return min(path_lengths)


def main():
    test_input_file = "test_input.dat"
    assert part1(test_input_file) == 31
    input_file = "input.dat"
    print(part1(input_file))

    assert part2(test_input_file) == 29
    print(part2(input_file))


if __name__ == "__main__":
    main()
