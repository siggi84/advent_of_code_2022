import functools
import itertools

import networkx as nx
from networkx.classes.function import subgraph


def parse(path):
    adjacency_list = {}
    rates = {}
    with open(path) as f:
        for l in f.readlines():
            ls = l.split()
            valve = ls[1]
            rate = int(ls[4].split("=")[1][:-1])
            neighbours = [v.replace(",", "") for v in ls[9:]]
            adjacency_list[valve] = neighbours
            rates[valve] = rate

    # Make sure it is non-directional
    n = len(adjacency_list)
    G = nx.Graph()
    for k in adjacency_list:
        G.add_node(k)
    for k, v in adjacency_list.items():
        for nn in v:
            G.add_edge(k, nn)

    all_shp = dict(nx.all_pairs_shortest_path_length(G))
    nodes_of_interest = ["AA"] + [k for k, r in rates.items() if r > 0]
    subgraph = {
        k: {n: v for n, v in all_shp[k].items() if n in nodes_of_interest and n != k}
        for k in nodes_of_interest
    }
    return subgraph, rates


def part1(al, rates):
    @functools.lru_cache()
    def helper(valve, tl, active):
        if tl < -1:
            return 0

        nbs = set(al[valve]) - active
        best_res = 0
        for nb in nbs:
            dt = al[valve][nb]
            if rates[valve]:
                sub_res = helper(nb, tl - 1 - dt, active | frozenset([valve]))
            else:
                sub_res = helper(nb, tl - dt, active | frozenset([valve]))
            if sub_res > best_res:
                best_res = sub_res

        return best_res + (tl - 1) * rates[valve]

    return helper("AA", 30, frozenset())


def part2(al, rates):
    @functools.lru_cache()
    def helper(valve, tl, active):
        if tl < -1:
            return 0

        nbs = set(al[valve]) - active
        best_res = 0
        for nb in nbs:
            dt = al[valve][nb]
            if rates[valve]:
                sub_res = helper(nb, tl - 1 - dt, active | frozenset([valve]))
            else:
                sub_res = helper(nb, tl - dt, active | frozenset([valve]))
            if sub_res > best_res:
                best_res = sub_res

        return best_res + (tl - 1) * rates[valve]

    valves = set(al.keys()) - {"AA"}
    n = len(valves)
    best_combination = 0
    for r in range(0, n // 2 + 2):
        for c in itertools.combinations(valves, r):
            me = helper("AA", 26, frozenset(c))
            elephant = helper("AA", 26, frozenset(valves - frozenset(c)))
            total = me + elephant
            if best_combination < total:
                best_combination = total

    return best_combination


def main():
    test_input_file = "test_input.dat"
    test_input = parse(test_input_file)
    assert part1(*test_input) == 1651

    input_file = "input.dat"
    input_data = parse(input_file)
    print(part1(*input_data))

    assert part2(*test_input) == 1707
    print(part2(*input_data))


if __name__ == "__main__":
    main()
