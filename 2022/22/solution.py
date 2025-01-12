import itertools
import math
import typing

DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]
NUM_DIRECTIONS = 4


def parse(path):
    with open(path) as f:
        txt = f.read().split("\n\n")
        instructions = txt[1].strip()
        grid = txt[0].splitlines()
        max_length = max(len(l) for l in grid)
        grid = [l + " " * (max_length - len(l)) for l in grid]

        instructions = [
            "".join(v[1])
            for v in itertools.groupby(instructions, key=lambda c: c.isdigit())
        ]
        instructions = [int(v) if v[0].isdigit() else v for v in instructions]
    return instructions, grid


def part1(instructions, grid):
    num_rows = len(grid)
    num_col = len(grid[0])
    coord = [0, grid[0].index(".")]
    dindex = 0
    direction = DIRECTIONS[dindex]
    for instr in instructions:
        if instr == "R":
            dindex = (dindex + 1) % NUM_DIRECTIONS
            direction = DIRECTIONS[dindex]
        elif instr == "L":
            dindex = (dindex - 1) % NUM_DIRECTIONS
            direction = DIRECTIONS[dindex]
        else:
            for _ in range(instr):
                next_coord = list(coord)
                next_coord[0] = (next_coord[0] + direction[0]) % num_rows
                next_coord[1] = (next_coord[1] + direction[1]) % num_col

                if grid[next_coord[0]][next_coord[1]] == ".":
                    coord = next_coord
                elif grid[next_coord[0]][next_coord[1]] == "#":
                    break
                else:
                    while grid[next_coord[0]][next_coord[1]] == " ":
                        next_coord[0] = (next_coord[0] + direction[0]) % num_rows
                        next_coord[1] = (next_coord[1] + direction[1]) % num_col
                    if grid[next_coord[0]][next_coord[1]] == ".":
                        coord = next_coord
                    elif grid[next_coord[0]][next_coord[1]] == "#":
                        break

    print(coord)
    return (coord[0] + 1) * 1000 + 4 * (coord[1] + 1) + dindex


class CubeFace(typing.NamedTuple):
    value: int
    rotation: int


# Define how the edges of a right handed dice touch, and under which angle/rotation.
CUBE_EDGES = {
    CubeFace(value=1, rotation=0): CubeFace(value=2, rotation=3),
    CubeFace(value=1, rotation=1): CubeFace(value=4, rotation=2),
    CubeFace(value=1, rotation=2): CubeFace(value=5, rotation=3),
    CubeFace(value=2, rotation=0): CubeFace(value=3, rotation=3),
    CubeFace(value=2, rotation=1): CubeFace(value=6, rotation=2),
    CubeFace(value=3, rotation=0): CubeFace(value=1, rotation=3),
    CubeFace(value=3, rotation=1): CubeFace(value=5, rotation=2),
    CubeFace(value=3, rotation=2): CubeFace(value=6, rotation=3),
    CubeFace(value=4, rotation=0): CubeFace(value=6, rotation=1),
    CubeFace(value=4, rotation=3): CubeFace(value=2, rotation=2),
    CubeFace(value=5, rotation=0): CubeFace(value=4, rotation=1),
    CubeFace(value=5, rotation=1): CubeFace(value=6, rotation=0),
}
CUBE_EDGES = CUBE_EDGES | dict((v, k) for k, v in CUBE_EDGES.items())


def fold_cube(grid):
    """BFS to figure out which block corresponds to which number and rotation of the cube."""
    grid_res = int(math.sqrt(sum([c != " " for l in grid for c in l]) // 6))
    n = len(grid) // grid_res
    m = len(grid[0]) // grid_res
    blocks = [
        (i, j)
        for i in range(n)
        for j in range(m)
        if grid[i * grid_res][j * grid_res] != " "
    ]

    block_assignments = {blocks[0]: CubeFace(1, 0)}
    q = [blocks[0]]

    while q:
        block = q.pop(0)
        face = block_assignments[block]
        (block_i, block_j) = block
        for epi, (delta_i, delta_j) in enumerate(DIRECTIONS):
            next_block = (block_i + delta_i, block_j + delta_j)
            if next_block not in blocks or next_block in block_assignments:
                continue

            ni, nj = next_block
            if not ((0 <= ni < n) and (0 <= nj < m)):
                continue

            active_edge = CubeFace(face.value, (epi + face.rotation) % NUM_DIRECTIONS)
            nm = CUBE_EDGES[active_edge]
            r = (nm.rotation - epi + 2) % NUM_DIRECTIONS

            block_assignments[next_block] = CubeFace(nm.value, r)
            q.append(next_block)

    inv_block_assignments = {v.value: k for k, v in block_assignments.items()}
    return block_assignments, inv_block_assignments, grid_res


def part2(instructions, grid):
    block_assignments, inv_block_assignments, grid_res = fold_cube(grid)
    blocks = sorted(block_assignments)
    current_block = blocks[0]
    coord, dindex = [current_block[0] * grid_res, current_block[1] * grid_res], 0

    for instr in instructions:
        if instr == "R":
            dindex = (dindex + 1) % NUM_DIRECTIONS
        elif instr == "L":
            dindex = (dindex - 1) % NUM_DIRECTIONS
        else:
            for _ in range(instr):
                direction = DIRECTIONS[dindex]
                next_coord = list(coord)
                next_coord[0] = next_coord[0] + direction[0]
                next_coord[1] = next_coord[1] + direction[1]
                next_block = (next_coord[0] // grid_res, next_coord[1] // grid_res)
                next_dindex = dindex

                if next_block != current_block:
                    current_assignment = block_assignments[current_block]

                    exit_edge = CubeFace(
                        current_assignment.value,
                        (current_assignment.rotation + dindex) % NUM_DIRECTIONS,
                    )
                    next_edge = CUBE_EDGES[exit_edge]

                    next_block = inv_block_assignments[next_edge.value]
                    next_assignment = block_assignments[next_block]

                    next_dindex = (
                        next_edge.rotation - next_assignment.rotation + 2
                    ) % NUM_DIRECTIONS
                    block_coord = [
                        coord[0] - current_block[0] * grid_res,
                        coord[1] - current_block[1] * grid_res,
                    ]

                    if dindex == 0:
                        rel_coord = block_coord[0]
                    elif dindex == 1:
                        rel_coord = grid_res - 1 - block_coord[1]
                    elif dindex == 2:
                        rel_coord = grid_res - 1 - block_coord[0]
                    else:
                        rel_coord = block_coord[1]

                    if next_dindex == 0:
                        next_coord = (rel_coord, 0)
                    elif next_dindex == 1:
                        next_coord = (0, grid_res - 1 - rel_coord)
                    elif next_dindex == 2:
                        next_coord = (grid_res - 1 - rel_coord, grid_res - 1)
                    else:
                        next_coord = (grid_res - 1, rel_coord)

                    next_coord = [
                        next_block[0] * grid_res + next_coord[0],
                        next_block[1] * grid_res + next_coord[1],
                    ]

                if grid[next_coord[0]][next_coord[1]] == ".":
                    coord = next_coord
                    current_block = next_block
                    dindex = next_dindex
                elif grid[next_coord[0]][next_coord[1]] == "#":
                    break
                else:
                    raise Exception("Something unexpected happened!")

    return 1000 * (coord[0] + 1) + 4 * (coord[1] + 1) + dindex


def main():
    test_instructions, test_grid = parse("test_input.dat")
    instructions, grid = parse("input.dat")
    print(fold_cube(grid))

    assert part1(test_instructions, test_grid) == 6032
    print(part1(instructions, grid))

    assert part2(test_instructions, test_grid) == 5031
    print(part2(instructions, grid))


if __name__ == "__main__":
    main()
