import pathlib
from dataclasses import dataclass
from typing import Optional


class Directory:
    name: str
    parent: "Directory"
    sub_dirs: dict[str, "Directory"]
    files: dict[str, "File"]

    def __init__(self, name: str, parent: Optional["Directory"] = None):
        self.name: str = name
        self.parent: "Directory" = parent if parent else self
        self.sub_dirs: dict[str, "Directory"] = {}
        self.files: dict[str, "File"] = {}

    @property
    def size(self):
        total_files_size = sum(n.size for n in self.files.values())
        total_sub_dir_size = sum(n.size for n in self.sub_dirs.values())
        return total_files_size + total_sub_dir_size


@dataclass
class File:
    name: str
    parent: "Directory"
    size: int


def parse(input_data):
    lines = [l for l in input_data.split("\n") if l]
    root = Directory("", None)
    root.parent = root

    current: Directory = root
    assert lines[0] == "$ cd /"
    for line in lines[1:]:
        line_splitted = line.split()
        if line == "$ ls":
            continue

        if line_splitted[0].isnumeric():
            size, name = int(line_splitted[0]), line_splitted[1]
            assert name not in current.sub_dirs
            file = File(name, current, size)
            current.files[name] = file
        elif line.startswith("dir"):
            _, name = line_splitted
            assert name not in current.sub_dirs
            directory = Directory(name, current)
            current.sub_dirs[name] = directory
        elif line == "$ cd ..":
            assert current != root
            current = current.parent
        elif line.startswith("$ cd"):
            _, _, dir_name = line_splitted
            assert dir_name in current.sub_dirs
            current = current.sub_dirs[dir_name]
        else:
            raise Exception()

    return root


def part1(input_data):
    def helper(directory: Directory) -> int:
        size = 0
        for sub_dir in directory.sub_dirs.values():
            if sub_dir.size <= 100_000:
                size += sub_dir.size
            size += helper(sub_dir)
        return size

    root = parse(input_data)
    return helper(root)


def part2(input_data):
    root = parse(input_data)
    total_size = 70000000
    update_size = 30000000
    to_delete = update_size - (total_size - root.size)

    def helper(directory):
        min_size = 1e100
        if to_delete < directory.size < min_size:
            min_size = directory.size

        for sub_dir in directory.sub_dirs.values():
            sub_dir_min_size = helper(sub_dir)
            if sub_dir_min_size < min_size:
                min_size = sub_dir_min_size
        return min_size

    return helper(root)


def main():
    input_data = pathlib.Path("input.dat").read_text()
    test_input_data = pathlib.Path("test_input.dat").read_text()

    assert part1(test_input_data) == 95437
    print(part1(input_data))

    assert part2(test_input_data) == 24933642
    print(part2(input_data))


if __name__ == "__main__":
    main()
