import pathlib

def main():
    input = pathlib.Path("input.dat")
    entries = input.read_text().split("\n")
    calorie_sums = [0]
    for e in entries:
        if e == "":
            calorie_sums.append(0)
            continue
        else:
            calorie_sums[-1] += int(e)
        
    calorie_sums = sorted(calorie_sums)
    print(calorie_sums[-1])
    print(sum(calorie_sums[-3:]))

if __name__ == "__main__":
    main()
