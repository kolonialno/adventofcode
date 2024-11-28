from pathlib import Path


def main(lines, test_bag):
    id_sum = 0
    power_sum = 0
    for line in lines:
        bag = {}
        game_key, game_value = line.split(": ")
        sets = game_value.split("; ")
        valid = True

        for s in sets:
            cubes = s.split(", ")
            for cube in cubes:
                key, value = cube.split(" ")
                if int(key) > bag.get(value, 0):
                    bag[value] = int(key)

                if int(key) > test_bag[value]:
                    valid = False

            power = 1
            for val in bag.values():
                power *= val

        power_sum += power
        if valid:
            id_sum += int(game_key.split(" ")[1])
    print(id_sum, power_sum)


if __name__ == "__main__":
    with open(Path(__file__).parent / "input_test1.txt") as f:
        lines = [x.strip("\n") for x in f.readlines()]
    main(lines, {"red": 12, "green": 13, "blue": 14})

    with open(Path(__file__).parent / "input.txt") as f:
        lines = [x.strip("\n") for x in f.readlines()]
    main(lines, {"red": 12, "green": 13, "blue": 14})
