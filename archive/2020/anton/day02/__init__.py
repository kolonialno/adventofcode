from collections import Counter


def parse_entries() -> list:
    with open("inputs/02.txt") as in_file:
        rows = in_file.readlines()
    split_rows = [x.split(" ") for x in rows]
    entries = [
        (tuple(map(int, x.split("-"))), y[0], z.strip()) for (x, y, z) in split_rows
    ]
    return entries


def does_password_conform_sled(password_entry) -> bool:
    char_range, char, password = password_entry
    lower, upper = char_range
    char_counts = Counter(password)
    return lower <= char_counts[char] <= upper


def does_password_conform_toboggan(password_entry) -> bool:
    char_positions, char, password = password_entry
    first, second = char_positions
    return (password[first - 1] == char) != (password[second - 1] == char)


def main() -> list:
    entries = parse_entries()
    return len([x[2] for x in filter(does_password_conform_sled, entries)])


def secondary() -> list:
    entries = parse_entries()
    return len([x[2] for x in filter(does_password_conform_toboggan, entries)])
