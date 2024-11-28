from pathlib import Path

word_digits = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
}


def main(lines):
    sum = 0
    for line in lines:
        number = [None, None]
        string_left = ""
        string_right = ""
        for i in range(len(line)):
            if not number[0]:
                try:
                    number_left = int(line[i])
                    number[0] = str(number_left)
                except ValueError:
                    pass

                string_left += line[i]
                for word, value in word_digits.items():
                    if word in string_left:
                        number[0] = value
                        break

            if not number[1]:
                try:
                    number_right = int(line[len(line) - i - 1])
                    number[1] = str(number_right)
                except ValueError:
                    pass

                string_right = line[len(line) - i - 1] + string_right
                for word, value in word_digits.items():
                    if word in string_right:
                        number[1] = value
                        break

            if number[0] and number[1]:
                break
        sum += int("".join(number))
    print(sum)


if __name__ == "__main__":
    with open(Path(__file__).parent / "input_test1.txt") as f:
        lines = [x.strip("\n") for x in f.readlines()]
    main(lines)

    with open(Path(__file__).parent / "input.txt") as f:
        lines = [x.strip("\n") for x in f.readlines()]
    main(lines)

    with open(Path(__file__).parent / "input_test2.txt") as f:
        lines = [x.strip("\n") for x in f.readlines()]
    main(lines)
