from sys import stdin

lines = [line.strip() for line in stdin]


def most_and_least_common(bit_strings):
    data = {}
    for binary in bit_strings:
        for i, v in enumerate(binary):
            if i not in data:
                data[i] = [0, 0]
            data[i][int(v)] += 1

    most, least = [], []
    for i in data.keys():
        most.append(1 if data[i][1] >= data[i][0] else 0)
        least.append(1 if data[i][1] < data[i][0] else 0)

    return most, least


def a():
    most, least = most_and_least_common(lines)

    gamma = "".join([str(x) for x in most])
    epsilon = "".join([str(x) for x in least])

    return int(gamma, base=2) * int(epsilon, base=2)


def b():
    def _filter(criteria):
        length = len(lines[0])
        remaining = {line for line in lines}
        for i in range(length):
            criterias = most_and_least_common(remaining)

            to_remove = {
                line for line in remaining if int(line[i]) != criterias[criteria][i]
            }

            if len(remaining) == 1:
                break

            remaining = remaining - to_remove

        return remaining.pop()

    oxygen = _filter(0)
    scrubber = _filter(1)

    return int(oxygen, base=2) * int(scrubber, base=2)


print(a())
print(b())
