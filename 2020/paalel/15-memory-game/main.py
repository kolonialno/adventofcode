def gen_next_list_element(list):
    last_element = number_list[-1]
    if last_element not in number_list[:-1]:
        return 0

    return number_list[-2::-1].index(last_element) + 1


def start_sequence_repeated(start_list, number_list):
    l = -1 * len(start_list)
    return number_list[l:] == start_list


def part1(N, number_list):
    """
    Generate list by:
     - Adding 0 if the last_element has not been seen before.
     - Steps since last seen if seen before.

    return element N.
    """

    for _ in range(N - len(number_list)):
        number_list.append(gen_next_list_element(number_list))

    return number_list[-1]


def part2(N, number_list):
    """
    Generate list by:
     - Adding 0 if the last_element has not been seen before.
     - Steps since last seen if seen before.

    return element N. N is now so large that we have to look for repeating
    patterns.
    """

    start_list = list(number_list)
    start_list = [20, 0, 1, 11, 6, 3]

    for _ in range(N - len(number_list)):
        number_list.append(gen_next_list_element(number_list))
        if start_sequence_repeated(start_list, number_list):
            print("vreak")
            break

    print(start_list)
    print("------------------------------------")
    print(number_list[-20:])


if __name__ == "__main__":
    N = 2020
    number_list = [20, 0, 1, 11, 6, 3]
    print("result part 1: ", part1(N, number_list))
    part2(50000, number_list)
