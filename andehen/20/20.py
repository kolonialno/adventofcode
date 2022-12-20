def mix_numbers(numbers, n_times=1, decryption_key=1):

    decrypted_numbers = [n * decryption_key for n in numbers]
    N = len(numbers)

    positions = [(i, n) for i, n in enumerate(decrypted_numbers)]

    for t in range(n_times):
        for i, n in enumerate(decrypted_numbers):
            current_position = positions.index((i, n))
            del positions[current_position]
            new_position = (n + current_position) % (N - 1)

            # Wrap at 0
            if new_position == 0:
                new_position = N - 1

            positions.insert(new_position, (i, n))

    return [n for _, n in positions]


def get_groove_cooridnate(mix_numbers):

    N = len(mix_numbers)

    zero_index = mix_numbers.index(0)

    return sum([mix_numbers[(nth + zero_index) % N] for nth in [1000, 2000, 3000]])


with open("input.txt") as f:
    numbers = [int(n) for n in f.read().splitlines()]

print(get_groove_cooridnate(mix_numbers(numbers, n_times=1, decryption_key=1)))
print(get_groove_cooridnate(mix_numbers(numbers, n_times=10, decryption_key=811589153)))
