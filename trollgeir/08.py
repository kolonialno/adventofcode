def parse_segments(segments):
    return [frozenset(i) for i in segments.split()]


def create_decoding_dict(segments):
    encoding = {}
    for segment in segments:
        if len(segment) == 2:
            encoding[1] = segment
        if len(segment) == 3:
            encoding[7] = segment
        if len(segment) == 4:
            encoding[4] = segment
        if len(segment) == 7:
            encoding[8] = segment

    # Deduce E+G segment as a tool for further deduction
    e_g = (encoding[7] | encoding[4]) ^ encoding[8]

    v0_6_9 = [i for i in segments if len(i) == 6]
    for segment in v0_6_9:
        if not encoding[1] < segment:
            encoding[6] = segment
        else:  # 0 or 9
            if e_g < segment:
                encoding[0] = segment
            else:
                encoding[9] = segment

    v2_3_5 = [i for i in segments if len(i) == 5]
    for segment in v2_3_5:
        if encoding[1] < segment:
            encoding[3] = segment
        else:  # 2 or 5
            if e_g < segment:
                encoding[2] = segment
            else:
                encoding[5] = segment

    # reverse encoding
    decoding = dict((v, k) for k, v in encoding.items())
    return decoding


# Puzzle1
count = 0
with open("inputs/08.txt") as f:
    for line in f.readlines():
        output_segments = parse_segments(line.split("|")[1])
        for segment in output_segments:
            if len(segment) in [2, 3, 4, 7]:
                count += 1

print("Answer1:", count)

# Puzzle2
summed_output = 0
with open("inputs/08.txt") as f:
    for line in f.readlines():
        input_segments, output_segments = map(parse_segments, line.split("|"))
        d = create_decoding_dict(input_segments)
        summed_output += int("".join([str(d[i]) for i in output_segments]))


print("Answer2:", summed_output)
