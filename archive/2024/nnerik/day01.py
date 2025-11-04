from collections import defaultdict

# Read input
input = open(0).read().strip().split("\n")
left_list, right_list = [], []
for line in input:
    a, b = line.split()
    left_list.append(int(a))
    right_list.append(int(b))

# Part 1
distance = 0
for left, right in zip(sorted(left_list), sorted(right_list)):
    distance += abs(left - right)

print("Part 1:", distance)


# Part 2
def get_counts(data):
    counts = defaultdict(int)
    for item in data:
        counts[item] += 1
    return counts


similarity = 0
left_counts = get_counts(left_list)
right_counts = get_counts(right_list)
for location_id, count in left_counts.items():
    similarity += location_id * count * right_counts[location_id]

print("Part 2:", similarity)
