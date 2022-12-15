import re
import tqdm


def manhatten_dist(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)


def get_x_range_covered(sensor, y, x_min, x_max):

    d = manhatten_dist(*sensor)
    dy = abs(sensor[1] - y)

    if dy > d:
        return None

    xr_start = sensor[0] - (d - dy)
    xr_end = sensor[0] + (d - dy)

    if xr_start > x_max and xr_end > x_max:
        return None

    if xr_end < xr_start:
        return None

    if xr_start < x_min:
        xr_start = x_min
    if xr_end > x_max:
        xr_end = x_max

    return (xr_start, xr_end)


def get_sensors_or_beacons(sensors, y):

    n = set()
    for sensor in sensors:
        if y == sensor[1]:
            n.add((sensor[0], sensor[1]))
        if y == sensor[3]:
            n.add((sensor[2], sensor[3]))
    return len(n)


def distress_signal(x, y):
    return x * 4000000 + y


def get_xranges_covered(sensors, y, x_min, x_max):
    x_ranges = []
    for sensor in sensors:
        x_range = get_x_range_covered(sensor, y, x_min, x_max)
        if x_range is not None:
            x_ranges.append(x_range)
    return x_ranges


def get_num_covered_positions(sensors, y, x_min, x_max):
    ranges = get_xranges_covered(sensors, y, x_min, x_max)
    ranges = sorted(ranges, key=lambda x: (x[0], x[1]))

    lengths = []
    current_start, current_end = ranges[0]
    for i in range(1, len(ranges)):
        if ranges[i][0] > current_end:
            lengths.append(current_end - current_start + 1)
            current_start, current_end = ranges[i]
        else:
            current_end = max(current_end, ranges[i][1])
    lengths.append(current_end - current_start + 1)

    num_sensors_or_beacons = get_sensors_or_beacons(sensors, y)
    return sum(lengths) - num_sensors_or_beacons


def get_hole(ranges, x_min, x_max):
    if ranges[0][0] > x_min:
        return x_min

    if max([x for r in ranges for x in r]) < x_max:
        return x_max

    current_x = ranges[0][1]

    for i in range(1, len(ranges)):
        if current_x < ranges[i][0]:
            return current_x + 1
        current_x = max(current_x, ranges[i][1])

    return None


def find_possible_location(sensors, x_min, x_max):
    x_coverage = []
    for y in tqdm.tqdm(range(x_max + 1)):
        x_ranges = get_xranges_covered(sensors, y, x_min, x_max)
        x_coverage.append(x_ranges)

    for y in tqdm.tqdm(range(len(x_coverage))):
        sorted_xrange = sorted(x_coverage[y], key=lambda x: (x[0], x[1]))
        x = get_hole(sorted_xrange, x_min, x_max)
        if x:
            return distress_signal(x, y)


sensors = []

with open("input.txt") as f:
    for l in f.read().splitlines():
        x1, x2 = re.findall(r"x=(-?\d*)", l)
        y1, y2 = re.findall(r"y=(-?\d*)", l)
        sensors.append((int(x1), int(y1), int(x2), int(y2)))


print(get_num_covered_positions(sensors, 2000000, float("-inf"), float("inf")))
print(find_possible_location(sensors, 0, 4000000))
