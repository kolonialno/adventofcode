import numpy as np

from collections import defaultdict

units = [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]]
rotations = [
    np.array([a, b, c])
    for a in units
    for b in units
    for c in units
    if np.linalg.det(np.array([a, b, c])) == 1
]


def match(known, data):
    for rot in rotations:
        candidate = [rot.dot(e) for e in data]
        matches = defaultdict(int)
        for scanner in (k - c for k in known for c in candidate):
            pos = tuple(scanner)
            matches[pos] += 1
            if matches[pos] == 12:
                return [beacon + scanner for beacon in candidate], scanner
    return None, None


with open("input.txt") as f:
    scanners = []
    while f.readline():
        beacons = []
        while line := f.readline().strip():
            beacons.append(np.array([int(x) for x in line.split(",")]))
        scanners.append(beacons)

beacon_map = {tuple(beacon) for beacon in scanners[0]}
scanner_map = [np.array([0, 0, 0])]

todo, rest = [scanners[0]], scanners[1:]
while rest:
    known = todo.pop()
    to_remove = []
    for n, data in enumerate(rest):
        beacons, scanner = match(known, data)
        if beacons is not None:
            todo.append(beacons)
            beacon_map |= {tuple(beacon) for beacon in beacons}
            to_remove.append(n)
            scanner_map.append(scanner)
    for n in reversed(to_remove):
        rest = rest[0:n] + rest[n + 1 :]

print("Part 1:", len(beacon_map))

dist = 0
for i, s1 in enumerate(scanner_map):
    for s2 in scanner_map[i + 1 :]:
        dist = max(dist, int(np.linalg.norm(s1 - s2, ord=1)))

print("Part 2:", dist)
