from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

lines = out.splitlines()
times = extract_integers_from_text(lines[0])
distances = extract_integers_from_text(lines[1])

races_data = [(times[i], distances[i]) for i in range(len(times))]

total_ways = 1
for race in races_data:
    speed = 1
    can_beat = False
    time, dist = race
    ways = 0
    while speed < time:
        if dist/speed < (time-speed):
            ways += 1
        speed += 1

    print(ways)
    total_ways *= ways

print(total_ways)
