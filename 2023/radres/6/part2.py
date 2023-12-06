from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

lines = out.splitlines()
# lines = """Time:      7  15   30
# Distance:  9  40  200""".splitlines()
time = []
dist = []

for c in lines[0].split(":")[1]:
    if c.isdigit():
        time.append(c)
time = int("".join(time))

for c in lines[1].split(":")[1]:
    if c.isdigit():
        dist.append(c)
dist = int("".join(dist))

total_ways = 1
for race in [(time,dist)]:
    speed = 1
    can_beat = False
    time, dist = race
    ways = 0
    while speed < time:
        if dist/speed < (time-speed):
            ways += 1
        speed += 1

    total_ways *= ways

print(total_ways)
