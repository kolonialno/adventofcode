instructions = [(line[0], int(line[1:])) for line in open("instructions.txt").read().strip().split("\n")]

# Part 1
# direction = 90 # E
# pos = (0, 0)
# for command, unit in instructions:
#     if command == "N" or (command == "F" and direction == 0):
#         pos = (pos[0] + unit, pos[1])
#     elif command == "S" or (command == "F" and direction == 180):
#         pos = (pos[0] - unit, pos[1])
#     elif command == "E" or (command == "F" and direction == 90):
#         pos = (pos[0], pos[1] + unit)
#     elif command == "W" or (command == "F" and direction == 270):
#         pos = (pos[0], pos[1] - unit)
#     elif command == "L":
#         direction = (direction - unit) % 360
#     elif command == "R":
#         direction = (direction + unit) % 360
# print(abs(pos[0]) + abs(pos[1]))

# Part 2
pos = (0, 0)
waypoint = (1, 10)
for command, unit in instructions:
    if command == "N":
        waypoint = (waypoint[0] + unit, waypoint[1])
    elif command == "S":
        waypoint = (waypoint[0] - unit, waypoint[1])
    elif command == "E":
        waypoint = (waypoint[0], waypoint[1] + unit)
    elif command == "W":
        waypoint = (waypoint[0], waypoint[1] - unit)
    elif command == "L":
        for i in range(unit // 90):
            waypoint = (waypoint[1], waypoint[0] * -1)
    elif command == "R":
        for i in range(unit // 90):
            waypoint = (waypoint[1] * -1, waypoint[0])
    elif command == "F":
        pos = (pos[0] + (waypoint[0] * unit), pos[1] + (waypoint[1] * unit))

print(abs(pos[0]) + abs(pos[1]))
