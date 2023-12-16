from helpers import *
import os
import time
import copy
with open("input.txt", "r") as f:
    out = f.read()

# out = r""".|...\....
# |.-.\.....
# .....|-...
# ........|.
# ..........
# .........\
# ..../.\\..
# .-.-/..|..
# .|....-|.\
# ..//.|...."""

def manhattan_distance(p1, p2):
    return abs(p1[0]-p2[0]) + abs(p1[1]-p2[1])
class Light:
    def __init__(self, pos=(0,0), direction=(0,1)):
        self.pos = pos
        self.dir = direction

    def rotate_clockwise(self):
        if self.dir == (0,1):
            self.dir = (1,0)
        elif self.dir == (1,0):
            self.dir = (0,-1)
        elif self.dir == (0,-1):
            self.dir = (-1,0)
        elif self.dir == (-1,0):
            self.dir = (0,1)

    def rotate_counter_clockwise(self):
        if self.dir == (0,1):
            self.dir = (-1,0)
        elif self.dir == (-1,0):
            self.dir = (0,-1)
        elif self.dir == (0,-1):
            self.dir = (1,0)
        elif self.dir == (1,0):
            self.dir = (0,1)

    def move(self):
        before = self.pos
        if self.dir == (0,1):
            self.pos = (self.pos[0], self.pos[1]+1)
        elif self.dir == (0,-1):
            self.pos = (self.pos[0], self.pos[1]-1)
        elif self.dir == (1,0):
            self.pos = (self.pos[0]+1, self.pos[1])
        elif self.dir == (-1,0):
            self.pos = (self.pos[0]-1, self.pos[1])
        after = self.pos
        assert(manhattan_distance(before, after) == 1)



def print_lights(grid, lights):
    grid = copy.deepcopy(grid)
    for light in lights:
        if not light_is_valid(light, grid):
            continue
        if light.dir == (0,1):
            grid[light.pos[0]][light.pos[1]] = ">"
        elif light.dir == (0,-1):
            grid[light.pos[0]][light.pos[1]] = "<"
        elif light.dir == (1,0):
            grid[light.pos[0]][light.pos[1]] = "v"
        elif light.dir == (-1,0):
            grid[light.pos[0]][light.pos[1]] = "^"
    for i in range(len(grid)):
        for j in range(len(grid[i])):
            print(grid[i][j], end="")
        print()
    time.sleep(0.1)
    os.system("clear")


def light_is_valid(light, grid):
    if light.pos[0] < 0 or light.pos[1] < 0:
        return False
    if light.pos[0] >= len(grid) or light.pos[1] >= len(grid[0]):
        return False
    return True

def move_lights(grid, lights):
    i = 0
    while i < len(lights):
        light = lights[i]
        if not light_is_valid(light, grid):
            lights.remove(light)
            continue
        travelled.add(light.pos)
        if grid[light.pos[0]][light.pos[1]] == "." or grid[light.pos[0]][light.pos[1]] == "#":
            light.move()
        elif grid[light.pos[0]][light.pos[1]] == "|":
            if light.dir in [(0,1), (0,-1)]:
                if light.pos in splitter_used:
                    lights.remove(light)
                    continue
                splitter_used.add(light.pos)
                new_light = Light(light.pos, light.dir)
                light.rotate_counter_clockwise()
                light.move()

                new_light.rotate_clockwise()
                new_light.move()
                lights.append(new_light)
            else:
                light.move()
        elif grid[light.pos[0]][light.pos[1]] == "-":
            if light.dir in [(1,0), (-1,0)]:
                if light.pos in splitter_used:
                    lights.remove(light)
                    continue
                splitter_used.add(light.pos)
                new_light = Light(light.pos, light.dir)
                light.rotate_counter_clockwise()
                light.move()

                new_light.rotate_clockwise()
                new_light.move()
                lights.append(new_light)
            else:
                light.move()
        elif grid[light.pos[0]][light.pos[1]] == "/":
            if light.dir in [(0,1), (0,-1)]:
                light.rotate_counter_clockwise()
                light.move()
            elif light.dir in [(1,0), (-1,0)]:
                light.rotate_clockwise()
                light.move()
        elif grid[light.pos[0]][light.pos[1]] == "\\":
            if light.dir in [(0,1), (0,-1)]:
                light.rotate_clockwise()
                light.move()
            elif light.dir in [(1,0), (-1,0)]:
                light.rotate_counter_clockwise()
                light.move()

        

        i += 1


initial_lights = []

grid = [[c for c in line] for line in out.splitlines()]
for i in range(len(grid)):
    initial_lights.append(Light((i,0), (0,1)))
    initial_lights.append(Light((i,len(grid[0])-1), (0,-1)))

for j in range(len(grid[0])):
    initial_lights.append(Light((0,j), (1,0)))
    initial_lights.append(Light((len(grid)-1,j), (-1,0)))


max_count = 0

for light in initial_lights:
    travelled = set()
    splitter_used = set() # (splitter_location)
    lights = [light]
    print(light.pos, light.dir)
    while lights:
        # print("light count ", len(lights))
        # print_lights(grid,lights)

        move_lights(grid, lights)
        # print(len(lights))
        # clean_lights(lights, grid)
    max_count = max(max_count, len(travelled))
    print(len(travelled))
print(max_count)

