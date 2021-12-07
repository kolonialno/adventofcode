from functools import cache

with open("inputs/06.txt") as f:
     lanternfish = [int(i) for i in f.readline().split(",")]

@cache # cache is fun :) 
def spawn_fish(time_to_spawn, remaining_days):
     offspring = 0
     for i in range(remaining_days,0,-1):
          if time_to_spawn == 0: # Spawn!
             time_to_spawn = 7
             offspring += spawn_fish(8, i-1) # offspring start next day
          time_to_spawn -= 1

     return offspring + 1 # include itself

answer1 = sum([spawn_fish(i, 80) for i in lanternfish])
answer2 = sum([spawn_fish(i, 256) for i in lanternfish])

print(f"Answer1: {answer1}")  # 350605
print(f"Answer2: {answer2}")  # 1592778185024
