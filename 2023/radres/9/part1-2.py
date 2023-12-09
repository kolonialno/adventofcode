from helpers import *
with open("input.txt", "r") as f:
    out = f.read()

# out = """0 3 6 9 12 15
# 1 3 6 10 15 21
# 10 13 16 21 30 45"""
def find_triangle(numbers):
    triangle = [numbers]

    while not all([el == 0 for el in triangle[-1]]):
        vec = triangle[-1]
        next_vec = []
        for i in range(len(vec)-1):
            next_vec.append(vec[i+1] - vec[i])
        triangle.append(next_vec)
    return triangle

def get_next(triangle):
    return triangle[0][-1] + sum([nums[-1] for nums in triangle[1:]])

def get_left_history(triangle):
    histories = [0]
    for i in range(len(triangle)-1):
        histories.append(triangle[-2-i][0] - histories[-1])
    return histories[-1]
        
part1 = 0
part2 = 0
for line in out.splitlines():
    numbers = extract_integers_from_text(line)
    triangle = find_triangle(numbers)

    part2 += get_left_history(triangle)
    part1 += get_next(triangle)
print(part1)
print(part2)




