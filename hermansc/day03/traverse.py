import math

forest = []
with open("forest.txt", "r") as fh:
    for row in fh:
        forest.append(list(row.strip()))

slopes = [
    (3, 1),
    (1, 1),
    (5, 1),
    (7, 1),
    (1, 2),
]
min_down = min([t[1] for t in slopes])
max_right = max(t[0] for t in slopes)
ratio = math.ceil(max_right / min_down)

columns = len(forest[0])
rows = len(forest)
x = math.ceil((ratio * rows) / columns)

for idx in range(len(forest)):
    forest[idx] = forest[idx] * x

all_trees = []
for right, down in slopes:
    start_coord = (0, 0)
    trees = 0
    while start_coord[0] < len(forest):

        if forest[start_coord[0]][start_coord[1]] == "#":
            trees += 1

        start_coord = (start_coord[0] + down, start_coord[1] + right)

    all_trees.append(trees)

print(math.prod(all_trees))
