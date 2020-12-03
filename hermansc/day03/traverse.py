slopes = [
    (3, 1),
    (1, 1),
    (5, 1),
    (7, 1),
    (1, 2),
]

forest = []
with open("forest.txt", "r") as fh:
    for row in fh:
        forest.append([0 if item == "." else 1 for item in list(row.strip())])

product = 1
for right, down in slopes:
    coord = (0, 0)
    trees = 0
    while coord[0] < len(forest):
        trees += forest[coord[0]][(coord[1] % len(forest[0]))]
        coord = (coord[0] + down, coord[1] + right)
    product *= trees

print(product)
