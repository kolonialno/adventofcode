import re

with open("input.txt") as f:
    x1, x2, y1, y2 = (int(n) for n in re.findall("-?\d+", f.readline()))

count = 0
for m in range(-int(-((8 * x1 + 1) ** 0.5 - 1) // 2), x2 + 1):
    for n in range(-y1 - 1, y1 - 1, -1):
        x, y, vx, vy = 0, 0, m, n
        while x <= x2 and y >= y1:
            if x >= x1 and y <= y2:
                count += 1
                break
            x, vx = x + vx, max(0, vx - 1)
            y, vy = y + vy, vy - 1

print("Part 1:", y1 * (y1 + 1) // 2)  # Fingers crossed
print("Part 2:", count)
