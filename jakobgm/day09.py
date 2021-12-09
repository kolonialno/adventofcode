from pathlib import Path

import numpy as np
from scipy.ndimage import label

heights = np.array([
    [int(h) for h in row]
    for row
    in Path("inputs/09.txt").read_text().strip().split("\n")
])
rows, columns = len(heights), len(heights[0])
expanded = 10 * np.ones((rows + 2, columns + 2), dtype=int)
expanded[1:rows + 1, 1:columns + 1] = heights

above = expanded[:rows, 1:columns + 1]
below = expanded[2:, 1:columns + 1]
left = expanded[1:rows + 1, :columns]
right = expanded[1:rows + 1, 2:]

minima = (heights < above) & (heights < below) & (heights < left) & (heights < right)
print((1 + heights[minima]).sum())

ridges = (heights < 9).astype(int)
basins, _ = label(ridges)
sizes = np.unique(basins[basins != 0], return_counts=True)[1]
print(np.prod(np.sort(sizes)[-3:]))
