import numpy as np
import pandas as pd


def smallest_center(window):
    if len(window) == 2:  # Edge case (literally!)
        if window.iloc[0] > window.iloc[1]:
            return window.index[0] != 0
        else:
            return window.index[0] == 0
    else:
        return window.iloc[0] > window.iloc[1] < window.iloc[2]

def explore_basin(point):
    def ascend(p):  # Recursive function
        basin_cells.append(p)
        directions = [
            (p[0] - 1, p[1]),  # north
            (p[0] + 1, p[1]),  # south
            (p[0], p[1] - 1),  # east
            (p[0], p[1] + 1),  # west
        ]

        for d in directions:
            if d in basin_cells:
                continue
            try:
                if heatmap.loc[d] < 9:
                    ascend(d)
            except KeyError:  # Out of grid
                pass

    basin_cells = []
    ascend(point)
    return len(basin_cells)


data = pd.read_csv("inputs/09.txt", header=None, dtype=str, squeeze=True)
heatmap = data.str.split("", expand=True)  # split strings into columns
heatmap = heatmap.iloc[:, 1:-1]  # slice away edge-columns (empty from str.split)
heatmap.columns = heatmap.index  # reset column index
heatmap = heatmap.astype(int)

# Roll both axes and combine masks
low_points = heatmap.rolling(window=3, min_periods=2, center=True).apply(
    smallest_center
).astype(bool) & heatmap.rolling(window=3, min_periods=2, center=True, axis=1).apply(
    smallest_center
).astype(
    bool
)

answer1 = (heatmap[low_points] + 1).sum().sum()

# Stack the heatmap low-points and get the coordinates
stacked = low_points.stack()
low_points_coordinates = stacked[stacked].index

basin_sizes = [explore_basin(p) for p in low_points_coordinates]
answer2 = np.prod(sorted(basin_sizes)[-3:])

print(f"Answer1: {answer1}")  # 348664
print(f"Answer2: {answer2}")  # 100220525.0
