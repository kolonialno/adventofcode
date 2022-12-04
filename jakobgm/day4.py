from pathlib import Path

import polars as pl


# Read problem into DataFrame
rows = []
for pair in Path("input/4.txt").read_text().strip().splitlines():
    first, second = pair.split(",")
    first_lower, first_upper = map(int, first.split("-"))
    second_lower, second_upper = map(int, second.split("-"))
    rows.append(
        {
            "first_lower": first_lower,
            "first_upper": first_upper,
            "second_lower": second_lower,
            "second_upper": second_upper,
        }
    )
problem = pl.DataFrame(rows)

# Task 1
P = [
    [pl.col("first_lower"), pl.col("first_upper")],
    [pl.col("second_lower"), pl.col("second_upper")],
]
overlapping_pairs = problem.filter(
    (
        (
            P[0][0].is_between(*P[1], include_bounds=True)
            & P[0][1].is_between(*P[1], include_bounds=True)
        )
        | (
            P[1][0].is_between(*P[0], include_bounds=True)
            & P[1][1].is_between(*P[0], include_bounds=True)
        )
    )
)
print(overlapping_pairs.height)

# Task 2
overlapping_pairs = problem.filter(
    (
        P[0][0].is_between(*P[1], include_bounds=True)
        | P[0][1].is_between(*P[1], include_bounds=True)
        | P[1][0].is_between(*P[0], include_bounds=True)
        | P[1][1].is_between(*P[0], include_bounds=True)
    )
)
print(overlapping_pairs.height)
