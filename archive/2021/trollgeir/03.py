from enum import Enum, auto

import pandas as pd


class ExtractionType(Enum):
    GAMMA = auto()
    EPSILON = auto()
    CO2 = auto()
    O2 = auto()


def bitarray_to_int(series: pd.Series) -> str:
    return int(series.astype(int).astype(str).sum(), 2)


def most_common_bitarray(matrix: pd.DataFrame) -> pd.Series:
    return matrix.mode().sum().astype(bool)


def extract_diagnostics(matrix: pd.DataFrame, type: ExtractionType) -> int:
    bitmask = most_common_bitarray(matrix)

    if type == ExtractionType.GAMMA:
        return bitarray_to_int(bitmask)

    if type == ExtractionType.EPSILON:
        return bitarray_to_int(~bitmask)

    if type in [ExtractionType.O2, ExtractionType.CO2]:
        for i in matrix.columns:
            if type == ExtractionType.CO2:
                bitmask = ~bitmask
            # Reduce
            matrix = matrix[matrix[i] == bitmask[i]]
            if len(matrix) == 1:  # Completion criteria
                return bitarray_to_int(matrix.iloc[0])
            # Update bitmask
            bitmask = most_common_bitarray(matrix)


# Read data
data = pd.read_csv("inputs/03.txt", header=None, dtype=str, squeeze=True)

matrix = data.str.split("", expand=True)  # split strings into columns
matrix = matrix.iloc[:, 1:-1]  # slice away edge-columns (empty from str.split)
matrix = matrix.T.reset_index(drop=True).T  # reset column index
matrix = matrix.astype(int).astype(bool)  # cast to bool-matrix (pErFoRmAnCe ReAsOnS!11)

# Puzzle 1
epsilon = extract_diagnostics(matrix, ExtractionType.EPSILON)
gamma = extract_diagnostics(matrix, ExtractionType.GAMMA)

answer1 = gamma * epsilon

# Puzzle 2
O2 = extract_diagnostics(matrix, ExtractionType.O2)
CO2 = extract_diagnostics(matrix, ExtractionType.CO2)

answer2 = O2 * CO2

print(f"Answer1: {answer1}")  # 4191876
print(f"Answer2: {answer2}")  # 3414905
