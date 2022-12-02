import pytest

from solve02 import score, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter("02")


@pytest.mark.parametrize(
    "input, expected",
    [
        ((0, 1), 8),
        ((1, 0), 1),
        ((2, 2), 6),
    ],
)
def test_score(input, expected):
    assert (
        score(*input) == expected
    ), "Should calculate the correct score for a single round"


def test_solve1(data):
    assert solve1(data) == 15, "Should calculate the correct score for the whole game"


def test_solve2(data):
    assert solve2(data) == 12, "Should calculate the correct score for the whole game"
