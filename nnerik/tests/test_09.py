import pytest

from solve09 import move_tail, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(9)


@pytest.fixture
def data_b(data_getter):
    return data_getter(9, "b")


@pytest.mark.parametrize(
    "head, tail, expected",
    [
        ((3, 1), (1, 1), (2, 1)),
        ((1, 3), (1, 1), (1, 2)),
        ((2, 1), (1, 3), (2, 2)),
        ((3, 2), (1, 3), (2, 2)),
        ((1, 1), (1, 1), (1, 1)),
        ((2, 3), (2, 2), (2, 2)),
        ((3, 3), (2, 2), (2, 2)),
    ],
)
def test_move_tail(head, tail, expected):
    assert move_tail(*head, *tail) == expected


def test_solve1(data):
    assert solve1(data) == 13


def test_solve2(data, data_b):
    assert solve2(data) == 1
    assert solve2(data_b) == 36
