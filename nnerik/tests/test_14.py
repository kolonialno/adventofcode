import pytest

from solve14 import create_cave, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(14)


def test_create_cave(data):
    cave, depth = create_cave(data)
    assert cave == {
        (498, 4),
        (498, 5),
        (498, 6),
        (497, 6),
        (496, 6),
        (503, 4),
        (502, 4),
        (502, 5),
        (502, 6),
        (502, 7),
        (502, 8),
        (502, 9),
        (501, 9),
        (500, 9),
        (499, 9),
        (498, 9),
        (497, 9),
        (496, 9),
        (495, 9),
        (494, 9),
    }
    assert depth == 9


def test_solve1(data):
    assert solve1(data) == 24


def test_solve2(data):
    assert solve2(data) == 93
