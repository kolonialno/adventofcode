import pytest

from solve08 import is_visible, scenic_score, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(8)


def test_scenic_score(data):
    rows = data.splitlines()
    assert scenic_score(data.splitlines(), 1, 2) == 4
    assert scenic_score(data.splitlines(), 3, 2) == 8


def test_solve1(data):
    assert solve1(data) == 21


def test_solve2(data):
    assert solve2(data) == 8
