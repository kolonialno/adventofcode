import pytest

from solve06 import solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(6)


def test_solve1(data):
    assert solve1(data) == 7


def test_solve2(data):
    assert solve2(data) == 19
