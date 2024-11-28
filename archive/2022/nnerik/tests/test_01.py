import pytest

from solve01 import solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(1)


def test_solve1(data):
    assert solve1(data) == 24000


def test_solve2(data):
    assert solve2(data) == 45000
