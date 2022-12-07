import pytest

from solve07 import solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(7)


def test_solve1(data):
    assert solve1(data) == 95437


def test_solve2(data):
    assert solve2(data) == 24933642
