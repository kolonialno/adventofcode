import pytest

from solve05 import parse_inputs, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(5)


def test_reader(data):
    stacks, moves = parse_inputs(data)
    assert stacks == [["Z", "N"], ["M", "C", "D"], ["P"]]
    assert moves == [(1, 2, 1), (3, 1, 3), (2, 2, 1), (1, 1, 2)]


def test_solve1(data):
    assert solve1(data) == "CMZ"


def test_solve2(data):
    assert solve2(data) == "MCD"
