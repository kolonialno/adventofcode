import pytest

from solve10 import solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(10)


@pytest.fixture
def image(data_getter):
    return data_getter(10, "image")


def test_solve1(data):
    assert solve1(data) == 13140


def test_solve2(data, image):
    assert solve2(data) == "\n" + image
