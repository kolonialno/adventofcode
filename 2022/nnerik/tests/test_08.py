import pytest

from solve08 import score_function_factory, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(8)


def test_is_visible(data):
    rows = data.splitlines()
    score_fn = score_function_factory(
        lambda s, u: s or u,
        lambda _: False,
        lambda _: True,
    )
    assert score_fn(rows, 1, 2) is True
    assert score_fn(rows, 1, 3) is False
    assert score_fn(rows, 1, 4) is True


def test_scenic_score(data):
    rows = data.splitlines()
    score_fn = score_function_factory(
        lambda s, u: (1 if s is None else s) * u,
        lambda u: 1 if u is None else u + 1,
        lambda u: u or 0,
    )
    assert score_fn(rows, 1, 2) == 4
    assert score_fn(rows, 3, 2) == 8


def test_solve1(data):
    assert solve1(data) == 21


def test_solve2(data):
    assert solve2(data) == 8
