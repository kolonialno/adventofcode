import pytest

from solve03 import common_item, item_priority, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter("03")


@pytest.mark.parametrize("item, priority", [("a", 1), ("z", 26), ("A", 27), ("Z", 52)])
def test_priority(item, priority):
    assert item_priority(item) == priority


@pytest.mark.parametrize(
    "sets, common",
    [
        (("vJrwpWtwJgWr", "hcsFMMfFFhFp"), "p"),
        (
            (
                "vJrwpWtwJgWrhcsFMMfFFhFp",
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                "PmmdzqPrVvPwwTWBwg",
            ),
            "r",
        ),
    ],
)
def test_common_item(sets, common):
    assert common_item(*sets) == common


def test_solve1(data):
    assert solve1(data) == 157


def test_solve2(data):
    assert solve2(data) == 70
