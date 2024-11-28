import pytest

from solve04 import Assignment, parse_assignment_pair, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(4)


def test_assignment_equality():
    a1 = Assignment(1, 2)
    a2 = Assignment(1, 2)
    a3 = Assignment(1, 3)
    assert (a1 == a2) is True, "Should be equal"
    assert (a1 == a3) is False, "Should not be equal"


def test_assignment_less_than_or_equal():
    a = Assignment()  # Empty assignment
    a34 = Assignment(3, 4)
    a25 = Assignment(2, 5)
    a46 = Assignment(4, 6)

    assert (a <= a34) is True, "The empty assignment should be less than any assignment"
    assert (a34 <= a34) is True, "Should be <= to itself"
    assert (a34 <= a25) is True, "Should be <= when fully contained"
    assert (a34 <= a46) is False, "Should not be <= when not fully contained"


def test_assignment_intersections():
    a34 = Assignment(3, 4)
    a46 = Assignment(4, 6)
    a44 = Assignment(4, 4)
    a56 = Assignment(5, 6)
    a = Assignment()

    assert a34 & a34 == a34, "Should intersect with itself"
    assert a34 & a46 == a44, "Should intersect with an overlaping assignment"
    assert a34 & a56 == a, "Should not intersect with an overlaping assignment"


def test_parse_assignment_pair():
    a, b = parse_assignment_pair("2-4,6-8")
    assert a == Assignment(2, 4), "Should be an assignment of sections 2-4"
    assert b == Assignment(6, 8), "Should be an assignment of sections 6-8"


def test_solve1(data):
    assert solve1(data) == 2


def test_solve2(data):
    assert solve2(data) == 4
