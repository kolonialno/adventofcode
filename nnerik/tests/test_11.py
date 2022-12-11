import pytest

from solve11 import get_monkeys, run_monkeys, solve1, solve2


@pytest.fixture
def data(data_getter):
    return data_getter(11)


def test_get_monkeys(data):
    monkeys = get_monkeys(data)
    assert len(monkeys) == 4

    assert monkeys[0].worries == [79, 98]
    assert monkeys[0].operation(2) == 38
    assert monkeys[1].operation(3) == 9
    assert monkeys[2].operation(4) == 16

    assert monkeys[0].next_monkey(38) == 3
    assert monkeys[1].next_monkey(38) == 2


def test_run_monkeys(data):
    monkeys = get_monkeys(data)
    inspections = run_monkeys(monkeys, 3)
    assert inspections == [2, 4, 3, 5]
    assert [m.worries for m in monkeys] == [
        [20, 23, 27, 26],
        [2080, 25, 167, 207, 401, 1046],
        [],
        [],
    ]


def test_solve1(data):
    assert solve1(data) == 10605


def test_solve2(data):
    assert solve2(data) == 2713310158
