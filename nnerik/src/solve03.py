from functools import reduce
from operator import and_


def item_priority(item):
    return ord(item) % 32 + 26 * (item < "a")


def common_item(*args):
    return reduce(and_, (set(s) for s in args)).pop()


def split_items(items):
    size = len(items) // 2
    return items[:size], items[size:]


def solve1(data):
    return sum(
        item_priority(common_item(*split_items(items))) for items in data.splitlines()
    )


def solve2(data):
    return sum(
        item_priority(common_item(*items))
        for items in zip(*[iter(data.splitlines())] * 3)
    )
