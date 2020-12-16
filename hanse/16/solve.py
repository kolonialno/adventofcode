from sys import stdin
from collections import defaultdict

fields = {}
my_ticket = None
nearby_tickets = []

blocks = [block for block in stdin.read().strip().split("\n\n")]

for block in blocks:
    lines = [line.strip() for line in block.split("\n")]

    if lines[0] == "your ticket:":
        my_ticket = tuple(map(int, lines[1].split(",")))
    elif lines[0] == "nearby tickets:":
        nearby_tickets = [tuple(map(int, ticket.split(","))) for ticket in lines[1:]]
    else:
        for line in lines:
            field, value = line.split(": ")
            a, b = value.split(" or ")
            ranges = (
                tuple(map(int, a.split("-"))),
                tuple(map(int, b.split("-"))),
            )
            fields[field] = ranges


field_ranges = fields.values()


def is_valid(value, range):
    (a, b) = range
    return (value >= a[0] and value <= a[1]) or (value >= b[0] and value <= b[1])


def is_all_valid(value):
    for field_range in field_ranges:
        if is_valid(value, field_range):
            return True

    return False


def a():
    invalid_values = []
    for ticket in nearby_tickets:
        for value in ticket:
            if not is_all_valid(value):
                invalid_values.append(value)
                break

    return sum(invalid_values)


def resolve_field_positions(tickets):
    candidates = defaultdict(set)
    ticket_length = len(tickets[0])

    for current in fields.items():
        for i in range(ticket_length):
            if not any(not is_valid(ticket[i], current[1]) for ticket in tickets):
                candidates[i].add(current[0])

    used = set()
    positions = {}
    for candidate in sorted(candidates.items(), key=lambda x: len(x[1])):
        remaining = candidate[1] - used
        if len(remaining) == 1:
            used |= candidate[1]
            positions[remaining.pop()] = candidate[0]

    return positions


def b():
    invalid_tickets = set()
    for ticket in nearby_tickets:
        for value in ticket:
            if not is_all_valid(value):
                invalid_tickets.add(ticket)
                break

    valid_tickets = [
        ticket for ticket in nearby_tickets if ticket not in invalid_tickets
    ]

    positions = resolve_field_positions(valid_tickets)

    departure_fields = [
        field for field in fields.keys() if field.startswith("departure ")
    ]

    product = 1
    for field in departure_fields:
        product *= my_ticket[positions[field]]

    return product


print(a())
print(b())
