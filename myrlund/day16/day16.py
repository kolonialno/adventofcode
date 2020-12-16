import sys
import re


def split_input_text(input_text):
    rules, your, nearby = input_text.split("\n\n", 2)
    return (rules, your.split("\n", 1)[1], nearby.split("\n", 1)[1])


RULES_RE = re.compile(r"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)")


def parse_rules(rules_str: str):
    rules = {}
    for line in rules_str.splitlines():
        label, r1_from, r1_to, r2_from, r2_to = RULES_RE.match(line).groups()
        rules[label] = (
            range(int(r1_from), int(r1_to) + 1),
            range(int(r2_from), int(r2_to) + 1),
        )
    return rules


def parse_ticket(line):
    return [int(v) for v in line.split(",")]


def invalid_numbers(all_rule_ranges: range, n):
    for rule_range in all_rule_ranges:
        if n in rule_range:
            return
    yield n


def is_valid_number(rule_ranges, n):
    for rule_range in rule_ranges:
        if n in rule_range:
            return True
    return False


def is_valid_ticket(all_rule_ranges, ticket):
    return not any([not is_valid_number(all_rule_ranges, n) for n in ticket])


def find_solved_label(position_labels):
    for pos, labels in position_labels.items():
        if len(labels) == 1:
            (solved_label,) = labels
            return solved_label, pos

    return None


with open(sys.argv[1]) as f:
    rules, my_ticket_raw, nearby_tickets_raw = split_input_text(f.read())

parsed_rules = parse_rules(rules)
all_rule_ranges = [r for rs in parsed_rules.values() for r in rs]
all_labels = parsed_rules.keys()

nearby_tickets = [parse_ticket(line) for line in nearby_tickets_raw.splitlines()]
my_ticket = parse_ticket(my_ticket_raw)

part = int(sys.argv[2]) if len(sys.argv) > 2 else 1

if part == 1:
    invalid_number_generators = [
        invalid_numbers(all_rule_ranges, n) for ticket in nearby_tickets for n in ticket
    ]
    invalid_numbers = [n for gen in invalid_number_generators for n in gen]
    print(sum(invalid_numbers))

if part == 2:
    valid_nearby_tickets = [
        ticket for ticket in nearby_tickets if is_valid_ticket(all_rule_ranges, ticket)
    ]

    # Keep track of what we have and have not solved.
    possible_labels_for_index = {
        index: set(all_labels) for index in range(len(all_labels))
    }
    remaining_labels = set(all_labels)
    solved_labels = {}

    # Zoom through tickets until we have a solution, narrowing the labels examined as
    # we make progress.
    for ticket in valid_nearby_tickets:
        newly_solved_labels = set()

        for label in remaining_labels:
            # Look for indexes violating the label's rules.
            invalid_numbers = [is_valid_number(parsed_rules[label], n) for n in ticket]

            # For each number violating the rules, remove the label as a remaining
            # option.
            invalid_indexes = {
                index for index, is_valid in enumerate(invalid_numbers) if not is_valid
            }
            for invalid_index in invalid_indexes:
                possible_labels_for_index[invalid_index] -= {label}

            # Solve and prune until we get no further.
            while result := find_solved_label(possible_labels_for_index):
                solved_label, label_index = result
                solved_labels[solved_label] = label_index

                newly_solved_labels.add(solved_label)

                # Prune the solved label from the possibility map.
                for index in possible_labels_for_index.keys():
                    possible_labels_for_index[index] -= {solved_label}

        # Stop examining labels that we have already solved.
        remaining_labels -= set(newly_solved_labels)

    # Calculate solution number from my ticket.
    departure_indexes = [
        index for label, index in solved_labels.items() if label.startswith("departure")
    ]
    my_departure_numbers = [my_ticket[index] for index in departure_indexes]

    product = 1
    for departure_number in my_departure_numbers:
        product *= departure_number
    print(product)
