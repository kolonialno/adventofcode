class BagIndex:
    def __init__(self):
        self.index = {}

    def add_bag(self, bag_name):
        if bag_name not in self.index:
            self.index[bag_name] = {}

    def add_contents(self, holder_bag, inner_bag, number):
        self.index[holder_bag][inner_bag] = number

    def count_contains(self, bag_name):
        return len([x for x in self.index if self.can_contain(x, bag_name)])

    def can_contain(self, holder_bag, bag_name):
        for bag in self.index[holder_bag]:
            if bag == bag_name or self.can_contain(bag, bag_name):
                return True
        return False

    def nested_count(self, bag_name):
        if self.index[bag_name] == {}:
            return 0
        return sum(
            value + (value * self.nested_count(key))
            for key, value in self.index[bag_name].items()
        )


def tokenise_line(line):
    pieces = [z.strip() for z in line.replace("bags contain", ",").split(",")]
    holder_bag = pieces[0]
    insides = [
        (int(x[0]), " ".join(x[1:3]))
        for x in map(lambda y: y.split()[:-1], pieces[1:])
        if x[0] != "no"
    ]

    return holder_bag, insides


def build_bag_index(filename):
    bag_index = BagIndex()
    with open(filename) as in_file:
        lines = (x.strip() for x in in_file.read().split(".")[:-1])
    for line in lines:
        holder, items = tokenise_line(line)
        bag_index.add_bag(holder)
        for item in items:
            bag_index.add_contents(holder, item[1], item[0])
    return bag_index


def testcase() -> int:
    index = build_bag_index("inputs/07testcase.txt")
    return index.count_contains("shiny gold")


def testcase_secondary() -> int:
    index = build_bag_index("inputs/07testcase.txt")
    return index.nested_count("shiny gold")


def main() -> int:
    index = build_bag_index("inputs/07.txt")
    return index.count_contains("shiny gold")


def secondary() -> int:
    index = build_bag_index("inputs/07.txt")
    return index.nested_count("shiny gold")
