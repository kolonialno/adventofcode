from helpers import *
import os
import time

with open("input.txt", "r") as f:
    out = f.read().strip()
# out = """px{a<2006:qkq,m>2090:A,rfg}
# pv{a>1716:R,A}
# lnx{m>1548:A,A}
# rfg{s<537:gd,x>2440:R,A}
# qs{s>3448:A,lnx}
# qkq{x<1416:A,crn}
# crn{x>2662:A,R}
# in{s<1351:px,qqz}
# qqz{s>2770:qs,m<1801:hdj,R}
# gd{a>3333:R,R}
# hdj{m>838:A,pv}

# {x=787,m=2655,a=1222,s=2876}
# {x=1679,m=44,a=2067,s=496}
# {x=2036,m=264,a=79,s=2244}
# {x=2461,m=1339,a=466,s=291}
# {x=2127,m=1623,a=2188,s=1013}"""

blocks = out.split("\n\n")
rules = {}
for line in blocks[0].splitlines():
    line = line.strip("}")
    key, value = line.split("{")
    rules[key] = value




all_ranges = []
workflows = [rules["in"]]

categories = {"x": 0, "m": 1, "a": 2, "s": 3}

def find_ranges(workflow, ranges, all_ranges):
    ranges = ranges.copy()
    for flow in workflow.split(","):
        if flow[0] == "R":
            continue
        if flow[0] == "A":
            all_ranges.append(ranges)
            continue
        if len(flow) <= 3:
            find_ranges(rules[flow], ranges, all_ranges)
        elif ":" in flow:
            check, result = flow.split(":")
            if ">" in check:
                category, value = check.split(">")

                #split ranges
                copy_ranges = ranges.copy()
                copy_ranges[categories[category]] = range(int(value)+1, ranges[categories[category]].stop)
                ranges[categories[category]] = range(ranges[categories[category]].start, int(value))
                if result == "A":
                    all_ranges.append(copy_ranges)
                    continue
                elif result == "R":
                    continue
                else:
                    find_ranges(rules[result], copy_ranges, all_ranges)
            elif "<" in check:
                category, value = check.split("<")

                #split ranges
                copy_ranges = ranges.copy()
                copy_ranges[categories[category]] = range(ranges[categories[category]].start, int(value)-1)
                ranges[categories[category]] = range(int(value), ranges[categories[category]].stop)
                if result == "A":
                    all_ranges.append(copy_ranges)
                    continue
                elif result == "R":
                    continue
                else:
                    find_ranges(rules[result], copy_ranges, all_ranges)

print(rules["in"])
find_ranges(rules["in"], [range(1,4000), range(1,4000), range(1,4000), range(1,4000)], all_ranges)


result = 0
expected = 167409079868000
for ranges in all_ranges:
    print(ranges)
    #range min max
    total = 1
    print("products")
    for ran in ranges:
        print(ran.stop - ran.start + 1)
        low, high = ran.start, ran.stop
        total *= (high - low + 1)
    print()
    result += total
print(f"result: {result}")
print(expected/result)