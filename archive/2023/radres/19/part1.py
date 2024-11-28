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

parts = []
for line in blocks[1].splitlines():
    line = line.strip("{").strip("}")
    parts.append([int(word.split("=")[1]) for word in line.split(",")])

categories = {"x": 0, "m": 1, "a": 2, "s": 3}

total = 0
for part in parts:
    accepted = True
    workflows = [rules["in"]]
    print(part)
    while workflows:
        print(workflows)
        rule = workflows.pop()
        if accepted:
            for flow in rule.split(","):
                if flow[0] == "R":
                    accepted = False
                    break
                if flow[0] == "A":
                    break
                if len(flow) <= 3:
                    workflows.append(rules[flow])
                    print("appending rule", flow)
                elif ":" in flow:
                    check, result = flow.split(":")
                    if ">" in check:
                        category, value = check.split(">")
                        if part[categories[category]] > int(value):
                            if result == "A":
                                break
                            elif result == "R":
                                accepted = False
                                break
                            else:
                                workflows.append(rules[result])
                                break
                        else:
                            continue
                    elif "<" in check:
                        category, value = check.split("<")
                        if part[categories[category]] < int(value):
                            if result == "A":
                                break
                            elif result == "R":
                                accepted = False
                                break
                            else:
                                workflows.append(rules[result])
                                break
                        else:
                            continue

    if accepted:
        total += sum(part)

print(total)


