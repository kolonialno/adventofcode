# Make a list of form submissions from each group. One single form is represented as a set.
# [
#     [
#         {a, b, c}, # One form submission
#     ],
#     [
#         {a}, {b}, {c} # Three forms submissions in this group
#     ]
# ]
groups = [[set(c) for c in l.rstrip().split("\n")] for l in open("forms.txt").read().split("\n\n")]

# Part 1
# Within a group we take the union of all submissions and thus get the unique answers
# for this group.
print(sum(len(set.union(*g)) for g in groups))

# Part 2
# Within a group we take the intersection and thus get the answers within the group shared
# between every submission (i.e. the intersection :-))
print(sum(len(set.intersection(*g)) for g in groups))
