import re
from collections import defaultdict
from pathlib import Path

problem = Path("input/5.txt").read_text().strip()
stacks, procedure = problem.split("\n\n")

stacks = list(reversed(stacks.splitlines()))[1:]
channels = defaultdict(list)
new_channels = defaultdict(list)
for row in stacks:
    for channel, box in enumerate(row[1::4]):
        if box.isalpha():
            channels[channel + 1].append(box)
            new_channels[channel + 1].append(box)

pattern = re.compile(r"move (\d+) from (\d) to (\d)")
for move in procedure.splitlines():
    match = pattern.match(move)
    quantity, source, destination = map(int, match.groups())
    for _ in range(int(quantity)):
        channels[destination].append(channels[source].pop())

    new_channels[source], boxes = (
        new_channels[source][:-quantity],
        new_channels[source][-quantity:],
    )
    new_channels[destination].extend(boxes)


print("".join([channel[-1] for channel in channels.values()]))
print("".join([channel[-1] for channel in new_channels.values()]))
