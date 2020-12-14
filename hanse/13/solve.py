from sys import stdin

earliest_timestamp = int(next(stdin))
schedule = [None if x == "x" else int(x) for x in next(stdin).split(",")]


def a():
    departures = {k: k for k in schedule if k is not None}

    for bus in departures.keys():
        while departures[bus] < earliest_timestamp:
            departures[bus] += bus

    (departure, bus) = min(
        (departures[bus] - earliest_timestamp, bus) for bus in departures
    )

    return departure * bus


def b():
    departures = [(i, bus) for (i, bus) in enumerate(schedule) if bus is not None]

    query = "=".join(["0"] + [f"((t+%2B+{i})+mod+{bus})" for (i, bus) in departures])

    return f"https://www.wolframalpha.com/input/?i={query}"


print(a())
print(b())
