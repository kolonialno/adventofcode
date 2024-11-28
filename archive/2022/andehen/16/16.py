import re
import networkx as nx

valves = {}

with open("input.txt") as f:
    for l in f.read().splitlines():
        valve_txt, tunnel_txt = l.split(";")
        id = valve_txt.split()[1]
        rate = re.findall(r"rate=(\d*)", valve_txt)[0]
        tunnels = [t.replace(",", "") for t in tunnel_txt.split()[4:]]
        valves[id] = {"rate": int(rate), "tunnels": tunnels}


G = nx.DiGraph()

for key, valve in valves.items():
    G.add_node(key)
    for tunnel in valve["tunnels"]:
        G.add_edge(key, tunnel)


def get_pressure_and_path_length(G, t, current_valve, target_valve):
    try:
        path_length = nx.shortest_path_length(G, current_valve, target_valve)
        time_to_open_valve = path_length + 1
        time_left = 30 - t - time_to_open_valve
        if time_left <= 0:
            return None, None
        pressure = (time_left + 1) * valves[target_valve]["rate"]

        return time_to_open_valve, pressure
    except nx.NetworkXError:
        return None, None


def copy_and_remove(closed_valves, valve):

    l = closed_valves.copy()
    l.remove(valve)
    return l


def get_maximum_pressure(G, current_valve, current_pressure, closed_valves, t, n, path):

    candidates = []

    for target_valve in closed_valves:
        time_to_open_valve, pressure = get_pressure_and_path_length(
            G, t, current_valve, target_valve
        )
        if time_to_open_valve:
            candidates.append((target_valve, time_to_open_valve, pressure))

    if candidates:
        pressures = [
            get_maximum_pressure(
                G,
                c[0],
                current_pressure + c[2],
                copy_and_remove(closed_valves, c[0]),
                t + c[1],
                n + 1,
                path + [(c[0], t + c[1], c[2])],
            )
            for c in candidates
        ]
        return max(pressures)

    return current_pressure


current_valve = "AA"
closed_valves = [key for key, valve in valves.items() if valve["rate"] > 0]

print(get_maximum_pressure(G, current_valve, 0, closed_valves, 1, 0, []))
