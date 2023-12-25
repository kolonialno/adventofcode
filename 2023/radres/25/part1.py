import networkx as nx
import matplotlib.pyplot as plt


with open("input.txt", "r") as f:
    out = f.read().strip()

class Node:
    def __init__(self, name):
        self.name = name
        self.connections = set()
        self.visited = False

node_dict = {}
for line in out.splitlines():
    left, right = line.split(":")
    left = left.strip()
    right = right.strip().split(" ")

    # Ensure each node is created only once
    if left not in node_dict:
        node_dict[left] = Node(left)

    for n in right:
        if n not in node_dict:
            node_dict[n] = Node(n)
        
        # Add connections
        node_dict[left].connections.add(node_dict[n])
        node_dict[n].connections.add(node_dict[left])

def count_cluster(node):
    to_count = [node]
    count = 0
    while to_count:
        n = to_count.pop()
        if n.visited:
            continue
        n.visited = True
        count += 1
        to_count.extend(n.connections)
    return count

# cut ties manually
node_dict["vbk"].connections.remove(node_dict["gqr"])
node_dict["gqr"].connections.remove(node_dict["vbk"])

node_dict["sdv"].connections.remove(node_dict["mxv"])
node_dict["mxv"].connections.remove(node_dict["sdv"])

node_dict["scr"].connections.remove(node_dict["klj"])
node_dict["klj"].connections.remove(node_dict["scr"])

print(count_cluster(node_dict["vbk"]))
print(count_cluster(node_dict["gqr"]))

# PLOTTING
G = nx.Graph()
for node in node_dict.values():
    G.add_node(node.name)
    for conn in node.connections:
        G.add_edge(node.name, conn.name)
nx.draw(G, with_labels=True, node_color='lightblue', font_weight='bold', node_size=700)
plt.show()
