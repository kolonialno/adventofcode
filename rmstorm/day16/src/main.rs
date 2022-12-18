use petgraph::graph::EdgeReference;
use petgraph::visit::EdgeRef;
use petgraph::{algo::dijkstra, stable_graph::NodeIndex, Graph, Undirected};
use regex::Regex;
use std::cmp::{max, min};
use std::{collections::HashMap, str::FromStr, string::ParseError};

#[derive(Debug)]
struct CaveSystem {
    flows: HashMap<String, isize>,
    node_ids: HashMap<String, NodeIndex>,
    caves: Graph<String, isize, Undirected>,
}

#[derive(Debug, Clone)]
struct Solution1 {
    cur_node: NodeIndex,
    opened: Vec<String>,
    time_spend: isize,
    flow: isize,
}
impl Solution1 {
    fn minutes_left(&self) -> isize {
        30 - self.time_spend
    }
    fn update_flow(&mut self, increase: isize, max_value: &mut isize) {
        self.flow += increase;
        *max_value = (*max_value).max(self.flow);
    }
    fn max_flow_left(&self, cs: &CaveSystem) -> isize {
        let mut optimal = self.minutes_left() + 2;
        cs.flows
            .iter()
            .filter_map(|(c, f)| {
                if !self.opened.contains(c) {
                    optimal -= 1;
                    return Some(*f * optimal);
                }
                None
            })
            .sum::<isize>()
            + self.flow
    }
}

#[derive(Debug, Clone)]
struct Solution2 {
    cur_nodes: [NodeIndex; 2],
    opened: Vec<String>,
    time_spend: [isize; 2],
    flow: isize,
}
impl Solution2 {
    fn update_flow(&mut self, increase: isize, max_value: &mut isize) {
        self.flow += increase;
        *max_value = (*max_value).max(self.flow);
    }
    fn cur_best_outcome(&self, cs: &CaveSystem) -> isize {
        let mut optimal = 26 - self.time_spend.iter().min().unwrap();
        cs.flows
            .iter()
            .filter_map(|(c, f)| {
                if !self.opened.contains(c) {
                    optimal -= 1;
                    return Some(*f * optimal);
                }
                None
            })
            .sum::<isize>()
            + self.flow
    }
}

impl FromStr for CaveSystem {
    type Err = ParseError;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^Valve (\w+) has flow rate=(\d+); tunnel[s]? lead[s]? to valve[s]? ((?:(?:\w\w)(?:,\s)?)*)$",).unwrap();
        let mut flows: HashMap<String, isize> = HashMap::new();
        let mut node_ids: HashMap<String, NodeIndex> = HashMap::new();
        let mut caves = Graph::<String, isize, Undirected>::new_undirected();
        let mut edges = vec![];
        for l in input.lines() {
            let caps = re.captures(l).unwrap();
            flows.insert(caps[1].to_string(), caps[2].parse().unwrap());
            node_ids.insert(caps[1].to_string(), caves.add_node(caps[1].to_string()));
            for dest in caps[3].split(", ") {
                edges.push((caps[1].to_string(), dest.to_string()))
            }
        }
        for edge in edges {
            caves.update_edge(
                *node_ids.get(&edge.0).unwrap(),
                *node_ids.get(&edge.1).unwrap(),
                1,
            );
        }
        Ok(CaveSystem {
            flows,
            node_ids,
            caves,
        })
    }
}
impl CaveSystem {
    fn condense_clever(&self) -> CaveSystem {
        let deletable_nodes: Vec<String> = self
            .caves
            .node_indices()
            .filter_map(|i| {
                if self.flows[&self.caves[i]] == 0 && self.caves.edges(i).count() == 2 {
                    return Some(self.caves[i].clone());
                }
                None
            })
            .collect();
        let mut caves = self.caves.clone();
        for d in deletable_nodes {
            let i = caves.node_indices().find(|i| caves[*i] == d).unwrap();
            let nw: isize = caves.edges(i).map(|e| e.weight()).sum();
            let nn: Vec<NodeIndex> = caves.neighbors(i).collect();
            dbg!(d, i, nw, &nn);
            caves.add_edge(nn[0], nn[1], nw);
            caves.remove_node(i);
        }
        let mut new_nodes = vec![];
        for i in caves.node_indices() {
            new_nodes.push((&caves[i]).clone());
        }
        CaveSystem {
            flows: new_nodes
                .iter()
                .map(|c| (c.clone(), self.flows[c]))
                .collect(),
            node_ids: new_nodes
                .iter()
                .map(|c| {
                    (
                        c.clone(),
                        caves.node_indices().find(|n| &caves[*n] == c).unwrap(),
                    )
                })
                .collect(),
            caves,
        }
    }
    fn condense(&self) -> CaveSystem {
        let mut caves = self.caves.filter_map(
            |_, n| match (self.flows[n], n.as_str()) {
                (_, "AA") => Some(n.clone()),
                (0, _) => None,
                (_, _) => Some(n.clone()),
            },
            |_, _| None,
        );
        let mut new_edges = vec![];
        let mut new_nodes = vec![];
        let g = &self.caves;
        for i in caves.node_indices() {
            new_nodes.push((&caves[i]).clone());
            let from = g.node_indices().find(|n| &g[*n] == &caves[i]).unwrap();
            for j in caves.node_indices() {
                if i == j || i > j {
                    continue;
                }
                let to = g.node_indices().find(|n| &g[*n] == &caves[j]).unwrap();
                let node_map = dijkstra(&self.caves, from, Some(to), |_| 1);
                new_edges.push((i, j, node_map[&to]))
            }
        }
        caves.extend_with_edges(&new_edges);
        CaveSystem {
            flows: new_nodes
                .iter()
                .map(|c| (c.clone(), self.flows[c]))
                .collect(),
            node_ids: new_nodes
                .iter()
                .map(|c| {
                    (
                        c.clone(),
                        caves.node_indices().find(|n| &caves[*n] == c).unwrap(),
                    )
                })
                .collect(),
            caves,
        }
    }
}

fn solve_pt1(cs: &CaveSystem, mut sol: Solution1, max_value: &mut isize) -> Solution1 {
    if sol.minutes_left() == 0
        || sol.max_flow_left(&cs) < *max_value
        || sol.max_flow_left(&cs) == sol.flow
    {
        return sol;
    }

    let cur_node = sol.cur_node.clone();
    let cur_node_name = (&cs.caves[cur_node]).clone();
    let time_spend = sol.time_spend;
    let flow = sol.flow.clone();

    if !sol.opened.contains(&cur_node_name) && cs.flows[&cur_node_name] > 0 {
        sol.opened.push(cur_node_name.clone());
        sol.time_spend += 1;
        sol.update_flow(cs.flows[&cur_node_name] * sol.minutes_left(), max_value);
        sol = solve_pt1(cs, sol, max_value);
        sol.opened.pop();
        sol.time_spend = time_spend;
        sol.flow = flow;
    }
    for e in cs.caves.edges(cur_node) {
        if sol.minutes_left() > *e.weight() && !sol.opened.contains(&cs.caves[e.target()]) {
            sol.cur_node = e.target();
            sol.time_spend += e.weight();
            sol = solve_pt1(cs, sol, max_value);
            sol.time_spend -= e.weight();
        }
    }
    sol.cur_node = cur_node;
    sol
}

fn solve_pt2(cs: &CaveSystem, mut sol: Solution2, max_value: &mut isize) -> Solution2 {
    // dbg!(&sol);
    // dbg!(sol.cur_best_outcome(&cs));
    dbg!(&max_value);
    if (26 - sol.time_spend.iter().min().unwrap()) == 0
        || sol.cur_best_outcome(&cs) < *max_value
        || sol.cur_best_outcome(&cs) == sol.flow
        || sol.cur_best_outcome(&cs) < 2590
    {
        return sol;
    }

    let cur_nodes = sol.cur_nodes.clone();
    let cur_node_names = [
        (&cs.caves[cur_nodes[0]]).clone(),
        (&cs.caves[cur_nodes[1]]).clone(),
    ];
    let time_spend = sol.time_spend;

    for p in 0..2 {
        if !sol.opened.contains(&cur_node_names[p]) && cs.flows[&cur_node_names[p]] > 0 {
            let flow = sol.flow.clone();
            sol.opened.push(cur_node_names[p].clone());
            sol.time_spend[p] += 1;
            sol.update_flow(
                cs.flows[&cur_node_names[p]] * (26 - sol.time_spend[p]),
                max_value,
            );
            sol = solve_pt2(cs, sol, max_value);
            sol.opened.pop();
            sol.time_spend = time_spend;
            sol.flow = flow;
        }
    }

    let p = if time_spend[0] <= time_spend[1] { 0 } else { 1 };
    let mut edges: Vec<petgraph::graph::EdgeReference<isize>> =
        cs.caves.edges(cur_nodes[p]).collect();
    let heuristic = |e: &EdgeReference<isize>| {
        (26 - sol.time_spend[p] - 1 - e.weight()) * cs.flows[&cs.caves[e.target()]]
    };
    edges.sort_by(|e1, e2| heuristic(e2).cmp(&heuristic(e1)));
    // dbg!(&edges
    //     .iter()
    //     .map(|e| cs.flows[&cs.caves[e.target()]])
    //     .collect::<Vec<isize>>());
    for e in edges {
        if (26 - sol.time_spend[p]) - 1 > *e.weight() {
            sol.cur_nodes[p] = e.target();
            sol.time_spend[p] += e.weight();
            sol = solve_pt2(cs, sol, max_value);
            sol.time_spend[p] -= e.weight();
        }
    }
    sol.time_spend = time_spend;
    sol.cur_nodes = cur_nodes;
    // }
    sol
}

fn main() {
    let input = include_str!("input.txt");
    let cave_system = CaveSystem::from_str(input).unwrap();
    dbg!(&cave_system);
    let cave_system = dbg!(cave_system.condense_clever());
    // let mut max_value = 0;
    // let sol = Solution1 {
    //     cur_node: cave_system
    //         .caves
    //         .node_indices()
    //         .find(|n| &cave_system.caves[*n] == "AA")
    //         .unwrap(),
    //     opened: vec![],
    //     time_spend: 0,
    //     flow: 0,
    // };
    // solve_pt1(&cave_system, sol, &mut max_value);
    // dbg!(max_value);

    let mut max_value = 0;
    let sol = Solution2 {
        cur_nodes: [cave_system
            .caves
            .node_indices()
            .find(|n| &cave_system.caves[*n] == "AA")
            .unwrap(); 2],
        time_spend: [0, 0],
        opened: vec![],
        flow: 0,
    };
    solve_pt2(&cave_system, sol, &mut max_value);
    dbg!(max_value);

    // dbg!(caves.neighbors(*node_ids.get("DD").unwrap()));
    // dbg!(caves.neighbors(*node_ids.get("DD").unwrap()).count());
    // dbg!(flows);
    // dbg!(node_ids);
    // dbg!(caves);
    // println!("Hello, world!");
}
