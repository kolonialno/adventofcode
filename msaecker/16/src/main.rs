use std::{
    cmp::max,
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

use helpers::get_input_file;

#[derive(Clone, Copy, Debug)]
struct Edge {
    cost: usize,
    source: usize,
    target: usize,
}

#[derive(Clone, Copy, Debug)]
struct FlowEdge {
    target: usize,
    duration: usize,
    flow_rate: usize,
}

#[derive(Clone, Debug)]
struct Vertex {
    name: String,
    index: usize,
    outgoing_edges: Vec<Edge>,
    valve_edges: Vec<FlowEdge>,
    flow_rate: usize,
}

impl Vertex {
    fn new(name: String, index: usize) -> Self {
        Self {
            name,
            index,
            outgoing_edges: Vec::default(),
            valve_edges: Vec::default(),
            flow_rate: 0,
        }
    }

    fn new_valve(name: String, flow_rate: usize, index: usize) -> Self {
        Self {
            name,
            index,
            outgoing_edges: Vec::default(),
            valve_edges: Vec::default(),
            flow_rate,
        }
    }

    fn add_edge(&mut self, edge: Edge) {
        self.outgoing_edges.push(edge);
    }

    fn is_valve(&self) -> bool {
        self.flow_rate > 0
    }
}

impl Edge {
    fn new(source: usize, target: usize, cost: usize) -> Self {
        Self {
            cost,
            source,
            target,
        }
    }
}

struct Graph {
    nodes: Vec<Vertex>,
}

impl Graph {
    const MAX_MINUTES: usize = 30;
    const MAX_MINUTES_ELEPHANT: usize = Self::MAX_MINUTES - 4;

    fn new(input: &str) -> Self {
        let mut graph = Graph {
            nodes: Vec::default(),
        };
        let mut node_indexes: HashMap<String, usize> = HashMap::default();
        let mut edges: Vec<(String, String)> = Vec::default();

        for line in input.lines() {
            let (lhs, rhs) =
                if let Some((lhs_i, rhs_i)) = line.split_once("; tunnels lead to valves ") {
                    (lhs_i, rhs_i)
                } else if let Some((lhs_i, rhs_i)) = line.split_once("; tunnel leads to valve ") {
                    (lhs_i, rhs_i)
                } else {
                    panic!("Stop screwing with me...");
                };
            let (valve, flow) = lhs.split_once(" has flow ").unwrap();
            let vertex_name = String::from(valve.split_once(' ').unwrap().1);
            let vertex_idx = graph.add_vertex(vertex_name.clone());

            node_indexes.insert(vertex_name.clone(), vertex_idx);

            let mut valve_name = String::from("Valve ");
            valve_name.push_str(vertex_name.as_str());
            let valve_flow = flow.split_once('=').unwrap().1.parse::<usize>().unwrap();
            if valve_flow > 0 {
                graph.add_valve(valve_name, valve_flow, vertex_idx);
            }

            if rhs.starts_with("s ") {
                let s = rhs.chars().skip(2).collect::<String>();
                for target in s.split(", ") {
                    edges.push((vertex_name.clone(), String::from(target)));
                }
            } else {
                for target in rhs.split(", ") {
                    edges.push((vertex_name.clone(), String::from(target)));
                }
            }
        }
        for (source, target) in edges {
            graph.add_edge(
                *node_indexes.get(&source).unwrap(),
                *node_indexes.get(&target).unwrap(),
            );
        }

        graph.add_valve_edges(0);
        for index in 1..graph.nodes.len() {
            if graph.nodes[index].is_valve() {
                graph.add_valve_edges(index);
            }
        }

        graph
    }

    fn add_valve_edges(&mut self, start: usize) {
        let mut unvisited = VecDeque::default();
        let start_edge = Edge::new(start, start, 0);
        unvisited.push_back((0, &start_edge));
        let mut visited: HashSet<usize> = HashSet::default();
        let mut weighted_edges: Vec<FlowEdge> = Vec::default();
        while !unvisited.is_empty() {
            let (m, edge) = unvisited.pop_front().unwrap();
            if visited.contains(&edge.target) {
                continue;
            }
            let target = &self.nodes[edge.target];
            visited.insert(edge.target);
            if target.is_valve() && edge.target != start {
                if !weighted_edges.iter().any(|x| x.target == edge.target) {
                    weighted_edges.push(FlowEdge {
                        target: edge.target,
                        duration: m,
                        flow_rate: target.flow_rate,
                    });
                }
            } else {
                if target
                    .outgoing_edges
                    .iter()
                    .any(|x| x.target == start_edge.source)
                    && !target.valve_edges.is_empty()
                {
                    // node already has all weighted edges and a back pointer
                    //weighted_edges;
                }
                for new_edge in target.outgoing_edges.iter() {
                    unvisited.push_back((m + new_edge.cost, new_edge));
                }
            }
        }
        self.nodes[start].valve_edges = weighted_edges;
    }

    fn add_vertex(&mut self, name: String) -> usize {
        let node_idx = self.nodes.len();
        let vertex = Vertex::new(name, node_idx);
        self.nodes.push(vertex);
        node_idx
    }

    fn add_valve(&mut self, name: String, flow_rate: usize, origin: usize) {
        let valve_idx = self.nodes.len();
        let mut valve = Vertex::new_valve(name, flow_rate, valve_idx);
        valve.add_edge(Edge::new(valve_idx, origin, 0));
        self.nodes.push(valve);
        self.nodes[origin].add_edge(Edge::new(origin, valve_idx, 1));
    }

    fn add_edge(&mut self, source: usize, target: usize) {
        self.nodes[source].add_edge(Edge::new(source, target, 1));
    }

    fn max_flow(&self, elephant: bool) -> usize {
        let max_minutes = if elephant {
            Self::MAX_MINUTES_ELEPHANT
        } else {
            Self::MAX_MINUTES
        };
        let start_edge = FlowEdge {
            target: 0,
            duration: 0,
            flow_rate: 0,
        };
        let mut max_flow = 0;
        let mut unvisited: Vec<UnvisitedElement> = Vec::default();
        unvisited.push(UnvisitedElement::new(
            &start_edge,
            &start_edge,
            HashSet::default(),
            0,
            0,
            0,
        ));
        while !unvisited.is_empty() {
            let mut elem = unvisited.pop().unwrap();

            let mut any = false;
            if (!elephant || elem.time_human <= elem.time_elephant)
                && (!elem.visited.contains(&elem.edge_human.target) || elem.edge_human.target == 0)
            {
                elem.time_human += elem.edge_human.duration;
                elem.cur_flow += elem.edge_human.flow_rate * (max_minutes - elem.time_human);
                elem.visited.insert(elem.edge_human.target);

                for edge_h in self.nodes[elem.edge_human.target]
                    .valve_edges
                    .iter()
                    .filter(|x| {
                        !elem.visited.contains(&x.target)
                            && x.duration < max_minutes - elem.time_human
                    })
                {
                    any = true;
                    unvisited.push(UnvisitedElement::new(
                        edge_h,
                        elem.edge_elephant,
                        elem.visited.clone(),
                        elem.cur_flow,
                        elem.time_human,
                        elem.time_elephant,
                    ));
                }
            }
            if !any
                && elephant
                && (!elem.visited.contains(&elem.edge_elephant.target)
                    || elem.edge_elephant.target == 0)
            {
                elem.time_elephant += elem.edge_elephant.duration;
                elem.cur_flow += elem.edge_elephant.flow_rate * (max_minutes - elem.time_elephant);
                elem.visited.insert(elem.edge_elephant.target);

                for edge_e in self.nodes[elem.edge_elephant.target]
                    .valve_edges
                    .iter()
                    .filter(|x| {
                        !elem.visited.contains(&x.target)
                            && x.duration < max_minutes - elem.time_elephant
                    })
                {
                    any = true;
                    unvisited.push(UnvisitedElement::new(
                        elem.edge_human,
                        edge_e,
                        elem.visited.clone(),
                        elem.cur_flow,
                        elem.time_human,
                        elem.time_elephant,
                    ));
                }
            }
            if !any {
                max_flow = max(max_flow, elem.cur_flow);
            }
        }
        max_flow
    }
}

struct UnvisitedElement<'a, 'b> {
    edge_human: &'a FlowEdge,
    edge_elephant: &'b FlowEdge,
    visited: HashSet<usize>,
    cur_flow: usize,
    time_human: usize,
    time_elephant: usize,
}
impl<'a, 'b> UnvisitedElement<'a, 'b> {
    fn new(
        edge_human: &'a FlowEdge,
        edge_elephant: &'b FlowEdge,
        visited: HashSet<usize>,
        cur_flow: usize,
        time_human: usize,
        time_elephant: usize,
    ) -> Self {
        Self {
            edge_human,
            edge_elephant,
            visited,
            cur_flow,
            time_human,
            time_elephant,
        }
    }
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    let graph = Graph::new(&input);

    println!("Max flow alone: {}", graph.max_flow(false));
    println!("Max flow elephant: {}", graph.max_flow(true));
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use crate::Graph;

    fn get_input() -> String {
        let mut test_file = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        test_file.push("test.txt");
        fs::read_to_string(test_file).unwrap()
    }

    #[test]
    fn part_1_example() {
        let graph = Graph::new(&get_input());
        assert_eq!(graph.max_flow(false), 1651);
    }

    #[test]
    fn part_2_example() {
        let graph = Graph::new(&get_input());
        assert_eq!(graph.max_flow(true), 1707);
    }
}
