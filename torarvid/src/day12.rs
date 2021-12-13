use std::collections::{HashMap, HashSet};

use crate::util::file_by_lines;

pub fn run() {
    let lines = file_by_lines("day12.txt");
    let mut g = Graph::new(lines);
    let paths = g.find_all_paths("start", "end", false);
    println!("Part 1: {}", paths.len());
    let paths = g.find_all_paths("start", "end", true);
    println!("Part 2: {}", paths.len());
}

#[derive(Debug)]
struct Graph {
    nodes: HashMap<String, Node>,
}

impl Graph {
    fn new(edge_strings: Vec<String>) -> Graph {
        let mut nodes = HashMap::new();

        for edge_string in edge_strings {
            let mut parts = edge_string.split("-");
            let from = parts.next().unwrap().to_string();
            let to = parts.next().unwrap().to_string();
            let from_node = nodes
                .entry(from.clone())
                .or_insert(Node::from(from.clone()));
            from_node.add_edge(to.clone());
            let to_node = nodes.entry(to.clone()).or_insert(Node::from(to.clone()));
            to_node.add_edge(from.clone());
        }

        Graph { nodes }
    }

    fn find_all_paths(&mut self, from: &str, to: &str, allow_twice: bool) -> Vec<Vec<String>> {
        fn rec(
            from: &str,
            to: &str,
            visited: &HashSet<String>,
            nodes: &HashMap<String, Node>,
            mut allow_twice: bool,
        ) -> Vec<Vec<String>> {
            let mut paths = Vec::new();
            if visited.contains(from) {
                if !allow_twice || from == "start" {
                    return vec![];
                }
                allow_twice = false;
            }
            let mut visited = visited.clone();
            let allow_multiple = from.chars().next().unwrap().is_uppercase();
            if !allow_multiple {
                visited.insert(from.to_string());
            }
            for edge in nodes.get(from).unwrap().edges.iter() {
                if edge == to {
                    paths.push(vec![to.to_string()]);
                } else {
                    let mut new_paths = rec(edge, to, &visited, nodes, allow_twice);
                    paths.append(&mut new_paths);
                }
            }
            for path in paths.iter_mut() {
                path.insert(0, from.to_string());
            }
            paths
        }
        rec(from, to, &HashSet::new(), &self.nodes, allow_twice)
    }
}

#[derive(Debug)]
struct Node {
    name: String,
    edges: HashSet<String>,
}

impl Node {
    fn from(name: String) -> Node {
        Node {
            name,
            edges: HashSet::new(),
        }
    }

    fn add_edge(&mut self, edge: String) {
        self.edges.insert(edge);
    }
}
