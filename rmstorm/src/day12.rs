use petgraph::graph::NodeIndex;
use petgraph::Graph;
use petgraph::Outgoing;

use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

trait VecPutAndGetIndex {
    type Item;
    fn push_gi(&mut self, item: Self::Item) -> usize;
}

impl<T> VecPutAndGetIndex for Vec<T> {
    type Item = T;
    fn push_gi(&mut self, item: T) -> usize {
        let idx = self.len();
        self.push(item);
        idx
    }
}

fn read<R: Read>(io: R) -> Result<Graph<String, String>, Error> {
    let mut dag = Graph::<String, String>::new();
    for line in BufReader::new(io).lines() {
        let nodes: Vec<String> = line
            .unwrap()
            .trim()
            .split("-")
            .map(|s| s.to_string())
            .collect();
        let mut edge_nodes: Vec<NodeIndex> = Vec::new();
        for node in nodes.into_iter() {
            if dag.node_weights().all(|nw| nw != &node) {
                edge_nodes.push(dag.add_node(node));
            } else {
                edge_nodes.push(dag.node_indices().find(|i| dag[*i] == node).unwrap());
            }
        }
        dag.add_edge(edge_nodes[0], edge_nodes[1], "0".to_string());
        if dag[edge_nodes[0]] != "start" && dag[edge_nodes[1]] != "end" {
            dag.add_edge(edge_nodes[1], edge_nodes[0], "0".to_string());
        }
    }
    Ok(dag)
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ValidPath {
    path: Vec<NodeIndex>,
    visited_twice: bool,
}

fn travers(dag: &Graph<String, String>, path: &mut ValidPath, total_paths: &mut u32) {
    for neigh in dag.neighbors_directed(path.path[path.path.len() - 1], Outgoing) {
        if dag[neigh] != "start".to_string()
            && (dag[neigh].to_uppercase() == dag[neigh]
                || !path.visited_twice
                || !path.path.contains(&neigh))
        {
            let visiting_twice = dag[neigh].to_uppercase() != dag[neigh]
                && path.path.contains(&neigh)
                && !path.visited_twice;
            if visiting_twice {
                path.visited_twice = true;
            }
            path.path.push(neigh);
            if dag[neigh] == "end" {
                *total_paths += 1;
            } else {
                travers(&dag, path, total_paths);
            }
            path.path.pop();
            if visiting_twice {
                path.visited_twice = false;
            }
        }
    }
}

pub fn day12() {
    let dag = read(File::open("inputs/day12.txt").unwrap()).unwrap();
    println!("dag: {:?}", dag);

    let start_index = dag
        .node_indices()
        .find(|i| dag[*i] == "start".to_string())
        .unwrap();
    let mut total_paths = 0;
    let mut path = &mut ValidPath {
        path: vec![start_index],
        visited_twice: false,
    };
    travers(&dag, path, &mut total_paths);

    println!("\nanswer1: {:?}", total_paths);
}
