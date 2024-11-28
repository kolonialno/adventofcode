use petgraph::algo;
use petgraph::graph::NodeIndex;
use petgraph::Graph;

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<Vec<Vec<u32>>, Error> {
    let mut node_weights: Vec<Vec<u32>> = vec![];
    for line in BufReader::new(io).lines() {
        node_weights.push(
            line.unwrap()
                .trim()
                .chars()
                .map(|node_weight| node_weight.to_digit(10_u32).unwrap())
                .collect(),
        )
    }
    Ok(node_weights)
}

fn find_path(node_weights: &Vec<Vec<u32>>, extra_size: u32) {
    let mut graph = Graph::<u32, u32>::new();
    let mut nodes: Vec<Vec<NodeIndex>> = vec![];
    for step in 0..extra_size {
        for i in 0..node_weights.len() {
            let mut inner_nodes: Vec<NodeIndex> = vec![];
            for inner_step in 0..extra_size {
                for ii in 0..node_weights[i].len() {
                    inner_nodes.push(
                        graph.add_node((node_weights[i][ii] + step + inner_step - 1) % 9 + 1),
                    );
                }
            }
            nodes.push(inner_nodes);
        }
    }
    for i in 0..nodes.len() {
        for ii in 0..nodes[i].len() {
            if ii > 0 {
                graph.add_edge(nodes[i][ii - 1], nodes[i][ii], graph[nodes[i][ii]]);
                graph.add_edge(nodes[i][ii], nodes[i][ii - 1], graph[nodes[i][ii - 1]]);
            }
            if i > 0 {
                graph.add_edge(nodes[i - 1][ii], nodes[i][ii], graph[nodes[i][ii]]);
                graph.add_edge(nodes[i][ii], nodes[i - 1][ii], graph[nodes[i - 1][ii]]);
            }
        }
    }
    let path = algo::dijkstra(
        &graph,
        nodes[0][0],
        Some(nodes[nodes.len() - 1][nodes[0].len() - 1]),
        |e| *e.weight(),
    );
    println!(
        "path_length = {:?}",
        path[&nodes[nodes.len() - 1][nodes[0].len() - 1]]
    );
}

pub fn day15() {
    let node_weights = read(File::open("inputs/day15.txt").unwrap()).unwrap();
    find_path(&node_weights, 1);
    find_path(&node_weights, 5);
}
