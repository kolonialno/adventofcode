use std::collections::{BinaryHeap, HashMap, HashSet};

use crate::util::file_by_lines;

pub fn run() {
    let lines = file_by_lines("day15.txt");
    let mut graph = Graph::new(lines);

    let start = (0, 0);
    let target = (graph.max_x, graph.max_y);
    let distances = graph.dijkstra(start);
    println!("Part 1: {}", distances.get(&target).unwrap());

    graph.expand_5x();
    let distances = graph.dijkstra(start);
    let target = (graph.max_x, graph.max_y);
    println!("Part 2: {}", distances.get(&target).unwrap());
}

struct Graph {
    nodes: HashMap<(i32, i32), u32>,
    max_x: i32,
    max_y: i32,
}

impl Graph {
    fn new(lines: Vec<String>) -> Graph {
        let mut nodes = HashMap::new();
        for (y, line) in lines.iter().enumerate() {
            for (x, char) in line.chars().enumerate() {
                nodes.insert((x as i32, y as i32), char.to_digit(10).unwrap());
            }
        }
        let (max_x, max_y) = (lines[0].len() as i32 - 1, lines.len() as i32 - 1);
        Graph {
            nodes,
            max_x,
            max_y,
        }
    }

    fn dijkstra(&self, start: (i32, i32)) -> HashMap<(i32, i32), u32> {
        let mut distances = HashMap::with_capacity(self.nodes.len());
        let mut visited = HashSet::with_capacity(self.nodes.len());
        let mut queue = BinaryHeap::with_capacity(1000);
        distances.insert(start, 0);
        queue.push(Node {
            cost: 0,
            pos: (0, 0),
        });

        while !queue.is_empty() {
            let current = queue.pop().unwrap();
            visited.insert(current.pos);
            for edge in self.edges_for(current.pos) {
                let next = edge.to;
                if !visited.contains(&next) {
                    let distance = distances.get(&current.pos).unwrap() + edge.weight;
                    if !distances.contains_key(&next) || distance < *distances.get(&next).unwrap() {
                        distances.insert(next, distance);
                        queue.push(Node {
                            cost: distance,
                            pos: next,
                        });
                    }
                }
            }
        }
        distances
    }

    fn edges_for(&self, node: (i32, i32)) -> Vec<Edge> {
        let mut edges = Vec::new();
        for (x, y) in vec![
            (node.0 + 1, node.1),
            (node.0 - 1, node.1),
            (node.0, node.1 + 1),
            (node.0, node.1 - 1),
        ] {
            if x >= 0 && x <= self.max_x && y >= 0 && y <= self.max_y {
                let value = self.nodes.get(&(x, y)).unwrap();
                let edge = Edge {
                    from: (node.0, node.1),
                    to: (x, y),
                    weight: *value,
                };
                edges.push(edge);
            }
        }
        edges
    }

    fn expand_5x(&mut self) {
        let new_max_x = 5 * (self.max_x + 1) - 1;
        let new_max_y = 5 * (self.max_y + 1) - 1;

        for y in 0..=new_max_y {
            for x in 0..=new_max_x {
                if x <= self.max_x && y <= self.max_y {
                    continue;
                }
                let src_x = x % (self.max_x + 1);
                let src_y = y % (self.max_y + 1);
                let adder_x: u32 = (x / (self.max_x + 1)).try_into().unwrap();
                let adder_y: u32 = (y / (self.max_y + 1)).try_into().unwrap();
                let mut val = (*self.nodes.get(&(src_x, src_y)).unwrap()) + adder_x + adder_y;
                if val > 9 {
                    val -= 9;
                }
                self.nodes.insert((x, y), val);
            }
        }
        self.max_x = new_max_x;
        self.max_y = new_max_y;
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct Edge {
    from: (i32, i32),
    to: (i32, i32),
    weight: u32,
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct Node {
    cost: u32,
    pos: (i32, i32),
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
