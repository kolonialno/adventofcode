#![feature(extend_one)]
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref GRAPH: Caves = include_str!("../input.txt").parse::<Caves>().unwrap();
}

fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part2() -> u32 {
    dfs_2("start", "end", HashSet::new(), false)
}

fn part1() -> u32 {
    dfs("start", "end", HashSet::new())
}

fn dfs_2<'a>(source: &'a str, dest: &str, mut seen: HashSet<&'a str>, mut twice: bool) -> u32 {
    if source == dest {
        return 1;
    }
    if source == "start" && seen.contains("start") {
        return 0;
    }
    if seen.contains(source) && Caves::is_small_cave(source) {
        if twice == false {
            twice = true
        } else {
            return 0;
        }
    }
    seen.insert(source);

    let mut paths = 0;
    for neighbour in GRAPH.get_neighbours(source) {
        paths += dfs_2(neighbour, dest, seen.clone(), twice);
    }
    paths
}

fn dfs<'a>(source: &'a str, dest: &str, mut seen: HashSet<&'a str>) -> u32 {
    if source == dest {
        return 1;
    }
    if seen.contains(source) && Caves::is_small_cave(source) {
        return 0;
    }
    seen.insert(source);

    let mut paths = 0;
    for neighbour in GRAPH.get_neighbours(source) {
        paths += dfs(neighbour, dest, seen.clone());
    }
    paths
}

#[derive(Debug, Clone)]
struct Caves {
    tree: HashMap<String, Neighbours>,
}

type Neighbours = Vec<String>;

impl Caves {
    fn is_small_cave(node: &str) -> bool {
        node.chars().any(|c| c.is_ascii_lowercase())
    }

    fn get_neighbours(&self, node: &str) -> Vec<&str> {
        self.tree
            .get(node)
            .unwrap()
            .iter()
            .map(|n| n.as_str())
            .collect_vec()
    }
}

impl FromStr for Caves {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tree = HashMap::new();
        for l in s.lines() {
            let nodes = l.split("-").collect_vec();
            let s = nodes[0];
            let e = nodes[1];
            tree.entry(s.to_string())
                .or_insert(Vec::new())
                .push(e.to_string());
            tree.entry(e.to_string())
                .or_insert(Vec::new())
                .push(s.to_string());
        }

        Ok(Self { tree })
    }
}
