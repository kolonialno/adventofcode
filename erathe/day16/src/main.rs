use std::collections::{HashMap, HashSet, VecDeque};

use regex::{Captures, Regex};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"[A-Z]{2}|(\d+)"#).unwrap();
}

fn main() {
    println!("part 1: {:?}", part1());
    println!("part 2: {:?}", part2());
}

fn part2() -> i32 {
    let mut cave = Cave::default();
    include_str!("../input.txt")
        .lines()
        .for_each(|l| parse_valve(l, &mut cave));

    let start = cave.get_or_insert(String::from("AA"));

    // precalc all paths between rooms with valves;
    cave.generate_distances(start);

    let mut all: Vec<(i32, HashSet<usize>)> = Vec::new();
    let visited: HashSet<usize> = HashSet::new();
    dfs_2(&mut cave, start, 0, 26, visited, &mut all);
    let mut b_pairs = vec![];
    println!("{:?}", all.len());
    for (idx, (steam, set)) in all.clone().iter().enumerate() {
        if idx % 100 == 0 {
            println!("{idx}");
        }
        let mut best_pair = 0;
        all.iter().for_each(|(steam2, set2)| {
            if set.is_disjoint(set2) {
                best_pair = best_pair.max(steam + steam2);
            }
        });
        b_pairs.push(best_pair);
    }

    b_pairs.into_iter().max().unwrap()
}

fn dfs_2(
    cave: &Cave,
    point: usize,
    mut steam: i32,
    mut m_left: i32,
    mut visited: HashSet<usize>,
    all: &mut Vec<(i32, HashSet<usize>)>,
) {
    // out of time push and quit.
    if m_left <= 0 {
        all.push((steam, visited));
        return;
    }

    if cave.rooms_with_flow.contains(&point) {
        visited.insert(point);
        m_left -= 1;
        steam += m_left * cave.arena[point].flow_rate;
        if cave.all_valves_activated(&visited) {
            all.push((steam, visited));
            return;
        }
    }

    for target in cave.valid_targets(&visited) {
        let path = cave.distances.get(&(point, target)).unwrap();
        dfs_2(cave, target, steam, m_left - path, visited.clone(), all);
    }
}

fn part1() -> i32 {
    let mut cave = Cave::default();
    include_str!("../input-test.txt")
        .lines()
        .for_each(|l| parse_valve(l, &mut cave));

    let start = cave.get_or_insert(String::from("AA"));

    // precalc all paths between rooms with valves;
    cave.generate_distances(start);

    let mut all: Vec<i32> = Vec::new();
    let visited: HashSet<usize> = HashSet::new();
    dfs(&mut cave, start, 0, 30, visited, &mut all);
    all.into_iter().max().unwrap()
}

fn dfs(
    cave: &Cave,
    point: usize,
    mut steam: i32,
    mut m_left: i32,
    mut visited: HashSet<usize>,
    all: &mut Vec<i32>,
) {
    // out of time push and quit.
    if m_left <= 0 {
        all.push(steam);
        return;
    }

    if cave.rooms_with_flow.contains(&point) {
        visited.insert(point);
        m_left -= 1;
        steam += m_left * cave.arena[point].flow_rate;
        if cave.all_valves_activated(&visited) {
            all.push(steam);
            return;
        }
    }

    for target in cave.valid_targets(&visited) {
        let path = cave.distances.get(&(point, target)).unwrap();
        dfs(cave, target, steam, m_left - path, visited.clone(), all);
    }
}

#[derive(Debug, Default)]
struct Valve {
    value: String,
    idx: usize,
    flow_rate: i32,
    paths: Vec<usize>,
}

impl Valve {
    fn new(idx: usize, value: String) -> Self {
        Self {
            idx,
            value,
            flow_rate: 0,
            paths: vec![],
        }
    }
}

#[derive(Debug, Default)]
struct Cave {
    arena: Vec<Valve>,
    rooms_with_flow: HashSet<usize>,
    distances: HashMap<(usize, usize), i32>,
}

impl Cave {
    fn get_or_insert(&mut self, value: String) -> usize {
        for valve in &self.arena {
            if valve.value == value {
                return valve.idx;
            }
        }
        let idx = self.arena.len();
        self.arena.push(Valve::new(idx, value));
        idx
    }

    fn generate_distances(&mut self, start: usize) {
        let mut room_clone = self.rooms_with_flow.clone();
        room_clone.extend([start]);
        for room in &self.rooms_with_flow {
            for other in &room_clone {
                if self.distances.get(&(*room, *other)).is_none() {
                    let dist = self.shortest_path_to(*room, *other);
                    self.distances.insert((*room, *other), dist);
                    self.distances.insert((*other, *room), dist);
                }
            }
        }
    }

    fn all_valves_activated(&self, visited: &HashSet<usize>) -> bool {
        visited.eq(&self.rooms_with_flow)
    }

    fn shortest_path_to(&self, start: usize, end: usize) -> i32 {
        let mut q = VecDeque::from([start]);
        let mut came_from: HashMap<usize, usize> = HashMap::new();
        while let Some(c) = q.pop_back() {
            if c == end {
                break;
            }
            for n in &self.arena[c].paths {
                if came_from.get(n).is_none() {
                    q.push_front(*n);
                    came_from.insert(*n, c);
                }
            }
        }
        let mut current = end;
        let mut path = 0;
        while current != start {
            let c = came_from.get(&current).unwrap();
            path += 1;
            current = *c;
        }
        path
    }

    fn valid_targets(&self, visited: &HashSet<usize>) -> HashSet<usize> {
        self.rooms_with_flow
            .difference(visited)
            .map(|s| *s)
            .collect()
    }
}

fn parse_valve(inp: &str, cave: &mut Cave) {
    let caps: Vec<Captures> = RE.captures_iter(inp).collect();
    let idx = cave.get_or_insert(caps[0][0].to_string());
    let flow_rate = caps[1][0].parse::<i32>().unwrap();
    cave.arena[idx].flow_rate = flow_rate;
    if flow_rate > 0 {
        cave.rooms_with_flow.insert(idx);
    }

    let paths = caps
        .iter()
        .skip(2)
        .map(|cap| cave.get_or_insert(cap[0].to_string()))
        .collect::<Vec<usize>>();
    cave.arena[idx].paths = paths;
}
