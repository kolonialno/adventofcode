#![feature(int_abs_diff)]
use itertools::Itertools;
use std::{
    collections::{BinaryHeap, HashMap},
    str::FromStr,
};

const HALL: [Coord; 7] = [(1, 1), (1, 2), (1, 4), (1, 6), (1, 8), (1, 10), (1, 11)];

fn main() {
    let crabs = include_str!("../input.txt").parse::<Crabs>().unwrap();
    println!("{}", part1(crabs));
}

fn part1(crabs: Crabs) -> i32 {
    let mut cache = HashMap::<Vec<Crab>, i32>::new();
    let mut q = BinaryHeap::new();
    q.push(Cost(0, crabs.0));
    while let Some(Cost(curr_cost, state)) = q.pop() {
        if let Some(cost) = cache.get(&state) {
            if curr_cost > *cost {
                continue;
            }
        }
        if state.iter().all(|c| c.is_done()) {
            println!("{}", curr_cost);
            break;
        }

        for (idx, crab) in state.iter().enumerate() {
            if crab.is_room_valid(&state) {
                if crab.is_done() {
                    continue;
                }

                for c in crab.goal.iter().sorted().rev() {
                    if let Some(cost) =
                        check_and_do_move(crab, idx, c, &state, curr_cost, &mut cache)
                    {
                        q.push(cost);
                        break;
                    }
                }
            }

            // If crab is not in the hall (only valid move if in hall is to go home)
            // check all possible moves in hall
            if !crab.is_in_hall() {
                for c in &HALL {
                    if let Some(cost) =
                        check_and_do_move(crab, idx, c, &state, curr_cost, &mut cache)
                    {
                        q.push(cost);
                    }
                }
            }
        }
    }
    0
}

fn check_and_do_move(
    crab: &Crab,
    idx: usize,
    c: &Coord,
    state: &Vec<Crab>,
    curr_cost: i32,
    cache: &mut HashMap<Vec<Crab>, i32>,
) -> Option<Cost> {
    let n_cost = (path_cost(&crab.position, c, &state) * crab.energy) + curr_cost;
    if n_cost > curr_cost {
        let mut n_c = state.clone();
        n_c[idx].position = *c;
        if let Some(cost) = cache.get(&n_c) {
            if !(n_cost < *cost) {
                return None;
            }
        }
        cache.insert(n_c.clone(), n_cost);
        return Some(Cost(n_cost, n_c));
    }
    None
}

fn any_at_pos(crabs: &Vec<Crab>, c: Coord) -> bool {
    crabs.iter().any(|crab| crab.position == c)
}

// This is suboptimal
fn path_cost(c_c: &Coord, (e1, e2): &Coord, crabs: &Vec<Crab>) -> i32 {
    let (mut s1, mut s2) = c_c;
    while s1 > 1 {
        s1 -= 1;
        if any_at_pos(&crabs, (s1, s2)) {
            return 0;
        }
    }
    let dx = i32::signum(e2 - s2);
    while s2 != *e2 {
        s2 += dx;
        if any_at_pos(&crabs, (1, s2)) {
            return 0;
        }
    }
    if *e1 > 1 {
        while s1 < *e1 {
            s1 += 1;
            if any_at_pos(&crabs, (s1, *e2)) {
                return 0;
            }
        }

        return (c_c.0.abs_diff(1) + c_c.1.abs_diff(*e2) + e1.abs_diff(1)) as i32;
    }

    (c_c.0.abs_diff(1) + c_c.1.abs_diff(*e2)) as i32
}

// need this newtype to make binary-heap a min-heap
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Cost(i32, Vec<Crab>);

impl Ord for Cost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.0.cmp(&self.0).then_with(|| self.0.cmp(&other.0))
    }
}

impl PartialOrd for Cost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// This type is only for easy parsing with FromStr
#[derive(Debug, Clone)]
struct Crabs(Vec<Crab>);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct Crab {
    position: Coord,
    goal: Vec<Coord>,
    energy: i32,
}

impl Crab {
    fn new(goal: Vec<Coord>, position: Coord, energy: i32) -> Self {
        Self {
            position,
            goal,
            energy,
        }
    }

    fn is_done(&self) -> bool {
        self.goal.contains(&self.position)
    }

    fn is_room_valid(&self, crabs: &Vec<Crab>) -> bool {
        !crabs
            .iter()
            .filter(|&c| c.energy != self.energy)
            .any(|c| self.goal.contains(&c.position))
    }

    fn is_in_hall(&self) -> bool {
        self.position.0 == 1
    }
}

type Coord = (i32, i32);

impl FromStr for Crabs {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut crabs = Vec::new();
        for (row, line) in s.lines().enumerate() {
            for (col, c) in line.chars().enumerate() {
                let (row, col) = (row as i32, col as i32);
                match c {
                    'A' => crabs.push(Crab::new(
                        vec![(5, 3), (4, 3), (3, 3), (2, 3)],
                        (row, col),
                        1,
                    )),
                    'B' => crabs.push(Crab::new(
                        vec![(5, 5), (4, 5), (3, 5), (2, 5)],
                        (row, col),
                        10,
                    )),
                    'C' => crabs.push(Crab::new(
                        vec![(5, 7), (4, 7), (3, 7), (2, 7)],
                        (row, col),
                        100,
                    )),
                    'D' => crabs.push(Crab::new(
                        vec![(5, 9), (4, 9), (3, 9), (2, 9)],
                        (row, col),
                        1000,
                    )),
                    _ => {}
                }
            }
        }
        Ok(Crabs(crabs))
    }
}
