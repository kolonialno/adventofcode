use std::{collections::VecDeque, str::FromStr};

use anyhow::Context;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
struct CrateStacks {
    stacks: Vec<VecDeque<char>>,
}

impl CrateStacks {
    /// Picks up the given number of crates from the given stack number.
    fn pick_crates(&mut self, stack_number: &usize, num_crates: usize) -> Vec<char> {
        let from_stack = &mut self.stacks[stack_number - 1];
        let mut crates = (0..num_crates)
            .map(|_| from_stack.pop_front().unwrap())
            .collect_vec();

        crates.reverse();
        crates
    }

    /// Places the given crates into the given stack number.
    fn place_crates(&mut self, stack_number: &usize, crates: Vec<char>) {
        for moved_crate in crates {
            self.stacks[stack_number - 1].push_front(moved_crate);
        }
    }

    /// Moves the affected crates one by one, effectively reversing their order.
    pub fn perform_moves_individually(&mut self, crate_move: Move) {
        for _ in 0..crate_move.count {
            let moved_crates = self.pick_crates(&crate_move.from, 1);
            let moved_crate = moved_crates.first().unwrap();

            self.stacks[&crate_move.to - 1].push_front(moved_crate.to_owned());
        }
    }

    /// Moves the affected crates without changing their orders, i.e. batched.
    pub fn perform_moves_batched(&mut self, crate_move: Move) {
        let moved_crates = self.pick_crates(&crate_move.from, crate_move.count);
        self.place_crates(&crate_move.to, moved_crates);
    }
}

/// String representation is the crates at the top of the stacks in order, i.e.
/// our puzzle answer format.
impl ToString for CrateStacks {
    fn to_string(&self) -> String {
        self.stacks
            .iter()
            .map(|stack| stack.front().unwrap_or(&' '))
            .join("")
    }
}

impl FromStr for CrateStacks {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Intermediary grid representation, each cell represented by an
        // Option<char>.
        let crate_layers: Vec<Vec<Option<char>>> = s
            .lines()
            // Only deal with lines describing crate placements.
            .take_while(|line| line.contains('['))
            // Parse each line, by splitting into (max) 4 character chunks and
            // grabbing the first alphabetic char from that segment.
            .map(|line| {
                line.chars()
                    .chunks(4)
                    .into_iter()
                    .map(|mut segment| segment.find(|c| c.is_alphabetic()))
                    .collect()
            })
            .collect();

        // Set up a VecDeque for each column in the grid representation.
        let num_stacks = crate_layers.first().unwrap().len();
        let mut stacks: Vec<VecDeque<char>> =
            Vec::from_iter((0..num_stacks).map(|_| VecDeque::new()));

        // Iterating through the rows in the grid, push chars into the back of
        // the stacks. We'll treat the front as the top.
        for crate_layer in crate_layers.into_iter() {
            for (stack_idx, c) in crate_layer.into_iter().enumerate() {
                if let Some(c) = c {
                    stacks[stack_idx].push_back(c);
                }
            }
        }

        Ok(Self { stacks })
    }
}

#[derive(Debug)]
struct Move {
    pub count: usize,
    pub from: usize,
    pub to: usize,
}

impl FromStr for Move {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
        }

        let cap = RE.captures(s).context("No Move match for given string")?;
        Ok(Self {
            count: cap.get(1).unwrap().as_str().parse().unwrap(),
            from: cap.get(2).unwrap().as_str().parse().unwrap(),
            to: cap.get(3).unwrap().as_str().parse().unwrap(),
        })
    }
}

/// Parses the two sections of the input. Mostly defers to the FromStr
/// implementations of CrateStacks and Move.
fn parse_input(s: &str) -> (CrateStacks, impl Iterator<Item = Move> + '_) {
    let stacks: CrateStacks = s.parse().unwrap();
    let moves = s.lines().filter_map(|line| Move::from_str(line).ok());

    (stacks, moves)
}

fn run_part_one(s: &str) -> String {
    let (mut stacks, moves) = parse_input(s);

    for crate_move in moves {
        stacks.perform_moves_individually(crate_move);
    }

    stacks.to_string()
}

fn run_part_two(s: &str) -> String {
    let (mut stacks, moves) = parse_input(s);

    for crate_move in moves {
        stacks.perform_moves_batched(crate_move);
    }

    stacks.to_string()
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), "CMZ");
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), "MCD");
    }
}
