#![feature(iter_advance_by)]
use std::{collections::HashMap, iter::Cycle, ops::RangeInclusive};

use itertools::Itertools;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref DIMENSIONS: Vec<u64> = [1, 1, 1, 2, 2, 2, 3, 3, 3]
        .into_iter()
        .permutations(3)
        .unique()
        .map(|s| s.iter().sum())
        .collect::<Vec<u64>>();
}

const BOARD: [u8; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
type Cache = HashMap<(u64, u64, u64, u64, bool), (u64, u64)>;
fn pts(p: u64) -> u64 {
    BOARD[((p - 1) as usize) % BOARD.len()] as u64
}
fn main() {
    println!("part1: {}", part1());
    println!("part2: {}", part2());
}

fn rec(p1: u64, s1: u64, p2: u64, s2: u64, turn: bool, cache: &mut Cache) -> (u64, u64) {
    if s1 >= 21 {
        return (1, 0);
    } else if s2 >= 21 {
        return (0, 1);
    }

    // There is probably a more concise way to write this
    // memoization-stuff
    let (mut p1w, mut p2w) = (0, 0);
    for i in DIMENSIONS.iter() {
        if turn {
            if let Some((p1inc, p2inc)) = cache.get(&(pts(p1 + i), s1 + pts(p1 + i), p2, s2, false))
            {
                p1w += p1inc;
                p2w += p2inc;
            } else {
                let (p1inc, p2inc) = rec(pts(p1 + i), s1 + pts(p1 + i), p2, s2, false, cache);
                p1w += p1inc;
                p2w += p2inc;
                cache.insert(
                    (pts(p1 + i), s1 + pts(p1 + i), p2, s2, false),
                    (p1inc, p2inc),
                );
            }
        } else {
            if let Some((p1inc, p2inc)) = cache.get(&(p1, s1, pts(p2 + i), s2 + pts(p2 + i), true))
            {
                p1w += p1inc;
                p2w += p2inc;
            } else {
                let (p1inc, p2inc) = rec(p1, s1, p2 + i, s2 + pts(p2 + i), true, cache);
                p1w += p1inc;
                p2w += p2inc;
                cache.insert(
                    (p1, s1, pts(p2 + i), s2 + pts(p2 + i), true),
                    (p1inc, p2inc),
                );
            }
        }
    }

    (p1w, p2w)
}

fn part2() -> u64 {
    let cache = &mut Cache::new();
    let (p1w, p2w) = rec(4, 0, 6, 0, true, cache);
    p1w.max(p2w)
}

// Nothing of this turned out to be reusable (:
fn part1() -> u64 {
    let (mut p1, mut p2) = (Player::new(4), Player::new(6));
    let mut die = Die::new(1..=100);
    loop {
        p1.position = pts(p1.position + die.roll());
        p1.points += p1.position;
        if p1.points >= 1000 {
            break;
        }

        p2.position = pts(p2.position + die.roll());
        p2.points += p2.position;
        if p2.points >= 1000 {
            break;
        }
    }

    p1.points.min(p2.points) * die.rolls
}

struct Player {
    points: u64,
    position: u64,
}

impl Player {
    fn new(position: u64) -> Self {
        Self {
            points: 0,
            position,
        }
    }
}

struct Die {
    rolls: u64,
    die: Cycle<RangeInclusive<u64>>,
}

impl Die {
    fn new(range: RangeInclusive<u64>) -> Self {
        Self {
            rolls: 0,
            die: range.into_iter().cycle(),
        }
    }

    fn roll(&mut self) -> u64 {
        let mut s = 0;
        for _ in 0..3 {
            self.rolls += 1;
            s += self.die.next().unwrap();
        }
        s
    }
}
