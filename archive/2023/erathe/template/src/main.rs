use itertools::Itertools;
use regex::Regex;
use std::{str::FromStr, time::Instant};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#""#).unwrap();
}

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {:?} in {:?}", part1(input), t1.elapsed());
    let t2 = Instant::now();
    println!("part 2: {:?} in {:?}", part2(input), t2.elapsed());
}

fn part1(input: &str) -> Option<u64> {
    None
}

fn part2(input: &str) -> Option<u64> {
    None
}

struct SomeType;

impl FromStr for SomeType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let _m = RE.captures(s).unwrap();
        Ok(Self)
    }
}
