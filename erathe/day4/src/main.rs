use regex::Regex;
use std::{ops::Range, str::FromStr};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"^(\d+)-(\d+),(\d+)-(\d+)$"#).unwrap();
}

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> usize {
    input
        .lines()
        .map(|l| l.parse::<ElfPair>().unwrap())
        .filter(|pair| range_overlaps(&pair.0, &pair.1))
        .count()
}

fn part1(input: &str) -> usize {
    input
        .lines()
        .map(|l| l.parse::<ElfPair>().unwrap())
        .filter(|pair| range_includes_other(&pair.0, &pair.1))
        .count()
}

fn range_includes_other(r1: &Range<usize>, r2: &Range<usize>) -> bool {
    r1.start <= r2.start && r1.end >= r2.end || r2.start <= r1.start && r2.end >= r1.end
}

fn range_overlaps(r1: &Range<usize>, r2: &Range<usize>) -> bool {
    r1.contains(&r2.start)
        || r1.contains(&(r2.end - 1))
        || r2.contains(&r1.start)
        || r2.contains(&(r1.end - 1))
}

struct ElfPair(Range<usize>, Range<usize>);

impl FromStr for ElfPair {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let m = RE.captures(s).unwrap();
        Ok(Self(
            m[1].parse::<usize>().unwrap()..m[2].parse::<usize>().unwrap() + 1,
            m[3].parse::<usize>().unwrap()..m[4].parse::<usize>().unwrap() + 1,
        ))
    }
}
