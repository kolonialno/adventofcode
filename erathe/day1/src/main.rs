use regex::Regex;
use std::{collections::BinaryHeap, str::FromStr};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"^$"#).unwrap();
}

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|s| {
            s.split('\n')
                .fold(0, |acc, x| acc + x.parse::<u32>().unwrap())
        })
        .max()
        .unwrap()
}

fn part1(input: &str) -> u32 {
    let s = input.split("\n\n").map(|s| {
        s.split('\n')
            .fold(0, |acc, x| acc + x.parse::<u32>().unwrap())
    });
    let mut largest = BinaryHeap::from_iter(s);
    let mut res = 0;
    for _ in 0..3 {
        res += largest.pop().unwrap();
    }
    res
}
