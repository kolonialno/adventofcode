use std::collections::BinaryHeap;

use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part1(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|s| {
            s.split('\n')
                .fold(0, |acc, x| acc + x.parse::<u32>().unwrap())
        })
        .max()
        .unwrap()
}

fn part2(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|s| {
            s.split('\n')
                .fold(0, |acc, x| acc + x.parse::<u32>().unwrap())
        })
        .sorted()
        .rev()
        .take(3)
        .sum()
}
