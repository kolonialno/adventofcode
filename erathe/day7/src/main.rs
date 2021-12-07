#![feature(int_abs_diff)]
use std::{collections::HashMap, time::Instant};

fn sumtorial(n: u32) -> u32 {
    n * ((n + 1) / 2)
}

fn main() {
    let (mut min, mut max) = (0, 0);
    let input = include_str!("../input.txt")
        .split(",")
        .map(|s| s.parse::<u32>().unwrap())
        .fold(HashMap::<u32, u32>::new(), |mut acc, c_pos| {
            if c_pos < min {
                min = c_pos
            } else if c_pos > max {
                max = c_pos
            }
            *acc.entry(c_pos).or_insert(0) += 1;
            acc
        });

    let start_1 = Instant::now();
    println!("part1: {}", part1(&input, min, max));
    println!("part 1 took: {:?}", start_1.elapsed());
    let start_2 = Instant::now();
    println!("part 2: {}", part2(&input, min, max));
    println!("part 2 took: {:?}", start_2.elapsed());
}

fn part2(input: &HashMap<u32, u32>, min: u32, max: u32) -> u32 {
    let mut diff = u32::MAX;
    for i in min..(max + 1) {
        let mut p_diff: u32 = 0;
        for (pos, crabs) in input {
            p_diff += sumtorial(i.abs_diff(*pos) * crabs);
            if p_diff > diff {
                break;
            }
        }
        if p_diff < diff {
            diff = p_diff
        }
    }
    diff
}

fn part1(input: &HashMap<u32, u32>, min: u32, max: u32) -> u32 {
    let mut diff = u32::MAX;
    for i in min..(max + 1) {
        let mut p_diff: u32 = 0;
        for (pos, crabs) in input {
            p_diff += i.abs_diff(*pos) * crabs;
            if p_diff > diff {
                break;
            }
        }
        if p_diff < diff {
            diff = p_diff
        }
    }
    diff
}
