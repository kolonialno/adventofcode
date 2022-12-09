use crate::{Solution, SolutionPair};
use std::fs::File;
use std::io::{BufRead, BufReader};

use regex::Regex;

fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}

fn read_config_and_run(multicrate: bool) -> String {
    // Get stack configuration
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));
    let stacks: Vec<Vec<char>> = reader
        .lines()
        .take_while(|p| !p.as_ref().unwrap().is_empty())
        .map(|line| {
            line.unwrap()
                .chars()
                .skip(1)
                .step_by(4)
                .collect::<Vec<char>>()
        })
        .collect();

    let instruction_sep_line = stacks.len() + 1;

    let mut stacks = transpose(stacks);

    // Prune empty characters and numbers (last line)
    stacks
        .iter_mut()
        .for_each(|stack| stack.retain(|c| c.is_alphabetic()));

    // Start simulation
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));
    let re = Regex::new("(\\d+)").unwrap();

    reader.lines().skip(instruction_sep_line).for_each(|line| {
        let ops: Vec<usize> = re
            .captures_iter(&line.unwrap())
            .map(|c| c[0].parse::<usize>().unwrap())
            .collect();

        let (num_moves, stack_a, stack_b) = (ops[0], ops[1] - 1, ops[2] - 1);

        (0..num_moves).rev().for_each(|i| {
            let crate_ = match multicrate {
                true => stacks[stack_a].remove(i),
                false => stacks[stack_a].remove(0),
            };
            stacks[stack_b].insert(0, crate_);
        });
    });

    stacks.iter().map(|s| &s[0]).collect::<String>()
}

pub fn solve() -> SolutionPair {
    let res1 = read_config_and_run(false);
    let res2 = read_config_and_run(true);

    (Solution::Str(res1), Solution::Str(res2))
}
