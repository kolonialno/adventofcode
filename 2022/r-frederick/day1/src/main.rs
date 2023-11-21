use std::fs;

use day1::{solve_part1, solve_part2};

fn transform(input: String) -> Vec<Vec<u32>> {
    input
        .split("\n\n")
        .map(|line| line
             .lines()
             .map(|v| v.parse().unwrap())
             .collect())
        .collect()
}

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Unable to find input.txt");

    let input = transform(contents);

    println!("answer to part 1: {}", solve_part1(&input));
    println!("answer to part 2: {}", solve_part2(&input));
}
