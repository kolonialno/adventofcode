use itertools::Itertools;
use std::fs::read_to_string;

fn buffer_start(signal: &Vec<char>, marker_size: usize) -> usize {
    return (0..(signal.len() - marker_size))
        .filter(|&i| signal[i..i + marker_size].iter().duplicates().count() == 0)
        .next()
        .unwrap()
        + marker_size;
}

fn main() {
    let problem = &read_to_string("../input/6.txt")
        .unwrap()
        .trim()
        .chars()
        .collect();
    println!("Task 1: {}", buffer_start(problem, 4));
    println!("Task 2: {}", buffer_start(problem, 14));
}
