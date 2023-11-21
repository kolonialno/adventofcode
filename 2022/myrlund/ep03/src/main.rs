use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::str::Lines;

use itertools::{Chunk, Itertools};

fn split_at_middle(s: &str) -> (&str, &str) {
    let middle = s.len() / 2;
    s.split_at(middle)
}

fn first_shared_char((s1, s2): (&str, &str)) -> char {
    for c in s2.chars() {
        if s1.contains(c) {
            return c;
        }
    }

    unreachable!()
}

fn run_part_one(s: &str) -> u32 {
    s.lines()
        .map(split_at_middle)
        .map(first_shared_char)
        .map(score_char)
        .sum()
}

fn find_shared_char(lines: Chunk<Lines>) -> char {
    // Find the set intersection between the characters within the given lines
    let shared_chars: HashSet<char, RandomState> = lines
        .map(|line| HashSet::from_iter(line.chars()))
        .reduce(|acc, set| &acc & &set)
        .unwrap();

    // Pull out the first (only) shared char
    *shared_chars.iter().next().unwrap()
}

fn run_part_two(s: &str) -> u32 {
    let line_chunks = s.lines().chunks(3);
    line_chunks
        .into_iter()
        .map(find_shared_char)
        .map(score_char)
        .sum()
}

fn score_char(c: char) -> u32 {
    let ascii = u32::from(c);

    if ascii < 96 {
        ascii - 38
    } else {
        ascii - 96
    }
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
        assert_eq!(run_part_one(SAMPLE), 157);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 70);
    }
}
