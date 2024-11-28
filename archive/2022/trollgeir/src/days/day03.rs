use crate::{Solution, SolutionPair};
use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn char_to_priority(character: char) -> usize {
    if character.is_lowercase() {
        character as usize - 96
    } else {
        character as usize - 38
    }
}

pub fn solve() -> SolutionPair {
    // Task 1
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));

    let sum_priorities_1: usize = reader
        .lines()
        .map(|line| -> usize {
            let content = line.expect("Cannot parse line");
            let (a, b) = content.split_at(content.len() / 2);
            let diff_char = a.chars().filter(|c| b.chars().contains(c)).last().unwrap();
            char_to_priority(diff_char)
        })
        .sum();

    // Task 2
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));
    let sum_priorities_2: usize = reader
        .lines()
        .chunks(3)
        .into_iter()
        .map(|line_chunk| -> usize {
            let diff_char = line_chunk
                .map(|l| l.unwrap())
                .reduce(|a, b| a.chars().filter(|c| b.chars().contains(c)).collect())
                .unwrap()
                .chars()
                .last();

            char_to_priority(diff_char.unwrap())
        })
        .sum();

    (
        Solution::U32(sum_priorities_1 as u32),
        Solution::U32(sum_priorities_2 as u32),
    )
}
