use std::{collections::VecDeque, hash::Hash};

use itertools::Itertools;

fn first_distinct_buffer_ends<Item: Clone + Eq + Hash>(
    it: impl Iterator<Item = Item>,
    bufsize: usize,
) -> Option<usize> {
    let mut buf = VecDeque::new();

    for (i, c) in it.enumerate() {
        // Update the buffer
        if buf.len() == bufsize {
            buf.pop_back();
        }
        buf.push_front(c);

        // Check if the number of unique items in buf is equal to its length
        if buf.iter().unique().count() == bufsize {
            return Some(i);
        }
    }

    None
}

fn run_part_one(s: &str) -> usize {
    first_distinct_buffer_ends(s.chars(), 4).unwrap() + 1
}

fn run_part_two(s: &str) -> usize {
    first_distinct_buffer_ends(s.chars(), 14).unwrap() + 1
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
        assert_eq!(run_part_one(SAMPLE), 5);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
    }
}
