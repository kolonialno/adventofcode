use std::{collections::HashSet, ops::Range};

use anyhow::Result;

fn parse_range(s: &str) -> Result<HashSet<u32>> {
    let (i1, i2) = s.split_once('-').unwrap();
    let range: Range<u32> = i1.parse::<u32>()?..i2.parse::<u32>()? + 1;
    Ok(HashSet::from_iter(range))
}

fn parse_assignments(assignments: &str) -> (HashSet<u32>, HashSet<u32>) {
    let (r1, r2) = assignments.split_once(',').unwrap();

    (parse_range(r1).unwrap(), parse_range(r2).unwrap())
}

fn are_subsets((s1, s2): &(HashSet<u32>, HashSet<u32>)) -> bool {
    s1.is_subset(s2) || s2.is_subset(s1)
}

fn have_overlaps((s1, s2): &(HashSet<u32>, HashSet<u32>)) -> bool {
    !(s1 & s2).is_empty()
}

fn run_part_one(s: &str) -> usize {
    s.lines().map(parse_assignments).filter(are_subsets).count()
}

fn run_part_two(s: &str) -> usize {
    s.lines()
        .map(parse_assignments)
        .filter(have_overlaps)
        .count()
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
        assert_eq!(run_part_one(SAMPLE), 2);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 4);
    }
}
