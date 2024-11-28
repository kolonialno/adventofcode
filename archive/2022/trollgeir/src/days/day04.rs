use itertools::Itertools;

use crate::{Solution, SolutionPair};
// use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct SectionAssignment {
    lower: u8,
    upper: u8,
}

impl SectionAssignment {
    fn from(section_assignment_string: &str) -> Self {
        let (lower, upper) = section_assignment_string
            .split('-')
            .into_iter()
            .map(|digit_string| digit_string.parse::<u8>().unwrap())
            .next_tuple()
            .unwrap();

        Self { lower, upper }
    }
    fn partially_overlaps(&self, other: &Self) -> bool {
        self.lower <= other.upper && self.upper >= other.lower
    }
    fn fully_overlaps(&self, other: &Self) -> bool {
        self.lower <= other.lower && self.upper >= other.upper
    }
    fn any_fully_overlaps(&self, other: &Self) -> bool {
        self.fully_overlaps(other) || other.fully_overlaps(self)
    }
}

fn find_overlapping_pairs(reader: BufReader<File>, full_overlap: bool) -> u32 {
    reader
        .lines()
        .map(|line| {
            let (first, second) = line
                .unwrap()
                .split(',')
                .map(SectionAssignment::from)
                .next_tuple()
                .unwrap();
            if full_overlap {
                first.any_fully_overlaps(&second) as u32
            } else {
                first.partially_overlaps(&second) as u32
            }
        })
        .sum()
}

pub fn solve() -> SolutionPair {
    // Task 1
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));
    let num_fully_overlapping_pairs = find_overlapping_pairs(reader, true);

    // Task 2
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));
    let num_partial_overlapping_pairs: u32 = find_overlapping_pairs(reader, false);

    (
        Solution::U32(num_fully_overlapping_pairs),
        Solution::U32(num_partial_overlapping_pairs),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_overlap() {
        let sa = SectionAssignment::from("4-6");
        assert!(sa.lower == 4);
        assert!(sa.upper == 6);

        let a = SectionAssignment { lower: 1, upper: 4 };
        let b = SectionAssignment { lower: 0, upper: 5 };
        assert!(!a.fully_overlaps(&b));
        assert!(b.fully_overlaps(&a));

        let a = SectionAssignment { lower: 4, upper: 8 };
        let b = SectionAssignment { lower: 0, upper: 5 };
        assert!(a.partially_overlaps(&b));
        assert!(b.partially_overlaps(&a));

        let a = SectionAssignment { lower: 6, upper: 8 };
        let b = SectionAssignment { lower: 0, upper: 5 };
        assert!(!a.partially_overlaps(&b));
        assert!(!b.partially_overlaps(&a));
    }
}
