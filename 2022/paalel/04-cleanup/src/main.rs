use std::fs::File;
use std::io::{BufRead, BufReader};

struct Range {
    start: u32,
    end: u32,
}
impl Range {
    fn subset(&self, r2: &Range) -> bool {
        (self.start >= r2.start) && (self.end <= r2.end)
    }

    fn overlap(&self, r2: &Range) -> bool {
        (r2.start <= self.start && self.start <= r2.end)
            || (r2.start <= self.end && self.end <= r2.end)
    }
}

type T = Vec<(Range, Range)>;

fn from_str(r: &str) -> Range {
    let (u1, u2) = r.split_once("-").unwrap();
    Range {
        start: u1.parse().unwrap(),
        end: u2.parse().unwrap(),
    }
}

fn parse_input(filename: &str) -> T {
    let file = File::open(filename).expect("No file found!");
    let reader = BufReader::new(file);

    let mut ranges: T = vec![];
    for line in reader.lines().map(|l| l.expect("Could not parse line!")) {
        let (r1, r2) = line.split_once(",").unwrap();
        ranges.push((from_str(r1), from_str(r2)));
    }
    ranges
}
fn problem_1(ranges: &T) -> u32 {
    ranges
        .into_iter()
        .filter(|(r1, r2)| r1.subset(r2) | r2.subset(r1))
        .count()
        .try_into()
        .unwrap()
}

fn problem_2(ranges: &T) -> u32 {
    ranges
        .into_iter()
        .filter(|(r1, r2)| r1.overlap(r2) | r2.overlap(r1))
        .count()
        .try_into()
        .unwrap()
}

fn main() {
    println!("Hello, day 4!");

    let test_ranges = parse_input("test_input.txt");
    assert_eq!(problem_1(&test_ranges), 2);

    let ranges = parse_input("input.txt");
    println!("Solution to problem 1: {}", problem_1(&ranges));

    assert_eq!(problem_2(&test_ranges), 4);
    println!("Solution to problem 2: {}", problem_2(&ranges));
}
