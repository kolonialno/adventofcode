use std::env;
use std::fs;
use std::io::{self, BufRead};
use std::ops::RangeInclusive;

#[derive(Debug)]
struct ElfPairTuple(RangeInclusive<u32>, RangeInclusive<u32>);

impl ElfPairTuple {
    fn from_string(s: String) -> ElfPairTuple {
        let elves: Vec<u32> = s
            .split(&[',', '-'])
            .map(|i| i.parse::<u32>().expect("Should be u32."))
            .collect();

        return ElfPairTuple(elves[0]..=elves[1], elves[2]..=elves[3]);
    }

    fn has_full_overlap(&self) -> bool {
        if self.0.contains(&self.1.start()) && self.0.contains(&self.1.end()) {
            return true;
        }
        if self.1.contains(&self.0.start()) && self.1.contains(&self.0.end()) {
            return true;
        }
        return false;
    }

    fn has_partial_overlap(&self) -> bool {
        if self.0.contains(&self.1.start()) || self.0.contains(&self.1.end()) {
            return true;
        }
        if self.1.contains(&self.0.start()) || self.1.contains(&self.0.end()) {
            return true;
        }
        return false;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let file = fs::File::open(file_path).expect("The given file path should be able to open.");
    let reader = io::BufReader::new(file);

    let mut full_overlap = 0;
    let mut partial_overlap = 0;

    for line in reader.lines() {
        let l = line.expect("Line should be readable.");
        let pair = ElfPairTuple::from_string(l);
        if pair.has_full_overlap() {
            full_overlap += 1;
        }
        if pair.has_partial_overlap() {
            partial_overlap += 1;
        }
    }

    println!("Number of fully overlapping elves: {}", full_overlap);
    println!("Number of partially overlapping elves: {}", partial_overlap);
}
