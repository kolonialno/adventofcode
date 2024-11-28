use regex::Regex;
use std::{
    fs::File,
    io::{BufRead, BufReader, Result},
};

fn main() -> Result<()> {
    let file = File::open("../input/day02a.txt")?;
    let input = BufReader::new(file);
    let policy_pattern = Regex::new(r"(\d+)-(\d+) (\w): (\w+)").unwrap();

    let mut total_valid_1: usize = 0;
    let mut total_valid_2: usize = 0;

    for line in input.lines() {
        let line = line?;
        let caps = policy_pattern.captures(&line).unwrap();
        let lower: usize = caps[1].parse().unwrap();
        let upper: usize = caps[2].parse().unwrap();
        let letter = &caps[3];
        let password = &caps[4];

        let range = lower..=upper;
        let letter_occurrences: usize = password.matches(&letter).count();
        let is_valid_1 = range.contains(&letter_occurrences);
        total_valid_1 += is_valid_1 as usize;

        let match1 = &password[lower - 1..=lower - 1] == letter;
        let match2 = &password[upper - 1..=upper - 1] == letter;
        total_valid_2 += (match1 ^ match2) as usize;
    }
    println!("Task 1: {}", total_valid_1);
    println!("Task 2: {}", total_valid_2);
    Ok(())
}
