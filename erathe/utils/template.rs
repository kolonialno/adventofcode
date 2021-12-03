use regex::Regex;
use std::str::FromStr;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"^$"#).unwrap();
}

fn main() {
    let input = include_str!("../input.txt");

    // println!("{}", part1(input));
    // println!("{}", part2(input));
}

fn part2(input: &str) {}

fn part1(input: &str) {}

struct R {}

impl FromStr for R {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let m = RE.captures(s).unwrap();
        Ok(Self {})
    }
}
