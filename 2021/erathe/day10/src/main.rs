use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    str::FromStr,
};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref MAP: HashMap<char, char> =
        HashMap::<char, char>::from([('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]);
    static ref V_MAP: HashMap<char, u64> =
        HashMap::<char, u64>::from([(')', 1), (']', 2), ('}', 3), ('>', 4)]);
}
fn main() {
    let input = include_str!("../input.txt")
        .lines()
        .map(|c| c.parse::<Line>().unwrap())
        .collect_vec();

    println!("{}", part1(input.clone()));
    println!("{}", part2(input));
}

fn part2(input: Vec<Line>) -> u64 {
    let t = input
        .into_iter()
        .filter_map(Line::get_rem)
        .map(Line::get_value)
        .sorted()
        .collect_vec();
    t[t.len() / 2]
}

// Did this one before switching to using a struct
fn part1(input: Vec<Line>) -> u32 {
    let v_map = HashMap::<char, u32>::from([(')', 3), (']', 57), ('}', 1197), ('>', 25137)]);

    let mut invalid_chars = vec![];
    for line in input {
        let mut q = VecDeque::new();
        for c in line.get_raw() {
            if MAP.keys().contains(&c) {
                q.push_front(c);
                continue;
            } else {
                if let Some(m) = q.pop_front() {
                    if c != *MAP.get(&m).unwrap() {
                        invalid_chars.push(c);
                        break;
                    }
                }
            }
        }
    }

    invalid_chars
        .iter()
        .map(|c| v_map.get(c).unwrap())
        .sum::<u32>()
}

#[derive(Clone, Debug)]
struct Line(Vec<char>);

impl Line {
    fn get_raw(self) -> Vec<char> {
        self.0
    }

    fn get_value(self) -> u64 {
        self.0.into_iter().fold(0u64, |mut acc, c| {
            acc = (acc * 5) + V_MAP.get(&c).unwrap();
            acc
        })
    }

    fn get_rem(self) -> Option<Self> {
        let mut q = VecDeque::new();
        for c in self.0 {
            if MAP.keys().contains(&c) {
                q.push_front(c);
                continue;
            } else {
                if let Some(m) = q.pop_front() {
                    if c != *MAP.get(&m).unwrap() {
                        return None;
                    }
                }
            }
        }
        Some(Self(
            q.into_iter().map(|c| *MAP.get(&c).unwrap()).collect_vec(),
        ))
    }
}

impl FromStr for Line {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.chars().collect_vec()))
    }
}
