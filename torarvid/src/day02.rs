use std::str::FromStr;
use crate::util::file_by_lines;

enum Instruction {
    Down(u64),
    Up(u64),
    Forward(u64),
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("down ") {
            Ok(Instruction::Down(s[5..].parse().unwrap()))
        } else if s.starts_with("up ") {
            Ok(Instruction::Up(s[3..].parse().unwrap()))
        } else if s.starts_with("forward ") {
            Ok(Instruction::Forward(s[8..].parse().unwrap()))
        } else {
            Err(format!("Can't parse instruction {}", s))
        }
    }
}

pub fn run() {
    let instructions: Vec<_> = file_by_lines("day02.txt").iter()
        .map(|s| Instruction::from_str(s).unwrap()).collect();

    let mut depth = 0;
    let mut forward = 0;
    for instruction in &instructions {
        match instruction {
            Instruction::Down(n) => depth += n,
            Instruction::Up(n) => depth -= n,
            Instruction::Forward(n) => forward += n,
        }
    }

    println!("Part 1: {}", depth * forward);

    // Part 2

    let mut aim = 0;
    let mut pos = 0;
    let mut depth = 0;
    for i in instructions {
        match i {
            Instruction::Down(n) => {
                aim += n;
            },
            Instruction::Up(n) => {
                aim -= n;
            },
            Instruction::Forward(n) => {
                pos += n;
                depth += aim * n;
            },
        }
    }

    println!("Part 2: {}", depth * pos);
}
