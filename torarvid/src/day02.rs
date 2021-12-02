use crate::util::file_by_lines;

enum Instruction {
    Down(u64),
    Up(u64),
    Forward(u64),
}

impl Instruction {
    fn from_str(s: &str) -> Instruction {
        if s.starts_with("down ") {
            Instruction::Down(s[5..].parse().unwrap())
        } else if s.starts_with("up ") {
            Instruction::Up(s[3..].parse().unwrap())
        } else {
            Instruction::Forward(s[8..].parse().unwrap())
        }
    }
}

impl From<&String> for Instruction {
    fn from(s: &String) -> Instruction {
        Instruction::from_str(s)
    }
}

pub fn run() {
    let instructions: Vec<_> = file_by_lines("day02.txt").iter()
        .map(|s| Instruction::from(s)).collect();

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
