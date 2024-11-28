use arrayvec::ArrayVec;
use regex::Regex;
use std::{array, str::FromStr};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"^move (\d+) from (\d+) to (\d+)$"#).unwrap();
}

fn main() {
    let (initial_state_string, input) = include_str!("../input.txt").split_once("\n\n").unwrap();

    println!("{:?}", part1(initial_state_string, input));
    println!("{:?}", part2(initial_state_string, input));
}

fn part2(initial_state_string: &str, input: &str) -> String {
    let mut state = generate_initial_state(initial_state_string);

    for command in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        let idx = state[command.from].len() - command.amount;
        let popped = state[command.from].split_off(idx);
        state[command.to].extend(popped);
    }

    state
        .into_iter()
        .map(|mut v| v.pop().unwrap())
        .collect::<String>()
}

fn part1(initial_state_string: &str, input: &str) -> String {
    let mut state = generate_initial_state(initial_state_string);

    for command in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        for _ in 0..command.amount {
            let f = state[command.from].pop().unwrap();
            state[command.to].push(f);
        }
    }

    state
        .into_iter()
        .map(|mut v| v.pop().unwrap())
        .collect::<String>()
}

struct Command {
    amount: usize,
    from: usize,
    to: usize,
}

impl FromStr for Command {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let m = RE.captures(s).unwrap();
        Ok(Self {
            amount: m[1].parse::<usize>().unwrap(),
            from: m[2].parse::<usize>().unwrap() - 1,
            to: m[3].parse::<usize>().unwrap() - 1,
        })
    }
}

fn generate_initial_state(state_string: &str) -> [Vec<char>; 9] {
    let mut state: [Vec<char>; 9] = array::from_fn(|_| Vec::new());
    state_string.lines().rev().skip(1).for_each(|st| {
        st.chars()
            .skip(1)
            .step_by(4)
            .enumerate()
            .for_each(|(i, c)| {
                if c.is_ascii_alphabetic() {
                    state[i].push(c);
                }
            })
    });
    state
}
