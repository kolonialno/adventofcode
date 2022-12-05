use arrayvec::ArrayVec;
use regex::Regex;
use std::str::FromStr;

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

fn part2(initial_state_string: &str, input: &str) -> [char; 9] {
    let mut res: [char; 9] = ['_'; 9];
    let mut state = generate_initial_state(initial_state_string);

    for command in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        let mut buffer = ArrayVec::<char, 60>::new();
        for _ in 0..command.amount {
            buffer.insert(0, state[command.from].pop().unwrap());
        }
        state[command.to]
            .try_extend_from_slice(buffer.as_slice())
            .unwrap();
    }

    for i in 0..9 {
        res[i] = state[i].pop().unwrap();
    }
    res
}

fn part1(initial_state_string: &str, input: &str) -> [char; 9] {
    let mut res: [char; 9] = ['_'; 9];
    let mut state = generate_initial_state(initial_state_string);

    for command in input.lines().map(|l| l.parse::<Command>().unwrap()) {
        for _ in 0..command.amount {
            let f = state[command.from].pop().unwrap();
            state[command.to].push(f);
        }
    }

    for i in 0..9 {
        res[i] = state[i].pop().unwrap();
    }
    res
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

fn generate_initial_state(state_string: &str) -> ArrayVec<ArrayVec<char, 60>, 9> {
    let mut state: ArrayVec<ArrayVec<char, 60>, 9> = ArrayVec::from([
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
        ArrayVec::new_const(),
    ]);
    let mut indexes: [usize; 40] = [0; 40];
    state_string.lines().rev().for_each(|st| {
        st.chars().enumerate().for_each(|(i, c)| {
            if c.is_ascii_digit() {
                indexes[i] = c.to_digit(10).unwrap() as usize - 1;
            } else if c.is_ascii_alphabetic() {
                state[indexes[i]].push(c)
            }
        })
    });
    state
}
