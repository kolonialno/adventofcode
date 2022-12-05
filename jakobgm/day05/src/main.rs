use std::{array, fs::read_to_string, io::Error, str::FromStr};

#[derive(Debug)]
struct CrateMove {
    quantity: usize,
    source: usize,
    destination: usize,
}

impl FromStr for CrateMove {
    type Err = Error;

    fn from_str(instruction: &str) -> Result<CrateMove, Self::Err> {
        let captures: Vec<&str> = instruction.split_whitespace().skip(1).step_by(2).collect();
        Ok(CrateMove {
            quantity: captures[0].parse().unwrap(),
            source: captures[1].parse().unwrap(),
            destination: captures[2].parse().unwrap(),
        })
    }
}

impl CrateMove {
    fn apply(self: &Self, channels: &mut [Vec<char>; 9]) {
        for _ in 0..self.quantity {
            let object = channels[self.source - 1].pop().unwrap();
            channels[self.destination - 1].push(object);
        }
    }

    fn batch_apply(self: &Self, channels: &mut [Vec<char>; 9]) {
        let objects =
            channels[self.source - 1].split_off(channels[self.source - 1].len() - self.quantity);
        channels[self.destination - 1].extend(objects);
    }
}

fn main() {
    let problem = read_to_string("../input/5.txt").unwrap();
    let (stacks, procedure) = problem.trim().split_once("\n\n").unwrap();
    let mut channels: [Vec<char>; 9] = array::from_fn(|_| Vec::new());
    for row in stacks
        .split("\n")
        .collect::<Vec<&str>>()
        .iter()
        .rev()
        .skip(1)
    {
        for (channel, b) in row.chars().skip(1).step_by(4).enumerate() {
            if b.is_alphabetic() {
                channels[channel].push(b);
            }
        }
    }
    let mut new_channels = channels.clone();

    for instruction in procedure.split("\n") {
        let crate_move: CrateMove = instruction.parse().unwrap();
        crate_move.apply(&mut channels);
        crate_move.batch_apply(&mut new_channels);
    }
    println!(
        "Task 1: {}",
        channels
            .map(|channel| channel.last().unwrap().clone())
            .iter()
            .collect::<String>()
    );
    println!(
        "Task 2: {}",
        new_channels
            .map(|channel| channel.last().unwrap().clone())
            .iter()
            .collect::<String>()
    );
}
