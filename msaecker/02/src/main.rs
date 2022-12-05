use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use helpers::get_input_file;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Sign {
    Rock,
    Paper,
    Scissors,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Outcome {
    Win,
    Draw,
    Loss,
}

impl Outcome {
    fn parse(input: &str) -> Self {
        match input {
            "X" => Outcome::Loss,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => panic!("unknown outcome"),
        }
    }
}

impl Sign {
    fn parse(input: &str) -> Self {
        match input {
            "A" | "X" => Sign::Rock,
            "B" | "Y" => Sign::Paper,
            "C" | "Z" => Sign::Scissors,
            _ => panic!("Unknown sign"),
        }
    }
}

fn play(my_sign: Sign, other_sign: Sign) -> Outcome {
    match (my_sign, other_sign) {
        (Sign::Rock, Sign::Paper) => Outcome::Loss,
        (Sign::Rock, Sign::Scissors) => Outcome::Win,

        (Sign::Paper, Sign::Scissors) => Outcome::Loss,
        (Sign::Paper, Sign::Rock) => Outcome::Win,

        (Sign::Scissors, Sign::Rock) => Outcome::Loss,
        (Sign::Scissors, Sign::Paper) => Outcome::Win,

        (_, _) => Outcome::Draw,
    }
}

fn derive_sign(outcome: Outcome, other_sign: Sign) -> Sign {
    match outcome {
        Outcome::Win => match other_sign {
            Sign::Rock => Sign::Paper,
            Sign::Paper => Sign::Scissors,
            Sign::Scissors => Sign::Rock,
        },
        Outcome::Draw => other_sign,
        Outcome::Loss => match other_sign {
            Sign::Rock => Sign::Scissors,
            Sign::Paper => Sign::Rock,
            Sign::Scissors => Sign::Paper,
        },
    }
}

fn score(sign: Sign, outcome: Outcome) -> usize {
    let mut score = 0;
    match sign {
        Sign::Rock => score += 1,
        Sign::Paper => score += 2,
        Sign::Scissors => score += 3,
    }
    match outcome {
        Outcome::Win => score += 6,
        Outcome::Draw => score += 3,
        Outcome::Loss => {}
    }
    score
}

fn main() {
    let input_file = get_input_file().unwrap();
    let file = File::open(input_file.clone()).unwrap();
    let lines = BufReader::new(file).lines();

    // part 1
    let mut total_score = 0;
    for line in lines.flatten() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let other_sign = Sign::parse(parts[0]);
        let my_sign = Sign::parse(parts[1]);
        total_score += score(my_sign, play(my_sign, other_sign));
    }
    println!("Final score: {}", total_score);

    // part 2
    let file = File::open(input_file).unwrap();
    let lines = BufReader::new(file).lines();
    let mut total_score = 0;
    for line in lines.flatten() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let other_sign = Sign::parse(parts[0]);
        let outcome = Outcome::parse(parts[1]);
        let my_sign = derive_sign(outcome, other_sign);
        total_score += score(my_sign, play(my_sign, other_sign));
    }
    println!("Real score: {}", total_score);
}
