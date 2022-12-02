use crate::{Solution, SolutionPair};
use std::fs::File;
use std::io::{BufRead, BufReader};

///////////////////////////////////////////////////////////////////////////////
#[derive(Copy, Clone)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}

impl Hand {
    fn play(&self, other: Hand) -> Outcome {
        match (*self, other) {
            (Hand::Rock, Hand::Paper) => Outcome::Lose,
            (Hand::Paper, Hand::Rock) => Outcome::Win,
            (Hand::Rock, Hand::Scissors) => Outcome::Win,
            (Hand::Scissors, Hand::Rock) => Outcome::Lose,
            (Hand::Scissors, Hand::Paper) => Outcome::Win,
            (Hand::Paper, Hand::Scissors) => Outcome::Lose,
            _ => Outcome::Draw,
        }
    }

    fn get_hand_for_outcome(&self, outcome: Outcome) -> Hand {
        match outcome {
            Outcome::Win => match self {
                Hand::Rock => Hand::Paper,
                Hand::Paper => Hand::Scissors,
                Hand::Scissors => Hand::Rock,
            },
            Outcome::Lose => match self {
                Hand::Rock => Hand::Scissors,
                Hand::Paper => Hand::Rock,
                Hand::Scissors => Hand::Paper,
            },
            Outcome::Draw => *self,
        }
    }

    fn score(&self) -> u8 {
        match self {
            Hand::Rock => 1,
            Hand::Paper => 2,
            Hand::Scissors => 3,
        }
    }
}

enum Outcome {
    Win,
    Lose,
    Draw,
}

impl Outcome {
    fn score(&self) -> u8 {
        match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Lose => 0,
        }
    }
}

pub fn solve() -> SolutionPair {
    // Task 1
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open file.txt"));

    let total_score_solution_1 = reader
        .lines()
        .map(|line| {
            let parsed_line = line.unwrap();
            let mut symbol_iterator = parsed_line
                .split_whitespace()
                .map(|symbol| match symbol {
                    "A" | "X" => Some(Hand::Rock),
                    "B" | "Y" => Some(Hand::Paper),
                    "C" | "Z" => Some(Hand::Scissors),
                    _ => None,
                })
                .map(|hand| hand.expect("Symbol not recognized"));

            let other_hand = symbol_iterator.next().unwrap();
            let our_hand = symbol_iterator.next().unwrap();

            (our_hand.play(other_hand).score() + our_hand.score()) as u32
        })
        .sum();

    // Task 2
    let reader = BufReader::new(File::open("input/input.txt").expect("Cannot open file.txt"));

    let total_score_solution_2 = reader
        .lines()
        .map(|line| {
            let parsed_line = line.unwrap();
            let mut symbol_iterator = parsed_line.split_whitespace();

            let other_hand = symbol_iterator
                .next()
                .map(|line| match line {
                    "A" => Some(Hand::Rock),
                    "B" => Some(Hand::Paper),
                    "C" => Some(Hand::Scissors),
                    _ => None,
                })
                .map(|hand| hand.expect("Symbol not recognized"))
                .unwrap();

            let our_hand = symbol_iterator
                .next()
                .map(|line| match line {
                    "X" => Some(Outcome::Lose),
                    "Y" => Some(Outcome::Draw),
                    "Z" => Some(Outcome::Win),
                    _ => None,
                })
                .map(|outcome| outcome.expect("Symbol not recognized"))
                .map(|outcome| other_hand.get_hand_for_outcome(outcome))
                .unwrap();

            (our_hand.play(other_hand).score() + our_hand.score()) as u32
        })
        .sum();

    (
        Solution::U32(total_score_solution_1),
        Solution::U32(total_score_solution_2),
    )
}
