use crate::Hand::{Paper, Rock, Scissor};
use crate::OutCome::{Draw, Loss, Win};
use std::env;
use std::fs;
use std::io::{self, BufRead};

#[derive(Clone, Copy, PartialEq)]
enum Strategy {
    PartOne = 1,
    PartTwo = 2,
}

impl Strategy {
    fn from_string(value: &String) -> Strategy {
        let parse_result = value.parse::<u32>();
        match parse_result {
            Ok(1) => Strategy::PartOne,
            Ok(2) => Strategy::PartTwo,
            _ => panic!("Should be either 1 or 2."),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Hand {
    Rock,
    Paper,
    Scissor,
}

impl Hand {
    fn score(self) -> u32 {
        match self {
            Rock => 1,
            Paper => 2,
            Scissor => 3,
        }
    }

    fn parse(c: &str) -> Hand {
        match c {
            "X" | "A" => Rock,
            "Y" | "B" => Paper,
            "Z" | "C" => Scissor,
            _ => panic!["Cannot parse. Invalid char."],
        }
    }
}

#[derive(Clone, Copy)]
enum OutCome {
    Win,
    Draw,
    Loss,
}

impl OutCome {
    fn parse(c: &str) -> OutCome {
        match c {
            "X" => Loss,
            "Y" => Draw,
            "Z" => Win,
            _ => panic!["Cannot parse. Invalid char."],
        }
    }

    fn score(self) -> u32 {
        match self {
            Win => 6,
            Draw => 3,
            Loss => 0,
        }
    }
}

#[derive(Clone, Copy)]
struct GameRound {
    outcome: OutCome,
    player: Hand,
}

impl GameRound {
    fn total_score(self) -> u32 {
        self.outcome.score() + self.player.score()
    }

    fn get_outcome(opponent: Hand, player: Hand) -> OutCome {
        match (opponent, player) {
            (Rock, Paper) | (Scissor, Rock) | (Paper, Scissor) => Win,
            (Rock, Scissor) | (Scissor, Paper) | (Paper, Rock) => Loss,
            _ => Draw,
        }
    }

    fn get_player_hand(opponent: Hand, outcome: OutCome) -> Hand {
        match (opponent, outcome) {
            (Scissor, Win) | (Rock, Draw) | (Paper, Loss) => Rock,
            (Rock, Win) | (Paper, Draw) | (Scissor, Loss) => Paper,
            (Paper, Win) | (Scissor, Draw) | (Rock, Loss) => Scissor,
        }
    }

    fn parse(round_string: String, strategy: Strategy) -> GameRound {
        match strategy {
            Strategy::PartOne => {
                let round: Vec<&str> = round_string.split(' ').collect();

                let opponent = Hand::parse(round[0]);
                let player = Hand::parse(round[1]);

                GameRound {
                    outcome: Self::get_outcome(player, opponent),
                    player,
                }
            }
            Strategy::PartTwo => {
                let round: Vec<&str> = round_string.split(' ').collect();

                let opponent = Hand::parse(round[0]);
                let outcome = OutCome::parse(round[1]);

                GameRound {
                    outcome,
                    player: Self::get_player_hand(opponent, outcome),
                }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    let file = fs::File::open(file_path).expect("The given file path should be able to open.");
    let reader = io::BufReader::new(file);

    let strategy = Strategy::from_string(&args[2]);

    let score = reader
        .lines()
        .map(|l| l.expect("All lines in the file should be readable."))
        .fold(0, |total_score, game_string| {
            total_score + GameRound::parse(game_string, strategy).total_score()
        });
    println!("Total score: {}", score)
}
