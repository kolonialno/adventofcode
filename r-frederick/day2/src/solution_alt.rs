use std::str::FromStr;

use Choice::{Paper, Rock, Scissors};
use RoundResult::{Draw, Lose, Win};

enum Choice {
    Rock,
    Paper,
    Scissors,
}

impl FromStr for Choice {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" | "X" => Ok(Rock),
            "B" | "Y" => Ok(Paper),
            "C" | "Z" => Ok(Scissors),
            _ => Err(format!("Invalid input received for choice {s}").into()),
        }
    }
}

impl From<Choice> for u32 {
    fn from(val: Choice) -> Self {
        match val {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }
}

struct Moves {
    opponent: Choice,
    your: Choice,
}

impl FromStr for Moves {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(" ") {
            Some((opponent_move, your_move)) => Ok(Moves {
                opponent: Choice::from_str(opponent_move)?,
                your: Choice::from_str(your_move)?,
            }),
            _ => Err("Error going from_str to Moves".into()),
        }
    }
}

impl From<Moves> for Outcome {
    fn from(val: Moves) -> Outcome {
        let result = match (&val.opponent, &val.your) {
            (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Win,
            (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Lose,
            (_, _) => Draw,
        };

        Outcome {
            result,
            opponent_move: val.opponent,
        }
    }
}

#[derive(Clone)]
enum RoundResult {
    Win,
    Lose,
    Draw,
}

impl FromStr for RoundResult {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(Lose),
            "Y" => Ok(Draw),
            "Z" => Ok(Win),
            _ => Err(format!("Invalid RoundResult input received {s}").into()),
        }
    }
}

impl From<RoundResult> for u32 {
    fn from(val: RoundResult) -> Self {
        match val {
            Win => 6,
            Lose => 0,
            Draw => 3,
        }
    }
}

impl From<Outcome> for Moves {
    fn from(o: Outcome) -> Self {
        let your_move: Choice = match (&o.opponent_move, o.result) {
            (Rock, Win) | (Scissors, Lose) | (Paper, Draw) => Paper,
            (Rock, Lose) | (Scissors, Draw) | (Paper, Win) => Scissors,
            (Rock, Draw) | (Scissors, Win) | (Paper, Lose) => Rock,
        };

        Moves {
            opponent: o.opponent_move,
            your: your_move,
        }
    }
}

struct Outcome {
    result: RoundResult,
    opponent_move: Choice,
}

impl From<Outcome> for u32 {
    fn from(val: Outcome) -> u32 {
        let total: u32 = val.result.clone().into();
        let your_move_value: u32 = Moves::from(val).your.into();

        total + your_move_value
    }
}

impl FromStr for Outcome {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(" ") {
            Some((opponent_move, result)) => Ok(Outcome {
                result: RoundResult::from_str(result)?,
                opponent_move: Choice::from_str(opponent_move)?,
            }),
            _ => Err("Error going from_str to Outcome".into()),
        }
    }
}

pub fn solve_part1(input: &Vec<&str>) -> u32 {
    input.iter().fold(0, |acc, line| {
        acc + u32::from(Outcome::from(Moves::from_str(line).unwrap()))
    })
}

pub fn solve_part2(input: &Vec<&str>) -> u32 {
    input.iter().fold(0, |acc, line| {
        acc + u32::from(Outcome::from_str(line).unwrap())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input = vec!["A Y", "B X", "C Z"];

        let expected = 15;

        assert_eq!(solve_part1(&input), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input = vec!["A Y", "B X", "C Z"];

        let expected = 12;

        assert_eq!(solve_part2(&input), expected);
    }
}
