use std::io::{self, BufRead};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Outcome {
    Win,
    Loss,
    Draw,
}

impl From<&str> for Outcome {
    fn from(s: &str) -> Self {
        match s {
            "X" => Outcome::Loss,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => unreachable!(),
        }
    }
}

impl Outcome {
    fn score(self) -> i32 {
        match self {
            Self::Win => 6,
            Self::Draw => 3,
            Self::Loss => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}

impl From<&str> for Shape {
    fn from(s: &str) -> Self {
        match s {
            "A" => Self::Rock,
            "B" => Self::Paper,
            "C" => Self::Scissors,
            _ => unreachable!(),
        }
    }
}

impl Shape {
    fn from_own_str(s: &str) -> Self {
        match s {
            "X" => Self::Rock,
            "Y" => Self::Paper,
            "Z" => Self::Scissors,
            _ => unreachable!(),
        }
    }

    fn from_opponent_str(s: &str) -> Self {
        Self::from(s)
    }

    fn shape_for_outcome(&self, outcome: &Outcome) -> Self {
        if *outcome == Outcome::Draw {
            return self.clone();
        }
        match self {
            Self::Paper => match outcome {
                Outcome::Win => Self::Scissors,
                Outcome::Loss => Self::Rock,
                Outcome::Draw => unreachable!(),
            },
            Shape::Rock => match outcome {
                Outcome::Win => Self::Paper,
                Outcome::Loss => Self::Scissors,
                Outcome::Draw => unreachable!(),
            },
            Shape::Scissors => match outcome {
                Outcome::Win => Self::Rock,
                Outcome::Loss => Self::Paper,
                Outcome::Draw => unreachable!(),
            },
        }
    }

    fn selection_score(&self) -> i32 {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }

    fn outcome_against(&self, other: &Shape) -> Outcome {
        if self == other {
            return Outcome::Draw;
        }

        match self {
            Shape::Rock => match other {
                Shape::Scissors => Outcome::Win,
                Shape::Paper => Outcome::Loss,
                _ => unreachable!(),
            },
            Shape::Paper => match other {
                Shape::Rock => Outcome::Win,
                Shape::Scissors => Outcome::Loss,
                _ => unreachable!(),
            },
            Shape::Scissors => match other {
                Shape::Paper => Outcome::Win,
                Shape::Rock => Outcome::Loss,
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug)]
struct ExplicitRound(Shape, Shape);

impl From<&str> for ExplicitRound {
    fn from(s: &str) -> Self {
        let (s1, s2) = s.split_once(' ').unwrap();
        ExplicitRound(Shape::from_opponent_str(s1), Shape::from_own_str(s2))
    }
}

impl ExplicitRound {
    fn calculate_score(s: String) -> i32 {
        let round = ExplicitRound::from(s.as_str());
        round.score()
    }

    fn score(&self) -> i32 {
        self.1.outcome_against(&self.0).score() + self.1.selection_score()
    }
}

struct OutcomeRound(Shape, Outcome);

impl From<&str> for OutcomeRound {
    fn from(s: &str) -> Self {
        let (s1, s2) = s.split_once(' ').unwrap();
        Self(Shape::from(s1), Outcome::from(s2))
    }
}

impl OutcomeRound {
    fn calculate_score(s: String) -> i32 {
        Self::from(s.as_str()).score()
    }

    fn score(&self) -> i32 {
        let own_shape = self.0.shape_for_outcome(&self.1);
        own_shape.outcome_against(&self.0).score() + own_shape.selection_score()
    }
}

fn run_part_one(reader: impl BufRead) -> i32 {
    let iter = reader.lines().into_iter().map_while(|line| line.ok());
    let scores = iter.map(ExplicitRound::calculate_score);
    scores.sum()
}

fn run_part_two(reader: impl BufRead) -> i32 {
    let iter = reader.lines().into_iter().map_while(|line| line.ok());
    let scores = iter.map(OutcomeRound::calculate_score);
    scores.sum()
}

fn main() {
    // let part_one = run_part_one(io::stdin().lock());
    // println!("Part 1: {part_one}");

    let part_two = run_part_two(io::stdin().lock());
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use crate::{run_part_one, run_part_two};

    #[test]
    fn sample_part_one_is_correct() {
        let f = File::open("sample.txt").unwrap();
        let buf = BufReader::new(f);

        assert_eq!(run_part_one(buf), 15);
    }

    #[test]
    fn sample_part_two_is_correct() {
        let f = File::open("sample.txt").unwrap();
        let buf = BufReader::new(f);

        assert_eq!(run_part_two(buf), 12);
    }
}
