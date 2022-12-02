use crate::Hand::{Paper, Rock, Scissors};
use crate::Outcome::{Draw, Lose, Win};

enum Outcome {
    Win,
    Lose,
    Draw,
}

impl Outcome {
    fn score(&self) -> i32 {
        match self {
            Win => 6,
            Lose => 0,
            Draw => 3,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}

impl Hand {
    fn value(&self) -> i32 {
        match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }

    fn parse(hand: &str) -> Hand {
        match hand {
            "A" | "X" => Rock,
            "B" | "Y" => Paper,
            "C" | "Z" => Scissors,
            _ => panic!("Unexpected hand"),
        }
    }

    fn beats(&self, other: Hand) -> Outcome {
        match (self, other) {
            (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => Win,
            (Scissors, Rock) | (Paper, Scissors) | (Rock, Paper) => Lose,
            _ => Draw,
        }
    }

    fn determined_decision(decision: &str, opponent: Hand) -> Hand {
        match (decision, &opponent) {
            // X = lose
            ("X", Rock) => Scissors,
            ("X", Paper) => Rock,
            ("X", Scissors) => Paper,
            // Y = draw
            ("Y", _) => opponent,
            // Z = win
            ("Z", Rock) => Paper,
            ("Z", Paper) => Scissors,
            ("Z", Scissors) => Rock,
            _ => panic!("Unexpected combo!"),
        }
    }
}

fn main() {
    let game: Vec<(Hand, Hand)> = include_str!("../input.txt")
        .lines()
        .collect::<Vec<&str>>()
        .into_iter()
        .map(line_to_play_based_on_assumption)
        .collect();

    let score = score_game(game);

    println!("Score based on guessed rules: {score}");

    let game2: Vec<(Hand, Hand)> = include_str!("../input.txt")
        .lines()
        .collect::<Vec<&str>>()
        .into_iter()
        .map(line_to_play_based_on_strategy)
        .collect();

    let score = score_game(game2);

    println!("Score with real rules: {score}");
}

fn line_to_play_based_on_assumption(line: &str) -> (Hand, Hand) {
    let hands = line.split(' ').map(Hand::parse).collect::<Vec<Hand>>();
    (hands[0].clone(), hands[1].clone())
}

fn line_to_play_based_on_strategy(line: &str) -> (Hand, Hand) {
    let parts = line.split(' ').collect::<Vec<&str>>();
    let opponent = Hand::parse(parts[0]);
    let me = Hand::determined_decision(parts[1], opponent.clone());
    (opponent, me)
}

fn score_game(game: Vec<(Hand, Hand)>) -> i32 {
    game.into_iter()
        .map(|(h1, h2)| {
            let hand_value = &h2.value();
            let exchange_value = h2.beats(h1).score();
            hand_value + exchange_value
        })
        .sum()
}

#[cfg(test)]
mod test {
    use crate::Hand::{Paper, Rock};
    use crate::{line_to_play_based_on_assumption, line_to_play_based_on_strategy, score_game};

    #[test]
    fn parse_play() {
        let input = include_str!("../sample.txt").lines().collect::<Vec<&str>>()[0];
        let (elf, me) = line_to_play_based_on_assumption(input);
        assert_eq!(elf, Rock);
        assert_eq!(me, Paper);
    }

    #[test]
    fn score_for_game1() {
        let game = include_str!("../sample.txt")
            .lines()
            .collect::<Vec<&str>>()
            .into_iter()
            .map(line_to_play_based_on_assumption)
            .collect();
        let score = score_game(game);
        assert_eq!(score, 15);
    }

    #[test]
    fn score_for_game2() {
        let game = include_str!("../sample.txt")
            .lines()
            .collect::<Vec<&str>>()
            .into_iter()
            .map(line_to_play_based_on_strategy)
            .collect();
        let score = score_game(game);
        assert_eq!(score, 12);
    }
}
