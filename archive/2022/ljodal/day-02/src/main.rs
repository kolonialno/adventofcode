use std::str::FromStr;

#[derive(Copy, Clone)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}

impl Hand {
    fn beats(&self) -> Hand {
        match self {
            Self::Rock => Self::Scissors,
            Self::Paper => Self::Rock,
            Self::Scissors => Self::Paper,
        }
    }

    fn loses_to(&self) -> Hand {
        match self {
            Self::Rock => Self::Paper,
            Self::Paper => Self::Scissors,
            Self::Scissors => Self::Rock,
        }
    }
}

impl FromStr for Hand {
    type Err = String;
    fn from_str(value: &str) -> Result<Hand, Self::Err> {
        match value {
            "A" => Ok(Self::Rock),
            "B" => Ok(Self::Paper),
            "C" => Ok(Self::Scissors),
            _ => Err("Invalid hand".into()),
        }
    }
}

enum DesiredOutcome {
    Win,
    Lose,
    Draw,
}

impl FromStr for DesiredOutcome {
    type Err = String;
    fn from_str(value: &str) -> Result<DesiredOutcome, Self::Err> {
        match value {
            "X" => Ok(Self::Lose),
            "Y" => Ok(Self::Draw),
            "Z" => Ok(Self::Win),
            _ => Err("Invalid outcome".into()),
        }
    }
}

fn choose_hand(opponent: &Hand, desired_outcome: DesiredOutcome) -> Hand {
    match desired_outcome {
        DesiredOutcome::Win => opponent.loses_to(),
        DesiredOutcome::Lose => opponent.beats(),
        DesiredOutcome::Draw => opponent.clone()
    }
}

fn calculate_game_score(opponent: Hand, you: Hand) -> i32 {
    let shape_points = match you {
        Hand::Rock => 1,
        Hand::Paper => 2,
        Hand::Scissors => 3,
    };

    let outcome_points = match (opponent, you) {
        // Opponent wins
        (Hand::Rock, Hand::Scissors) => 0,
        (Hand::Paper, Hand::Rock) => 0,
        (Hand::Scissors, Hand::Paper) => 0,
        // You win
        (Hand::Paper, Hand::Scissors) => 6,
        (Hand::Scissors, Hand::Rock) => 6,
        (Hand::Rock, Hand::Paper) => 6,
        // Draws
        _ => 3,
    };

    shape_points + outcome_points
}

fn calculate_score(input: &str) -> i32 {
    input
        .lines()
        .map(|hands| {
            let (opponent, desired_outcome) = hands.split_once(" ").expect("Invalid input");
            (
                Hand::from_str(opponent).expect("Invalid hand found"),
                DesiredOutcome::from_str(desired_outcome).expect("Invalid hand found"),
            )
        })
        .map(|(opponent, desired_outcome)| (choose_hand(&opponent, desired_outcome), opponent))
        .map(|(you, opponent)| calculate_game_score(opponent, you))
        .reduce(|total, this| total + this)
        .expect("No games played")
}

fn main() {
    let input = include_str!("./input.txt");
    let score = calculate_score(input);
    println!("Score: {score:?}");
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test() {
        let input = include_str!("./test.txt");
        assert_eq!(calculate_score(input), 12);
    }
}
