use std::error::Error;
use std::fs;

#[derive(Clone, Copy, Debug)]
enum OpponentMove {
    A = 1,
    B = 2,
    C = 3,
}

impl OpponentMove {
    fn new(chr: &str) -> Option<Self> {
        match chr {
            "A" => Option::from(Self::A),
            "B" => Option::from(Self::B),
            "C" => Option::from(Self::C),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum YouMove {
    X = 1,
    Y = 2,
    Z = 3,
}

impl YouMove {
    fn new(char: &str) -> Option<Self> {
        match char {
            "X" => Option::from(Self::X),
            "Y" => Option::from(Self::Y),
            "Z" => Option::from(Self::Z),
            _ => None,
        }
    }
}

fn score_one(you_move: YouMove, opponent_move: OpponentMove) -> Option<u32> {
    let move_score = you_move as u32;
    let result_score = match (opponent_move as i8 - you_move as i8 + 3) % 3 {
        0 => Some(3), // draw
        1 => Some(0), // loose
        2 => Some(6), // win
        _ => None,
    }?;
    Some(result_score + move_score)
}

fn score_two(you_move: YouMove, opponent_move: OpponentMove) -> Option<u32> {
    // Result is given
    let result_score = match you_move {
        YouMove::X => 0, // loose
        YouMove::Y => 3, // draw
        YouMove::Z => 6, // win
    };

    // Calculate which move number gives the wanted result
    let diff = match you_move as u32 {
        1 => Some(2), // diff to loose
        2 => Some(0), // diff for draw
        3 => Some(1), // diff to win
        _ => None,
    }?;
    let you_move_number = (opponent_move as u32 - 1 + diff) % 3 + 1;

    Some(result_score + you_move_number)
}

fn main() -> Result<(), Box<dyn Error>> {
    let moves: Vec<(OpponentMove, YouMove)> = fs::read_to_string("input.txt")?
        .split("\n")
        .map(|line| {
            let mut parts = line.split(" ");
            let opponent = OpponentMove::new(parts.next().unwrap()).unwrap();
            let you = YouMove::new(parts.next().unwrap()).unwrap();
            (opponent, you)
        })
        .collect();

    let answer1: u32 = moves
        .iter()
        .map(|(opponent_move, you_move)| score_one(*you_move, *opponent_move).unwrap())
        .sum();
    println!("{}", answer1);

    let answer2: u32 = moves
        .iter()
        .map(|(opponent_move, you_move)| score_two(*you_move, *opponent_move).unwrap())
        .sum();
    println!("{}", answer2);

    Result::Ok(())
}
