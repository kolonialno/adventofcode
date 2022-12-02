use std::str::FromStr;

fn main() {
    let input = include_str!("../input.txt");
    println!(
        "{:?}",
        input
            .lines()
            .map(|l| l.parse::<Game>().unwrap())
            .map(|game| game.resolve())
            .sum::<u16>()
    );
}

#[derive(Debug)]
enum Move {
    Rock,
    Paper,
    Scissor,
}

#[derive(Debug)]
enum Res {
    Win,
    Draw,
    Lose,
}

//part 1
// #[derive(Debug)]
// struct Game(Move, Move);
#[derive(Debug)]
struct Game(Move, Res);

impl Game {
    fn resolve(&self) -> u16 {
        match self {
            Self(Move::Rock, Res::Win) => 8,
            Self(Move::Rock, Res::Draw) => 4,
            Self(Move::Rock, Res::Lose) => 3,
            Self(Move::Paper, Res::Win) => 9,
            Self(Move::Paper, Res::Draw) => 5,
            Self(Move::Paper, Res::Lose) => 1,
            Self(Move::Scissor, Res::Win) => 7,
            Self(Move::Scissor, Res::Draw) => 6,
            Self(Move::Scissor, Res::Lose) => 2,
        }
    }

    //part 1
    // fn resolve(&self) -> u16 {
    //     match self {
    //         Self(Move::Rock, Move::Rock) => 4,
    //         Self(Move::Rock, Move::Paper) => 8,
    //         Self(Move::Rock, Move::Scissor) => 3,
    //         Self(Move::Paper, Move::Rock) => 1,
    //         Self(Move::Paper, Move::Paper) => 5,
    //         Self(Move::Paper, Move::Scissor) => 9,
    //         Self(Move::Scissor, Move::Rock) => 7,
    //         Self(Move::Scissor, Move::Paper) => 2,
    //         Self(Move::Scissor, Move::Scissor) => 6,
    //     }
    // }
}

impl FromStr for Game {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(
            match s.as_bytes()[0] {
                b'A' => Move::Rock,
                b'B' => Move::Paper,
                b'C' => Move::Scissor,
                _ => panic!("wtf"),
            },
            match s.as_bytes()[2] {
                b'X' => Res::Lose,
                b'Y' => Res::Draw,
                b'Z' => Res::Win,
                _ => panic!("wtf"),
            },
        ))
    }
}
