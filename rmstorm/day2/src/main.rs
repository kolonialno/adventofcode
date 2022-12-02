#[derive(Debug, Clone, Copy)]
enum Choice {
    Rock,
    Paper,
    Scissor,
}

impl From<u8> for Choice {
    fn from(v: u8) -> Choice {
        match v as char {
            'A' => Choice::Rock,
            'B' => Choice::Paper,
            'C' => Choice::Scissor,
            'X' => Choice::Rock,
            'Y' => Choice::Paper,
            'Z' => Choice::Scissor,
            _ => panic!("{:?} was not valid", v),
        }
    }
}

impl From<Choice> for i32 {
    fn from(v: Choice) -> i32 {
        match v {
            Choice::Rock => 1,
            Choice::Paper => 2,
            Choice::Scissor => 3,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Outcome {
    Win,
    Draw,
    Lose,
}

impl From<u8> for Outcome {
    fn from(v: u8) -> Outcome {
        match v as char {
            'X' => Outcome::Lose,
            'Y' => Outcome::Draw,
            'Z' => Outcome::Win,
            _ => panic!("{:?} was not valid", v),
        }
    }
}

fn score1(them: Choice, you: Choice) -> i32 {
    let them: i32 = them.into();
    let you: i32 = you.into();
    you + 3 * ((you - them + 4) % 3)
}

fn score2(them: Choice, outcome: Outcome) -> i32 {
    match (them, outcome) {
        (Choice::Rock, Outcome::Lose) => score1(them, Choice::Scissor),
        (Choice::Rock, Outcome::Draw) => score1(them, Choice::Rock),
        (Choice::Rock, Outcome::Win) => score1(them, Choice::Paper),
        (Choice::Paper, Outcome::Lose) => score1(them, Choice::Rock),
        (Choice::Paper, Outcome::Draw) => score1(them, Choice::Paper),
        (Choice::Paper, Outcome::Win) => score1(them, Choice::Scissor),
        (Choice::Scissor, Outcome::Lose) => score1(them, Choice::Paper),
        (Choice::Scissor, Outcome::Draw) => score1(them, Choice::Scissor),
        (Choice::Scissor, Outcome::Win) => score1(them, Choice::Rock),
    }
}

fn main() {
    let input = include_bytes!("input.txt");
    dbg!(input
        .split(|e| *e as char == '\n')
        .map(|l| score1(l[0].into(), l[2].into()))
        .sum::<i32>());

    dbg!(input
        .split(|e| *e as char == '\n')
        .map(|l| score2(l[0].into(), l[2].into()))
        .sum::<i32>());
}
