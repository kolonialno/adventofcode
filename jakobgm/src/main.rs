use std::fs;

#[derive(Debug)]
enum Outcome {
    Win,
    Draw,
    Loss,
}

impl Outcome {
    fn points(self: Self) -> usize {
        return match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Loss => 0,
        };
    }

    fn from(input: &str) -> Outcome {
        return match input {
            "X" => Outcome::Loss,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => panic!("Invalid Outcome specifier!"),
        };
    }
}

#[derive(PartialEq, Debug)]
enum Move {
    Rock,
    Paper,
    Scissor,
}

impl Move {
    fn from(input: &str) -> Move {
        return match input {
            "A" | "X" => Move::Rock,
            "B" | "Y" => Move::Paper,
            "C" | "Z" => Move::Scissor,
            _ => panic!("Invalid Move specifier!"),
        };
    }

    fn points(&self) -> usize {
        return match self {
            Move::Rock => 1,
            Move::Paper => 2,
            Move::Scissor => 3,
        };
    }

    fn wins_against(&self) -> Move {
        return match self {
            Move::Rock => Move::Scissor,
            Move::Paper => Move::Rock,
            Move::Scissor => Move::Paper,
        };
    }

    fn loses_against(&self) -> Move {
        return match self {
            Move::Rock => Move::Paper,
            Move::Paper => Move::Scissor,
            Move::Scissor => Move::Rock,
        };
    }

    fn play(&self, other: &Move) -> Outcome {
        if &self.wins_against() == other {
            return Outcome::Win;
        } else if self == &other.wins_against() {
            return Outcome::Loss;
        } else {
            return Outcome::Draw;
        }
    }
}

fn main() {
    let problem = fs::read_to_string("input/1.txt").unwrap();
    let mut elves: Vec<usize> = problem
        .trim()
        .split("\n\n")
        .map(|i| i.split("\n").map(|j| j.parse::<usize>().unwrap()))
        .map(|j| j.sum::<usize>())
        .collect();

    elves.sort();
    elves.reverse();
    println!("Part 1a: {}", elves[0]);
    println!("Part 1b: {}", elves[0..3].into_iter().sum::<usize>());

    let problem = fs::read_to_string("input/2.txt").unwrap();
    let mut part_a: usize = 0;
    let mut part_b: usize = 0;
    for line in problem.trim().split("\n") {
        let round: Vec<&str> = line.split(" ").collect();
        let them = Move::from(round[0]);
        let me = Move::from(round[1]);
        part_a += me.points() + me.play(&them).points();

        let outcome = Outcome::from(round[1]);
        let me = match outcome {
            Outcome::Loss => them.wins_against(),
            Outcome::Draw => them,
            Outcome::Win => them.loses_against(),
        };
        part_b += me.points() + outcome.points();
    }
    println!("Part 2a: {}", part_a);
    println!("Part 2b: {}", part_b);
}
