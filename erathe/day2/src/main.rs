use std::{str::FromStr, time::Instant};

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {:?} in {:?}", part1(input), t1.elapsed());
    let t2 = Instant::now();
    println!("part 2: {:?} in {:?}", part2(input), t2.elapsed());
}

fn part1(input: &str) -> Option<u32> {
    Some(
        input
            .lines()
            .map(|g| g.parse::<Game>().unwrap())
            .filter(|game| {
                !game.rounds.iter().any(|r| {
                    r.red.is_some_and(|r| r > 12)
                        || r.green.is_some_and(|r| r > 13)
                        || r.blue.is_some_and(|r| r > 14)
                })
            })
            .map(|game| game.id)
            .sum(),
    )
}

fn part2(input: &str) -> Option<u32> {
    Some(
        input
            .lines()
            .map(|g| g.parse::<Game>().unwrap())
            .map(|game| {
                let red = game
                    .rounds
                    .iter()
                    .max_by_key(|r| r.red)
                    .unwrap()
                    .red
                    .unwrap();
                let blue = game
                    .rounds
                    .iter()
                    .max_by_key(|r| r.blue)
                    .unwrap()
                    .blue
                    .unwrap();
                let green = game
                    .rounds
                    .iter()
                    .max_by_key(|r| r.green)
                    .unwrap()
                    .green
                    .unwrap();
                red * green * blue
            })
            .sum(),
    )
}

struct Game {
    id: u32,
    rounds: Vec<Round>,
}

#[derive(Default)]
struct Round {
    blue: Option<u32>,
    red: Option<u32>,
    green: Option<u32>,
}

impl FromStr for Game {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (game, rounds) = s.split_once(": ").unwrap();
        let id = game.split(" ").last().unwrap().parse::<u32>().unwrap();
        let rounds = rounds
            .split("; ")
            .map(|round| {
                let mut r = Round::default();
                round.split(", ").for_each(|ball| {
                    let (num, color) = ball.split_once(" ").unwrap();
                    match color {
                        "red" => r.red = Some(num.parse::<u32>().unwrap()),
                        "blue" => r.blue = Some(num.parse::<u32>().unwrap()),
                        "green" => r.green = Some(num.parse::<u32>().unwrap()),
                        _ => panic!("shouldn't happen"),
                    }
                });
                r
            })
            .collect::<Vec<Round>>();
        Ok(Game { id, rounds })
    }
}
