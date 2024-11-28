use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
    time::Instant,
};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"\d+"#).unwrap();
}

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
            .map(|l| l.parse::<Card>().unwrap())
            .map(|card| card.winning_numbers.intersection(&card.numbers).count())
            .filter(|c| *c > 0)
            .map(|c| 2u32.pow(c as u32 - 1))
            .sum(),
    )
}

fn part2(input: &str) -> Option<usize> {
    let mut cards: HashMap<usize, usize> = HashMap::new();
    input
        .lines()
        .map(|l| l.parse::<Card>().unwrap())
        .for_each(|card| {
            let num_cards = *cards.entry(card.id).or_insert(1);
            let wins = card.winning_numbers.intersection(&card.numbers).count();
            for i in card.id + 1..=card.id + wins {
                *cards.entry(i).or_insert(1) += num_cards;
            }
        });
    Some(cards.values().sum())
}

#[derive(Debug)]
struct Card {
    id: usize,
    winning_numbers: HashSet<u32>,
    numbers: HashSet<u32>,
}

impl FromStr for Card {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut winning_numbers = HashSet::new();
        let mut numbers = HashSet::new();
        let (card, card_numbers) = s.split_once(": ").unwrap();
        let (w_nums, nums) = card_numbers.split_once(" | ").unwrap();
        let id = RE
            .captures(card)
            .unwrap()
            .get(0)
            .unwrap()
            .as_str()
            .parse::<usize>()
            .unwrap();
        for w_num in RE.captures_iter(w_nums) {
            winning_numbers.insert(w_num.get(0).unwrap().as_str().parse::<u32>().unwrap());
        }
        for w_num in RE.captures_iter(nums) {
            numbers.insert(w_num.get(0).unwrap().as_str().parse::<u32>().unwrap());
        }

        Ok(Self {
            id,
            winning_numbers,
            numbers,
        })
    }
}
