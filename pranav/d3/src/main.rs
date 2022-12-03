use itertools::{Chunk, Itertools};
use std::collections::HashSet;

const POINTS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

fn split_in_half(input: &str) -> (&str, &str) {
    input.split_at(input.len() / 2)
}

fn find_letter(input: &str) -> &str {
    let (first, second) = split_in_half(input);
    let a: HashSet<&str> = HashSet::from_iter(first.split("").collect::<Vec<&str>>());
    let b: HashSet<&str> = HashSet::from_iter(second.split("").collect::<Vec<&str>>());
    let mut c = &a & &b;
    c.remove("");
    c.into_iter().nth(0).unwrap()
}

fn find_letter2(input: Vec<&str>) -> &str {
    let a: HashSet<&str> = HashSet::from_iter(input[0].split("").collect::<Vec<&str>>());
    let b: HashSet<&str> = HashSet::from_iter(input[1].split("").collect::<Vec<&str>>());
    let c: HashSet<&str> = HashSet::from_iter(input[2].split("").collect::<Vec<&str>>());
    let d = &a & &b;
    let mut e = &c & &d;
    e.remove("");
    e.into_iter().nth(0).unwrap()
}

fn a() -> i32 {
    include_str!("input.txt")
        .lines()
        .map(|line| find_letter(line))
        .map(|letter| (POINTS.find(letter).unwrap() + 1) as i32)
        .sum()
}

fn b() -> i32 {
    include_str!("input.txt")
        .lines()
        .chunks(3)
        .into_iter()
        .map(|mut c| {
            let h1: HashSet<&str> =
                HashSet::from_iter(c.next().unwrap().split("").collect::<Vec<&str>>());
            let mut f = c.fold(h1, |acc, h| {
                let h2: HashSet<&str> = HashSet::from_iter(h.split("").collect::<Vec<&str>>());
                acc.intersection(&h2).copied().collect()
            });
            f.remove("");
            f.into_iter().nth(0).unwrap()
        })
        .map(|letter| (POINTS.find(letter).unwrap() + 1) as i32)
        .sum()
}

fn main() {
    // println!("{:?}", a());
    println!("{:?}", b());
}
