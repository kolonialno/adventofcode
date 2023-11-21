use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::read_to_string;

const ITEMS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

fn split_into_compartments(contents: &str) -> (Vec<char>, Vec<char>) {
    let (cmp_1, cmp_2) = contents.split_at(contents.len() / 2);
    return (cmp_1.chars().collect(), cmp_2.chars().collect());
}

fn get_prio(c: char) -> usize {
    match ITEMS.find(c) {
        Some(x) => x + 1,
        None => panic!("Could not find {} in alphabet", c),
    }
}

fn get_intersection(cmp_1: Vec<char>, cmp_2: Vec<char>) -> Vec<char> {
    let (set_1, set_2): (HashSet<char>, HashSet<char>) = (
        cmp_1.iter().cloned().collect(),
        cmp_2.iter().cloned().collect(),
    );

    return set_1.intersection(&set_2).cloned().collect();
}

fn get_most_common_item(bags: Vec<&str>) -> char {
    let mut item_count = HashMap::new();

    for bag in bags {
        let unique_items: HashSet<char> = HashSet::from_iter(bag.chars());
        for item in unique_items.into_iter() {
            match item_count.get_mut(&item) {
                Some(i) => *i += 1,
                None => {
                    item_count.insert(item, 1);
                }
            }
        }
    }

    item_count.retain(|_, v| *v == 3);
    let most_common_item = *item_count
        .keys()
        .copied()
        .collect::<Vec<char>>()
        .first()
        .expect("Should not be empty");

    return most_common_item;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let problem = read_to_string(file_path).unwrap();
    let elves = problem
        .trim()
        .split("\n")
        .into_iter()
        .collect::<Vec<&str>>();

    let chuked_elves = problem.trim().split("\n").into_iter().chunks(3);

    let mut sum = 0;
    for line in elves {
        let (cmp_1, cmp_2) = split_into_compartments(line);
        let intersection = get_intersection(cmp_1, cmp_2);
        let prio = get_prio(*intersection.first().unwrap());
        sum += prio;
    }
    println!("Sum of duplicate items in each sack: {}", sum);

    let mut sum = 0;
    for elves in &chuked_elves {
        let most_common_item = get_most_common_item(elves.collect::<Vec<&str>>());
        let prio = get_prio(most_common_item);
        sum += prio
    }
    println!("Sum of all badges: {}", sum);
}
