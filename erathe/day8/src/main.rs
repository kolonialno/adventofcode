use itertools::Itertools;
use std::{collections::HashMap, panic};
fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

fn part1(input: &str) -> usize {
    let uniq: [usize; 4] = [2, 3, 4, 7];
    input
        .lines()
        .flat_map(|s| {
            s.split(" | ")
                .skip(1)
                .flat_map(|s| s.split(" ").filter(|&s| uniq.contains(&s.len())))
        })
        .count()
}

fn part2(input: &str) -> i32 {
    let signals = input
        .lines()
        .map(|s| {
            s.split(" | ")
                .map(|s| s.split(" ").collect::<Vec<_>>())
                .collect::<Vec<Vec<_>>>()
        })
        .collect::<Vec<Vec<Vec<_>>>>();
    let mut res = 0;
    for s in signals {
        let mut solved = HashMap::<i32, String>::new();
        let input = &s[0];
        let output = &s[1];
        while solved.keys().len() < 10 {
            for i in input {
                match i.len() {
                    2 => {
                        solved.entry(1).or_insert(i.to_string());
                    }
                    3 => {
                        solved.entry(7).or_insert(i.to_string());
                    }
                    4 => {
                        solved.entry(4).or_insert(i.to_string());
                    }
                    5 => {
                        if let Some(v) = solved.get(&6) {
                            if i.as_bytes().iter().all(|i_b| v.as_bytes().contains(i_b)) {
                                solved.entry(5).or_insert(i.to_string());
                                continue;
                            }
                        }
                        if let Some(v) = solved.get(&1) {
                            if v.as_bytes().iter().all(|i_b| i.as_bytes().contains(i_b)) {
                                solved.entry(3).or_insert(i.to_string());
                                continue;
                            }
                        }
                        if solved.keys().contains(&5) && solved.keys().contains(&3) {
                            solved.entry(2).or_insert(i.to_string());
                            continue;
                        }
                    }
                    6 => {
                        if let Some(v) = solved.get(&4) {
                            if v.as_bytes().iter().all(|i_b| i.as_bytes().contains(i_b)) {
                                solved.entry(9).or_insert(i.to_string());
                                continue;
                            }
                        }
                        if solved.keys().contains(&9) {
                            if let Some(v) = solved.get(&1) {
                                if v.as_bytes().iter().all(|i_b| i.as_bytes().contains(i_b)) {
                                    solved.entry(0).or_insert(i.to_string());
                                    continue;
                                }
                            }
                        }
                        if solved.keys().contains(&9) && solved.keys().contains(&0) {
                            solved.entry(6).or_insert(i.to_string());
                            continue;
                        }
                    }
                    7 => {
                        solved.entry(8).or_insert(i.to_string());
                    }
                    _ => panic!("something went wrong, tried to match {}", i.len()),
                }
            }
        }

        let mut p_res = String::new();
        for l in output {
            for (key, val) in &solved {
                if val.as_bytes().iter().sorted().collect::<Vec<_>>()
                    == l.as_bytes().iter().sorted().collect::<Vec<_>>()
                {
                    p_res.push(char::from_digit(*key as u32, 10).unwrap());
                    break;
                }
            }
        }
        res += p_res.parse::<i32>().unwrap();
    }

    res
}
