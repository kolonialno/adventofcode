use itertools::Itertools;
use std::{panic, time::Instant};
fn main() {
    let input = include_str!("../input.txt");

    let dur_1 = Instant::now();
    println!("part 1: {}", part1(&input));
    println!("part 1 took: {:?}", dur_1.elapsed());

    let dur_2 = Instant::now();
    println!("part 2: {}", part2(&input));
    println!("part 2 took: {:?}", dur_2.elapsed());
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
                .map(|s| {
                    s.split(" ")
                        .map(|s| s.chars().sorted().collect::<String>())
                        .collect_vec()
                })
                .collect_vec()
        })
        .collect_vec();
    let mut res = 0;
    for s in signals {
        let mut solved = [""; 10];
        let input = &s[0];
        let output = &s[1];
        while solved.contains(&"") {
            for i in input {
                match i.len() {
                    2 => solved[1] = i,
                    3 => solved[7] = i,
                    4 => solved[4] = i,
                    5 => {
                        if solved[6].len() > 0 {
                            if i.as_bytes()
                                .iter()
                                .all(|b| solved[6].as_bytes().contains(b))
                            {
                                solved[5] = i;
                                continue;
                            }
                        }
                        if solved[1].len() > 0 {
                            if solved[1]
                                .as_bytes()
                                .iter()
                                .all(|b| i.as_bytes().contains(b))
                            {
                                solved[3] = i;
                                continue;
                            }
                        }
                        if solved[5].len() > 0 && solved[3].len() > 0 {
                            solved[2] = i;
                            continue;
                        }
                    }
                    6 => {
                        if solved[4].len() > 0 {
                            if solved[4]
                                .as_bytes()
                                .iter()
                                .all(|b| i.as_bytes().contains(b))
                            {
                                solved[9] = i;
                                continue;
                            }
                        }
                        if solved[9].len() > 0 && solved[1].len() > 0 {
                            if solved[1]
                                .as_bytes()
                                .iter()
                                .all(|b| i.as_bytes().contains(b))
                            {
                                solved[0] = i;
                                continue;
                            }
                        }
                        if solved[9].len() > 0 && solved[0].len() > 0 {
                            solved[6] = i;
                            continue;
                        }
                    }
                    7 => solved[8] = i,
                    _ => panic!("something went wrong, tried to match {}", i),
                }
            }
        }

        let mut p_res = String::new();
        for l in output {
            if let Some((idx, _)) = solved
                .iter()
                .find_position(|&&s| s.as_bytes() == l.as_bytes())
            {
                p_res.push(char::from_digit(idx as u32, 10).unwrap());
            }
        }
        res += p_res.parse::<i32>().unwrap();
    }

    res
}
