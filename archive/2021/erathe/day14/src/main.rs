#![feature(slice_group_by)]

use itertools::{Itertools, MinMaxResult};
use std::{collections::HashMap, str::FromStr};

fn main() {
    let mut pair_map = HashMap::new();
    let (polymer, instructions) = include_str!("../input.txt")
        .split("\n\n")
        .collect_tuple()
        .unwrap();
    for instruction in instructions.lines() {
        let (t, c) = instruction.split(" -> ").collect_tuple().unwrap();
        pair_map.insert(t.chars().collect_vec(), char::from_str(c).unwrap());
    }

    println!("{}", part1(&pair_map, polymer));
    println!("{}", part2(&pair_map, polymer));
}

// struct for first attempt at part 2
// struct Link {
//     prev: Option<char>,
//     next: Option<char>,
//     v: char,
// }

// impl Link {
//     fn new(prev: Option<char>, next: Option<char>, v: char) -> Self {
//         Self { prev, next, v }
//     }
// }

fn part2(pair_map: &HashMap<Vec<char>, char>, init: &str) -> u64 {
    let mut dp_vec = Vec::new();
    let init_v = init.chars().tuple_windows().collect::<Vec<(char, char)>>();

    // get first iteration and insert into vector to hold all iterations
    let mut it_0 = HashMap::new();
    for &k in &init_v {
        *it_0.entry(Vec::from([k.0, k.1])).or_insert(0) += 1
    }
    dp_vec.push(it_0);

    for i in 0..40 {
        let mut new_map = HashMap::new();
        for (key, value) in &dp_vec[i] {
            if let Some(c) = pair_map.get(key) {
                *new_map.entry(Vec::from([key[0], *c])).or_insert(0u64) += value;
                *new_map.entry(Vec::from([*c, key[1]])).or_insert(0u64) += value;
            }
        }
        dp_vec.push(new_map);
    }

    let mut res = HashMap::new();
    let back_value = init.chars().nth_back(0).unwrap();
    for (key, value) in &dp_vec[40] {
        if key.contains(&back_value) {
            if key[1] == back_value {
                *res.entry(key[1]).or_insert(0) += value;
            }
            if key[0] != back_value {
                *res.entry(key[0]).or_insert(0) += value;
            }
        } else {
            *res.entry(key[0]).or_insert(0) += value;
        }
    }

    match res.values().minmax() {
        MinMaxResult::MinMax(a, b) => b - a,
        MinMaxResult::NoElements => 0,
        MinMaxResult::OneElement(_) => 0,
    }

    // first attempt at part 2 using a sort of linked list
    // to see if it was fast enough to always push to the back of a
    // preallocated vec (Spoiler alert: it was not)

    // let chars = init.chars().collect_vec();
    // let mut links = Vec::with_capacity(600_000_000_000);
    // for i in 0..chars.len() {
    //     if i == 0 {
    //         links.push(Link::new(None, Some(chars[i + 1]), chars[i]));
    //         continue;
    //     } else if i == chars.len() - 1 {
    //         links.push(Link::new(Some(chars[i - 1]), None, chars[i]));
    //         continue;
    //     } else {
    //         links.push(Link::new(Some(chars[i - 1]), Some(chars[i + 1]), chars[i]));
    //     }
    // }
    // for i in 0..40 {
    //     let len = links.len();
    //     let mut prev: char = 'l';
    //     for i in 0..len {
    //         if let Some(n_v) = links[i].next {
    //             if let Some(c) = pair_map.get(&Vec::from([links[i].v, n_v])) {
    //                 links.push(Link::new(Some(links[i].v), Some(n_v), *c));
    //                 links[i].next = Some(*c);
    //                 if prev != 'l' {
    //                     links[i].prev = Some(prev);
    //                 }
    //                 prev = *c;
    //             }
    //         }
    //     }
    // }
    // let link_vals = links.iter().map(|l| l.v).collect_vec();
    // match link_vals
    //     .into_iter()
    //     .sorted()
    //     .collect_vec()
    //     .group_by(|a, b| a == b)
    //     .minmax_by_key(|&c| c.len())
    // {
    //     MinMaxResult::MinMax(a, b) => b.len() - a.len(),
    //     MinMaxResult::NoElements => 0,
    //     MinMaxResult::OneElement(_) => 0,
    // }
}

// bruteforce for part 1
fn part1(pair_map: &HashMap<Vec<char>, char>, init: &str) -> usize {
    let mut n_vec = init.chars().collect_vec();
    for _ in 0..10 {
        let mut w_vec = Vec::new();
        for i in 0..n_vec.len() - 1 {
            let p = Vec::from([n_vec[i], n_vec[i + 1]]);
            w_vec.push(n_vec[i]);
            if let Some(m) = pair_map.get(&p) {
                w_vec.push(*m);
            }
            if i == n_vec.len() - 2 {
                w_vec.push(n_vec[i + 1])
            }
        }
        n_vec = w_vec.clone();
    }

    match n_vec
        .into_iter()
        .sorted()
        .collect_vec()
        .group_by(|a, b| a == b)
        .minmax_by_key(|&c| c.len())
    {
        MinMaxResult::MinMax(a, b) => b.len() - a.len(),
        MinMaxResult::NoElements => 0,
        MinMaxResult::OneElement(_) => 0,
    }
}
