use std::time::Instant;

use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {} in {:?}", part1(input), t1.elapsed());
    let t2 = Instant::now();
    println!("part 2: {} in {:?}", part2(input), t2.elapsed()); // ~80µs
    let t3 = Instant::now();
    println!(
        "part 2 without heap allocations: {} in {:?}",
        part2_alt(input),
        t3.elapsed()
    ); // ~50µs
}

fn part1(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|s| {
            s.split('\n')
                .fold(0, |acc, x| acc + x.parse::<u32>().unwrap())
        })
        .max()
        .unwrap()
}

fn part2(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|s| {
            s.split('\n')
                .fold(0, |acc, x| acc + x.parse::<u32>().unwrap())
        })
        .sorted()
        .rev()
        .take(3)
        .sum()
}

fn part2_alt(input: &str) -> u32 {
    let mut largest: [u32; 3] = [0, 0, 0];
    input.split("\n\n").for_each(|s| {
        let cals = s
            .split('\n')
            .fold(0, |acc, x| acc + x.parse::<u32>().unwrap());
        for i in 0..3 {
            if largest[i] < cals {
                largest[i] = cals;
                largest.sort();
                break;
            }
        }
    });
    largest[0] + largest[1] + largest[2]
}
