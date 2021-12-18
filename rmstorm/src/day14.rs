use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<(HashMap<[char; 2], char>, Vec<char>), Error> {
    let mut br = BufReader::new(io);

    // First line contains starting polymer
    let mut first_line = String::new();
    br.read_line(&mut first_line).unwrap();
    let first_line: Vec<char> = first_line.trim().chars().collect();

    // Read the rest of the lines
    let mut rules = HashMap::new();
    for line in br.lines() {
        let line: Vec<String> = line?
            .trim()
            .split(" -> ")
            .filter(|&i| i.len() != 0)
            .map(|i| String::from(i))
            .collect();
        if line.len() != 0 {
            rules.insert(
                line[0].chars().collect::<Vec<char>>().try_into().unwrap(),
                line[1].chars().next().unwrap(),
            );
        }
    }
    Ok((rules, first_line))
}

pub fn day14() {
    let (rules, polymer) = read(File::open("inputs/day14.txt").unwrap()).unwrap();
    println!("{:?}", polymer);
    println!("{:?}", rules);

    let mut pair_counts: HashMap<[char; 2], u64> = HashMap::new();
    for i in 1..polymer.len() {
        pair_counts
            .entry(polymer[i - 1..i + 1].try_into().unwrap())
            .and_modify(|e| *e += 1)
            .or_insert(1);
    }

    let mut counts: HashMap<char, u64> = HashMap::new();
    for x in polymer {
        *counts.entry(x).or_default() += 1;
    }

    for _ in 0..40 {
        let nc = HashMap::new();
        pair_counts =
            pair_counts
                .iter()
                .fold(nc, |mut acc: HashMap<[char; 2], u64>, (key, value)| {
                    if value > &0 {
                        *counts.entry(rules[key]).or_default() += value;
                        acc.entry([rules[key], key[1]])
                            .and_modify(|e| *e += value)
                            .or_insert(*value);
                        acc.entry([key[0], rules[key]])
                            .and_modify(|e| *e += value)
                            .or_insert(*value);
                    }
                    acc
                });
    }
    let max = counts.iter().max_by_key(|(_, v)| *v).map(|(k, _)| k);
    let min = counts.iter().min_by_key(|(_, v)| *v).map(|(k, _)| k);
    println!("{:?}", counts[max.unwrap()] - counts[min.unwrap()]);
}
