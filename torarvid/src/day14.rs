use std::collections::HashMap;

use crate::util::file_by_lines;

fn recursive(
    first: char,
    second: char,
    map: &HashMap<(char, char), char>,
    i: usize,
    cache: &mut HashMap<(char, char, usize), HashMap<char, i64>>,
) -> HashMap<char, i64> {
    if i == 0 {
        return HashMap::new();
    }
    if let Some(cached) = cache.get(&(first, second, i)) {
        return cached.clone();
    }
    let mapping = map.get(&(first, second)).unwrap();
    let mut counts = recursive(first, *mapping, map, i - 1, cache);
    recursive(*mapping, second, map, i - 1, cache)
        .into_iter()
        .for_each(|(k, v)| {
            *counts.entry(k).or_insert(0) += v;
        });
    *counts.entry(*mapping).or_insert(0) += 1;
    cache.insert((first, second, i), counts.clone());
    counts
}

fn solve(template: &Vec<char>, map: &HashMap<(char, char), char>, iterations: usize) -> i64 {
    let mut countz = HashMap::new();
    template.iter().for_each(|c| {
        *countz.entry(*c).or_insert(0) += 1;
    });
    template.windows(2).for_each(|w| {
        recursive(w[0], w[1], &map, iterations, &mut HashMap::new())
            .into_iter()
            .for_each(|(k, v)| {
                *countz.entry(k).or_insert(0) += v;
            });
    });
    countz.values().max().unwrap() - countz.values().min().unwrap()
}

pub fn run() {
    let lines = file_by_lines("day14.txt");

    let mut map = HashMap::new();
    let mut line_iter = lines.iter();
    let template = line_iter.next().unwrap().chars().collect::<Vec<char>>();
    line_iter.next(); // skip empty line
    for line in line_iter {
        let mut sections = line.split(" -> ");
        let mut from_chars = sections.next().unwrap().chars();
        let from = (from_chars.next().unwrap(), from_chars.next().unwrap());
        let to = sections.next().unwrap().chars().next().unwrap();
        map.insert(from, to);
    }

    println!("Part 1: {}", solve(&template, &map, 10));
    println!("Part 2: {}", solve(&template, &map, 40));
}
