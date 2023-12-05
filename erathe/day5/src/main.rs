use itertools::Itertools;
use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    ops::Range,
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

fn part1(input: &str) -> Option<usize> {
    let almanac = input.parse::<Almanac>().unwrap();
    Some(
        almanac
            .seeds
            .into_iter()
            .map(|seed| {
                almanac.steps.iter().fold(seed, |acc, step| {
                    if let Some(range) = step
                        .maps
                        .iter()
                        .find(|r| r.source <= acc && (r.source + r.len - 1) >= acc)
                    {
                        return range.dest + (acc - range.source);
                    }
                    acc
                })
            })
            .min()
            .unwrap(),
    )
}

fn part2(input: &str) -> Option<usize> {
    let almanac = input.parse::<Almanac>().unwrap();
    let mut seeds = HashSet::new();
    almanac.seeds.into_iter().tuples().for_each(|(start, len)| {
        for i in start..(start + len) {
            seeds.insert(i);
        }
    });
    Some(
        seeds
            .into_iter()
            .map(|seed| {
                almanac.steps.iter().fold(seed, |acc, step| {
                    if let Some(range) = step
                        .maps
                        .iter()
                        .find(|r| r.source <= acc && (r.source + r.len - 1) >= acc)
                    {
                        return range.dest + (acc - range.source);
                    }
                    acc
                })
            })
            .min()
            .unwrap(),
    )
}

#[derive(Debug)]
struct SoilMap {
    dest: usize,
    source: usize,
    len: usize,
}

#[derive(Debug)]
struct Step {
    maps: Vec<SoilMap>,
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<usize>,
    steps: Vec<Step>,
}

impl FromStr for Almanac {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut seeds = Vec::new();
        let mut steps = Vec::new();
        s.split("\n\n").enumerate().for_each(|(idx, l)| {
            if idx == 0 {
                for seed in RE.captures_iter(l) {
                    seeds.push(seed.get(0).unwrap().as_str().parse::<usize>().unwrap());
                }
            } else {
                let mut step = Step { maps: Vec::new() };
                l.lines().skip(1).for_each(|l| {
                    let nums = l.split(" ").collect_vec();
                    let dest = nums[0].parse::<usize>().unwrap();
                    let source = nums[1].parse::<usize>().unwrap();
                    let len = nums[2].parse::<usize>().unwrap();
                    step.maps.push(SoilMap { dest, source, len })
                });
                steps.push(step);
            }
        });
        Ok(Almanac { seeds, steps })
    }
}
