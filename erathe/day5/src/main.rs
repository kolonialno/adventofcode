use itertools::{EitherOrBoth::*, Itertools};
use regex::Regex;
use std::{collections::HashMap, str::FromStr};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"^(\d+),(\d+) -> (\d+),(\d+)$"#).unwrap();
}

fn get_range(s: i32, e: i32) -> Vec<i32> {
    let mut r = (s.min(e)..=s.max(e)).collect_vec();
    if s > e {
        r.reverse();
    }
    r
}
fn main() {
    println!(
        "{}",
        include_str!("../input.txt")
            .lines()
            .map(|s| s.parse::<Line>().unwrap())
            // uncomment for part 1
            // .filter(|Line([[sx, sy], [ex, ey]])| sx == ex || sy == ey)
            .fold(HashMap::new(), |mut acc, Line([[sx, sy], [ex, ey]])| {
                for pair in get_range(sx, ex)
                    .into_iter()
                    .zip_longest(get_range(sy, ey).into_iter())
                    .map(|r| match r {
                        Both(l, r) => (l, r),
                        Left(l) => (l, sy),
                        Right(r) => (sx, r),
                    })
                {
                    *acc.entry([pair.0, pair.1]).or_insert(0) += 1;
                }
                acc
            })
            .values()
            .filter(|v| **v > 1)
            .count()
    );
}

#[derive(Debug)]
struct Line([[i32; 2]; 2]);

impl FromStr for Line {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let m = RE.captures(s).unwrap();
        Ok(Self([
            [m[1].parse().unwrap(), m[2].parse().unwrap()],
            [m[3].parse().unwrap(), m[4].parse().unwrap()],
        ]))
    }
}
