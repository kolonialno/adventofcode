use itertools::Itertools;
use regex::Regex;
use std::time::Instant;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(r#"\d+"#).unwrap();
}

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {:?} in {:?}", part1(input), t1.elapsed());
}

fn part1(input: &str) -> f64 {
    let k = input
        .lines()
        .map(|l| {
            RE.captures_iter(l)
                .map(|cap| cap.get(0).unwrap().as_str().parse::<f64>().unwrap())
                .collect_vec()
        })
        .collect_vec();
    let races = k[0].iter().zip(k[1].iter());
    races
        .map(|(time, distance)| {
            let discriminant = time.powf(2.0) - (4.0 * distance);
            if discriminant <= 0.0 {
                panic!("should not happen");
            }
            let x1 = (time + discriminant.sqrt()) / (2.0);
            let x2 = (time - discriminant.sqrt()) / (2.0);
            let (min_x, max_x) = if x1 < x2 { (x1, x2) } else { (x2, x1) };
            if !(((x1 as i64) as f64) < x1) {
                return (min_x + 1.0, max_x - 1.0);
            }
            (min_x.ceil(), max_x.floor())
        })
        .map(|(min, max)| (max - min) + 1.0)
        .product()
}
