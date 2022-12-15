use itertools::Itertools;
use regex::Regex;
type Coordinate = (i64, i64);

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref RE: Regex = Regex::new(
        r#"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$"#
    )
    .unwrap();
}

const ROW_P1: i64 = 2000000;
const MAX: i64 = 4000000;

fn main() {
    let input = include_str!("../input.txt");

    println!("{:?}", part1(input));
    println!("{:?}", part2(input));
}

fn part2(input: &str) -> i64 {
    let sensors = input.lines().map(|l| parse_sensor(l)).collect_vec();

    for row in 0..=MAX {
        let mut ranges = vec![];
        for s in &sensors {
            let dist = s.pos.0.abs_diff(row) as i64;
            if dist > s.distance_to_b {
                continue;
            }
            let range = -s.distance_to_b + dist + s.pos.1..=s.distance_to_b - dist + s.pos.1;
            ranges.push(range);
        }
        for r in &ranges {
            let end = r.end();
            if end < &MAX && !ranges.iter().any(|ra| ra.contains(&(end + 1))) {
                return (end + 1) * MAX + row;
            }
        }
    }
    -1
}

fn part1(input: &str) -> usize {
    let sensors = input.lines().map(|l| parse_sensor(l)).collect_vec();

    let mut ranges = vec![];
    for s in &sensors {
        let dist = s.pos.0.abs_diff(ROW_P1) as i64;
        if dist > s.distance_to_b {
            continue;
        }
        let range = -s.distance_to_b + dist + s.pos.1..=s.distance_to_b - dist + s.pos.1;
        ranges.push(range);
    }
    (-MAX * 2..MAX * 2)
        .filter(|i| ranges.iter().any(|r| r.contains(i)))
        .count()
}

fn distance_to(c1: Coordinate, c2: Coordinate) -> i64 {
    (c1.0.abs_diff(c2.0) + c1.1.abs_diff(c2.1)) as i64
}

fn parse_sensor(input: &str) -> Sensor {
    let m = RE.captures(input).unwrap();
    let pos = (m[2].parse::<i64>().unwrap(), m[1].parse::<i64>().unwrap());
    let c_beacon = (m[4].parse::<i64>().unwrap(), m[3].parse::<i64>().unwrap());
    let distance_to_b = distance_to(pos, c_beacon);
    Sensor { pos, distance_to_b }
}

#[derive(Debug)]
struct Sensor {
    pos: Coordinate,
    distance_to_b: i64,
}
