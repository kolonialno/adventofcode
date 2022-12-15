use std::{
    collections::BTreeSet,
    ops::{Add, Range},
};

use nom::{
    bytes::complete::tag,
    character::complete::newline,
    multi::separated_list0,
    sequence::{preceded, separated_pair},
    IResult, Parser,
};
use rayon::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
struct Pos(i32, i32);

impl Pos {
    fn distance_to(&self, other: &Pos) -> u32 {
        self.0.abs_diff(other.0) + self.1.abs_diff(other.1)
    }
}

impl Add for Pos {
    type Output = Pos;

    fn add(self, rhs: Self) -> Self::Output {
        Pos(self.0 + rhs.0, self.1 + rhs.1)
    }
}

#[derive(Debug)]
struct SignalLock {
    sensor: Pos,
    beacon: Pos,
    radius: u32,
}

impl SignalLock {
    fn new(sensor: Pos, beacon: Pos) -> Self {
        Self {
            sensor,
            beacon,
            radius: sensor.distance_to(&beacon),
        }
    }

    fn width_at(&self, y: i32) -> u32 {
        let relative_y_position = (y - self.sensor.1).unsigned_abs();

        if relative_y_position > self.radius {
            0
        } else {
            (self.radius - relative_y_position) * 2
        }
    }

    fn coverage_at(&self, y: i32) -> Option<Range<i32>> {
        let width = self.width_at(y) as i32;
        if width == 0 {
            None
        } else {
            Some((self.sensor.0 - width / 2)..(self.sensor.0 + width / 2 + 1))
        }
    }
}

fn run_part_one(s: &str, y: i32) -> usize {
    let (_, signal_locks) = parse_signal_locks(s).unwrap();

    let mut ranges = signal_locks
        .iter()
        .filter_map(move |sl| sl.coverage_at(y))
        .collect::<Vec<_>>();
    ranges.sort_by(|r1, r2| r1.start.cmp(&r2.start));

    let covered_xs = ranges.into_iter().flatten().collect::<BTreeSet<_>>();
    let beacon_xs = signal_locks
        .into_iter()
        .filter_map(|sl| if sl.beacon.1 == y { Some(y) } else { None })
        .collect::<BTreeSet<_>>();

    covered_xs.difference(&beacon_xs).count()
}

fn run_part_two(s: &str, pos_max: i32) -> i64 {
    let (_, signal_locks) = parse_signal_locks(s).unwrap();

    let (x, y) = (0..=pos_max)
        .par_bridge()
        .find_map_any(|y| {
            let mut ranges = signal_locks
                .iter()
                .filter_map(move |sl| sl.coverage_at(y))
                .collect::<Vec<_>>();
            ranges.sort_by(|r1, r2| r1.start.cmp(&r2.start));

            let first_range = ranges.remove(0);
            let uncovered_x = ranges
                .into_iter()
                .try_fold(first_range, |r1, r2| {
                    if r1.end >= r2.end {
                        return Ok(r1);
                    }

                    if r1.contains(&(r2.start - 1)) {
                        return Ok(r1.start..r2.end);
                    }

                    Err(r1.end)
                })
                // Cover edge cases at beginning and end of lines
                .and_then(|r| if r.start > 0 { Err(0) } else { Ok(r) })
                .and_then(|r| if r.end < pos_max { Err(pos_max) } else { Ok(r) })
                .err();

            uncovered_x.map(|x| (x, y))
        })
        .unwrap();

    x as i64 * 4_000_000 + y as i64
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input, 2_000_000);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input, 4_000_000);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE, 10), 26);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE, 20), 56000011);
    }
}

fn pos(input: &str) -> IResult<&str, Pos> {
    separated_pair(
        preceded(tag("x="), nom::character::complete::i32),
        tag(", "),
        preceded(tag("y="), nom::character::complete::i32),
    )
    .map(|(x, y)| Pos(x, y))
    .parse(input)
}

fn parse_signal_lock(input: &str) -> IResult<&str, SignalLock> {
    separated_pair(
        preceded(tag("Sensor at "), pos),
        tag(": "),
        preceded(tag("closest beacon is at "), pos),
    )
    .map(|(sensor, beacon)| SignalLock::new(sensor, beacon))
    .parse(input)
}

fn parse_signal_locks(input: &str) -> IResult<&str, Vec<SignalLock>> {
    separated_list0(newline, parse_signal_lock)(input)
}
