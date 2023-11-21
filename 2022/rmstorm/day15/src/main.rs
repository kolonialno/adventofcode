use std::{fmt, ops::RangeInclusive};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, map_res, opt, recognize},
    sequence::{preceded, separated_pair},
    IResult,
};

fn get_x_or_y(i: &str) -> IResult<&str, i32> {
    preceded(
        alt((tag("x="), tag("y="))),
        map_res(recognize(preceded(opt(tag("-")), digit1)), str::parse),
    )(i)
}

fn parse(i: &str) -> IResult<&str, Pair> {
    let get_xy = |i| separated_pair(get_x_or_y, tag(", "), get_x_or_y)(i);
    map(
        separated_pair(
            preceded(tag("Sensor at "), get_xy),
            tag(": closest beacon is at "),
            get_xy,
        ),
        |e| e.into(),
    )(i)
}

struct Pair {
    sensor: [i32; 2],
    beacon: [i32; 2],
}

impl From<((i32, i32), (i32, i32))> for Pair {
    fn from(p: ((i32, i32), (i32, i32))) -> Self {
        Pair {
            sensor: [p.0 .0, p.0 .1],
            beacon: [p.1 .0, p.1 .1],
        }
    }
}

impl fmt::Debug for Pair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "s:{:?}, b:{:?}", self.sensor, self.beacon)
    }
}

impl Pair {
    fn radius(&self) -> i32 {
        (self.sensor[0] - self.beacon[0]).abs() + (self.sensor[1] - self.beacon[1]).abs()
    }
    fn covered(&self, pos: [i32; 2]) -> bool {
        ((self.sensor[0] - pos[0]).abs() + (self.sensor[1] - pos[1]).abs()) <= self.radius()
    }
    fn row_range(&self, row: i32) -> Option<RangeInclusive<i32>> {
        let diff = self.radius() - (row - self.sensor[1]).abs();
        if diff > 0 {
            Some((self.sensor[0] - diff)..=(self.sensor[0] + diff))
        } else {
            None
        }
    }
}

fn find_covering_pair(pairs: &Vec<Pair>, pos: [i32; 2]) -> Option<&Pair> {
    for p in pairs {
        if p.covered(pos) {
            return Some(p);
        }
    }
    None
}

fn main() {
    let input = include_str!("input.txt");
    let pairs: Vec<Pair> = input.lines().map(|l| parse(l).unwrap().1).collect();

    for row in 0..=4000000 {
        let mut cur_pair = find_covering_pair(&pairs, [0, row]).unwrap();
        let mut cur_left = cur_pair.row_range(row).unwrap().last().unwrap() + 1;

        while cur_left < 4000000 {
            match find_covering_pair(&pairs, [cur_left, row]) {
                Some(cp) => cur_pair = cp,
                None => panic!("{:?}", 4000000 * cur_left as u128 + row as u128),
            };
            cur_left = cur_pair.row_range(row).unwrap().last().unwrap() + 1;
        }
    }
}
