use std::{
    cmp::{max, min},
    collections::HashSet,
    fs,
    ops::{Add, Range, Sub},
    str::FromStr,
};

use helpers::get_input_file;
use itertools::Itertools;
const UPPER_LIMIT: isize = 4000000;
const LOWER_LIMIT: isize = 0;

#[derive(Copy, Clone, Hash, Default, Eq, PartialEq, Debug)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn manhattan_distance(&self, other: &Self) -> usize {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }

    fn covered_area_line(&self, beacon: Position, line: isize) -> Vec<Position> {
        let mut coords = Vec::default();
        let distance = self.manhattan_distance(&beacon);
        let y_distance = line - self.y;
        if y_distance.abs() > distance as isize {
            return coords;
        }
        let x_diff = distance as isize - y_distance.abs() as isize;

        for x in self.x - x_diff..=self.x + x_diff {
            let new_pos = Position::new(x as isize, line);
            if new_pos != beacon {
                coords.push(new_pos);
            }
        }
        coords
    }

    fn covered_area_ranges(&self, beacon: Position) -> Vec<(Range<isize>, isize)> {
        let mut coords = Vec::default();
        let distance = self.manhattan_distance(&beacon);

        for y in 0..=UPPER_LIMIT {
            if y.abs_diff(self.y) > distance {
                continue;
            }
            let x_diff = distance.abs_diff((self.y - y).unsigned_abs()) as isize;
            coords.push((
                max(self.x - x_diff, LOWER_LIMIT)..min(self.x + x_diff, UPPER_LIMIT) + 1,
                y,
            ));
        }
        coords
    }

    fn tuning_frequency(&self) -> isize {
        self.x * 4000000 + self.y
    }
}

impl FromStr for Position {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (lhs, rhs) = s.split_once(", ").unwrap();
        Ok(Position {
            x: lhs.split_once('=').unwrap().1.parse::<isize>().unwrap(),
            y: rhs.split_once('=').unwrap().1.parse::<isize>().unwrap(),
        })
    }
}

impl Sub for Position {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Position {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

fn main() {
    // const INTERESTING_LINE: isize = 10;
    // let input = fs::read_to_string("15/test.txt").unwrap();
    const INTERESTING_LINE: isize = 2000000;
    let input = fs::read_to_string(get_input_file()).unwrap();
    let mut covered_coords: HashSet<Position> = HashSet::default();
    let mut covered_areas: Vec<(Range<isize>, isize)> = Vec::default();
    input
        .split('\n')
        .filter(|s| !s.is_empty())
        .map(|line| {
            let (sensor_string, beacon_string) = line.split_once(':').unwrap();
            let sensor =
                Position::from_str(sensor_string.chars().skip(10).collect::<String>().as_str())
                    .unwrap();
            let beacon =
                Position::from_str(beacon_string.chars().skip(22).collect::<String>().as_str())
                    .unwrap();
            covered_coords.extend(
                sensor
                    .covered_area_line(beacon, INTERESTING_LINE)
                    .into_iter(),
            );
            covered_areas.extend(sensor.covered_area_ranges(beacon).into_iter());
        })
        .count();
    println!(
        "Max covered spots in line {}: {}",
        INTERESTING_LINE,
        covered_coords.len()
    );
    covered_areas.sort_by_key(|(_, y)| *y);
    for (y, group) in covered_areas.into_iter().group_by(|x| x.1).into_iter() {
        let mut start = LOWER_LIMIT;
        for r1 in group.sorted_by_key(|x| x.0.start) {
            if r1.0.contains(&start) || r1.0.start < start {
                start = max(start, r1.0.end);
            }
        }
        if start <= UPPER_LIMIT {
            let beacon = Position::new(start, y);
            println!(
                "Beacon at {:?} with frequency: {}",
                beacon,
                beacon.tuning_frequency()
            );
        }
    }
}
