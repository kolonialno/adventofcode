use rayon::prelude::*;
use std::{collections::HashSet, ops::RangeInclusive};

use regex::Regex;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
struct Coordinate {
    x: isize,
    y: isize,
}

impl Coordinate {
    fn distance(&self, other: &Coordinate) -> usize {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }

    fn tuning_frequency(&self) -> isize {
        self.x * 4000000 + self.y
    }
}

#[derive(Debug)]
struct Diamond {
    center: Coordinate,
    radius: usize,
}

impl Diamond {
    // Returns true if this diamond overlaps with another diamond
    fn overlaps(&self, other: &Diamond) -> bool {
        self.center.distance(&other.center) < self.radius + other.radius
    }
}

#[derive(Debug)]
struct Sensor {
    position: Coordinate,
    closest_beacon: Coordinate,
    distance: usize,
}

impl From<&str> for Sensor {
    fn from(input: &str) -> Self {
        let re = Regex::new(
            r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)",
        )
        .unwrap();

        let captures = re.captures(input).expect("Invalid input");
        let position = Coordinate {
            x: captures[1].parse().unwrap(),
            y: captures[2].parse().unwrap(),
        };
        let closest_beacon = Coordinate {
            x: captures[3].parse().unwrap(),
            y: captures[4].parse().unwrap(),
        };

        Self {
            position,
            closest_beacon,
            distance: position.distance(&closest_beacon),
        }
    }
}

impl Sensor {
    // Get the covered area of this sensor at the given line
    fn covered_area(&self, line: isize) -> Option<RangeInclusive<isize>> {
        if line.abs_diff(self.position.y) > self.distance {
            return None;
        }
        let x = self.position.x;
        let distance: isize = self.distance as isize - line.abs_diff(self.position.y) as isize;

        Some((x - distance)..=(x + distance))
    }

    fn covers(&self, coordinate: &Coordinate) -> bool {
        self.position.distance(&coordinate) <= self.distance
    }
}

fn parse_sensors(input: &str) -> Vec<Sensor> {
    input.lines().map(|line| line.into()).collect()
}

fn solve_part1(input: &str, line: isize) -> usize {
    let mut positions: HashSet<Coordinate> = HashSet::new();
    let sensors = parse_sensors(input);

    for sensor in &sensors {
        if let Some(range) = sensor.covered_area(line) {
            for x in range {
                positions.insert(Coordinate { x, y: line });
            }
        }
    }

    for sensor in &sensors {
        positions.remove(&sensor.position);
        positions.remove(&sensor.closest_beacon);
    }

    positions.len()
}

fn solve_part2(input: &str, max: isize) -> isize {
    let sensors = parse_sensors(input);
    let x_range: Vec<isize> = (0..=max).into_iter().collect();

    for line in 0..=max {
        if line % 1000 == 0 {
            println!("Searching line={line}");
        }

        // Calculate ranges covered
        let ranges: Vec<RangeInclusive<isize>> = sensors
            .iter()
            .filter_map(|sensor| sensor.covered_area(line))
            .collect();

        let x = x_range.par_iter().find_any(|x| {
            if !ranges.iter().any(|range| range.contains(&x)) {
                true
            } else {
                false
            }
        });

        if let Some(x) = x {
            return Coordinate {
                x: *x as isize,
                y: line,
            }
            .tuning_frequency();
        }
    }

    panic!("No possible distress beacon positions")
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input, 2000000);
    println!("Part 1: {sum}");
    let sum = solve_part2(input, 4000000);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coordinate() {
        let a = Coordinate { x: 0, y: 0 };
        let b = Coordinate { x: 6, y: 6 };

        assert_eq!(a.distance(&b), 12);

        let a = Coordinate { x: 14, y: 11 };
        assert_eq!(a.tuning_frequency(), 56000011);
    }

    #[test]
    fn test_diamond_overlap() {
        let a = Diamond {
            center: Coordinate { x: 5, y: 5 },
            radius: 3,
        };
        let b = Diamond {
            center: Coordinate { x: 2, y: 3 },
            radius: 2,
        };
        let c = Diamond {
            center: Coordinate { x: 3, y: 8 },
            radius: 3,
        };

        assert!(!a.overlaps(&b));
        assert!(!b.overlaps(&a));
        assert!(a.overlaps(&c));
        assert!(c.overlaps(&a));
        assert!(!b.overlaps(&c));
        assert!(!c.overlaps(&b));
    }

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input, 10), 26, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input, 20), 56000011, "Wrong result for pt. 2");
    }
}
