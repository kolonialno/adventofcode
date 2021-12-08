use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};
use std::iter::Map;
use std::ops::Range;
use std::str::FromStr;

#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq)]
struct Coord {
    x: u32,
    y: u32,
}

impl FromStr for Coord {
    type Err = Error;

    fn from_str(coord: &str) -> Result<Self, Self::Err> {
        let coord: Vec<u32> = coord.split(",").map(|i| i.parse().unwrap()).collect();
        Ok(Coord {
            x: coord[0],
            y: coord[1],
        })
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct VentLine {
    start: Coord,
    end: Coord,
}

impl VentLine {
    fn straight(&self) -> bool {
        if self.start.x == self.end.x || self.start.y == self.end.y {
            return true;
        }
        false
    }
}

impl IntoIterator for VentLine {
    type Item = Coord;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let coord_iter: Map<std::ops::Range<u32>, Box<dyn Fn(u32) -> Coord>> = if self.straight() {
            if self.start.x == self.end.x {
                if self.end.y > self.start.y {
                    (self.start.y..self.end.y + 1).map(Box::new(|i| Coord {
                        x: self.start.x,
                        y: i,
                    }))
                } else {
                    (self.end.y..self.start.y + 1).map(Box::new(|y| Coord {
                        x: self.start.x,
                        y: y,
                    }))
                }
            } else {
                if self.end.x > self.start.x {
                    (self.start.x..self.end.x + 1).map(Box::new(|x| Coord {
                        x: x,
                        y: self.start.y,
                    }))
                } else {
                    (self.end.x..self.start.x + 1).map(Box::new(|x| Coord {
                        x: x,
                        y: self.start.y,
                    }))
                }
            }
        } else {
            if self.end.x > self.start.x {
                if self.end.y > self.start.y {
                    (0..self.end.x - self.start.x + 1).map(Box::new(|i| Coord {
                        x: self.start.x + i,
                        y: self.start.y + i,
                    }))
                } else {
                    (0..self.end.x - self.start.x + 1).map(Box::new(|i| Coord {
                        x: self.start.x + i,
                        y: self.start.y - i,
                    }))
                }
            } else {
                if self.end.y > self.start.y {
                    (0..self.start.x - self.end.x + 1).map(Box::new(|i| Coord {
                        x: self.start.x - i,
                        y: self.start.y + i,
                    }))
                } else {
                    (0..self.start.x - self.end.x + 1).map(Box::new(|i| Coord {
                        x: self.start.x - i,
                        y: self.start.y - i,
                    }))
                }
            }
        };
        coord_iter.collect::<Vec<Coord>>().into_iter()
    }
}

impl FromStr for VentLine {
    type Err = Error;

    fn from_str(vent_line: &str) -> Result<Self, Self::Err> {
        let mut vent_iter = vent_line.split(" -> ").map(|i| i.parse().unwrap());
        Ok(VentLine {
            start: vent_iter.next().unwrap(),
            end: vent_iter.next().unwrap(),
        })
    }
}

fn read<R: Read>(io: R) -> Result<Vec<VentLine>, Error> {
    Ok(BufReader::new(io)
        .lines()
        .map(|line| line.unwrap().trim().parse().unwrap())
        .collect())
}

pub fn day05() {
    let vent_lines = read(File::open("inputs/day05.txt").unwrap()).unwrap();

    let mut danger_zones = HashMap::new();

    for vent_line in vent_lines.iter() {
        for c in vent_line.into_iter() {
            let counter = danger_zones.entry(c).or_insert(0);
            *counter += 1;
        }
    }
    let mut total_danger_zones = 0;
    for (key, val) in danger_zones.iter() {
        if val >= &2 {
            total_danger_zones += 1;
        }
    }
    println!("total_danger_zones = {:#?}", total_danger_zones);
}
