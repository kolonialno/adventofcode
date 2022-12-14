use std::{
    cmp::{max, min},
    collections::HashMap,
    fs,
    ops::{Add, Sub},
    str::FromStr,
};

use helpers::get_input_file;
use itertools::Itertools;

#[derive(Copy, Clone, Hash, Default, Eq, PartialEq, Debug)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn path_coords(lhs: Position, rhs: Position) -> Vec<Position> {
        let x_range = min(lhs.x, rhs.x)..=max(lhs.x, rhs.x);
        let y_range = min(lhs.y, rhs.y)..=max(lhs.y, rhs.y);
        x_range
            .cartesian_product(y_range.into_iter())
            .map(|(x, y)| Position::new(x, y))
            .collect()
    }
}

impl FromStr for Position {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (lhs, rhs) = s.split_once(',').unwrap();
        Ok(Position {
            x: lhs.parse::<isize>().unwrap(),
            y: rhs.parse::<isize>().unwrap(),
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

const DIRECTIONS: [Position; 3] = [
    Position { x: 0, y: 1 },
    Position { x: -1, y: 1 },
    Position { x: 1, y: 1 },
];

fn trickle_sand(coords: &mut HashMap<Position, char>, max_y: isize) {
    const SAND_START: Position = Position { x: 500, y: 0 };
    let mut sand = SAND_START;
    'sand_loop: while !coords.contains_key(&sand) {
        for direction in DIRECTIONS {
            let new_pos = sand + direction;
            if new_pos.y > max_y {
                // falling into the vortex
                break 'sand_loop;
            }
            if !coords.contains_key(&new_pos) {
                sand = new_pos;

                continue 'sand_loop;
            }
        }

        coords.insert(sand, 'o');
        sand = SAND_START;
    }
}

fn trickle_sand_with_bottom(coords: &mut HashMap<Position, char>, max_y: isize) {
    const SAND_START: Position = Position { x: 500, y: 0 };
    let mut sand = SAND_START;
    'sand_loop: while !coords.contains_key(&sand) {
        for direction in DIRECTIONS {
            let new_pos = sand + direction;
            if new_pos.y > max_y + 2 {
                // falling into the vortex
                coords.insert(sand, 'o');
                sand = SAND_START;
                continue 'sand_loop;
            }
            if !coords.contains_key(&new_pos) {
                sand = new_pos;
                if sand.y == max_y + 1 {
                    coords.insert(sand, 'o');
                    sand = SAND_START;
                }

                continue 'sand_loop;
            }
        }

        coords.insert(sand, 'o');
        sand = SAND_START;
    }
}

fn stringify_cave(coords: &HashMap<Position, char>) -> String {
    let min_y = 0;
    let max_y = coords.iter().map(|p| p.0.y).max().unwrap();
    let min_x = coords.iter().map(|p| p.0.x).min().unwrap() - 2;
    let max_x = coords.iter().map(|p| p.0.x).max().unwrap() + 2;
    let mut cave = String::default();
    for y in min_y..=max_y + 1 {
        cave += format!(
            "{}\n",
            (min_x..=max_x)
                .map(|x| {
                    let pos = Position::new(x, y);
                    if coords.contains_key(&pos) {
                        coords[&pos]
                    } else if pos.y == max_y + 1 {
                        '#'
                    } else {
                        '.'
                    }
                })
                .collect::<String>()
        )
        .as_str();
    }
    cave
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    // let input = fs::read_to_string("14/test.txt").unwrap();
    let mut coords = std::iter::Iterator::flatten(input.lines().map(|line| {
        let coords = line
            .split(" -> ")
            .map(|coords| Position::from_str(coords).unwrap())
            .collect_vec();
        Iterator::flatten(
            coords
                .iter()
                .zip(coords.iter().skip(1))
                .map(|(lhs, rhs)| Position::path_coords(*lhs, *rhs)),
        )
        .collect_vec()
    }))
    .map(|x| (x, '#'))
    .collect::<HashMap<Position, char>>();

    let max_y = coords.iter().map(|p| p.0.y).max().unwrap();

    let nr_of_rocks = coords.len();
    trickle_sand(&mut coords, max_y);
    println!(
        "Settled sand spots without bottom: {}",
        coords.len() - nr_of_rocks
    );

    trickle_sand_with_bottom(&mut coords, max_y);
    println!(
        "Settled sand spots with bottom: {}",
        coords.len() - nr_of_rocks
    );

    fs::write("14/cave.txt", stringify_cave(&coords)).unwrap();
}
