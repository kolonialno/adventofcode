use std::{collections::HashSet, io::Error, str::FromStr};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Tree {
    height: u32,
    coordinate: Coordinate,
}

impl PartialOrd for Tree {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Tree {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.height.cmp(&other.height)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coordinate(u32, u32);

#[derive(Debug, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Grid {
    rows: Vec<Vec<Tree>>,
}

impl Grid {
    fn rows(&self) -> &Vec<Vec<Tree>> {
        &self.rows
    }

    fn cols(&self) -> Vec<Vec<Tree>> {
        transpose(self.rows.clone())
    }

    pub fn max_scenic_value(&self) -> u32 {
        self.rows
            .iter()
            .map(|row| {
                row.iter()
                    .map(|t| self.scenic_value(&t.coordinate))
                    .max()
                    .unwrap()
            })
            .max()
            .unwrap()
    }

    pub fn scenic_value(&self, coordinate: &Coordinate) -> u32 {
        let directions = [
            Direction::Down,
            Direction::Left,
            Direction::Right,
            Direction::Up,
        ];

        directions
            .iter()
            .map(|d| self.scenic_value_in_direction(coordinate, d))
            .product()
    }

    fn scenic_value_in_direction(&self, coordinate: &Coordinate, direction: &Direction) -> u32 {
        let this_tree = &self.rows[coordinate.1 as usize][coordinate.0 as usize];

        let trees: Vec<_> = self.iter_in_direction(coordinate, direction).collect();
        let num_trees = trees.len();

        let view = trees.iter().enumerate().find_map(|(i, t)| {
            if t.height >= this_tree.height {
                Some(i)
            } else {
                None
            }
        });
        if let Some(next_tree) = view {
            (next_tree as u32) + 1
        } else {
            num_trees as u32
        }
    }

    fn iter_in_direction<'a>(
        &'a self,
        coordinate: &'a Coordinate,
        direction: &'a Direction,
    ) -> Box<dyn Iterator<Item = &Tree> + '_> {
        match direction {
            Direction::Right => {
                Box::new(self.rows[coordinate.1 as usize][coordinate.0 as usize + 1..].iter())
            }
            Direction::Left => Box::new(
                self.rows[coordinate.1 as usize][0..coordinate.0 as usize]
                    .iter()
                    .rev(),
            ),
            Direction::Up => Box::new(
                self.rows[0..coordinate.1 as usize]
                    .iter()
                    .rev()
                    .map(|row| &row[coordinate.0 as usize]),
            ),
            Direction::Down => Box::new(
                self.rows[coordinate.1 as usize + 1..]
                    .iter()
                    .map(|row| &row[coordinate.0 as usize]),
            ),
        }
    }
}

impl FromStr for Grid {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Grid {
            rows: s
                .lines()
                .enumerate()
                .map(|(y, l)| {
                    l.chars()
                        .enumerate()
                        .map(|(x, c)| Tree {
                            height: c.to_digit(10).unwrap(),
                            coordinate: Coordinate(x as u32, y as u32),
                        })
                        .collect()
                })
                .collect(),
        })
    }
}

fn run_part_one(s: &str) -> usize {
    let grid: Grid = s.parse().unwrap();

    let mut visible: HashSet<&Tree> = HashSet::new();

    for row in grid.rows() {
        let from_right = ascending(row.iter());
        let from_left = ascending(row.iter().rev());

        visible.extend(from_left);
        visible.extend(from_right);
    }

    let cols = grid.cols();
    for col in cols.iter() {
        let from_top = ascending(col.iter());
        let from_bottom = ascending(col.iter().rev());

        visible.extend(from_top);
        visible.extend(from_bottom);
    }

    visible.len()
}

fn run_part_two(s: &str) -> u32 {
    let grid: Grid = s.parse().unwrap();
    grid.max_scenic_value()
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

fn ascending<T: Ord + Copy>(it: impl Iterator<Item = T>) -> Vec<T> {
    let mut values = Vec::new();
    for val in it {
        let max = values.iter().max();
        if max.is_none() || Some(&val) > max {
            values.push(val);
        }
    }
    values
}

fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());

    let height = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();

    (0..height)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 21);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 8);
    }
}
