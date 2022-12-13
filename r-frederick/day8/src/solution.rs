use itertools::Itertools;
use std::collections::HashMap;
use std::ops::{Add, AddAssign};

type Height = isize;

#[derive(Hash, Eq, Clone, Debug)]
struct Point(isize, isize);

impl PartialEq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.0 = self.0 + rhs.0;
        self.1 = self.1 + rhs.1;
    }
}

struct Trees {
    grid: HashMap<Point, Height>,
    width: isize,
    length: isize,
}

impl Trees {
    fn from_lines(lines: &Vec<&str>) -> Self {
        let length: isize = lines.len() as isize;
        let width: isize = lines[0].len() as isize;

        let mut grid: HashMap<Point, Height> = HashMap::new();

        for (line, y) in lines.iter().zip(0isize..) {
            for (height, x) in line.split("").filter(|x| !x.is_empty()).zip(0isize..) {
                grid.insert(Point(x, y), height.parse::<isize>().unwrap());
            }
        }

        Trees {
            grid,
            width,
            length,
        }
    }

    fn contains_point(&self, point: &Point) -> bool {
        point.0 >= 0 && point.0 < self.width && point.1 >= 0 && point.1 < self.length
    }

    fn visible(&self) -> isize {
        (self.width * 2) + (self.length * 2) - 4
            + (1isize..self.width - 1)
                .cartesian_product(1isize..self.length - 1)
                .filter(|p| self.is_visible(Point(p.1, p.0)))
                .count() as isize
    }

    fn is_visible(&self, point: Point) -> bool {
        let deltas: [Point; 4] = [Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1)];
        let evaluated_height = self.grid.get(&point).unwrap();

        for delta in deltas {
            let mut current = point.clone();

            loop {
                current += delta.clone();

                if !self.contains_point(&current) {
                    return true;
                }

                match self.grid.get(&current) {
                    Some(height) => {
                        if height >= evaluated_height {
                            break;
                        }
                    }
                    None => return true,
                }
            }
        }

        false
    }

    fn highest_scenic_score(&self) -> usize {
        (1isize..self.width - 1)
            .cartesian_product(1isize..self.length - 1)
            .fold(0, |acc, p| {
                std::cmp::max(acc, self.scenic_score(Point(p.1, p.0)))
            })
    }

    fn scenic_score(&self, point: Point) -> usize {
        let deltas: [Point; 4] = [Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1)];
        let evaluated_height = self.grid.get(&point).unwrap();
        let mut score = 1;

        for delta in deltas {
            let mut current = point.clone();
            let mut local_score = 0;

            loop {
                current += delta.clone();

                if !self.contains_point(&current) {
                    score *= local_score;
                    break;
                }

                match self.grid.get(&current) {
                    Some(height) => {
                        local_score += 1;
                        if height >= evaluated_height {
                            score *= local_score;
                            break;
                        }
                    }
                    None => break,
                }
            }
        }

        score
    }
}

pub fn solve_part1(input: &Vec<&str>) -> isize {
    let trees = Trees::from_lines(input);

    trees.visible()
}

pub fn solve_part2(input: &Vec<&str>) -> usize {
    let trees = Trees::from_lines(input);

    trees.highest_scenic_score()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let lines = vec!["30373", "25512", "65332", "33549", "35390"];

        let expected = 21;

        assert_eq!(solve_part1(&lines), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let lines = vec!["30373", "25512", "65332", "33549", "35390"];

        let expected = 8;

        assert_eq!(solve_part2(&lines), expected);
    }
}
