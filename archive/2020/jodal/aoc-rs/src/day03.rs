use std::convert::TryFrom;
use std::convert::TryInto;
use std::str::Lines;

pub fn solve_a(map: &Map) -> u32 {
    map.traverse((3, 1))
}

pub fn solve_b(map: &Map) -> u32 {
    map.traverse((1, 1))
        * map.traverse((3, 1))
        * map.traverse((5, 1))
        * map.traverse((7, 1))
        * map.traverse((1, 2))
}

#[derive(Debug)]
pub struct Map {
    rows: Vec<Vec<Feature>>,
}

#[derive(Debug)]
pub enum Feature {
    Open,
    Tree,
}

impl TryFrom<char> for Feature {
    type Error = char;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '.' => Ok(Feature::Open),
            '#' => Ok(Feature::Tree),
            _ => Err(c),
        }
    }
}

impl Map {
    pub fn from_lines(lines: Lines) -> Map {
        Map {
            rows: lines
                .filter(|&l| l.len() > 0)
                .map(|l| {
                    l.chars()
                        .map(|c| match c.try_into() {
                            Ok(feature) => feature,
                            Err(chr) => panic!("Unknown feature: {}", chr),
                        })
                        .collect()
                })
                .collect(),
        }
    }

    pub fn get(&self, (x, y): (usize, usize)) -> Option<&Feature> {
        if let Some(row) = self.rows.get(y) {
            return row.get(x % row.len());
        }
        None
    }

    pub fn traverse(&self, slope: (usize, usize)) -> u32 {
        let mut pos = (0, 0);
        let mut num_trees = 0;

        loop {
            let feature = self.get(pos);
            match feature {
                Some(Feature::Tree) => num_trees += 1,
                Some(_) => {}
                None => break, // Outside map
            }
            pos = (pos.0 + slope.0, pos.1 + slope.1);
        }

        num_trees
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            r###"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"###,
        );
        let map = Map::from_lines(input.lines());
        let answer = solve_a(&map);
        assert_eq!(answer, 7);
    }

    #[test]
    fn example_b() {
        let input = String::from(
            r###"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"###,
        );
        let map = Map::from_lines(input.lines());
        let answer = solve_b(&map);
        assert_eq!(answer, 336);
    }
}
