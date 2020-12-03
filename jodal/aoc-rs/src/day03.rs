#[derive(Debug)]
pub struct Map {
    rows: Vec<Vec<Feature>>,
}

#[derive(Debug)]
pub enum Feature {
    Open,
    Tree,
}

impl Map {
    pub fn from_lines(lines: Vec<String>) -> Map {
        Map {
            rows: lines
                .iter()
                .filter(|&l| l.len() > 0)
                .map(|l| {
                    l.chars()
                        .map(|c| match c {
                            '.' => Feature::Open,
                            '#' => Feature::Tree,
                            _ => panic!("Unknown feature"),
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

pub fn solve_a(map: Map) -> u32 {
    map.traverse((3, 1))
}

pub fn solve_b(map: Map) -> u32 {
    map.traverse((1, 1))
        * map.traverse((3, 1))
        * map.traverse((5, 1))
        * map.traverse((7, 1))
        * map.traverse((1, 2))
}

#[cfg(test)]
mod tests {
    #[test]
    fn example_a() {
        let map = super::Map::from_lines(
            r###"
..##.......
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
"###
            .to_string()
            .lines()
            .map(|s| s.to_string())
            .collect(),
        );
        let answer = super::solve_a(map);
        assert_eq!(answer, 7);
    }

    #[test]
    fn example_b() {
        let map = super::Map::from_lines(
            r###"
..##.......
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
"###
            .to_string()
            .lines()
            .map(|s| s.to_string())
            .collect(),
        );
        let answer = super::solve_b(map);
        assert_eq!(answer, 336);
    }
}