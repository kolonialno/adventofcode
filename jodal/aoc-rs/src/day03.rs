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
}

pub fn solve_a(map: Map) -> u8 {
    let delta = (3, 1);
    let mut pos = (0, 0);
    let mut num_trees = 0;

    loop {
        let feature = map.get(pos);
        match feature {
            Some(Feature::Tree) => num_trees += 1,
            Some(_) => {}
            None => break, // Outside map
        }
        pos = (pos.0 + delta.0, pos.1 + delta.1);
    }

    num_trees
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
}
