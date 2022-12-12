use std::{collections::HashMap, ops::Index, str::FromStr};

use pathfinding::prelude::astar;

#[derive(Debug)]
struct Tile(u8);

impl Tile {
    fn can_easily_access(&self, other: &Tile) -> bool {
        if self.0 < other.0 {
            self.0.abs_diff(other.0) <= 1
        } else {
            true
        }
    }
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        if c == 'S' {
            return Tile::from('a');
        }
        if c == 'E' {
            return Tile::from('z');
        }

        if !c.is_ascii_lowercase() {
            unreachable!();
        }

        Tile((c as u8) - 97)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
struct Pos(usize, usize);

impl Pos {
    fn distance_to(&self, other: &Pos) -> usize {
        self.0.abs_diff(other.0) + self.1.abs_diff(other.1)
    }
}

#[derive(Debug)]
struct HeightMap {
    tiles: Vec<Vec<Tile>>,
    height: usize,
    width: usize,

    start: Pos,
    end: Pos,

    path_length_cache: HashMap<Pos, usize>,
}

impl Index<&Pos> for HeightMap {
    type Output = Tile;

    fn index(&self, index: &Pos) -> &Self::Output {
        &self.tiles[index.1][index.0]
    }
}

impl FromStr for HeightMap {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tiles: Vec<Vec<Tile>> = s
            .lines()
            .map(|line| line.chars().map(Tile::from).collect())
            .collect();

        let height = s.lines().count();
        let width = s.lines().next().unwrap().len();

        // Create a string copy, without whitespace, to find Pos for start and
        // end.
        let mut s_without_whitespace = s.to_owned();
        s_without_whitespace.retain(|c| !c.is_whitespace());

        let start_idx = s_without_whitespace.find('S').unwrap();
        let start = Pos(start_idx % width, start_idx / width);

        let end_idx = s_without_whitespace.find('E').unwrap();
        let end = Pos(end_idx % width, end_idx / width);

        Ok(Self {
            tiles,
            height,
            width,
            start,
            end,
            path_length_cache: HashMap::new(),
        })
    }
}

impl HeightMap {
    fn accessible_neighbors(&self, pos: &Pos) -> Vec<Pos> {
        let current_tile = &self[pos];

        self.neighbors(pos)
            .into_iter()
            .filter(|p| current_tile.can_easily_access(&self[p]))
            .collect()
    }

    fn neighbors(&self, pos: &Pos) -> Vec<Pos> {
        let x = pos.0 as i32;
        let y = pos.1 as i32;

        vec![(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
            .into_iter()
            .filter(|&(x, y)| x >= 0 && x < self.width as i32 && y >= 0 && y < self.height as i32)
            .map(|(x, y)| Pos(x as usize, y as usize))
            .collect()
    }

    fn shortest_path_length(&mut self, start: &Pos) -> Option<usize> {
        if self.path_length_cache.contains_key(start) {
            return self.path_length_cache.get(start).copied();
        }

        let (_, cost) = self.traverse(start, false)?;
        Some(cost)
    }

    fn cache_path_len(&mut self, pos: &Pos, path_length: usize) {
        self.path_length_cache.insert(*pos, path_length);
    }

    fn traverse(&mut self, start: &Pos, allow_short_circuit: bool) -> Option<(Vec<Pos>, usize)> {
        let path = astar(
            start,
            |p| {
                if allow_short_circuit {
                    if let Some(cached_distance) = self.path_length_cache.get(p) {
                        return vec![(self.end, *cached_distance)];
                    }
                }
                self.accessible_neighbors(p)
                    .into_iter()
                    .zip(vec![1].repeat(4))
                    .collect::<Vec<(Pos, usize)>>()
            },
            |p| p.distance_to(&self.end) as usize,
            |p| *p == self.end,
        )?;

        for (idx, pos) in path.0.iter().enumerate() {
            self.cache_path_len(pos, path.1 - idx);
        }

        Some(path)
    }

    fn find_best_starting_point(&mut self) -> usize {
        let starting_points = self
            .tiles
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(move |(x, tile)| (Pos(x, y), tile))
            })
            .filter_map(|(pos, tile)| if tile.0 == 0 { Some(pos) } else { None })
            .collect::<Vec<_>>();

        starting_points
            .iter()
            .filter_map(|start| self.shortest_path_length(start))
            .min()
            .unwrap()
    }
}

fn run_part_one(s: &str) -> usize {
    let mut map = s.parse::<HeightMap>().unwrap();
    let start = map.start;
    map.shortest_path_length(&start).unwrap()
}

fn run_part_two(s: &str) -> usize {
    let mut map = s.parse::<HeightMap>().unwrap();
    map.find_best_starting_point()
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 31);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 29);
    }
}
