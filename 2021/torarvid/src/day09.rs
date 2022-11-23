use std::collections::VecDeque;

use crate::util::file_by_lines;

struct Map {
    matrix: Vec<Vec<usize>>,
}

impl Map {
    fn new(lines: &Vec<String>) -> Map {
        let cell_value = |c: char| -> usize { c.to_digit(10).unwrap().try_into().unwrap() };
        let row = |line: &String| -> Vec<usize> { line.chars().map(cell_value).collect() };
        let matrix = lines.iter().map(row).collect();
        Map { matrix }
    }

    fn neighbors(&self, x: usize, y: usize) -> Vec<(usize, usize)> {
        let mut neighbors = Vec::new();
        if x > 0 {
            neighbors.push((x - 1, y));
        }
        if x < self.matrix[0].len() - 1 {
            neighbors.push((x + 1, y));
        }
        if y > 0 {
            neighbors.push((x, y - 1));
        }
        if y < self.matrix.len() - 1 {
            neighbors.push((x, y + 1));
        }
        neighbors
    }

    fn low_points(&self) -> Vec<(usize, usize)> {
        let mut points = Vec::new();
        for (y, row) in self.matrix.iter().enumerate() {
            for (x, cell) in row.iter().enumerate() {
                let neighbors = self.neighbors(x, y);
                let higher_neighbors = neighbors
                    .iter()
                    .filter(|(x, y)| self.matrix[*y][*x] > *cell)
                    .count();
                if higher_neighbors == neighbors.len() {
                    points.push((x, y));
                }
            }
        }
        points
    }

    fn part1(&self) -> usize {
        self.low_points()
            .iter()
            .map(|(x, y)| self.matrix[*y][*x] + 1)
            .sum()
    }

    fn part2(&self) -> usize {
        let mut basins = self
            .low_points()
            .iter()
            .map(|low_point| self.basin_size(*low_point))
            .collect::<Vec<_>>();
        basins.sort_by(|a, b| b.cmp(a));
        basins[0] * basins[1] * basins[2]
    }

    fn basin_size(&self, source_cell: (usize, usize)) -> usize {
        let mut queue: VecDeque<(usize, usize)> = VecDeque::new();
        let mut visited = Vec::new();
        queue.push_back(source_cell);
        while !queue.is_empty() {
            let (x, y) = queue.pop_front().unwrap();
            if !visited.contains(&(x, y)) {
                visited.push((x, y));
                queue.extend(
                    self.neighbors(x, y)
                        .iter()
                        .filter(|(x, y)| self.matrix[*y][*x] < 9),
                );
            }
        }
        visited.len()
    }
}

pub fn run() {
    let lines = &file_by_lines("day09.txt");
    let map = Map::new(lines);
    println!("Part 1: {}", map.part1());
    println!("Part 2: {}", map.part2());
}
