use std::collections::{HashSet, VecDeque};
use std::str::FromStr;

const DIRS: [(i32, i32); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn main() {
    let mut grid = include_str!("../input.txt").parse::<OctopiGrid>().unwrap();

    let mut i = 1;
    while grid.increment_q_and_resolve() != 100 {
        if i == 100 {
            println!("total flashes after 100 iterations: {:?}", grid.flashes);
        }
        i += 1;
    }
    println!("syncronized after {} iterations", i);
}

#[derive(Debug)]
struct OctopiGrid {
    grid: [[i32; 10]; 10],
    flashes: u64,
}

impl OctopiGrid {
    fn increment_q_and_resolve(&mut self) -> u32 {
        let mut q = VecDeque::new();
        let mut seen = HashSet::new();
        for row in 0..10 {
            for col in 0..10 {
                self.grid[row][col] += 1;
                if self.grid[row][col] > 9 {
                    q.push_back((row, col));
                    seen.insert((row, col));
                }
            }
        }
        self.resolve_flashes(q, seen)
    }

    fn resolve_flashes(
        &mut self,
        mut q: VecDeque<(usize, usize)>,
        mut seen: HashSet<(usize, usize)>,
    ) -> u32 {
        let mut local_flash = 0;
        while let Some((x, y)) = q.pop_front() {
            self.flashes += 1;
            local_flash += 1;
            for (nx, ny) in Self::get_neighbours((x as i32, y as i32)) {
                self.grid[nx][ny] += 1;
                if self.grid[nx][ny] > 9 && !seen.contains(&(nx, ny)) {
                    seen.insert((nx, ny));
                    q.push_back((nx, ny));
                }
            }
        }
        for (x, y) in seen {
            self.grid[x][y] = 0;
        }
        local_flash
    }

    fn get_neighbours((x, y): (i32, i32)) -> Vec<(usize, usize)> {
        DIRS.iter()
            .map(|&(dx, dy)| (x + dx, y + dy))
            .filter(|&(x, y)| x > -1 && y > -1 && x < 10 && y < 10)
            .fold(vec![], |mut acc, (x, y)| {
                acc.push((x as usize, y as usize));
                acc
            })
    }
}

impl FromStr for OctopiGrid {
    type Err = String;
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut grid = [[0; 10]; 10];
        for (row, oc) in input.lines().enumerate() {
            for (col, c) in oc.chars().enumerate() {
                grid[row][col] = c.to_digit(10).unwrap() as i32;
            }
        }
        Ok(Self { grid, flashes: 0 })
    }
}
