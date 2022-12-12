use std::{
    cmp::{min, Reverse},
    fs::{self},
    ops::Add,
};

use helpers::get_input_file;

#[derive(Copy, Clone, Hash, Default, Eq, PartialEq, Debug)]
struct Position {
    x: isize,
    y: isize,
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

const DIRECTIONS: [Position; 4] = [
    Position { x: -1, y: 0 },
    Position { x: 1, y: 0 },
    Position { x: 0, y: 1 },
    Position { x: 0, y: -1 },
];

struct Grid {
    position: Position,
    goal: Position,
    grid: Vec<Vec<usize>>,
    visited: Vec<Vec<bool>>,
    shortest: Vec<Vec<usize>>,
}

impl Grid {
    fn parse(input: &str) -> Grid {
        let mut position = Position { x: -1, y: -1 };
        let mut goal = Position { x: -1, y: -1 };
        let mut grid = Vec::default();
        for (ri, line) in input.split('\n').enumerate() {
            if line.trim().is_empty() {
                continue;
            }
            let mut row = Vec::default();
            for (ci, c) in line.chars().enumerate() {
                row.push(match c {
                    'a'..='z' => c as usize - 96,
                    'S' => {
                        position = Position {
                            x: ci as isize,
                            y: ri as isize,
                        };
                        'a' as usize - 96
                    }
                    'E' => {
                        goal = Position {
                            x: ci as isize,
                            y: ri as isize,
                        };
                        'z' as usize - 96
                    }
                    _ => panic!("Unknown input!"),
                });
            }
            grid.push(row);
        }

        let visited = vec![vec![false; grid[0].len()]; grid.len()];
        let shortest = vec![vec![usize::MAX; grid[0].len()]; grid.len()];
        Self {
            position,
            goal,
            grid,
            visited,
            shortest,
        }
    }

    fn can_enter(&self, origin: Position, target: Position) -> bool {
        let x_diff = target.x.abs_diff(origin.x);
        let y_diff = target.y.abs_diff(origin.y);
        if (x_diff == 1 && y_diff == 0 || x_diff == 0 && y_diff == 1)
            && target.x < self.grid[0].len() as isize
            && target.x >= 0
            && target.y < self.grid.len() as isize
            && target.y >= 0
        {
            let current_value = self.grid[origin.y as usize][origin.x as usize];
            let target_value = self.grid[target.y as usize][target.x as usize];
            return target_value as isize - current_value as isize <= 1;
        }
        false
    }

    fn already_visited(&self, target: Position) -> bool {
        self.visited[target.y as usize][target.x as usize]
    }

    fn update_shortest(&mut self, pos: Position, value: usize) {
        let x = pos.x as usize;
        let y = pos.y as usize;
        self.shortest[y][x] = min(self.shortest[y][x], value);
    }

    fn update_visited(&mut self, pos: Position, value: bool) {
        let x = pos.x as usize;
        let y = pos.y as usize;
        self.visited[y][x] = value;
    }

    fn reset(&mut self) {
        self.visited = vec![vec![false; self.grid[0].len()]; self.grid.len()];
        self.shortest = vec![vec![usize::MAX; self.grid[0].len()]; self.grid.len()];
    }

    fn dijkstra(&mut self, position: Position) -> usize {
        self.reset();
        let mut unvisited = vec![(0, position)];
        self.update_shortest(self.position, 0);
        while !unvisited.is_empty() {
            let (distance, pos) = unvisited.pop().unwrap();
            if self.already_visited(pos) {
                continue;
            }

            for dir in DIRECTIONS {
                let target = pos + dir;
                if self.can_enter(pos, target) && !self.already_visited(target) {
                    self.update_shortest(target, distance + 1);
                    unvisited.push((distance + 1, target));
                }
            }
            unvisited.sort_unstable_by_key(|(distance, _)| Reverse(*distance));
            self.update_visited(pos, true);
        }
        self.shortest[self.goal.y as usize][self.goal.x as usize]
    }

    fn starting_points(&self) -> Vec<Position> {
        let mut starting_points = Vec::default();
        for (y, row) in self.grid.iter().enumerate() {
            for (x, &val) in row.iter().enumerate() {
                if val == 1 {
                    starting_points.push(Position {
                        x: x as isize,
                        y: y as isize,
                    });
                }
            }
        }
        starting_points
    }
}

fn main() {
    let input = fs::read_to_string(get_input_file()).unwrap();
    // let input = fs::read_to_string("12/test.txt").unwrap();
    let mut grid = Grid::parse(input.as_str());
    println!(
        "Shortest path from original starting point: {}",
        grid.dijkstra(grid.position)
    );

    let mut lowest = usize::MAX;
    for point in grid.starting_points() {
        lowest = min(lowest, grid.dijkstra(point));
    }
    println!("Shortest scenic trail: {}", lowest);
}
