use std::collections::HashMap;

use crate::util::file_by_lines;

pub fn run() {
    let lines = file_by_lines("day11.txt");
    let mut grid = Grid::new(&lines);
    let mut flashes: usize = 0;
    for _ in 0..100 {
        grid.step();
        flashes += grid.count_flashers();
    }
    println!("Part 1: {}", flashes);

    grid = Grid::new(&lines);
    for i in 0..100000 {
        grid.step();
        if grid.count_flashers() == 100 {
            println!("Part 2: {}", i + 1);
            break;
        }
    }
}

#[derive(Debug)]
struct Grid {
    cells: HashMap<(i32, i32), Cell>,
}

impl Grid {
    fn new(input: &Vec<String>) -> Grid {
        let mut cells = HashMap::new();
        for (y, line) in input.iter().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let cell = Cell {
                    x: x as i32,
                    y: y as i32,
                    value: c.to_digit(10).unwrap() as i32,
                    flashed: false,
                };
                cells.insert((cell.x, cell.y), cell);
            }
        }
        Grid { cells }
    }

    fn adjacents(&self, x: i32, y: i32) -> Vec<(i32, i32)> {
        let mut adjacents = Vec::new();
        for i in -1..=1 {
            for j in -1..=1 {
                if i == 0 && j == 0 {
                    continue;
                }
                if self.cells.contains_key(&(x + i, y + j)) {
                    adjacents.push((x + i, y + j));
                }
            }
        }
        adjacents
    }

    fn step(&mut self) {
        let mut flashers = Vec::new();
        for cell in self.cells.values_mut() {
            cell.flashed = false;
            if cell.step_maybe_flash() {
                flashers.push(cell.clone());
            }
        }

        while flashers.len() > 0 {
            let flasher = flashers.pop().unwrap();
            for adj in self.adjacents(flasher.x, flasher.y) {
                let cell = self.cells.get_mut(&adj).unwrap();
                if cell.step_maybe_flash() {
                    flashers.push(cell.clone());
                }
            }
        }
    }

    fn count_flashers(&self) -> usize {
        self.cells.values().filter(|cell| cell.flashed).count()
    }
}

#[derive(Debug, Clone)]
struct Cell {
    x: i32,
    y: i32,
    value: i32,
    flashed: bool,
}

impl Cell {
    fn step_maybe_flash(&mut self) -> bool {
        if self.flashed {
            return false;
        }
        self.value += 1;
        if self.value > 9 {
            self.value = 0;
            self.flashed = true;
        }
        self.flashed
    }
}
