use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    let map = Map::from(&content[..]);
    println!("Part1: {}", solve_part1(map.clone()));
    println!("Part2: {}", solve_part2(map));
}

#[derive(Clone, Debug, PartialEq)]
enum CellType {
    Floor,
    EmptyChair,
    OccupiedChair,
}

impl From<char> for CellType {
    fn from(c: char) -> Self {
        match c {
            '.' => CellType::Floor,
            'L' => CellType::EmptyChair,
            _ => panic!("Unknown char"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Map {
    cells: Vec<Vec<CellType>>,
}

impl From<&str> for Map {
    fn from(s: &str) -> Self {
        let mut map = Map { cells: Vec::new() };
        s.lines().for_each(|line| {
            let cells_in_line = line.chars().map(CellType::from).collect::<Vec<CellType>>();
            map.cells.push(cells_in_line);
        });
        map
    }
}

impl Map {
    fn step1(&self) -> Map {
        let mut new_map = Map {
            cells: self.cells.clone(),
        };
        for (row_idx, row) in self.cells.iter().enumerate() {
            for (cell_idx, cell) in row.iter().enumerate() {
                match cell {
                    CellType::EmptyChair => {
                        let neighbors = self.count_neighbors(row_idx as isize, cell_idx as isize);
                        if neighbors == 0 {
                            new_map.cells[row_idx][cell_idx] = CellType::OccupiedChair;
                        }
                    }
                    CellType::OccupiedChair => {
                        let neighbors = self.count_neighbors(row_idx as isize, cell_idx as isize);
                        if neighbors >= 4 {
                            new_map.cells[row_idx][cell_idx] = CellType::EmptyChair;
                        }
                    }
                    _ => {}
                }
            }
        }
        new_map
    }

    fn count_neighbors(&self, row: isize, col: isize) -> usize {
        vec![
            self.neighbor_is_valid(row - 1, col - 1),
            self.neighbor_is_valid(row - 1, col),
            self.neighbor_is_valid(row - 1, col + 1),
            self.neighbor_is_valid(row, col - 1),
            self.neighbor_is_valid(row, col + 1),
            self.neighbor_is_valid(row + 1, col - 1),
            self.neighbor_is_valid(row + 1, col),
            self.neighbor_is_valid(row + 1, col + 1),
        ]
        .iter()
        .flatten()
        .filter(|c| ***c == CellType::OccupiedChair)
        .count()
    }

    fn step2(&self) -> Map {
        let mut new_map = Map {
            cells: self.cells.clone(),
        };
        for (row_idx, row) in self.cells.iter().enumerate() {
            for (cell_idx, cell) in row.iter().enumerate() {
                match cell {
                    CellType::EmptyChair => {
                        let neighbors = self.count_neighbors_2(row_idx as isize, cell_idx as isize);
                        if neighbors == 0 {
                            new_map.cells[row_idx][cell_idx] = CellType::OccupiedChair;
                        }
                    }
                    CellType::OccupiedChair => {
                        let neighbors = self.count_neighbors_2(row_idx as isize, cell_idx as isize);
                        if neighbors >= 5 {
                            new_map.cells[row_idx][cell_idx] = CellType::EmptyChair;
                        }
                    }
                    _ => {}
                }
            }
        }
        new_map
    }

    fn count_neighbors_2(&self, row: isize, col: isize) -> u8 {
        self.neighbor_occupied_at(row, col, -1, -1, 1)
            + self.neighbor_occupied_at(row, col, -1, 0, 1)
            + self.neighbor_occupied_at(row, col, -1, 1, 1)
            + self.neighbor_occupied_at(row, col, 0, -1, 1)
            + self.neighbor_occupied_at(row, col, 0, 1, 1)
            + self.neighbor_occupied_at(row, col, 1, -1, 1)
            + self.neighbor_occupied_at(row, col, 1, 0, 1)
            + self.neighbor_occupied_at(row, col, 1, 1, 1)
    }

    fn neighbor_occupied_at(
        &self,
        row: isize,
        col: isize,
        x_offset: isize,
        y_offset: isize,
        distance: isize,
    ) -> u8 {
        if let Some(n) =
            self.neighbor_is_valid(row + y_offset * distance, col + x_offset * distance)
        {
            if *n == CellType::Floor {
                return self.neighbor_occupied_at(row, col, x_offset, y_offset, distance + 1);
            }
            if *n == CellType::OccupiedChair {
                return 1;
            }
        }
        0
    }

    fn neighbor_is_valid(&self, row: isize, col: isize) -> Option<&CellType> {
        self.cells
            .get(row as usize)
            .and_then(|r| r.get(col as usize))
    }

    fn count_cells(&self, cell_type: &CellType) -> usize {
        let mut count = 0;
        for row in &self.cells {
            count += row.iter().filter(|c| *c == cell_type).count();
        }
        count
    }
}

fn solve_part1(map: Map) -> usize {
    let mut m = map;
    loop {
        let new_map = m.step1();
        if new_map == m {
            return new_map.count_cells(&CellType::OccupiedChair);
        }
        m = new_map;
    }
}

fn solve_part2(map: Map) -> usize {
    let mut m = map;
    loop {
        let new_map = m.step2();
        if new_map == m {
            return new_map.count_cells(&CellType::OccupiedChair);
        }
        m = new_map;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let map = Map::from(
            "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL",
        );
        assert_eq!(solve_part1(map.clone()), 37);
        assert_eq!(solve_part2(map), 26);
    }
}
