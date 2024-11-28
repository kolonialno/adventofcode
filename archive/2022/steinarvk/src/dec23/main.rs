type Result<T> = std::result::Result<T, anyhow::Error>;

use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::io::Read;

struct Map<T> {
    number_of_rows: i32,
    number_of_columns: i32,
    row_data: Vec<Vec<T>>,
}

impl Map<char> {
    fn from_stdin() -> Result<Map<char>> {
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        Map::from_string(&buffer)
    }

    fn from_string(s: &str) -> Result<Map<char>> {
        let rows: Vec<&str> = s.trim().split('\n').collect();
        let number_of_rows = rows.len();
        let number_of_cols = rows[0].len();

        if !(rows.iter().all(|s| s.trim().len() == number_of_cols)) {
            anyhow::bail!("rows are not all the same length");
        }

        let row_data = rows
            .iter()
            .map(|row| row.trim().chars().collect())
            .collect();

        Ok(Map {
            number_of_rows: number_of_rows.try_into().unwrap(),
            number_of_columns: number_of_cols.try_into().unwrap(),
            row_data,
        })
    }
}

impl<T> Map<T>
where
    T: Clone,
{
    fn new(number_of_columns: i32, number_of_rows: i32, starting_value: &T) -> Map<T> {
        let row_data = (0..number_of_rows)
            .map(|_| {
                (0..number_of_columns)
                    .map(|_| starting_value.clone())
                    .collect()
            })
            .collect();
        Map {
            number_of_rows,
            number_of_columns,
            row_data,
        }
    }

    fn at_mut(&mut self, (col, row): (i32, i32)) -> Option<&mut T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&mut self.row_data[row as usize][col as usize])
    }

    fn find(&self, needle: T) -> Vec<(i32, i32)>
    where
        T: PartialEq,
    {
        let mut rv: Vec<(i32, i32)> = Vec::new();

        for (y, row) in self.row_data.iter().enumerate() {
            for (x, value) in row.iter().enumerate() {
                if *value == needle {
                    rv.push((x.try_into().unwrap(), y.try_into().unwrap()));
                }
            }
        }

        rv
    }

    fn show<F>(&self, format_cell: F) -> String
    where
        F: Fn(&T) -> String,
    {
        let mut rv: Vec<String> = Vec::new();

        for row in &self.row_data {
            for col in row {
                rv.push(format_cell(col));
            }
            rv.push("\n".to_string());
        }

        rv.join("")
    }
}

type Point = (i32, i32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    North,
    West,
    East,
    South,
}

impl Direction {
    fn from_index(index: usize) -> Direction {
        match index.rem_euclid(4) {
            0 => Direction::North,
            1 => Direction::South,
            2 => Direction::West,
            3 => Direction::East,
            _ => panic!("Euclid has fallen"),
        }
    }

    fn dx(&self) -> i32 {
        match self {
            Direction::East => 1,
            Direction::West => -1,
            _ => 0,
        }
    }

    fn dy(&self) -> i32 {
        match self {
            Direction::North => -1,
            Direction::South => 1,
            _ => 0,
        }
    }

    fn step(&self, origin: Point) -> Point {
        (origin.0 + self.dx(), origin.1 + self.dy())
    }
}

struct Dance {
    positions: HashSet<Point>,
    round_number: usize,
}

impl Dance {
    fn from(points: &[Point]) -> Dance {
        Dance {
            positions: points.iter().copied().collect(),
            round_number: 0,
        }
    }

    fn is_occupied(&self, point: Point) -> bool {
        self.positions.contains(&point)
    }

    fn has_any_neighbours(&self, point: Point) -> bool {
        let (x, y) = point;
        for dx in -1..=1 {
            for dy in -1..=1 {
                if dx == 0 && dy == 0 {
                    continue;
                }
                if self.is_occupied((x + dx, y + dy)) {
                    return true;
                }
            }
        }
        false
    }

    fn scan_direction(&self, point: Point, dir: Direction) -> bool {
        let dxdys = match dir {
            Direction::North => [(-1, -1), (0, -1), (1, -1)],
            Direction::South => [(-1, 1), (0, 1), (1, 1)],
            Direction::West => [(-1, -1), (-1, 0), (-1, 1)],
            Direction::East => [(1, -1), (1, 0), (1, 1)],
        };
        for (dx, dy) in dxdys {
            let (x, y) = point;
            if self.is_occupied((x + dx, y + dy)) {
                return true;
            }
        }
        false
    }

    fn run_round(&mut self) -> Result<bool> {
        let mut counts: HashMap<Point, i32> = HashMap::new();
        let mut plans: HashMap<Point, Point> = HashMap::new();
        let mut moved = 0;

        for p in self.positions.iter() {
            if !self.has_any_neighbours(*p) {
                continue;
            }

            match (0..4)
                .map(|x| Direction::from_index(x + self.round_number))
                .filter(|x| !self.scan_direction(*p, *x))
                .map(|x| x.step(*p))
                .next()
            {
                None => continue,
                Some(next) => {
                    plans.insert(*p, next);
                    counts
                        .entry(next)
                        .and_modify(|x| *x += 1)
                        .or_insert_with(|| 1);
                }
            }
        }

        let mut next_gen: HashSet<Point> = HashSet::new();

        for p in self.positions.iter() {
            next_gen.insert(match plans.get(p) {
                None => *p,
                Some(next) => {
                    if counts.get(next).unwrap() > &1 {
                        *p
                    } else {
                        moved += 1;
                        *next
                    }
                }
            });
        }

        self.positions = next_gen;
        self.round_number += 1;

        Ok(moved > 0)
    }

    fn show(&self) {
        let xs: Vec<i32> = self.positions.iter().map(|x| x.0).collect();
        let ys: Vec<i32> = self.positions.iter().map(|x| x.1).collect();
        let x0 = xs.iter().min().unwrap();
        let x1 = xs.iter().max().unwrap();
        let y0 = ys.iter().min().unwrap();
        let y1 = ys.iter().max().unwrap();
        let dx = x1 - x0 + 1;
        let dy = y1 - y0 + 1;

        let mut m = Map::new(dx, dy, &'.');

        for p in self.positions.iter() {
            let (x, y) = p;
            let p = (x - x0, y - y0);
            *m.at_mut(p).unwrap() = '#';
        }

        println!("{}", m.show(|x| x.to_string()));
    }

    fn empty_dancefloor(&self) -> i32 {
        let xs: Vec<i32> = self.positions.iter().map(|x| x.0).collect();
        let ys: Vec<i32> = self.positions.iter().map(|x| x.1).collect();
        let x0 = xs.iter().min().unwrap();
        let x1 = xs.iter().max().unwrap();
        let y0 = ys.iter().min().unwrap();
        let y1 = ys.iter().max().unwrap();
        let dx = x1 - x0 + 1;
        let dy = y1 - y0 + 1;
        let space = dx * dy;
        space - (self.positions.len() as i32)
    }
}

fn main() -> Result<()> {
    let mut dance = Dance::from(&Map::from_stdin()?.find('#'));

    while dance.run_round()? {
        if env::var("SHOW_MAPS").is_ok() {
            println!("After round {}", dance.round_number);
            dance.show();
        }

        if dance.round_number == 10 {
            println!("Answer part A: {}", dance.empty_dancefloor());
        }
    }
    println!("Answer part B: {}", dance.round_number);

    Ok(())
}
