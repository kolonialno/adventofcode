type Result<T> = std::result::Result<T, anyhow::Error>;

use std::collections::VecDeque;
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
    fn at(&self, (col, row): (i32, i32)) -> Option<&T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&self.row_data[row as usize][col as usize])
    }

    fn at_mut(&mut self, (col, row): (i32, i32)) -> Option<&mut T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&mut self.row_data[row as usize][col as usize])
    }

    fn indexed_map<F, B>(&self, f: F) -> Map<B>
    where
        F: Fn((i32, i32), &T) -> B,
        B: Clone,
    {
        let row_data = self
            .row_data
            .iter()
            .enumerate()
            .map(|(row_index, row)| {
                row.iter()
                    .enumerate()
                    .map(|(col_index, value)| {
                        f(
                            (col_index.try_into().unwrap(), row_index.try_into().unwrap()),
                            value,
                        )
                    })
                    .collect()
            })
            .collect();
        Map {
            number_of_rows: self.number_of_rows,
            number_of_columns: self.number_of_columns,
            row_data,
        }
    }

    fn in_bounds(&self, (col, row): (i32, i32)) -> bool {
        !(row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns)
    }

    fn neighbours(&self, (col, row): (i32, i32)) -> Vec<(i32, i32)> {
        [(0, -1), (0, 1), (-1, 0), (1, 0)]
            .iter()
            .filter_map(|(dx, dy)| {
                let p = (col + dx, row + dy);
                if self.in_bounds(p) {
                    Some(p)
                } else {
                    None
                }
            })
            .collect()
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

    fn map<F, B>(&self, f: F) -> Map<B>
    where
        F: Fn(&T) -> B,
        B: Clone,
    {
        self.indexed_map(|_, x| f(x))
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

fn step_valid(heightmap: &Map<i32>, a: (i32, i32), b: (i32, i32)) -> bool {
    match (heightmap.at(a), heightmap.at(b)) {
        (Some(h0), Some(h1)) => (*h0 + 1) >= *h1,
        _ => false,
    }
}

fn number_of_valid_steps_to_goal(
    heightmap: &Map<i32>,
    start: (i32, i32),
    goal: (i32, i32),
) -> Option<i32> {
    let mut visited = heightmap.map(|_| false);

    let mut q = VecDeque::new();
    q.push_back((0, start));

    while !q.is_empty() {
        let (steps, p) = q.pop_front().unwrap();

        if p == goal {
            return Some(steps);
        }

        if *visited.at(p).unwrap() {
            continue;
        }
        *visited.at_mut(p).unwrap() = true;

        for nb in heightmap.neighbours(p) {
            if step_valid(heightmap, p, nb) {
                q.push_back((steps + 1, nb));
            }
        }
    }

    None
}

fn shortest_path_to_zero(heightmap: &Map<i32>, top: (i32, i32)) -> Option<i32> {
    let mut visited = heightmap.map(|_| false);

    let mut q = VecDeque::new();
    q.push_back((0, top));

    while !q.is_empty() {
        let (steps, p) = q.pop_front().unwrap();

        if *heightmap.at(p).unwrap() == 0 {
            return Some(steps);
        }

        if *visited.at(p).unwrap() {
            continue;
        }
        *visited.at_mut(p).unwrap() = true;

        for nb in heightmap.neighbours(p) {
            if step_valid(heightmap, nb, p) {
                q.push_back((steps + 1, nb));
            }
        }
    }

    None
}

fn main() -> Result<()> {
    let heightmap = Map::from_stdin()?;

    let start = *heightmap.find('S').first().unwrap();
    let end = *heightmap.find('E').first().unwrap();

    let heightmap = heightmap.map(|ch| match *ch {
        ch if ('a'..='z').contains(&ch) => (ch as u32 - 'a' as u32) as i32,
        'S' => 0,
        'E' => ('z' as u32 - 'a' as u32) as i32,
        _ => panic!("unknown map cell: {}", ch),
    });

    println!(
        "Heightmap:\n{}\n\n",
        heightmap.show(|x| format!("{}", (*x as u8 + b'a') as char))
    );

    println!(
        "Length of shortest path from start to goal (part A): {}",
        number_of_valid_steps_to_goal(&heightmap, start, end).unwrap()
    );

    println!(
        "Length of shortest path from zero to goal (part B): {}",
        shortest_path_to_zero(&heightmap, end).unwrap()
    );

    Ok(())
}
