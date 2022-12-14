type Result<T> = std::result::Result<T, anyhow::Error>;

struct Map<T> {
    number_of_rows: i32,
    number_of_columns: i32,
    row_data: Vec<Vec<T>>,
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

    fn values(&self) -> Vec<T> {
        let mut rv = Vec::new();

        for row in &self.row_data {
            for col in row {
                rv.push(col.clone());
            }
        }

        rv
    }
}

fn parse_point(s: &str) -> Result<(i32, i32)> {
    let mut xy = s.split(',');
    let x = xy.next().unwrap().parse()?;
    let y = xy.next().unwrap().parse()?;
    assert!(xy.next().is_none());
    Ok((x, y))
}

fn simulate_sand(map: &mut Map<char>, mut x: i32, mut y: i32) -> Result<Option<(i32, i32)>> {
    if map.at((x, y)).cloned() == Some('o') {
        return Ok(None);
    }

    loop {
        if x < 0 || x > map.number_of_columns {
            anyhow::bail!("simulation went out of bounds on x-axis: {}", x);
        }

        if map.at((x, y + 1)).is_none() {
            return Ok(None);
        }

        if map.at((x, y + 1)).cloned() == Some('.') {
            y += 1;
            continue;
        }

        if map.at((x - 1, y + 1)).cloned() == Some('.') {
            y += 1;
            x -= 1;
            continue;
        }

        if map.at((x + 1, y + 1)).cloned() == Some('.') {
            y += 1;
            x += 1;
            continue;
        }

        *map.at_mut((x, y)).unwrap() = 'o';
        return Ok(Some((x, y)));
    }
}

fn main() -> Result<()> {
    // 1000x200 ought to be enough for anybody.
    let width = 1000;
    let height = 200;

    let mut map: Map<char> = Map::new(width, height, &'.');

    let mut maxy = 0;

    for line in std::io::stdin().lines() {
        let line = line?;
        let line = line.trim();
        let points: Result<Vec<(i32, i32)>> =
            line.split("->").map(|x| parse_point(x.trim())).collect();
        let points = points?;

        assert!(points.len() > 1);

        maxy = maxy.max(points.iter().map(|xy| xy.1).max().unwrap());

        for ((x0, y0), (x1, y1)) in points.iter().cloned().zip(points[1..].iter().cloned()) {
            if x0 == x1 {
                for y in y0.min(y1)..=y0.max(y1) {
                    *map.at_mut((x0, y)).unwrap() = '#';
                }
            } else if y0 == y1 {
                for x in x0.min(x1)..=x0.max(x1) {
                    *map.at_mut((x, y0)).unwrap() = '#';
                }
            } else {
                anyhow::bail!(
                    "line is neither vertical nor horizontal: {},{} to {},{}",
                    x0,
                    y0,
                    x1,
                    y1
                );
            }
        }
    }

    let mut map_one = map.map(|ch| *ch);

    while simulate_sand(&mut map_one, 500, 0)?.is_some() {}

    eprintln!("{}", map_one.show(|v| v.to_string()));
    eprintln!();

    let grains_of_sand = map_one.values().iter().filter(|ch| **ch == 'o').count();

    println!("Grains of sand (with abyss): {}", grains_of_sand);

    let mut map_two = map.map(|ch| *ch);

    let floory = maxy + 2;
    for x in 0..width {
        *map_two.at_mut((x, floory)).unwrap() = '#';
    }

    while simulate_sand(&mut map_two, 500, 0)?.is_some() {}

    eprintln!("{}", map_two.show(|v| v.to_string()));
    eprintln!();

    let grains_of_sand = map_two.values().iter().filter(|ch| **ch == 'o').count();

    println!("Grains of sand (with floor): {}", grains_of_sand);

    Ok(())
}
