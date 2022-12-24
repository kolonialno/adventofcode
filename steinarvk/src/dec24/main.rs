type Result<T> = std::result::Result<T, anyhow::Error>;

use priority_queue::PriorityQueue;
use std::collections::HashMap;
use std::env;
use std::io::Read;

type Point = (i32, i32);

struct WindyMap {
    initial_map: Map<char>,
    width: i32,
    height: i32,
    period: i32,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct State {
    p: Point,
    t: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    North,
    West,
    East,
    South,
}

impl Direction {
    fn all() -> Vec<Direction> {
        vec![
            Direction::North,
            Direction::West,
            Direction::East,
            Direction::South,
        ]
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

    fn horizontal(&self) -> bool {
        self.dx() != 0
    }

    fn symbol(&self) -> char {
        match self {
            Direction::North => '^',
            Direction::South => 'v',
            Direction::West => '<',
            Direction::East => '>',
        }
    }

    fn opposite(&self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
            Direction::East => Direction::West,
        }
    }

    fn sign(&self) -> i32 {
        match self {
            Direction::North => -1,
            Direction::South => 1,
            Direction::West => -1,
            Direction::East => 1,
        }
    }

    fn step(&self, origin: Point) -> Point {
        (origin.0 + self.dx(), origin.1 + self.dy())
    }
}

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

    fn submap(&self, top_left: (i32, i32), size: (i32, i32)) -> Map<T> {
        let (x0, y0) = top_left;
        let x1 = x0 + size.0 - 1;
        let y1 = y0 + size.1 - 1;

        assert!(x0 >= 0 && x0 < self.number_of_columns);
        assert!(y0 >= 0 && y0 < self.number_of_rows);
        assert!(x1 >= 0 && x1 < self.number_of_columns);
        assert!(y1 >= 0 && y1 < self.number_of_rows);

        let mut rv: Vec<Vec<T>> = Vec::new();
        for y in y0..=y1 {
            let mut row: Vec<T> = Vec::new();
            for x in x0..=x1 {
                row.push(self.at((x, y)).unwrap().clone());
            }
            rv.push(row);
        }

        Map {
            number_of_rows: size.1,
            number_of_columns: size.0,
            row_data: rv,
        }
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

fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

impl WindyMap {
    fn from_stdin() -> Result<WindyMap> {
        let initial_map = Map::from_stdin()?;
        let width = initial_map.number_of_columns - 2;
        let height = initial_map.number_of_rows - 2;
        let initial_map = initial_map.submap((1, 1), (width, height));
        let period = width * height / gcd(width, height);
        Ok(WindyMap {
            initial_map,
            width,
            height,
            period,
        })
    }

    fn has_wind_at(&self, p: Point, t: i32, wind_direction: Direction) -> bool {
        let initial_p = if wind_direction.horizontal() {
            (
                (p.0 + wind_direction.opposite().sign() * t).rem_euclid(self.width),
                p.1,
            )
        } else {
            (
                p.0,
                (p.1 + wind_direction.opposite().sign() * t).rem_euclid(self.height),
            )
        };
        if let Some(ch) = self.initial_map.at(initial_p) {
            *ch == wind_direction.symbol()
        } else {
            false
        }
    }

    fn has_any_wind_at(&self, p: Point, t: i32) -> bool {
        Direction::all().iter().any(|d| self.has_wind_at(p, t, *d))
    }

    fn show_at(&self, t: i32) {
        println!(
            "t={}\n{}",
            t,
            self.initial_map
                .indexed_map(|p, _| {
                    let winds: Vec<Direction> = Direction::all()
                        .iter()
                        .filter(|d| self.has_wind_at(p, t, **d))
                        .copied()
                        .collect();
                    match &winds[..] {
                        [d] => d.symbol(),
                        [] => '.',
                        _ => char::from_digit(winds.len() as u32, 10).unwrap(),
                    }
                })
                .show(|x| x.to_string())
        );
    }

    fn upper_left_point(&self) -> (i32, i32) {
        (0, -1)
    }

    fn lower_right_point(&self) -> (i32, i32) {
        (self.width - 1, self.height)
    }

    fn in_bounds(&self, p: Point) -> bool {
        let (x, y) = p;
        x >= 0 && y >= 0 && x < self.width && y < self.height
    }

    fn walkable(&self, p: Point) -> bool {
        p == self.upper_left_point() || p == self.lower_right_point() || self.in_bounds(p)
    }

    fn heuristic_distance(&self, start: Point, goal: Point) -> i32 {
        (start.0 - goal.0).abs() + (start.1 - goal.1).abs()
    }

    fn period_index(&self, t: i32) -> i32 {
        t.rem_euclid(self.period)
    }

    fn distance_between(&self, start: Point, goal: Point, start_time: i32) -> Result<i32> {
        let mut pq: PriorityQueue<State, i32> = PriorityQueue::new();
        let mut g: HashMap<(Point, i32), i32> = HashMap::new();

        g.insert((start, self.period_index(start_time)), start_time);
        pq.push(
            State {
                p: start,
                t: start_time,
            },
            0,
        );

        while let Some((state, _)) = pq.pop().clone() {
            if state.p == goal {
                return Ok(state.t);
            }

            let mut nbs: Vec<Point> = Direction::all().iter().map(|d| d.step(state.p)).collect();
            nbs.push(state.p);

            for nb in nbs {
                // Note that the elves are perfectly fine walking directly _into_ a storm,
                // as long as they don't end at the same tile as it.
                // If this wasn't fine (which would make more sense), we'd need to add a
                // check for a "directional obstacle" at the tile we're moving into measured
                // at the _current_ turn: moving with the wind or perpendicular to it is fine,
                // moving directly against it makes the move illegal. However, in this question,
                // such a move is not considered illegal -- it's even a part of the example,
                // minute 4 to 5. If we treat this as an obstacle, we'll get the wrong answer.
                // Therefore, the only check we need to do is whether or not there will be any
                // storm at the tile we're moving into on the _next_ turn.
                // Walk on through the wind!
                let ok = self.walkable(nb) && !self.has_any_wind_at(nb, state.t + 1);

                if ok {
                    let state = State {
                        p: nb,
                        t: state.t + 1,
                    };
                    let key = (state.p, self.period_index(state.t));
                    let accept = if let Some(previous_score) = g.get(&key) {
                        state.t < *previous_score
                    } else {
                        true
                    };
                    if accept {
                        let priority = state.t + self.heuristic_distance(state.p, goal);
                        g.insert(key, state.t);
                        pq.push(state, -priority);
                    }
                }
            }
        }

        anyhow::bail!("no path found")
    }
}

fn main() -> Result<()> {
    let m = WindyMap::from_stdin()?;

    if env::var("SHOW_MAPS").is_ok() {
        for i in 0..=m.period {
            m.show_at(i);
        }
    }

    let t = m.distance_between(m.upper_left_point(), m.lower_right_point(), 0)?;
    println!("Answer part A: {}", t);

    let t = m.distance_between(m.lower_right_point(), m.upper_left_point(), t)?;
    let t = m.distance_between(m.upper_left_point(), m.lower_right_point(), t)?;
    println!("Answer part B: {}", t);

    Ok(())
}
