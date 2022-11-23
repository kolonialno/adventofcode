use std::{
    borrow::Borrow,
    collections::{BinaryHeap, HashMap, HashSet},
    hash::Hash,
    str::FromStr,
    time::Instant,
};

const DIRS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

fn main() {
    let t = Instant::now();
    let grid = include_str!("../input.txt").parse::<Grid>().unwrap();

    // If we want to know the actual path
    // let mut came_from = HashMap::from([((0, 0), None)]);

    let mut p_q = BinaryHeap::from([grid.get((0, 0))]);
    let mut cost = HashMap::from([((0, 0), 0)]);

    while let Some(Node { position, cost: _ }) = p_q.pop() {
        if position == grid.get_end_coord() {
            break;
        }

        for n in grid.get_neighbours(&position) {
            let n_cost = *cost.entry(position).or_insert(0) + n.cost;
            if !cost.contains_key(&n.position) || n_cost < *cost.get(&n.position).unwrap() {
                cost.insert(n.position, n_cost);
                p_q.push(Node {
                    position: n.position,
                    cost: n_cost,
                })
            }
        }
    }

    println!("{:?}", cost.get(&grid.get_end_coord()).unwrap());
    println!("elapsed: {:?}", t.elapsed())
}

type Position = (i32, i32);
#[derive(Debug, Copy, Clone, Eq)]
struct Node {
    position: Position,
    cost: u32,
}

// by implementing these std-traid for Node
// Binary-Heap will function as a min-heap
// instead of a max-heap for Node
impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// by implementing these std-traits for Node
// we can lookup by Position
// in the hash set (without knowing the cost), very cool
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

impl Hash for Node {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.position.hash(state);
    }
}

impl Borrow<Position> for Node {
    fn borrow(&self) -> &Position {
        &self.position
    }
}

#[derive(Debug)]
struct Grid(HashSet<Node>);

impl Grid {
    fn get(&self, c: (i32, i32)) -> Node {
        *self.0.get(&c).unwrap()
    }

    fn get_neighbours(&self, (x, y): &(i32, i32)) -> Vec<Node> {
        DIRS.iter().fold(Vec::new(), |mut acc, (dx, dy)| {
            if let Some(n) = self.0.get(&(x + dx, y + dy)) {
                acc.push(*n);
            }
            acc
        })
    }

    // somewhat hacky
    fn get_end_coord(&self) -> (i32, i32) {
        let n = (self.0.len() as f64).sqrt() as i32;
        (n - 1, n - 1)
    }
}

impl FromStr for Grid {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid: HashSet<Node> = HashSet::new();
        let mut grid = Self(grid);
        for i in 0..=4 {
            for j in 0..=4 {
                for (row, r) in s.lines().enumerate() {
                    for (col, c) in r.chars().enumerate() {
                        let (col, row) = ((col as i32) + (j * 100), (row as i32) + (i * 100));
                        grid.0.insert(Node {
                            position: (col, row),
                            cost: get_cost(c, &grid, col, row),
                        });
                    }
                }
            }
        }

        Ok(grid)
    }
}

fn get_cost(c: char, grid: &Grid, col: i32, row: i32) -> u32 {
    let d = if col > 99 {
        grid.0.get(&(col - 100, row)).unwrap().cost + 1
    } else if row > 99 {
        grid.0.get(&(col, row - 100)).unwrap().cost + 1
    } else {
        char::to_digit(c, 10).unwrap()
    };

    if d > 9 {
        1
    } else {
        d
    }
}
