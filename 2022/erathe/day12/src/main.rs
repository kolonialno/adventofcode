use std::collections::{HashMap, VecDeque};

type Coordinate = (i32, i32);
const DIRS: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part1(input: &str) -> usize {
    let (grid, mut starts, end) = initialise(input);
    let start = starts.pop_front().unwrap();
    if let Some(path) = bfs(start, end, &grid) {
        return path;
    }
    0
}

fn part2(input: &str) -> usize {
    let mut steps_min = usize::MAX;
    let (grid, mut starts, end) = initialise(input);
    while let Some(start) = starts.pop_back() {
        if let Some(path) = bfs(start, end, &grid) {
            steps_min = steps_min.min(path);
        }
    }
    steps_min
}

fn bfs(start: Coordinate, end: Coordinate, grid: &HashMap<Coordinate, i32>) -> Option<usize> {
    let mut q = VecDeque::from([start]);
    let mut came_from: HashMap<Coordinate, Coordinate> = HashMap::new();
    while let Some(c) = q.pop_back() {
        if c == end {
            break;
        }
        for n in get_legal_neighbours(&c, grid) {
            if came_from.get(&n).is_none() {
                q.push_front(n);
                came_from.insert(n, c);
            }
        }
    }

    let mut current = end;
    let mut steps = 0;
    while current != start {
        steps += 1;
        let Some(c) = came_from.get(&current) else {
            return None;
        };
        current = *c;
    }
    Some(steps)
}

fn get_legal_neighbours(c: &Coordinate, grid: &HashMap<Coordinate, i32>) -> Vec<Coordinate> {
    let mut neighbours = vec![];
    let height = grid.get(c).unwrap();
    for (dx, dy) in DIRS {
        if let Some(coordinate) = grid.get(&(c.0 + dx, c.1 + dy)) {
            if coordinate - height <= 1 {
                neighbours.push((c.0 + dx, c.1 + dy));
            }
        }
    }
    neighbours
}

fn initialise(input: &str) -> (HashMap<Coordinate, i32>, VecDeque<Coordinate>, Coordinate) {
    let mut grid = HashMap::<(i32, i32), i32>::new();
    let mut starts = VecDeque::new();
    let mut end = (0, 0);
    for (y, line) in input.lines().enumerate() {
        for (x, char) in line.chars().enumerate() {
            let (x, y) = (x as i32, y as i32);
            match char {
                'S' => {
                    starts.push_front((x, y));
                    grid.insert((x, y), 'a' as i32 - 0x30)
                }
                'E' => {
                    end = (x, y);
                    grid.insert((x, y), 'z' as i32 - 0x30)
                }
                'a' => {
                    starts.push_back((x, y));
                    grid.insert((x, y), char as i32 - 0x30)
                }
                _ => grid.insert((x, y), char as i32 - 0x30),
            };
        }
    }
    (grid, starts, end)
}
