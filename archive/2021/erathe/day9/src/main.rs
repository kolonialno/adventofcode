use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};

const DIRS: [(i32, i32); 4] = [(-1, -0), (0, -1), (0, 1), (1, 0)];
const DIRS_ALL: [(i32, i32); 8] = [
    (-1, -1),
    (-1, -0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];
fn main() {
    let grid = include_str!("../input.txt")
        .lines()
        .map(|s| s.chars().map(|c| c.to_digit(10).unwrap()))
        .enumerate()
        .fold(HashMap::<(i32, i32), i32>::new(), |mut acc, (row, line)| {
            for (col, c) in line.into_iter().enumerate() {
                acc.insert((row as i32, col as i32), c as i32);
            }
            acc
        });

    let mut lows = vec![];
    let mut lows_pos = vec![];
    for ((x, y), &h) in &grid {
        if DIRS_ALL.iter().all(|(dx, dy)| {
            if let Some(&height) = grid.get(&(x + dx, y + dy)) {
                if h < height {
                    return true;
                }
                return false;
            }
            true
        }) {
            lows.push(h + 1);
            lows_pos.push((x, y));
        }
    }

    //part 1
    println!("{}", lows.iter().sum::<i32>());

    let mut basins = vec![];
    for (&x, &y) in lows_pos {
        let mut visited = HashSet::from([(x, y)]);
        let mut to_visit = VecDeque::from([(x, y)]);
        while let Some((x, y)) = to_visit.pop_front() {
            visited.insert((x, y));
            for (dx, dy) in DIRS {
                if !visited.contains(&(x + dx, y + dy)) {
                    if let Some(&h) = grid.get(&(x + dx, y + dy)) {
                        if h < 9 {
                            to_visit.push_back((x + dx, y + dy));
                        }
                    }
                }
            }
        }
        basins.push(visited.len());
    }

    //part 2
    println!(
        "{:?}",
        basins.iter().sorted().rev().take(3).product::<usize>()
    );
}
