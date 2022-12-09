use std::{array, cmp::Ordering, collections::HashSet};

type Coordinate = (i32, i32);
const DIRS: [Coordinate; 9] = [
    (-1, 0),
    (0, 1),
    (1, 0),
    (0, -1),
    (-1, 1),
    (1, 1),
    (1, -1),
    (-1, -1),
    (0, 0),
];

fn main() {
    let input = include_str!("../input.txt");

    println!("part 1: {:?}", part1(input));
    println!("part 2: {:?}", part2(input));
}

fn solve<const N: usize>(mut knots: [Knot; N], input: &str) -> usize {
    for (d, l) in input.lines().map(|l| l.split_once(" ").unwrap()) {
        // head direction
        let (dx, dy) = DIRS[match d {
            "L" => 0,
            "U" => 1,
            "R" => 2,
            "D" => 3,
            _ => panic!("wtf"),
        }];
        // move
        for _ in 0..l.parse::<i32>().unwrap() {
            knots[0].position.0 += dx;
            knots[0].position.1 += dy;
            for i in 1..knots.len() {
                knots[i].slither(knots[i - 1].position)
            }
        }
    }
    knots.last().unwrap().visited.len()
}

fn part2(input: &str) -> usize {
    let knots: [Knot; 10] = array::from_fn(|_| Knot::default());
    solve(knots, input)
}

fn part1(input: &str) -> usize {
    let knots: [Knot; 2] = array::from_fn(|_| Knot::default());
    solve(knots, input)
}

#[derive(Debug)]
struct Knot {
    position: Coordinate,
    visited: HashSet<Coordinate>,
}

impl Knot {
    fn track(&mut self) {
        self.visited.insert(self.position);
    }

    fn slither(&mut self, pos: Coordinate) {
        if !self.is_adj(pos) {
            match pos.0.cmp(&self.position.0) {
                Ordering::Less => self.position.0 -= 1,
                Ordering::Greater => self.position.0 += 1,
                _ => (),
            }
            match pos.1.cmp(&self.position.1) {
                Ordering::Less => self.position.1 -= 1,
                Ordering::Greater => self.position.1 += 1,
                _ => (),
            }
            self.track();
        }
    }

    fn is_adj(&self, pos: Coordinate) -> bool {
        DIRS.iter()
            .any(|(dx, dy)| (self.position.0 + dx, self.position.1 + dy) == pos)
    }
}

impl Default for Knot {
    fn default() -> Self {
        Knot {
            position: (0, 0),
            visited: HashSet::from([(0, 0)]),
        }
    }
}
