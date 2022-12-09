use std::{array, collections::HashSet};

type Coordinate = (i32, i32);

fn main() {
    let input = include_str!("../input.txt");
    let knots: [Knot; 10] = array::from_fn(|_| Knot::default());
    println!("part 1 and 2: {:?}", solve(knots, input));
}

fn solve<const N: usize>(mut knots: [Knot; N], input: &str) -> (usize, usize) {
    for (d, l) in input.lines().map(|l| l.split_once(" ").unwrap()) {
        // head direction
        let (dx, dy) = match d {
            "L" => (-1, 0),
            "U" => (0, 1),
            "R" => (1, 0),
            "D" => (0, -1),
            _ => panic!("wtf"),
        };
        for _ in 0..l.parse::<i32>().unwrap() {
            // move head
            knots[0].position.0 += dx;
            knots[0].position.1 += dy;
            // move rest
            for i in 1..knots.len() {
                knots[i].slither(knots[i - 1].position)
            }
        }
    }
    // return second knot for part 1 and last for part 2
    (knots[1].visited.len(), knots[9].visited.len())
}

#[derive(Debug)]
struct Knot {
    position: Coordinate,
    visited: HashSet<Coordinate>,
}

impl Knot {
    fn slither(&mut self, pos: Coordinate) {
        if !self.is_adj(pos) {
            self.position.0 += (pos.0 - self.position.0).signum();
            self.position.1 += (pos.1 - self.position.1).signum();
            self.visited.insert(self.position);
        }
    }

    fn is_adj(&self, pos: Coordinate) -> bool {
        self.position.0.abs_diff(pos.0) <= 1 && self.position.1.abs_diff(pos.1) <= 1
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
