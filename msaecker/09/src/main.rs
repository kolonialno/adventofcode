use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

use helpers::get_input_file;

#[derive(Clone, Copy)]
enum Direction {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Clone, Copy)]
struct Move {
    direction: Direction,
    distance: usize,
}

impl Move {
    fn parse(input: &str) -> Self {
        let (dir, dis) = input.split_once(' ').unwrap();
        let direction = match dir {
            "L" => Direction::Left,
            "R" => Direction::Right,
            "U" => Direction::Up,
            _ => Direction::Down,
        };
        let distance = dis.parse::<usize>().unwrap();
        Self {
            direction,
            distance,
        }
    }
}

#[derive(Copy, Clone, Hash, Default, Eq, PartialEq, Debug)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn is_adjacent(&self, other: Self) -> bool {
        self.x.abs_diff(other.x) <= 1 && self.y.abs_diff(other.y) <= 1
    }

    fn diff(&self, new_head: Position) -> Position {
        if self.is_adjacent(new_head) {
            Position::new(0, 0)
        } else {
            let x = match self.x.cmp(&new_head.x) {
                std::cmp::Ordering::Less => 1,
                std::cmp::Ordering::Greater => -1,
                std::cmp::Ordering::Equal => 0,
            };
            let y = match self.y.cmp(&new_head.y) {
                std::cmp::Ordering::Less => 1,
                std::cmp::Ordering::Greater => -1,
                std::cmp::Ordering::Equal => 0,
            };
            Position::new(x, y)
        }
    }

    fn follow(&mut self, new_head: Position) {
        let diff = self.diff(new_head);
        self.x += diff.x;
        self.y += diff.y;
    }
}

#[derive(Eq, PartialEq, Clone)]
struct Knot {
    pos: Position,
    visited_positions: HashSet<Position>,
}

impl Knot {
    fn movement(&mut self, dir: Direction) {
        match dir {
            Direction::Left => self.pos.x -= 1,
            Direction::Right => self.pos.x += 1,
            Direction::Up => self.pos.y += 1,
            Direction::Down => self.pos.y -= 1,
        }
    }

    fn follow(&mut self, new_head: Position) {
        self.pos.follow(new_head);
        self.visited();
    }

    fn visited(&mut self) {
        self.visited_positions.insert(self.pos);
    }
}

impl Default for Knot {
    fn default() -> Self {
        let mut k = Self {
            pos: Default::default(),
            visited_positions: Default::default(),
        };
        k.visited();
        k
    }
}

#[derive(Clone)]
struct Rope {
    knots: Vec<Knot>,
}

impl Rope {
    fn new(number: usize) -> Self {
        Self {
            knots: vec![Knot::default(); number],
        }
    }

    fn move_head(&mut self, movement: Move) {
        for _ in 0..movement.distance {
            self.knots[0].movement(movement.direction);
            let mut current_head_pos = self.knots[0].pos;
            for knot in self.knots.iter_mut().skip(1) {
                knot.follow(current_head_pos);
                current_head_pos = knot.pos;
            }
        }
    }
}

fn main() {
    // let file = File::open("09/test.txt").unwrap();
    let file = File::open(get_input_file()).unwrap();
    let lines = std::iter::Iterator::flatten(BufReader::new(file).lines());
    let mut rope = Rope::new(2);
    let mut nested_rope = Rope::new(10);
    for line in lines {
        let head_movement = Move::parse(line.as_str());
        rope.move_head(head_movement);
        nested_rope.move_head(head_movement);
    }
    println!(
        "Visited positions: {}",
        rope.knots[rope.knots.len() - 1].visited_positions.len()
    );
    println!(
        "Visited positions tail 9: {}",
        nested_rope.knots[nested_rope.knots.len() - 1]
            .visited_positions
            .len()
    );
}

#[cfg(test)]
mod tests {
    use crate::Position;

    #[test]
    fn diff() {
        let pos = Position::default();
        // adjacent
        assert_eq!(pos.diff(Position::new(0, 0)), Position::new(0, 0));
        assert_eq!(pos.diff(Position::new(1, 0)), Position::new(0, 0));
        assert_eq!(pos.diff(Position::new(-1, 0)), Position::new(0, 0));
        assert_eq!(pos.diff(Position::new(0, 1)), Position::new(0, 0));
        assert_eq!(pos.diff(Position::new(0, -1)), Position::new(0, 0));
        assert_eq!(pos.diff(Position::new(1, 1)), Position::new(0, 0));
        assert_eq!(pos.diff(Position::new(-1, -1)), Position::new(0, 0));

        // one direction
        assert_eq!(pos.diff(Position::new(2, 0)), Position::new(1, 0));
        assert_eq!(pos.diff(Position::new(-2, 0)), Position::new(-1, 0));
        assert_eq!(pos.diff(Position::new(0, 2)), Position::new(0, 1));
        assert_eq!(pos.diff(Position::new(0, -2)), Position::new(0, -1));

        // diagonal
        assert_eq!(pos.diff(Position::new(2, 1)), Position::new(1, 1));
        assert_eq!(pos.diff(Position::new(2, -1)), Position::new(1, -1));
        assert_eq!(pos.diff(Position::new(-2, 1)), Position::new(-1, 1));
        assert_eq!(pos.diff(Position::new(1, 2)), Position::new(1, 1));
        assert_eq!(pos.diff(Position::new(1, -2)), Position::new(1, -1));
        assert_eq!(pos.diff(Position::new(-2, 1)), Position::new(-1, 1));
        assert_eq!(pos.diff(Position::new(-2, -1)), Position::new(-1, -1));
    }
}
