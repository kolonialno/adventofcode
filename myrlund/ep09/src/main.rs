use std::{collections::HashSet, io::Error, ops::Sub, str::FromStr};

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl FromStr for Direction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Self::Up),
            "D" => Ok(Self::Down),
            "L" => Ok(Self::Left),
            "R" => Ok(Self::Right),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coordinate(i32, i32);

impl Coordinate {
    fn step_direction(&self, direction: &Direction) -> Self {
        let vec = match direction {
            Direction::Up => Vector2D(0, -1),
            Direction::Down => Vector2D(0, 1),
            Direction::Left => Vector2D(-1, 0),
            Direction::Right => Vector2D(1, 0),
        };
        self.step(&vec)
    }

    fn step(&self, vec: &Vector2D) -> Self {
        Self(self.0 + vec.0, self.1 + vec.1)
    }
}

#[derive(Debug, Clone, Copy)]
struct Vector2D(i32, i32);

impl Vector2D {
    fn dampen_movement(&self) -> Self {
        // If the rope isn't pulled tight, don't move
        if !(self.0.abs() > 1 || self.1.abs() > 1) {
            return Vector2D(0, 0);
        }

        // Otherwise, move both dimensions one step in the vector direction.
        let x = if self.0 != 0 {
            self.0 / self.0.abs()
        } else {
            self.0
        };
        let y = if self.1 != 0 {
            self.1 / self.1.abs()
        } else {
            self.1
        };

        Vector2D(x, y)
    }
}

impl Sub for Coordinate {
    type Output = Vector2D;

    fn sub(self, rhs: Self) -> Self::Output {
        Vector2D(self.0 - rhs.0, self.1 - rhs.1)
    }
}

#[derive(Debug)]
struct Rope {
    head: Coordinate,
    rest: Vec<Coordinate>,
}

impl Rope {
    fn new(initial_coordinate: Coordinate, rope_length: usize) -> Self {
        Self {
            head: initial_coordinate,
            rest: vec![initial_coordinate].repeat(rope_length - 1),
        }
    }

    fn move_head(&mut self, direction: &Direction) -> Coordinate {
        self.head = self.head.step_direction(direction);

        // Build new rope positions
        let mut next_rest = Vec::new();

        let mut prev_knot = self.head;
        for knot in self.rest.iter() {
            let vec = prev_knot - *knot;
            let knot_movement = vec.dampen_movement();
            let next_knot = knot.step(&knot_movement);

            prev_knot = next_knot;
            next_rest.push(next_knot);
        }

        self.rest = next_rest;

        prev_knot
    }
}

fn simulate_rope_movement(s: &str, rope_length: usize) -> usize {
    let operations = s.lines().map(|line| {
        let (dir, steps) = line.split_once(' ').expect("Invalid operation");
        (
            dir.parse::<Direction>().unwrap(),
            steps.parse::<u32>().unwrap(),
        )
    });

    let mut tail_positions = HashSet::new();
    let mut rope = Rope::new(Coordinate(0, 4), rope_length);
    for (direction, steps) in operations {
        for _ in 0..steps {
            let tail = rope.move_head(&direction);
            tail_positions.insert(tail);
        }
    }

    tail_positions.len()
}

fn run_part_one(s: &str) -> usize {
    simulate_rope_movement(s, 2)
}

fn run_part_two(s: &str) -> usize {
    simulate_rope_movement(s, 10)
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE1: &str = include_str!("../sample1.txt");
    static SAMPLE2: &str = include_str!("../sample2.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE1), 13);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE2), 36);
    }
}
