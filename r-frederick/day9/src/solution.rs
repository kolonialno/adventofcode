use std::collections::HashSet;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Point(isize, isize);

impl Point {
    fn go(&mut self, movement: Movement) {
        self.0 += movement.0;
        self.1 += movement.1;
    }

    fn difference(&self, other: &Point) -> Movement {
        Movement(other.0 - self.0, other.1 - self.1)
    }

    fn move_with(&mut self, other: &Point) {
        let difference = self.difference(other);
        let new_point = match difference {
            // no movement
            Movement(0, 0) => self.clone(),
            Movement(1, 0) | Movement(0, -1) | Movement(-1, 0) | Movement(0, 1) => self.clone(),
            Movement(1, 1) | Movement(1, -1) | Movement(-1, 1) | Movement(-1, -1) => self.clone(),
            // linear movements
            Movement(x, y) if x >= -2 && x <= 2 && y == 0 => Point(self.0 + x / 2, self.1),
            Movement(x, y) if y >= -2 && y <= 2 && x == 0 => Point(self.0, self.1 + y / 2),
            // diagonal movements
            Movement(2, 1) | Movement(1, 2) | Movement(2, 2) => Point(self.0 + 1, self.1 + 1),
            Movement(2, -1) | Movement(1, -2) | Movement(2, -2) => Point(self.0 + 1, self.1 - 1),
            Movement(-2, -1) | Movement(-1, -2) | Movement(-2, -2) => Point(self.0 - 1, self.1 - 1),
            Movement(-2, 1) | Movement(-1, 2) | Movement(-2, 2) => Point(self.0 - 1, self.1 + 1),
            // invalid movements
            _ => panic!("Oh no! The rope broke with difference of: {:?}", difference),
        };

        self.0 = new_point.0;
        self.1 = new_point.1;
    }
}

#[derive(Clone, Copy, Debug)]
struct Movement(isize, isize);

fn generate_movements(from_str: &str) -> Vec<Movement> {
    if let Some((direction, scalar)) = from_str.split_once(" ") {
        let repeats = scalar.parse::<usize>().unwrap();

        return match direction {
            "R" => [Movement(1isize, 0isize)].repeat(repeats),
            "L" => [Movement(-1isize, 0isize)].repeat(repeats),
            "D" => [Movement(0isize, -1isize)].repeat(repeats),
            "U" => [Movement(0isize, 1isize)].repeat(repeats),
            _ => vec![],
        };
    }

    vec![]
}

pub fn solve(instructions: &Vec<&str>, rope_length: usize) -> usize {
    let mut visited: HashSet<Point> = HashSet::new();
    let mut points = [Point(0, 0)].repeat(rope_length);

    for instruction in instructions.iter() {
        let movements = generate_movements(instruction);
        for movement in movements {
            points[0].go(movement);

            for step in 1..points.len() {
                let sub_head = points[step - 1].clone();
                points[step].move_with(&sub_head);
            }
            visited.insert(points.last().unwrap().clone());
        }
    }

    visited.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve_part1_returns_correct_answer() {
        let input = vec!["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"];

        let expected = 13;

        assert_eq!(solve(&input, 2), expected);
    }

    #[test]
    fn solve_part2_returns_correct_answer() {
        let input = vec!["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"];

        let expected = 1;

        assert_eq!(solve(&input, 10), expected);
    }
}
