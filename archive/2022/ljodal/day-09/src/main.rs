use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Pos {
    x: isize,
    y: isize,
}

impl Pos {
    fn is_touching(&self, other: &Pos) -> bool {
        let y_diff = self.y.abs_diff(other.y);
        let x_diff = self.x.abs_diff(other.x);

        x_diff == 0 && y_diff < 2 || y_diff == 0 && x_diff < 2 || x_diff == 1 && y_diff == 1
    }

    fn move_tail(&self, tail: Pos) -> Pos {
        if tail.is_touching(&self) {
            return tail;
        }

        if self.x == tail.x {
            // On same row, move up/down
            let y = if self.y > tail.y {
                tail.y + 1
            } else {
                tail.y - 1
            };
            Pos { x: self.x, y }
        } else if self.y == tail.y {
            // In same column, move left/right
            let x = if self.x > tail.x {
                tail.x + 1
            } else {
                tail.x - 1
            };
            Pos { x, y: self.y }
        } else {
            // Move diagonally
            let x = if self.x > tail.x {
                tail.x + 1
            } else {
                tail.x - 1
            };
            let y = if self.y > tail.y {
                tail.y + 1
            } else {
                tail.y - 1
            };
            Pos { x, y }
        }
    }
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn move_head(&self, head: Pos) -> Pos {
        match self {
            Direction::Up => Pos {
                x: head.x,
                y: head.y + 1,
            },
            Direction::Down => Pos {
                x: head.x,
                y: head.y - 1,
            },
            Direction::Left => Pos {
                x: head.x - 1,
                y: head.y,
            },
            Direction::Right => Pos {
                x: head.x + 1,
                y: head.y,
            },
        }
    }
}

#[derive(Debug)]
struct Move {
    direction: Direction,
    count: usize,
}

fn parse_moves(input: &str) -> Vec<Move> {
    input
        .lines()
        .map(|line| {
            let (dir, count) = line.split_once(" ").expect("Invalid move");
            let count: usize = count.parse().expect("Invalid move count");

            let direction = match dir {
                "U" => Direction::Up,
                "D" => Direction::Down,
                "L" => Direction::Left,
                "R" => Direction::Right,
                _ => panic!("Invalid move direction"),
            };

            Move { direction, count }
        })
        .collect()
}

fn solve_part1(input: &str) -> usize {
    let moves = parse_moves(input);

    let (mut head, mut tail): (Pos, Pos) = (Pos { x: 0, y: 0 }, Pos { x: 0, y: 0 });
    let mut tail_positions: HashSet<Pos> = HashSet::new();
    tail_positions.insert(tail);

    for m in moves.iter() {
        for _ in 0..m.count {
            head = m.direction.move_head(head);
            tail = head.move_tail(tail);
            assert!(
                tail.is_touching(&head),
                "Tail should be touching head after move"
            );
            tail_positions.insert(tail);
        }
    }

    tail_positions.len()
}

fn solve_part2(input: &str) -> usize {
    let moves = parse_moves(input);

    let mut positions: [Pos; 10] = [Pos { x: 0, y: 0 }; 10];

    let mut tail_positions: HashSet<Pos> = HashSet::new();
    tail_positions.insert(positions[9]);

    for m in moves.iter() {
        for _ in 0..m.count {
            let head = m.direction.move_head(positions[0]);
            positions[0] = head;
            for i in 1..10 {
                let pos = positions[i - 1].move_tail(positions[i]);
                positions[i] = pos;
            }
            tail_positions.insert(positions[9]);
        }
    }

    tail_positions.len()
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pos() {
        let pos_0x0 = Pos { x: 0, y: 0 };
        let pos_0x1 = Pos { x: 0, y: 1 };
        let pos_0x2 = Pos { x: 0, y: 2 };
        let pos_0x3 = Pos { x: 0, y: 3 };
        let pos_1x1 = Pos { x: 1, y: 1 };
        let pos_1x2 = Pos { x: 1, y: 2 };
        let pos_2x2 = Pos { x: 2, y: 2 };
        assert!(pos_0x0.is_touching(&pos_0x0));
        assert!(pos_0x0.is_touching(&pos_0x1));
        assert!(!pos_0x0.is_touching(&pos_0x2));
        assert!(!pos_0x0.is_touching(&pos_0x3));
        assert!(pos_0x0.is_touching(&pos_1x1));
        assert!(!pos_0x0.is_touching(&pos_1x2));
        assert!(!pos_0x0.is_touching(&pos_0x2));
        assert!(!pos_0x0.is_touching(&pos_2x2));
        assert!(!pos_0x2.is_touching(&pos_2x2));
    }

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 13, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2_small() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 1, "Wrong result for pt. 2");
    }

    #[test]
    fn test_part2_large() {
        let input = include_str!("./test_2.txt");
        assert_eq!(solve_part2(input), 36, "Wrong result for pt. 2");
    }
}
