use std::fs;
use std::str::Lines;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    println!("Part1: {}", solve_part1(content.lines()));
    println!("Part2: {}", solve_part2(content.lines()));
}

enum Direction {
    N = 0,
    E,
    S,
    W,
}

impl From<i32> for Direction {
    fn from(i: i32) -> Self {
        match i {
            0 => Direction::N,
            1 => Direction::E,
            2 => Direction::S,
            3 => Direction::W,
            _ => panic!("Unknown"),
        }
    }
}

fn solve_part1(lines: Lines) -> i32 {
    let mut dir = Direction::E;
    let mut x = 0;
    let mut y = 0;
    for line in lines {
        let chars = line.chars().collect::<Vec<char>>();
        let command = chars[0];
        let amount = chars[1..]
            .iter()
            .collect::<String>()
            .parse::<i32>()
            .unwrap();
        match command {
            'N' => y += amount,
            'E' => x += amount,
            'S' => y -= amount,
            'W' => x -= amount,
            'F' => match dir {
                Direction::N => y += amount,
                Direction::E => x += amount,
                Direction::S => y -= amount,
                Direction::W => x -= amount,
            },
            'R' => dir = Direction::from(((dir as i32) + amount / 90) % 4),
            'L' => dir = Direction::from(((dir as i32) + 4 - amount / 90) % 4),
            _ => panic!(format!("Unknown command {}", command)),
        }
    }
    x.abs() + y.abs()
}

fn solve_part2(lines: Lines) -> i32 {
    let mut ship_x = 0;
    let mut ship_y = 0;
    let mut waypoint_x = 10;
    let mut waypoint_y = 1;
    for line in lines {
        let chars = line.chars().collect::<Vec<char>>();
        let command = chars[0];
        let amount = chars[1..]
            .iter()
            .collect::<String>()
            .parse::<i32>()
            .unwrap();
        match command {
            'N' => waypoint_y += amount,
            'E' => waypoint_x += amount,
            'S' => waypoint_y -= amount,
            'W' => waypoint_x -= amount,
            'F' => {
                ship_x += waypoint_x * amount;
                ship_y += waypoint_y * amount;
            }
            'R' => {
                for _ in (0..amount).step_by(90) {
                    let temp = waypoint_y;
                    waypoint_y = -waypoint_x;
                    waypoint_x = temp;
                }
            }
            'L' => {
                for _ in (0..amount).step_by(90) {
                    let temp = waypoint_x;
                    waypoint_x = -waypoint_y;
                    waypoint_y = temp;
                }
            }
            _ => panic!(format!("Unknown command {}", command)),
        }
    }
    ship_x.abs() + ship_y.abs()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = "F10
N3
F7
R90
F11";
        assert_eq!(solve_part1(test_data.lines()), 25);
        assert_eq!(solve_part2(test_data.lines()), 286);
    }
}
