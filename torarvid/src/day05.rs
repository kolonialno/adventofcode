use std::collections::HashMap;
use std::str::FromStr;
use crate::util::file_by_lines;

#[derive(Debug)]
struct Line { x1: i32, y1: i32, x2: i32, y2: i32 }

impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(" -> ");
        let mut x = parts.next().unwrap().split(",");
        let mut y = parts.next().unwrap().split(",");
        Ok(Line {
            x1: x.next().unwrap().parse().unwrap(),
            y1: x.next().unwrap().parse().unwrap(),
            x2: y.next().unwrap().parse().unwrap(),
            y2: y.next().unwrap().parse().unwrap()
        })
    }
}

impl Line {
    fn is_straight_line(&self) -> bool {
        self.x1 == self.x2 || self.y1 == self.y2
    }

    fn is_45_degree_line(&self) -> bool {
        let x_diff = (self.x1 - self.x2).abs();
        let y_diff = (self.y1 - self.y2).abs();
        x_diff == y_diff
    }

    fn is_straight_or_45_degree_line(&self) -> bool {
        self.is_45_degree_line() || self.is_straight_line()
    }

    fn mark_coordinates(&self, coords: &mut HashMap<(i32, i32), i32>) {
        let (x_diff, y_diff) = (self.x2 - self.x1, self.y2 - self.y1);
        let x_step = if x_diff == 0 { 0 } else { x_diff / x_diff.abs() };
        let y_step = if y_diff == 0 { 0 } else { y_diff / y_diff.abs() };
        let length = x_diff.abs().max(y_diff.abs());

        for i in 0..length + 1 {
            let (x, y) = (self.x1 + i * x_step, self.y1 + i * y_step);
            *coords.entry((x, y)).or_insert(0) += 1;
        }
    }
}

fn solve(instructions: &Vec<Line>, filter: fn(&Line) -> bool) -> usize {
    let mut map = HashMap::new();
    for line in instructions.iter().filter(|i| filter(&i)) {
        line.mark_coordinates(&mut map)
    }
    map.values().filter(|&v| *v > 1).count()
}

pub fn run() {
    let instructions: Vec<_> = file_by_lines("day05.txt").iter()
        .map(|s| Line::from_str(s).unwrap()).collect();

    println!("Part 1: {}", solve(&instructions, Line::is_straight_line));
    println!("Part 2: {}", solve(&instructions, Line::is_straight_or_45_degree_line));
}
