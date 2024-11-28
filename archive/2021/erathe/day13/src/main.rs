use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    str::FromStr,
};

fn main() {
    let mut paper = include_str!("../input-mats.txt").parse::<Paper>().unwrap();

    //part 1
    if let Some(instruction) = paper.pop_instruction() {
        paper.handle_instruction(instruction);
    }
    println!("{}", paper.count_dots());

    //part 2
    while let Some(instruction) = paper.pop_instruction() {
        paper.handle_instruction(instruction);
    }
    println!("{}", paper);
}

#[derive(Debug)]
struct Paper {
    grid: HashMap<(i32, i32), char>,
    instructions: VecDeque<(char, i32)>,
}

impl Paper {
    fn count_dots(&self) -> usize {
        self.grid.len()
    }

    fn handle_instruction(&mut self, (axis, coord): (char, i32)) {
        for (x, y) in self.get_to_be_moved(axis, coord) {
            self.grid.remove(&(x, y));
            match axis {
                'y' => self.grid.entry((x, coord - (y - coord))).or_insert('#'),
                'x' => self.grid.entry((coord - (x - coord), y)).or_insert('#'),
                _ => &mut 'l',
            };
        }
    }

    fn get_to_be_moved(&mut self, axis: char, coord: i32) -> Vec<(i32, i32)> {
        match axis {
            'y' => self
                .grid
                .keys()
                .map(|(x, y)| (*x, *y))
                .filter(|&(_x, y)| y > coord)
                .collect_vec(),
            'x' => self
                .grid
                .keys()
                .map(|(x, y)| (*x, *y))
                .filter(|&(x, _y)| x > coord)
                .collect_vec(),
            _ => unreachable!(),
        }
    }

    fn pop_instruction(&mut self) -> Option<(char, i32)> {
        self.instructions.pop_front()
    }
}

impl Display for Paper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let xs = self.grid.keys().map(|(x, _)| *x).max().unwrap();
        let ys = self.grid.keys().map(|(_, y)| *y).max().unwrap();

        for row in 0..=ys {
            for col in 0..=xs {
                if let Some(_) = self.grid.get(&(col, row)) {
                    write!(f, "#");
                } else {
                    write!(f, " ");
                }
            }
            write!(f, "\n");
        }
        Ok(())
    }
}

impl FromStr for Paper {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut instructions = VecDeque::new();
        let mut grid = HashMap::new();
        for l in s.lines() {
            if l.starts_with("fold along") {
                let f_i = &l[11..].split("=").collect_vec();
                instructions.push_back((
                    char::from_str(f_i[0]).unwrap(),
                    f_i[1].parse::<i32>().unwrap(),
                ))
            } else if !l.is_empty() {
                let (x, y) = l
                    .split(",")
                    .map(|c| c.parse::<i32>().unwrap())
                    .collect_tuple()
                    .unwrap();
                grid.insert((x, y), '#');
            }
        }
        Ok(Self { grid, instructions })
    }
}
