use std::{
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader},
};

use helpers::get_input_file;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Op {
    value: isize,
    cycle_length: usize,
}

impl Op {
    fn noop() -> Self {
        Self {
            value: 0,
            cycle_length: 1,
        }
    }

    fn parse(input: &str) -> Self {
        if input == "noop" {
            Self::noop()
        } else {
            let (_, value) = input.split_once(' ').unwrap();
            Self {
                value: value.parse::<isize>().unwrap(),
                cycle_length: 2,
            }
        }
    }
}

struct Crt {
    rows: Vec<Vec<char>>,
}

impl Crt {
    fn new(rows: usize, cols: usize) -> Self {
        Self {
            rows: vec![vec!['.'; cols]; rows],
        }
    }

    fn draw(&mut self, cycle: isize, register: isize) {
        let row = (cycle - 1) as usize / self.rows[0].len();
        let col = (cycle - 1) as usize % self.rows[0].len();
        if register.abs_diff(col as isize) <= 1 {
            self.rows[row][col] = '#';
        }
    }
}

impl Display for Crt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = Ok(());
        for row in self.rows.iter() {
            res = writeln!(f, "{}", row.iter().collect::<String>());
        }
        res
    }
}

fn main() {
    let file = File::open(get_input_file()).unwrap();
    // let file = File::open("10/test.txt").unwrap();
    let lines = std::iter::Iterator::flatten(BufReader::new(file).lines());
    let mut register: isize = 1;
    let mut cycle: isize = 0;
    let mut score = 0;
    let mut op;
    let mut crt = Crt::new(6, 40);
    for line in lines {
        op = Op::parse(line.as_str());
        for _ in 0..op.cycle_length - 1 {
            cycle += 1;
            crt.draw(cycle, register);
            if cycle == 19 || cycle > 19 && (cycle - 19) % 40 == 0 {
                score += register * (cycle + 1);
            }
        }
        cycle += 1;
        crt.draw(cycle, register);
        register += op.value;
        if cycle == 19 || cycle > 19 && (cycle - 19) % 40 == 0 {
            score += register * (cycle + 1);
        }
    }
    println!("Score: {}", score);
    println!("{}", crt);
}
