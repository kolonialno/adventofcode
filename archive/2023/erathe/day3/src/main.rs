use itertools::Itertools;
use regex::Regex;
use std::{str::FromStr, time::Instant};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref NUMBERS: Regex = Regex::new(r#"\d+"#).unwrap();
}

const DIRS: [(isize, isize); 8] = [
    (-1, 0),
    (1, 0),
    (0, 1),
    (0, -1),
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1),
];

fn main() {
    let input = include_str!("../input.txt");

    let t1 = Instant::now();
    println!("part 1: {:?} in {:?}", part1(input), t1.elapsed());
    let t2 = Instant::now();
    println!("part 2: {:?} in {:?}", part2(input), t2.elapsed());
}

fn part1(input: &str) -> Option<u32> {
    Some(input.parse::<Grid>().unwrap().sum_numbers_with_symbols())
}

fn part2(input: &str) -> Option<u32> {
    Some(input.parse::<Grid>().unwrap().sum_cogs())
}

struct NumberPosition {
    x_start: isize,
    x_end: isize,
    y: isize,
}

struct SymbolPosition {
    x: isize,
    y: isize,
}

struct PartNumber {
    part_number: u32,
    position: NumberPosition,
}

struct Symbol {
    symbol_char: char,
    position: SymbolPosition,
}

struct Grid {
    part_numbers: Vec<PartNumber>,
    symbols: Vec<Symbol>,
}

impl Grid {
    fn sum_numbers_with_symbols(self) -> u32 {
        self.part_numbers
            .iter()
            .filter(|num| {
                for x in num.position.x_start..=num.position.x_end {
                    for (dx, dy) in DIRS {
                        if self
                            .symbols
                            .iter()
                            .any(|s| s.position.x == x + dx && s.position.y == num.position.y + dy)
                        {
                            return true;
                        }
                    }
                }
                false
            })
            .map(|n| n.part_number)
            .sum()
    }

    fn sum_cogs(self) -> u32 {
        self.symbols
            .iter()
            .filter(|s| s.symbol_char == '*')
            .map(|s| {
                self.part_numbers
                    .iter()
                    .filter(|p_number| {
                        for (dx, dy) in DIRS {
                            let r = p_number.position.x_start..=p_number.position.x_end;
                            if r.contains(&(s.position.x + dx))
                                && s.position.y + dy == p_number.position.y
                            {
                                return true;
                            }
                        }
                        false
                    })
                    .map(|p_number| p_number.part_number)
                    .collect_vec()
            })
            .filter(|parts| parts.len() == 2)
            .map(|parts| parts.iter().product::<u32>())
            .sum()
    }
}

impl FromStr for Grid {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut grid: Grid = Grid {
            part_numbers: Vec::new(),
            symbols: Vec::new(),
        };
        let lines = s
            .lines()
            .map(|line| line.chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        let mut st = String::new();
        let mut capturing = false;
        let mut start = 0;
        for y in 0..lines.len() {
            for x in 0..lines[y].len() {
                let c = lines[y][x];
                if c.is_digit(10) {
                    if !capturing {
                        capturing = true;
                        start = x;
                    }
                    st.push(c);
                    // if we are at end of line or next index is not number
                    if x == lines[y].len() - 1 || !lines[y][x + 1].is_digit(10) {
                        capturing = false;
                        grid.part_numbers.push(PartNumber {
                            part_number: st.parse::<u32>().unwrap(),
                            position: NumberPosition {
                                x_start: start as isize,
                                x_end: x as isize,
                                y: y as isize,
                            },
                        });
                        st.clear();
                    }
                } else if c != '.' {
                    grid.symbols.push(Symbol {
                        symbol_char: c,
                        position: SymbolPosition {
                            x: x as isize,
                            y: y as isize,
                        },
                    })
                }
            }
        }
        Ok(grid)
    }
}
