use std::{ops::Add, str::FromStr};

use crate::util::file_by_lines;

pub fn run() {
    let numbers = file_by_lines("day18.txt")
        .iter()
        .map(|line| line.parse::<Number>().unwrap())
        .collect::<Vec<_>>();
    let mut sum = numbers[0].clone();
    for number in numbers.iter().skip(1) {
        sum = sum + number;
    }
    println!("Part 1: {}", sum.magnitude());

    let mut max_mag = 0;
    for (i, a) in numbers.iter().enumerate() {
        for (j, b) in numbers.iter().enumerate() {
            if j == i {
                continue;
            }
            let sum = a.clone() + b;
            let mag = sum.magnitude();
            if mag > max_mag {
                max_mag = mag;
            }
        }
    }
    println!("Part 2: {}", max_mag);
}

#[derive(Debug, Clone)]
struct Number {
    values: Vec<u64>,
    levels: Vec<isize>,
}

impl FromStr for Number {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut number = Number {
            values: Vec::new(),
            levels: Vec::new(),
        };
        let mut level = 0;
        for char in s.chars() {
            match char {
                '[' => level += 1,
                ']' => level -= 1,
                ',' => {}
                d => {
                    number.values.push(d.to_digit(10).unwrap().into());
                    number.levels.push(level);
                }
            }
        }
        Ok(number)
    }
}

impl Number {
    fn explode(&mut self) -> bool {
        for i in 0..self.levels.len() - 1 {
            if self.levels[i] <= 4 {
                continue;
            }
            let left_val = self.values[i];
            let right_val = self.values[i + 1];
            if i > 0 {
                self.values[i - 1] += left_val;
            }
            if i < self.values.len() - 2 {
                self.values[i + 2] += right_val;
            }
            self.values.remove(i);
            self.levels.remove(i);
            self.values[i] = 0;
            self.levels[i] -= 1;
            return true;
        }
        false
    }

    fn split(&mut self) -> bool {
        for i in 0..self.values.len() {
            if self.values[i] < 10 {
                continue;
            }
            let left = (self.values[i] / 2) as u64;
            let right = ((self.values[i] + 1) / 2) as u64;
            self.values.insert(i + 1, right);
            self.values[i] = left;
            self.levels.insert(i + 1, self.levels[i] + 1);
            self.levels[i] += 1;
            return true;
        }
        false
    }

    fn reduce(&mut self) {
        while self.explode() || self.split() {}
    }

    fn magnitude(&self) -> u64 {
        let mut v = self.values.clone();
        let mut l = self.levels.clone();
        while v.len() > 1 {
            let max_i = l.iter().enumerate().max_by_key(|&(_, &x)| x).unwrap().0;
            let mag = 3 * v[max_i - 1] + 2 * v[max_i];
            v[max_i - 1] = mag;
            l[max_i - 1] -= 1;
            v.remove(max_i);
            l.remove(max_i);
        }
        v[0]
    }
}

impl Add<&Number> for Number {
    type Output = Number;

    fn add(self, other: &Number) -> Self::Output {
        let mut new_values = self.values.clone();
        new_values.extend(other.values.clone());
        let mut new_levels = self.levels.clone();
        new_levels.extend(other.levels.clone());
        new_levels.iter_mut().for_each(|x| *x += 1);
        let mut sum = Number {
            values: new_values,
            levels: new_levels,
        };
        sum.reduce();
        sum
    }
}
