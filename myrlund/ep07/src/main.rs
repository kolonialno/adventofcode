use std::{collections::HashMap, str::FromStr};

use anyhow::{Context, Error};

#[derive(Debug)]
struct Term {
    current_dir: Vec<String>,
    dirs: HashMap<String, i32>,
}

impl FromStr for Term {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut term = Term::new();
        for line in s.lines() {
            if let Some(dir) = line.strip_prefix("$ cd ") {
                term.cd(dir);
            } else if line.starts_with("dir ") || line.starts_with("$ ls") {
            } else {
                let (size, filename) = line.split_once(' ').unwrap();
                term.index_file(
                    filename,
                    size.parse().context("Size is not a valid number")?,
                );
            }
        }
        Ok(term)
    }
}

impl Term {
    fn new() -> Self {
        Self {
            current_dir: Vec::new(),
            dirs: HashMap::new(),
        }
    }

    fn cd(&mut self, target: &str) {
        if target == ".." {
            self.current_dir.pop();
        } else {
            self.current_dir.push(target.to_owned());
        }
    }

    fn current_paths(&self) -> Vec<String> {
        let mut paths = Vec::new();
        for (i, dir) in self.current_dir.iter().enumerate() {
            let base_path = self.current_dir[0..i].join("/");
            paths.push(base_path + "/" + dir);
        }
        paths
    }

    fn index_file(&mut self, _filename: &str, size: i32) {
        for dir in self.current_paths() {
            *self.dirs.entry(dir.to_owned()).or_default() += size;
        }
    }

    fn used_space(&self) -> i32 {
        *self.dirs.values().max().unwrap()
    }

    fn dir_sizes(self) -> impl Iterator<Item = i32> {
        self.dirs.into_values()
    }
}

fn run_part_one(input: &str) -> i32 {
    let term: Term = input.parse().unwrap();

    term.dir_sizes().filter(|&size| size <= 100000).sum()
}

fn run_part_two(input: &str) -> i32 {
    let term: Term = input.parse().unwrap();

    let total_size = 70000000;
    let needed_free_space = 30000000;

    let root_size = term.used_space();
    let current_free_space = total_size - root_size;
    let missing_free_space = needed_free_space - current_free_space;

    term.dir_sizes()
        .filter(|&size| size >= missing_free_space)
        .min()
        .unwrap()
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

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 95437);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 24933642);
    }
}
