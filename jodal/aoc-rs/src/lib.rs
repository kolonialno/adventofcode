pub mod day01;
pub mod day02;
pub mod day03;
pub mod day04;

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn read_lines<P>(filename: P) -> io::Result<Vec<String>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .collect())
}

pub fn read_numbers<P>(filename: P) -> io::Result<Vec<i32>>
where
    P: AsRef<Path>,
{
    Ok(read_lines(filename)?
        .iter()
        .map(|line| line.parse().expect("Failed parsing number"))
        .collect())
}
