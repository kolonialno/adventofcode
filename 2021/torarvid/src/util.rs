use std::fs::File;
use std::io::prelude::*;

pub fn file_by_lines_as_i32(filename: &str) -> Vec<i32> {
    let lines = file_by_lines(filename);
    // parse each line into a Vec<i32>
    let mut numbers: Vec<i32> = Vec::new();
    for line in lines {
        if line.len() > 0 {
            numbers.push(line.parse::<i32>().unwrap());
        }
    }
    numbers
}

pub fn file_by_lines(filename: &str) -> Vec<String> {
    // Open input file and read each line
    let mut input = String::new();
    let path = format!("inputs/{}", filename);
    File::open(&path)
        .expect(&format!("Failed to open {}", &path))
        .read_to_string(&mut input)
        .expect(&format!("Failed to read {}", &path));
    // split input into lines of type String
    let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
    lines
}
