use std::env;
use std::fs;
use std::io::{self, BufRead};

enum LineType {
    Number { value: u32 },
    NewLine,
}

fn parse_line(line: String) -> LineType {
    let l = line.parse::<u32>();

    match l {
        Ok(calories) => LineType::Number { value: calories },
        Err(_) => LineType::NewLine,
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    let n = &args[2]
        .parse::<usize>()
        .expect("The second argument should be numeric");

    let file = fs::File::open(file_path).expect("The given file path should be able to open.");
    let reader = io::BufReader::new(file);

    let mut calories_per_elf = reader
        .lines()
        .map(|l| l.expect("All lines in the file should be readable."))
        .fold(vec![0], |mut acc, line| match parse_line(line) {
            LineType::Number { value } => {
                let calories_count = acc.pop().expect("Accumulator array should not be empty");
                acc.push(calories_count + value);
                acc
            }
            LineType::NewLine => {
                acc.push(0);
                acc
            }
        });

    calories_per_elf.sort();

    let sum_calories: u32 = calories_per_elf.iter().rev().take(*n).sum();

    println!("Max calories: {}", sum_calories);
}
