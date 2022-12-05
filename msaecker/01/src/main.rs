use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use helpers::get_input_file;

fn main() {
    let input_file = get_input_file().unwrap();
    let file = File::open(input_file).unwrap();
    let lines = BufReader::new(file).lines();

    let mut calories = vec![0];
    let mut elf = 0;

    for line in lines.flatten() {
        if line.is_empty() {
            elf += 1;
            calories.push(0);
        } else {
            calories[elf] += line.parse::<isize>().unwrap();
        }
    }

    calories.sort_unstable();
    println!(
        "Highest calories of single elf: {}",
        calories[calories.len() - 1]
    );

    println!(
        "Highest calories of three elves: {}",
        calories.iter().rev().take(3).sum::<isize>()
    );
}
