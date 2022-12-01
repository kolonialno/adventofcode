use std::fs::File;
use std::io::{BufRead, BufReader};

struct Elf {
    meals: Vec<u32>,
}

impl Elf {
    fn sum(&self) -> u32 {
        self.meals.iter().sum()
    }
}

fn read_input(filename: String) -> Vec<Elf> {
    let file = File::open(filename).expect("No file found!");
    let reader = BufReader::new(file);

    let mut elves: Vec<Elf> = Vec::new();
    let mut elf = Elf { meals: Vec::new() };

    for line in reader.lines().map(|l| l.expect("Could not parse line!")) {
        match line.parse::<u32>() {
            Ok(meal) => elf.meals.push(meal),
            Err(_error) => {
                elves.push(elf);
                elf = Elf { meals: Vec::new() }
            }
        };
    }

    elves.push(elf);
    return elves;
}
fn problem1(elves: &Vec<Elf>) -> u32 {
    elves.iter().map(|e| e.sum()).max().unwrap()
}

fn problem2(mut elves: Vec<Elf>) -> u32 {
    elves.sort_by(|a, b| b.sum().cmp(&a.sum()));
    elves.iter().map(|e| e.sum()).collect::<Vec<u32>>()[..3]
        .iter()
        .sum()
}

fn main() {
    println!("Hello, day 1!");

    let test_elves = read_input("../test_input.txt".to_string());
    assert!(problem1(&test_elves) == 24000);

    let elves = read_input("../input.txt".to_string());
    println!("Solution to problem 1: {}", problem1(&elves));

    assert!(problem2(test_elves) == 45000);
    println!("Solution to problem 2: {}", problem2(elves));
}
