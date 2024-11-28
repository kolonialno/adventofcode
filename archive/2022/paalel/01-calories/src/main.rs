use std::fs::File;
use std::io::{BufRead, BufReader};

fn read_input(filename: String) -> Vec<u32> {
    let file = File::open(filename).expect("No file found!");
    let reader = BufReader::new(file);

    let mut calories: Vec<u32> = Vec::new();
    let mut temp_sum: u32 = 0;

    for line in reader.lines().map(|l| l.expect("Could not parse line!")) {
        match line.parse::<u32>() {
            Ok(meal) => temp_sum += meal,
            Err(_error) => {
                calories.push(temp_sum);
                temp_sum = 0;
            }
        };
    }

    calories.push(temp_sum);
    return calories;
}
fn problem1(calories: &Vec<u32>) -> u32 {
    *calories.iter().max().unwrap()
}

fn problem2(mut calories: Vec<u32>) -> u32 {
    calories.sort_by(|a, b| b.cmp(&a));
    calories[..3].iter().sum()
}

fn main() {
    println!("Hello, day 1!");

    let test_calories = read_input("./test_input.txt".to_string());
    assert!(problem1(&test_calories) == 24000);

    let calories = read_input("./input.txt".to_string());
    println!("Solution to problem 1: {}", problem1(&calories));

    assert!(problem2(test_calories) == 45000);
    println!("Solution to problem 2: {}", problem2(calories));
}
