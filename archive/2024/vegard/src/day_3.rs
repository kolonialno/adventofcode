use core::str;
use std::fs::File;
use std::io::{self, BufRead};
use regex::Regex;

fn solve() {
    let file = File::open("./data/3.txt");
    let reader = io::BufReader::new(file.unwrap());
    let string = reader.lines().next().unwrap().unwrap();

    // Task 1
    let sum_1= find_and_mul(&string);
    println!("Total, task 1 is {:?}", sum_1);

    // Task 2
    let mut sum_2=0;
    // Split the don't sections
    let first_split = string.split("don't()").collect::<Vec<&str>>();

    // First string starts as do()
    sum_2 += find_and_mul(first_split[0]);

    // Go through rest and call find_and_mul after a do()
    for candidates in &first_split[1..] {
        let second_split = candidates.split("do()").collect::<Vec<&str>>();
        for candidate in &second_split[1..] {
            sum_2 += find_and_mul(candidate);
        }
    }

    println!("Total, task 2 is {:?}", sum_2);

}

fn find_and_mul(str: &str) -> i32 {
    let mut sum=0;
    let re = Regex::new(r"mul\(\d+,\d+\)").unwrap();
    for pattern in re.find_iter(&str).map(|m| m.as_str()) {
        sum += mul(pattern);
    }
    return sum;
}

fn mul(str: &str) -> i32 {
    // Assumes a string on the form "mul(x,y)"
    let factors = str.get(0..str.len()-1).unwrap().get(4..).unwrap().split(",").collect::<Vec<&str>>();
    return factors[0].parse::<i32>().unwrap() * factors[1].parse::<i32>().unwrap();
}

pub fn __main__() {
    solve();
}