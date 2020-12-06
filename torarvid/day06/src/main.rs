use std::format;
use std::fs;
use std::str::Lines;

fn main() {
    let filename = "input.txt";
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file {}", filename));
    println!("Part1: {}", solve_part1(contents.lines()));
    println!("Part2: {}", solve_part2(contents.lines()));
}

fn solve_part1(input: Lines) -> usize {
    let mut group_yesses = String::from("");
    let mut total_yesses = 0;
    for line in input {
        if line.chars().count() == 0 {
            total_yesses += group_yesses.chars().count();
            group_yesses = String::from("");
            continue;
        }
        for ch in line.chars() {
            if !group_yesses.contains(ch) {
                group_yesses.push(ch);
            }
        }
    }
    total_yesses += group_yesses.chars().count();
    total_yesses
}

fn solve_part2(input: Lines) -> usize {
    let mut group_yesses = String::from("abcdefghijklmnopqrstuvwxyz");
    let mut total_yesses = 0;
    for line in input {
        if line.chars().count() == 0 {
            total_yesses += group_yesses.chars().count();
            group_yesses = String::from("abcdefghijklmnopqrstuvwxyz");
            continue;
        }
        for ch in group_yesses.chars().collect::<Vec<char>>() {
            if !line.contains(ch) {
                group_yesses = group_yesses.replace(ch, "");
            }
        }
    }
    total_yesses += group_yesses.chars().count();
    total_yesses
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let test_data = String::from(
            "abc

a
b
c

ab
ac

a
a
a
a

b",
        );
        assert_eq!(solve_part1(test_data.lines()), 11);
        assert_eq!(solve_part2(test_data.lines()), 6);
    }
}
