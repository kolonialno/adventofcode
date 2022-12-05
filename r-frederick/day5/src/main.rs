use std::{collections::VecDeque, fs};

use regex::Regex;

mod solution;

fn parse_row(line: &str) -> Vec<Option<&str>> {
    let re = Regex::new(r"\[(?P<label>[A-Z])\]\s?|(\s\s\s\s?)").unwrap();

    re.captures_iter(line)
        .map(|c| match c.name("label") {
            Some(l) => Some(l.as_str()),
            None => None,
        })
        .collect()
}

fn transform(input: &str) -> (Vec<VecDeque<&str>>, Vec<&str>) {
    let (state_block, instructions_block) = input.split_once("\n\n").unwrap();

    let state: Vec<VecDeque<&str>> = state_block.lines().fold(Vec::new(), |mut acc, l| {
        let parsed_line = parse_row(l);

        for (stack_number, c) in parsed_line.iter().enumerate() {
            if acc.len() <= stack_number {
                acc.push(VecDeque::new())
            }

            match c {
                Some(char_str) => acc[stack_number].push_back(char_str),
                None => continue,
            }
        }

        acc
    });

    (state, instructions_block.lines().collect())
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to find input.txt");

    let (result_state, instructions) = transform(&contents);

    println!(
        "answer to part 1: {}",
        solution::solve_part1(&mut result_state.clone(), &instructions)
    );
    println!(
        "answer to part 2: {}",
        solution::solve_part2(&mut result_state.clone(), &instructions)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transform_works() {
        let contents = fs::read_to_string("sample.txt").expect("Unable to find sample.txt");
        let (result_state, _instructions) = transform(&contents);

        let expected_state = vec![
            VecDeque::from(["N", "Z"]),
            VecDeque::from(["D", "C", "M"]),
            VecDeque::from(["P"]),
        ];

        assert_eq!(result_state, expected_state)
    }
}
