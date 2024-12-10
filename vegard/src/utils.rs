use std::fs::File;
use std::io::{self, BufRead};

pub fn read_to_lines(filename: &str) -> Vec<String> {
    let file = File::open(filename);
    let reader = io::BufReader::new(file.unwrap());
    let mut lines = Vec::new();

    for line in reader.lines() {
        lines.push(line.unwrap());
    }

    lines
}

pub fn read_to_int_vectors(filename: &str, separator: &str) -> Vec<Vec<i32>> {
    read_to_lines(filename)
        .iter()
        .map(|line| {
            line.split(separator)
                .map(|x| x.parse::<i32>().unwrap())
                .collect()
        })
        .collect()
}

pub fn read_to_matrix(filename: &str) -> Vec<Vec<char>> {
    let file = File::open(filename);
    let reader = io::BufReader::new(file.unwrap());
    let mut matrix = Vec::new();

    for line in reader.lines() {
        matrix.push(line.unwrap().chars().collect::<Vec<char>>());
    }

    matrix
}

pub fn check_string_match_in_matrix(
    matrix: &Vec<Vec<char>>,
    row_index: usize,
    col_index: usize,
    letter: &char,
) -> bool {
    // Checks if an the element of a char matrix is equal to a given char
    // Returns false instead of panic if the index is out of bounds
    let mut is_match = true;
    match matrix.get(row_index) {
        Some(row) => match row.get(col_index) {
            Some(_) => {
                if matrix[row_index][col_index] != *letter {
                    is_match = false;
                }
            }
            None => {
                is_match = false;
            }
        },
        None => {
            is_match = false;
        }
    }

    is_match
}
