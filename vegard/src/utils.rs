use std::fs::File;
use std::io::{self, BufRead};

#[allow(dead_code)]
pub fn read_to_lines(filename: &str) -> Vec<String> {
    let file = File::open(filename);
    let reader = io::BufReader::new(file.unwrap());
    let mut lines = Vec::new();

    for line in reader.lines() {
        lines.push(line.unwrap());
    }

    lines
}

#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn read_to_matrix(filename: &str) -> Vec<Vec<char>> {
    let file = File::open(filename);
    let reader = io::BufReader::new(file.unwrap());
    let mut matrix = Vec::new();

    for line in reader.lines() {
        matrix.push(line.unwrap().chars().collect::<Vec<char>>());
    }

    matrix
}

#[allow(dead_code)]
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

pub fn rotate_90(direction: &(i32, i32), clockwise: bool) -> (i32, i32) {
    // Rotate a vector 90 degrees clockwise or counterclockwise
    // The vector must be one of the four cardinal directions
    let steps = [(-1, 0), (0, 1), (1, 0), (0, -1)];

    let offset: isize = if clockwise { 1 } else { -1 };
    steps[(steps.iter().position(|step| step == direction).unwrap() as isize + offset).rem_euclid(4)
        as usize]
}
