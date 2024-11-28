extern crate nalgebra as na;

use na::{DMatrix, Dynamic, MatrixSlice};

type Matrix = DMatrix<i32>;

fn left(matrix: &Matrix, i: usize, j: usize) -> MatrixSlice<i32, Dynamic, Dynamic> {
    matrix.slice((0, j), (i, 1))
}
fn right(matrix: &Matrix, i: usize, j: usize) -> MatrixSlice<i32, Dynamic, Dynamic> {
    matrix.slice((i + 1, j), (matrix.ncols() - i - 1, 1))
}
fn up(matrix: &Matrix, i: usize, j: usize) -> MatrixSlice<i32, Dynamic, Dynamic> {
    matrix.slice((i, 0), (1, j))
}
fn down(matrix: &Matrix, i: usize, j: usize) -> MatrixSlice<i32, Dynamic, Dynamic> {
    matrix.slice((i, j + 1), (1, matrix.nrows() - j - 1))
}

fn input_to_matrix(string: &str, size: usize) -> Matrix {
    Matrix::from_row_iterator(
        size,
        size,
        string
            .replace('\n', "")
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i32),
    )
}

fn problem_1(matrix: &Matrix) -> i32 {
    let size = matrix.nrows();
    let mut counter = (4 * size - 4) as i32;

    for i in 1..size - 1 {
        for j in 1..size - 1 {
            let element = matrix[(i, j)];

            if element > left(matrix, i, j).max()
                || element > right(matrix, i, j).max()
                || element > up(matrix, i, j).max()
                || element > down(matrix, i, j).max()
            {
                counter += 1;
            }
        }
    }
    counter
}

fn score(slice: MatrixSlice<i32, Dynamic, Dynamic>, element: &i32, reverse: bool) -> i32 {
    let position = if reverse {
        slice.iter().rev().position(|m| m >= &element)
    } else {
        slice.iter().position(|m| m >= &element)
    };

    (match position {
        Some(p) => p + 1,
        None => slice.len(),
    }) as i32
}

fn problem_2(matrix: &Matrix) -> i32 {
    let size = matrix.nrows();
    let mut scenic_score: i32 = 0;

    for i in 0..size {
        for j in 0..size {
            let element = matrix[(i, j)];
            let element_score: i32 = score(left(matrix, i, j), &element, true)
                * score(right(matrix, i, j), &element, false)
                * score(up(matrix, i, j), &element, true)
                * score(down(matrix, i, j), &element, false);

            scenic_score = scenic_score.max(element_score);
        }
    }
    scenic_score
}

fn main() {
    println!("Hello, day 8!");
    let s = include_str!("../input.txt");
    let matrix = input_to_matrix(s, 99);
    println!("Solution to problem 1: {}", problem_1(&matrix));
    println!("Solution to problem 2: {}", problem_2(&matrix));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_problem_1() {
        let s = include_str!("../test_input.txt");
        let matrix = input_to_matrix(s, 5);
        assert_eq!(problem_1(&matrix), 21);
    }

    #[test]
    fn test_problem_2() {
        let s = include_str!("../test_input.txt");
        let matrix = input_to_matrix(s, 5);
        assert_eq!(problem_2(&matrix), 8);
    }
}
