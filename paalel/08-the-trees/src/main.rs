extern crate nalgebra as na;
use na::DMatrix;

type Matrix = DMatrix<i32>;

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
            if matrix[(i, j)] > matrix.slice((i, 0), (1, j)).max() // up
                || matrix[(i, j)] > matrix.slice((i, j + 1), (1, size - j - 1)).max() // down
                || matrix[(i, j)] > matrix.slice((0, j), (i, 1)).max() // left
                || matrix[(i, j)] > matrix.slice((i + 1, j), (size - i - 1, 1)).max()
            // right
            {
                counter += 1;
            }
        }
    }
    counter
}

fn problem_2(matrix: &Matrix) -> i32 {
    let size = matrix.nrows();
    let mut scenic_score: i32 = 0;
    for i in 0..size {
        for j in 0..size {
            let element = matrix[(i, j)];
            let mut element_score: i32 = 1;

            // left
            element_score *= match matrix
                .slice((i, 0), (1, j))
                .iter()
                .rev()
                .position(|m| m >= &element)
            {
                Some(p) => p + 1,
                None => j,
            } as i32;

            // up
            element_score *= match matrix
                .slice((0, j), (i, 1))
                .iter()
                .rev()
                .position(|m| m >= &element)
            {
                Some(p) => p + 1,
                None => i,
            } as i32;

            // down
            element_score *= match matrix
                .slice((i, j + 1), (1, size - j - 1))
                .iter()
                .position(|m| m >= &element)
            {
                Some(p) => p + 1,
                None => size - j - 1,
            } as i32;

            // right
            element_score *= match matrix
                .slice((i + 1, j), (size - i - 1, 1))
                .iter()
                .position(|m| m >= &element)
            {
                Some(p) => p + 1,
                None => size - i - 1,
            } as i32;

            scenic_score = if element_score > scenic_score {
                element_score
            } else {
                scenic_score
            };
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
