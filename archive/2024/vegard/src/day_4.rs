use crate::utils::check_string_match_in_matrix;
use crate::utils::read_to_matrix;

fn solve() {
    let matrix = read_to_matrix("data/4.txt");

    // Part 1
    let mut matches = 0;
    for (i, row) in matrix.iter().enumerate() {
        for (j, _col) in row.iter().enumerate() {
            if matrix[i][j] == 'X' {
                matches += find_matches_1(&matrix, i, j,&"XMAS");
            }
        }
    }

    println!("Matches pt 1: {}", matches);

    // Part 2
    let mut matches = 0;
    for (i, row) in matrix.iter().enumerate() {
        for (j, _col) in row.iter().enumerate() {
            if matrix[i][j] == 'A' {
                if is_match_2(&matrix, i, j,&"MAS") {
                    matches += 1;
                }
            }
        }
    }
    println!("Matches pt 2: {}", matches);
}

fn is_match_2(matrix: &Vec<Vec<char>>, i: usize, j: usize, pattern: &str) -> bool {
    let pattern_chars: Vec<char> = pattern.chars().collect();

    // Check if element is on the border of the matrix
    if i==0 || j==0 || i==matrix.len()-1 || j==matrix[0].len()-1 {
        return false;
    }

    if
        (
            // Check match from top left to bottom right
            (
                check_string_match_in_matrix(&matrix, i+1, j+1, &pattern_chars[0])
                & check_string_match_in_matrix(&matrix, i-1, j-1, &pattern_chars[2])
            ) |
            (
                check_string_match_in_matrix(&matrix, i+1, j+1, &pattern_chars[2])
                & check_string_match_in_matrix(&matrix, i-1, j-1, &pattern_chars[0])
            )
        ) &
        (
            // Check match from top right to bottom left
            (
                check_string_match_in_matrix(&matrix, i+1, j-1, &pattern_chars[0])
                & check_string_match_in_matrix(&matrix, i-1, j+1, &pattern_chars[2])
            ) |
            (
                check_string_match_in_matrix(&matrix, i+1, j-1, &pattern_chars[2])
                & check_string_match_in_matrix(&matrix, i-1, j+1, &pattern_chars[0])
            )
        )
    {
        return true;
    } else {
        return false;
    }
}

fn find_matches_1(matrix: &Vec<Vec<char>>, i: usize, j: usize, pattern: &str) -> i32 {
    // Checks for pattern in all directions from element i,j in the matrix
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut matches = 0;
    let directions = [
        (0,1), // Right
        (0,-1), // Left
        (1,0), // Down
        (-1,0), // Up
        (1,1), // Down Right
        (1,-1), // Down Left
        (-1,1), // Up Right
        (-1,-1), // Up Left
        ];

    for direction in directions.iter() {
        let direction_row = direction.0;
        let direction_col = direction.1;
        if check_match_in_direction(&matrix, i, j, direction_row, direction_col, &pattern_chars) {
            matches += 1;
        }
    }

    matches
}

fn check_match_in_direction(matrix: &Vec<Vec<char>>, i: usize, j: usize, direction_row: isize, direction_col: isize, pattern_chars: &Vec<char>) -> bool {
    let mut is_match_current = true;
    if
        ((i as isize) + direction_row*(pattern_chars.len() as isize) < -1)
        | ((j as isize) + direction_col*(pattern_chars.len() as isize) < -1)
    {
        return false;
    }
    for (k, letter) in pattern_chars.iter().enumerate() {
        let row_index = (i as isize+(direction_row*k as isize)) as usize;
        let col_index = (j as isize+(direction_col*k as isize)) as usize;
        if check_string_match_in_matrix(&matrix, row_index, col_index, letter) {
            continue;
        } else {
            is_match_current = false;
            break;
        }
    }
    is_match_current
}

pub fn __main__() {
    solve();
}