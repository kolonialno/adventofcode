use crate::utils::read_to_matrix;
use itertools::Itertools;
use std::collections::HashSet;

fn solve() {
    let input_map: Vec<Vec<char>> = read_to_matrix("data/8.txt");

    // Get symbol coordinates
    let mut symbol_coordinates: Vec<(char, usize, usize)> = Vec::new();
    input_map.iter().enumerate().for_each(|(row_index, row)| {
        row.iter()
            .enumerate()
            .filter(|(_, &symbol)| symbol != '.')
            .for_each(|(col_index, &symbol)| {
                symbol_coordinates.push((symbol, row_index, col_index));
            })
    });

    // Get unique symbols
    let unique_symbols: HashSet<char> = symbol_coordinates.iter().map(|x| x.0).collect();

    // Find all antinodes (incl duplicates and out of boudns)
    let mut antinodes: Vec<(usize, usize)> = Vec::new();
    let mut antinodes_2: Vec<(usize, usize)> = Vec::new();
    for symbol in unique_symbols {
        for pairs in symbol_coordinates
            .iter()
            .filter(|x| x.0 == symbol)
            .combinations(2)
            .map(|x| ((x[0].1, x[0].2), (x[1].1, x[1].2)))
        {
            antinodes.append(&mut find_antinodes(pairs.0, pairs.1));
            antinodes_2.append(&mut find_antinodes_2(pairs.0, pairs.1, &input_map));
        }
    }
    // Validate antinodes
    let valid_antinodes: HashSet<(usize, usize)> = antinodes
        .iter()
        .filter(|node| (node.0 < input_map.len()) & (node.1 < input_map[0].len()))
        .copied()
        .collect();
    let valid_antinodes_2: HashSet<(usize, usize)> = antinodes_2
        .iter()
        .filter(|node| (node.0 < input_map.len()) & (node.1 < input_map[0].len()))
        .copied()
        .collect();

    println!("Num antinodes: {:?}", valid_antinodes.len());
    println!("Num antinodes 2: {:?}", valid_antinodes_2.len());
}

fn find_antinodes(a: (usize, usize), b: (usize, usize)) -> Vec<(usize, usize)> {
    let mut antinodes: Vec<(usize, usize)> = Vec::new();
    if (2 * a.0 >= b.0) & (2 * a.1 >= b.1) {
        antinodes.push((2 * a.0 - b.0, 2 * a.1 - b.1))
    }
    if (2 * b.0 >= a.0) & (2 * b.1 >= a.1) {
        antinodes.push((2 * b.0 - a.0, 2 * b.1 - a.1))
    }

    antinodes
}

fn find_antinodes_2(
    a: (usize, usize),
    b: (usize, usize),
    input_map: &Vec<Vec<char>>,
) -> Vec<(usize, usize)> {
    let mut antinodes: Vec<(usize, usize)> = Vec::new();

    for row in 0..input_map.len() {
        if let Ok(col)= get_col(row, a, b) {
            antinodes.push((row, col))
        }
    }

    antinodes
}

fn get_col(row: usize, a: (usize, usize),b: (usize, usize)) -> Result<usize, &'static str> {
    // Get the column that makes the (row,col) point in line with a and b if it
    // is one the same line and within the bounds of the map
    // Using algebra, we have col = (b.1-a.1)*(row-a.0)/(b.0-a.0) + a.1
    let numerator = (b.1 as isize - a.1 as isize) * (row as isize - a.0 as isize);
    let denominator = b.0 as isize - a.0 as isize;
    if denominator == 0 {
        return Err("Denominator is zero");
    } else if numerator % denominator != 0 {
        return Err("Numerator is not divisible by denominator");
    } else {
        let col =numerator / denominator + a.1 as isize;
        if col < 0 {
            return Err("Column is negative");
        } else {
            return Ok(col as usize);
        }
    }
}


pub fn __main__() {
    solve();
}
