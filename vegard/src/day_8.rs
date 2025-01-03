use crate::utils::read_to_matrix;
use itertools::Itertools;
use std::{
    array,
    collections::{HashMap, HashSet},
};

fn solve() {
    let input_map: Vec<Vec<char>> = read_to_matrix("data/8_demo.txt");

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
    for symbol in unique_symbols {
        for pairs in symbol_coordinates
            .iter()
            .filter(|x| x.0 == symbol)
            .combinations(2)
            .map(|x| ((x[0].1, x[0].2), (x[1].1, x[1].2)))
        {
            antinodes.append(&mut find_antinodes(pairs.0, pairs.1))
        }
    }
    // Validate antinodes
    let valid_antinodes: HashSet<(usize, usize)> = antinodes
        .iter()
        .filter(|node| (node.0 < input_map.len()) & (node.1 < input_map[0].len()))
        .copied()
        .collect();

    println!("Antinodes: {:?}", valid_antinodes);
    println!("Num antinodes: {:?}", valid_antinodes.len());
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

pub fn __main__() {
    solve();
}
