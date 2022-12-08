use std::{
    cmp::max,
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

use helpers::get_input_file;

fn left_and_top(tree_grid: &[Vec<u8>], visible_trees: &mut HashSet<(usize, usize)>) {
    let mut max_top = vec![0; tree_grid[0].len()];
    let mut max_left = 0;
    for row in 0..tree_grid.len() {
        for col in 0..tree_grid[row].len() {
            let tree = tree_grid[row][col];
            if row == 0 || tree > max_top[col] {
                max_top[col] = tree;
                visible_trees.insert((row, col));
            }
            if col == 0 || tree > max_left {
                max_left = tree;
                visible_trees.insert((row, col));
            }
        }
    }
}

fn right_and_bottom(tree_grid: &[Vec<u8>], visible_trees: &mut HashSet<(usize, usize)>) {
    let mut max_bottom = vec![0; tree_grid[0].len()];
    let mut max_right = 0;
    for row in (0..tree_grid.len()).rev() {
        for col in (0..tree_grid[row].len()).rev() {
            let tree = tree_grid[row][col];
            if row == tree_grid.len() - 1 || tree > max_bottom[col] {
                max_bottom[col] = tree;
                visible_trees.insert((row, col));
            }
            if col == tree_grid[row].len() - 1 || tree > max_right {
                max_right = tree;
                visible_trees.insert((row, col));
            }
        }
    }
}

fn compute_distance(tree_grid: &[Vec<u8>], x: usize, y: usize) -> usize {
    let tree = tree_grid[y][x];
    let mut left = 0;
    let mut top = 0;
    let mut right = 0;
    let mut bottom = 0;
    for col in (0..x).rev() {
        left += 1;
        if tree_grid[y][col] >= tree {
            break;
        }
    }
    for col in (x + 1)..tree_grid[y].len() {
        right += 1;
        if tree_grid[y][col] >= tree {
            break;
        }
    }
    for row in (0..y).rev() {
        top += 1;
        if tree_grid[row][x] >= tree {
            break;
        }
    }
    for row in (y + 1)..tree_grid.len() {
        bottom += 1;
        if tree_grid[row][x] >= tree {
            break;
        }
    }

    top * bottom * left * right
}

fn main() {
    // let file = File::open("08/test.txt").unwrap();
    let file = File::open(get_input_file()).unwrap();
    let lines = std::iter::Iterator::flatten(BufReader::new(file).lines());
    let mut tree_grid: Vec<Vec<u8>> = Vec::default();
    for line in lines {
        tree_grid.push(
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect(),
        );
    }
    let mut visible_trees: HashSet<(usize, usize)> = HashSet::new();
    left_and_top(&tree_grid, &mut visible_trees);
    right_and_bottom(&tree_grid, &mut visible_trees);
    println!("Visible trees: {}", visible_trees.len());

    let mut highest_score = 0;
    for row in 0..tree_grid.len() {
        for col in 0..tree_grid[row].len() {
            highest_score = max(highest_score, compute_distance(&tree_grid, col, row));
        }
    }
    println!("Max score: {}", highest_score);
}
