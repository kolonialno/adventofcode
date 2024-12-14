use std::{collections::HashSet, vec};

use crate::utils::{read_to_matrix, rotate_90};

fn solve() {
    let input_map: Vec<Vec<char>> = read_to_matrix("data/6.txt");

    let mut position: (usize, usize) = (0, 0);
    let mut visited_positions: Vec<(usize, usize)> = Vec::new();
    let mut direction: (i32, i32) = (-1, 0); // Up

    // Part 1
    // Get starting point
    if let Some((row_num, row)) = input_map
        .iter()
        .enumerate()
        .find(|(_, row)| row.contains(&'^'))
    {
        position = (row_num, row.iter().position(|&val| val == '^').unwrap())
    }
    visited_positions.push(position);

    // Loop until out of map
    loop {
        let next_position = (
            (position.0 as i32 + direction.0) as usize,
            (position.1 as i32 + direction.1) as usize,
        );
        if stopping_criterion(&input_map, next_position) {
            break;
        } else if input_map[next_position.0][next_position.1] == '#' {
            direction = rotate_90(&direction, true);
        } else {
            visited_positions.push(next_position);
            position = next_position;
        }
    }

    // Find n unique (hash set has n complexity and should be the fastest here)
    let num_unique_positions = visited_positions
        .iter()
        .cloned()
        .collect::<HashSet<_>>()
        .len();

    println!("Part 1, num visited positions: {}", num_unique_positions);

    // Part 2
    let potential_block_positions = visited_positions[1..].to_vec(); // If no block on route, guard will leave
    let mut tested_block_positions: Vec<(usize, usize)> = Vec::new(); // To improve speed, we only test each position once
    let mut block_positions: Vec<(usize, usize)> = Vec::new();

    potential_block_positions
        .iter()
        .enumerate()
        .for_each(|(index, block)| {
            if !tested_block_positions.contains(block) {
                if is_looping_block(&input_map, block, &visited_positions[index]) {
                    block_positions.push(*block);
                }
                tested_block_positions.push(*block);
            }
        });

    let num_unique_block_positions = block_positions
        .iter()
        .cloned()
        .collect::<HashSet<_>>()
        .len();
    println!(
        "Task 2, num possible blocks: {}",
        num_unique_block_positions
    )
}

fn stopping_criterion(input_map: &Vec<Vec<char>>, position: (usize, usize)) -> bool {
    (position.0 >= input_map.len()) | (position.1 >= input_map[0].len())
}

fn is_looping_block(
    input_map: &Vec<Vec<char>>,
    block_position: &(usize, usize),
    from_position: &(usize, usize),
) -> bool {
    // Copy the map and insert the block
    let mut adjusted_input_map = input_map.clone();
    adjusted_input_map[block_position.0][block_position.1] = '#';
    // Infer the direction the path was on (enables "warm start" to improve
    // comp. time), and rotate 90 degrees due to new block
    let mut direction = rotate_90(
        &(
            block_position.0 as i32 - from_position.0 as i32,
            block_position.1 as i32 - from_position.1 as i32,
        ),
        true,
    );
    let mut position = *from_position;
    let mut visited_positions: Vec<(usize, usize)> = Vec::new();
    let mut visited_position_directions: Vec<(i32, i32)> = Vec::new(); // need to check both position and direction to confirm loop

    // Loop until out of map or in inf loop
    loop {
        let mut next_position = (
            (position.0 as i32 + direction.0) as usize,
            (position.1 as i32 + direction.1) as usize,
        );
        if stopping_criterion(&adjusted_input_map, next_position) {
            return false;
        } else if adjusted_input_map[next_position.0][next_position.1] == '#' {
            direction = rotate_90(&direction, true);
            next_position = position;
        }
        // Check if we are in loop
        if visited_positions
            .iter()
            .zip(visited_position_directions.iter())
            .find(|(&p, &d)| (p == next_position) && (d == direction))
            .is_some()
        {
            return true;
        };

        position = next_position;
        visited_positions.push(position);
        visited_position_directions.push(direction);
    }
}

pub fn __main__() {
    solve();
}
