use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<Vec<Vec<u32>>, Error> {
    Ok(BufReader::new(io)
        .lines()
        .map(|line| {
            line.unwrap()
                .trim()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect()
        })
        .collect())
}

fn get_adjacent(map: &Vec<Vec<u32>>, coord: [usize; 2]) -> Vec<[usize; 2]> {
    let mut ans_vec = Vec::new();
    let x_start = if coord[1] > 0 { coord[1] - 1 } else { 0 };
    let x_end = if coord[1] + 1 < map[0].len() {
        coord[1] + 2
    } else {
        coord[1] + 1
    };
    if coord[0] > 0 {
        for x in x_start..x_end {
            ans_vec.push([coord[0] - 1, x])
        }
    }
    for x in x_start..x_end {
        ans_vec.push([coord[0], x])
    }
    if coord[0] + 1 < map.len() {
        for x in x_start..x_end {
            ans_vec.push([coord[0] + 1, x])
        }
    }
    return ans_vec;
}

fn flash_adjacent(levels: &mut Vec<Vec<u32>>, flashes: &mut u32, coord: [usize; 2]) {
    for a_coord in get_adjacent(levels, coord) {
        if levels[a_coord[0]][a_coord[1]] == 10 {
            *flashes += 1;
            levels[a_coord[0]][a_coord[1]] = 11;
            flash_adjacent(levels, flashes, a_coord)
        } else if levels[a_coord[0]][a_coord[1]] < 10 {
            levels[a_coord[0]][a_coord[1]] += 1;
        }
    }
}

fn step(levels: &mut Vec<Vec<u32>>) -> u32 {
    let mut flashes = 0;
    for i in 0..levels.len() {
        for ii in 0..levels[0].len() {
            levels[i][ii] += 1;
        }
    }

    let mut cur_flashes = 1;
    while flashes != cur_flashes {
        cur_flashes = flashes;
        for i in 0..levels.len() {
            for ii in 0..levels[0].len() {
                if levels[i][ii] == 10 {
                    flashes += 1;
                    levels[i][ii] = 11;
                    flash_adjacent(levels, &mut flashes, [i, ii])
                }
            }
        }
    }

    for i in 0..levels.len() {
        for ii in 0..levels[0].len() {
            if levels[i][ii] == 11 {
                levels[i][ii] = 0;
            }
        }
    }
    flashes
}

pub fn day11() {
    let mut levels = read(File::open("inputs/day11.txt").unwrap()).unwrap();
    for level_line in levels.iter() {
        println!("sets: {:?}", level_line);
    }
    println!("");

    let mut total_flashes = 0;
    for step_num in 0..400 {
        total_flashes += step(&mut levels);
        let all = levels
            .iter()
            .map(|ll| ll.iter().all(|o| o == &0_u32))
            .all(|r| r);
        if all {
            for level_line in levels.iter() {
                println!("sets: {:?}", level_line);
            }
            println!("{:?}", step_num + 1);
        }
    }
    println!("sets: {:?}", total_flashes);
}
