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

fn get_adjacent_coords(map: &Vec<Vec<u32>>, coord: [usize; 2]) -> Vec<[usize; 2]> {
    let mut adjacents = vec![];
    if coord[0] > 0 {
        adjacents.push([coord[0] - 1, coord[1]])
    }
    if coord[1] > 0 {
        adjacents.push([coord[0], coord[1] - 1])
    }
    if coord[1] + 1 < map[0].len() {
        adjacents.push([coord[0], coord[1] + 1])
    }
    if coord[0] + 1 < map.len() {
        adjacents.push([coord[0] + 1, coord[1]])
    }
    adjacents
}

fn find_adjacents(map: &Vec<Vec<u32>>, basin_points: &mut Vec<[usize; 2]>, coord: [usize; 2]) {
    for a_coord in get_adjacent_coords(&map, coord) {
        if &map[a_coord[0]][a_coord[1]] < &9 {
            if !basin_points.contains(&a_coord) {
                basin_points.push(a_coord);
                find_adjacents(map, basin_points, a_coord);
            }
        }
    }
}

fn find_basin_size(map: &Vec<Vec<u32>>, coord: [usize; 2]) -> usize {
    let mut basin_points = vec![coord];
    find_adjacents(map, &mut basin_points, coord);
    basin_points.len()
}

pub fn day09() {
    let height_map = read(File::open("inputs/day09.txt").unwrap()).unwrap();
    let mut ans1_sum = 0;
    let mut low_points: Vec<[usize; 2]> = Vec::new();
    for (i, h_line) in height_map.iter().enumerate() {
        for (ii, h) in h_line.iter().enumerate() {
            if h < &9
                && get_adjacent_coords(&height_map, [i, ii])
                    .iter()
                    .all(|a_coord| &height_map[a_coord[0]][a_coord[1]] > h)
            {
                ans1_sum += h + 1;
                low_points.push([i, ii]);
                print!("\u{001b}[31m{:?}\u{001b}[0m", h);
            } else {
                print!("{:?}", h);
            }
        }
        println!()
    }
    println!("answer 1: {:?}", ans1_sum);
    let mut basin_sizes: Vec<usize> = low_points.iter().map(|c| find_basin_size(&height_map, *c)).collect();
    basin_sizes.sort_by(|a, b| b.cmp(a));
    println!("answer 2: {:?}", basin_sizes[0] * basin_sizes[1] * basin_sizes[2]);
}
