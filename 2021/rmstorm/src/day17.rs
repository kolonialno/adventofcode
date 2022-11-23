use std::cmp::max;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<Vec<Vec<i32>>, Error> {
    let mut br = BufReader::new(io);
    let mut target = String::new();
    br.read_line(&mut target).unwrap();
    let trim_chars: &[_] = &['x', 'y', '='];
    Ok(target
        .trim()
        .trim_start_matches("target area: ")
        .split(", ")
        .map(|i| {
            i.trim_start_matches(trim_chars)
                .split("..")
                .map(|num| i32::from_str_radix(num, 10).unwrap())
                .collect()
        })
        .collect())
}

fn simulate(mut vector: [i32; 2], target: &Vec<Vec<i32>>) -> bool {
    let mut pos = [0, 0];
    while pos[1] > target[1][0] {
        pos[0] += vector[0];
        pos[1] += vector[1];
        vector[0] = max(vector[0] - 1, 0);
        vector[1] = vector[1] - 1;

        if pos[0] >= target[0][0] && pos[0] <= target[0][1] {
            if pos[1] >= target[1][0] && pos[1] <= target[1][1] {
                return true;
            }
        }
    }
    false
}

pub fn day17() {
    let target = read(File::open("inputs/day17.txt").unwrap()).unwrap();
    println!("target: {:?}", target);
    println!("max heigth: {:?}", (target[1][0] * (target[1][0] + 1)) / 2);

    let mut solutions = 0;
    for x in 0..target[0][1] + 1 {
        for y in target[1][0]..(-target[1][0]) {
            if simulate([x, y], &target) {
                solutions += 1;
            }
        }
    }
    println!("total solutions: {:?}", solutions);
}
