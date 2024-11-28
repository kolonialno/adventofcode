use std::collections::HashSet;

use crate::util::file_by_lines;

pub fn run() {
    let lines = file_by_lines("day13.txt");
    let mut coords = HashSet::new();
    let mut blank_line = 0;
    for (i, line) in lines.iter().enumerate() {
        if line.is_empty() {
            blank_line = i;
            break;
        }
        let mut parts = line.split(",");
        let x = parts.next().unwrap().trim().parse::<i32>().unwrap();
        let y = parts.next().unwrap().trim().parse::<i32>().unwrap();
        coords.insert((x, y));
    }

    let fold_instructions = lines
        .iter()
        .skip(blank_line + 1)
        .map(|line| {
            let mut parts = line.split("fold along ");
            parts.next();
            let mut parts = parts.next().unwrap().split("=");
            let axis = parts.next().unwrap().trim();
            let value = parts.next().unwrap().trim().parse::<i32>().unwrap();
            (axis, value)
        })
        .collect::<Vec<_>>();

    let (axis, value) = fold_instructions[0];
    fold(&mut coords, axis, value);

    println!("Part 1: {}", coords.len());

    for (axis, value) in fold_instructions.iter().skip(1) {
        fold(&mut coords, *axis, *value);
    }

    println!("Part 2:");
    print(&coords);
}

fn fold(coords: &mut HashSet<(i32, i32)>, axis: &str, value: i32) {
    let mut to_replace = HashSet::new();
    for (x, y) in coords.iter() {
        if axis == "x" && *x > value {
            to_replace.insert((*x, *y));
        } else if axis == "y" && *y > value {
            to_replace.insert((*x, *y));
        }
    }
    for (x, y) in to_replace.iter() {
        coords.remove(&(*x, *y));
        if axis == "x" {
            coords.insert((2 * value - x, *y));
        } else if axis == "y" {
            coords.insert((*x, 2 * value - y));
        }
    }
}

fn print(coords: &HashSet<(i32, i32)>) {
    let max_x = coords.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = coords.iter().map(|(_, y)| *y).max().unwrap();
    for y in 0..=max_y {
        for x in 0..=max_x {
            if coords.contains(&(x, y)) {
                print!("#");
            } else {
                print!(" ");
            }
        }
        println!();
    }
}
