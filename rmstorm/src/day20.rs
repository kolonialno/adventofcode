use ndarray::{arr1, arr2, Array1, Array2};
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<(Vec<Vec<char>>, Vec<char>), Error> {
    let mut br = BufReader::new(io);

    // First line contains rule
    let mut first_line = String::new();
    br.read_line(&mut first_line).unwrap();
    let first_line = first_line
        .trim()
        .chars()
        .map(|c| if c == '#' { '1' } else { '0' })
        .collect();

    // Read the rest of the lines
    let mut image_lines = vec![];
    for line in br.lines() {
        let line: Vec<char> = line?
            .trim()
            .chars()
            .map(|c| if c == '#' { '1' } else { '0' })
            .collect();
        if line.len() != 0 {
            image_lines.push(line);
        }
    }
    Ok((image_lines, first_line))
}

fn rule_id(image: &Vec<Vec<char>>, coord: [isize; 2], bg: char) -> usize {
    let mut rule = vec![];
    for i in coord[0] - 1..coord[0] + 2 {
        for ii in coord[1] - 1..coord[1] + 2 {
            if i < 0 || ii < 0 || i >= image.len() as isize || ii >= image[0].len() as isize {
                rule.push(bg);
            } else {
                rule.push(image[i as usize][ii as usize]);
            }
        }
    }
    let rule: String = rule.into_iter().collect();
    usize::from_str_radix(&rule, 2).unwrap()
}

fn derive_new_image(image: &Vec<Vec<char>>, rule: &Vec<char>, bg: char) -> Vec<Vec<char>> {
    let mut new_image = vec![];
    for i in -1..image.len() as isize + 1 {
        let mut nem_image_line = vec![];
        for ii in -1..image[0].len() as isize + 1 {
            nem_image_line.push(rule[rule_id(&image, [i, ii], bg)]);
        }
        new_image.push(nem_image_line)
    }
    new_image
}

pub fn day20() {
    let (mut image, rule) = read(File::open("inputs/day20.txt").unwrap()).unwrap();

    for i in 0..50 {
        let bg = if i % 2 == 0 { '0' } else { '1' };
        image = derive_new_image(&image, &rule, bg);
    }

    let answer: u32 = image
        .iter()
        .map(|v| v.iter().map(|&c| c as u32 - '0' as u32).sum::<u32>())
        .sum();
    println!("answer {:?}", answer);
}
