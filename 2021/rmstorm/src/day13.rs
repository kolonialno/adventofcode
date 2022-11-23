use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<(Vec<[i32; 2]>, Vec<[i32; 2]>), Error> {
    let br = BufReader::new(io);
    let mut coords = vec![];
    let mut folds = vec![];

    for line in br.lines() {
        let line: String = line?.trim().to_string();
        println!("{:?}", line);
        match line.chars().next() {
            Some('f') => match line.chars().nth(11).unwrap() {
                'x' => folds.push([line.split("=").nth(1).unwrap().parse().unwrap(), 0]),
                'y' => folds.push([0, line.split("=").nth(1).unwrap().parse().unwrap()]),
                _ => {}
            },
            None => {}
            _ => coords.push(
                line.split(",")
                    .map(|i| i.parse().unwrap())
                    .collect::<Vec<i32>>()
                    .try_into()
                    .unwrap(),
            ),
        }
    }
    Ok((coords, folds))
}

pub fn day13() {
    let (mut coords, folds) = read(File::open("inputs/day13.txt").unwrap()).unwrap();
    let mut myhash: HashSet<[i32; 2]> = HashSet::from_iter(coords.iter().cloned());

    for (i, fold) in folds.iter().enumerate() {
        coords = match fold[0] {
            0 => coords
                .iter()
                .map(|a| [a[0], (-(a[1] - fold[1]).abs()).rem_euclid(fold[1])])
                .collect(),
            _ => coords
                .iter()
                .map(|a| [(-(a[0] - fold[0]).abs()).rem_euclid(fold[0]), a[1]])
                .collect(),
        };
        if i == 1 {
            let mut myhash: HashSet<[i32; 2]> = HashSet::from_iter(coords.iter().cloned());
            println!("myhash: {:?}", myhash.len());
        }
    }

    for i in 0..6 {
        for ii in 0..40 {
            if coords.contains(&[ii, i]) {
                print!("#")
            } else {
                print!(" ")
            }
        }
        print!("\n")
    }
}
