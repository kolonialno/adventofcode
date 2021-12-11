use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Read};

fn read<R: Read>(io: R) -> Result<Vec<i32>, Error> {
    Ok(BufReader::new(io)
        .lines()
        .next()
        .unwrap()?
        .trim()
        .split(',')
        .map(|i| i.parse().unwrap())
        .collect())
}

pub fn day07() {
    let inital_hpos = read(File::open("inputs/day07.txt").unwrap()).unwrap();
    let max = *inital_hpos.iter().max().unwrap();
    let min = *inital_hpos.iter().min().unwrap();

    let mut diffs = Vec::<i32>::with_capacity(max as usize);
    for i in min..max {
        diffs.push(inital_hpos.iter().map(|p| (*p-i).abs()).sum());
    }
    println!("answer 1: {:?}", diffs.iter().enumerate().min_by_key(|&(_, item)| item).unwrap());

    
    let mut diffs = Vec::<i32>::with_capacity(max as usize);
    for i in min..max {
        diffs.push(inital_hpos.iter().map(|p| ((*p-i).abs())*((*p-i).abs()+1)/2).sum());
    }
    println!("answer 2: {:?}", diffs.iter().enumerate().min_by_key(|&(_, item)| item).unwrap());
}
