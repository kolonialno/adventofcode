use std::fs::File;
use std::io::{BufRead, BufReader, Error, ErrorKind, Read};

fn read<R: Read>(io: R) -> Result<Vec<i64>, Error> {
    let br = BufReader::new(io);
    let mut v = vec![];
    for line in br.lines() {
        v.push(
            line?
                .trim()
                .parse()
                .map_err(|e| Error::new(ErrorKind::InvalidData, e))?,
        );
    }
    Ok(v)
}

fn part1() {
    let v = read(File::open("inputs/day01.txt").unwrap()).unwrap();
    let mut increment_counter = 0;
    for (pos1, e1) in v.iter().enumerate() {
        if pos1 > 0 && (e1 - v[pos1 - 1]) > 0 {
            increment_counter += 1;
        }
    }
    println!("{:?}", increment_counter)
}

fn part2() {
    let v = read(File::open("inputs/day01.txt").unwrap()).unwrap();
    let mut increment_counter = 0;
    let mut prev_sum: i64 = 1000000;
    for (pos1, _e1) in v.iter().enumerate() {
        if pos1 > 1 {
            let cur_sum: i64 = (&v[pos1 - 2..pos1 + 1]).iter().sum();
            if cur_sum > prev_sum {
                increment_counter += 1;
            }
            prev_sum = cur_sum;
        }
    }
    println!("{:?}", increment_counter)
}

pub fn day01() {
    part1();
    part2();
}
