use crate::{Solution, SolutionPair};
use std::fs::File;
use std::io::{BufReader, Read};

fn has_dup<T: PartialEq>(slice: &[T]) -> bool {
    for i in 1..slice.len() {
        if slice[i..].contains(&slice[i - 1]) {
            return true;
        }
    }
    false
}

fn find_marker<const BUFFER_SIZE: usize>() -> u64 {
    let mut reader = BufReader::new(File::open("input/input.txt").expect("Cannot open input.txt"));
    let mut cursor = 0;
    let mut buffer: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];

    while has_dup(&buffer) {
        reader.read_exact(&mut buffer).unwrap();
        reader.seek_relative(1 - BUFFER_SIZE as i64).unwrap();
        cursor += 1;
    }

    cursor - 1 + buffer.len() as u64
}

pub fn solve() -> SolutionPair {
    let solution1 = find_marker::<4>() as u32;
    let solution2 = find_marker::<14>() as u32;

    (Solution::U32(solution1), Solution::U32(solution2))
}
