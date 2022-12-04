use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    const INPUT_FILE: &str = "input.txt";
    let file = File::open(INPUT_FILE).unwrap();
    let lines = BufReader::new(file).lines().flatten();

    let mut fully_contained = 0;
    let mut overlap = 0;
    for line in lines {
        let (lhs, rhs) = line.split_once(',').unwrap();
        let (lhs_begin, lhs_end) = lhs.split_once('-').unwrap();
        let (rhs_begin, rhs_end) = rhs.split_once('-').unwrap();
        let lhs_range = lhs_begin.parse::<isize>().unwrap()..=lhs_end.parse::<isize>().unwrap();
        let rhs_range = rhs_begin.parse::<isize>().unwrap()..=rhs_end.parse::<isize>().unwrap();
        if rhs_range.contains(lhs_range.start()) && rhs_range.contains(lhs_range.end())
            || lhs_range.contains(rhs_range.start()) && lhs_range.contains(rhs_range.end())
        {
            fully_contained += 1;
        }
        if lhs_range.contains(rhs_range.start())
            || lhs_range.contains(rhs_range.end())
            || rhs_range.contains(lhs_range.start())
            || rhs_range.contains(lhs_range.end())
        {
            overlap += 1;
        }
    }
    println!("Fully contained sectors: {}", fully_contained);
    println!("Overlapping sectors: {}", overlap);
}
