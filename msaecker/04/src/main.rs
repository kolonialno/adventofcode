use std::{
    fs::File,
    io::{BufRead, BufReader},
    ops::RangeInclusive,
};

fn get_ranges(line: String) -> (RangeInclusive<isize>, RangeInclusive<isize>) {
    let (lhs, rhs) = line.split_once(',').unwrap();
    let (lhs_begin, lhs_end) = lhs.split_once('-').unwrap();
    let (rhs_begin, rhs_end) = rhs.split_once('-').unwrap();
    let lhs_range = lhs_begin.parse::<isize>().unwrap()..=lhs_end.parse::<isize>().unwrap();
    let rhs_range = rhs_begin.parse::<isize>().unwrap()..=rhs_end.parse::<isize>().unwrap();
    (lhs_range, rhs_range)
}

fn is_any_fully_contained(
    lhs_range: &RangeInclusive<isize>,
    rhs_range: &RangeInclusive<isize>,
) -> bool {
    rhs_range.contains(lhs_range.start()) && rhs_range.contains(lhs_range.end())
        || lhs_range.contains(rhs_range.start()) && lhs_range.contains(rhs_range.end())
}

fn has_overlap(lhs_range: &RangeInclusive<isize>, rhs_range: &RangeInclusive<isize>) -> bool {
    lhs_range.contains(rhs_range.start())
        || lhs_range.contains(rhs_range.end())
        || rhs_range.contains(lhs_range.start())
        || rhs_range.contains(lhs_range.end())
}

fn main() {
    const INPUT_FILE: &str = "input.txt";
    let file = File::open(INPUT_FILE).unwrap();
    let lines = BufReader::new(file).lines().flatten();

    let mut fully_contained = 0;
    let mut overlap = 0;
    for line in lines {
        let (lhs_range, rhs_range) = get_ranges(line);
        if is_any_fully_contained(&lhs_range, &rhs_range) {
            fully_contained += 1;
        }
        if has_overlap(&lhs_range, &rhs_range) {
            overlap += 1;
        }
    }
    println!("Fully contained sectors: {}", fully_contained);
    println!("Overlapping sectors: {}", overlap);
}
