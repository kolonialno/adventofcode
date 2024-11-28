use crate::util::file_by_lines_as_i32;

pub fn run() {
    let numbers = file_by_lines_as_i32("day01.txt");

    let part1 = numbers.windows(2).filter(|w| w[0] < w[1]).count();
    println!("Part 1: {}", part1);

    // Part 2
    let grouped: Vec<_> = numbers.windows(3).map(|w| w[0] + w[1] + w[2]).collect();

    let part2 = grouped.windows(2).filter(|w| w[0] < w[1]).count();
    println!("Part 2: {}", part2);
}
