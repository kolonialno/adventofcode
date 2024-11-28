use std::fs;

mod solution;

fn transform(input: &str) -> Vec<Vec<&str>> {
    input.lines().map(|l| l.split(',').collect()).collect()
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to find input.txt");

    let input = transform(&contents);

    println!("answer to part 1: {}", solution::solve_part1(&input));
    println!("answer to part 2: {}", solution::solve_part2(&input));
}
