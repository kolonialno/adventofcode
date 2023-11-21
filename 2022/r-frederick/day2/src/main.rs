use std::fs;

mod solution;
mod solution_alt;

fn transform(input: &str) -> Vec<&str> {
    input
        .lines()
        .collect()
}

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Unable to find input.txt");

    let input = transform(&contents);

    println!("answer to part 1: {}", solution::solve_part1(&input));
    println!("answer to part 1 (alt): {}", solution_alt::solve_part1(&input));

    println!("answer to part 2: {}", solution::solve_part2(&input));
    println!("answer to part 2 (alt): {}", solution_alt::solve_part2(&input));
}
