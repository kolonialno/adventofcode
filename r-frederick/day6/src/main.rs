use std::fs;

mod solution;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to find input.txt");

    println!(
        "answer to part 1: {}",
        solution::solve(&contents, 4)
    );
    println!(
        "answer to part 2: {}",
        solution::solve(&contents, 12)
    );
}
