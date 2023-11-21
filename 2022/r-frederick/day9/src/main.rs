mod solution;

fn main() {
    let contents = include_str!("../input.txt").lines().collect();

    println!("answer to part 1: {}", solution::solve(&contents, 2));
    println!("answer to part 2: {}", solution::solve(&contents, 10));
}
