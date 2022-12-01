use std::fs;

fn main() {
    let problem = fs::read_to_string("input/1.txt").unwrap();
    let mut elves: Vec<usize> = problem
        .trim()
        .split("\n\n")
        .map(|i| i.split("\n").map(|j| j.parse::<usize>().unwrap()))
        .map(|j| j.sum::<usize>())
        .collect();

    elves.sort();
    elves.reverse();
    println!("{}", elves[0]);
    println!("{}", elves[0..3].into_iter().sum::<usize>())
}
