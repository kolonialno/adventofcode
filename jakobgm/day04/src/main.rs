use std::fs::read_to_string;

fn main() {
    let problem = read_to_string("../input/4.txt").unwrap();
    let pairs: Vec<[usize; 4]> = problem
        .trim()
        .split("\n")
        .map(|line| line.split_once(",").unwrap())
        .map(|(first, second)| {
            let (a, b) = first.split_once("-").unwrap();
            let (x, y) = second.split_once("-").unwrap();
            [a, b, x, y].map(|i| i.parse::<usize>().unwrap())
        })
        .collect();

    let overlapping_pairs = pairs
        .iter()
        .filter(|[a, b, x, y]| {
            ((a..=b).contains(&x) && (a..=b).contains(&y))
                || ((x..=y).contains(&a) && (x..=y).contains(&b))
        })
        .count();
    println!("Task 1: {}", overlapping_pairs);

    let partially_overlapping_pairs = pairs
        .iter()
        .filter(|[a, b, x, y]| {
            (a..=b).contains(&x)
                || (a..=b).contains(&y)
                || (x..=y).contains(&a)
                || (x..=y).contains(&b)
        })
        .count();
    println!("Task 2: {}", partially_overlapping_pairs);
}
