use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> usize {
    input
        .lines()
        .map(|line| line.parse().unwrap())
        .tuple_windows::<(i32, i32, i32, i32)>()
        .filter(|v| v.3 > v.0)
        .count()
}

fn part1(input: &str) -> usize {
    input
        .lines()
        .map(|line| line.parse().unwrap())
        .tuple_windows::<(i32, i32)>()
        .filter(|(v1, v2)| v2 > v1)
        .count()
}
