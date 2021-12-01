use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> i32 {
    input
        .lines()
        .map(|line| line.parse().unwrap())
        .tuple_windows::<(i32, i32, i32)>()
        .map(|tuple| tuple.0 + tuple.1 + tuple.2)
        .fold((999999, 0), |acc, val| {
            if val > acc.0 {
                return (val, acc.1 + 1);
            }
            (val, acc.1)
        })
        .1
}

fn part1(input: &str) -> i32 {
    input
        .lines()
        .map(|line| line.parse().unwrap())
        .fold((999999, 0), |acc, val| {
            if val > acc.0 {
                return (val, acc.1 + 1);
            }
            (val, acc.1)
        })
        .1
}
