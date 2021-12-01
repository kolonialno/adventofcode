use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");

    println!("{}", part1(input));
    println!("{}", part2(input));
}

fn part2(input: &str) -> i32 {
    let mut increases = 0;
    input
        .lines()
        .map(|line| line.parse().unwrap())
        .tuple_windows::<(i32, i32, i32)>()
        .map(|tuple| tuple.0 + tuple.1 + tuple.2)
        .fold(999999, |acc, val| {
            if val > acc {
                increases += 1
            }
            val
        });

    increases
}

fn part1(input: &str) -> i32 {
    let mut increases = 0;
    input
        .lines()
        .map(|line| line.parse().unwrap())
        .fold(999999, |acc, val| {
            if val > acc {
                increases += 1
            }
            val
        });

    increases
}
