use std::{borrow::Borrow, collections::HashSet, fs::read_to_string, iter::zip};

fn priority(c: &char) -> u16 {
    if c.is_uppercase() {
        return *c as u16 - 'A' as u16 + 27;
    } else {
        return *c as u16 - 'a' as u16 + 1;
    }
}

fn main() {
    let problem = read_to_string("../../input/3.txt").unwrap();
    let priority_sum: u16 = problem
        .trim()
        .split("\n")
        .map(|i| {
            let i = i.to_string();
            let midpoint = i.len() / 2;
            let first = HashSet::<char>::from_iter(i[..midpoint].chars());
            let second = HashSet::<char>::from_iter(i[midpoint..].chars());
            let common = first.intersection(&second).next().unwrap();
            priority(common)
        })
        .sum();

    println!("3a: {}", priority_sum);

    let mut badge_sum: u16 = 0;
    for ((first, second), third) in zip(
        zip(
            problem.trim().split("\n").step_by(3),
            problem.trim().split("\n").skip(1).step_by(3),
        ),
        problem.trim().split("\n").skip(2).step_by(3),
    ) {
        badge_sum += [first, second, third]
            .iter()
            .map(|i| HashSet::<char>::from_iter(i.chars()))
            .reduce(|i, j| HashSet::<char>::from_iter(i.intersection(&j).cloned()))
            .map(|l| priority(&l.iter().next().unwrap()))
            .unwrap();
    }
    println!("3b: {}", badge_sum);
}
