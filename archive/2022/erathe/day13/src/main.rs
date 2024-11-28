use eval::{eval, to_value, Value};
use itertools::EitherOrBoth::{Both, Left, Right};
use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");
    part1(input);
    part2(input);
}

fn process_values(v1: &Value, v2: &Value) -> Command {
    match (v1, v2) {
        // left is out of items
        (Value::Null, _) => {
            return Command::Success;
        }
        // right is out of items
        (_, Value::Null) => {
            return Command::Break;
        }
        // base case
        (Value::Number(v1), Value::Number(v2)) => {
            match (v1.as_i64().unwrap() - v2.as_i64().unwrap()).signum() {
                1 => return Command::Break,
                0 => return Command::Continue,
                -1 => return Command::Success,
                _ => panic!("something went wrong with signum"),
            }
        }
        (Value::Array(arr1), Value::Number(v2)) => {
            return process_values(&to_value(arr1), &to_value(vec![v2]))
        }
        (Value::Number(v1), Value::Array(arr2)) => {
            return process_values(&to_value(vec![v1]), &to_value(arr2))
        }
        (Value::Array(arr1), Value::Array(arr2)) => {
            // pad with null values so we can determine longest / shortest
            for (av1, av2) in arr1.iter().zip_longest(arr2.iter()).map(|x| match x {
                Both(a, b) => (a, b),
                Left(a) => (a, &Value::Null),
                Right(b) => (&Value::Null, b),
            }) {
                match process_values(av1, av2) {
                    Command::Break => return Command::Break,
                    Command::Success => return Command::Success,
                    Command::Continue => continue,
                }
            }
        }
        _ => panic!("couldn't parse"),
    }
    Command::Continue
}

#[derive(PartialEq, Eq)]
enum Command {
    Break,
    Continue,
    Success,
}

fn part2(input: &str) {
    let mut values = input
        .lines()
        .filter(|l| !l.is_empty())
        .map(|s| eval(s).unwrap())
        .collect::<Vec<Value>>();
    values.sort_unstable_by(|a, b| match process_values(a, b) {
        Command::Success => std::cmp::Ordering::Less,
        Command::Break => std::cmp::Ordering::Greater,
        Command::Continue => panic!("should not happen"),
    });
    let res = values
        .into_iter()
        .enumerate()
        .filter(|(_, v)| v == &to_value(vec![vec![2]]) || v == &to_value(vec![vec![6]]))
        .map(|(idx, _)| idx + 1)
        .product::<usize>();
    println!("part 2: {:?}", res)
}

fn part1(input: &str) {
    let values = input.split("\n\n").map(|s| {
        let (one, two) = s.split_once('\n').unwrap();
        (eval(one).unwrap(), eval(two).unwrap())
    });
    let res = values
        .enumerate()
        .filter(|(_, (one, two))| process_values(one, two) == Command::Success)
        .map(|(idx, _)| idx + 1)
        .sum::<usize>();
    println!("part 1: {:?}", res);
}
