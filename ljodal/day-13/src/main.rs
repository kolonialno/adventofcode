use std::cmp::Ordering;

use json::JsonValue;

#[derive(Debug, Clone, Eq)]
enum Value {
    Number(usize),
    List(Vec<Value>),
}

impl Value {
    fn is_list(&self) -> bool {
        match self {
            Self::List(_) => true,
            _ => false,
        }
    }

    fn as_list(&self) -> Value {
        if self.is_list() {
            self.clone()
        } else {
            Self::List(vec![self.clone()])
        }
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        let value = json::parse(value).expect("Invalid input");
        (&value).into()
    }
}

impl From<&JsonValue> for Value {
    fn from(value: &JsonValue) -> Self {
        if value.is_number() {
            return Value::Number(value.as_usize().expect("Invalid number"));
        } else if value.is_array() {
            return Value::List(value.members().map(|item| item.into()).collect());
        }
        panic!("Invalid input")
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(left), Value::Number(right)) => left == right,
            (Value::List(left), Value::List(right)) => {
                left.len() == right.len()
                    && left.iter().zip(right).all(|(left, right)| left == right)
            }
            (left, right) => left.as_list() == right.as_list(),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Number(left), Value::Number(right)) => left.cmp(right),
            (Value::List(left), Value::List(right)) => {
                for (a, b) in left.iter().zip(right) {
                    let ord = a.cmp(b);

                    if ord.is_gt() {
                        return ord;
                    } else if ord.is_lt() {
                        return ord;
                    }
                }

                if left.len() > right.len() {
                    return Ordering::Greater;
                } else if left.len() < right.len() {
                    return Ordering::Less;
                }

                Ordering::Equal
            }
            // One is a list, other a number
            (left, right) => left.as_list().cmp(&right.as_list()),
        }
    }
}

#[derive(Debug)]
struct Pair(Value, Value);

impl Pair {
    fn is_correct(&self) -> bool {
        return self.0 < self.1;
    }
}

impl IntoIterator for Pair {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        vec![self.0, self.1].into_iter()
    }
}

impl From<&str> for Pair {
    fn from(value: &str) -> Self {
        let (a, b) = value.split_once("\n").expect("Expected one input line");
        Pair(a.into(), b.into())
    }
}

fn solve_part1(input: &str) -> usize {
    let pairs: Vec<Pair> = input.split("\n\n").map(|value| value.into()).collect();
    pairs
        .iter()
        .enumerate()
        .filter(|(_, pair)| pair.is_correct())
        .map(|(i, _)| i + 1)
        .sum()
}

fn solve_part2(input: &str) -> usize {
    let sep1: Value = "[[2]]".into();
    let sep2: Value = "[[6]]".into();

    let mut values: Vec<Value> = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|value| value.into())
        .collect();
    values.push(sep1.clone());
    values.push(sep2.clone());
    values.sort();

    values
        .iter()
        .enumerate()
        .filter(|(_, value)| **value == sep1 || **value == sep2)
        .map(|(i, _)| i + 1)
        .product()
}

fn main() {
    let input = include_str!("./input.txt");
    let sum = solve_part1(input);
    println!("Part 1: {sum}");
    let sum = solve_part2(input);
    println!("Part 2: {sum}");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_pair(input: &str) -> Pair {
        input.into()
    }

    #[test]
    fn test_pair() {
        assert!(parse_pair("[1,1,3,1,1]\n[1,1,5,1,1]").is_correct());
        assert!(parse_pair("[[1],[2,3,4]]\n[[1],4]").is_correct());
        assert!(!parse_pair("[9]\n[[8,7,6]]").is_correct());
        assert!(parse_pair("[[4,4],4,4]\n[[4,4],4,4,4]").is_correct());
        assert!(!parse_pair("[7,7,7,7]\n[7,7,7]").is_correct());
        assert!(parse_pair("[]\n[3]").is_correct());
        assert!(!parse_pair("[[[]]]\n[[]]").is_correct());
        assert!(
            !parse_pair("[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]").is_correct()
        );
    }

    #[test]
    fn test_part1() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part1(input), 13, "Wrong result for pt. 1");
    }

    #[test]
    fn test_part2() {
        let input = include_str!("./test.txt");
        assert_eq!(solve_part2(input), 140, "Wrong result for pt. 2");
    }
}
