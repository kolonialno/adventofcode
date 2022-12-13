use std::{
    collections::{hash_map::RandomState, HashSet},
    str::FromStr,
};

use nom::{
    branch::alt, bytes::complete::tag, character::complete::digit1, multi::separated_list0,
    sequence::delimited, IResult,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Expr {
    Num(i32),
    List(Vec<Expr>),
}

/// Recursive parser for an expression, as used in FromStr.
impl Expr {
    fn num_parser(input: &str) -> IResult<&str, Expr> {
        digit1(input).map(|(s, n)| (s, Expr::Num(n.parse().unwrap())))
    }

    fn list_parser(input: &str) -> IResult<&str, Expr> {
        delimited(tag("["), separated_list0(tag(","), Self::parser), tag("]"))(input)
            .map(|(s, l)| (s, Expr::List(l)))
    }

    fn parser(input: &str) -> IResult<&str, Expr> {
        alt((Self::num_parser, Self::list_parser))(input)
    }
}

impl FromStr for Expr {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_, expr) = Self::parser(s).expect("Unable to parse expression");
        Ok(expr)
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Expr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self {
            Self::Num(self_num) => match other {
                Self::Num(other_num) => self_num.cmp(other_num),
                Self::List(other_exprs) => vec![self.clone()].cmp(other_exprs),
            },
            Self::List(self_exprs) => match other {
                Self::Num(other_num) => self_exprs.cmp(&vec![Self::Num(*other_num)]),
                Self::List(other_exprs) => self_exprs.cmp(other_exprs),
            },
        }
    }
}

fn run_part_one(s: &str) -> usize {
    s.split("\n\n")
        .map(|lines| {
            let (e1, e2) = lines.split_once('\n').unwrap();
            (e1.parse::<Expr>().unwrap(), e2.parse::<Expr>().unwrap())
        })
        .map(|(e1, e2)| e1.cmp(&e2))
        .enumerate()
        .filter_map(|(i, ord)| if ord.is_le() { Some(i + 1) } else { None })
        .sum()
}

fn run_part_two(s: &str) -> i32 {
    let mut expressions = s
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.parse::<Expr>().unwrap())
        .collect::<Vec<_>>();

    // Build the divider packets and add them to the expression vec
    let divider_packets = vec!["[[2]]", "[[6]]"]
        .into_iter()
        .map(|s| s.parse::<Expr>().unwrap())
        .collect::<Vec<_>>();
    expressions.append(&mut divider_packets.clone());

    // Create a set from the divider packets to speed things up
    let divider_packets: HashSet<Expr, RandomState> =
        HashSet::from_iter(divider_packets.into_iter());

    // Sort the expressions and find the divider packet indices, calculating
    // their product
    expressions.sort();
    expressions
        .iter()
        .enumerate()
        .filter_map(|(i, expr)| {
            if divider_packets.contains(expr) {
                Some((i + 1) as i32)
            } else {
                None
            }
        })
        .product()
}

fn main() {
    let input = include_str!("../input.txt");

    let part_one = run_part_one(input);
    println!("Part 1: {part_one}");

    let part_two = run_part_two(input);
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod tests {
    use crate::{run_part_one, run_part_two};

    static SAMPLE: &str = include_str!("../sample.txt");

    #[test]
    fn sample_part_one_is_correct() {
        assert_eq!(run_part_one(SAMPLE), 13);
    }

    #[test]
    fn sample_part_two_is_correct() {
        assert_eq!(run_part_two(SAMPLE), 140);
    }
}
